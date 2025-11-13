#![feature(likely_unlikely)]
pub mod mem;
pub mod utils;
pub mod instrs;

use std::{ops::Rem, process::exit, sync::Arc, thread::sleep_ms, time::Instant};

use colourful::ColourBrush;

use crate::{instrs::{Instr, InstrCache}, mem::{Memory, Ptr}, utils::{bfi_32, bfi_64, sbfx_32, sbfx_64, ubfx_32, ubfx_64}};

pub struct Emulator {
    pub mem: Arc<Memory>,

    cache: InstrCache,
    mode: Priv,
    pub x  : Regs,
    pc : u64,
    csr: [u64; 4096],
}


#[derive(Debug, Clone, Copy)]
pub enum Priv {
    User = 0,
    Supervisor = 1,
    Machine = 3,
}


const CSR_MVENDORID : usize = 0xF11;
const CSR_MARCHID : usize = 0xF12;
const CSR_MIMPID : usize = 0xF13;
const CSR_MHARTID : usize = 0xF14;

const CSR_MSTATUS: usize = 0x300;
const CSR_MISA: usize = 0x301;
const CSR_MEDELEG: usize = 0x302;
const CSR_MIDELEG: usize = 0x303;
const CSR_MIE: usize = 0x304;
const CSR_MTVEC  : usize = 0x305;
const CSR_MCOUNTEREN : usize = 0x306;
const CSR_MSCRATCH : usize = 0x340;
const CSR_MEPC   : usize = 0x341;
const CSR_MCAUSE : usize = 0x342;
const CSR_MTVAL  : usize = 0x343;
const CSR_MIP: usize = 0x344;
const CSR_MTINST: usize = 0x34A;
const CSR_MTVAL2: usize = 0x34B;

const CSR_CYCLE  : usize = 0xC00;
const CSR_SEPC: usize = 0x141;
const CSR_SCAUSE: usize = 0x142;
const CSR_STVAL: usize = 0x143;
const CSR_STVEC: usize = 0x105;
const CSR_SSTATUS: usize = 0x100;


const CSR_TSELECT : usize = 0x7A0;
const CSR_TDATA1  : usize = 0x7A1;
const CSR_TDATA2  : usize = 0x7A2;
const CSR_TCONTROL: usize = 0x7A5;


const EXC_INSTR_ADDR_MISALIGNED: u64 = 0;
const EXC_INSTR_ACCESS_FAULT:     u64 = 1;
const EXC_ILLEGAL_INSTRUCTION:    u64 = 2;
const EXC_BREAKPOINT:             u64 = 3;
const EXC_LOAD_ADDR_MISALIGNED:   u64 = 4;
const EXC_LOAD_ACCESS_FAULT:      u64 = 5;
const EXC_STORE_ADDR_MISALIGNED:  u64 = 6;
const EXC_STORE_ACCESS_FAULT:     u64 = 7;
const EXC_ECALL_UMODE:            u64 = 8;
const EXC_ECALL_SMODE:            u64 = 9;
// 10 is reserved
const EXC_ECALL_MMODE:            u64 = 11;
const EXC_INSTR_PAGE_FAULT:       u64 = 12;
const EXC_LOAD_PAGE_FAULT:        u64 = 13;
// 14 reserved
const EXC_STORE_PAGE_FAULT:       u64 = 15;


const INT_SOFT_S: u64 = 1;
const INT_SOFT_M: u64 = 3;

const INT_TIMER_S: u64 = 5;
const INT_TIMER_M: u64 = 7;

const INT_EXT_S: u64 = 9;
const INT_EXT_M: u64 = 11;

const MTIME   : usize = 0x0200_BFF8;
const MTIMECMP: usize = 0x0200_4000;


const INTERRUPTS: [u64; 3] = [
    3,  // Machine software
    7,  // Machine timer
    11, // Machine external
];


impl Emulator {
    pub fn new() -> Self {
        Self {
            mem: Arc::new(Memory::new()),
            x: Regs::new(),
            pc: 0,
            csr: [0; _],
            mode: Priv::Machine,
            cache: InstrCache::new(),
            
        }
    }

    pub fn trap(&mut self, cause: u64, tval: u64) {

        let medeleg = self.csr_read(CSR_MEDELEG);
        let mideleg = self.csr_read(CSR_MIDELEG);

        let interrupt = (cause >> 63) != 0;
        let code = cause & (u64::MAX >> 1);

        let delegated_to_s = if interrupt {
            ((mideleg >> code) & 1) != 0
        } else {
            ((medeleg >> code) & 1) != 0
        };

        if delegated_to_s {
            self._trap(
                CSR_SSTATUS, CSR_SEPC, CSR_SCAUSE, CSR_STVAL, CSR_STVEC,
                1, 5, 8, 1, Priv::Supervisor, cause, tval
            );
            return;
        }

        self._trap(
            CSR_MSTATUS, CSR_MEPC, CSR_MCAUSE, CSR_MTVAL, CSR_MTVEC,
            3, 7, 11, 2, Priv::Machine, cause, tval);
    }


    fn _trap(
        &mut self, status_csr: usize, epc_csr: usize, cause_csr: usize,
        tval_csr: usize, tvec_csr: usize, ie_bit: u32, pie_bit: u32,
        pp_bit: u32, pp_len: u32, new_mode: Priv, cause: u64, tval: u64
    ) {
        let interrupt = (cause >> 63) & 1;
        let raw_code  = cause & 0x7FFF_FFFF_FFFF_FFFF;
        let code5     = raw_code & 0x1F;

        // real mcause value
        let mcause = (interrupt << 63) | code5;

        // read & update status
        let mut status = self.csr_read(status_csr);
        let ie = ubfx_64(status, ie_bit, 1);

        status = bfi_64(status, pie_bit, 1, ie);
        status = bfi_64(status, ie_bit, 1, 0);
        status = bfi_64(status, pp_bit, pp_len, self.mode as u64);

        self.csr_write(status_csr, status);
        self.csr_write(epc_csr, self.pc);
        self.csr_write(cause_csr, mcause);
        self.csr_write(tval_csr, tval);

        // switch privilege
        self.mode = new_mode;

        // redirect PC
        let tvec = self.csr_read(tvec_csr);
        let base = tvec & !0b11;
        let tmode = tvec & 0b11;

        if tmode == 1 && interrupt != 0 {
            self.pc = base + 4 * code5;
        } else {
            self.pc = base;
        }
    }


    pub fn tick_timer(&mut self, start: &Instant, timeout_ms: u64, last_ns: &mut u128) -> bool {
        let cycle = self.csr_read(CSR_CYCLE) + 1;
        self.csr_write(CSR_CYCLE, cycle);

        if cycle % 10000 != 0 { return true }
        let mut mtime = self.mem.read_u64(Ptr(MTIME as _));

        const MTIME_TICK_NS: u128 = 100; // 10MHz
        let elapsed = start.elapsed();

        if elapsed.as_millis() as u64 > timeout_ms { return false }

        let now = elapsed.as_nanos();
        let dt = now - *last_ns;

        let div = dt / MTIME_TICK_NS;
        let rem = dt % MTIME_TICK_NS;

        mtime = mtime.wrapping_add(div as u64);

        *last_ns = now - rem;
        self.mem.write(self.mode, Ptr(MTIME as _), &mtime.to_ne_bytes());

        let bit = mtime >= self.mem.read_u64(Ptr(MTIMECMP as _));
        let bit = bit as u64;
        let mip = bfi_64(self.csr_read(CSR_MIP), 7, 1, bit);
        self.csr_write(CSR_MIP, mip);
        true
    }
    


    pub fn new_run(&mut self, timeout_ms: u64, code: &[u8]) -> bool {
        let start = Instant::now();
        let mut last_ns = 0;

        self.x.write(2, 0xB000_0000);
        self.pc = 0x8000_0000;
        self.mem.write(Priv::Machine, Ptr(MTIMECMP as _), &u64::MAX.to_ne_bytes());

        self.mem.write(self.mode, Ptr(self.pc), code);

        loop {
            let cont = self.tick_timer(&start, timeout_ms, &mut last_ns);
            if !cont { return false }


            // check interrupts
            let pending = self.csr_read(CSR_MIP);
            let enabled = self.csr_read(CSR_MIE);
            let global_enabled = ubfx_64(self.csr_read(CSR_MSTATUS), 3, 1);


            if (pending & enabled != 0) && global_enabled == 1 {
                for &code in INTERRUPTS.iter() {
                    let mask = 1 << code;
                    if (pending & mask != 0) && (enabled & mask != 0) {
                        let cause = (1 << 63) | code; // Bit 63 = interrupt
                        self.trap(cause, 0);
                        break;
                    }
                }
            }


            // decode
            if (self.pc & 0b11) != 0 {
                self.trap(EXC_INSTR_ADDR_MISALIGNED, self.pc);
                continue;
            }

            let instr = self.cache.decode(&self.mem, self.pc);

            // execute
            match instr {
                Instr::IAdd { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize).wrapping_add(imm as i64 as u64)
                    )
                },


                Instr::ISlt { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        ((self.x.read(rs1 as usize) as i64) < imm as i64) as u64
                    )
                },


                Instr::ISltu { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        (self.x.read(rs1 as usize) < imm as i64 as u64) as u64
                    )
                },


                Instr::IXor { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize) ^ imm as i64 as u64
                    )
                },


                Instr::IOr { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize) | imm as i64 as u64
                    )
                },


                Instr::IAnd { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize) & imm as i64 as u64
                    )
                },


                Instr::ISll { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize) << ((imm & 0x3F) as u64)
                    )
                },


                Instr::ISrl { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        self.x.read(rs1 as usize) >> ((imm & 0x3F) as u64)
                    )

                },
                

                Instr::ISra { rd, rs1, imm } => {
                    self.x.write(
                        rd as usize, 
                        ((self.x.read(rs1 as usize) as i64) >> ((imm & 0x3F) as u32)) as u64
                    )
                },


                Instr::IAddw { rd, rs1, imm } => {
                    let lhs = self.x.read(rs1 as usize) as i32;
                    let rhs = imm as i32;
                    let result = lhs.wrapping_add(rhs);
                    self.x.write(rd as usize, result as i64 as u64);
                },


                Instr::ISllw { rd, rs1, imm } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = (lhs << rhs) as u32;
                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::ISrlw { rd, rs1, imm } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = lhs >> rhs;
                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::ISraw { rd, rs1, imm } => {
                    let lhs = self.x.read(rs1 as usize) as i32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = lhs >> rhs;
                    self.x.write(rd as usize, result as i64 as u64);
                },


                Instr::RAdd { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs.wrapping_add(rhs);
                    self.x.write(rd as usize, result);
                },


                Instr::RSub { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs.wrapping_sub(rhs);
                    self.x.write(rd as usize, result);
                },


                Instr::RSll { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs << (rhs & 0x3F);
                    self.x.write(rd as usize, result);
                },


                Instr::RSlt { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = ((lhs as i64) < (rhs as i64)) as u64;
                    self.x.write(rd as usize, result);
                },


                Instr::RSlut { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = (lhs < rhs) as u64;
                    self.x.write(rd as usize, result);
                },


                Instr::RXor { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs ^ rhs;
                    self.x.write(rd as usize, result);
                },


                Instr::RSrl { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs >> (rhs & 0x3F);
                    self.x.write(rd as usize, result);
                },


                Instr::RSra { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = ((lhs as i64) >> (rhs & 0x3F)) as u64;
                    self.x.write(rd as usize, result);
                },


                Instr::ROr { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs | rhs;
                    self.x.write(rd as usize, result);
                },


                Instr::RAnd { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs & rhs;
                    self.x.write(rd as usize, result);
                },


                Instr::RMul { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    let result = lhs.wrapping_mul(rhs);
                    self.x.write(rd as usize, result);
                },

                
                Instr::RMulh { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let lhs = lhs as i64 as i128;
                    let rhs = rhs as i64 as i128;

                    let product = lhs * rhs;
                    let result = (product >> 64) as i64 as u64;
                    self.x.write(rd as usize, result);
                },


                Instr::RMulhsu { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let lhs = lhs as i64 as i128;
                    let rhs = rhs as u64 as u128;

                    let b128 = rhs as i128;

                    let product = lhs * b128;
                    let result = (product >> 64) as u64;
                    self.x.write(rd as usize, result);
                },


                Instr::RMulhu { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
           
                    let lhs = lhs as u128;
                    let rhs = rhs as u128;

                    let product = lhs * rhs;
                    let result = (product >> 64) as u64;
                    self.x.write(rd as usize, result);
 
                },


                Instr::RDiv { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i64;
                    let rhs = self.x.read(rs2 as usize) as i64;
                    
                    let result =
                        if rhs == 0 { u64::MAX }
                        else if lhs == i64::MIN && rhs == -1 { lhs as u64 }
                        else { (lhs / rhs) as u64 };

                    self.x.write(rd as usize, result);
                },


                Instr::RDivu { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    
                    let result =
                        if rhs == 0 { u64::MAX }
                        else { lhs / rhs };

                    self.x.write(rd as usize, result);
                },


                Instr::RRem { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i64;
                    let rhs = self.x.read(rs2 as usize) as i64;
                    
                    let result =
                        if rhs == 0 { lhs as u64}
                        else if lhs == i64::MIN && rhs == -1 { 0 }
                        else { (lhs % rhs) as u64 };

                    self.x.write(rd as usize, result);
                },


                Instr::RRemu { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);
                    
                    let result =
                        if rhs == 0 { lhs }
                        else { lhs % rhs };

                    self.x.write(rd as usize, result);
                },


                Instr::RAddw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = lhs.wrapping_add(rhs);
                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSubw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = lhs.wrapping_sub(rhs);
                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RMulw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i64;
                    let rhs = self.x.read(rs2 as usize) as i64;

                    let result = lhs.wrapping_mul(rhs);
                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RDivw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i32;
                    let rhs = self.x.read(rs2 as usize) as i32;

                    let result = 
                        if rhs == 0 { -1 } 
                        else if lhs == i32::MIN && rhs == -1 { i32::MIN } 
                        else { lhs.wrapping_div(rhs) };

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RDivuw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = 
                        if rhs == 0 { u32::MAX } 
                        else { lhs.wrapping_div(rhs) };

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RRemw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i32;
                    let rhs = self.x.read(rs2 as usize) as i32;

                    let result = 
                        if rhs == 0 { lhs } 
                        else if lhs == i32::MIN && rhs == -1 { 0 } 
                        else { lhs.wrapping_rem(rhs) };

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RRemuw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = 
                        if rhs == 0 { lhs } 
                        else { lhs.wrapping_rem(rhs) };

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSllw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = lhs << (rhs & 0x1F);

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSrlw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as u32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = lhs >> (rhs & 0x1F);

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSraw { rd, rs1, rs2 } => {
                    let lhs = self.x.read(rs1 as usize) as i32;
                    let rhs = self.x.read(rs2 as usize) as u32;

                    let result = lhs >> (rhs & 0x1F);

                    self.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::Jal { rd, offset } => {
                    self.x.write(rd as usize, self.pc.wrapping_add(4));
                    self.pc = self.pc.wrapping_add(offset as i32 as i64 as u64);
                    continue;
                },


                Instr::Auipc { rd, offset } => {
                    self.x.write(rd as usize, self.pc.wrapping_add(offset as i64 as u64));
                },


                Instr::Lui { rd, imm } => {
                    self.x.write(rd as usize, imm as i64 as u64);
                },


                Instr::CsrRw { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    self.csr_write(csr, self.x.read(rs1 as usize));

                    self.x.write(rd as usize, t);
                },


                Instr::CsrRs { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    if rs1 != 0 {
                        self.csr_write(csr, t | self.x.read(rs1 as usize));
                    }

                    self.x.write(rd as usize, t);
                },


                Instr::CsrRc { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    if rs1 != 0 {
                        self.csr_write(csr, t & !self.x.read(rs1 as usize));
                    }

                    self.x.write(rd as usize, t);
                },


                Instr::CsrRwi { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    if rs1 != 0 {
                        self.csr_write(csr, (rs1 & 0x1f) as _);
                    }

                    self.x.write(rd as usize, t);
                }


                Instr::CsrRsi { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    if rs1 != 0 {
                        self.csr_write(csr, t | rs1 as u64);
                    }

                    self.x.write(rd as usize, t);
                },


                Instr::CsrRci { rd, rs1, csr } => {
                    let csr = csr as usize;
                    let t = self.csr_read(csr);

                    if rs1 != 0 {
                        self.csr_write(csr, t & !(rs1 as u64));
                    }

                    self.x.write(rd as usize, t);
                },


                Instr::SECall { } => {
                    //println!("ecall");
                    let cause = match self.mode {
                        Priv::User        => EXC_ECALL_UMODE,
                        Priv::Supervisor  => EXC_ECALL_SMODE,
                        Priv::Machine     => EXC_ECALL_MMODE,
                    };


                    let num = self.x.read(17);
                    if num == 93 {
                        break;
                    } else if num == 0xCC {

                        let panic_log_len_ptr = Ptr(0xFFFF_F000);
                        let panic_log_ptr = Ptr(0xFFFF_F010);

                        let len = self.mem.read_u32(panic_log_len_ptr);
                        let mut msg = Vec::with_capacity(len as usize);

                        for i in 0..len {
                            msg.push(self.mem.read_u8(Ptr(panic_log_ptr.0 + i as u64)));
                        }

                        let msg = core::str::from_utf8(&msg).unwrap();

                        panic!("{msg}");


                    }

                    self.trap(cause, 0);
                    continue;
                },


                Instr::SEBreak { } => {
                    self.trap(EXC_BREAKPOINT, self.pc);
                    continue;
                },


                Instr::SSRet { } => {
                    self._ret(CSR_SSTATUS, CSR_SEPC, 1, 5, 8, 1);
                    continue;
                },


                Instr::SMRet { } => {
                    self._ret(CSR_MSTATUS, CSR_MEPC, 3, 7, 11, 2);
                    continue;
                },


                Instr::SWfi { } => {
                    while self.csr_read(CSR_MIE) & self.csr_read(CSR_MIP) == 0 {
                        let cont = self.tick_timer(&start, timeout_ms, &mut last_ns);
                        if !cont { return false }
                    }
                },


                Instr::LB { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u8(ptr) as i8 as i64 as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LBu { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u8(ptr) as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LH { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u16(ptr) as i16 as i64 as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LHu { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u16(ptr) as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LW { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u32(ptr) as i32 as i64 as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LWu { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u32(ptr) as u64;

                    self.x.write(rd as usize, result);
                },


                Instr::LD { rd, rs1, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = self.mem.read_u64(ptr);

                    self.x.write(rd as usize, result);
                },

                Instr::SB { rs1, rs2, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &[(self.x.read(rs2 as usize) & 0xFF) as u8];

                    self.mem.write(self.mode, ptr, slice);
                },


                Instr::SH { rs1, rs2, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &((self.x.read(rs2 as usize) & 0xFFFF) as u16).to_ne_bytes();

                    self.mem.write(self.mode, ptr, slice);
                },


                Instr::SW { rs1, rs2, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &((self.x.read(rs2 as usize) & 0xFFFF_FFFF) as u32).to_ne_bytes();

                    self.mem.write(self.mode, ptr, slice);
                },


                Instr::SD { rs1, rs2, offset } => {
                    let ptr = (self.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &self.x.read(rs2 as usize).to_ne_bytes();

                    self.mem.write(self.mode, ptr, slice);
                },


                Instr::BEq { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let cond = lhs == rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::BNe { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let cond = lhs != rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::BLt { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize) as i64;
                    let rhs = self.x.read(rs2 as usize) as i64;

                    let cond = lhs < rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::BLtu { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let cond = lhs < rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::BGe { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize) as i64;
                    let rhs = self.x.read(rs2 as usize) as i64;

                    let cond = lhs >= rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::BGeu { rs1, rs2, imm } => {
                    let lhs = self.x.read(rs1 as usize);
                    let rhs = self.x.read(rs2 as usize);

                    let cond = lhs >= rhs;

                    if cond {
                        self.pc = self.pc.wrapping_add(imm as u64);
                        continue;
                    }
                },


                Instr::JAlr { rd, rs1, imm } => {
                    let t = self.pc + 4;
                    self.pc = self.x.read(rs1 as usize).wrapping_add(imm as u64) & (!1);
                    self.x.write(rd as usize, t);
                    continue;
                },

                Instr::Nop => (),

                Instr::Unknown => {
                    self.trap(EXC_ILLEGAL_INSTRUCTION, self.mem.read_u32(Ptr(self.pc)) as u64);
                    continue;
                },


                Instr::NotEncoded => unreachable!(),
            }


            // finish
            self.pc += 4;
        }


        return true;

    }


    pub fn run(&mut self, timeout_ms: u64, code: &[u8]) -> bool {
        return self.new_run(timeout_ms, code);
    }


    fn _ret(&mut self, status: usize, epc: usize, ie_bit: u32, pie_bit: u32, pp_bit: u32, pp_len: u32) {
        let mut mstatus = self.csr_read(status);
        let mpie = ubfx_64(mstatus, pie_bit, 1);
        let mpp  = ubfx_64(mstatus, pp_bit, pp_len);

        // Restore privilege
        self.mode = match mpp {
            0 => Priv::User,
            1 => Priv::Supervisor,
            3 => Priv::Machine,
            _ => Priv::User,
        };

        // Restore interrupt enable: MIE = MPIE
        mstatus = bfi_64(mstatus, ie_bit, 1, mpie);
        // Clear MPIE
        mstatus = bfi_64(mstatus, pie_bit, 1, 0);
        // Set MPP = User (0)
        mstatus = bfi_64(mstatus, pp_bit, pp_len, 0);

        self.csr_write(status, mstatus);
        self.pc = self.csr_read(epc);
    }


    pub fn csr_read(&mut self, csr: usize) -> u64 {
        match csr {
            CSR_SSTATUS => {
                let mstatus = self.csr[CSR_MSTATUS];

                // Map bits: SSTATUS is view of MSTATUS
                let sie  = (mstatus >> 1) & 1;
                let spie = (mstatus >> 5) & 1;
                let spp  = (mstatus >> 8) & 1;

                // Only keep SIE, SPIE, SPP, UXL
                let uxl = (mstatus >> 32) & 0b11;

                (sie << 1)
                | (spie << 5)
                | (spp << 8)
                | (uxl << 32)
            }

            _ => self.csr[csr],
        }
    }


    pub fn csr_write(&mut self, csr: usize, value: u64) {
        match csr {
            CSR_SSTATUS => {
                // Updates to SSTATUS must write into MSTATUS
                let mut mstatus = self.csr[CSR_MSTATUS];

                // Mask writable fields (SIE=1, SPIE=5, SPP=8)
                let sie  = (value >> 1) & 1;
                let spie = (value >> 5) & 1;
                let spp  = (value >> 8) & 1;

                mstatus = (mstatus & !(1 << 1)) | (sie << 1);
                mstatus = (mstatus & !(1 << 5)) | (spie << 5);
                mstatus = (mstatus & !(1 << 8)) | (spp << 8);

                self.csr[CSR_MSTATUS] = mstatus;
            }

            CSR_MSTATUS => {
                // Mask off WLRL bits
                let mask = 0
                    | (1 << 3)  // MIE
                    | (1 << 7)  // MPIE
                    | (3 << 11) // MPP (2 bits)
                    | (3 << 32); // UXL
                self.csr[CSR_MSTATUS] = value & mask;
            }

            CSR_MTVEC => {
                // Low 2 bits must be 0 (direct) or 1 (vectored)
                let mode = value & 0b11;
                let base = value & !0b11;
                self.csr[CSR_MTVEC] = base | mode.min(1);
            }

            // Most CSRs just store the raw value
            _ => self.csr[csr] = value,
        }
    }
}


pub struct Regs {
    regs: [u64; 32]
}


impl Regs {
    pub fn new() -> Self {
        Self {
            regs: [0; 32],
        }
    }


    pub fn read(&self, idx: usize) -> u64 {
        self.regs[idx]
    }


    pub fn write(&mut self, idx: usize, data: u64) {
        if idx == 0 { return }
        self.regs[idx] = data;
    }
}


#[inline(always)]
fn read_cntvct() -> u64 {
    let value: u64;
    unsafe {
        core::arch::asm!("mrs {}, cntvct_el0", out(reg) value);
    }
    value
}

#[inline(always)]
fn read_cntfrq() -> u64 {
    let freq: u64;
    unsafe {
        core::arch::asm!("mrs {}, cntfrq_el0", out(reg) freq);
    }
    freq
}
