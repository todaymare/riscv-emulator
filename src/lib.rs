#![feature(likely_unlikely)]
#![feature(array_ptr_get)]
#![feature(cold_path)]
#![forbid(unused_must_use)]
pub mod mem;
pub mod utils;
pub mod instrs;

use std::{hint::cold_path, mem::discriminant, ops::Rem, process::exit, ptr::null, sync::{atomic::{AtomicU64, Ordering}, Arc, Mutex}, thread::sleep_ms, time::Instant};

use colourful::ColourBrush;

use crate::{instrs::{CodePtr, Instr, InstrCache}, mem::{Memory, Ptr}, utils::{bfi_32, bfi_64, sbfx_32, sbfx_64, ubfx_32, ubfx_64}};

pub struct Emulator {
    pub local: Mutex<Local>,
    pub shared: Shared,
}


pub struct Shared {
    pub mem: Memory,
    pub csr: Csr,
    timeout_ms: u64,
}


pub struct Local {
    cache: InstrCache,
    mode: Priv,
    pub x: Regs,

    start: Instant,
    last_ns: u64,

}


pub struct Csr {
    csr: [AtomicU64; 4096],
}


unsafe impl Send for Emulator {}
unsafe impl Sync for Emulator {}


#[derive(Debug, Clone, Copy)]
pub enum Priv {
    User = 0,
    Supervisor = 1,
    Machine = 3,
}


pub const CSR_MVENDORID : usize = 0xF11;
pub const CSR_MARCHID : usize = 0xF12;
pub const CSR_MIMPID : usize = 0xF13;
pub const CSR_MHARTID : usize = 0xF14;

pub const CSR_MSTATUS: usize = 0x300;
pub const CSR_MISA: usize = 0x301;
pub const CSR_MEDELEG: usize = 0x302;
pub const CSR_MIDELEG: usize = 0x303;
pub const CSR_MIE: usize = 0x304;
pub const CSR_MTVEC  : usize = 0x305;
pub const CSR_MCOUNTEREN : usize = 0x306;
pub const CSR_MSCRATCH : usize = 0x340;
pub const CSR_MEPC   : usize = 0x341;
pub const CSR_MCAUSE : usize = 0x342;
pub const CSR_MTVAL  : usize = 0x343;
pub const CSR_MIP: usize = 0x344;
pub const CSR_MTINST: usize = 0x34A;
pub const CSR_MTVAL2: usize = 0x34B;

pub const CSR_CYCLE  : usize = 0xC00;
pub const CSR_SEPC: usize = 0x141;
pub const CSR_SCAUSE: usize = 0x142;
pub const CSR_STVAL: usize = 0x143;
pub const CSR_STVEC: usize = 0x105;
pub const CSR_SSTATUS: usize = 0x100;
pub const CSR_SIP : usize = 0x144;


pub const CSR_TSELECT : usize = 0x7A0;
pub const CSR_TDATA1  : usize = 0x7A1;
pub const CSR_TDATA2  : usize = 0x7A2;
pub const CSR_TCONTROL: usize = 0x7A5;


pub const EXC_INSTR_ADDR_MISALIGNED: u64 = 0;
pub const EXC_INSTR_ACCESS_FAULT:     u64 = 1;
pub const EXC_ILLEGAL_INSTRUCTION:    u64 = 2;
pub const EXC_BREAKPOINT:             u64 = 3;
pub const EXC_LOAD_ADDR_MISALIGNED:   u64 = 4;
pub const EXC_LOAD_ACCESS_FAULT:      u64 = 5;
pub const EXC_STORE_ADDR_MISALIGNED:  u64 = 6;
pub const EXC_STORE_ACCESS_FAULT:     u64 = 7;
pub const EXC_ECALL_UMODE:            u64 = 8;
pub const EXC_ECALL_SMODE:            u64 = 9;
// 10 is reserved
pub const EXC_ECALL_MMODE:            u64 = 11;
pub const EXC_INSTR_PAGE_FAULT:       u64 = 12;
pub const EXC_LOAD_PAGE_FAULT:        u64 = 13;
// 14 reserved
pub const EXC_STORE_PAGE_FAULT:       u64 = 15;


pub const INT_SOFT_S: u64 = 1;
pub const INT_SOFT_M: u64 = 3;

pub const INT_TIMER_S: u64 = 5;
pub const INT_TIMER_M: u64 = 7;

pub const INT_EXT_S: u64 = 9;
pub const INT_EXT_M: u64 = 11;

pub const MTIME   : usize = 0x0200_BFF8;
pub const MTIMECMP: usize = 0x0200_4000;


const INTERRUPTS: [u64; 6] = [
    INT_SOFT_S,
    INT_SOFT_M,
    INT_TIMER_S,
    INT_TIMER_M,
    INT_EXT_S,
    INT_EXT_M,
];


impl Emulator {
    pub fn new(timeout_ms: u64) -> Self {
        Self {
            shared: Shared {
                mem: Memory::new(),
                csr: Csr::new(),
                timeout_ms,
            },

            local: Mutex::new(Local {
                x: Regs::new(),
                mode: Priv::Machine,
                cache: InstrCache::new(),
                start: Instant::now(),
                last_ns: 0,
            }),
        }
    }



    pub fn run(&self, code: &[u8]) -> bool {

        let shared = &self.shared;
        let mut local = self.local.lock().unwrap();
        let local = &mut *local;
        let mut cycle = 0;
        local.start = Instant::now();
        local.last_ns = 0;
        local.x.write(2, 0xB000_0000);

        shared.mem.write(Priv::Machine, Ptr(MTIMECMP as _), &u64::MAX.to_ne_bytes());
        shared.mem.write(local.mode, Ptr(0x8000_0000), code);

        let mut code = local.cache.set_pc(&shared.mem, 0x8000_0000);

        loop {
            cycle += 1;
            if core::hint::unlikely(cycle % 128 == 0) {
                shared.csr.write(CSR_CYCLE, cycle);
                let (new_code, cont) = local.tick(cycle, shared, code);
                if !cont { return false }

                if let Some(new_code) = new_code {
                    code = new_code;
                }
            }




            // decode
            let instr = code.get();
            //println!("pc: 0x{:x}, instr: {instr:?}", exec.cache.pc());

            // execute
            match instr {
                Instr::IAdd { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize).wrapping_add(imm as i64 as u64)
                    )
                },


                Instr::ISlt { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        ((local.x.read(rs1 as usize) as i64) < imm as i64) as u64
                    )
                },


                Instr::ISltu { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        (local.x.read(rs1 as usize) < imm as i64 as u64) as u64
                    )
                },


                Instr::IXor { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize) ^ imm as i64 as u64
                    )
                },


                Instr::IOr { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize) | imm as i64 as u64
                    )
                },


                Instr::IAnd { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize) & imm as i64 as u64
                    )
                },


                Instr::ISll { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize) << ((imm & 0x3F) as u64)
                    )
                },


                Instr::ISrl { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        local.x.read(rs1 as usize) >> ((imm & 0x3F) as u64)
                    )

                },
                

                Instr::ISra { rd, rs1, imm } => {
                    core::hint::likely(true);
                    local.x.write(
                        rd as usize, 
                        ((local.x.read(rs1 as usize) as i64) >> ((imm & 0x3F) as u32)) as u64
                    )
                },


                Instr::IAddw { rd, rs1, imm } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i32;
                    let rhs = imm as i32;
                    let result = lhs.wrapping_add(rhs);
                    local.x.write(rd as usize, result as i64 as u64);
                },


                Instr::ISllw { rd, rs1, imm } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = (lhs << rhs) as u32;
                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::ISrlw { rd, rs1, imm } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = lhs >> rhs;
                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::ISraw { rd, rs1, imm } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i32;
                    let rhs = (imm & 0x1F) as u32;
                    let result = lhs >> rhs;
                    local.x.write(rd as usize, result as i64 as u64);
                },


                Instr::RAdd { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs.wrapping_add(rhs);
                    local.x.write(rd as usize, result);
                },


                Instr::RSub { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs.wrapping_sub(rhs);
                    local.x.write(rd as usize, result);
                },


                Instr::RSll { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs << (rhs & 0x3F);
                    local.x.write(rd as usize, result);
                },


                Instr::RSlt { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = ((lhs as i64) < (rhs as i64)) as u64;
                    local.x.write(rd as usize, result);
                },


                Instr::RSlut { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = (lhs < rhs) as u64;
                    local.x.write(rd as usize, result);
                },


                Instr::RXor { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs ^ rhs;
                    local.x.write(rd as usize, result);
                },


                Instr::RSrl { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs >> (rhs & 0x3F);
                    local.x.write(rd as usize, result);
                },


                Instr::RSra { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = ((lhs as i64) >> (rhs & 0x3F)) as u64;
                    local.x.write(rd as usize, result);
                },


                Instr::ROr { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs | rhs;
                    local.x.write(rd as usize, result);
                },


                Instr::RAnd { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs & rhs;
                    local.x.write(rd as usize, result);
                },


                Instr::RMul { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    let result = lhs.wrapping_mul(rhs);
                    local.x.write(rd as usize, result);
                },

                
                Instr::RMulh { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let lhs = lhs as i64 as i128;
                    let rhs = rhs as i64 as i128;

                    let product = lhs * rhs;
                    let result = (product >> 64) as i64 as u64;
                    local.x.write(rd as usize, result);
                },


                Instr::RMulhsu { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let lhs = lhs as i64 as i128;
                    let rhs = rhs as u64 as u128;

                    let b128 = rhs as i128;

                    let product = lhs * b128;
                    let result = (product >> 64) as u64;
                    local.x.write(rd as usize, result);
                },


                Instr::RMulhu { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
           
                    let lhs = lhs as u128;
                    let rhs = rhs as u128;

                    let product = lhs * rhs;
                    let result = (product >> 64) as u64;
                    local.x.write(rd as usize, result);
 
                },


                Instr::RDiv { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;
                    
                    let result =
                        if rhs == 0 { u64::MAX }
                        else if lhs == i64::MIN && rhs == -1 { lhs as u64 }
                        else { (lhs / rhs) as u64 };

                    local.x.write(rd as usize, result);
                },


                Instr::RDivu { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    
                    let result =
                        if rhs == 0 { u64::MAX }
                        else { lhs / rhs };

                    local.x.write(rd as usize, result);
                },


                Instr::RRem { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;
                    
                    let result =
                        if rhs == 0 { lhs as u64}
                        else if lhs == i64::MIN && rhs == -1 { 0 }
                        else { (lhs % rhs) as u64 };

                    local.x.write(rd as usize, result);
                },


                Instr::RRemu { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);
                    
                    let result =
                        if rhs == 0 { lhs }
                        else { lhs % rhs };

                    local.x.write(rd as usize, result);
                },


                Instr::RAddw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = lhs.wrapping_add(rhs);
                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSubw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = lhs.wrapping_sub(rhs);
                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RMulw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;

                    let result = lhs.wrapping_mul(rhs);
                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RDivw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i32;
                    let rhs = local.x.read(rs2 as usize) as i32;

                    let result = 
                        if rhs == 0 { -1 } 
                        else if lhs == i32::MIN && rhs == -1 { i32::MIN } 
                        else { lhs.wrapping_div(rhs) };

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RDivuw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = 
                        if rhs == 0 { u32::MAX } 
                        else { lhs.wrapping_div(rhs) };

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RRemw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i32;
                    let rhs = local.x.read(rs2 as usize) as i32;

                    let result = 
                        if rhs == 0 { lhs } 
                        else if lhs == i32::MIN && rhs == -1 { 0 } 
                        else { lhs.wrapping_rem(rhs) };

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RRemuw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = 
                        if rhs == 0 { lhs } 
                        else { lhs.wrapping_rem(rhs) };

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSllw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = lhs << (rhs & 0x1F);

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSrlw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as u32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = lhs >> (rhs & 0x1F);

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::RSraw { rd, rs1, rs2 } => {
                    core::hint::likely(true);
                    let lhs = local.x.read(rs1 as usize) as i32;
                    let rhs = local.x.read(rs2 as usize) as u32;

                    let result = lhs >> (rhs & 0x1F);

                    local.x.write(rd as usize, result as i32 as i64 as u64);
                },


                Instr::Jal { rd, offset } => {
                    let pc = local.cache.pc(code);
                    let (new_cptr, success) = local.set_pc(shared, code, pc.wrapping_add(offset as i32 as i64 as u64));
                    if success {
                        local.x.write(rd as usize, pc.wrapping_add(4));
                    }

                    code = new_cptr;

                    continue;
                },


                Instr::Auipc { rd, offset } => {
                    local.x.write(rd as usize, local.cache.pc(code).wrapping_add(offset as i64 as u64));
                },


                Instr::Lui { rd, imm } => {
                    local.x.write(rd as usize, imm as i64 as u64);
                },


                Instr::CsrRw { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    shared.csr.write(csr, local.x.read(rs1 as usize));

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRs { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    if rs1 != 0 {
                        shared.csr.write(csr, t | local.x.read(rs1 as usize));
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRc { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    if rs1 != 0 {
                        shared.csr.write(csr, t & !local.x.read(rs1 as usize));
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRwi { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    if rs1 != 0 {
                        shared.csr.write(csr, (rs1 & 0x1f) as _);
                    }

                    local.x.write(rd as usize, t);
                }


                Instr::CsrRsi { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    if rs1 != 0 {
                        shared.csr.write(csr, t | rs1 as u64);
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRci { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as usize;
                    let t = shared.csr.read(csr);

                    if rs1 != 0 {
                        shared.csr.write(csr, t & !(rs1 as u64));
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::SECall { } => {
                    core::hint::cold_path();
                    //println!("ecall");
                    let cause = match local.mode {
                        Priv::User        => EXC_ECALL_UMODE,
                        Priv::Supervisor  => EXC_ECALL_SMODE,
                        Priv::Machine     => EXC_ECALL_MMODE,
                    };


                    let num = local.x.read(17);
                    if num == 93 {
                        break;
                    }

                    code = local.trap(shared, code, cause, 0);
                    continue;
                },


                Instr::SEBreak { } => {
                    core::hint::cold_path();
                    code = local.trap(shared, code, EXC_BREAKPOINT, local.cache.pc(code));
                    continue;
                },


                Instr::SSRet { } => {
                    core::hint::cold_path();
                    code = local.ret(shared, code, CSR_SSTATUS, CSR_SEPC, 1, 5, 8, 1);
                    continue;
                },


                Instr::SMRet { } => {
                    core::hint::cold_path();
                    code = local.ret(shared, code, CSR_MSTATUS, CSR_MEPC, 3, 7, 11, 2);
                    continue;
                },


                Instr::SWfi { } => {
                    core::hint::cold_path();
                    let mstatus = shared.csr.read(CSR_MSTATUS);
                    let mie = (mstatus >> 3) & 1; // MIE bit in mstatus

                    if mie != 0 {
                        while (shared.csr.read(CSR_MIE) & shared.csr.read(CSR_MIP)) == 0 {
                            cycle += 1;
                            if core::hint::unlikely(cycle % 128 == 0) {
                                shared.csr.write(CSR_CYCLE, cycle);
                                let (new_code, cont) = local.tick(cycle, shared, code);
                                if !cont { return false }

                                if let Some(new_code) = new_code {
                                    code = new_code;
                                    break;
                                }
                            }
                        }
                    }
                },


                Instr::LB { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u8(ptr) as i8 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LBu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u8(ptr) as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LH { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u16(ptr) as i16 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LHu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u16(ptr) as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LW { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u32(ptr) as i32 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LWu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u32(ptr) as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LD { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let result = shared.mem.read_u64(ptr);

                    local.x.write(rd as usize, result);
                },

                Instr::SB { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &[(local.x.read(rs2 as usize) & 0xFF) as u8];

                    shared.mem.write(local.mode, ptr, slice);
                },


                Instr::SH { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &((local.x.read(rs2 as usize) & 0xFFFF) as u16).to_ne_bytes();

                    shared.mem.write(local.mode, ptr, slice);
                },


                Instr::SW { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &((local.x.read(rs2 as usize) & 0xFFFF_FFFF) as u32).to_ne_bytes();

                    shared.mem.write(local.mode, ptr, slice);
                },


                Instr::SD { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = Ptr(ptr);

                    let slice = &local.x.read(rs2 as usize).to_ne_bytes();

                    shared.mem.write(local.mode, ptr, slice);
                },


                Instr::BEq { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs == rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::BNe { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs != rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::BLt { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;

                    let cond = lhs < rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::BLtu { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs < rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::BGe { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;

                    let cond = lhs >= rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::BGeu { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs >= rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, pc.wrapping_add(imm as u64)).0;
                        continue;
                    }
                },


                Instr::JAlr { rd, rs1, imm } => {
                    let pc = local.cache.pc(code);
                    let t = pc + 4;
                    let (new_cptr, status) = local.set_pc(shared, code, local.x.read(rs1 as usize).wrapping_add(imm as u64) & (!1));
                    if status {
                        local.x.write(rd as usize, t);
                    }

                    code = new_cptr;
                    continue;
                },

                Instr::Nop => (),


                Instr::FenceI => {
                    let pc = local.cache.pc(code);
                    local.cache.invalidate();
                    code = local.cache.set_pc(&shared.mem, pc);
                }

                Instr::Unknown => {
                    code = local.trap(shared, code, EXC_ILLEGAL_INSTRUCTION, shared.mem.read_u32(Ptr(local.cache.pc(code))) as u64);
                    continue;
                },


                Instr::Readjust => {
                    let pc = local.cache.pc(code);
                    code = local.set_pc(shared, code, pc).0;
                    continue;
                },
            }


            // finish
            code.next();
        }


        return true;

    }
}


impl Csr {
    pub fn new() -> Self {
        let csr = Self {
            csr: [const { AtomicU64::new(0) }; _],
        };
        
        // mstatus.UXL = 2 (64-bit user-mode XLEN)
        let mstatus_init = 2u64 << 32;
        csr.csr[CSR_MSTATUS].store(mstatus_init, Ordering::Release);

        csr

    }


    #[inline(always)]
    pub fn read(&self, csr: usize) -> u64 {
        match csr {
            CSR_SSTATUS => {
                let mstatus = self.csr[CSR_MSTATUS].load(std::sync::atomic::Ordering::Relaxed);

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

            CSR_SIP => {
                let mip     = self.csr[CSR_MIP].load(Ordering::Relaxed);
                let mideleg = self.csr[CSR_MIDELEG].load(Ordering::Relaxed);

                // Only delegated bits from MIP are visible in SIP
                mip & mideleg
            }

            
            CSR_TSELECT => {
                // "we have no triggers"
                1
            }


            CSR_MISA => {
                2 << 62
                | 1 << 8
                | 1 << 12
                | 1 << 18
                | 1 << 20
            }

            _ => self.csr[csr].load(std::sync::atomic::Ordering::Relaxed),
        }
    }


    pub fn write(&self, csr: usize, value: u64) {
        match csr {
            CSR_SSTATUS => {
                // Updates to SSTATUS must write into MSTATUS
                let mut mstatus = self.csr[CSR_MSTATUS].load(std::sync::atomic::Ordering::Relaxed);

                // Mask writable fields (SIE=1, SPIE=5, SPP=8)
                let sie  = (value >> 1) & 1;
                let spie = (value >> 5) & 1;
                let spp  = (value >> 8) & 1;

                mstatus = (mstatus & !(1 << 1)) | (sie << 1);
                mstatus = (mstatus & !(1 << 5)) | (spie << 5);
                mstatus = (mstatus & !(1 << 8)) | (spp << 8);

                self.csr[CSR_MSTATUS].store(mstatus, std::sync::atomic::Ordering::Relaxed);
            }

            CSR_MSTATUS => {
                // Bits software is allowed to affect
                let writable = (1 << 3)      // MIE
                             | (1 << 7)      // MPIE
                             | (3 << 11);    // MPP

                let mut new = self.csr[CSR_MSTATUS].load(Ordering::Relaxed);

                // Update only writable bits from 'value'
                new = (new & !writable) | (value & writable);

                // Hard-wire UXL = 2 (64-bit)
                new = (new & !(3 << 32)) | (2u64 << 32);

                self.csr[CSR_MSTATUS].store(new, Ordering::Relaxed);

            }

            CSR_MTVEC => {
                // Low 2 bits must be 0 (direct) or 1 (vectored)
                let mode = value & 0b11;
                let base = value & !0b11;
                self.csr[CSR_MTVEC].store(base | mode.min(1), std::sync::atomic::Ordering::Relaxed);
            }

            CSR_SIP => {
                let mut mip     = self.csr[CSR_MIP].load(Ordering::Relaxed);
                let mideleg = self.csr[CSR_MIDELEG].load(Ordering::Relaxed);

                // Only SSIP (bit 1) is writable via SIP
                let mask = 1 << 1;

                // Only writable if delegated to S-mode
                let writable = mask & mideleg;

                mip = (mip & !writable) | (value & writable);

                self.csr[CSR_MIP].store(mip, Ordering::Relaxed);
            }

            CSR_MIP => {
                // Machine-mode write: only software interrupt bits writable
                let mask = (1 << 1) | (1 << 5) | (1 << 9);

                self.csr[CSR_MIP].store(
                    (self.csr[CSR_MIP].load(Ordering::Relaxed) & !mask) |
                    (value & mask),
                    Ordering::Relaxed
                );
            }

            CSR_TSELECT | CSR_TDATA1 | CSR_TDATA2 | CSR_TCONTROL => {
                // Ignore writes; triggers unsupported
            }

            CSR_MISA => {
            }

            // Most CSRs just store the raw value
            _ => self.csr[csr].store(value, std::sync::atomic::Ordering::Relaxed),
        }
    }
}


impl Local {
    #[inline(always)]
    #[must_use]
    pub fn set_pc(&mut self, shared: &Shared, code_ptr: CodePtr, pc: u64) -> (CodePtr, bool) {
        if pc & 0b11 != 0 {
            (self.trap(shared, code_ptr, EXC_INSTR_ADDR_MISALIGNED, pc), false)
        } else {
            (self.cache.set_pc(&shared.mem, pc), true)
        }
    }


    fn ret(
        &mut self, shared: &Shared, code_ptr: CodePtr,
        status_csr: usize, epc_csr: usize,
        ie_bit: u32, pie_bit: u32, pp_bit: u32, pp_len: u32
    ) -> CodePtr {
        let mut mstatus = shared.csr.read(status_csr);
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

        shared.csr.write(status_csr, mstatus);
        let epc = shared.csr.read(epc_csr);
        self.set_pc(shared, code_ptr, epc).0
    }


    #[inline(always)]
    pub fn tick(&mut self, cycle: u64, shared: &Shared, code_ptr: CodePtr) -> (Option<CodePtr>, bool) {
        // check interrupts
        let pending = shared.csr.read(CSR_MIP);
        let enabled = shared.csr.read(CSR_MIE);
        let global_enabled = ubfx_64(shared.csr.read(CSR_MSTATUS), 3, 1);


        let mut new_code = None;
        if ((pending & enabled) != 0) && global_enabled == 1 {
            for &code in INTERRUPTS.iter() {
                let mask = 1 << code;
                if (pending & mask != 0) && ((enabled & mask) != 0) {
                    let cause = (1 << 63) | code; // Bit 63 = interrupt
                    new_code = Some(self.trap(shared, code_ptr, cause, 0));
                    break;
                }
            }
        }



        if core::hint::likely(cycle % 1024 != 0) { return (new_code, true) }

        #[cold]
        fn cold(local: &mut Local, shared: &Shared) -> bool {
            let mut mtime = shared.mem.read_u64(Ptr(MTIME as _));

            const MTIME_TICK_NS: u64 = 100; // 10MHz
            let elapsed = local.start.elapsed();

            if core::hint::unlikely(elapsed.as_millis() as u64 > shared.timeout_ms) { return false }

            let now = elapsed.as_nanos() as u64;
            let dt = now - local.last_ns;

            let div = dt / MTIME_TICK_NS;
            let rem = dt % MTIME_TICK_NS;

            mtime = mtime.wrapping_add(div as u64);

            local.last_ns = now - rem;
            shared.mem.write(local.mode, Ptr(MTIME as _), &mtime.to_ne_bytes());

            let bit = mtime >= shared.mem.read_u64(Ptr(MTIMECMP as _));
            let bit = bit as u64;
            let mip = bfi_64(shared.csr.read(CSR_MIP), 7, 1, bit);
            shared.csr.write(CSR_MIP, mip);
            true
        }

        (new_code, cold(self, shared))
    }


    pub fn trap(&mut self, shared: &Shared, code_ptr: CodePtr, cause: u64, tval: u64) -> CodePtr {
        let medeleg = shared.csr.read(CSR_MEDELEG);
        let mideleg = shared.csr.read(CSR_MIDELEG);

        let interrupt = (cause >> 63) != 0;
        let code = cause & 0x7FFF_FFFF_FFFF_FFFF;

        if interrupt {
            if code == INT_EXT_M as u64 || code == INT_EXT_S as u64 {
                let mut mip = shared.csr.read(CSR_MIP);
                mip &= !(1 << code);
                shared.csr.write(CSR_MIP, mip);
            }
        }


        let delegated_to_s = if interrupt {
            ((mideleg >> code) & 1) != 0
        } else {
            ((medeleg >> code) & 1) != 0
        };


        if delegated_to_s {
            return self._trap(
                shared, code_ptr,
                CSR_SSTATUS, CSR_SEPC, CSR_SCAUSE, CSR_STVAL, CSR_STVEC,
                1, 5, 8, 1, Priv::Supervisor, cause, tval
            )
        }

        self._trap(
            shared, code_ptr,
            CSR_MSTATUS, CSR_MEPC, CSR_MCAUSE, CSR_MTVAL, CSR_MTVEC,
            3, 7, 11, 2, Priv::Machine, cause, tval)
    }


    fn _trap(
        &mut self,
        shared: &Shared,
        code_ptr: CodePtr,

        status_csr: usize, epc_csr: usize, cause_csr: usize,
        tval_csr: usize, tvec_csr: usize,
        ie_bit: u32, pie_bit: u32, pp_bit: u32, pp_len: u32,
        new_mode: Priv, cause: u64, tval: u64
    ) -> CodePtr {
        let interrupt = (cause >> 63) & 1;
        let code  = cause & 0x7FFF_FFFF_FFFF_FFFF;

        // real mcause value
        let mcause = (interrupt << 63) | code;

        // read & update status
        let mut status = shared.csr.read(status_csr);
        let ie = ubfx_64(status, ie_bit, 1);

        status = bfi_64(status, pie_bit, 1, ie);
        status = bfi_64(status, ie_bit, 1, 0);
        status = bfi_64(status, pp_bit, pp_len, self.mode as u64);

        shared.csr.write(status_csr, status);
        shared.csr.write(epc_csr, self.cache.pc(code_ptr));
        shared.csr.write(cause_csr, mcause);
        shared.csr.write(tval_csr, tval);

        // switch privilege
        self.mode = new_mode;

        // redirect PC
        let tvec = shared.csr.read(tvec_csr);
        let base = tvec & !0b11;
        let tmode = tvec & 0b11;

        let pc = 
            if tmode == 1 && interrupt != 0 {
                base + 4 * (code & 0x1F)
            } else {
                base
            };

        self.set_pc(shared, code_ptr, pc).0
    }




}


pub struct Regs {
    pub regs: [u64; 32]
}


impl Regs {
    pub fn new() -> Self {
        Self {
            regs: [0; 32],
        }
    }


    #[inline(always)]
    pub fn read(&self, idx: usize) -> u64 {
        unsafe { *self.regs.get_unchecked(idx) }
    }


    #[inline(always)]
    pub fn write(&mut self, idx: usize, data: u64) {
        if idx == 0 { return }
        unsafe { *self.regs.get_unchecked_mut(idx) = data; }
    }
}


