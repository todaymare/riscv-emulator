#![feature(likely_unlikely)]
#![feature(array_ptr_get)]
#![feature(cold_path)]
#![forbid(unused_must_use)]
pub mod mem;
pub mod utils;
pub mod instrs;

use std::{hint::cold_path, mem::discriminant, ops::{Deref, Range, Rem}, process::exit, ptr::null, sync::{atomic::{AtomicU64, Ordering}, Arc, Mutex}, thread::sleep_ms, time::Instant};

use colourful::ColourBrush;
use elf::abi::SHT_STRTAB;

use crate::{instrs::{CodePtr, Instr, InstrCache}, mem::{Memory, PhysPtr, VirtPtr}, utils::{bfi_32, bfi_64, sbfx_32, sbfx_64, ubfx_32, ubfx_64}};

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

    pub to_host: Option<u64>,
    pub cpu_string: Option<u64>,
    pub sig: Option<(Range<u64>, Box<[u8]>)>,
    pub initial_pc: u64,
}


pub struct Csr {
    csr: [AtomicU64; 4096],
}


unsafe impl Send for Emulator {}
unsafe impl Sync for Emulator {}


#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
pub const CSR_SATP : usize = 0x180;


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
                to_host: None,
                cpu_string: None,
                initial_pc: 0,
                sig: None,
            }),
        }
    }



    pub fn run(&self) -> bool {
        let shared = &self.shared;
        let mut local = self.local.lock().unwrap();
        let local = &mut *local;
        let mut cycle = 0;
        local.start = Instant::now();
        local.last_ns = 0;

        shared.mem.write(PhysPtr(MTIMECMP as _), &u64::MAX.to_ne_bytes());

        let mut code = local.cache.set_pc(&shared.mem, VirtPtr(local.initial_pc), PhysPtr(local.initial_pc));

        let result = 'result: loop {
            cycle += 1;
            if core::hint::unlikely(cycle % 128 == 0) {
                shared.csr.write(CSR_CYCLE, cycle);
                let (new_code, cont) = local.tick(cycle, shared, code);
                if !cont { break 'result false }

                if let Some(new_code) = new_code {
                    code = new_code;
                }
            }




            // decode
            let instr = code.get();
            //println!("pc: 0x{:x?}, instr: {instr:?}", local.cache.pc(code));

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
                    let (new_cptr, success) = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(offset as i32 as i64 as u64)));
                    if success {
                        local.x.write(rd as usize, pc.0.wrapping_add(4));
                    }

                    code = new_cptr;

                    continue;
                },


                Instr::Auipc { rd, offset } => {
                    local.x.write(rd as usize, local.cache.pc(code).0.wrapping_add(offset as i64 as u64));
                },


                Instr::Lui { rd, imm } => {
                    local.x.write(rd as usize, imm as i64 as u64);
                },


                Instr::CsrRw { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };


                    let value = local.x.read(rs1 as usize);
                    if let Err(e) = local.write_csr(shared, code, csr, value) {
                        code = e;
                        continue;
                    }



                    local.x.write(rd as usize, t);
                },


                Instr::CsrRs { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };



                    if rs1 != 0 {
                        let value = t | local.x.read(rs1 as usize);
                        if let Err(e) = local.write_csr(shared, code, csr, value) {
                            code = e;
                            continue;
                        }
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRc { rd, rs1, csr } => {
                    core::hint::cold_path();
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };


                    if rs1 != 0 {
                        let value = t & !local.x.read(rs1 as usize);
                        if let Err(e) = local.write_csr(shared, code, csr, value) {
                            code = e;
                            continue;
                        }
                    }



                    local.x.write(rd as usize, t);
                },


                Instr::CsrRwi { rd, rs1, csr } => {
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };



                    if rs1 != 0 {
                        let value = (rs1 & 0x1f) as _;
                        if let Err(e) = local.write_csr(shared, code, csr, value) {
                            code = e;
                            continue;
                        }
                    }

                    local.x.write(rd as usize, t);
                }


                Instr::CsrRsi { rd, rs1, csr } => {
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };


                    if rs1 != 0 {
                        let value = t | rs1 as u64;
                        if let Err(e) = local.write_csr(shared, code, csr, value) {
                            code = e;
                            continue;
                        }
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::CsrRci { rd, rs1, csr } => {
                    let csr = csr as u64;
                    let t = match local.read_csr(shared, code, cycle, csr) {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };




                    if rs1 != 0 {
                        let value = t & !(rs1 as u64);

                        if let Err(e) = local.write_csr(shared, code, csr, value) {
                            code = e;
                            continue;
                        }
                    }

                    local.x.write(rd as usize, t);
                },


                Instr::SECall { } => {
                    core::hint::cold_path();
                    let cause = match local.mode {
                        Priv::User        => EXC_ECALL_UMODE,
                        Priv::Supervisor  => EXC_ECALL_SMODE,
                        Priv::Machine     => EXC_ECALL_MMODE,
                    };


                    #[cfg(not(feature = "test-harness"))]
                    {
                        let num = local.x.read(17);
                        if num == 93 {
                            break true;
                        }
                    }

                    code = local.trap(shared, code, cause, 0);
                    continue;
                },


                Instr::SEBreak { } => {
                    core::hint::cold_path();
                    code = local.trap(shared, code, EXC_BREAKPOINT, local.cache.pc(code).0);
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

                    //println!("wfi");
                    if mie != 0 {
                        while (shared.csr.read(CSR_MIE) & shared.csr.read(CSR_MIP)) == 0 {
                            cycle += 1;
                            if core::hint::unlikely(cycle % 128 == 0) {
                                shared.csr.write(CSR_CYCLE, cycle);
                                let (new_code, cont) = local.tick(cycle, shared, code);
                                if !cont { break 'result false; }

                                if let Some(new_code) = new_code {
                                    code = new_code;
                                    break;
                                }
                            }
                        }
                    }

                    //println!("wfi ovr");
                },


                Instr::LB { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u8(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };

                    let result = result as i8 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LBu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u8(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };

                    local.x.write(rd as usize, result as u64);
                },


                Instr::LH { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u16(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };


                    let result = result as i16 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LHu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u16(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };

                    let result = result as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LW { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u32(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };


                    let result = result as i32 as i64 as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LWu { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u32(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };

                    let result = result as u64;

                    local.x.write(rd as usize, result);
                },


                Instr::LD { rd, rs1, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let result = local.read_u64(shared, code, AccessType::Load, ptr);
                    let result = match result {
                        Ok(v) => v,
                        Err(v) => {
                            code = v;
                            continue;
                        },
                    };

                    local.x.write(rd as usize, result);
                },

                Instr::SB { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let slice = &[(local.x.read(rs2 as usize) & 0xFF) as u8];

                    if let Err(e) = local.write(shared, code, ptr, slice) {
                        code = e;
                        continue;
                    }
                },


                Instr::SH { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);

                    let slice = &((local.x.read(rs2 as usize) & 0xFFFF) as u16).to_ne_bytes();

                    if let Err(e) = local.write(shared, code, ptr, slice) {
                        code = e;
                        continue;
                    }
                },


                Instr::SW { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);
                    let rs2 = local.x.read(rs2 as usize) & 0xFFFF_FFFF;
                    let slice = &(rs2 as u32).to_ne_bytes();

                    //println!("host is 0x{:x?} writing to 0x{:x} 0x{:x?} rs2 is {rs2:b}", local.to_host, ptr.0, phys_ptr);
                    #[cfg(feature = "test-harness")]
                    if let Ok(ptr) = local.mmu_translate(shared, code, AccessType::Load, ptr)
                        && let Some(tohost) = local.to_host
                        && core::hint::unlikely(ptr.0 == tohost) {

                        cold_path();

                        if local.sig.is_some() {
                            let sig = local.sig.as_ref().unwrap();
                            let mem_sig = shared.mem.read(ptr, (sig.0.end - sig.0.start) as usize);

                            if mem_sig == &*sig.1 {
                                local.x.write(10, 0);
                            } else {
                                local.x.write(10, 1);
                            }
                        } else {
                            local.x.write(10, rs2 >> 1);
                        }


                        break 'result true;
                    }


                    if let Err(e) = local.write(shared, code, ptr, slice) {
                        code = e;
                        continue;
                    }

                },


                Instr::SD { rs1, rs2, offset } => {
                    let ptr = (local.x.read(rs1 as usize) as i64 + offset as i64) as u64;
                    let ptr = VirtPtr(ptr);
                    //println!("storing at 0x{:x}", ptr.0);

                    let slice = &local.x.read(rs2 as usize).to_ne_bytes();

                    if let Err(e) = local.write(shared, code, ptr, slice) {
                        code = e;
                        continue;
                    }

                },


                Instr::BEq { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs == rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::BNe { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs != rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::BLt { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;

                    let cond = lhs < rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::BLtu { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs < rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::BGe { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize) as i64;
                    let rhs = local.x.read(rs2 as usize) as i64;

                    let cond = lhs >= rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::BGeu { rs1, rs2, imm } => {
                    let lhs = local.x.read(rs1 as usize);
                    let rhs = local.x.read(rs2 as usize);

                    let cond = lhs >= rhs;

                    if cond {
                        let pc = local.cache.pc(code);
                        code = local.set_pc(shared, code, VirtPtr(pc.0.wrapping_add(imm as u64))).0;
                        continue;
                    }
                },


                Instr::JAlr { rd, rs1, imm } => {
                    let pc = local.cache.pc(code);
                    let t = pc.0 + 4;
                    let (new_cptr, status) = local.set_pc(shared, code, VirtPtr(local.x.read(rs1 as usize).wrapping_add(imm as u64) & (!1)));
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
                    code = local.set_pc(shared, code, pc).0;
                }


                Instr::Fence => {}


                Instr::FenceVMA => {
                }


                Instr::Unknown => {
                    code = local.trap(shared, code, EXC_ILLEGAL_INSTRUCTION, local.cache.code(code) as u64);
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
        };


        shared.csr.write(CSR_CYCLE, cycle);


        result

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


    pub fn read_interp(&self, csr: usize, cycle: u64) -> u64 {
        match csr {
            CSR_SSTATUS => {
                let m = self.csr[CSR_MSTATUS].load(Ordering::Relaxed);

                let mut sstatus = 0;

                // SIE/SPIE/SPP come from mstatus
                sstatus |= (m >> 1) & 1 << 1;   // SIE
                sstatus |= (m >> 5) & 1 << 5;   // SPIE
                sstatus |= (m >> 8) & 1 << 8;   // SPP

                // FS = 0, XS = 0
                // SUM, MXR = 0

                // UXL field (should equal MSTATUS.UXL)
                sstatus |= (m & (3 << 32));

                // SD = 0 because FS=XS=0

                sstatus
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
                (2u64 << 62)   |    // RV64 (MXL = 2)
                (1 << 8)       |    // I extension
                (1 << 12)           // M extension
            }


            CSR_CYCLE => cycle,

            _ => self.csr[csr].load(std::sync::atomic::Ordering::Relaxed),
        }
    }


    #[inline(always)]
    pub fn read(&self, csr: usize) -> u64 {
        match csr {
            CSR_CYCLE => self.csr[csr].load(Ordering::Relaxed),
            _ => self.read_interp(csr, 0),
        }

    }


    pub fn write(&self, csr: usize, value: u64) {
        match csr {
            CSR_SSTATUS => {

                let mut m = self.csr[CSR_MSTATUS].load(Ordering::Relaxed);

                let sie  = (value >> 1) & 1;
                let spie = (value >> 5) & 1;
                let spp  = (value >> 8) & 1;

                // write into mstatus
                m = (m & !(1 << 1)) | (sie << 1);
                m = (m & !(1 << 5)) | (spie << 5);
                m = (m & !(1 << 8)) | (spp << 8);

                self.csr[CSR_MSTATUS].store(m, Ordering::Relaxed);
            }

            CSR_MSTATUS => {
                let old = self.csr[CSR_MSTATUS].load(Ordering::Relaxed);
                let mut new = old;

                // Writable bits: MIE, MPIE, MPP (WARL)
                // MPP legal values: 0, 1, 3
                let mie  = (value >> 3) & 1;
                let mpie = (value >> 7) & 1;
                let mut mpp  = (value >> 11) & 0b11;
                let mprv = (value >> 17) & 1;

                // enforce WARL for MPP
                if mpp == 2 { mpp = 0; }

                new = (new & !(1 << 3))  | (mie << 3);
                new = (new & !(1 << 7))  | (mpie << 7);
                new = (new & !(3 << 11)) | (mpp << 11);
                new = (new & !(1 << 17)) | (mprv << 17);

                // Hardwire UXL = 2 (RV64)
                new = (new & !(3 << 32)) | (2u64 << 32);

                // Hardwire SXL = 2 for S-mode
                new = (new & !(3 << 34)) | (2u64 << 34);

                // FS, XS, SUM, MXR, SD remain zero

                self.csr[CSR_MSTATUS].store(new, Ordering::Relaxed);

            }

            CSR_SATP => {
                let old = self.csr[CSR_SATP].load(Ordering::Relaxed);

                //
                // Extract components from incoming value
                //
                let mode = (value >> 60) & 0xF;     // MODE field
                let asid = (value >> 44) & 0xFFFF;  // ASID is 16 bits in RV64 (WARL)
                let ppn  = value & ((1 << 44) - 1); // PPN is always 44 bits (WARL)

                //
                // MODE is WARL: legal values = 0 (Bare), 8 (Sv39), 9 (Sv48), 10(Sv57)
                // Anything else is ignored.
                //
                let legal_mode = match mode {
                    0 | 8 | 9 | 10 => mode,
                    _ => (old >> 60) & 0xF,   // keep old
                };

                //
                // Construct new SATP (legal parts only)
                //
                let new =
                    (legal_mode << 60) |       // mode
                    (asid        << 44) |       // truncated ASID
                    (ppn & ((1 << 44) - 1));    // legal 44-bit PPN

                //
                // Commit new value
                //
                self.csr[CSR_SATP].store(new, Ordering::Relaxed);
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
    pub fn read_csr(&mut self, shared: &Shared, code_ptr: CodePtr, cycle: u64, csr: u64) -> Result<u64, CodePtr> {
        let perm = ubfx_64(csr, 8, 2);

        if perm > self.mode as u64 {
            return Err(self.trap(shared, code_ptr, EXC_ILLEGAL_INSTRUCTION, self.cache.code(code_ptr) as _))
        }

        Ok(shared.csr.read_interp(csr as usize, cycle))
    }


    pub fn write_csr(&mut self, shared: &Shared, code_ptr: CodePtr, csr: u64, value: u64) -> Result<(), CodePtr> {
        let perm = ubfx_64(csr, 8, 2);
        let csr_type = ubfx_64(csr, 10, 2);

        if perm > self.mode as u64 || csr_type == 0b01 {
            return Err(self.trap(shared, code_ptr, EXC_ILLEGAL_INSTRUCTION, self.cache.code(code_ptr) as _))
        }


        shared.csr.write(csr as usize, value);
        
        if csr as usize == CSR_SATP {
            println!("writing to satp value os 0b{value:b}");
            let pc = self.cache.pc(code_ptr);
            println!("the vpc is 0x{:x}", pc.0);
            self.cache.invalidate();
            return Err(self.set_pc(shared, code_ptr, VirtPtr(pc.0 + 4)).0)
        }

        Ok(())
    }


    #[inline(always)]
    #[must_use]
    pub fn set_pc(&mut self, shared: &Shared, code_ptr: CodePtr, vpc: VirtPtr) -> (CodePtr, bool) {
        if vpc.0 & 0b11 != 0 {
            return (self.trap(shared, code_ptr, EXC_INSTR_ADDR_MISALIGNED, vpc.0), false)
        }


        let pc = match self.mmu_translate(shared, code_ptr, AccessType::Fetch, vpc) {
            Ok(v) => v,
            Err(v) => {
                return (v, false)
            },
        };


        if Some(pc.0) == self.cpu_string {
            let addr = self.x.read(10);
            let mut addr = self.mmu_translate(shared, code_ptr, AccessType::Load, VirtPtr(addr)).unwrap();

            let mut msg = vec![];

            loop {
                let byte = shared.mem.read_u8(addr);
                if byte == 0 { break };

                msg.push(byte);

                addr.0 += 1;
            }

            panic!("cpustring: {}", str::from_utf8(&msg).unwrap());
            return (code_ptr, true);
        }


        (self.cache.set_pc(&shared.mem, vpc, pc), true)
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
        self.set_pc(shared, code_ptr, VirtPtr(epc)).0
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



        if core::hint::likely(cycle % 256 != 0) { return (new_code, true) }

        #[cold]
        fn cold(local: &mut Local, shared: &Shared) -> bool {
            let mut mtime = shared.mem.read_u64(PhysPtr(MTIME as _));

            const MTIME_TICK_NS: u64 = 100; // 10MHz
            let elapsed = local.start.elapsed();

            if core::hint::unlikely(elapsed.as_millis() as u64 > shared.timeout_ms) { return false }

            let now = elapsed.as_nanos() as u64;
            let dt = now - local.last_ns;

            let div = dt / MTIME_TICK_NS;
            let rem = dt % MTIME_TICK_NS;

            mtime = mtime.wrapping_add(div as u64);

            local.last_ns = now - rem;
            shared.mem.write(PhysPtr(MTIME as _), &mtime.to_ne_bytes());


            let bit = mtime >= shared.mem.read_u64(PhysPtr(MTIMECMP as _));
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
        shared.csr.write(epc_csr, self.cache.pc(code_ptr).0);
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

        self.set_pc(shared, code_ptr, VirtPtr(pc)).0
    }


    pub fn read_u8(&mut self, shared: &Shared, code_ptr: CodePtr, ty: AccessType, ptr: VirtPtr) -> Result<u8, CodePtr> {
        let ptr = self.mmu_translate(shared, code_ptr, ty, ptr)?;
        Ok(shared.mem.read(ptr, 1)[0])
    }


    pub fn read_u16(&mut self, shared: &Shared, code_ptr: CodePtr, ty: AccessType, ptr: VirtPtr) -> Result<u16, CodePtr> {
        let ptr = self.mmu_translate(shared, code_ptr, ty, ptr)?;
        Ok(shared.mem.read_u16(ptr))
    }


    pub fn read_u32(&mut self, shared: &Shared, code_ptr: CodePtr, ty: AccessType, ptr: VirtPtr) -> Result<u32, CodePtr> {
        let ptr = self.mmu_translate(shared, code_ptr, ty, ptr)?;
        Ok(shared.mem.read_u32(ptr))
    }


    pub fn read_u64(&mut self, shared: &Shared, code_ptr: CodePtr, ty: AccessType, ptr: VirtPtr) -> Result<u64, CodePtr> {
        let ptr = self.mmu_translate(shared, code_ptr, ty, ptr)?;
        Ok(shared.mem.read_u64(ptr))
    }


    pub fn write(&mut self, shared: &Shared, code_ptr: CodePtr, ptr: VirtPtr, slice: &[u8]) -> Result<(), CodePtr> {
        let ptr = self.mmu_translate(shared, code_ptr, AccessType::Store, ptr)?;
        shared.mem.write(ptr, slice);
        Ok(())
    }


    pub fn mmu_translate(&mut self, shared: &Shared, code_ptr: CodePtr, ty: AccessType, ptr: VirtPtr) -> Result<PhysPtr, CodePtr> {
        let satp = shared.csr.read(CSR_SATP);
        let mode = ubfx_64(satp, 60, 4);

        if mode == 0 {
            return Ok(PhysPtr(ptr.0));
        }


        if self.mode == Priv::Machine {
            if matches!(ty, AccessType::Fetch) {
                return Ok(PhysPtr(ptr.0));
            }

            let mstatus = shared.csr.read(CSR_MSTATUS);
            let mprv = (mstatus >> 17) & 1 != 0;

            if !mprv {
                return Ok(PhysPtr(ptr.0));
            }

            let mpp = (mstatus >> 11) & 0b11;
            if mpp == 0b11 {
                return Ok(PhysPtr(ptr.0));
            }
        }

        let vpn2 = ubfx_64(ptr.0, 30, 9);
        let vpn1 = ubfx_64(ptr.0, 21, 9);
        let vpn0 = ubfx_64(ptr.0, 12, 9);

        let vpn = [vpn0, vpn1, vpn2];
        let mut ppn = ubfx_64(satp, 0, 44);
        //println!("root addr is 0x{ppn:x}");

        let mut exit_level = 0;
        for level in (0..=2).rev() {
            let pte_addr = (ppn << 12) + vpn[level] * 8;
            //println!("addr is 0x{:x}", pte_addr);
            let mut pte = shared.mem.read_u64(PhysPtr(pte_addr));
            //println!("pte 0b{pte:b}");

            let v = (pte & 1) != 0;
            let r = ((pte >> 1) & 1) != 0;
            let w = ((pte >> 2) & 1) != 0;
            let x = ((pte >> 3) & 1) != 0;

            if !v || (!r && w) {
                return Err(self.trap(shared, code_ptr, ty.fault(), ptr.0))
            }

            ppn = (pte >> 10) & ((1 << 44) - 1);


            if r || x {
                let a = ((pte >> 6) & 1) != 0;
                let d = ((pte >> 7) & 1) != 0;

                if !a {
                    pte |= 1 << 6;
                    shared.mem.write(PhysPtr(pte_addr), &pte.to_ne_bytes());
                }

                if matches!(ty, AccessType::Store) && !d {
                    pte |= 1 << 7;
                    shared.mem.write(PhysPtr(pte_addr), &pte.to_ne_bytes());
                }

                let u = ((pte >> 4) & 1) != 0;
                let sstatus = shared.csr.read(CSR_SSTATUS);
                let mxr = ubfx_64(sstatus, 19, 1) != 0;
                let sum = ubfx_64(sstatus, 18, 1) != 0;
                let cur_priv = self.mode;


                if cur_priv == Priv::User && !u {
                    return Err(self.trap(shared, code_ptr, ty.fault(), ptr.0));
                } 

                if cur_priv == Priv::Supervisor && u && !sum {
                    return Err(self.trap(shared, code_ptr, ty.fault(), ptr.0));
                }


                if ty == AccessType::Load && !(r || (mxr && x)) {
                    return Err(self.trap(shared, code_ptr, EXC_LOAD_PAGE_FAULT, ptr.0));
                }


                if ty == AccessType::Store && !w {
                    return Err(self.trap(shared, code_ptr, EXC_STORE_PAGE_FAULT, ptr.0));
                }


                if ty == AccessType::Fetch && !x {
                    return Err(self.trap(shared, code_ptr, EXC_INSTR_PAGE_FAULT, ptr.0));
                }


                exit_level = level;
                break;
            }
        }

        let offset_masks = [0xFFF, 0x1FFFFF, 0x3FFFFFFF];
        let offset = ptr.0 & offset_masks[exit_level];

        let pa = (ppn << 12) | offset;
        Ok(PhysPtr(pa))
    }




}


#[derive(Clone, Copy, PartialEq, Eq)]
enum AccessType {
    Fetch,
    Load,
    Store,
}


impl AccessType {
    pub fn fault(self) -> u64 {
        match self {
            AccessType::Fetch => EXC_INSTR_PAGE_FAULT,
            AccessType::Load => EXC_LOAD_PAGE_FAULT,
            AccessType::Store => EXC_STORE_PAGE_FAULT,
        }
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

