pub mod mem;
pub mod utils;

use std::{ops::{Index, IndexMut}, thread::sleep_ms};

use crate::{mem::{Memory, Ptr}, utils::{bfi_32, sbfx_32, sbfx_64, ubfx_32}};

pub struct Emulator {
    mem: Memory,

    mode: Priv,
    x  : Regs,
    pc : u64,
    csr: [u64; 4096],
}


#[derive(Debug, Clone, Copy)]
pub enum Priv {
    User = 0,
    Supervisor = 1,
    Machine = 3,
}


const CSR_CYCLE  : usize = 0xC00;
const CSR_MEPC   : usize = 0x341;
const CSR_MCAUSE : usize = 0x342;
const CSR_MTVAL  : usize = 0x343;
const CSR_MTVEC  : usize = 0x305;
const CSR_MSTATUS: usize = 0x300;
const CSR_MIDELEG: usize = 0x303;
const CSR_MEDELEG: usize = 0x302;
const CSR_SEPC: usize = 0x141;
const CSR_SCAUSE: usize = 0x142;
const CSR_STVAL: usize = 0x143;
const CSR_STVEC: usize = 0x105;
const CSR_SSTATUS: usize = 0x100;
const CSR_SSCRATCH: usize = 0x140;
const CSR_SATP: usize = 0x180;


impl Emulator {
    pub fn new() -> Self {
        Self {
            mem: Memory::new(),
            x: Regs::new(),
            pc: 0,
            csr: [0; _],
            mode: Priv::Machine,
            
        }
    }

    pub fn trap(&mut self, cause: u64, tval: u64) {
        let medeleg = self.csr[CSR_MEDELEG];
        let mideleg = self.csr[CSR_MIDELEG];

        let interrupt = (cause >> 63) != 0;
        let code = cause & 0x7FFF_FFFF_FFFF_FFFF;

        let delegated = if interrupt {
            ((mideleg >> code) & 1) != 0
        } else {
            ((medeleg >> code) & 1) != 0
        };

        if delegated {
            // Supervisor trap
            let mut sstatus = self.csr[CSR_SSTATUS];
            let sie = (sstatus >> 1) & 1;
            sstatus = (sstatus & !(1 << 5)) | (sie << 5); // SPIE = SIE
            sstatus &= !(1 << 1);                         // SIE = 0
            let spp = (self.mode as u64) & 1;             // previous privilege (U=0, S=1)
            sstatus = (sstatus & !(1 << 8)) | (spp << 8); // SPP = prev mode
            self.csr[CSR_SSTATUS] = sstatus;

            self.csr[CSR_SEPC] = self.pc;
            self.csr[CSR_SCAUSE] = cause;
            self.csr[CSR_STVAL] = tval;

            self.mode = Priv::Supervisor;

            let stvec = self.csr[CSR_STVEC];
            let base = stvec & !0b11;
            let mode = stvec & 0b11;
            if mode == 1 && interrupt {
                self.pc = base + 4 * code;
            } else {
                self.pc = base;
            }
            return;
        }

        // Machine trap
        let mut mstatus = self.csr[CSR_MSTATUS];
        const MSTATUS_MPP_MASK: u64 = 0b11 << 11;

        let mie = (mstatus >> 3) & 1;
        mstatus = (mstatus & !(1 << 7)) | (mie << 7); // MPIE = MIE
        mstatus &= !(1 << 3);                         // MIE = 0
        mstatus = (mstatus & !MSTATUS_MPP_MASK) | ((self.mode as u64) << 11);
        self.csr[CSR_MSTATUS] = mstatus;

        self.csr[CSR_MEPC] = self.pc;
        self.csr[CSR_MCAUSE] = cause;
        self.csr[CSR_MTVAL] = tval;

        self.mode = Priv::Machine;

        let mtvec = self.csr[CSR_MTVEC];
        let base = mtvec & !0b11;
        let mode = mtvec & 0b11;
        if mode == 1 && interrupt {
            self.pc = base + 4 * code;
        } else {
            self.pc = base;
        }
    }


    pub fn mret(&mut self) {
        let mstatus = self.csr[CSR_MSTATUS];
        let mpp = (mstatus >> 11) & 0b11;

        self.mode = match mpp {
            0 => Priv::User,
            1 => Priv::Supervisor,
            2 => Priv::Machine,

            _ => panic!("INVALID MPP")
        };

        self.csr[CSR_MSTATUS] &= !(0b11 << 11);
        self.pc = self.csr[CSR_MEPC];
    }


    pub fn run(&mut self, code: &[u8]) {
        self.pc = 0x8000_0000;

        self.mem.write(self.mode, Ptr(self.pc), code);


        loop {
            let instr = self.mem.read_u32(Ptr(self.pc));
            let opcode = ubfx_32(instr, 0, 7);

            match opcode {
                // register-imm arithm
                0b0010011 => {
                    let funct = ubfx_32(instr, 12, 3);
                    let dst = ubfx_32(instr,  7,  5) as usize;
                    let src = ubfx_32(instr, 15,  5) as usize;
                    let src = self.x.read(src);
                    let imm = sbfx_64(instr as u64, 20, 12);

                    let result = match funct {
                        // add
                        0b000 => src.wrapping_add(imm),

                        // slti
                        0b010 => ((src as i64) < (imm as i64)) as u64,

                        // sltiu
                        0b011 => (src < imm) as u64,


                        // xori
                        0b100 => src ^ imm,

                        // ori
                        0b110 => src | imm,

                        // andi
                        0b111 => src & imm,


                        // slli
                        0b001 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            src << shamt
                        }


                        0b101 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            let funct7 = ubfx_32(instr, 25, 7);

                            match funct7 {
                                // srli
                                0b0000000 => {
                                    src >> shamt
                                }

                                // srai
                                0b0100000 => {
                                    (src as i64 >> shamt) as u64
                                },

                                _ => panic!("invalid funct value"),
                            }
                        }

                        _ => panic!("unkown iinstr"),
                    };

                    dbg!(src, imm, dst, result);
                    self.x.write(dst, result);
                },


                // register-register arithm
                0b0110011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let funct7 = ubfx_32(instr, 25, 7);
                    let funct = funct7 << 3 | funct3;

                    let rs1    = ubfx_32(instr, 15, 5) as usize;
                    let rs2    = ubfx_32(instr, 20, 5) as usize;
                    let rs1    = self.x.read(rs1);
                    let rs2    = self.x.read(rs2);
                    let dst    = ubfx_32(instr,  7, 5) as usize;


                    let result = match funct {
                        // add
                        0b0000000_000 => rs1.wrapping_add(rs2),

                        // sub
                        0b0100000_000 => rs1.wrapping_sub(rs2),

                        // sll
                        0b0000000_001 => rs1 << (rs2 & 0x3F),

                        // slt
                        0b0000000_010 => ((rs1 as i64) < (rs2 as i64)) as u64,

                        // slut
                        0b0000000_011 => (rs1 < rs2) as u64,

                        // xor
                        0b0000000_100 => rs1 ^ rs2,

                        // srl
                        0b0000000_101 => rs1 >> (rs2 & 0x3F),

                        // sra
                        0b0100000_101 => ((rs1 as i64) >> (rs2 & 0x3F)) as u64,

                        // or
                        0b0000000_110 => rs1 | rs2,

                        // and
                        0b0000000_111 => rs1 & rs2,

                        _ => panic!("unkown rinstr"),
                    };

                    dbg!(rs1, rs2, dst, result as i64);
                    self.x.write(dst, result);
                }


                // jal
                0b1101111 => {
                    let rd = ubfx_32(instr, 7, 5) as usize;

                    let mut offset = 0;

                    offset = bfi_32(offset, 20, 1, ubfx_32(instr, 31, 1));   // imm[20]
                    offset = bfi_32(offset, 12, 8, ubfx_32(instr, 12, 8));   // imm[19:12]
                    offset = bfi_32(offset, 11, 1, ubfx_32(instr, 20, 1));   // imm[11]
                    offset = bfi_32(offset, 1, 10, ubfx_32(instr, 21, 10));  // imm[10:1]
                    offset = sbfx_32(offset, 0, 20);

                    self.x.write(rd, self.pc.wrapping_add(4));
                    self.pc = self.pc.wrapping_add(offset as i32 as u64);
                    continue;
                }


                // lui
                0b0110111 => {
                    let rd = ubfx_32(instr, 7, 5) as usize;
                    let imm = ubfx_32(instr, 12, 20);
                    let imm = ((imm as i32) << 12) as i64 as u64;
                    self.x.write(rd, imm);
                }


                // auipc
                0b0010111 => {
                    let rd = ubfx_32(instr, 7, 5) as usize;
                    let imm = ubfx_32(instr, 12, 20);
                    let imm = ((imm as i32) << 12) as i64 as u64;
                    self.x.write(rd, self.pc.wrapping_add(imm));
                }


                // fence & fence.i
                0b0001111 => {
                    // WHAT THE FUCK IS A FENCE RAAAAA
                }


                0b1110011 => {
                    let csr = ubfx_32(instr, 20, 12) as usize;
                    let funct3 = ubfx_32(instr, 12, 3);
                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rd = ubfx_32(instr, 7, 5) as usize;
                    let t = self.csr[csr];

                    match funct3 {
                        // csrrw
                        0b001 => {
                            self.csr[csr] = self.x.read(rs1);
                        }


                        // csrrs 
                        0b010 => {
                            if rs1 != 0 {
                                self.csr[csr] = t | self.x.read(rs1);
                            }
                        }


                        // csrrc
                        0b011 => {
                            if rs1 != 0 {
                                self.csr[csr] = t & !self.x.read(rs1);
                            }
                        }


                        // csrrwi
                        0b101 => {
                            let zimm = ubfx_32(instr, 15, 5) as u64;
                            self.csr[csr] = zimm;
                        }


                        // csrrsi
                        0b110 => {
                            let zimm = ubfx_32(instr, 15, 5) as u64;
                            if zimm != 0 {
                                self.csr[csr] = t | zimm;
                            }
                        }


                        // csrrci
                        0b111 => {
                            let zimm = ubfx_32(instr, 15, 5) as u64;
                            if zimm != 0 {
                                self.csr[csr] = t & !zimm;
                            }
                        }

                        _ => panic!("invalid csr instruction"),
                    };


                    self.x.write(rd, t);
                }


                0b1110011 => {
                    let funct12 = ubfx_32(instr, 20, 12);
                    match funct12 {
                        // ecall
                        0x000 => {
                            let cause = match self.mode {
                                Priv::User        => 8,
                                Priv::Supervisor  => 9,
                                Priv::Machine     => 11,
                            };

                            self.trap(cause, 0);
                            continue;
                        }

                        // ebreak
                        0x001 => {
                            self.trap(3, 0);
                            continue;
                        }

                        _ => panic!("unknown system instruction"),
                    }
                }



                _ => panic!("unkown opcode {opcode:b}"),
            }

            self.pc += 4;
            self.csr[CSR_CYCLE] += 1;
            sleep_ms(100);
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

