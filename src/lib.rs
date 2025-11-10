pub mod mem;
pub mod utils;

use std::{ops::{Index, IndexMut}, thread::sleep_ms};

use crate::{mem::{Memory, MemoryAuthority, Ptr}, utils::{bfi_32, sbfx_32, sbfx_64, ubfx_32}};

pub struct Emulator {
    mem: Memory,

    x  : Regs,
    pc : u64,
}


impl Emulator {
    pub fn new() -> Self {
        Self {
            mem: Memory::new(),
            x: Regs::new(),
            pc: 0,
        }
    }


    pub fn run(&mut self, code: &[u8]) {
        self.pc = 0x8000_0000;

        self.mem.write(MemoryAuthority::Admin, Ptr(self.pc), code);


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



                _ => panic!("unkown opcode {opcode:b}"),
            }

            self.pc += 4;
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

