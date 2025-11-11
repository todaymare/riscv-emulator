pub mod mem;
pub mod utils;

use std::thread::sleep_ms;

use crate::{mem::{Memory, Ptr}, utils::{bfi_32, bfi_64, sbfx_32, sbfx_64, ubfx_32, ubfx_64}};

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
const CSR_MIE: usize = 0x304;
const CSR_MIP: usize = 0x344;
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
        let interrupt = (cause >> 63) != 0;
        let code = cause & (u64::MAX >> 1);


        let mut status = self.csr[status_csr];
        let ie = ubfx_64(status, ie_bit, 1);

        // xPIE = xIE
        status = bfi_64(status, pie_bit, 1, ie);
        // xIE = 0
        status = bfi_64(status, ie_bit, 1, 0);
        // xPP = previous mode
        status = bfi_64(status, pp_bit, pp_len, self.mode as u64);

        self.csr[status_csr] = status;
        self.csr[epc_csr] = self.pc;
        self.csr[cause_csr] = cause;
        self.csr[tval_csr] = tval;

        // Enter new privilege mode
        self.mode = new_mode;

        // Set PC from trap vector
        let tvec = self.csr[tvec_csr];
        let base = tvec & !0b11;
        let tmode = tvec & 0b11;
        if tmode == 1 && interrupt {
            self.pc = base + 4 * code;
        } else {
            self.pc = base;
        }
    }


    pub fn run(&mut self, code: &[u8]) {
        self.x.write(2, 0xB000_0000);
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

                    self.x.write(dst, result);
                },


                // register-imm arithm pt2
                0b0011011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let funct7 = ubfx_32(instr, 25, 7);
                    let rd  = ubfx_32(instr,  7,  5) as usize;
                    let rs1 = ubfx_32(instr, 15,  5) as usize;
                    let rs1 = self.x.read(rs1 as usize);
                    let imm = sbfx_64(instr as u64, 20, 12);

                    let result = match funct3 {
                        // addiw
                        0b000 => {
                            rs1.wrapping_add(imm as i64 as u64) as u32
                        }

                        // slliw
                        0b001 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            (rs1 << shamt) as u32
                        }


                        // srliw
                        0b101 if funct7 == 0b00000_00 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            rs1 as u32 >> shamt
                        }


                        // sraiw
                        0b101 if funct7 == 0b01000_00 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            (rs1 as i32 >> shamt) as u32
                        }


                        _ => panic!("unknown iinstrpt2")
                    };


                    self.x.write(rd, result as i32 as i64 as u64);


                }


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

                    self.x.write(dst, result);
                }


                // register-register arithm pt2
                0b0111011 => {
                    let funct7 = ubfx_32(instr, 25, 7);
                    let funct3 = ubfx_32(instr, 12, 3);

                    let rs1    = ubfx_32(instr, 15, 5) as usize;
                    let rs2    = ubfx_32(instr, 20, 5) as usize;
                    let rs1    = self.x.read(rs1);
                    let rs2    = self.x.read(rs2);
                    let rd     = ubfx_32(instr,  7, 5) as usize;

                    let result = match funct3 {
                        // addw
                        0b000 if funct7 == 0b00000_00 => {
                            let result = (rs1 as u32).wrapping_add(rs2 as u32);
                            result
                        }

                        // subw
                        0b000 if funct7 == 0b10000_00 => {
                            let result = (rs1 as u32).wrapping_sub(rs2 as u32);
                            result
                        }


                        // sllw
                        0b001 => {
                            let shamt = rs2 & 0x3F;
                            (rs1 << shamt) as u32
                        }

                        // srlw
                        0b101 if funct7 == 0b00000_00  => {
                            let shamt = rs2 & 0x3F;
                            rs1 as u32 >> shamt
                        }

                        // sraiw
                        0b101 if funct7 == 0b01000_00 => {
                            let shamt = ubfx_32(instr, 20, 6) & 0x3F;
                            (rs1 as i32 >> shamt) as u32
                        }


                        _ => panic!("unkown rinstrpt2")
                    };


                    self.x.write(rd, result as i32 as i64 as u64);
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


                        0b000 => {
                            let funct12 = ubfx_32(instr, 20, 12);
                            match funct12 {
                                // ecall
                                0b00000_00_00000 => {
                                    let cause = match self.mode {
                                        Priv::User        => 8,
                                        Priv::Supervisor  => 9,
                                        Priv::Machine     => 11,
                                    };


                                    let num = self.x.read(17);
                                    if num == 93 {
                                        println!("terminating");
                                        for x in 0..32 {
                                            println!("x{x} - {}({:x})", self.x.read(x), self.x.read(x));
                                        }

                                        println!("CYCLE COUNT: {}", self.csr[CSR_CYCLE]);
                                        break;
                                    }

                                    self.trap(cause, 0);
                                    continue;
                                }

                                // ebreak
                                0b00000_00_00001 => {
                                    self.trap(3, self.pc);
                                    continue;
                                }

                                // sret
                                0b00010_00_00010 => {
                                    self._ret(CSR_SSTATUS, CSR_SEPC, 1, 5, 8, 1);
                                }


                                // mret
                                0b00110_00_00010 => {
                                    self._ret(CSR_MSTATUS, CSR_MEPC, 3, 7, 11, 2);
                                }

                                // wfi
                                0b00010_00_00101 => {
                                    while self.csr[CSR_MIE] & self.csr[CSR_MIP] == 0 {}
                                }

                                // sfence.vma
                                _ if ubfx_32(instr, 25, 7) == 0b0001001 => {},

                                _ => panic!("unknown system instruction"),
                            }
                        }

                        _ => panic!("invalid csr instruction"),
                    };


                    self.x.write(rd, t);
                }


                // loads
                0b0000011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let rd = ubfx_32(instr, 7, 5);
                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rs1 = self.x.read(rs1);
                    let offset = sbfx_32(instr, 20, 12) as i32 as i64;

                    let ptr = (rs1 as i64 + offset) as u64;
                    let ptr = Ptr(ptr);

                    let result = match funct3 {
                        // lb
                        0b000 => {
                            let byte = self.mem.read(ptr, 1)[0];

                            byte as i8 as i64 as u64
                        }

                        // lh
                        0b001 => {
                            let byte = self.mem.read(ptr, 2);
                            let value = i16::from_ne_bytes(byte.try_into().unwrap());

                            value as i64 as u64
                        }

                        // lw
                        0b010 => {
                            let byte = self.mem.read(ptr, 4);
                            let value = i32::from_ne_bytes(byte.try_into().unwrap());

                            value as i64 as u64
                        }

                        // lbu
                        0b100 => {
                            let byte = self.mem.read(ptr, 1)[0];

                            byte as u64
                        }

                        // lhu
                        0b101 => {
                            let byte = self.mem.read(ptr, 2);
                            let value = u16::from_ne_bytes(byte.try_into().unwrap());

                            value as u64
                        }

                        // lwu
                        0b110 => {
                            let byte = self.mem.read(ptr, 4);
                            let value = u32::from_ne_bytes(byte.try_into().unwrap());

                            value as u64
                        }


                        // ld
                        0b011 => {
                            let byte = self.mem.read(ptr, 8);
                            let value = u64::from_ne_bytes(byte.try_into().unwrap());

                            value
                        }

                        _ => panic!("unkown load")
                    };

                    self.x.write(rd as usize, result);
                }


                // stores
                0b0100011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let imm5 = ubfx_32(instr, 7, 5);
                    let imm7 = ubfx_32(instr, 25, 7);
                    let imm = (imm7 << 5) | imm5;
                    let imm = sbfx_32(imm, 0, 20);

                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rs2 = ubfx_32(instr, 20, 5) as usize;

                    let ptr = self.x.read(rs1).wrapping_add(imm as i32 as i64 as u64);
                    let ptr = Ptr(ptr);

                    match funct3 {
                        // sb
                        0b000 => {
                            self.mem.write(self.mode, ptr, &[(self.x.read(rs2) & 0xFF) as u8]);
                        }


                        // sh
                        0b001 => {
                            self.mem.write(self.mode, ptr, &((self.x.read(rs2) & 0xFFFF) as u16).to_ne_bytes());
                        }


                        // sw
                        0b010 => {
                            self.mem.write(self.mode, ptr, &((self.x.read(rs2) & 0xFFFF_FFFF) as u32).to_ne_bytes());
                        }


                        // sd
                        0b011 => {
                            self.mem.write(self.mode, ptr, &self.x.read(rs2).to_ne_bytes());
                        }


                        _ => panic!("unkown store"),
                    }
                }


                // branching
                0b1100011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rs2 = ubfx_32(instr, 20, 5) as usize;
                    let imm =
                        ubfx_32(instr, 7, 1) << 11
                      | ubfx_32(instr, 8, 4)
                      | ubfx_32(instr, 25, 6) << 5
                      | ubfx_32(instr, 31, 1) << 12; // who the fuck wrote this spec bro
                    let imm = sbfx_64((imm << 1) as u64, 0, 13);


                    let cond = match funct3 {
                        // beq
                        0b000 => {
                            let rs1 = self.x.read(rs1);
                            let rs2 = self.x.read(rs2);

                            rs1 == rs2
                        }

                        // bne
                        0b001 => {
                            let rs1 = self.x.read(rs1);
                            let rs2 = self.x.read(rs2);

                            rs1 != rs2
                        }

                        // blt
                        0b100 => {
                            let rs1 = self.x.read(rs1) as i64;
                            let rs2 = self.x.read(rs2) as i64;

                            rs1 < rs2
                        }


                        // bge
                        0b101 => {
                            let rs1 = self.x.read(rs1) as i64;
                            let rs2 = self.x.read(rs2) as i64;

                            rs1 >= rs2
                        }

                        // bltu
                        0b110 => {
                            let rs1 = self.x.read(rs1);
                            let rs2 = self.x.read(rs2);

                            rs1 < rs2
                        }

                        // bgeu
                        0b111 => {
                            let rs1 = self.x.read(rs1);
                            let rs2 = self.x.read(rs2);

                            rs1 >= rs2
                        }


                        _ => panic!("unknown branch")
                    };

                    if cond {
                        self.pc = self.pc.wrapping_add(imm);
                        continue;
                    }
                }



                // jumps
                0b1100111 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rd = ubfx_32(instr, 7, 5) as usize;
                    let imm = sbfx_32(instr, 20, 12) as i64 as u64;


                    match funct3 {
                        // jalr
                        0b000 => {
                            let t = self.pc + 4;
                            self.pc = self.x.read(rs1).wrapping_add(imm) & (!1);
                            self.x.write(rd, t);
                            continue;
                        }
                        _ => panic!("unknown jump"),
                    }
                }


                _ => panic!("unkown opcode {opcode:b}"),
            }

            self.pc += 4;
            self.csr[CSR_CYCLE] += 1;
            //sleep_ms(100);
        }
    }


    fn _ret(&mut self, status: usize, epc: usize, ie_bit: u32, pie_bit: u32, pp_bit: u32, pp_len: u32) {
        let mut mstatus = self.csr[status];
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

        self.csr[status] = mstatus;
        self.pc = self.csr[epc];
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

