pub mod mem;
pub mod utils;

use std::{ops::Rem, process::exit, thread::sleep_ms, time::Instant};

use colourful::ColourBrush;

use crate::{mem::{Memory, Ptr}, utils::{bfi_32, bfi_64, sbfx_32, sbfx_64, ubfx_32, ubfx_64}};

pub struct Emulator {
    mem: Memory,

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


    pub fn tick_timer(&mut self, start: &Instant, last_ns: &mut u128) {
        self.csr[CSR_CYCLE] += 1;

        if self.csr[CSR_CYCLE] % 100 != 0 { return }
        let mut mtime = self.mem.read_u64(Ptr(MTIME as _));

        const MTIME_TICK_NS: u128 = 100; // 10MHz
        let now = start.elapsed().as_nanos();
        let dt = now - *last_ns;

        let div = dt / MTIME_TICK_NS;
        let rem = dt % MTIME_TICK_NS;

        mtime = mtime.wrapping_add(div as u64);

        *last_ns = now - rem;
        self.mem.write(self.mode, Ptr(MTIME as _), &mtime.to_ne_bytes());

        let bit = mtime >= self.mem.read_u64(Ptr(MTIMECMP as _));
        let bit = bit as u64;
        self.csr[CSR_MIP] = bfi_64(self.csr[CSR_MIP], 7, 1, bit);
    }


    pub fn run(&mut self, code: &[u8]) -> bool {
        let start = Instant::now();
        let mut last_ns = 0;

        self.x.write(2, 0xB000_0000);
        self.pc = 0x8000_0000;
        self.mem.write(Priv::Machine, Ptr(MTIMECMP as _), &u64::MAX.to_ne_bytes());

        self.mem.write(self.mode, Ptr(self.pc), code);

        loop {
            self.tick_timer(&start, &mut last_ns);


            // check interrupts
            let pending = self.csr[CSR_MIP];
            let enabled = self.csr[CSR_MIE];
            let global_enabled = ubfx_64(self.csr[CSR_MSTATUS], 3, 1);


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


            // execute

            //println!("pc: 0x{:x}", self.pc);
            let instr = self.mem.read_u32(Ptr(self.pc));
            let opcode = ubfx_32(instr, 0, 7);

            macro_rules! unknown_opcode {
                () => {{
                    self.trap(2, instr as u64);
                    continue;
                }};
            }

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
                            let funct6 = ubfx_32(instr, 26, 6);

                            match funct6 {
                                // srli
                                0b000000 => {
                                    src >> shamt
                                }

                                // srai
                                0b010000 => {
                                    (src as i64 >> shamt) as u64
                                },

                                _ => unknown_opcode!(),
                            }
                        }

                        _ => unknown_opcode!(),
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
                            let shamt = ubfx_32(instr, 20, 5);
                            (rs1 << shamt) as u32
                        }


                        // srliw
                        0b101 if funct7 == 0b00000_00 => {
                            let shamt = ubfx_32(instr, 20, 5);
                            rs1 as u32 >> shamt
                        }


                        // sraiw
                        0b101 if funct7 == 0b01000_00 => {
                            let shamt = ubfx_32(instr, 20, 5);
                            (rs1 as i32 >> shamt) as u32
                        }


                        _ => unknown_opcode!()
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

                        _ => unknown_opcode!(),
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
                        0b000 if funct7 == 0b0000_000 => {
                            let result = (rs1 as u32).wrapping_add(rs2 as u32);
                            result
                        }

                        // subw
                        0b000 if funct7 == 0b0100_000 => {
                            let result = (rs1 as u32).wrapping_sub(rs2 as u32);
                            result
                        }


                        // sllw
                        0b001 => {
                            let shamt = rs2 & 0x1F;
                            (rs1 << shamt) as u32
                        }

                        // srlw
                        0b101 if funct7 == 0b00000_00  => {
                            let shamt = rs2 & 0x1F;
                            rs1 as u32 >> shamt
                        }

                        // sraw
                        0b101 if funct7 == 0b01000_00 => {
                            let shamt = rs2 & 0x1F;
                            (rs1 as i32 >> shamt) as u32
                        }


                        _ => unknown_opcode!(), 
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
                            //dbg!("gheyy");
                            let funct12 = ubfx_32(instr, 20, 12);
                            //println!("0b{funct12:<012b}");
                            match funct12 {
                                // ecall
                                0b00000_00_00000 => {
                                    //println!("ecall");
                                    let cause = match self.mode {
                                        Priv::User        => 8,
                                        Priv::Supervisor  => 9,
                                        Priv::Machine     => 11,
                                    };


                                    let num = self.x.read(17);
                                    if num == 93 {
                                        break;
                                    } else if num == 0xCC {

                                        let panic_log_len_ptr = Ptr(0xFFFF_F000);
                                        let panic_log_ptr = Ptr(0xFFFF_F010);

                                        let len = self.mem.read_u32(panic_log_len_ptr);
                                        let slice = self.mem.read(panic_log_ptr, len as usize);
                                        let msg = core::str::from_utf8(slice).unwrap();

                                        panic!("{msg}");


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
                                    continue;
                                }


                                // mret
                                0b00110_00_00010 => {
                                    self._ret(CSR_MSTATUS, CSR_MEPC, 3, 7, 11, 2);
                                    continue;
                                }

                                // wfi
                                0b00010_00_00101 => {
                                    while self.csr[CSR_MIE] & self.csr[CSR_MIP] == 0 {
                                        self.tick_timer(&start, &mut last_ns);
                                    }
                                }

                                // sfence.vma
                                _ if ubfx_32(instr, 25, 7) == 0b0001001 => (), 

                                _ => unknown_opcode!(),
                            }
                        }

                        _ => unknown_opcode!(),
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

                        _ => unknown_opcode!()
                    };

                    //println!("0x{:x}", result);
                    self.x.write(rd as usize, result);
                }


                // stores
                0b0100011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let imm5 = ubfx_32(instr, 7, 5);
                    let imm7 = ubfx_32(instr, 25, 7);
                    let imm = (imm7 << 5) | imm5;
                    let imm = sbfx_32(imm, 0, 12);
                    let imm = imm as i32 as i64 as u64;

                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rs2 = ubfx_32(instr, 20, 5) as usize;

                    let ptr = self.x.read(rs1).wrapping_add(imm);
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


                        _ => unknown_opcode!(),
                    }
                }


                // branching
                0b1100011 => {
                    let funct3 = ubfx_32(instr, 12, 3);
                    let rs1 = ubfx_32(instr, 15, 5) as usize;
                    let rs2 = ubfx_32(instr, 20, 5) as usize;
                    let imm =
                        ubfx_32(instr, 7, 1) << 11
                      | ubfx_32(instr, 8, 4) << 1
                      | ubfx_32(instr, 25, 6) << 5
                      | ubfx_32(instr, 31, 1) << 12; // who the fuck wrote this spec bro
                    let imm = sbfx_64(imm as u64, 0, 13);


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


                        _ => unknown_opcode!()
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
                    let imm = sbfx_32(instr, 20, 12) as i32 as i64 as u64;


                    match funct3 {
                        // jalr
                        0b000 => {
                            let t = self.pc + 4;
                            self.pc = self.x.read(rs1).wrapping_add(imm) & (!1);
                            self.x.write(rd, t);
                            continue;
                        }
                        _ => unknown_opcode!(),
                    }
                }


                _ => unknown_opcode!(),
            }

            self.pc += 4;
        }


        return true;
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
