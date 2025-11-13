use std::{collections::HashMap, ptr::null};

use crate::{mem::{Memory, Ptr}, utils::{bfi_32, sbfx_32, sbfx_64, ubfx_32}, Emulator};

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u64)]
pub enum Instr  {
    IAdd    { rd: u8, rs1: u8, imm: i16     },
    ISlt    { rd: u8, rs1: u8, imm: i16     },
    ISltu   { rd: u8, rs1: u8, imm: i16     },
    IXor    { rd: u8, rs1: u8, imm: i16     },
    IOr     { rd: u8, rs1: u8, imm: i16     },
    IAnd    { rd: u8, rs1: u8, imm: i16     },
    ISll    { rd: u8, rs1: u8, imm: i16     },
    ISrl    { rd: u8, rs1: u8, imm: i16     },
    ISra    { rd: u8, rs1: u8, imm: i16     },
    IAddw   { rd: u8, rs1: u8, imm: i16     },
    ISllw   { rd: u8, rs1: u8, imm: i16     },
    ISrlw   { rd: u8, rs1: u8, imm: i16     },
    ISraw   { rd: u8, rs1: u8, imm: i16     },

    RAdd    { rd: u8, rs1: u8, rs2: u8      },
    RSub    { rd: u8, rs1: u8, rs2: u8      },
    RSll    { rd: u8, rs1: u8, rs2: u8      },
    RSlt    { rd: u8, rs1: u8, rs2: u8      },
    RSlut   { rd: u8, rs1: u8, rs2: u8      },
    RXor    { rd: u8, rs1: u8, rs2: u8      },
    RSrl    { rd: u8, rs1: u8, rs2: u8      },
    RSra    { rd: u8, rs1: u8, rs2: u8      },
    ROr     { rd: u8, rs1: u8, rs2: u8      },
    RAnd    { rd: u8, rs1: u8, rs2: u8      },
    RMul    { rd: u8, rs1: u8, rs2: u8      },
    RMulh   { rd: u8, rs1: u8, rs2: u8      },
    RMulhsu { rd: u8, rs1: u8, rs2: u8      },
    RMulhu  { rd: u8, rs1: u8, rs2: u8      },
    RDiv    { rd: u8, rs1: u8, rs2: u8      },
    RDivu   { rd: u8, rs1: u8, rs2: u8      },
    RRem    { rd: u8, rs1: u8, rs2: u8      },
    RRemu   { rd: u8, rs1: u8, rs2: u8      },
    RAddw   { rd: u8, rs1: u8, rs2: u8      },
    RSubw   { rd: u8, rs1: u8, rs2: u8      },
    RMulw   { rd: u8, rs1: u8, rs2: u8      },
    RDivw   { rd: u8, rs1: u8, rs2: u8      },
    RDivuw  { rd: u8, rs1: u8, rs2: u8      },
    RRemw   { rd: u8, rs1: u8, rs2: u8      },
    RRemuw  { rd: u8, rs1: u8, rs2: u8      },
    RSllw   { rd: u8, rs1: u8, rs2: u8      },
    RSrlw   { rd: u8, rs1: u8, rs2: u8      },
    RSraw   { rd: u8, rs1: u8, rs2: u8      },

    Jal     { rd: u8, offset: i32           },
    Auipc   { rd: u8, offset: i32           },
    Lui     { rd: u8, imm: i32              },


    CsrRw   { rd: u8, rs1: u8, csr: u16     },
    CsrRs   { rd: u8, rs1: u8, csr: u16     },
    CsrRc   { rd: u8, rs1: u8, csr: u16     },
    CsrRwi  { rd: u8, rs1: u8, csr: u16     },
    CsrRsi  { rd: u8, rs1: u8, csr: u16     },
    CsrRci  { rd: u8, rs1: u8, csr: u16     },

    SECall  {                               },
    SEBreak {                               },
    SSRet   {                               },
    SMRet   {                               },
    SWfi    {                               },

    LB      { rd: u8, rs1: u8, offset: i16  },
    LBu     { rd: u8, rs1: u8, offset: i16  },
    LH      { rd: u8, rs1: u8, offset: i16  },
    LHu     { rd: u8, rs1: u8, offset: i16  },
    LW      { rd: u8, rs1: u8, offset: i16  },
    LWu     { rd: u8, rs1: u8, offset: i16  },
    LD      { rd: u8, rs1: u8, offset: i16  },


    SB      { rs1: u8, rs2: u8, offset: i16 },
    SH      { rs1: u8, rs2: u8, offset: i16 },
    SW      { rs1: u8, rs2: u8, offset: i16 },
    SD      { rs1: u8, rs2: u8, offset: i16 },


    BEq     { rs1: u8, rs2: u8, imm: i16    },
    BNe     { rs1: u8, rs2: u8, imm: i16    },
    BLt     { rs1: u8, rs2: u8, imm: i16    },
    BLtu    { rs1: u8, rs2: u8, imm: i16    },
    BGe     { rs1: u8, rs2: u8, imm: i16    },
    BGeu    { rs1: u8, rs2: u8, imm: i16    },


    JAlr    { rd: u8, rs1: u8, imm: i16     },

    Nop,
    Unknown,

    Readjust,
}

const PAGE_INSTR_SIZE : u64 = 1024;
const ACTUAL_PAGE_INSTR_SIZE : u64 = PAGE_INSTR_SIZE + 1;
const PAGE_BYTE_SIZE  : u64 = PAGE_INSTR_SIZE * 4;

const PAGE_BITS : u32= PAGE_BYTE_SIZE.ilog2(); // log2(16KB)
const PAGE_MASK : u64 = (1 << PAGE_BITS) - 1;
const INSTR_INDEX_MASK : u64 = PAGE_INSTR_SIZE as u64 - 1;


pub struct InstrCache {
    pages: HashMap<u64, *const [Instr; ACTUAL_PAGE_INSTR_SIZE as usize]>,

    page_pc: u64,
    page_ptr: *const [Instr; ACTUAL_PAGE_INSTR_SIZE as usize],
    code_ptr: *const Instr,
}


impl InstrCache {
    pub fn new() -> InstrCache {
        Self {
            pages: HashMap::new(),
            page_pc: 0,
            page_ptr: null(),
            code_ptr: null(),
        }
    }


    pub fn set_pc(&mut self, mem: &Memory, pc: u64) {
        let page = pc >> PAGE_BITS;
        let page_pc = pc & !PAGE_MASK;

        let page_ptr =
            if core::hint::likely(self.page_pc == page_pc) { self.page_ptr }
            else { self.load_page(page, mem) };

        self.page_pc = page_pc;
        self.page_ptr = page_ptr;

        let offset = (pc - self.page_pc) / size_of::<u32>() as u64;
        self.code_ptr = unsafe { self.page_ptr.as_ptr().add(offset as usize) };
    }


    #[inline(always)]
    pub fn pc(&self) -> u64 {
        unsafe {
            let instr_index = self.code_ptr.offset_from(self.page_ptr.as_ptr()) as u64;
            self.page_pc + instr_index * size_of::<u32>() as u64
        }
    }


    #[inline(always)]
    pub fn get(&self) -> Instr {
        unsafe { *self.code_ptr }
    }


    #[inline(always)]
    pub fn next(&mut self) {
        unsafe { self.code_ptr = self.code_ptr.add(1) }
    }


    #[cold]
    fn load_page(&mut self, page: u64, mem: &Memory) -> *const [Instr; ACTUAL_PAGE_INSTR_SIZE as usize] {
        println!("loading page 0x{page:x}");
        let entry = self.pages.entry(page);

        match entry {
            std::collections::hash_map::Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                println!("not loaded");

                let base_ptr = page << PAGE_BITS;
                let ptr = Box::leak(Box::new(core::array::from_fn(|i| {
                    if i+1 == ACTUAL_PAGE_INSTR_SIZE as usize {
                        return Instr::Readjust;
                    }

                    let ptr = base_ptr + i as u64 * 4;
                    let ptr = Ptr(ptr);
                    let instr = mem.read_u32(ptr);

                    Instr::decode(instr)
                })));

                *vacant_entry.insert(ptr)
            },
        }
    }
}


impl Instr {
    pub fn decode(instr: u32) -> Instr {

        let opcode = ubfx_32(instr, 0, 7);

        return match opcode {
            // register-imm arithm
            0b0010011 => {
                let funct = ubfx_32(instr, 12, 3);
                let rd  = ubfx_32(instr,  7,  5) as u8;
                let rs1 = ubfx_32(instr, 15,  5) as u8;
                let imm = sbfx_32(instr, 20, 12) as i16; // is this fine?

                match funct {
                    // add
                    0b000 => Instr::IAdd { rd, rs1, imm },

                    // slti
                    0b010 => Instr::ISlt { rd, rs1, imm },

                    // sltiu
                    0b011 => Instr::ISltu { rd, rs1, imm },


                    // xori
                    0b100 => Instr::IXor  { rd, rs1, imm },

                    // ori
                    0b110 => Instr::IOr  { rd, rs1, imm },

                    // andi
                    0b111 => Instr::IAnd { rd, rs1, imm },


                    // slli
                    0b001 => {
                        Instr::ISll { rd, rs1, imm: imm & 0x3F }
                    },


                    0b101 => {
                        let funct6 = ubfx_32(instr, 26, 6) & 0x3F;

                        match funct6 {
                            // srli
                            0b000000 => Instr::ISrl { rd, rs1, imm },

                            // srai
                            0b010000 => Instr::ISra { rd, rs1, imm },

                            _ => Instr::Unknown,
                        }
                    },

                    _ => Instr::Unknown,
                }
            },


            // register-imm arithm pt2
            0b0011011 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let funct7 = ubfx_32(instr, 25, 7);
                let rd  = ubfx_32(instr,  7,  5) as u8;
                let rs1 = ubfx_32(instr, 15,  5) as u8;
                let imm = sbfx_32(instr, 20, 12) as i16;

                match funct3 {
                    // addiw
                    0b000 => {
                        Instr::IAddw { rd, rs1, imm }
                    }

                    // slliw
                    0b001 => {
                        Instr::ISllw { rd, rs1, imm }
                    }


                    // srliw
                    0b101 if funct7 == 0b00000_00 => {
                        Instr::ISrlw { rd, rs1, imm }
                    }


                    // sraiw
                    0b101 if funct7 == 0b01000_00 => {
                        Instr::ISraw { rd, rs1, imm }
                    }


                    _ => Instr::Unknown
                }
            }

            // register-register arithm
            0b0110011 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let funct7 = ubfx_32(instr, 25, 7);
                let funct = funct7 << 3 | funct3;

                let rs1    = ubfx_32(instr, 15, 5) as u8;
                let rs2    = ubfx_32(instr, 20, 5) as u8;
                let rd     = ubfx_32(instr,  7, 5) as u8;


                match funct {
                    0b0000000_000 => Instr::RAdd { rd, rs1, rs2 },
                    0b0100000_000 => Instr::RSub { rd, rs1, rs2 },
                    0b0000000_001 => Instr::RSll { rd, rs1, rs2 },
                    0b0000000_010 => Instr::RSlt { rd, rs1, rs2 },
                    0b0000000_011 => Instr::RSlut { rd, rs1, rs2 },
                    0b0000000_100 => Instr::RXor { rd, rs1, rs2 },
                    0b0000000_101 => Instr::RSrl { rd, rs1, rs2 },
                    0b0100000_101 => Instr::RSra { rd, rs1, rs2 },
                    0b0000000_110 => Instr::ROr { rd, rs1, rs2 },
                    0b0000000_111 => Instr::RAnd { rd, rs1, rs2 },
                    0b0000001_000 => Instr::RMul { rd, rs1, rs2 },
                    0b0000001_001 => Instr::RMulh { rd, rs1, rs2 },
                    0b0000001_010 => Instr::RMulhsu { rd, rs1, rs2 },
                    0b0000001_011 => Instr::RMulhu { rd, rs1, rs2 },
                    0b0000001_100 => Instr::RDiv { rd, rs1, rs2 },
                    0b0000001_101 => Instr::RDivu { rd, rs1, rs2 },
                    0b0000001_110 => Instr::RRem { rd, rs1, rs2 },
                    0b0000001_111 => Instr::RRemu { rd, rs1, rs2 },

                    _ => Instr::Unknown,
                }
            }


            // register-register arithm pt2
            0b0111011 => {
                let funct7 = ubfx_32(instr, 25, 7);
                let funct3 = ubfx_32(instr, 12, 3);
                let funct10 = funct7 << 3 | funct3;

                let rs1    = ubfx_32(instr, 15, 5) as u8;
                let rs2    = ubfx_32(instr, 20, 5) as u8;
                let rd     = ubfx_32(instr,  7, 5) as u8;


                match funct10 {
                    0b0000000_000 => Instr::RAddw { rd, rs1, rs2 },
                    0b0100000_000 => Instr::RSubw { rd, rs1, rs2 },
                    0b0000001_000 => Instr::RMulw { rd, rs1, rs2 },
                    0b0000001_100 => Instr::RDivw { rd, rs1, rs2 },
                    0b0000001_101 => Instr::RDivuw { rd, rs1, rs2 },
                    0b0000001_110 => Instr::RRemw { rd, rs1, rs2 },
                    0b0000001_111 => Instr::RRemuw { rd, rs1, rs2 },
                    0b0000000_001 => Instr::RSllw { rd, rs1, rs2 },
                    0b0000000_101 => Instr::RSrlw { rd, rs1, rs2 },
                    0b0100000_101 => Instr::RSraw { rd, rs1, rs2 },

                    _ => Instr::Unknown, 
                }
            }


            // jal
            0b1101111 => {
                let rd = ubfx_32(instr, 7, 5) as u8;

                let mut offset = 0;

                offset = bfi_32(offset, 20, 1, ubfx_32(instr, 31, 1));   // imm[20]
                offset = bfi_32(offset, 12, 8, ubfx_32(instr, 12, 8));   // imm[19:12]
                offset = bfi_32(offset, 11, 1, ubfx_32(instr, 20, 1));   // imm[11]
                offset = bfi_32(offset, 1, 10, ubfx_32(instr, 21, 10));  // imm[10:1]
                offset = sbfx_32(offset, 0, 21);

                Instr::Jal { rd, offset: offset as i32 }
            }


            // lui
            0b0110111 => {
                let rd = ubfx_32(instr, 7, 5) as u8;
                let imm = ubfx_32(instr, 12, 20) as i32;
                let imm = (imm as i32) << 12;
                Instr::Lui { rd, imm }
            }


            // auipc
            0b0010111 => {
                let rd = ubfx_32(instr, 7, 5) as u8;
                let imm = ubfx_32(instr, 12, 20);
                let imm = (imm as i32) << 12;

                Instr::Auipc { rd, offset: imm }
            }


            // fence & fence.i
            0b0001111 => {
                // WHAT THE FUCK IS A FENCE RAAAAA
                Instr::Nop
            }


            0b1110011 => {
                let csr = ubfx_32(instr, 20, 12) as u16;
                let funct3 = ubfx_32(instr, 12, 3);
                let rs1 = ubfx_32(instr, 15, 5) as u8;
                let rd = ubfx_32(instr, 7, 5) as u8;

                match funct3 {
                    0b001 => Instr::CsrRw { rd, rs1, csr },
                    0b010 => Instr::CsrRs { rd, rs1, csr },
                    0b011 => Instr::CsrRc { rd, rs1, csr },
                    0b101 => Instr::CsrRwi { rd, rs1, csr },
                    0b110 => Instr::CsrRsi { rd, rs1, csr },
                    0b111 => Instr::CsrRci { rd, rs1, csr },
                    0b000 => {
                        let funct12 = ubfx_32(instr, 20, 12);

                        match funct12 {
                            // ecall
                            0b00000_00_00000 => Instr::SECall { },

                            // ebreak
                            0b00000_00_00001 => Instr::SEBreak { },

                            // sret
                            0b00010_00_00010 => Instr::SSRet { },

                            // mret
                            0b00110_00_00010 => Instr::SMRet { },

                            // wfi
                            0b00010_00_00101 => Instr::SWfi { },

                            // sfence.vma
                            _ if ubfx_32(instr, 25, 7) == 0b0001001 => Instr::Nop, 

                            _ => Instr::Unknown,
                        }
                    }

                    _ => Instr::Unknown,
                }
            }


            // loads
            0b0000011 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let rd = ubfx_32(instr, 7, 5) as u8;
                let rs1 = ubfx_32(instr, 15, 5) as u8;
                let offset = sbfx_32(instr, 20, 12) as i32 as i16;

                match funct3 {
                    0b000 => Instr::LB { rd, rs1, offset },
                    0b001 => Instr::LH { rd, rs1, offset },
                    0b010 => Instr::LW { rd, rs1, offset },
                    0b100 => Instr::LBu { rd, rs1, offset },
                    0b101 => Instr::LHu { rd, rs1, offset },
                    0b110 => Instr::LWu { rd, rs1, offset },
                    0b011 => Instr::LD { rd, rs1, offset },

                    _ => Instr::Unknown
                }
            }


            // stores
            0b0100011 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let imm5 = ubfx_32(instr, 7, 5);
                let imm7 = ubfx_32(instr, 25, 7);
                let imm = (imm7 << 5) | imm5;
                let imm = sbfx_32(imm, 0, 12);
                let imm = imm as i32 as i16;
                let offset = imm;

                let rs1 = ubfx_32(instr, 15, 5) as u8;
                let rs2 = ubfx_32(instr, 20, 5) as u8;

                match funct3 {
                    0b000 => Instr::SB { rs1, rs2, offset },
                    0b001 => Instr::SH { rs1, rs2, offset },
                    0b010 => Instr::SW { rs1, rs2, offset },
                    0b011 => Instr::SD { rs1, rs2, offset },


                    _ => Instr::Unknown,
                }
            }


            // branching
            0b1100011 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let rs1 = ubfx_32(instr, 15, 5) as u8;
                let rs2 = ubfx_32(instr, 20, 5) as u8;
                let imm =
                    ubfx_32(instr, 7, 1) << 11
                  | ubfx_32(instr, 8, 4) << 1
                  | ubfx_32(instr, 25, 6) << 5
                  | ubfx_32(instr, 31, 1) << 12; // who the fuck wrote this spec bro
                let imm = sbfx_32(imm, 0, 13) as i32 as i16;


                match funct3 {
                    0b000 => Instr::BEq { rs1, rs2, imm },
                    0b001 => Instr::BNe { rs1, rs2, imm },
                    0b100 => Instr::BLt { rs1, rs2, imm },
                    0b101 => Instr::BGe { rs1, rs2, imm },
                    0b110 => Instr::BLtu { rs1, rs2, imm },
                    0b111 => Instr::BGeu { rs1, rs2, imm },

                    _ => Instr::Unknown
                }
            }



            // jumps
            0b1100111 => {
                let funct3 = ubfx_32(instr, 12, 3);
                let rs1 = ubfx_32(instr, 15, 5) as u8;
                let rd = ubfx_32(instr, 7, 5) as u8;
                let imm = sbfx_32(instr, 20, 12) as i32 as i16;


                match funct3 {
                    0b000 => Instr::JAlr { rd, rs1, imm },
                    _ => Instr::Unknown,
                }
            }

            _ => Instr::Unknown,
        }
    }
}
