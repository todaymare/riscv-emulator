use std::{alloc::{alloc, alloc_zeroed, Layout}, ops::Range};

use crate::Priv;

#[derive(Debug)]
pub struct Memory {
    regions: Vec<Region>,
    buff: *mut u8,
}


#[derive(Debug)]
pub struct Region {
    range: Range<u64>,
    perm : Priv,
}


impl Memory {
    pub fn new() -> Self {
        let buff = unsafe { alloc_zeroed(Layout::from_size_align(0xFFFF_FFFF, 8).unwrap()) };
        Self {
            regions: vec![
                Region::new(0x0200_0000..0x0200_FFFF, Priv::User   ), // clint
                Region::new(0x8000_0000..0xA000_0000, Priv::Machine), // rom
                Region::new(0xA000_0000..0xFFFF_FFFF, Priv::User   ), // ram
            ],

            buff,
        }
    }


    pub fn read_u32(&mut self, ptr: Ptr) -> u32 {
        u32::from_ne_bytes(self.read(ptr, 4).try_into().unwrap())
    }


    pub fn read_u64(&mut self, ptr: Ptr) -> u64 {
        u64::from_ne_bytes(self.read(ptr, 8).try_into().unwrap())
    }


    pub fn read<'me>(&mut self, ptr: Ptr, size: usize) -> &[u8] {
        for region in &mut self.regions {
            if region.range.contains(&ptr.0) {
                assert!(ptr.0 <= region.range.end - size as u64);

                unsafe {

                let ptr = self.buff.add(ptr.0 as usize);
                return core::slice::from_raw_parts(ptr, size);

                }


            }
        }

        panic!("bus error {:x}", ptr.0);
    }


    pub fn write<'me>(&mut self, perm: Priv, ptr: Ptr, data: &[u8]) {
        for region in &mut self.regions {
            if region.range.contains(&ptr.0) {
                if (perm as u64) < (region.perm as u64) {
                    panic!("permission error");
                }

                assert!(ptr.0 <= region.range.end - data.len() as u64);

                unsafe {

                let ptr = self.buff.add(ptr.0 as usize);
                core::ptr::copy(data.as_ptr(), ptr, data.len());

                }

                return;
            }
        }

        panic!("bus error");
    }
}


impl Region {
    pub fn new(range: Range<u64>, perm: Priv) -> Self {
        Self {
            range,
            perm,
        }
    }
}


pub struct Ptr(pub u64);
