use std::{alloc::{alloc, alloc_zeroed, Layout}, ops::Range, sync::atomic::AtomicU8};

use crate::Priv;

#[derive(Debug)]
pub struct Memory {
    regions: Vec<Region>,
    buff: SendPtr,
}


#[derive(Debug)]
pub struct SendPtr(*mut u8);


unsafe impl Send for SendPtr {}
unsafe impl Sync for SendPtr {}


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
                Region::new(0x1000_0000..0x2000_0000, Priv::User   ), // framebuffer
                Region::new(0x8000_0000..0xA000_0000, Priv::User   ), // rom
                Region::new(0xA000_0000..0xFFFF_FFFF, Priv::User   ), // ram
            ],

            buff: SendPtr(buff),
        }
    }


    pub fn read_u8(&self, ptr: Ptr) -> u8 {
        self.read_sized::<1>(ptr)[0]
    }


    pub fn read_u16(&self, ptr: Ptr) -> u16 {
        u16::from_ne_bytes(self.read_sized(ptr))
    }


    pub fn read_u32(&self, ptr: Ptr) -> u32 {
        u32::from_ne_bytes(self.read_sized(ptr))
    }


    pub fn read_u64(&self, ptr: Ptr) -> u64 {
        u64::from_ne_bytes(self.read_sized(ptr))
    }



    pub fn read<'me>(&self, ptr: Ptr, size: usize) -> &[u8] {
        for region in &self.regions {
            if region.range.contains(&ptr.0) {
                assert!(ptr.0 <= region.range.end - size as u64);

                unsafe {

                let mut ptr = self.buff.0.add(ptr.0 as usize);
                core::hint::black_box(&mut ptr);
                let slice = core::slice::from_raw_parts(ptr, size);
                return slice;

                }


            }
        }

        panic!("bus error {:x}", ptr.0);
    }


    pub fn read_sized<const N: usize>(&self, ptr: Ptr) -> [u8; N] {
        for region in &self.regions {
            if region.range.contains(&ptr.0) {
                assert!(ptr.0 <= region.range.end - N as u64);

                unsafe {
                let mut ptr = self.buff.0.add(ptr.0 as usize).cast();
                core::hint::black_box(&mut ptr);
                return *ptr;

                }


            }
        }

        panic!("bus error {:x}", ptr.0);
    }


    pub fn write<'me>(&self, perm: Priv, ptr: Ptr, data: &[u8]) {
        for region in &self.regions {
            if region.range.contains(&ptr.0) {
                if (perm as u64) < (region.perm as u64) {
                    panic!("permission error");
                }

                assert!(ptr.0 <= region.range.end - data.len() as u64);

                unsafe {

                let mut ptr = self.buff.0.add(ptr.0 as usize);
                core::hint::black_box(&mut ptr);
                core::ptr::copy_nonoverlapping(data.as_ptr(), ptr, data.len());
                /*
                for i in 0..data.len() {
                    (&*ptr.add(i)).store(data[i], std::sync::atomic::Ordering::Relaxed);
                }*/

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


#[derive(Clone, Copy)]
pub struct Ptr(pub u64);
