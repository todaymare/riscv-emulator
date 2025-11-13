use std::{alloc::{alloc, alloc_zeroed, Layout}, ops::Range, sync::atomic::AtomicU8};

use crate::Priv;

#[derive(Debug)]
pub struct Memory {
    regions: Vec<Region>,
    buff: SendPtr,
}


#[derive(Debug)]
pub struct SendPtr(*const AtomicU8);


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
        let buff = buff.cast::<AtomicU8>();
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
        let slice = self.read(ptr, 1);
        slice[0].load(std::sync::atomic::Ordering::Relaxed)
    }


    pub fn read_u16(&self, ptr: Ptr) -> u16 {
        let slice = self.read(ptr, 2);
        let slice = [
            slice[0].load(std::sync::atomic::Ordering::Relaxed),
            slice[1].load(std::sync::atomic::Ordering::Relaxed),
        ];

        u16::from_ne_bytes(slice)
    }


    pub fn read_u32(&self, ptr: Ptr) -> u32 {
        let slice = self.read(ptr, 4);
        let slice = [
            slice[0].load(std::sync::atomic::Ordering::Relaxed),
            slice[1].load(std::sync::atomic::Ordering::Relaxed),
            slice[2].load(std::sync::atomic::Ordering::Relaxed),
            slice[3].load(std::sync::atomic::Ordering::Relaxed),
        ];

        u32::from_ne_bytes(slice)
    }


    pub fn read_u64(&self, ptr: Ptr) -> u64 {
        let slice = self.read(ptr, 8);
        unsafe { core::hint::assert_unchecked(slice.len() == 8) };
        let slice = [
            slice[0].load(std::sync::atomic::Ordering::Relaxed),
            slice[1].load(std::sync::atomic::Ordering::Relaxed),
            slice[2].load(std::sync::atomic::Ordering::Relaxed),
            slice[3].load(std::sync::atomic::Ordering::Relaxed),
            slice[4].load(std::sync::atomic::Ordering::Relaxed),
            slice[5].load(std::sync::atomic::Ordering::Relaxed),
            slice[6].load(std::sync::atomic::Ordering::Relaxed),
            slice[7].load(std::sync::atomic::Ordering::Relaxed),
        ];

        u64::from_ne_bytes(slice)
    }



    fn read<'me>(&self, ptr: Ptr, size: usize) -> &[AtomicU8] {
        for region in &self.regions {
            if region.range.contains(&ptr.0) {
                assert!(ptr.0 <= region.range.end - size as u64);

                unsafe {

                let ptr = self.buff.0.add(ptr.0 as usize);
                return core::slice::from_raw_parts(ptr, size);

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


                let ptr = self.buff.0.add(ptr.0 as usize);
                for i in 0..data.len() {
                    (&*ptr.add(i)).store(data[i], std::sync::atomic::Ordering::Relaxed);
                }

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
