use std::{alloc::{alloc, alloc_zeroed, dealloc, Layout}, ops::Range, sync::atomic::{AtomicU32, AtomicU8}, thread::park_timeout};

use crate::{utils::{ubfx_32, ubfx_64}, Priv};


const REGIONS : [Region; 5] = [
    Region::new(0x8000_0000..0xFFFF_FFFF, Priv::User   ), // ram
    Region::new(0x8000_0000..0xA000_0000, Priv::User   ), // rom
    Region::new(0x4000_0000..0x400e1000, Priv::User   ), // framebuffer
    Region::new(0x4010_0000..0x4010_0010, Priv::User   ), // framebuffer settings
    Region::new(0x0200_0000..0x0200_FFFF, Priv::User   ), // clint
];

#[derive(Debug)]
pub struct Memory {
    buff: SendPtr,

    pub mmio_kbd_status: AtomicU32,
    pub mmio_kbd_key   : AtomicU32,
    pub mmio_kbd_mods  : AtomicU32,
    pub mmio_kbd_irq   : AtomicU32,
}


const MMIO_KBD_STATUS : u64 = 0x1000_0000;
const MMIO_KBD_KEY    : u64 = 0x1000_0004;
const MMIO_KBD_MODS   : u64 = 0x1000_0008;
const MMIO_KBD_IRQ    : u64 = 0x1000_000C;


const MEMORY_SIZE : u64 = 0xFFFF_FFFF;


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
        let buff = unsafe { alloc_zeroed(Layout::from_size_align(MEMORY_SIZE as _, 8).unwrap()) };
        Self {
            buff: SendPtr(buff.cast()),


            mmio_kbd_status: AtomicU32::default(),
            mmio_kbd_key: AtomicU32::default(),
            mmio_kbd_mods: AtomicU32::default(),
            mmio_kbd_irq: AtomicU32::default(),

            
        }
    }


    pub fn read_u8(&self, ptr: PhysPtr) -> u8 {
        self.read_sized::<1>(ptr)[0]
    }


    pub fn read_u16(&self, ptr: PhysPtr) -> u16 {
        u16::from_ne_bytes(self.read_sized(ptr))
    }


    pub fn read_u32(&self, ptr: PhysPtr) -> u32 {
        match ptr.0 {
            MMIO_KBD_STATUS => self.mmio_kbd_status.load(std::sync::atomic::Ordering::Acquire),
            MMIO_KBD_KEY    => self.mmio_kbd_key.swap(0, std::sync::atomic::Ordering::AcqRel),
            MMIO_KBD_MODS   => self.mmio_kbd_mods.load(std::sync::atomic::Ordering::Acquire),
            MMIO_KBD_IRQ    => self.mmio_kbd_irq.load(std::sync::atomic::Ordering::Acquire),
            _ => u32::from_ne_bytes(self.read_sized(ptr))
        }
        
    }


    pub fn read_u64(&self, ptr: PhysPtr) -> u64 {
        u64::from_ne_bytes(self.read_sized(ptr))
    }



    pub fn read<'me>(&self, ptr: PhysPtr, size: usize) -> &[u8] {
        assert!(ptr.0.saturating_add(size as u64) < MEMORY_SIZE);
        unsafe {
            let mut ptr = self.buff.0.add(ptr.0 as usize);
            core::hint::black_box(&mut ptr);
            core::slice::from_raw_parts(ptr, size)
        }

    }


    pub fn read_sized<const N: usize>(&self, ptr: PhysPtr) -> [u8; N] {
        assert!(ptr.0.saturating_add(N as u64) < MEMORY_SIZE, "0x{:x}", ptr.0);
        unsafe {
            let mut ptr = self.buff.0.add(ptr.0 as usize).cast();
            core::hint::black_box(&mut ptr);
            *ptr
        }

    }


    pub fn write<'me>(&self, ptr: PhysPtr, data: &[u8]) {
        assert!(ptr.0.saturating_add(data.len() as u64) < MEMORY_SIZE);

        if data.len() == 4 {
            match ptr.0 {
                MMIO_KBD_IRQ => {
                    self.mmio_kbd_irq.store(
                        u32::from_ne_bytes(data.try_into().unwrap()), 
                        std::sync::atomic::Ordering::Release
                    );
                    return;
                }

                _ => (),
            }
        }


        unsafe {
            let mut ptr = self.buff.0.add(ptr.0 as usize);
            core::hint::black_box(&mut ptr);
            core::ptr::copy_nonoverlapping(data.as_ptr(), ptr, data.len());
        }

    }
}


impl Drop for Memory {
    fn drop(&mut self) {
        unsafe { dealloc(self.buff.0, Layout::from_size_align(MEMORY_SIZE as _, 8).unwrap()); }
    }
}


impl Region {
    pub const fn new(range: Range<u64>, perm: Priv) -> Self {
        Self {
            range,
            perm,
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub struct VirtPtr(pub u64);


#[derive(Debug, Clone, Copy)]
pub struct PhysPtr(pub u64);
