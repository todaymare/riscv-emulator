use std::ops::Range;

#[derive(Debug)]
pub struct Memory {
    regions: Vec<Region>,
}


#[derive(Debug)]
pub struct Region {
    range: Range<u64>,
    mem  : Vec<u8>,
    perm : MemoryAuthority,
}


#[derive(Debug, Clone, Copy)]
pub enum MemoryAuthority {
    User = 0,
    Admin = 1,
}


impl Memory {
    pub fn new() -> Self {
        Self {
            regions: vec![
                Region::new(0x8000_0000..0xA000_0000, MemoryAuthority::Admin), // rom
                Region::new(0xA000_0000..0xF000_0000, MemoryAuthority::User ), // ram
            ],
        }
    }


    pub fn read_u32(&mut self, ptr: Ptr) -> u32 {
        u32::from_ne_bytes(self.read(ptr, 4).try_into().unwrap())
    }


    pub fn read<'me>(&mut self, ptr: Ptr, size: usize) -> &[u8] {
        for region in &mut self.regions {
            if region.range.contains(&ptr.0) {
                return region.read(ptr, size);
            }
        }

        panic!("bus error {:x}", ptr.0);
    }


    pub fn write<'me>(&mut self, perm: MemoryAuthority, ptr: Ptr, data: &[u8]) {
        for region in &mut self.regions {
            if region.range.contains(&ptr.0) {
                region.write(perm, ptr, data);
                return;
            }
        }

        panic!("bus error");
    }
}


impl Region {
    pub fn new(range: Range<u64>, perm: MemoryAuthority) -> Self {
        Self {
            range,
            mem: vec![],
            perm,
        }
    }


    pub fn read(&mut self, ptr: Ptr, size: usize) -> &[u8] {
        if self.range.contains(&ptr.0) {
            let start = ptr.0 - self.range.start;
            let end = start + size as u64;
            assert!(end <= self.range.end);

            if self.mem.len() < end as usize {
                self.mem.resize(end as usize, 0);
            }

            &self.mem[start as usize..end as usize]
            
        } else {
            panic!("invalid region error");
        }
    }


    pub fn write(&mut self, perm: MemoryAuthority, ptr: Ptr, data: &[u8]) {
        if (perm as u64) < (self.perm as u64) {
            panic!("permission error");
        }


        if self.range.contains(&ptr.0) {
            let start = ptr.0 - self.range.start;
            let end = start + data.len() as u64;
            assert!(end <= self.range.end);

            if self.mem.len() < end as usize {
                self.mem.resize(end as usize, 0);
            }

            self.mem[start as usize..end as usize].copy_from_slice(data);

        } else {
            panic!("invalid region error");
        }
    }

}


pub struct Ptr(pub u64);
