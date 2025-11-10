#[inline]
pub const fn ubfx_32(word: u32, pos: u32, len: u32) -> u32 {
    assert!(len > 0 && len < 32 && pos <= 32 - len);
    return (word >> pos) & ((1 << len) - 1);
}


#[inline]
pub const fn sbfx_32(word: u32, pos: u32, len: u32) -> u32 {
    assert!(len > 0 && len < 32 && pos <= 32 - len);
    return ((word as i32) << (32 - (pos + len)) >> (32 - len)) as u32;
}


#[inline]
pub const fn bfi_32(word: u32, pos: u32, len: u32, bits: u32) -> u32 {
    assert!(len > 0 && len < 32 && pos <= 32 - len);
    let mask = ((1 << len) - 1) << pos;
    return (word & !mask) | ((bits << pos) & mask);
}


#[inline]
pub const fn ubfx_64(word: u64, pos: u32, len: u32) -> u64 {
    assert!(len > 0 && len < 64 && pos <= 64 - len);
    return (word >> pos) & ((1 << len) - 1);
}


#[inline]
pub const fn sbfx_64(word: u64, pos: u32, len: u32) -> u64 {
    assert!(len > 0 && len < 64 && pos <= 64 - len);
    return ((word as i64) << (64 - (pos + len)) >> (64 - len)) as u64;
}


#[inline]
pub const fn bfi_64(word: u64, pos: u32, len: u32, bits: u64) -> u64 {
    assert!(len > 0 && len < 64 && pos <= 64 - len);
    let mask = ((1 << len) - 1) << pos;
    return (word & !mask) | ((bits << pos) & mask);
}
