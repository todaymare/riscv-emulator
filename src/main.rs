use riscv_emulator::{mem::{Memory, Ptr}, Emulator};

fn main() {
    let binary = std::fs::read("boot.bin").unwrap();
    let mut em = Emulator::new();

    em.run(&binary);
}
