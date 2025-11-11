use std::{env, panic, sync::Mutex};

use colourful::ColourBrush;
use riscv_emulator::{mem::{Memory, Ptr}, Emulator};

fn main() {
    let file = env::args().skip(1).next().unwrap();
    let binary = std::fs::read(&file).unwrap();
    let mut em = Emulator::new();
    let mut em = Mutex::new(em);


    panic::set_hook(Box::new(|_| {}));
    let result = panic::catch_unwind(|| {
        em.lock().unwrap().run(&binary)
    });


    print!("{file}: ");
    if let Err(e) = result {
        print!("{}: '", "CRASHED".red().bold());
        if let Some(msg) = e.downcast_ref::<&str>() {
            println!("{msg}'");
        } else if let Some(msg) = e.downcast_ref::<String>() {
            println!("{msg}'");
        } else {
            println!("unknown panic type'");
        }
        return;
    }


    let Ok(em) = em.lock()
    else {
        return;
    };

    let result = result.unwrap();
    if !result {
        println!("{}", "TIMEOUT".red().bold());
        return;
    }

    let exit_code = em.x.read(10);
    let testnum = em.x.read(3);

    if exit_code == 0 {
        println!("{}", "PASS".green());
    } else {
        println!("{} (exit_code={exit_code}, TESTNUM={testnum}", "FAIL".red().bold());
    }
}
