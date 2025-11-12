use std::{collections::HashMap, env, ffi::OsStr, panic, sync::Mutex};

use colourful::{Colour, ColourBrush};
use riscv_emulator::{mem::{Memory, Ptr}, Emulator};


#[derive(Default)]
struct Bucket {
    pass: usize,
    fail: usize,
    crashed: usize,
    timeout: usize,
}


fn main() {
    let file = env::args().skip(1).next().unwrap();


    if env::args().skip(2).next().is_none() {
        panic::set_hook(Box::new(|_| {}));
    }


    if std::fs::metadata(&file).unwrap().is_dir() {
        let mut buckets = HashMap::new();

        let mut entries: Vec<_> = std::fs::read_dir(&file)
            .unwrap()
            .filter_map(Result::ok)
            .collect();

        entries.sort_by_key(|e| e.path());

        for entry in entries {

            if entry.path().extension() == Some(OsStr::new("bin")) {
                let path = entry.path();
                let name = path.to_str().unwrap();

                if !name.contains("-p-") && !name.contains("-pt-") {
                    continue;
                }


                let bucket = path.file_name().unwrap().to_str().unwrap();
                let bucket = bucket.split_once('-').unwrap().0;
                if !buckets.contains_key(bucket) {
                    println!();
                    buckets.insert(bucket.to_string(), Bucket::default());
                }


                let bucket = buckets.get_mut(bucket).unwrap();

                let result = test_file(name);

                print!("{}: ", name);
                match result {
                    TestResult::Pass(_) => {
                        bucket.pass += 1;
                        println!("{}", "PASS".green().bold())
                    },
                    TestResult::Fail(em) => {
                        bucket.fail += 1;

                        let exit_code = em.x.read(10);
                        let testnum = em.x.read(3);


                        println!("{} (exit_code={exit_code}, TESTNUM={testnum})", "FAIL".red().bold())
                    },


                    TestResult::Crashed(msg) => {
                        bucket.crashed += 1;
                        println!("{}: '{msg}'", "CRASHED".red().bold())
                    },
                    TestResult::Timeout => {
                        bucket.timeout += 1;
                        println!("{}", "TIMEOUT".red().bold())
                    },
                }
            }
        }


        println!();
        println!("------------------------------------");
        println!();

        println!("BUCKETS:");
        let mut total = 0;
        let mut pass = 0;
        let mut fail = 0;
        let mut crashed = 0;
        let mut timeout = 0;

        let mut buckets = buckets.iter().collect::<Vec<_>>();
        buckets.sort_by_key(|x| x.0);
        let longest_name = buckets.iter().map(|x| x.0.len()).max().unwrap();

        for bucket in buckets {
            let bucket_total = bucket.1.pass + bucket.1.fail + bucket.1.crashed + bucket.1.timeout;
            total += bucket_total;
            pass += bucket.1.pass;
            fail += bucket.1.fail;
            crashed += bucket.1.crashed;
            timeout += bucket.1.timeout;

            let score = ((bucket.1.pass as f64/bucket_total as f64) * 100.0).round() as usize;

            let colour = 
                if score < 50 { Colour::rgb(255, 0, 0) }
                else if score == 100 { Colour::rgb(0, 255, 0) }
                else { Colour::rgb(255, 255, 0) };

            println!(
                "  {} | tests run: {}, score: {}, passed: {}, failed: {}, crashed: {}, timeout: {}", 
                format!("{:<w$}", bucket.0, w=longest_name).light_grey(),
                format!("{}/{bucket_total}", bucket.1.pass).colour(colour).bold(),
                score.colour(colour).bold(),
                if bucket.1.pass == 0 { "None".dark_grey().to_string() }
                else { bucket.1.pass.green().bold().to_string() },

                if bucket.1.fail == 0 { "None".dark_grey().to_string() }
                else { bucket.1.fail.red().bold().to_string() },

                if bucket.1.crashed == 0 { "None".dark_grey().to_string() }
                else { bucket.1.crashed.red().bold().to_string() },

                if bucket.1.timeout== 0 { "None".dark_grey().to_string() }
                else { bucket.1.timeout.red().bold().to_string() },
            );



        }

            
        println!();
        println!("------------------------------------");
        println!();

        println!("tests run: {}, score: {}, passed: {}, fail: {}, crashed: {}, timeout: {}", format!("{pass}/{total}").yellow().bold(), (((pass as f64/total as f64) * 100.0).round()).yellow().bold(), pass.green().bold(), fail.red().bold(), crashed.red().bold(), timeout.red().bold());
        println!();

        return;
    }


    let result = test_file(&file);


    print!("{}: ", file);
    match result {
        TestResult::Pass(_) => {
            println!("{}", "PASS".green().bold())
        },
        TestResult::Fail(em) => {
            let exit_code = em.x.read(10);
            let testnum = em.x.read(3);


            println!("{} (exit_code={exit_code}, TESTNUM={testnum})", "FAIL".red().bold())
        },


        TestResult::Crashed(msg) => {
            println!("{}: '{msg}'", "CRASHED".red().bold())
        },

        TestResult::Timeout => {
            println!("{}", "TIMEOUT".red().bold())
        },
    }
}



enum TestResult {
    Pass(Emulator),
    Fail(Emulator),
    Crashed(String),
    Timeout,
}



fn test_file(path: &str) -> TestResult {
    let file = std::fs::read(path).unwrap();

    let em = Emulator::new();
    let em = Mutex::new(em);

    let result = panic::catch_unwind(|| {
        em.lock().unwrap().run(&file)
    });

    if let Err(e) = result {
        if let Some(msg) = e.downcast_ref::<&str>() {
            return TestResult::Crashed(msg.to_string());
        } else if let Some(msg) = e.downcast_ref::<String>() {
            return TestResult::Crashed(msg.to_string());
        }

        return TestResult::Crashed("unknown panic type".to_string())
    }


    let Ok(em) = em.into_inner()
    else {
        unreachable!();
    };

    let result = result.unwrap();
    if !result {
        return TestResult::Timeout;
    }

    let exit_code = em.x.read(10);

    if exit_code == 0 {
        return TestResult::Pass(em)
    } else {
        return TestResult::Fail(em)
    }
}
