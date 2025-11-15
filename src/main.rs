use std::{any::Any, collections::HashMap, env, ffi::OsStr, option, panic, sync::{atomic::{AtomicU64, Ordering}, Arc, Mutex}, thread::JoinHandle, time::Instant};

use colourful::{Colour, ColourBrush};
use pixels::{Pixels, SurfaceTexture};
use riscv_emulator::{mem::{Memory, Ptr}, Csr, Emulator, Shared, CSR_MIP, INT_EXT_M, INT_EXT_S};
use winit::{application::ApplicationHandler, dpi::PhysicalSize, event::ElementState, event_loop::EventLoop, keyboard::Key, window::{Window, WindowAttributes}};


#[derive(Default)]
struct Bucket {
    pass: usize,
    fail: usize,
    crashed: usize,
    timeout: usize,
}


#[derive(Default)]
struct Options {
    clean: bool,
    testfilter: Vec<String>,
    timeout: u64,
    framebuffer: bool,
}


fn main() {
    let options_env = env::var("SESAME").unwrap_or("".to_string());
    let mut options = Options::default();
    options.timeout = u64::MAX;

    dbg!(&options_env);
    for opt in options_env.split(';') {
        let mut words = opt.split_whitespace();
        let word = words.next().unwrap_or("");
        match word {
            "clean" => options.clean = true,
            "testfilter" => {
                for filter in words {
                    options.testfilter.push(format!("-{filter}-"));
                }
            }

            "timeout" => {
                let ms = words.next().unwrap_or("1000");
                let ms = ms.parse();
                options.timeout = ms.unwrap_or(1000);
            }

            "framebuffer" => {
                options.framebuffer = true;
                println!("framebuffer");
            }
            _ => println!("unknown option '{opt}'"),
        }
    }


    let file = env::args().skip(1).next().unwrap();


    if options.clean {
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

                if !options.testfilter.iter().any(|x| name.contains(x)) {
                    continue;
                }


                let bucket = path.file_name().unwrap().to_str().unwrap();
                let bucket = bucket.split_once('-').unwrap().0;
                if !buckets.contains_key(bucket) {
                    println!();
                    buckets.insert(bucket.to_string(), Bucket::default());
                }


                let bucket = buckets.get_mut(bucket).unwrap();

                let result = test_file(&options, name);

                print!("{}: ", name);
                match result {
                    TestResult::Pass(_) => {
                        bucket.pass += 1;
                        println!("{}", "PASS".green().bold())
                    },
                    TestResult::Fail(em) => {
                        bucket.fail += 1;

                        let local = em.local.lock().unwrap();
                        let exit_code = local.x.read(10);
                        let testnum = local.x.read(3);


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


    let result = test_file(&options, &file);


    print!("{}: ", file);
    match result {
        TestResult::Pass(_) => {
            println!("{}", "PASS".green().bold())
        },
        TestResult::Fail(em) => {
            let local = em.local.lock().unwrap();
            let exit_code = local.x.read(10);
            let testnum = local.x.read(3);


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



fn test_file(opts: &Options, path: &str) -> TestResult {
    let mut now = Instant::now();
    let file = std::fs::read(path).unwrap();

    let em = Emulator::new(opts.timeout);
    let em = Arc::new(em);
    let shared = &em.shared;

    let em_thread = em.clone();
    let timeout = opts.timeout;
    let result = std::thread::spawn(move || 
        panic::catch_unwind(move || {
            let time = Instant::now();
            let em_thread = em_thread.clone();
            let result = em_thread.run(&file);
            let time = time.elapsed();
            let cycles = em_thread.shared.csr.read(0xC00);
            println!("{cycles} cycles in {time:?} {:.2}MIPS", (cycles as f64 / time.as_secs_f64()) / 1_000_000.0);
            result
        })
    );


    if opts.framebuffer {
        let event_loop = EventLoop::new().unwrap();

        let mut app = App {
            data: None,
            cpu: &result,
            shared,
        };


        event_loop.run_app(&mut app).unwrap();
    }



    let result = result.join().unwrap();
    //println!("took {:?}", now.elapsed());

    if let Err(e) = result {
        if let Some(msg) = e.downcast_ref::<&str>() {
            return TestResult::Crashed(msg.to_string());
        } else if let Some(msg) = e.downcast_ref::<String>() {
            return TestResult::Crashed(msg.to_string());
        }

        return TestResult::Crashed("unknown panic type".to_string())
    }



    let em = Arc::into_inner(em).unwrap();

    let result = result.unwrap();
    if !result {
        return TestResult::Timeout;
    }

    let exit_code = em.local.lock().unwrap().x.read(10);

    if exit_code == 0 {
        return TestResult::Pass(em)
    } else {
        return TestResult::Fail(em)
    }
}


struct App<'a> {
    data: Option<AppInner>,
    cpu: &'a JoinHandle<Result<bool, Box<dyn Any + Send + 'static>>>,
    shared: &'a Shared,
}


struct AppInner {
    window: &'static Window,
    pixels: Pixels<'static>,
}


impl ApplicationHandler for App<'_> {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        let width = 1920;
        let height = 1080;

        let attr = WindowAttributes::default()
            .with_title("sesame")
            .with_inner_size(PhysicalSize::new(width, height));

        let window = event_loop.create_window(attr).unwrap();


        let window = Box::new(window);
        let window = &*Box::leak(window);


        let st = SurfaceTexture::new(width, height, window);
        let pixels = Pixels::new(width/3, height/3, st).unwrap();

        window.request_redraw();

        self.data = Some(AppInner {
            window,
            pixels,
        });

        
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        match event {
            winit::event::WindowEvent::RedrawRequested => {
                let data = self.data.as_mut().unwrap();

                let settings = Ptr(0x4010_0000);


                loop {
                    let is_frame_ready = self.shared.mem.read_u8(settings);
                    if is_frame_ready == 1 {
                        break;
                    }


                    if self.cpu.is_finished() {
                        event_loop.exit();
                        return;
                    }


                }


                let buf = data.pixels.frame_mut();
                let ptr = Ptr(0x4000_0000);
                println!("0x{:x}", ptr.0 + buf.len() as u64);

                buf.copy_from_slice(self.shared.mem.read(ptr, buf.len()));
                self.shared.mem.write(riscv_emulator::Priv::Machine, settings, &[0]);

                data.pixels.render().unwrap();

                if self.cpu.is_finished() {
                    event_loop.exit();
                    return;
                }

                data.window.request_redraw();
            }


            winit::event::WindowEvent::KeyboardInput { device_id, event, is_synthetic } => {
                println!("event {event:?}");
        // Only care when key is pressed
                if event.state == ElementState::Pressed {
                    if let Key::Character(text) = &event.logical_key {
                        // Take the first char (winit gives a String)
                        if let Some(ch) = text.chars().next() {
                            let ascii = ch as u32;
                            self.shared.mem.mmio_kbd_key.store(ascii, Ordering::Release);
                            self.shared.mem.mmio_kbd_status.store(1, Ordering::Release);

                            let irq = self.shared.mem.mmio_kbd_irq.load(Ordering::Relaxed);
                            // If interrupts are enabled:
                            if irq != 0 {
                                let mut mip = self.shared.csr.read(CSR_MIP);
                                mip |= 1 << INT_EXT_M;
                                println!("0b{:b}", mip);
                                self.shared.csr.write(CSR_MIP, mip);
                            } else {
                                println!("irq was 0b{irq:b}");
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }
}
