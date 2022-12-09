use lox::{Printer, RunTime};
use wasm_bindgen::prelude::*;

mod utils;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub struct Lox {
    rt: RunTime,
}

#[derive(Debug, Clone)]
struct WasmPrinter {
    printfn: js_sys::Function,
}

impl WasmPrinter {
    pub fn new(f: &js_sys::Function) -> Self {
        Self { printfn: f.clone() }
    }

    pub fn print(&self, val: String) {
        let this = JsValue::null();
        let x = JsValue::from(val);
        self.printfn.call1(&this, &x).expect("couldn't call js");
    }
}

impl Printer for WasmPrinter {
    fn out(&self, val: String) {
        self.print(val);
    }

    fn err(&self, val: String) {
        self.print(val)
    }
}

#[wasm_bindgen]
impl Lox {
    #[wasm_bindgen(constructor)]
    pub fn new(printer: &js_sys::Function) -> Self {
        Self {
            rt: RunTime::new(WasmPrinter::new(printer)),
        }
    }

    pub fn run(&mut self, code: String) {
        self.rt.run(&code)
    }
}
