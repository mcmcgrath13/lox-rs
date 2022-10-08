use lox::RunTime;
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

#[wasm_bindgen]
impl Lox {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { rt: RunTime::new() }
    }

    pub fn run(&mut self, code: String) -> Result<String, String> {
        self.rt.run(&code)
    }
}

impl Default for Lox {
    fn default() -> Self {
        Self::new()
    }
}
