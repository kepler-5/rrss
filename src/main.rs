pub mod analysis;
pub mod frontend;
pub mod linter;

#[cfg(test)]
#[macro_use]
extern crate inner;
#[macro_use]
extern crate lazy_static;

fn main() {
    println!("Hello, world!");
}
