pub mod analysis;
pub mod driver;
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
