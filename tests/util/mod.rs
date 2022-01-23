use rrss::{exec::exec_using, frontend::parser::parse};

pub struct Code<'a>(pub &'a str);
pub struct Input<'a>(pub &'a str);

pub fn run(code: Code, input: Input) -> String {
    let mut output = Vec::new();
    exec_using(input.0.as_bytes(), &mut output, &parse(code.0).unwrap()).unwrap();
    std::str::from_utf8(&output).unwrap().to_owned()
}
