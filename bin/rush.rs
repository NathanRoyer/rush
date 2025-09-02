use rush::Context;

fn main() {
    let mut context = Context::new();
    let src = include_str!("test.rush");
    if let Err(error) = context.define("test", src) {
        println!("syntax error: {} (line {})", error.message, error.line);
        return;
    }

    if let Err(panic) = context.run() {
        println!("--------------------------------");
        println!("{panic}");
    }
}




