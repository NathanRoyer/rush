use rush::Context;

fn main() {
    let mut context = Context::new();
    let src = include_str!("test.rush");
    context.define("test", src).unwrap();

    if let Err(panic) = context.run() {
        println!("--------------------------------");
        println!("{panic}");
    }
}




