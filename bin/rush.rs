use rush::Context;

fn main() {
    let mut context = Context::new();
    let src = include_str!("test.rush");
    context.parse("test", src);
    context.run();
}




