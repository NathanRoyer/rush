use rush::init;

fn main() {
    let mut context = init();
    let src = include_str!("test.rush");
    context.parse("test", src);
    context.run();
}




