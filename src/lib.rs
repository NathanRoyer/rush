use litemap::LiteMap;
use std::sync::Arc;

mod tokenizer;
mod builtin;
mod engine;
mod parser;
mod names;

use engine::{Engine, Expression};
use builtin::BuiltIn;
use names::Names;

const VOID_TYPE: TypeIndex = 0;

pub type BuiltInFunc = fn(&mut Engine, Vec<Value>) -> FuncRes;
pub type FuncRes = Result<Value, Panic>;
type TypeList = Vec<TypeIndex>;
type ConstIndex = usize;
type TypeIndex = usize;
type FuncIndex = usize;
type NameIndex = usize;

#[derive(Clone)]
pub struct Value {
    built_in: BuiltIn,
    type_index: TypeIndex,
}

impl Default for Value {
    fn default() -> Self {
        Self::from(BuiltIn::None)
    }
}

pub struct Panic {
    message: &'static str,
    call_stack: Vec<String>,
    data: Vec<Value>,
}

impl Panic {
    pub fn new<const N: usize>(message: &'static str, data: [Value; N]) -> Self {
        Self {
            message,
            call_stack: Vec::new(),
            data: data.into(),
        }
    }

    fn add_to_call_stack(&mut self, step: String) {
        self.call_stack.push(step);
    }
}

#[allow(dead_code)]
struct Field {
    name: NameIndex,
    list: TypeList,
}

#[allow(dead_code)]
enum TypeData {
    Struct(Vec<Field>),
    Alias(TypeList),
}

struct Type {
    canonical_path: ItemPath,
    data: TypeData,
    methods: LiteMap<NameIndex, FuncIndex>,
}

enum FuncData {
    BuiltIn(BuiltInFunc),
    Rush(engine::Block),
}

#[allow(dead_code)]
struct Function {
    canonical_path: ItemPath,
    parameters: Vec<(NameIndex, TypeList)>,
    return_type: TypeList,
    data: FuncData,
}

pub struct Context {
    built_in_funcs: LiteMap<&'static str, BuiltInFunc>,
    constants: Vec<Expression>,
    functions: Vec<Function>,
    resolver: Resolver,
    types: Vec<Type>,
    names: Names,
}

#[derive(Clone, PartialEq, Eq)]
enum Item {
    Function(FuncIndex),
    Redirect(ItemPath),
    Const(ConstIndex),
    Type(TypeIndex),
    Group,
}

type ItemPath = Arc<[NameIndex]>;
type Resolver = LiteMap<ItemPath, Item>;

impl Context {
    fn empty() -> Self {
        Self {
            built_in_funcs: LiteMap::new(),
            resolver: LiteMap::new(),
            names: Names::empty(),
            constants: Vec::new(),
            functions: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn new() -> Self {
        let std_code = include_str!("std.rush");
        let mut this = Self::empty();
        this.names = Names::default();
        builtin::init(&mut this);
        this.parse("std", std_code);
        this
    }

    pub fn add_builtin(&mut self, name: &'static str, ptr: BuiltInFunc) {
        self.built_in_funcs.insert(name, ptr);
    }

    pub fn parse(&mut self, mod_name: &str, mut module: &str) {
        let Ok(tokens) = tokenizer::tokenize(&mut module) else {
            let line = match module.split_once('\n') {
                Some((line, _)) => line,
                None => module,
            };

            println!("failed to tokenize: {line}");
            return;
        };

        if let Err(error) = parser::parse(self, mod_name, &tokens) {
            println!("syntax error: {}", error.message);
            println!("line: {}", error.line);
        }
    }

    pub fn run(self) {
        let this = std::sync::Arc::new(self);
        let backup = this.clone();
        let funcs = &backup.functions;
        let mut engine = Engine::new(this);
        let main_name = backup.names.main;

        for (i, func) in funcs.iter().enumerate() {
            if func.canonical_path.last() == Some(&main_name) {
                if let Err(panic) = engine.call(i, vec![]) {
                    println!("--------------------------------");
                    println!("panic: {}", panic.message);

                    println!("\ncall stack:");
                    for step in panic.call_stack {
                        println!("- {step}");
                    }

                    if !panic.data.is_empty() {
                        println!("\ndebugging values:");
                    }

                    for value in panic.data {
                        let fg_type = &backup.types[value.type_index];
                        let path = backup.stringify_path(&fg_type.canonical_path);
                        let mut repr = String::new();
                        builtin::display(&mut repr, &engine, &value);
                        let mut truncated = false;

                        while repr.len() > 80 {
                            truncated = true;
                            repr.pop();
                        }

                        if truncated {
                            repr.push('…');
                        }

                        println!("- [{path}] {repr}");
                    }

                    println!("");
                    break;
                }
            }
        }
    }

    fn stringify_path(&self, path: &[NameIndex]) -> String {
        let mut out = String::new();

        for index in path {
            out += &self.names[*index];
            out += "::";
        }

        out.pop();
        out.pop();
        out
    }
}
