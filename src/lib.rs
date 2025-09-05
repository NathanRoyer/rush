use litemap::LiteMap;
use std::fmt::Write;
use std::sync::Arc;

mod tokenizer;
mod builtin;
mod engine;
mod parser;
mod names;

use engine::Expression;
use names::Names;

pub use builtin::{BuiltIn, Stores, Entry, ValueMap, StrIndex, VecIndex, MapIndex};
pub use parser::Error as ParsingError;
pub use engine::Engine;

const VOID_TYPE: TypeIndex = builtin::BuiltInType::Void as TypeIndex;

/// Rust functions exposed to rush code
pub type BuiltInFunc = fn(&mut Engine, Vec<Value>) -> FuncRes;
type FuncRes = Result<Value, Panic>;
type TypeList = Vec<TypeIndex>;

/// Index to a constant expression in a [`Context`]
pub type ConstIndex = usize;
/// Index to a type in a [`Context`]
pub type TypeIndex = usize;
/// Index to a function in a [`Context`]
pub type FuncIndex = usize;
/// Index to an identifier in a [`Context`]
pub type NameIndex = usize;

/// Object that can represent any rush value
#[derive(Clone, Debug)]
pub struct Value {
    pub built_in: BuiltIn,
    pub type_index: TypeIndex,
}

impl Default for Value {
    fn default() -> Self {
        Self::from(BuiltIn::None)
    }
}

/// Runtime failure
pub struct Panic {
    call_stack: Vec<(String, usize)>,
    message: &'static str,
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

    fn add_to_call_stack(&mut self, step: String, line: usize) {
        self.call_stack.push((step, line));
    }

    pub fn dump(&self, engine: &Engine) -> String {
        let mut out = String::new();

        let _ = writeln!(out, "panic: {}", self.message);

        let _ = writeln!(out, "\nbacktrace:");
        let mut i = self.call_stack.len();
        for (step, line) in &self.call_stack {
            let _ = writeln!(out, "{i}. {step} (line {line})");
            i -= 1;
        }

        if !self.data.is_empty() {
            let _ = writeln!(out, "\ndebugging values:");
        }

        let ctx = &engine.context;

        i = self.data.len();
        for value in &self.data {
            let fg_type = &ctx.types[value.type_index];
            let path = ctx.stringify_path(&fg_type.canonical_path);
            let mut repr = String::new();
            builtin::dump(&mut repr, &engine, &value);
            let mut truncated = false;

            while repr.len() > 80 {
                truncated = true;
                repr.pop();
            }

            if truncated {
                repr.push('…');
            }

            let _ = writeln!(out, "{i}. [{path}] {repr}");
            i -= 1;
        }

        out
    }
}

struct Field {
    name: NameIndex,
    spec: TypeList,
}

enum TypeData {
    Trait(Vec<NameIndex>),
    Struct(Vec<Field>),
    Alias(TypeList),
}

struct Type {
    canonical_path: ItemPath,
    data: TypeData,
    methods: LiteMap<NameIndex, FuncIndex>,
    traits: LiteMap<TypeIndex, ()>,
}

#[derive(Clone)]
enum FuncData {
    BuiltIn(BuiltInFunc),
    Rush(engine::Block),
    Redirect(FuncIndex),
}

#[derive(Clone)]
struct Function {
    canonical_path: ItemPath,
    parameters: Vec<(NameIndex, TypeList)>,
    return_type: TypeList,
    data: FuncData,
}

/// Registry of all defined items (function, type or constant)
pub struct Context {
    built_in_funcs: LiteMap<&'static str, BuiltInFunc>,
    constants: Vec<Expression>,
    functions: Vec<Function>,
    resolver: Resolver,
    types: Vec<Type>,
    names: Names,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
        this.define("std", std_code).unwrap();
        builtin::check_builtin_types(&this);
        this
    }

    pub fn add_builtin(&mut self, name: &'static str, ptr: BuiltInFunc) {
        self.built_in_funcs.insert(name, ptr);
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

    fn tag_trait_fellows(&mut self) {
        let len = self.types.len();
        let mut spec = Vec::new();

        for trait_index in 0..len {
            let handle = &self.types[trait_index];

            let TypeData::Trait(methods) = &handle.data else {
                continue;
            };

            spec.extend_from_slice(&methods);

            for type_index in 0..len {
                let handle = &mut self.types[type_index];

                let is_fellow = spec
                    .iter()
                    .all(|name| handle.methods.contains_key(name));

                if is_fellow {
                    handle.traits.insert(trait_index, ());
                }
            }

            spec.clear();
        }
    }

    pub fn define(&mut self, mod_name: &str, module: &str) -> Result<(), ParsingError> {
        let mut slice = module;
        let Ok(tokens) = tokenizer::tokenize(&mut slice) else {
            let tokenized_len = module.len() - slice.len();
            let tokenized = &module[..tokenized_len];
            let line = tokenized.lines().count();

            return Err(ParsingError {
                message: "failed to tokenize",
                line,
            });
        };

        parser::define(self, mod_name, &tokens)
    }

    pub fn run(mut self) -> Result<(), String> {
        self.tag_trait_fellows();
        let this = std::sync::Arc::new(self);
        let backup = this.clone();
        let funcs = &backup.functions;
        let mut engine = Engine::new(this);
        let main_name = backup.names.main;

        let mut main_i = None;

        for (i, func) in funcs.iter().enumerate() {
            if func.canonical_path.last() == Some(&main_name) {
                let None = main_i.replace(i) else {
                    return Err("multiple 'main' function definitions".to_string());
                };
            }
        }

        let Some(i) = main_i else {
            return Err("no 'main' function definition".to_string());
        };

        if let Err(panic) = engine.call(i, vec![]) {
            return Err(panic.dump(&engine));
        }

        Ok(())
    }
}
