use super::{NameIndex, TypeIndex, FuncIndex, Context, Value, BuiltInFunc, FuncRes, Panic};
use super::engine::{Engine, Expression};
use std::cmp::Ordering;
use litemap::LiteMap;
use std::fmt::Write;
use std::sync::Arc;

pub use store::Stores;
pub use util::display;
use util::{println, display_entry, get, set, ordering};
use math::{add, subtract};

mod store;
mod math;
mod util;

/*

- <iter>: .len, .next, .skip, .take, .until, .avoid, .contains, .find, .locate, .filter, .chain, .eval, .join(method)
- <num_iter>: .iter, .until
- <edit>: .insert, .remove
- <index>: .has, .get, .set
- <eq>: .eq, .ne
- <cmp>: .lt, .gt, .lte, .gte
- <math>: .add, .sub, .mul, .div, .clamp
- <num_from>: from_hex, from_dec, from_oct
- <num_to>: .to_hex, .to_dec, .to_oct
- <enc_int>: .encode_[be/le/ne]_[size]
- <dec_int>: decode_[be/le/ne]_[size]
- <enc_float>: .to_f32, .to_f64
- <dec_float>: from_f32, from_f64
- <float>: .round, .trunc, .fract, .abs, .ceil, .floor
- <char>: .as_char

- bool: <eq>, .as_i8, .as_u8, .map(true, false)
- int: <eq>, <cmp>, <num_iter>, <math>, <num_from>, <num_to>, <enc_int>, <dec_int>, .abs
- reg: <eq>, <cmp>, <num_iter>, <math>, <num_from>, <num_to>, <enc_int>, <dec_int>, <char>
- f32, f64: <eq>, <cmp>, <math>, <num_from>, <num_from>, <enc_float>, <dec_float>, <float>
- str: <eq>, <cmp>, <iter>, .concat, .replace, .split, .lines, .as_bytes, from_bytes, .parse_toml, .parse_json, .code_point
- vec: <eq>, <iter>, <index>, <edit>, .push, .extend
- map: <eq>, <iter>, <index>, <edit>, .keep_sorted, .keys

*/

pub mod raw_types {
    use super::TypeIndex;

    pub const VOID: TypeIndex = 0;
    pub const BOOL: TypeIndex = 1;
    pub const INT:  TypeIndex = 2;
    pub const REG:  TypeIndex = 3;
    pub const F64:  TypeIndex = 4;
    pub const VEC:  TypeIndex = 5;
    pub const STR:  TypeIndex = 6;
    pub const MAP:  TypeIndex = 7;
    pub const TYPE: TypeIndex = 8;
    pub const FUNC: TypeIndex = 9;
    pub const NAME: TypeIndex = 10;
}

pub mod names {
    use super::NameIndex;

    pub const NEGATE: NameIndex = 0;
    pub const DIFFERENT: NameIndex = 1;
    pub const EQUAL: NameIndex = 2;
    pub const GREATER_EQUAL: NameIndex = 3;
    pub const LESS_EQUAL: NameIndex = 4;
    pub const GREATER: NameIndex = 5;
    pub const LESS: NameIndex = 6;
    pub const DIVIDE: NameIndex = 7;
    pub const MULTIPLY: NameIndex = 8;
    pub const SUBTRACT: NameIndex = 9;
    pub const ADD: NameIndex = 10;
    pub const NEXT: NameIndex = 11;
    pub const SET: NameIndex = 12;
    pub const GET: NameIndex = 13;
    pub const DISPLAY: NameIndex = 14;
    pub const PRINTLN: NameIndex = 15;
    pub const MAIN: NameIndex = 16;

    pub const VOID: NameIndex = 17;
    pub const BOOL: NameIndex = 18;
    pub const INT:  NameIndex = 19;
    pub const REG:  NameIndex = 20;
    pub const F64:  NameIndex = 21;
    pub const VEC:  NameIndex = 22;
    pub const STR:  NameIndex = 23;
    pub const MAP:  NameIndex = 24;
    pub const TYPE: NameIndex = 25;
    pub const FUNC: NameIndex = 26;
    pub const NAME: NameIndex = 27;

    pub const NONE:  NameIndex = 28;
    pub const FALSE: NameIndex = 29;
    pub const TRUE:  NameIndex = 30;

    // insert new names before this comment and update _LEN
    pub const _LEN: NameIndex = 31;
}

pub mod funcs {
    use super::FuncIndex;

    pub const DISPLAY:  FuncIndex = 0;
    pub const PRINTLN:  FuncIndex = 1;
    pub const ADD:      FuncIndex = 2;
    pub const GET:      FuncIndex = 3;
    pub const SET:      FuncIndex = 4;
    pub const SUBTRACT: FuncIndex = 5;
    pub const LESS:     FuncIndex = 6;
    pub const EQUAL:    FuncIndex = 7;
    pub const GREATER:  FuncIndex = 8;

    // insert new names before this comment and update _LEN
    pub const _LEN: FuncIndex = 9;
}

pub type RefCount = Arc<()>;

pub struct RushStr {
    inner: String,
    ref_count: RefCount,
}

#[allow(dead_code)]
pub struct ValueVec {
    inner: Vec<Value>,
    ref_count: RefCount,
}

pub struct Entry {
    pub key: Value,
    pub value: Value,
}

pub struct ValueMap {
    inner: Vec<Entry>,
    sorted: bool,
    ref_count: RefCount,
}

pub type StrIndex = usize;
pub type VecIndex = usize;
pub type MapIndex = usize;

#[derive(Clone, Debug, Default)]
pub enum BuiltIn {
    #[default]
    None,
    Int(i128),
    Reg(usize),
    Bool(bool),
    Float(f64),
    Str(StrIndex, RefCount),
    Vec(VecIndex, RefCount),
    Map(MapIndex, RefCount),
    Type(TypeIndex),
    Func(FuncIndex),
    Name(NameIndex),
}

type Method = (NameIndex, FuncIndex);

fn methods(inner: &[Method]) -> LiteMap<NameIndex, FuncIndex> {
    LiteMap::from_iter(inner.iter().map(Clone::clone))
}

pub fn init() -> Context {
    use super::{Item, Type, TypeData, Function, FuncData, ItemPath};

    let names: [&str; names::_LEN] = [
        "negate", "different", "equal", "greater_equal", "less_equal",
        "greater", "less", "divide", "multiply", "subtract", "add",
        "next", "set", "get", "display", "println", "main", "void",
        "bool", "int", "reg", "f64", "vec", "str", "map", "type",
        "func", "name", "none", "false", "true",
    ];

    let names = names.into_iter().map(String::from).collect();
    let mut functions = Vec::new();
    let mut constants = Vec::new();
    let mut resolver = LiteMap::new();
    let mut types = Vec::new();

    let mut add_type = |name: NameIndex, index: TypeIndex, method_slice: &[Method]| {
        let path: ItemPath = [name].into();
        resolver.insert(path.clone(), Item::Type(index));

        types.push(Type {
            canonical_path: path,
            data: TypeData::Struct(vec![]),
            methods: methods(method_slice),
        });
    };

    let subtract_m = (names::SUBTRACT, funcs::SUBTRACT);
    let display_m = (names::DISPLAY, funcs::DISPLAY);
    let add_m = (names::ADD, funcs::ADD);
    let get_m = (names::GET, funcs::GET);
    let set_m = (names::SET, funcs::SET);

    let less_m = (names::LESS, funcs::LESS);
    let equal_m = (names::EQUAL, funcs::EQUAL);
    let greater_m = (names::GREATER, funcs::GREATER);

    let num_m = [display_m, add_m, subtract_m, less_m, equal_m, greater_m];
    let basic_m = [display_m, less_m, equal_m, greater_m];

    add_type(names::VOID, raw_types::VOID, &basic_m);
    add_type(names::BOOL, raw_types::BOOL, &basic_m);

    add_type(names::INT, raw_types::INT, &num_m);
    add_type(names::REG, raw_types::REG, &num_m);
    add_type(names::F64, raw_types::F64, &num_m);

    add_type(names::STR, raw_types::STR, &[display_m, add_m, less_m, equal_m, greater_m]);
    add_type(names::VEC, raw_types::VEC, &[display_m, add_m, get_m, set_m, less_m, equal_m, greater_m]);
    add_type(names::MAP, raw_types::MAP, &[display_m, get_m, set_m, less_m, equal_m, greater_m]);

    add_type(names::TYPE, raw_types::TYPE, &basic_m);
    add_type(names::FUNC, raw_types::FUNC, &basic_m);
    add_type(names::NAME, raw_types::NAME, &basic_m);

    let mut add_method = |name: NameIndex, func: FuncIndex, ptr: BuiltInFunc| {
        let path: ItemPath = [name].into();
        resolver.insert(path.clone(), Item::Function(func));

        functions.push(Function {
            canonical_path: path,
            parameters: vec![],
            return_type: vec![],
            data: FuncData::BuiltIn(ptr),
        });
    };

    // DISPLAY
    add_method(names::DISPLAY, funcs::DISPLAY, display_entry);
    add_method(names::PRINTLN, funcs::PRINTLN, println);
    add_method(names::ADD, funcs::ADD, add);
    add_method(names::GET, funcs::GET, get);
    add_method(names::SET, funcs::SET, set);
    add_method(names::SUBTRACT, funcs::SUBTRACT, subtract);
    add_method(names::LESS, funcs::LESS, ordering::<-1>);
    add_method(names::EQUAL, funcs::EQUAL, ordering::<0>);
    add_method(names::GREATER, funcs::GREATER, ordering::<1>);

    constants.push(Expression::Bool(false));
    constants.push(Expression::Bool(true));
    constants.push(Expression::None);

    resolver.insert([names::FALSE].into(), Item::Const(0));
    resolver.insert([names::TRUE].into(), Item::Const(1));
    resolver.insert([names::NONE].into(), Item::Const(2));

    Context {
        functions,
        constants,
        resolver,
        names,
        types,
    }
}

// | +   | int | reg | f64 |
// | int | int | int | f64 |
// | reg | int | reg | f64 |
// | f64 | f64 | f64 | f64 |

enum Pair {
    Int(i128, i128),
    Reg(usize, usize),
    Float(f64, f64),
}

fn prep_num(a: &BuiltIn, b: &BuiltIn) -> Option<Pair> {
    use BuiltIn::*;

    match (a, b) {
        (Int(a), Int(b)) => Some(Pair::Int(*a, *b)),
        (Int(a), Reg(b)) => Some(Pair::Int(*a, *b as i128)),
        (Int(a), Float(b)) => Some(Pair::Float(*a as f64, *b)),

        (Reg(a), Int(b)) => Some(Pair::Int(*a as i128, *b)),
        (Reg(a), Reg(b)) => Some(Pair::Reg(*a, *b)),
        (Reg(a), Float(b)) => Some(Pair::Float(*a as f64, *b)),

        (Float(a), Int(b)) => Some(Pair::Float(*a, *b as f64)),
        (Float(a), Reg(b)) => Some(Pair::Float(*a, *b as f64)),
        (Float(a), Float(b)) => Some(Pair::Float(*a, *b)),

        _ => Option::None,
    }
}

impl BuiltIn {
    pub fn type_index(&self) -> TypeIndex {
        match self {
            BuiltIn::None => raw_types::VOID,
            BuiltIn::Bool(_) => raw_types::BOOL,
            BuiltIn::Int(_) => raw_types::INT,
            BuiltIn::Reg(_) => raw_types::REG,
            BuiltIn::Float(_) => raw_types::F64,
            BuiltIn::Str(_, _) => raw_types::STR,
            BuiltIn::Vec(_, _) => raw_types::VEC,
            BuiltIn::Map(_, _) => raw_types::MAP,
            BuiltIn::Type(_) => raw_types::TYPE,
            BuiltIn::Func(_) => raw_types::FUNC,
            BuiltIn::Name(_) => raw_types::NAME,
        }
    }

    fn as_usize(&self) -> Option<usize> {
        match self {
            BuiltIn::None => None,
            BuiltIn::Bool(_) => None,
            BuiltIn::Int(n) => usize::try_from(*n).ok(),
            BuiltIn::Reg(n) => Some(*n),
            BuiltIn::Float(_) => None,
            BuiltIn::Str(_, _) => None,
            BuiltIn::Vec(_, _) => None,
            BuiltIn::Map(_, _) => None,
            BuiltIn::Type(_) => None,
            BuiltIn::Func(_) => None,
            BuiltIn::Name(_) => None,
        }
    }
}
