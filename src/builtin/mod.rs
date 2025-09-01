use super::{NameIndex, TypeIndex, FuncIndex, Context, Value, FuncRes, Panic, Item};
use super::engine::{Engine, Expression};
use std::cmp::Ordering;
use std::fmt::Write;
use std::sync::Arc;

pub use store::Stores;
pub use util::dump;
use util::{println, dump_entry, get, set, ordering, len, has, nth};
use math::{OP_ROUND, OP_FLOOR, OP_FRACT, OP_CEIL};
use math::{add, subtract, clamp, abs, f64_op};

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
    Bool(bool),
    Int(i128),
    Reg(usize),
    Float(f64),
    Str(StrIndex, RefCount),
    Vec(VecIndex, RefCount),
    Map(MapIndex, RefCount),
    Type(TypeIndex),
    Func(FuncIndex),
    Name(NameIndex),
}

#[repr(usize)]
enum BuiltInType {
    Void = super::VOID_TYPE,
    Bool,
    Int,
    Reg,
    Float,
    Str,
    Vec,
    Map,
    Type,
    Func,
    Name,
}

pub fn init(ctx: &mut Context) {
    ctx.built_in_funcs.insert("greater", ordering::<1>);
    ctx.built_in_funcs.insert("equal", ordering::<0>);
    ctx.built_in_funcs.insert("less", ordering::<-1>);
    ctx.built_in_funcs.insert("subtract", subtract);
    ctx.built_in_funcs.insert("println", println);
    ctx.built_in_funcs.insert("dump", dump_entry);
    ctx.built_in_funcs.insert("clamp", clamp);
    ctx.built_in_funcs.insert("add", add);
    ctx.built_in_funcs.insert("get", get);
    ctx.built_in_funcs.insert("set", set);
    ctx.built_in_funcs.insert("abs", abs);
    ctx.built_in_funcs.insert("len", len);
    ctx.built_in_funcs.insert("has", has);

    ctx.built_in_funcs.insert("nth_key", nth::<true>);
    ctx.built_in_funcs.insert("nth_value", nth::<false>);

    ctx.built_in_funcs.insert("round", f64_op::<OP_ROUND>);
    ctx.built_in_funcs.insert("floor", f64_op::<OP_FLOOR>);
    ctx.built_in_funcs.insert("fract", f64_op::<OP_FRACT>);
    ctx.built_in_funcs.insert("ceil", f64_op::<OP_CEIL>);

    ctx.constants.push(Expression::Bool(false));
    ctx.constants.push(Expression::Bool(true));
    ctx.constants.push(Expression::None);

    let false_n = ctx.names.get("false");
    let true_n = ctx.names.get("true");
    let none_n = ctx.names.get("NONE");

    ctx.resolver.insert([false_n].into(), Item::Const(0));
    ctx.resolver.insert([true_n].into(), Item::Const(1));
    ctx.resolver.insert([none_n].into(), Item::Const(2));
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
            BuiltIn::None => BuiltInType::Void as _,
            BuiltIn::Bool(_) => BuiltInType::Bool as _,
            BuiltIn::Int(_) => BuiltInType::Int as _,
            BuiltIn::Reg(_) => BuiltInType::Reg as _,
            BuiltIn::Float(_) => BuiltInType::Float as _,
            BuiltIn::Str(_, _) => BuiltInType::Str as _,
            BuiltIn::Vec(_, _) => BuiltInType::Vec as _,
            BuiltIn::Map(_, _) => BuiltInType::Map as _,
            BuiltIn::Type(_) => BuiltInType::Type as _,
            BuiltIn::Func(_) => BuiltInType::Func as _,
            BuiltIn::Name(_) => BuiltInType::Name as _,
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
