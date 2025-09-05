use super::{NameIndex, TypeIndex, FuncIndex, Context, Value, FuncRes, Panic, Item};
use super::engine::{Engine, Expression};
use std::cmp::Ordering;
use std::fmt::Write;
use std::sync::Arc;

pub use store::{Stores, Entry, ValueMap, StrIndex, VecIndex, MapIndex, RefCount};
pub use util::dump;

use util::{println, dump_entry, get, set, ordering, different, len, has, nth};
use math::{add, subtract, clamp, abs};

use math::{OP_ROUND, OP_FLOOR, OP_FRACT, OP_CEIL, f64_op};
use util::{T_F64, T_REG, T_INT, num_from};

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

/// Core underlying object of all [`Value`]s
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

// note: order matters
#[repr(usize)]
pub enum BuiltInType {
    Void = 0,
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
    ctx.built_in_funcs.insert("different", different);
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

    ctx.built_in_funcs.insert("reg_from", num_from::<T_REG>);
    ctx.built_in_funcs.insert("int_from", num_from::<T_INT>);
    ctx.built_in_funcs.insert("f64_from", num_from::<T_F64>);

    let mut push_const = |name: &str, expr: Expression| {
        let item = Item::Const(ctx.constants.len());
        ctx.constants.push(expr);
        let path = [ctx.names.get(name)];
        ctx.resolver.insert(path.into(), item);
    };

    push_const("false", Expression::Bool(false));
    push_const("true", Expression::Bool(true));
    push_const("NONE", Expression::None);
}

// makes sure the stdlib maps well with the internal constants
pub fn check_builtin_types(ctx: &Context) {
    let check = |name: &str, index: TypeIndex| {
        let Some(n) = ctx.types[index].canonical_path.last() else {
            panic!("No canonical_path for built-in type {name}");
        };

        let item = &ctx.names[*n];

        if name != item {
            panic!("built-in type {name} has unexpected name: {item}");
        }
    };

    assert_eq!(super::VOID_TYPE, BuiltInType::Void as _);

    check("void", BuiltInType::Void as _);
    check("bool", BuiltInType::Bool as _);
    check("int", BuiltInType::Int as _);
    check("reg", BuiltInType::Reg as _);
    check("f64", BuiltInType::Float as _);
    check("Str", BuiltInType::Str as _);
    check("Vec", BuiltInType::Vec as _);
    check("Map", BuiltInType::Map as _);
    check("type_ref", BuiltInType::Type as _);
    check("func_ref", BuiltInType::Func as _);
    check("name", BuiltInType::Name as _);
}

enum Pair {
    Int(i128, i128),
    Reg(usize, usize),
    Float(f64, f64),
}

fn prep_num(a: &BuiltIn, b: &BuiltIn) -> Option<Pair> {
    use BuiltIn::*;

    // | +   | int | reg | f64 |
    // | int | int | int | f64 |
    // | reg | int | reg | f64 |
    // | f64 | f64 | f64 | f64 |

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
