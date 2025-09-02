use std::mem::replace;
use super::*;

pub fn println(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    for value in parameters {
        let BuiltIn::Str(i, _rc) = value.built_in else {
            return Err(Panic::new("not a string", []));
        };

        let string = engine.stores.get_str(i).unwrap();
        print!("{string}");
    }

    println!("");
    Ok(Value::default())
}

pub fn dump_entry(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [value] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let mut repr = String::new();
    dump(&mut repr, engine, value);
    Ok(engine.stores.new_string(repr))
}

pub fn dump(dst: &mut String, engine: &Engine, value: &Value) {
    let stores = &engine.stores;
    match &value.built_in {
        BuiltIn::None => *dst += "<none>",
        BuiltIn::Int(inner) => _ = write!(dst, "{inner}"),
        BuiltIn::Reg(inner) => _ = write!(dst, "{inner}"),
        BuiltIn::Bool(inner) => _ = write!(dst, "{inner}"),
        BuiltIn::Float(inner) => _ = write!(dst, "{inner}"),
        BuiltIn::Str(i, _rc) => *dst += stores.get_str(*i).unwrap(),
        BuiltIn::Vec(i, _rc) => {
            *dst += "[ ";

            for item in stores.get_vec(*i).unwrap() {
                dump(dst, engine, item);
                *dst += ", ";
            }

            *dst += "]";
        },
        BuiltIn::Map(i, _rc) => {
            // todo: show type

            *dst += "{ ";
            let map = stores.get_map(*i).unwrap();

            for entry in &map.inner {
                dump(dst, engine, &entry.key);
                *dst += ": ";
                dump(dst, engine, &entry.value);
                *dst += ", ";
            }

            *dst += "}";
        },
        BuiltIn::Type(i) => {
            *dst += "<";
            let ctx = &engine.context;
            let path = &ctx.types[*i].canonical_path;
            *dst += &ctx.stringify_path(path);
            *dst += ">";
        },
        BuiltIn::Func(i) => {
            *dst += "<";
            let ctx = &engine.context;
            let path = &ctx.functions[*i].canonical_path;
            *dst += &ctx.stringify_path(path);
            *dst += ">";
        },
        BuiltIn::Name(i) => *dst += &engine.context.names[*i],
    }
}

pub fn ordering<const O: i8>(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [a, b] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let o = cmp(&engine, a, b) as i8;
    Ok(Value::from(BuiltIn::Bool(o == O)))
}

pub fn different(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [a, b] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let result = cmp(&engine, a, b) != Ordering::Equal;
    Ok(Value::from(BuiltIn::Bool(result)))
}

fn map_find(engine: &Engine, map: MapIndex, index: &Value) -> Result<usize, usize> {
    let map = engine.stores.get_map(map).unwrap();
    let entry_cmp = |a: &Entry| cmp(engine, &a.key, index);

    if map.sorted {
        map.inner.binary_search_by(entry_cmp)
    } else {
        for (i, entry) in map.inner.iter().enumerate() {
            if let Ordering::Equal = entry_cmp(entry) {
                return Ok(i);
            }
        }

        Err(map.inner.len())
    }
}

pub fn cmp(engine: &Engine, a: &Value, b: &Value) -> Ordering {
    let (a, b) = (&a.built_in, &b.built_in);
    let stores = &engine.stores;

    // is this a pair of numbers?

    if let Some(pair) = prep_num(a, b) {
        return match pair {
            Pair::Int(a, b) => i128::cmp(&a, &b),
            Pair::Reg(a, b) => usize::cmp(&a, &b),
            Pair::Float(a, b) if a == b => Ordering::Equal,
            Pair::Float(a, b) => f64::total_cmp(&a, &b),
        };
    }

    // maybe a pair of strings?

    if let (BuiltIn::Name(a), BuiltIn::Str(b, _rc2)) = (a, b) {
        let a = &engine.context.names[*a];
        let b = stores.get_str(*b).unwrap();
        return str::cmp(a, b);
    }

    if let (BuiltIn::Str(a, _rc1), BuiltIn::Name(b)) = (a, b) {
        let a = stores.get_str(*a).unwrap();
        let b = &engine.context.names[*b];
        return str::cmp(a, b);
    }

    // other values cannot be cast-compared

    let a_type = a.type_index();
    let b_type = b.type_index();
    let type_cmp = TypeIndex::cmp(&a_type, &b_type);

    let Ordering::Equal = type_cmp else {
        return type_cmp;
    };

    if let (BuiltIn::Name(a), BuiltIn::Name(b)) = (a, b) {
        let a = &engine.context.names[*a];
        let b = &engine.context.names[*b];
        return str::cmp(a, b);
    }

    if let (BuiltIn::None, BuiltIn::None) = (a, b) {
        return Ordering::Equal;
    }

    if let (BuiltIn::Bool(a), BuiltIn::Bool(b)) = (a, b) {
        return bool::cmp(a, b);
    }

    if let (BuiltIn::Str(a, _rc1), BuiltIn::Str(b, _rc2)) = (a, b) {
        let a = stores.get_str(*a).unwrap();
        let b = stores.get_str(*b).unwrap();
        return str::cmp(a, b);
    }

    if let (BuiltIn::Vec(a, _rc1), BuiltIn::Vec(b, _rc2)) = (a, b) {
        let a = stores.get_vec(*a).unwrap();
        let b = stores.get_vec(*b).unwrap();
        let mut i = 0;

        return loop {
            match (a.get(i), b.get(i)) {
                (Some(a), Some(b)) => match cmp(engine, a, b) {
                    Ordering::Equal => i += 1,
                    other => break other,
                },
                (Some(_), None) => break Ordering::Greater,
                (None, Some(_)) => break Ordering::Less,
                (None, None) => break Ordering::Equal,
            }
        };
    }

    let (BuiltIn::Map(a, _rc1), BuiltIn::Map(b, _rc2)) = (a, b) else {
        println!("[rush] internal cmp error: {:?} & {:?}", a, b);
        return Ordering::Equal;
    };

    let a = &stores.get_map(*a).unwrap().inner;
    let b = &stores.get_map(*b).unwrap().inner;
    let mut i = 0;

    loop {
        match (a.get(i), b.get(i)) {
            (Some(a), Some(b)) => match cmp(engine, &a.key, &b.key) {
                Ordering::Equal => match cmp(engine, &a.value, &b.value) {
                    Ordering::Equal => i += 1,
                    other => break other,
                },
                other => break other,
            },
            (Some(_), None) => break Ordering::Greater,
            (None, Some(_)) => break Ordering::Less,
            (None, None) => break Ordering::Equal,
        }
    }
}

fn first_class_map(this: &Value) -> bool {
    this.type_index == (BuiltInType::Map as TypeIndex)
}

pub fn get(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this, index] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    if let BuiltIn::Vec(i, _rc) = &this.built_in {
        let vec = engine.stores.get_vec(*i).unwrap();

        let Some(j) = index.built_in.as_usize() else {
            return Err(Panic::new("invalid vector index", []));
        };

        return Ok(vec.get(j).cloned().unwrap_or_default());
    }

    let BuiltIn::Map(i, _rc) = &this.built_in else {
        return Err(Panic::new("unsupported value", []));
    };

    let index_i = map_find(engine, *i, index);
    let map = engine.stores.get_map(*i).unwrap();

    match index_i {
        Ok(i) => Ok(map.inner[i].value.clone()),
        Err(_) if first_class_map(&this) => Ok(Value::default()),
        Err(_) => Err(Panic::new("no such field", [])),
    }
}

pub fn set(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let Ok([this, index, new_value]) = <[Value; 3]>::try_from(parameters) else {
        return Err(Panic::new("bad parameters", []));
    };

    if let BuiltIn::Vec(i, _rc) = &this.built_in {
        let vec = engine.stores.get_vec_mut(*i).unwrap();

        let Some(j) = index.built_in.as_usize() else {
            return Err(Panic::new("invalid vector index", []));
        };

        let Some(slot) = vec.get_mut(j) else {
            return Err(Panic::new("no slot at index", []));
        };

        return Ok(replace(slot, new_value));
    }

    let BuiltIn::Map(i, _rc) = &this.built_in else {
        return Err(Panic::new("unsupported value", []));
    };

    let index_i = map_find(engine, *i, &index);
    let map = engine.stores.get_map_mut(*i).unwrap();

    match index_i {
        Ok(i) => Ok(replace(&mut map.inner[i].value, new_value)),
        Err(i) if first_class_map(&this) => {
            let entry = Entry {
                key: index,
                value: new_value,
            };

            map.inner.insert(i, entry);
            Ok(Value::default())
        },
        Err(_) => Err(Panic::new("no such field", [])),
    }
}

pub fn len(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let s = &engine.stores;

    let result = match &this.built_in {
        BuiltIn::Vec(i, _rc) => s.get_vec(*i).unwrap().len(),
        BuiltIn::Str(i, _rc) => s.get_str(*i).unwrap().len(),
        _ => return Err(Panic::new("unsupported value", [])),
    };

    Ok(Value::from(BuiltIn::Reg(result)))
}

pub fn has(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this, index] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let s = &engine.stores;

    let result = match &this.built_in {
        BuiltIn::Vec(i, _rc) => {
            let Some(j) = index.built_in.as_usize() else {
                return Ok(Value::from(BuiltIn::Bool(false)));
            };

            j < s.get_vec(*i).unwrap().len()
        },
        BuiltIn::Map(i, _rc) => map_find(engine, *i, index).is_ok(),
        _ => return Err(Panic::new("unsupported value", [])),
    };

    Ok(Value::from(BuiltIn::Bool(result)))
}

pub fn nth<const KEY: bool>(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this, index] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let Some(j) = index.built_in.as_usize() else {
        return Err(Panic::new("unsupported value", []));
    };

    let BuiltIn::Map(i, _rc) = &this.built_in else {
        return Err(Panic::new("unsupported value", []));
    };

    let map = engine.stores.get_map(*i).unwrap();
    let Some(entry) = &map.inner.get(j) else {
        return Ok(Value::default());
    };

    match KEY {
        true => Ok(entry.key.clone()),
        false => Ok(entry.value.clone()),
    }
}

pub const T_F64: usize = 0;
pub const T_REG: usize = 1;
pub const T_INT: usize = 2;

pub fn num_from<const T: usize>(_engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {

    let [value] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let built_in = match () {
        () if T == T_F64 => match &value.built_in {
            BuiltIn::Reg(reg) => BuiltIn::Float(*reg as f64),
            BuiltIn::Int(int) => BuiltIn::Float(*int as f64),
            _ => BuiltIn::None,
        },
        () if T == T_INT => match &value.built_in {
            BuiltIn::Reg(reg) => BuiltIn::Int(*reg as i128),
            BuiltIn::Float(f) => {
                let trial = *f as i128;
                (trial as f64 == *f)
                    .then_some(BuiltIn::Int(trial))
                    .unwrap_or_default()
            },
            _ => BuiltIn::None,
        },
        () if T == T_REG => match &value.built_in {
            BuiltIn::Int(int) => {
                let trial = *int as usize;
                (trial as i128 == *int)
                    .then_some(BuiltIn::Reg(trial))
                    .unwrap_or_default()
            },
            BuiltIn::Float(f) => {
                let trial = *f as usize;
                (trial as f64 == *f)
                    .then_some(BuiltIn::Reg(trial))
                    .unwrap_or_default()
            },
            _ => BuiltIn::None,
        },
        _ => BuiltIn::None,
    };

    Ok(Value::from(built_in))
}
