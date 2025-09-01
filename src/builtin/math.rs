use super::*;

pub fn add(engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    use BuiltIn::*;

    let [a, b] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    if let Some(pair) = prep_num(&a.built_in, &b.built_in) {
        let result = match pair {
            Pair::Int(a, b) => a.checked_add(b).map(BuiltIn::Int),
            Pair::Reg(a, b) => a.checked_add(b).map(BuiltIn::Reg),
            Pair::Float(a, b) => Some(BuiltIn::Float(a + b)),
        };

        return Ok(Value::from(result.unwrap_or_default()));
    }

    match (&a.built_in, &b.built_in) {
        (Str(a, _rc1), Str(b, _rc2)) => {
            let get = |i| engine.stores.get_str(i).unwrap();
            let string = get(*a).to_string() + get(*b);
            Ok(engine.stores.new_string(string))
        },
        (Vec(a, _rc1), Vec(b, _rc2)) => {
            let get = |i| engine.stores.get_vec(i).unwrap();
            let mut vec = get(*a).to_vec();
            vec.extend_from_slice(get(*b));
            Ok(engine.stores.new_vec(vec))
        },
        _other => Err(Panic::new("unsupported values", [])),
    }
}

pub fn subtract(_engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [a, b] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let Some(pair) = prep_num(&a.built_in, &b.built_in) else {
        return Err(Panic::new("unsupported values", []));
    };

    let result = match pair {
        Pair::Int(a, b) => a.checked_sub(b).map(BuiltIn::Int),
        Pair::Reg(a, b) => a.checked_sub(b).map(BuiltIn::Reg),
        Pair::Float(a, b) => Some(BuiltIn::Float(a - b)),
    };

    Ok(Value::from(result.unwrap_or_default()))
}
