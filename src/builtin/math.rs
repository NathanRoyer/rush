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

pub fn clamp(_engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this, min, max] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let Some(pair) = prep_num(&this.built_in, &max.built_in) else {
        return Err(Panic::new("unsupported values", []));
    };

    let result = match pair {
        Pair::Int(this, max) => BuiltIn::Int(i128::min(this, max)),
        Pair::Reg(this, max) => BuiltIn::Reg(usize::min(this, max)),
        Pair::Float(this, max) => BuiltIn::Float(f64::min(this, max)),
    };

    let Some(pair) = prep_num(&result, &min.built_in) else {
        return Err(Panic::new("unsupported values", []));
    };

    let result = match pair {
        Pair::Int(this, min) => BuiltIn::Int(i128::max(this, min)),
        Pair::Reg(this, min) => BuiltIn::Reg(usize::max(this, min)),
        Pair::Float(this, min) => BuiltIn::Float(f64::max(this, min)),
    };

    Ok(Value::from(result))
}

pub fn abs(_engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let result = match &this.built_in {
        BuiltIn::Float(num) => BuiltIn::Float(num.abs()),
        BuiltIn::Int(num) => BuiltIn::Int(num.abs()),
        _other => return Err(Panic::new("unsupported values", [])),
    };

    Ok(Value::from(result))
}

pub const OP_ROUND: usize = 0;
pub const OP_FLOOR: usize = 0;
pub const OP_CEIL: usize = 0;
pub const OP_FRACT: usize = 0;

pub fn f64_op<const OP: usize>(_engine: &mut Engine, parameters: Vec<Value>) -> FuncRes {
    let [this] = parameters.as_slice() else {
        return Err(Panic::new("bad parameters", []));
    };

    let BuiltIn::Float(this) = &this.built_in else {
        return Err(Panic::new("unsupported values", []));
    };

    let float = match () {
        () if OP == OP_ROUND => this.round(),
        () if OP == OP_FLOOR => this.floor(),
        () if OP == OP_CEIL => this.ceil(),
        () if OP == OP_FRACT => {
            let built_in = BuiltIn::Float(this.fract());
            return Ok(Value::from(built_in));
        },
        _ => unreachable!(),
    };

    let reg = float as usize;
    let int = float as i128;

    let result = if (reg as f64) == float {
        BuiltIn::Reg(reg)
    } else if (int as f64) == float {
        BuiltIn::Int(int)
    } else {
        BuiltIn::Float(float)
    };

    Ok(Value::from(result))
}
