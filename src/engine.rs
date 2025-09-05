use super::builtin::{BuiltIn, Stores, Entry};
use super::tokenizer::CodeStr;
use litemap::LiteMap;
use std::sync::Arc;

use super::{
    TypeIndex, FuncIndex, NameIndex, ConstIndex, TypeList, Context,
    FuncData, Value, FuncRes, Panic, TypeData,
};

pub type Fields = LiteMap<NameIndex, Expression>;
pub type MatchArm = (TypeList, Expression);
pub type Block = Vec<Statement>;
pub type LocalIndex = usize;

#[allow(dead_code)]
#[derive(Clone, Debug, Default)]
pub enum Expression {
    Assignment(Option<Box<Expression>>, Box<Expression>),
    Method(Box<Expression>, NameIndex, Vec<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    Match(Box<Expression>, Vec<MatchArm>),
    Index(Box<Expression>, Box<Expression>),
    Field(Box<Expression>, NameIndex),
    If(Vec<(Expression, Block)>, Option<Block>),
    ConstStr(CodeStr<LocalIndex>),
    Struct(TypeIndex, Fields),
    Array(Vec<Expression>),
    Const(ConstIndex),
    Local(LocalIndex),
    Type(TypeIndex),
    Func(FuncIndex),
    Block(Block),
    Loop(Block),
    Integer(i128),
    Float(f64),
    Bool(bool),
    #[default]
    None,
}

#[derive(Clone, Debug)]
pub enum StatementData {
    For(Expression, Block),
    LocalPush(TypeList, Option<Expression>),
    While(Expression, Block),
    Return(Option<Expression>),
    Break(Option<Expression>),
    Eval(Expression),
    Continue,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub data: StatementData,
    pub line: usize,
}

/// Executor for functions from a [`Context`]
pub struct Engine {
    pub context: Arc<Context>,
    pub stores: Stores,
    locals: Vec<Value>,
    line: usize,
}

pub enum BlockExit {
    Return(Value),
    Panic(Panic),
    Break(Value),
    Continue,
}

impl Panic {
    fn as_exit<T>(self) -> Result<T, BlockExit> {
        Err(BlockExit::Panic(self))
    }
}

impl From<Panic> for BlockExit {
    fn from(res: Panic) -> Self {
        BlockExit::Panic(res)
    }
}

impl Engine {
    pub fn new(context: Arc<Context>) -> Self {
        Self {
            context,
            locals: vec![],
            stores: Stores::default(),
            line: 0,
        }
    }

    pub fn call(&mut self, i: FuncIndex, parameters: Vec<Value>) -> FuncRes {
        let ctx = self.context.clone();
        let mut func = &ctx.functions[i];
        let line_bck = self.line;
        let mut dbg_path = None;

        while let FuncData::Redirect(i) = &func.data {
            func = &ctx.functions[*i];
        }

        if false {
            let path = &func.canonical_path;
            let path = self.context.stringify_path(path);
            println!("<calling {path}>");
            dbg_path = Some(path);
        }

        let ret = match &func.data {
            FuncData::Redirect(_) => unreachable!(/* handled earlier */),
            FuncData::Rush(body) => {
                let backup = self.locals.drain(..).collect();
                let mut param_iter = parameters.into_iter();

                let msg = "value did not pass return type check";

                for (_name, spec) in &func.parameters {
                    let value = param_iter.next();
                    let value = value.unwrap_or_default();
                    let value = self.type_check(value, &spec, msg)?;
                    self.locals.push(value);
                }

                let _extra: Vec<_> = param_iter.collect();
                let panic = |msg| Err(Panic::new(msg, []));

                let ret = match self.run_block(body) {
                    Ok(ret) => Ok(ret),
                    Err(BlockExit::Return(ret)) => Ok(ret),
                    Err(BlockExit::Panic(panic)) => Err(panic),
                    Err(BlockExit::Break(_)) => panic("unexpected 'break'"),
                    Err(BlockExit::Continue) => panic("unexpected 'continue'"),
                };

                self.locals = backup;

                ret
            },
            FuncData::BuiltIn(cb) => cb(self, parameters),
        };

        let msg = "value did not pass return type check";

        let mut ret = match ret {
            Ok(value) => self.type_check(value, &func.return_type, msg),
            Err(x) => Err(x),
        };

        if let Err(panic) = &mut ret {
            let path = &func.canonical_path;
            let path = self.context.stringify_path(path);
            panic.add_to_call_stack(path, self.line);
        }

        self.line = line_bck;

        if let Some(name) = dbg_path {
            let ret = ret.as_ref().ok().unwrap();
            let mut ret_str = String::new();
            super::builtin::dump(&mut ret_str, self, &ret);
            println!("<ret of {name} = {ret_str}>");
        }

        ret
    }

    fn run_block(&mut self, block: &Block) -> Result<Value, BlockExit> {
        let local_offset = self.locals.len();
        let mut iter = block.iter();

        let ret = 'outer: loop {
            let Some(statement) = iter.next() else {
                break Ok(Value::from(BuiltIn::None));
            };

            self.line = statement.line;

            match &statement.data {
                StatementData::LocalPush(spec, maybe_expr) => {
                    let value = match maybe_expr {
                        Some(expr) => self.eval(expr)?,
                        None => Value::default(),
                    };

                    let msg = "value did not pass let binding type check";
                    let value = self.type_check(value, &spec, msg)?;
                    self.locals.push(value);
                },
                StatementData::For(iter, body) => {
                    let iter_val = self.eval(iter)?;

                    loop {
                        let this = iter_val.clone();
                        let method = self.context.names.next;
                        let item = self.method(this, method, vec![])?;

                        if let BuiltIn::None = item.built_in {
                            break;
                        }

                        self.locals.push(item);
                        let exit = self.run_block(body);
                        self.locals.pop();

                        match exit {
                            Ok(_value) => (),
                            Err(BlockExit::Break(_value)) => break,
                            Err(BlockExit::Continue) => continue,
                            Err(ret) => break 'outer Err(ret),
                        }
                    }
                },
                StatementData::While(cond_expr, body) => loop {
                    let condition = self.eval(cond_expr)?;
                    if let BuiltIn::Bool(false) = condition.built_in {
                        break;
                    }

                    match self.run_block(body) {
                        Ok(_value) => (),
                        Err(BlockExit::Break(_value)) => break,
                        Err(BlockExit::Continue) => continue,
                        Err(ret) => break 'outer Err(ret),
                    }
                },
                StatementData::Eval(expr) => match self.eval(expr)? {
                    value if value.type_index == super::VOID_TYPE => (),
                    other => break Ok(other),
                },
                StatementData::Return(maybe_expr) => {
                    let value = match maybe_expr {
                        Some(expr) => self.eval(expr)?,
                        None => Value::default(),
                    };

                    return Err(BlockExit::Return(value));
                },
                StatementData::Break(maybe_expr) => {
                    let value = match maybe_expr {
                        Some(expr) => self.eval(expr)?,
                        None => Value::default(),
                    };

                    return Err(BlockExit::Break(value));
                },
                StatementData::Continue => return Err(BlockExit::Continue),
            }
        };

        self.locals.truncate(local_offset);
        ret
    }

    fn eval(&mut self, expr: &Expression) -> Result<Value, BlockExit> {
        let value = match expr {
            Expression::Assignment(maybe_dst, src) => {
                self.assign(maybe_dst, src)?;
                Value::from(BuiltIn::None)
            },
            Expression::Match(subject, arms) => {
                let subject = self.eval(subject)?;
                let mut ret = None;

                for (type_list, expr) in arms {
                    if type_list.contains(&subject.type_index) {
                        ret = Some(self.eval(expr)?);
                        break;
                    }
                }

                let Some(ret) = ret else {
                    return Panic::new("no match found", []).as_exit();
                };

                ret
            },
            Expression::Method(subject, name, parameters) => {
                let this = self.eval(subject)?;
                let mut out_param = Vec::new();

                for expr in parameters {
                    out_param.push(self.eval(expr)?);
                }

                self.method(this, *name, out_param)?
            },
            Expression::FnCall(expr, parameters) => {
                let func = self.eval(expr)?;
                let mut out_param = Vec::new();

                let BuiltIn::Func(func_i) = func.built_in else {
                    return Panic::new("not a function", []).as_exit();
                };

                for expr in parameters {
                    out_param.push(self.eval(expr)?);
                }

                self.call(func_i, out_param)?
            },
            Expression::Index(subject, index) => {
                let this = self.eval(subject)?;
                let index = self.eval(index)?;
                self.method(this, self.context.names.get, vec![index])?
            },
            Expression::Field(subject, name) => {
                let this = self.eval(subject)?;
                let index = Value::from(BuiltIn::Name(*name));
                self.method(this, self.context.names.get, vec![index])?
            },
            Expression::If(checks, fallback) => {
                let mut ret = None;

                for (condition, then) in checks {
                    let cond_value = self.eval(condition)?.built_in;

                    let BuiltIn::Bool(condition) = cond_value else {
                        let msg = "non-boolean condition";
                        return Panic::new(msg, []).as_exit();
                    };

                    if condition {
                        ret = Some(self.run_block(then)?);
                        break;
                    }
                }

                match ret {
                    Some(value) => value,
                    None => match fallback {
                        Some(block) => self.run_block(block)?,
                        None => Value::default(),
                    },
                }
            },
            Expression::Array(items) => {
                let mut out = Vec::new();

                for expr in items {
                    out.push(self.eval(expr)?);
                }

                self.stores.new_vec(out)
            },
            Expression::Struct(type_index, fields) => {
                let ctx = self.context.clone();
                let handle = &ctx.types[*type_index];
                let mut remaining = fields.len();
                let mut entries = Vec::new();
                let none = Expression::None;

                let TypeData::Struct(field_defs) = &handle.data else {
                    return Panic::new("not a struct", []).as_exit();
                };

                for field in field_defs {
                    let maybe_expr = fields.get(&field.name);
                    remaining -= maybe_expr.is_some() as usize;

                    let msg = "value did not pass field type check";
                    let value = self.eval(maybe_expr.unwrap_or(&none))?;
                    let value = self.type_check(value, &field.spec, msg)?;

                    let entry = Entry {
                        key: Value::from(BuiltIn::Name(field.name)),
                        value,
                    };

                    entries.push(entry);
                }

                if remaining > 0 {
                    return Panic::new("unexpected field(s)", []).as_exit();
                }

                let index = Some(*type_index);
                self.stores.new_map(index, entries, false)
            },
            Expression::ConstStr(data) => {
                let mut string = String::new();

                for part in &data.dyn_parts {
                    string += &part.prefix;

                    let this = self.locals[part.local].clone();
                    let ret = self.method(this, self.context.names.dump, vec![])?;

                    let BuiltIn::Str(i, _rc) = ret.built_in else {
                        let msg = "dump method returned non-string";
                        return Panic::new(msg, []).as_exit();
                    };

                    string += self.stores.get_str(i).unwrap();
                }

                string += &data.lit_end;
                self.stores.new_string(string)
            },
            Expression::Local(i) => self.locals[*i].clone(),
            Expression::Const(i) => {
                let ctx = self.context.clone();
                self.eval(&ctx.constants[*i])?
            },
            Expression::Type(index) => BuiltIn::Type(*index).into(),
            Expression::Func(index) => BuiltIn::Func(*index).into(),
            Expression::Block(body) => self.run_block(body)?,
            Expression::Loop(body) => loop {
                match self.run_block(body) {
                    Ok(_value) => (),
                    Err(BlockExit::Break(value)) => break value,
                    Err(BlockExit::Continue) => (),
                    Err(ret) => return Err(ret),
                }
            },
            Expression::Integer(int) => BuiltIn::Int(*int).into(),
            Expression::Float(float) => BuiltIn::Float(*float).into(),
            Expression::Bool(inner) => BuiltIn::Bool(*inner).into(),
            Expression::None => BuiltIn::None.into(),
        };

        Ok(value)
    }

    fn assign(&mut self, maybe_dst: &Option<Box<Expression>>, src: &Expression) -> Result<(), BlockExit> {
        let new_value = self.eval(src)?;

        let Some(dst) = maybe_dst else {
            return Ok(());
        };

        let (subject, index) = match &**dst {
            Expression::Field(subject, name) => (subject, Value::from(BuiltIn::Name(*name))),
            Expression::Index(subject, index) => (subject, self.eval(index)?),
            Expression::Local(i) => {
                self.locals[*i] = new_value;
                return Ok(());
            },
            _other => {
                let msg = "cannot assign to this";
                return Panic::new(msg, []).as_exit();
            },
        };

        let this = self.eval(subject)?;
        let parameters = vec![index, new_value];
        let _ret = self.method(this, self.context.names.set, parameters)?;

        Ok(())
    }

    fn method(&mut self, this: Value, name: NameIndex, mut parameters: Vec<Value>) -> FuncRes {
        parameters.insert(0, this.clone());

        let bg_type_index = this.built_in.type_index();
        let fg_type = &self.context.types[this.type_index];
        let bg_type = &self.context.types[bg_type_index];

        if let Some(func_i) = fg_type.methods.get(&name) {
            return self.call(*func_i, parameters);
        }

        if let Some(func_i) = bg_type.methods.get(&name) {
            return self.call(*func_i, parameters);
        }

        let type_val = Value::from(BuiltIn::Type(this.type_index));
        let name_val = Value::from(BuiltIn::Name(name));

        Err(Panic::new("method not found", [type_val, name_val]))
    }

    fn type_check(&mut self, value: Value, spec: &[TypeIndex], panic_msg: &'static str) -> FuncRes {
        if spec.is_empty() {
            return Ok(value);
        }

        if spec.contains(&value.type_index) {
            return Ok(value);
        }

        if spec.contains(&value.built_in.type_index()) {
            return Ok(Value::from(value.built_in));
        }

        let mut failed = Vec::new();
        let ctx = self.context.clone();

        for index in spec {
            let type_val = Value::from(BuiltIn::Type(*index));
            let from = &self.context.names.from;
            let handle = &ctx.types[*index];
            failed.push(type_val.clone());

            if let TypeData::Alias(list) = &handle.data {
                match self.type_check(value.clone(), list, panic_msg) {
                    Err(panic) if panic.message == panic_msg => continue,
                    other => return other,
                };
            }

            let Some(func_i) = handle.methods.get(from) else {
                continue;
            };

            let ret = self.call(*func_i, vec![value.clone()])?;

            if ret.type_index == super::VOID_TYPE {
                continue;
            }

            if ret.type_index != *index {
                let values = [type_val, value.clone(), ret];
                let message = "unexpected 'from' method return value";
                return Err(Panic::new(message, values));
            }

            return Ok(ret);
        }

        let failed = self.stores.new_vec(failed);
        Err(Panic::new(panic_msg, [value, failed]))
    }
}

impl From<BuiltIn> for Value {
    fn from(built_in: BuiltIn) -> Self {
        let type_index = built_in.type_index();

        Self {
            built_in,
            type_index,
        }
    }
}
