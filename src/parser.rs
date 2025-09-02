use super::tokenizer::{Token, TokenData, CodeStr, DynPart};
use super::engine::{LocalIndex, Statement, Expression, MatchArm};

use super::{
    Function, TypeData, Field, NameIndex, ItemPath, Names,
    Context, TypeIndex, FuncData, TypeList, Item, Type,
};

use std::mem::replace;
use litemap::LiteMap;
use std::sync::Arc;

const PRIO_LIMIT: u8 = 3;
const ANY_TYPE: TypeIndex = TypeIndex::MAX;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MathOp {
    Different,
    Equal,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Divide,
    Multiply,
    Subtract,
    Add,
}

struct ExprPart {
    left_hand: Expression,
    math_op: MathOp,
}

type Tokens<'a> = std::slice::Iter<'a, TokenData<'a>>;
type MaybeToken<'a> = Option<&'a Token<'a>>;
type ParamDef = (NameIndex, TypeList);

fn tok<'a, 'b>(data: &'b TokenData<'a>) -> &'b Token<'a> {
    &**data
}

pub struct Error {
    pub line: usize,
    pub message: &'static str,
}

type Res<T> = Result<T, Error>;

pub fn parse(
    ctx: &mut Context,
    mod_name: &str,
    tokens: &[TokenData],
) -> Res<()> {
    let context = replace(ctx, Context::empty());

    let mut parser = Parser {
        context,
        current_path: Vec::new(),
        locals: Vec::new(),
        line: 1,
    };

    let mod_name = parser.get_name(mod_name)?;
    parser.set_mod_path(&[mod_name]);
    parser.declare_items(&mut tokens.iter())?;
    parser.declare_methods(&mut tokens.iter())?;
    parser.define_items(&mut tokens.iter())?;

    *ctx = parser.context;

    Ok(())
}

struct Parser {
    current_path: Vec<NameIndex>,
    locals: Vec<String>,
    context: Context,
    line: usize,
}

type Decl = fn(&mut Parser) -> Item;

impl Parser {
    fn set_mod_path(&mut self, mod_path: &[NameIndex]) {
        self.current_path = mod_path.to_vec();
    }

    fn error<T>(&self, message: &'static str) -> Res<T> {
        Err(Error {
            line: self.line,
            message,
        })
    }

    fn declare_items(&mut self, iter: &mut Tokens) -> Res<()> {
        while let Some(token) = iter.next() {
            self.line = token.line;

            let maybe_decl = match &**token {
                Token::Const => Some(Self::decl_const as Decl),
                Token::Struct => Some(Self::decl_type as Decl),
                Token::Type => Some(Self::decl_type as Decl),
                Token::Fn => Some(Self::decl_fn as Decl),
                Token::Impl => None,
                Token::Use => {
                    self.parse_use(iter)?;
                    continue;
                },
                _other => return self.error("unexpected token"),
            };

            if let Some(decl) = maybe_decl {
                let name = self.parse_name(iter)?;
                let path = self.rel_path(name);
                let item_link = decl(self);
                if self.context.resolver.insert(path, item_link).is_some() {
                    return self.error("another item already has this name");
                }
            }

            self.skip_item(iter)?;
        }

        Ok(())
    }

    fn decl_fn(&mut self) -> Item {
        let dummy = Function {
            canonical_path: Arc::new([]),
            parameters: Vec::new(),
            return_type: Vec::new(),
            data: FuncData::Rush(Vec::new()),
        };

        let index = self.context.functions.len();
        self.context.functions.push(dummy);
        Item::Function(index)
    }

    fn decl_type(&mut self) -> Item {
        let dummy = Type {
            canonical_path: Arc::new([]),
            data: TypeData::Struct(Vec::new()),
            methods: LiteMap::new(),
        };

        let index = self.context.types.len();
        self.context.types.push(dummy);
        Item::Type(index)
    }

    fn decl_const(&mut self) -> Item {
        let index = self.context.constants.len();
        self.context.constants.push(Expression::None);
        Item::Const(index)
    }

    fn index_of(&mut self, name: NameIndex) -> usize {
        self.current_path.push(name);
        let slice = self.current_path.as_slice();
        let index = self.context.resolver.get(slice).unwrap();
        self.current_path.pop();

        match index {
            Item::Function(index) => *index,
            Item::Const(index) => *index,
            Item::Type(index) => *index,
            _other => unreachable!(),
        }
    }

    fn declare_methods(&mut self, iter: &mut Tokens) -> Res<()> {
        while let Some(token) = iter.next() {
            self.line = token.line;

            match &**token {
                Token::Impl => {
                    self.decl_impl(iter)?;
                    continue;
                },
                Token::Struct => (),
                Token::Const => (),
                Token::Type => (),
                Token::Use => (),
                Token::Fn => (),
                _other => return self.error("unexpected token"),
            }

            self.skip_item(iter)?;
        }

        Ok(())
    }

    fn decl_impl(&mut self, iter: &mut Tokens) -> Res<()> {
        let index = self.parse_type_ref(iter, false)?;

        let Some(Token::Braced(tokens)) = iter.next().map(tok) else {
            return self.error("expected '{{'");
        };

        let mut iter = tokens.iter();

        while let Some(token) = iter.next() {
            self.line = token.line;

            let Token::Fn = &**token else {
                return self.error("expected 'fn'");
            };

            let name = self.parse_name(&mut iter)?;
            self.skip_item(&mut iter)?;

            let Item::Function(func_index) = self.decl_fn() else {
                unreachable!();
            };

            let handle = &mut self.context.types[index];
            if handle.methods.insert(name, func_index).is_some() {
                return self.error("another method already has this name");
            }
        }

        Ok(())
    }

    fn define_items(&mut self, iter: &mut Tokens) -> Res<()> {
        while let Some(token) = iter.next() {
            self.line = token.line;

            match &**token {
                Token::Struct => self.define_struct(iter)?,
                Token::Const => self.define_const(iter)?,
                Token::Type => self.define_alias(iter)?,
                Token::Impl => self.define_impl(iter)?,
                Token::Fn => self.define_fn(iter, None)?,
                Token::Use => self.skip_item(iter)?,
                _other => unreachable!(),
            }
        }

        Ok(())
    }

    fn parse_name(&mut self, iter: &mut Tokens) -> Res<NameIndex> {
        let Some(Token::Alphanumeric(name)) = iter.next().map(tok) else {
            return self.error("expected name");
        };

        self.get_name(name)
    }

    fn get_name(&mut self, name: &str) -> Res<NameIndex> {
        let first_char = name.chars().next().unwrap();

        if first_char != '_' && !first_char.is_alphabetic() {
            return self.error("invalid name");
        }

        Ok(self.context.names.get(name))
    }

    fn rel_path(&mut self, name: NameIndex) -> ItemPath {
        self.current_path.push(name);
        let ret = self.current_path.as_slice().into();
        self.current_path.pop();
        ret
    }

    fn skip_item(&mut self, iter: &mut Tokens) -> Res<()> {
        loop {
            let Some(next) = iter.next() else {
                return self.error("unexpected end of file");
            };

            match &**next {
                Token::Braced(_) => break,
                Token::Semi => break,
                _other => (),
            }
        }

        Ok(())
    }

    fn parse_use(&mut self, iter: &mut Tokens) -> Res<()> {
        let mut target = Vec::new();

        let last = loop {
            let name = self.parse_name(iter)?;
            target.push(name);

            match iter.next().map(tok) {
                Some(Token::Semi) => break name,
                Some(Token::Path) => (),
                _other => return self.error("expected ';' or '::'"),
            }
        };

        let target = target.as_slice().into();
        let item = Item::Redirect(target);
        let path = self.rel_path(last);

        if self.context.resolver.insert(path, item).is_some() {
            return self.error("another item already has this name");
        }

        Ok(())
    }

    fn define_const(&mut self, iter: &mut Tokens) -> Res<()> {
        let name = self.parse_name(iter)?;
        let index = self.index_of(name);

        let Some(Token::Equal) = iter.next().map(tok) else {
            return self.error("expected '='");
        };

        let (expr, Some(Token::Semi)) = self.parse_expr(iter)? else {
            return self.error("expected ';'");
        };

        self.context.constants[index] = expr;

        Ok(())
    }

    fn define_alias(&mut self, iter: &mut Tokens) -> Res<()> {
        let name = self.parse_name(iter)?;
        let canonical_path = self.rel_path(name);
        let index = self.index_of(name);

        let Some(Token::Equal) = iter.next().map(tok) else {
            return self.error("expected '='");
        };

        let (list, Some(Token::Semi)) = self.parse_type_spec(iter)? else {
            return self.error("expected '|' or ';'");
        };

        if list.is_empty() {
            return self.error("cannot alias 'any'");
        }

        let handle = &mut self.context.types[index];
        handle.canonical_path = canonical_path;
        handle.data = TypeData::Alias(list);

        Ok(())
    }

    fn define_impl(&mut self, iter: &mut Tokens) -> Res<()> {
        let type_index = self.parse_type_ref(iter, false)?;

        let Some(Token::Braced(tokens)) = iter.next().map(tok) else {
            return self.error("expected '{{'");
        };

        let mut iter = tokens.iter();

        while let Some(token) = iter.next() {
            self.line = token.line;

            let Token::Fn = &**token else {
                return self.error("expected 'fn'");
            };

            self.define_fn(&mut iter, Some(type_index))?;
        }

        Ok(())
    }

    fn define_struct(&mut self, iter: &mut Tokens) -> Res<()> {
        let name = self.parse_name(iter)?;
        let canonical_path = self.rel_path(name);
        let index = self.index_of(name);

        let Some(Token::Braced(tokens)) = iter.next().map(tok) else {
            return self.error("expected '{{'");
        };

        let mut iter = tokens.iter();
        let mut fields = Vec::new();

        while iter.len() != 0 {
            let name = self.parse_name(&mut iter)?;

            let Some(Token::Colon) = iter.next().map(tok) else {
                return self.error("expected ':'");
            };

            let (list, Some(Token::Comma)) = self.parse_type_spec(&mut iter)? else {
                return self.error("expected '|' or ','");
            };

            fields.push(Field {
                name,
                list,
            });
        }

        let handle = &mut self.context.types[index];
        handle.canonical_path = canonical_path;
        handle.data = TypeData::Struct(fields);

        Ok(())
    }

    fn define_fn(&mut self, iter: &mut Tokens, current_impl: Option<TypeIndex>) -> Res<()> {
        let name = self.parse_name(iter)?;

        let (index, allow_import) = match current_impl {
            Some(type_index) => {
                let handle = &self.context.types[type_index];
                (handle.methods[&name], true)
            },
            None => (self.index_of(name), false),
        };

        let next = iter.next().map(tok);

        if let (Some(Token::Semi), true) = (next, allow_import) {
            let item = self.resolve(name, &mut [].iter());

            let Ok(Some(Item::Function(src_i))) = item else {
                return self.error("imported function not found");
            };

            let src = &self.context.functions[src_i];
            self.context.functions[index] = src.clone();
            return Ok(());
        }

        let Some(Token::Tuple(param_tokens)) = next else {
            return self.error("expected parameters");
        };

        let mut par_iter = param_tokens.iter();
        let parameters = self.parse_param_defs(&mut par_iter)?;

        let (return_type, next) = match iter.next().map(tok) {
            Some(Token::SigRet) => self.parse_type_spec(iter)?,
            other => (vec![super::VOID_TYPE], other),
        };

        let data = match next {
            Some(Token::Semi) => {
                let name = &self.context.names[name];
                match self.context.built_in_funcs.get(name) {
                    Some(ptr) => FuncData::BuiltIn(*ptr),
                    None => return self.error("unknown builtin"),
                }
            },
            Some(Token::Braced(body)) => {
                for (name_i, _) in &parameters {
                    let name_str = &self.context.names[*name_i];
                    self.locals.push(name_str.to_string());
                }

                let body = self.parse_block(body)?;
                self.locals.clear();

                FuncData::Rush(body)
            },
            _other => return self.error("invalid body"),
        };

        let func = Function {
            canonical_path: self.rel_path(name),
            parameters,
            return_type,
            data,
        };

        self.context.functions[index] = func;

        Ok(())
    }

    fn parse_param_defs(&mut self, iter: &mut Tokens) -> Res<Vec<ParamDef>> {
        let mut params = Vec::new();

        while iter.len() != 0 {
            let name = self.parse_name(iter)?;

            let Some(Token::Colon) = iter.next().map(tok) else {
                return self.error("expected ':'");
            };

            let (list, next) = self.parse_type_spec(iter)?;
            params.push((name, list));

            match next {
                Some(Token::Comma) => (),
                Some(_other) => return self.error("unexpected token"),
                None => break,
            }
        }

        Ok(params)
    }

    fn resolve(&mut self, name: NameIndex, iter: &mut Tokens) -> Res<Option<Item>> {
        let mut path = self.current_path.clone();
        path.push(name);

        loop {
            let other = loop {
                match self.context.resolver.get(path.as_slice()) {
                    Some(Item::Redirect(dst)) => path = dst.to_vec(),
                    Some(other) => break other,
                    None => return Ok(None),
                }
            };

            if other != &Item::Group {
                break Ok(Some(other.clone()));
            }

            let Some(Token::Path) = iter.next().map(tok) else {
                return self.error("expected '::'");
            };

            path.push(self.parse_name(iter)?);
        }
    }

    fn parse_type_ref(&mut self, iter: &mut Tokens, allow_any: bool) -> Res<TypeIndex> {
        let first = match iter.next().map(tok) {
            Some(Token::Alphanumeric(name)) => self.get_name(name)?,
            Some(Token::Any) if allow_any => return Ok(ANY_TYPE),
            _other => return self.error("expected type ref"),
        };

        match self.resolve(first, iter)? {
            Some(Item::Type(i)) => Ok(i),
            _other => return self.error("invalid type"),
        }
    }

    fn parse_type_spec<'b, 'a>(
        &mut self,
        iter: &'b mut Tokens<'a>,
    ) -> Res<(Vec<TypeIndex>, Option<&'b Token<'a>>)> {
        let mut list = Vec::new();
        let mut allow_any = true;

        let next = loop {
            let index = self.parse_type_ref(iter, allow_any)?;

            if index == ANY_TYPE {
                break iter.next().map(tok);
            };

            list.push(index);
            allow_any = false;

            match iter.next().map(tok) {
                Some(Token::Or) => (),
                other => break other,
            }
        };

        Ok((list, next))
    }

    fn parse_block(&mut self, mut body: &[TokenData]) -> Res<Vec<Statement>> {
        let local_offset = self.locals.len();
        let mut statements = Vec::new();

        let skip = |body: &mut &[TokenData], iter: Tokens| {
            let offset = body.len() - iter.count();
            *body = &body[offset..];
        };

        while let Some(token) = body.first() {
            let mut iter = body[1..].iter();
            self.line = token.line;

            let mut opt_expr = || match iter.next().map(tok) {
                Some(Token::Semi) => Ok(None),
                _ => {
                    iter = body[1..].iter();
                    match self.parse_expr(&mut iter)? {
                        (expr, Some(Token::Semi)) => Ok(Some(expr)),
                        _ => return self.error("expected ';'"),
                    }
                },
            };

            let statement = match &**token {
                Token::Let => self.parse_let(&mut iter)?,
                Token::For => self.parse_for(&mut iter)?,
                Token::While => self.parse_while(&mut iter)?,
                Token::Break => Statement::Break(opt_expr()?),
                Token::Return => Statement::Return(opt_expr()?),
                Token::Continue => match iter.next().map(tok) {
                    Some(Token::Semi) => Statement::Continue,
                    _other => return self.error("expected ';'"),
                },
                _other => {
                    iter = body.iter();
                    let expr = match self.parse_expr(&mut iter)? {
                        (expr, Some(Token::Semi)) => {
                            Expression::Assignment(None, Box::new(expr))
                        },
                        (expr, None) => expr,
                        (_, Some(_other)) => return self.error("expected ';'"),
                    };

                    Statement::Eval(expr)
                },
            };

            skip(&mut body, iter);
            statements.push(statement);
        }

        self.locals.truncate(local_offset);
        Ok(statements)
    }

    fn parse_let(&mut self, iter: &mut Tokens) -> Res<Statement> {
        let Some(Token::Alphanumeric(name)) = iter.next().map(tok) else {
            return self.error("expected name");
        };

        let mut expr = None;

        let (spec, mut next) = match iter.next().map(tok) {
            Some(Token::Colon) => self.parse_type_spec(iter)?,
            other => (Vec::new(), other),
        };

        if let Some(Token::Equal) = next {
            let (e, n) = self.parse_expr(iter)?;
            expr = Some(e);
            next = n;
        }

        let Some(Token::Semi) = next else {
            return self.error("expected ';'");
        };

        self.locals.push(name.to_string());
        Ok(Statement::LocalPush(spec, expr))
    }

    fn parse_for(&mut self, iter: &mut Tokens) -> Res<Statement> {
        let Some(Token::Alphanumeric(name)) = iter.next().map(tok) else {
            return self.error("expected name");
        };

        let Some(Token::In) = iter.next().map(tok) else {
            return self.error("expected 'in'");
        };

        let (expr, Some(Token::Braced(body))) = self.parse_expr(iter)? else {
            return self.error("expected '{{'");
        };

        self.locals.push(name.to_string());
        let body = self.parse_block(body)?;
        let _name = self.locals.pop();

        Ok(Statement::For(expr, body))
    }

    fn parse_while(&mut self, iter: &mut Tokens) -> Res<Statement> {
        let (expr, Some(Token::Braced(body))) = self.parse_expr(iter)? else {
            return self.error("expected '{{'");
        };

        let body = self.parse_block(body)?;
        Ok(Statement::While(expr, body))
    }

    fn parse_expr<'a>(&mut self, iter: &mut Tokens<'a>) -> Res<(Expression, MaybeToken<'a>)> {
        let negate_m = self.context.names.negate;
        let mut parts = Vec::new();

        let wrap_negate = |negate: bool, expr: Expression| {
            match negate {
                true => Expression::Method(Box::new(expr), negate_m, Vec::new()),
                false => expr,
            }
        };

        loop {
            let (first, negate) = match iter.next().map(tok) {
                Some(Token::Minus) => (iter.next().map(tok), true),
                other => (other, false),
            };

            let Some(first) = first else {
                return self.error("expected expression");
            };

            let mut expr = self.parse_expr_base(first, iter)?;

            loop {
                let next = iter.next().map(tok);
                let other = match next {
                    Some(Token::Tuple(tokens)) => {
                        let mut parameters = Vec::new();
                        let mut iter = tokens.iter();

                        while iter.len() != 0 {
                            let expr = match self.parse_expr(&mut iter)? {
                                (expr, None) => expr,
                                (expr, Some(Token::Comma)) => expr,
                                _other => return self.error("expected comma"),
                            };

                            parameters.push(expr);
                        }

                        if let Expression::Field(subject, name) = expr {
                            expr = Expression::Method(subject, name, parameters);
                        } else {
                            expr = Expression::FnCall(Box::new(expr), parameters);
                        }

                        continue;
                    },
                    Some(Token::Path) => {
                        let Expression::Type(index) = expr else {
                            return self.error("unexpected '::'");
                        };

                        let name = self.parse_name(iter)?;
                        let handle = &self.context.types[index];
                        let Some(func_index) = handle.methods.get(&name) else {
                            return self.error("method not found");
                        };

                        expr = Expression::Func(*func_index);
                        continue;
                    },
                    Some(Token::Braced(tokens)) => {
                        let mut iter = tokens.iter();
                        let mut fields = Vec::new();

                        let Expression::Type(index) = expr else {
                            // could be 'if condition {'
                            let expr = wrap_negate(negate, expr);
                            let expr = self.finalize_expr(parts, expr);
                            return Ok((expr, next));
                        };

                        let TypeData::Struct(_) = self.context.types[index].data else {
                            return self.error("unexpected '{{'");
                        };

                        while iter.len() != 0 {
                            let name = self.parse_name(&mut iter)?;

                            let field_expr = match iter.next().map(tok) {
                                Some(Token::Comma) => {
                                    let text = &self.context.names[name];

                                    match self.find_local(text) {
                                        Some(i) => Expression::Local(i),
                                        _ => return self.error("not a local variable"),
                                    }
                                },
                                Some(Token::Colon) => match self.parse_expr(&mut iter)? {
                                    (field_expr, Some(Token::Comma)) => field_expr,
                                    _other => return self.error("expected ','"),
                                },
                                _ => return self.error("expected ':' or ','"),
                            };

                            fields.push((name, field_expr));
                        }

                        expr = Expression::Struct(index, fields);
                        continue;
                    },
                    Some(Token::Bracketed(tokens)) => {
                        let mut iter = tokens.iter();
                        let subject = Box::new(expr);

                        let (i_expr, None) = self.parse_expr(&mut iter)? else {
                            return self.error("unexpected token");
                        };

                        let index = Box::new(i_expr);
                        expr = Expression::Index(subject, index);
                        continue;
                    },
                    Some(Token::Else) => {
                        let Expression::If(checks, fallback) = &mut expr else {
                            return self.error("unexpected 'else'");
                        };

                        match iter.next().map(tok) {
                            Some(Token::Braced(tokens)) => {
                                let block = self.parse_block(tokens)?;
                                if fallback.replace(block).is_some() {
                                    return self.error("if already has a fallback 'else' block");
                                }
                            },
                            Some(Token::If) => {
                                let (condition, next) = self.parse_expr(iter)?;

                                let Some(Token::Braced(tokens)) = next else {
                                    return self.error("expected '{{' after condition");
                                };

                                let block = self.parse_block(tokens)?;
                                checks.push((condition, block));
                            },
                            _other => return self.error("expected '{{' or 'if'"),
                        }

                        continue;
                    },
                    Some(Token::Dot) => {
                        // todo: float

                        let subject = Box::new(expr);
                        let name = self.parse_name(iter)?;
                        expr = Expression::Field(subject, name);
                        continue;
                    },
                    Some(Token::Equal) => {
                        let (src, next) = self.parse_expr(iter)?;
                        let dst = Some(Box::new(expr));
                        let src = Box::new(src);
                        let expr = Expression::Assignment(dst, src);
                        let expr = wrap_negate(negate, expr);
                        let expr = self.finalize_expr(parts, expr);
                        return Ok((expr, next));
                    },
                    Some(other) => other,
                    None => {
                        let expr = wrap_negate(negate, expr);
                        let expr = self.finalize_expr(parts, expr);
                        return Ok((expr, None));
                    },
                };

                let math_op = match other {
                    Token::Different => MathOp::Different,
                    Token::IsEqual => MathOp::Equal,
                    Token::GreaterEqual => MathOp::GreaterEqual,
                    Token::LessEqual => MathOp::LessEqual,
                    Token::Superior => MathOp::Greater,
                    Token::Inferior => MathOp::Less,
                    Token::Slash => MathOp::Divide,
                    Token::Star => MathOp::Multiply,
                    Token::Minus => MathOp::Subtract,
                    Token::Plus => MathOp::Add,
                    other => {
                        let expr = wrap_negate(negate, expr);
                        let expr = self.finalize_expr(parts, expr);
                        return Ok((expr, Some(other)));
                    },
                };

                let expr = replace(&mut expr, Expression::None);

                let part = ExprPart {
                    left_hand: wrap_negate(negate, expr),
                    math_op,
                };

                parts.push(part);
                break;
            };
        }
    }

    fn parse_expr_base(&mut self, token: &Token, iter: &mut Tokens) -> Res<Expression> {
        match token {
            Token::Alphanumeric(text) => {
                let Ok(maybe_int) = parse_i128(text) else {
                    return self.error("invalid number");
                };

                if let Some(int) = maybe_int {
                    return Ok(Expression::Integer(int));
                }

                if let Some(i) = self.find_local(text) {
                    return Ok(Expression::Local(i));
                }

                let first = self.get_name(text)?;
                match self.resolve(first, iter)? {
                    Some(Item::Function(i)) => Ok(Expression::Func(i)),
                    Some(Item::Const(i)) => Ok(Expression::Const(i)),
                    Some(Item::Type(i)) => Ok(Expression::Type(i)),
                    _other => return self.error("invalid number, item or local"),
                }
            },
            Token::Bracketed(tokens) => {
                let mut items = Vec::new();
                let mut iter = tokens.iter();

                while iter.len() != 0 {
                    let expr = match self.parse_expr(&mut iter)? {
                        (expr, None) => expr,
                        (expr, Some(Token::Comma)) => expr,
                        _other => return self.error("expected comma"),
                    };

                    items.push(expr);
                }

                Ok(Expression::Array(items))
            },
            Token::CodeStr(data) => {
                let mut dyn_parts = Vec::new();

                for in_part in &data.dyn_parts {
                    let Some(local) = self.find_local(in_part.local) else {
                        return self.error("no such local variable");
                    };

                    let out_part = DynPart {
                        prefix: in_part.prefix.clone(),
                        local,
                    };

                    dyn_parts.push(out_part);
                }

                let code_str = CodeStr {
                    dyn_parts,
                    lit_end: data.lit_end.clone(),
                };

                Ok(Expression::ConstStr(code_str))
            },
            Token::Braced(tokens) => {
                let body = self.parse_block(tokens)?;
                Ok(Expression::Block(body))
            },
            Token::Match => {
                let (subject, Some(Token::Braced(arms))) = self.parse_expr(iter)? else {
                    return self.error("expected '{{'");
                };

                let mut iter = arms.iter();
                let arm_vec = self.parse_arms(&mut iter)?;

                let subject = Box::new(subject);
                Ok(Expression::Match(subject, arm_vec))
            },
            Token::Loop => {
                let Some(Token::Braced(body)) = iter.next().map(tok) else {
                    return self.error("expected '{{'");
                };

                Ok(Expression::Loop(self.parse_block(body)?))
            },
            Token::If => {
                let (condition, Some(Token::Braced(body))) = self.parse_expr(iter)? else {
                    return self.error("expected '{{'");
                };

                let block = self.parse_block(body)?;
                let checks = vec![(condition, block)];
                Ok(Expression::If(checks, None))
            },
            _other => return self.error("invalid expression / instruction"),
        }
    }

    fn find_local(&self, text: &str) -> Option<LocalIndex> {
        let locals = self.locals.iter().enumerate().rev();

        for (i, name) in locals {
            if name == text {
                return Some(i);
            }
        }

        None
    }

    fn parse_arms(&mut self, iter: &mut Tokens) -> Res<Vec<MatchArm>> {
        let mut arms = Vec::new();

        while iter.len() != 0 {
            let (list, next) = self.parse_type_spec(iter)?;

            let Some(Token::Then) = next else {
                return self.error("expected '|' or ';'");
            };

            let (expr, Some(Token::Comma)) = self.parse_expr(iter)? else {
                return self.error("expected ','");
            };

            arms.push((list, expr));
        }

        Ok(arms)
    }

    fn finalize_expr(&mut self, mut parts: Vec<ExprPart>, mut last_part: Expression) -> Expression {
        for priority in 0..PRIO_LIMIT {
            if parts.is_empty() {
                break;
            }

            let mut i = 0;

            while i < parts.len() {
                if parts[i].math_op.priority() == priority {
                    let part = parts.remove(i);

                    let right_hand = match i == parts.len() {
                        true => &mut last_part,
                        false => &mut parts[i].left_hand,
                    };

                    let method = part.math_op.method_name(&self.context.names);
                    let left = Box::new(part.left_hand);
                    let right = vec![replace(right_hand, Expression::None)];
                    *right_hand = Expression::Method(left, method, right);
                } else {
                    i += 1;
                }
            }
        }

        assert!(parts.is_empty());
        last_part
    }
}

fn parse_i128(text: &str) -> Result<Option<i128>, ()> {
    let Some(first) = text.chars().next() else {
        return Ok(None);
    };

    if first.is_ascii_digit() {
        let radix = match () {
            () if text.starts_with("0x") => 16,
            () if text.starts_with("0b") => 2,
            () if text.starts_with("0o") => 8,
            _other => 10,
        };

        match i128::from_str_radix(text, radix) {
            Ok(num) => Ok(Some(num)),
            Err(_) => Err(()),
        }
    } else {
        Ok(None)
    }
}

impl MathOp {
    fn method_name(self, names: &Names) -> NameIndex {
        match self {
            Self::Different => names.different,
            Self::Equal => names.equal,
            Self::GreaterEqual => names.greater_equal,
            Self::LessEqual => names.less_equal,
            Self::Greater => names.greater,
            Self::Less => names.less,
            Self::Divide => names.divide,
            Self::Multiply => names.multiply,
            Self::Subtract => names.subtract,
            Self::Add => names.add,
        }
    }

    fn priority(self) -> u8 {
        match self {
            Self::Different => 2,
            Self::Equal => 2,
            Self::GreaterEqual => 2,
            Self::LessEqual => 2,
            Self::Greater => 2,
            Self::Less => 2,
            Self::Divide => 0,
            Self::Multiply => 0,
            Self::Subtract => 1,
            Self::Add => 1,
        }
    }
}
