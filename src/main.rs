use std::fmt;
use std::env;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::{self, Write, BufWriter};
use log::{LevelFilter};
use env_logger;

// --- Big Integer Imports ---
use num_bigint::BigInt;
use num_traits::{Zero, One, Signed, ToPrimitive}; 

// --- Value and AST Definitions ---

#[derive(Debug, Clone, PartialEq)] 
enum Value {
    Integer(BigInt), 
    Float(f64),
    String(String),
    Boolean(bool), 
    Array(Vec<Value>), 
    Void,
}

impl Value {
    fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s), 
            Value::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }), 
            Value::Void => write!(f, "void"),
            Value::Array(v) => {
                write!(f, "[")?;
                for (i, val) in v.iter().enumerate() {
                    match val {
                        Value::String(s) => write!(f, "{}", s)?,
                        _ => write!(f, "{}", val)?,
                    }
                    if i < v.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Num(String),
    Str(String),
    Bool(bool),
    Prefix(char, Box<Expr>),
    Infix(Box<Expr>, char, Box<Expr>),
    Cmp(Box<Expr>, String, Box<Expr>), 
    Logic(Box<Expr>, String, Box<Expr>),
    Array(Vec<Expr>), 
    Slice(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>),
    Call(String, Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(id) => write!(f, "{}", id),
            Expr::Num(s) => write!(f, "{}", s), 
            Expr::Str(s) => write!(f, "\"{}\"", s),
            Expr::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }), 
            Expr::Prefix(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Infix(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Cmp(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs), 
            Expr::Logic(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Array(elements) => {
                write!(f, "[")?;
                for (i, expr) in elements.iter().enumerate() {
                    write!(f, "{}", expr)?;
                    if i < elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::Slice(array, start, end) => {
                write!(f, "{}[", array)?;
                if let Some(s) = start {
                    write!(f, "{}", s)?;
                }
                if start.is_some() || end.is_some() {
                    write!(f, ":")?;
                }
                if let Some(e) = end {
                    write!(f, "{}", e)?;
                }
                write!(f, "]")
            }
            Expr::Call(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Statement {
    Expr(Expr),
    Print(Option<String>, Vec<Expr>),
    Def(String, Vec<String>, Vec<Statement>),
    Return(Option<Expr>),
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
    While(Expr, Vec<Statement>), // ADDED
}

// --- Lexer and Token Definitions ---

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Ident(String),
    Keyword(String),
    Number(String), 
    StringLiteral(String),
    Op(char),
    Cmp(String), 
    Eof,
}

struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let input_chars: Vec<char> = input.chars().collect();
        Lexer { input: input_chars, pos: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.get(self.pos).cloned()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.get(self.pos).cloned();
        if ch.is_some() {
            self.pos += 1;
        }
        ch
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let Some(ch) = self.next_char() else {
            return Token::Eof;
        };
        
        if ch.is_ascii_digit() {
            let mut num = ch.to_string();
            while let Some(next_ch) = self.peek_char() {
                if next_ch.is_ascii_digit() {
                    num.push(self.next_char().unwrap());
                } else {
                    break;
                }
            }
            if self.peek_char() == Some('.') {
                num.push(self.next_char().unwrap());
                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_ascii_digit() {
                        num.push(self.next_char().unwrap());
                    } else {
                        break;
                    }
                }
            }
            Token::Number(num)
        } 
        else if ch == '"' || ch == '\'' {
            let delimiter = ch;
            let mut s = String::new();
            while let Some(next_ch) = self.next_char() {
                if next_ch == delimiter {
                    return Token::StringLiteral(s);
                }
                if next_ch == '\\' {
                    if let Some(escaped_ch) = self.next_char() {
                        match escaped_ch {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            '\'' => s.push('\''),
                            c => s.push(c),
                        }
                    } else {
                        break; 
                    }
                } else {
                    s.push(next_ch);
                }
            }
            Token::StringLiteral(s)
        } 
        else if ch.is_alphabetic() || ch == '_' {
            let mut ident = ch.to_string();
            while let Some(next_ch) = self.peek_char() {
                if next_ch.is_alphanumeric() || next_ch == '_' {
                    ident.push(self.next_char().unwrap());
                } else {
                    break;
                }
            }
            // ADDED "while"
            if ident == "print" || ident == "def" || ident == "fn" || ident == "return" || ident == "if" || ident == "else" || ident == "and" || ident == "or" || ident == "true" || ident == "false" || ident == "while" {
                Token::Keyword(ident)
            } else {
                Token::Ident(ident)
            }
        } 
        else if "+-*/%^".contains(ch) {
            if self.peek_char() == Some('=') {
                self.next_char();
                return Token::Cmp(format!("{}{}", ch, '='));
            }
            Token::Op(ch)
        }
        else if ch == '=' {
            if self.peek_char() == Some('=') {
                self.next_char(); 
                if self.peek_char() == Some('=') {
                    self.next_char();
                    return Token::Cmp("===".to_string());
                }
                return Token::Cmp("==".to_string());
            }
            Token::Op(ch)
        } else if ch == '!' {
            if self.peek_char() == Some('=') {
                self.next_char();
                if self.peek_char() == Some('=') {
                    self.next_char();
                    return Token::Cmp("!==".to_string());
                }
                return Token::Cmp("!=".to_string());
            }
            Token::Op(ch)
        } else if ch == '<' {
            if self.peek_char() == Some('=') {
                self.next_char();
                return Token::Cmp("<=".to_string());
            }
            Token::Cmp("<".to_string())
        } else if ch == '>' {
            if self.peek_char() == Some('=') {
                self.next_char();
                return Token::Cmp(">=".to_string());
            }
            Token::Cmp(">".to_string())
        }
        else {
            Token::Op(ch)
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.peek_char().map_or(false, |c| c.is_whitespace()) {
                self.pos += 1;
                continue;
            }
            if self.peek_char() == Some(';') {
                self.pos += 1; 
                while self.peek_char().map_or(false, |c| c != '\n') {
                    self.pos += 1;
                }
                continue; 
            }
            break;
        }
    }
}

// --- Parser ---

struct Parser {
    lexer: Lexer,
    current: Token,
}

impl Parser {
    fn new(input: &str) -> Parser {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token();
        Parser { lexer, current }
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while self.current != Token::Eof {
            let stmt = match self.current.clone() {
                Token::Keyword(k) if k == "print" => self.parse_print_statement(),
                Token::Keyword(k) if k == "fn" => self.parse_fn_statement(),
                Token::Keyword(k) if k == "return" => self.parse_return_statement(),
                Token::Keyword(k) if k == "if" => self.parse_if_statement(),
                Token::Keyword(k) if k == "while" => self.parse_while_statement(), // ADDED
                Token::Op(op) if op == '=' => {
                    return Err("The assignment operator '=' cannot start a statement.".to_string());
                }
                Token::Keyword(k) if k == "def" => return Err(format!("The 'def' keyword is deprecated.")),
                Token::Keyword(k) if k == "else" => return Err(format!("The 'else' keyword must immediately follow a closing ']' of an 'if' block.")),
                _ => {
                    let expr = self.expr_bp(0)?;
                    Ok(Statement::Expr(expr))
                }
            }?;
            statements.push(stmt);
        }
        Ok(statements)
    }

    fn parse_block_body(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while self.current != Token::Op(']') && self.current != Token::Eof {
            let stmt = match self.current.clone() {
                Token::Keyword(k) if k == "print" => self.parse_print_statement(),
                Token::Keyword(k) if k == "return" => self.parse_return_statement(),
                Token::Keyword(k) if k == "if" => self.parse_if_statement(),
                Token::Keyword(k) if k == "while" => self.parse_while_statement(), // ADDED
                Token::Keyword(k) if k == "def" => return Err(format!("The 'def' keyword is deprecated.")),
                Token::Keyword(k) if k == "else" => return Err(format!("The 'else' keyword must immediately follow a closing ']' of an 'if' block.")),
                Token::Op(op) if op == '=' => {
                    return Err("The assignment operator '=' cannot start a statement.".to_string());
                }
                _ => {
                    let expr = self.expr_bp(0)?;
                    Ok(Statement::Expr(expr))
                }
            }?;
            statements.push(stmt);
        }
        
        if self.current != Token::Op(']') {
            return Err(format!("Unclosed block body. Expected matching ']', found {:?}", self.current));
        }
        self.advance();
        Ok(statements)
    }

    fn parse_while_statement(&mut self) -> Result<Statement, String> {
        self.advance(); // consume 'while'
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'while', found {:?}", self.current));
        }
        self.advance(); // consume '('
        let condition = self.expr_bp(0)?;
        if self.current != Token::Op(')') {
            return Err(format!("Expected ')' after while condition, found {:?}", self.current));
        }
        self.advance(); // consume ')'
        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start while body, found {:?}", self.current));
        }
        self.advance(); // consume '['
        let body = self.parse_block_body()?;
        Ok(Statement::While(condition, body))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        self.advance();
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'if', found {:?}", self.current));
        }
        self.advance();
        let condition = self.expr_bp(0)?;
        if self.current != Token::Op(')') {
            return Err(format!("Expected ')' after if condition, found {:?}", self.current));
        }
        self.advance();
        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start if body, found {:?}", self.current));
        }
        self.advance();
        let if_body_statements = self.parse_block_body()?;
        let mut else_body_statements: Option<Vec<Statement>> = None;
        if let Token::Keyword(k) = self.current.clone() {
            if k == "else" {
                self.advance();
                if self.current != Token::Op('[') {
                    return Err(format!("Expected '[' to start else body, found {:?}", self.current));
                }
                self.advance();
                else_body_statements = Some(self.parse_block_body()?);
            }
        }
        Ok(Statement::If(condition, if_body_statements, else_body_statements))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.advance();
        let has_expr = match self.current.clone() {
            Token::Number(_) | Token::StringLiteral(_) | Token::Op('(') | Token::Op('[') | Token::Ident(_) | Token::Op('+') | Token::Op('-') | Token::Op('!') => true,
            Token::Keyword(k) if k == "true" || k == "false" => true,
            _ => false,
        };
        let return_expr = if has_expr {
            let expr = self.expr_bp(0)?;
            Some(expr)
        } else {
            None
        };
        Ok(Statement::Return(return_expr))
    }

    fn parse_print_statement(&mut self) -> Result<Statement, String> {
        self.advance();
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'print', found {:?}", self.current));
        }
        self.advance();
        let mut format_string: Option<String> = None;
        let mut expressions = Vec::new();
        if let Token::StringLiteral(s) = self.current.clone() {
            format_string = Some(s);
            self.advance();
            while self.current == Token::Op(',') {
                self.advance();
                let expr = self.expr_bp(0)?;
                expressions.push(expr);
            }
        } else if self.current != Token::Op(')') {
            let expr = self.expr_bp(0)?;
            expressions.push(expr);
            if self.current == Token::Op(',') {
                return Err(format!("Simple print allows only one argument."));
            }
        }
        if self.current != Token::Op(')') {
            return Err(format!("Expected closing ')' after print arguments, found {:?}", self.current));
        }
        self.advance();
        Ok(Statement::Print(format_string, expressions))
    }

    fn parse_fn_statement(&mut self) -> Result<Statement, String> {
        self.advance();
        let fn_name = match self.current.clone() {
            Token::Ident(id) => {
                self.advance();
                id
            }
            _ => return Err(format!("Expected function name after 'fn', found {:?}", self.current)),
        };
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after function name"));
        }
        self.advance();
        let mut params = Vec::new();
        while self.current != Token::Op(')') {
            let param_name = match self.current.clone() {
                Token::Ident(id) => {
                    self.advance();
                    params.push(id.clone());
                    id
                }
                Token::Eof => return Err("Unclosed parameter list".to_string()),
                _ => return Err(format!("Expected parameter name, found {:?}", self.current)),
            };
            if self.current == Token::Op(',') {
                self.advance();
            } else if self.current != Token::Op(')') {
                return Err(format!("Expected ',' or ')' after parameter {}", param_name));
            }
        }
        self.advance();
        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start function body"));
        }
        self.advance();
        let body_statements = self.parse_block_body()?;
        Ok(Statement::Def(fn_name, params, body_statements))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        if self.current == Token::Op(')') {
            self.advance();
            return Ok(args);
        }
        loop {
            let arg_expr = self.expr_bp(0)?;
            args.push(arg_expr);
            if self.current == Token::Op(')') {
                self.advance();
                break;
            } else if self.current == Token::Op(',') {
                self.advance();
            } else {
                return Err(format!("Expected ',' or ')' in arguments"));
            }
        }
        Ok(args)
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut lhs = match self.current.clone() {
            Token::Number(num_str) => {
                self.advance();
                Expr::Num(num_str) 
            }
            Token::Ident(id) => {
                self.advance();
                if self.current == Token::Op('(') {
                    self.advance();
                    let args = self.parse_arguments()?;
                    Expr::Call(id, args)
                } else {
                    Expr::Var(id)
                }
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::Str(s)
            }
            Token::Keyword(k) if k == "true" => { self.advance(); Expr::Bool(true) }
            Token::Keyword(k) if k == "false" => { self.advance(); Expr::Bool(false) }
            Token::Op('(') => {
                self.advance();
                let expr = self.expr_bp(0)?;
                if self.current != Token::Op(')') {
                    return Err(format!("Expected ')', found {:?}", self.current));
                }
                self.advance();
                expr
            }
            Token::Op('[') => {
                self.advance();
                let mut elements = Vec::new();
                if self.current == Token::Op(']') {
                    self.advance();
                    return Ok(Expr::Array(elements));
                }
                loop {
                    let expr = self.expr_bp(0)?;
                    elements.push(expr);
                    if self.current == Token::Op(']') {
                        self.advance();
                        break;
                    } else if self.current == Token::Op(',') {
                        self.advance();
                    } else {
                        return Err(format!("Expected ',' or ']' in array"));
                    }
                }
                Expr::Array(elements)
            }
            Token::Op(op) if op == '+' || op == '-' || op == '!' => {
                self.advance();
                let (_, r_bp) = prefix_binding_power(op);
                let rhs = self.expr_bp(r_bp)?;
                Expr::Prefix(op, Box::new(rhs))
            }
            t => return Err(format!("Bad token in prefix: {:?} (Expected expression start or operator)", t)),
        };
        
        loop {
            let op_token = self.current.clone();
            if op_token == Token::Op('[') {
                if 15 < min_bp { break; }
                self.advance();
                let mut start_expr: Option<Expr> = None;
                if self.current != Token::Op(':') && self.current != Token::Op(']') {
                    start_expr = Some(self.expr_bp(0)?);
                }
                if self.current == Token::Op(':') {
                    self.advance();
                    let mut end_expr: Option<Expr> = None;
                    if self.current != Token::Op(']') {
                        end_expr = Some(self.expr_bp(0)?);
                    }
                    if self.current != Token::Op(']') {
                        return Err(format!("Expected ']' after slice"));
                    }
                    self.advance();
                    lhs = Expr::Slice(Box::new(lhs), start_expr.map(Box::new), end_expr.map(Box::new));
                    continue;
                } else if self.current == Token::Op(']') {
                    self.advance();
                    let index_expr = start_expr.ok_or("Missing index")?;
                    lhs = Expr::Slice(Box::new(lhs), Some(Box::new(index_expr)), None);
                    continue;
                } else {
                    return Err(format!("Expected ':' or ']' inside array access"));
                }
            }

            let is_logic_op = match op_token {
                Token::Keyword(ref k) if k == "and" || k == "or" => true,
                _ => false,
            };

            let op_str = if is_logic_op {
                match op_token {
                    Token::Keyword(k) => k,
                    _ => unreachable!(),
                }
            } else {
                match op_token {
                    Token::Op(op) => op.to_string(),
                    Token::Cmp(op) => op,
                    Token::Eof => break,
                    _ => break,
                }
            };

            if op_str.len() == 2 && op_str.ends_with('=') && "+-*/%^".contains(op_str.chars().next().unwrap()) {
                let actual_op = op_str.chars().next().unwrap();
                if 2 < min_bp { break; }
                self.advance();
                let rhs = self.expr_bp(1)?;
                let assign_target = match &lhs {
                    Expr::Var(id) => Expr::Var(id.clone()),
                    Expr::Slice(arr, start, end) => Expr::Slice(arr.clone(), start.clone(), end.clone()),
                    _ => return Err(format!("Invalid compound assignment target")),
                };
                let arithmetic_expr = Expr::Infix(Box::new(assign_target.clone()), actual_op, Box::new(rhs));
                lhs = Expr::Infix(Box::new(assign_target), '=', Box::new(arithmetic_expr));
                continue;
            }

            if let Some((l_bp, r_bp, is_cmp)) = binding_power(op_str.as_str()) {
                if l_bp < min_bp { break; }
                self.advance();
                let rhs = self.expr_bp(r_bp)?;
                lhs = if is_cmp {
                    Expr::Cmp(Box::new(lhs), op_str, Box::new(rhs))
                } else if is_logic_op {
                    Expr::Logic(Box::new(lhs), op_str, Box::new(rhs))
                } else {
                    let single_char_op = op_str.chars().next().unwrap();
                    Expr::Infix(Box::new(lhs), single_char_op, Box::new(rhs))
                };
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}

fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 10),
        '!' => ((), 16),
        _ => ((), 0),
    }
}

fn binding_power(op: &str) -> Option<(u8, u8, bool)> {
    match op {
        "=" => Some((2, 1, false)),
        "or" => Some((3, 4, false)),
        "and" => Some((5, 6, false)),
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "===" | "!==" => Some((7, 8, true)),
        "+" | "-" => Some((9, 10, false)),
        "*" | "/" | "%" => Some((11, 12, false)),
        "^" => Some((13, 14, false)),
        _ => None,
    }
}

// --- Interpreter ---

type Environment = HashMap<String, Value>;
type FuncDefs = HashMap<String, (Vec<String>, Vec<Statement>)>;

enum FunctionControlFlow {
    Continue(Value), 
    Return(Value),   
    Print(String),   
}

fn eval(expr: &Expr, env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    match expr {
        Expr::Num(s) => {
            if s.contains('.') {
                let f = s.parse::<f64>().map_err(|e| format!("Invalid float: {}", e))?;
                Ok(Value::Float(f))
            } else {
                let i = s.parse::<BigInt>().map_err(|e| format!("Invalid integer: {}", e))?;
                Ok(Value::Integer(i))
            }
        },
        Expr::Str(s) => Ok(Value::String(s.clone())),
        Expr::Bool(b) => Ok(Value::Boolean(*b)),
        Expr::Var(id) => env.get(id).cloned().ok_or_else(|| format!("Uninitialized variable: {}", id)),
        Expr::Prefix(op, rhs) => {
            let val = eval(rhs, env, func_defs)?;
            match (*op, val) {
                ('-', Value::Integer(n)) => Ok(Value::Integer(-n)),
                ('+', Value::Integer(n)) => Ok(Value::Integer(n)),
                ('-', Value::Float(n)) => Ok(Value::Float(-n)),
                ('+', Value::Float(n)) => Ok(Value::Float(n)),
                ('!', Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                ('!', v) => Err(format!("'!' only on booleans, got {:?}", v)),
                (_, v) => Err(format!("Unary '{}' only on numbers, got {:?}", op, v)),
            }
        }
        Expr::Array(elements) => {
            let evaluated: Result<Vec<Value>, _> = elements.iter().map(|e| eval(e, env, func_defs)).collect();
            Ok(Value::Array(evaluated?))
        }
        Expr::Slice(array_expr, start_opt, end_opt) => {
            let array_val = eval(array_expr, env, func_defs)?;
            let elements = match array_val {
                Value::Array(v) => v,
                _ => return Err(format!("Not an array: {:?}", array_val)),
            };
            let len = elements.len() as isize;
            let start_index = if let Some(start_expr) = start_opt {
                let idx = match eval(start_expr, env, func_defs)? {
                    Value::Integer(n) => n.to_isize().ok_or("Index too large")?,
                    v => return Err(format!("Index must be Integer, got {:?}", v)),
                };
                let calc = if idx < 0 { len + idx } else { idx };
                calc.max(0).min(len) as usize
            } else if end_opt.is_some() {
                0
            } else {
                return Err("Missing index in R-value".to_string());
            };
            let end_index = if let Some(end_expr) = end_opt {
                let idx = match eval(end_expr, env, func_defs)? {
                    Value::Integer(n) => n.to_isize().ok_or("Index too large")?,
                    v => return Err(format!("Index must be Integer, got {:?}", v)),
                };
                let calc = if idx < 0 { len + idx } else { idx };
                calc.max(0).min(len) as usize
            } else if end_opt.is_some() || (start_opt.is_some() && end_opt.is_some()) {
                len as usize
            } else {
                start_index + 1
            };
            if start_index > end_index || start_index > len as usize {
                return Err(format!("Slice out of bounds"));
            }
            let result = elements[start_index..end_index].to_vec();
            if result.len() == 1 && end_opt.is_none() && start_opt.is_some() {
                Ok(result.into_iter().next().unwrap())
            } else {
                Ok(Value::Array(result))
            }
        }
        Expr::Infix(lhs, op, rhs) if *op == '=' => {
            let val = eval(rhs, env, func_defs)?;
            match &**lhs {
                Expr::Var(id) => {
                    env.insert(id.clone(), val.clone());
                    Ok(val)
                }
                Expr::Slice(array_expr, start_opt, end_opt) => {
                    if end_opt.is_some() {
                        return Err("Slice assignment not supported".to_string());
                    }
                    let index_expr = start_opt.as_ref().ok_or("Missing index")?;
                    let index = match eval(index_expr, env, func_defs)? {
                        Value::Integer(n) => n.to_isize().ok_or("Index too large")?,
                        v => return Err(format!("Index must be Integer, got {:?}", v)),
                    };
                    let array_var_name = match &**array_expr {
                        Expr::Var(id) => id,
                        _ => return Err("Array must be simple variable".to_string()),
                    };
                    let array_val_ref = env.get_mut(array_var_name).ok_or_else(|| format!("Uninitialized array"))?;
                    let elements = match array_val_ref {
                        Value::Array(v) => v,
                        _ => return Err("Not an array".to_string()),
                    };
                    let len = elements.len() as isize;
                    let actual_index = if index < 0 { len + index } else { index };
                    if actual_index < 0 || actual_index as usize >= elements.len() {
                        return Err(format!("Index out of bounds"));
                    }
                    elements[actual_index as usize] = val.clone();
                    Ok(val)
                }
                _ => Err("Invalid assignment target".to_string()),
            }
        }
        Expr::Infix(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;
            match (left_val, right_val) {
                (Value::Integer(l), Value::Integer(r)) => {
                    return match op {
                        '+' => Ok(Value::Integer(l + r)),
                        '-' => Ok(Value::Integer(l - r)),
                        '*' => Ok(Value::Integer(l * r)),
                        '%' => if r.is_zero() { Err("Modulo by zero".to_string()) } else { Ok(Value::Integer(l % r)) },
                        '/' => if r.is_zero() { Err("Division by zero".to_string()) } else { Ok(Value::Integer(l / r)) },
                        '^' => {
                            if r.is_positive() && r <= BigInt::from(u32::MAX) {
                                let exp: u32 = r.to_u32().ok_or("Exponent too large")?;
                                Ok(Value::Integer(l.pow(exp)))
                            } else if r.is_zero() {
                                Ok(Value::Integer(BigInt::one()))
                            } else {
                                Err("Exponent must be non-negative".to_string())
                            }
                        }
                        _ => Err(format!("Unknown op {}", op)),
                    };
                }
                (Value::String(mut l), Value::String(r)) if *op == '+' => { l.push_str(&r); Ok(Value::String(l)) }
                (Value::Array(mut l), Value::Array(r)) if *op == '+' => { l.extend(r); Ok(Value::Array(l)) }
                (l, r) if l.is_number() && r.is_number() => {
                    let l_f = match l { Value::Float(f) => f, Value::Integer(i) => i.to_f64().ok_or("Too large")?, _ => unreachable!() };
                    let r_f = match r { Value::Float(f) => f, Value::Integer(i) => i.to_f64().ok_or("Too large")?, _ => unreachable!() };
                    let res = match op {
                        '+' => l_f + r_f,
                        '-' => l_f - r_f,
                        '*' => l_f * r_f,
                        '%' => if r_f.abs() < f64::EPSILON { return Err("Modulo by zero".to_string()); } else { l_f % r_f },
                        '/' => if r_f.abs() < f64::EPSILON { return Err("Division by zero".to_string()); } else { l_f / r_f },
                        '^' => l_f.powf(r_f),
                        _ => return Err(format!("Unknown op {}", op)),
                    };
                    Ok(Value::Float(res))
                }
                (l, r) => Err(format!("Incompatible types for '{}': {:?} and {:?}", op, l, r)),
            }
        }
        Expr::Cmp(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;
            let result = match op.as_str() {
                "===" => left_val == right_val,
                "!==" => left_val != right_val,
                "==" | "!=" => {
                    let eq = match (&left_val, &right_val) {
                        (l, r) if l == r => true,
                        (Value::Integer(l), Value::Float(r)) => l.to_f64().map_or(false, |lf| lf == *r),
                        (Value::Float(l), Value::Integer(r)) => r.to_f64().map_or(false, |rf| *l == rf),
                        _ => false,
                    };
                    if op == "==" { eq } else { !eq }
                }
                "<" | ">" | "<=" | ">=" => {
                    match (&left_val, &right_val) {
                        (Value::Integer(l), Value::Integer(r)) => match op.as_str() {
                            "<" => l < r, ">" => l > r, "<=" => l <= r, ">=" => l >= r, _ => unreachable!(),
                        },
                        (Value::Float(l), Value::Float(r)) => match op.as_str() {
                            "<" => l < r, ">" => l > r, "<=" => l <= r, ">=" => l >= r, _ => unreachable!(),
                        },
                        (Value::String(l), Value::String(r)) => match op.as_str() {
                            "<" => l < r, ">" => l > r, "<=" => l <= r, ">=" => l >= r, _ => unreachable!(),
                        },
                        (l, r) => return Err(format!("Cannot compare {:?} and {:?}", l, r)),
                    }
                }
                _ => return Err(format!("Unknown cmp {}", op)),
            };
            Ok(Value::Boolean(result))
        }
        Expr::Logic(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let short_circuit = match (op.as_str(), &left_val) {
                ("and", Value::Boolean(false)) => Some(Value::Boolean(false)),
                ("or", Value::Boolean(true)) => Some(Value::Boolean(true)),
                _ => None,
            };
            if let Some(v) = short_circuit { return Ok(v); }
            let right_val = eval(rhs, env, func_defs)?;
            match (op.as_str(), left_val, right_val) {
                ("and", Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l && r)),
                ("or", Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(l || r)),
                (op_str, l, r) => Err(format!("'{}' only on booleans: {:?} {:?}", op_str, l, r)),
            }
        }
        Expr::Call(name, args) => execute_function(name, args, env, func_defs),
    }
}

type NativeFunction = fn(&str, &mut Environment, &FuncDefs, Vec<Value>) -> Result<Value, String>;

fn get_native_function(name: &str) -> Option<NativeFunction> {
    match name {
        "length" => Some(native_length),
        _ => None,
    }
}

fn native_length(_: &str, _: &mut Environment, _: &FuncDefs, mut args: Vec<Value>) -> Result<Value, String> {
    if args.len() != 1 { return Err("length expects 1 arg".to_string()); }
    match args.remove(0) {
        Value::Array(a) => Ok(Value::Integer(BigInt::from(a.len()))),
        v => Err(format!("length on non-array: {:?}", v)),
    }
}

fn execute_function(fn_name: &str, arg_exprs: &[Expr], caller_env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    let evaluated_args: Vec<Value> = arg_exprs.iter().map(|e| eval(e, caller_env, func_defs)).collect::<Result<_, _>>()?;
    if let Some(native) = get_native_function(fn_name) {
        native(fn_name, caller_env, func_defs, evaluated_args)
    } else if let Some((params, body)) = func_defs.get(fn_name) {
        if params.len() != evaluated_args.len() {
            return Err(format!("{} expects {} args, got {}", fn_name, params.len(), evaluated_args.len()));
        }
        let mut local_env = Environment::new();
        for (p, a) in params.iter().zip(evaluated_args) {
            local_env.insert(p.clone(), a);
        }
        let mut last = Value::Void;
        for (i, stmt) in body.iter().enumerate() {
            match run_statement_in_function(stmt, &mut local_env, func_defs) {
                Ok(flow) => match flow {
                    FunctionControlFlow::Return(v) => return Ok(v),
                    FunctionControlFlow::Continue(v) => last = v,
                    FunctionControlFlow::Print(s) => {
                        writeln!(io::stdout(), "{}", s).map_err(|e| e.to_string())?;
                        io::stdout().flush().ok();
                        let mut log = OpenOptions::new().create(true).append(true).open("runlog").map_err(|e| e.to_string())?;
                        writeln!(log, "Block Output (Stmt {}): {}", i + 1, s).map_err(|e| e.to_string())?;
                    }
                },
                Err(e) => return Err(format!("Error in {} (stmt {}): {}", fn_name, i + 1, e)),
            }
        }
        Ok(last)
    } else {
        Err(format!("Undefined function {}", fn_name))
    }
}

fn run_statement_in_function(stmt: &Statement, env: &mut Environment, func_defs: &FuncDefs) -> Result<FunctionControlFlow, String> {
    match stmt {
        Statement::Expr(e) => Ok(FunctionControlFlow::Continue(eval(e, env, func_defs)?)),
        Statement::Print(fmt, exprs) => {
            let vals: Vec<Value> = exprs.iter().map(|e| eval(e, env, func_defs)).collect::<Result<_, _>>()?;
            let output = if let Some(s) = fmt {
                let mut out = s.clone();
                let mut pos = 0;
                for v in vals {
                    let vs = match &v {
                        Value::String(t) => t.clone(),
                        Value::Boolean(b) => if *b { "true" } else { "false" }.to_string(),
                        Value::Array(a) => format!("{}", Value::Array(a.clone())),
                        _ => format!("{}", v),
                    };
                    if let Some(i) = out[pos..].find("{}") {
                        let start = pos + i;
                        out.replace_range(start..start + 2, &vs);
                        pos = start + vs.len();
                    } else { return Err("Too many args".to_string()); }
                }
                out
            } else {
                if vals.len() != 1 { return Err("Simple print needs 1 arg".to_string()); }
                match &vals[0] {
                    Value::Boolean(b) => if *b { "true" } else { "false" }.to_string(),
                    Value::String(s) => s.clone(),
                    v => format!("{}", v),
                }
            };
            Ok(FunctionControlFlow::Print(output))
        }
        Statement::If(cond, ifb, elseb) => {
            let c = eval(cond, env, func_defs)?;
            let do_if = match c { Value::Boolean(b) => b, _ => return Err("if condition not bool".to_string()) };
            let body = if do_if { ifb } else if let Some(e) = elseb { e } else { return Ok(FunctionControlFlow::Continue(Value::Void)); };
            let mut last = Value::Void;
            for s in body {
                match run_statement_in_function(s, env, func_defs)? {
                    FunctionControlFlow::Return(v) => return Ok(FunctionControlFlow::Return(v)),
                    FunctionControlFlow::Continue(v) => last = v,
                    FunctionControlFlow::Print(o) => {
                        writeln!(io::stdout(), "{}", o).map_err(|e| e.to_string())?;
                        let mut log = OpenOptions::new().create(true).append(true).open("runlog").map_err(|e| e.to_string())?;
                        writeln!(log, "Block Output: {}", o).map_err(|e| e.to_string())?;
                    }
                }
            }
            Ok(FunctionControlFlow::Continue(last))
        }
        Statement::While(cond, body) => {
            let mut last = Value::Void;
            loop {
                let c = eval(cond, env, func_defs)?;
                let go = match c { Value::Boolean(b) => b, _ => return Err("while condition not bool".to_string()) };
                if !go { break; }
                for s in body {
                    match run_statement_in_function(s, env, func_defs)? {
                        FunctionControlFlow::Return(v) => return Ok(FunctionControlFlow::Return(v)),
                        FunctionControlFlow::Continue(v) => last = v,
                        FunctionControlFlow::Print(o) => {
                            writeln!(io::stdout(), "{}", o).map_err(|e| e.to_string())?;
                            let mut log = OpenOptions::new().create(true).append(true).open("runlog").map_err(|e| e.to_string())?;
                            writeln!(log, "Loop Output: {}", o).map_err(|e| e.to_string())?;
                        }
                    }
                }
            }
            Ok(FunctionControlFlow::Continue(last))
        }
        Statement::Def(..) => Err("fn def not allowed inside function".to_string()),
        Statement::Return(e) => {
            let v = e.as_ref().map(|ex| eval(ex, env, func_defs)).transpose()?.unwrap_or(Value::Void);
            Ok(FunctionControlFlow::Return(v))
        }
    }
}

fn run_statement(stmt: &Statement, env: &mut Environment, func_defs: &mut FuncDefs) -> Result<String, String> {
    match stmt {
        Statement::Expr(e) => Ok(format!("{}", eval(e, env, func_defs)?)),
        Statement::Print(fmt, exprs) => {
            let vals: Vec<Value> = exprs.iter().map(|e| eval(e, env, func_defs)).collect::<Result<_, _>>()?;
            let output = if let Some(s) = fmt {
                let mut out = s.clone();
                let mut pos = 0;
                for v in vals {
                    let vs = match &v {
                        Value::String(t) => t.clone(),
                        Value::Boolean(b) => if *b { "true" } else { "false" }.to_string(),
                        Value::Array(a) => format!("{}", Value::Array(a.clone())),
                        _ => format!("{}", v),
                    };
                    if let Some(i) = out[pos..].find("{}") {
                        let start = pos + i;
                        out.replace_range(start..start + 2, &vs);
                        pos = start + vs.len();
                    }
                }
                out
            } else {
                match &vals[0] {
                    Value::Boolean(b) => if *b { "true" } else { "false" }.to_string(),
                    Value::String(s) => s.clone(),
                    v => format!("{}", v),
                }
            };
            writeln!(io::stdout(), "{}", output).map_err(|e| e.to_string())?;
            let mut log = OpenOptions::new().create(true).append(true).open("runlog").map_err(|e| e.to_string())?;
            writeln!(log, "Output: {}", output).map_err(|e| e.to_string())?;
            Ok(output)
        }
        Statement::Def(name, params, body) => {
            func_defs.insert(name.clone(), (params.clone(), body.clone()));
            Ok(String::new())
        }
        Statement::If(cond, ifb, elseb) => {
            let c = eval(cond, env, func_defs)?;
            let do_if = match c { Value::Boolean(b) => b, _ => return Err("if condition not bool".to_string()) };
            let body = if do_if { ifb } else if let Some(e) = elseb { e } else { return Ok(String::new()); };
            for s in body { let _ = run_statement(s, env, func_defs)?; }
            Ok(String::new())
        }
        Statement::While(cond, body) => {
            loop {
                let c = eval(cond, env, func_defs)?;
                let go = match c { Value::Boolean(b) => b, _ => return Err("while condition not bool".to_string()) };
                if !go { break; }
                for s in body { let _ = run_statement(s, env, func_defs)?; }
            }
            Ok(String::new())
        }
        Statement::Return(_) => Ok(String::new()),
    }
}

fn main() {
    let debug_file = OpenOptions::new().create(true).append(true).open("runlog").expect("open runlog");
    let debug_writer = BufWriter::new(debug_file);
    env_logger::Builder::new()
        .filter_level(LevelFilter::Debug)
        .target(env_logger::Target::Pipe(Box::new(debug_writer)))
        .init();

    let args: Vec<String> = env::args().collect();
    let mut filename = "direct_string_input";
    let file_content: String;

    if args.len() < 2 {
        eprintln!("Usage: {} <filename> or {} --code \"code\"", args[0], args[0]);
        return;
    }

    if args[1] == "--code" || args[1] == "-c" {
        if args.len() < 3 { eprintln!("--code needs string"); return; }
        file_content = args[2].clone();
    } else {
        filename = &args[1];
        file_content = fs::read_to_string(filename).expect("read file");
    }

    let mut parser = Parser::new(&file_content);
    let mut env = HashMap::new();
    let mut func_defs = HashMap::new();

    let mut log_file = OpenOptions::new().create(true).append(true).open("runlog").expect("open runlog");
    writeln!(log_file, "--- Starting script from {} ---", filename).unwrap();

    match parser.parse() {
        Ok(stmts) => {
            for (i, stmt) in stmts.into_iter().enumerate() {
                writeln!(log_file, "\nExecuting Statement {}", i + 1).unwrap();
                match run_statement(&stmt, &mut env, &mut func_defs) {
                    Ok(out) => if !out.is_empty() {
                        writeln!(log_file, "Result: {}", out).unwrap();
                    },
                    Err(e) => {
                        eprintln!("Runtime Error (Stmt {}): {}", i + 1, e);
                        writeln!(log_file, "Error (Stmt {}): {}", i + 1, e).unwrap();
                        break;
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Parsing Error: {}", e);
            writeln!(log_file, "Parsing Error: {}", e).unwrap();
        }
    }
}