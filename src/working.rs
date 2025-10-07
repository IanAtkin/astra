use std::fmt;
use std::env;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::{self, Write, BufWriter};
use log::{debug, LevelFilter};
use env_logger;

// --- Value and AST Definitions ---

#[derive(Debug, Clone, PartialEq)] 
enum Value {
    Integer(i64), 
    Float(f64),
    String(String),
    Boolean(bool), 
    Void,
}

impl Value {
    // Helper to check if a value is numeric (Integer or Float)
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
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Num(f64),
    Str(String),
    Prefix(char, Box<Expr>),
    Infix(Box<Expr>, char, Box<Expr>),
    Cmp(Box<Expr>, String, Box<Expr>), 
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
enum Statement {
    Expr(Expr),
    Print(Option<String>, Vec<Expr>),
    Def(String, Vec<String>, String),
    Return(Option<Expr>),
    // UPDATED: If statement with condition, raw IF body, and optional raw ELSE body
    If(Expr, String, Option<String>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(id) => write!(f, "{}", id),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Str(s) => write!(f, "\"{}\"", s),
            Expr::Prefix(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Infix(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Cmp(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs), 
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
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
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
                // Handle escape sequences
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
            // UPDATED: Added "else" as a keyword
            if ident == "print" || ident == "def" || ident == "fn" || ident == "return" || ident == "if" || ident == "else" {
                Token::Keyword(ident)
            } else {
                Token::Ident(ident)
            }
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
        debug!("Advanced to token {:?}", self.current);
    }

    fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while self.current != Token::Eof {
            debug!("Parsing statement, current token: {:?}", self.current);
            let stmt = match self.current.clone() {
                Token::Keyword(k) if k == "print" => self.parse_print_statement(),
                Token::Keyword(k) if k == "fn" => self.parse_fn_statement(),
                Token::Keyword(k) if k == "return" => self.parse_return_statement(),
                Token::Keyword(k) if k == "if" => self.parse_if_statement(),
                Token::Keyword(k) if k == "def" => return Err(format!("The 'def' keyword is deprecated. Please use 'fn' for function definitions (e.g., fn name(...) [...])")),
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

    // NEW: Helper function to parse the raw body string inside a [...] block,
    // handling nested blocks and updating the lexer's position.
    fn parse_block_body(&mut self) -> Result<String, String> {
        // Assumes current token is '[' and needs to be consumed.
        let start_pos_in_input = self.lexer.pos - 1; // Position of '['
        self.advance(); // consume '['

        let input = &self.lexer.input;
        let mut end_pos_in_input = self.lexer.pos;
        let mut bracket_count = 1;
        
        // Search for matching ']' while tracking nested brackets
        while end_pos_in_input < input.len() {
            match input[end_pos_in_input] {
                '[' => bracket_count += 1,
                ']' => {
                    bracket_count -= 1;
                    if bracket_count == 0 {
                        break;
                    }
                }
                _ => {}
            }
            end_pos_in_input += 1;
        }
        
        if bracket_count != 0 || end_pos_in_input == input.len() {
            return Err("Unclosed block body. Expected matching ']'".to_string());
        }

        // Capture the raw body including the enclosing brackets
        let raw_body: String = input[start_pos_in_input..=end_pos_in_input].iter().collect();
        
        // Update lexer position past the closing ']'
        self.lexer.pos = end_pos_in_input;
        self.advance(); // Now pointing at the closing ']'
        if self.current != Token::Op(']') {
             return Err(format!("Expected closing ']' after block body scan, found {:?}", self.current));
        }
        self.advance(); // consume ']'

        Ok(raw_body)
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        debug!("Parsing if statement");
        self.advance(); // consume 'if'

        // 1. Parse condition (if (condition))
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'if', found {:?}", self.current));
        }
        self.advance(); // consume '('

        let condition = self.expr_bp(0)?;

        if self.current != Token::Op(')') {
            return Err(format!("Expected ')' after if condition, found {:?}", self.current));
        }
        self.advance(); // consume ')'

        // 2. Parse IF body ([...])
        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start if body, found {:?}", self.current));
        }
        
        let if_body = self.parse_block_body()?;

        // 3. Check for optional 'else' block
        let mut else_body: Option<String> = None;

        if let Token::Keyword(k) = self.current.clone() {
            if k == "else" {
                debug!("Found 'else' keyword");
                self.advance(); // consume 'else'
                
                if self.current != Token::Op('[') {
                    return Err(format!("Expected '[' to start else body, found {:?}", self.current));
                }
                
                else_body = Some(self.parse_block_body()?);
            }
        }
        
        debug!("Parsed if statement with condition {:?}, if body {}, and else body {:?}", condition, if_body, else_body);
        Ok(Statement::If(condition, if_body, else_body))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        debug!("Parsing return statement");
        self.advance(); // consume 'return' keyword

        let has_expr = match self.current {
            Token::Number(_) | Token::StringLiteral(_) | Token::Op('(') | Token::Ident(_) | Token::Op('+') | Token::Op('-') => true,
            _ => false,
        };

        let return_expr = if has_expr {
            let expr = self.expr_bp(0)?;
            Some(expr)
        } else {
            None
        };

        debug!("Parsed return statement: Return({:?})", return_expr);
        Ok(Statement::Return(return_expr))
    }

    fn parse_print_statement(&mut self) -> Result<Statement, String> {
        debug!("Parsing print statement");
        self.advance(); // Consume 'print'
        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'print', found {:?}", self.current));
        }
        self.advance(); // Consume '('

        let mut format_string: Option<String> = None;
        let mut expressions = Vec::new();

        if let Token::StringLiteral(s) = self.current.clone() {
            format_string = Some(s);
            self.advance();

            while self.current == Token::Op(',') {
                self.advance();
                debug!("Parsing print argument (formatted), current token: {:?}", self.current);
                let expr = self.expr_bp(0)?;
                expressions.push(expr);
            }

        } else if self.current != Token::Op(')') {
            debug!("Parsing print argument (simple), current token: {:?}", self.current);
            let expr = self.expr_bp(0)?;
            expressions.push(expr);

            if self.current == Token::Op(',') {
                return Err(format!("When using 'print(expr)' format (without a format string), only a single expression is allowed. Found ',' after argument: {:?}", expressions[0]));
            }
        }
        
        if self.current != Token::Op(')') {
            return Err(format!("Expected closing ')' after print arguments, found {:?}", self.current));
        }
        self.advance(); // Consume ')'
        debug!("Parsed print statement: Print({:?}, {:?})", format_string, expressions);
        Ok(Statement::Print(format_string, expressions))
    }

    fn parse_fn_statement(&mut self) -> Result<Statement, String> {
        debug!("Parsing fn statement");
        self.advance();
        let fn_name = match self.current.clone() {
            Token::Ident(id) => {
                self.advance();
                id
            }
            _ => return Err(format!("Expected function name (identifier) after 'fn', found {:?}", self.current)),
        };
        if self.current != Token::Op('(') {
            return Err(format!(
                "Expected '(' to start parameter list in function definition, found {:?}. Syntax must be: fn {}() [...]", 
                self.current, fn_name
            ));
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
                Token::Eof => return Err("Unclosed parameter list in function definition. Expected ')'".to_string()),
                _ => return Err(format!("Expected parameter name or ')' in function definition, found {:?}", self.current)),
            };
            if self.current == Token::Op(',') {
                self.advance();
            } else if self.current != Token::Op(')') {
                return Err(format!("Expected ',' or ')' after parameter {}, found {:?}", param_name, self.current));
            }
        }
        self.advance();
        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start function body (e.g., fn {}() [body]), found {:?}", fn_name, self.current));
        }
        // Use the new block parsing helper
        let raw_body = self.parse_block_body()?;
        
        debug!("Parsed fn {}({:?}) [{}]", fn_name, params, raw_body);
        Ok(Statement::Def(fn_name, params, raw_body))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        if self.current == Token::Op(')') {
            self.advance();
            return Ok(args);
        }
        loop {
            debug!("Parsing argument, current token: {:?}", self.current);
            let arg_expr = self.expr_bp(0)?;
            args.push(arg_expr);
            if self.current == Token::Op(')') {
                self.advance();
                break;
            } else if self.current == Token::Op(',') {
                self.advance();
            } else {
                return Err(format!("Expected ',' or ')' in function call arguments, found {:?}", self.current));
            }
        }
        Ok(args)
    }

    fn expr_bp(&mut self, min_bp: u8) -> Result<Expr, String> {
        debug!("Parsing expression with min_bp {}, current token: {:?}", min_bp, self.current);
        let mut lhs = match self.current.clone() {
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
            Token::Number(num_str) => {
                self.advance();
                let f = num_str.parse::<f64>().map_err(|e| format!("Invalid number: {}", e))?;
                Expr::Num(f) 
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::Str(s)
            }
            Token::Op('(') => {
                self.advance();
                let expr = self.expr_bp(0)?;
                if self.current != Token::Op(')') {
                    return Err(format!("Expected ')', found {:?}", self.current));
                }
                self.advance();
                expr
            }
            Token::Op(op) if op == '+' || op == '-' => {
                self.advance();
                let (_, r_bp) = prefix_binding_power(op);
                let rhs = self.expr_bp(r_bp)?;
                Expr::Prefix(op, Box::new(rhs))
            }
            t => return Err(format!("Bad token in prefix: {:?} (Expected expression start or operator)", t)),
        };
        loop {
            let op_token = self.current.clone();
            let op = match op_token {
                Token::Op(op) => op.to_string(),
                Token::Cmp(op) => op,
                Token::Eof => break,
                _ => break,
            };

            if let Some((l_bp, r_bp, is_cmp)) = binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.advance();
                debug!("Parsing infix/cmp op {}, right expr with bp {}", op, r_bp);
                let rhs = self.expr_bp(r_bp)?;
                
                lhs = if is_cmp {
                    Expr::Cmp(Box::new(lhs), op, Box::new(rhs))
                } else {
                    // This is safe because non-comparison operators are guaranteed to be single chars
                    Expr::Infix(Box::new(lhs), op.chars().next().unwrap(), Box::new(rhs))
                };
                continue;
            }
            break;
        }
        debug!("Parsed expression: {:?}", lhs);
        Ok(lhs)
    }
}

fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 10),
        _ => ((), 0),
    }
}

// Combined binding power function to handle Infix (char) and Cmp (String) operators
fn binding_power(op: &str) -> Option<(u8, u8, bool)> { // (l_bp, r_bp, is_comparison)
    match op {
        "=" => Some((2, 1, false)), // Assignment
        // All comparison operators share the same precedence 
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "===" | "!==" => Some((3, 4, true)), 
        "+" | "-" => Some((5, 6, false)), // Addition/Subtraction
        "*" | "/" => Some((7, 8, false)), // Multiplication/Division
        _ => None,
    }
}

// --- Interpreter ---

type Environment = HashMap<String, Value>;
type FuncDefs = HashMap<String, (Vec<String>, String)>;

enum FunctionControlFlow {
    Continue(Value), 
    Return(Value),   
    Print(String),   
}

/// Executes a raw block body string by parsing it and running the statements.
fn execute_block_body(raw_body: &str, env: &mut Environment, func_defs: &FuncDefs, is_in_function: bool) -> Result<Option<Value>, String> {
    let sanitized_body = raw_body
        .replace('\u{a0}', " ")
        .replace('\u{200b}', "")
        .trim_matches(|c: char| c == '[' || c == ']' || c.is_whitespace())
        .to_string();

    if sanitized_body.is_empty() {
        return Ok(None);
    }

    let mut body_parser = Parser::new(&sanitized_body);
    let statements = body_parser.parse()?;
    
    let mut last_value = Value::Void;

    for (i, stmt) in statements.into_iter().enumerate() {
        if is_in_function {
            match run_statement_in_function(&stmt, env, func_defs) {
                Ok(flow) => {
                    match flow {
                        FunctionControlFlow::Return(val) => {
                            debug!("Explicit return triggered from block with value: {:?}", val);
                            // If we hit an explicit return signal, we return the value directly,
                            // signaling to the caller function/block to stop execution immediately.
                            return Ok(Some(val));
                        }
                        FunctionControlFlow::Continue(val) => {
                            last_value = val;
                        }
                        FunctionControlFlow::Print(output) => {
                            // Handle print statement output (write to stdout and log)
                            writeln!(io::stdout(), "{}", output).map_err(|e| format!("Failed to write to stdout: {}", e))?;
                            io::stdout().flush().map_err(|e| format!("Failed to flush stdout: {}", e))?;
                            let mut log_file = OpenOptions::new()
                                .create(true)
                                .append(true)
                                .open("runlog")
                                .map_err(|e| format!("Failed to open runlog: {}", e))?;
                            writeln!(log_file, "Block Output (Stmt {}): {}", i + 1, output)
                                .map_err(|e| format!("Failed to write to runlog: {}", e))?;
                            log_file.flush().map_err(|e| format!("Failed to flush runlog: {}", e))?;
                        }
                    }
                }
                Err(e) => {
                    return Err(format!("Block Execution Error (Stmt {}): {}", i + 1, e));
                }
            }
        } else { // Top-level execution (no return flow needed)
             match run_statement(&stmt, env, &mut func_defs.clone()) {
                Ok(_) => continue,
                Err(e) => return Err(e),
            }
        }
    }
    
    if is_in_function {
        // If execution finishes without hitting an explicit return, return the last statement's value
        Ok(Some(last_value))
    } else {
        Ok(None)
    }
}


fn eval(expr: &Expr, env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    debug!("Evaluating expr: {:?}", expr);
    match expr {
        Expr::Num(n) => {
            // If the float has no fractional part, treat it as an Integer
            if n.fract() == 0.0 {
                // Safely convert to i64
                Ok(Value::Integer(*n as i64))
            } else {
                Ok(Value::Float(*n))
            }
        },
        Expr::Str(s) => Ok(Value::String(s.clone())),
        Expr::Var(id) => env
            .get(id)
            .cloned()
            .ok_or_else(|| format!("Cannot evaluate uninitialized variable: {}", id)),
        Expr::Call(name, args) => execute_function(name, args, env, func_defs),
        
        // Assignment (=)
        Expr::Infix(lhs, op, rhs) if *op == '=' => {
            let var_name = match &**lhs {
                Expr::Var(id) => id,
                _ => return Err("Assignment target must be a variable".to_string()),
            };
            let val = eval(rhs, env, func_defs)?;
            env.insert(var_name.clone(), val.clone());
            Ok(val)
        }
        
        // Unary (+/-)
        Expr::Prefix(op, rhs) => {
            let val = eval(rhs, env, func_defs)?;
            match op {
                '+' => Ok(val),
                '-' => match val {
                    Value::Integer(n) => Ok(Value::Integer(-n)),
                    Value::Float(n) => Ok(Value::Float(-n)),
                    _ => Err(format!("Unary minus only works on numbers, found {:?}", val)),
                },
                _ => Err(format!("Unknown prefix operator: {}", op)),
            }
        }
        
        // Arithmetic (+, -, *, /)
        Expr::Infix(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;
            
            if !left_val.is_number() || !right_val.is_number() {
                // String concatenation
                if *op == '+' {
                    match (left_val, right_val) {
                        (Value::String(mut l), Value::String(r)) => {
                            l.push_str(&r);
                            return Ok(Value::String(l));
                        }
                        (l, r) => return Err(format!("Incompatible types for operator '{}': {:?} and {:?}", op, l, r)),
                    }
                } else {
                    return Err(format!("Incompatible types for operator '{}': {:?} and {:?}", op, left_val, right_val));
                }
            }
            
            let (l_f, r_f) = match (left_val, right_val) {
                (Value::Integer(l), Value::Integer(r)) => {
                    // Integer-only arithmetic. Division is the only one that promotes to float.
                    return match op {
                        '+' => Ok(Value::Integer(l + r)),
                        '-' => Ok(Value::Integer(l - r)),
                        '*' => Ok(Value::Integer(l * r)),
                        '/' => {
                            if r == 0 {
                                Err("Division by zero".to_string())
                            } else {
                                // Division promotes to float
                                Ok(Value::Float(l as f64 / r as f64))
                            }
                        }
                        _ => Err(format!("Unknown numeric infix operator: {}", op)),
                    };
                }
                // Mixed or Float arithmetic, both are promoted to float
                (Value::Integer(l), Value::Float(r)) => (l as f64, r),
                (Value::Float(l), Value::Integer(r)) => (l, r as f64),
                (Value::Float(l), Value::Float(r)) => (l, r),
                _ => unreachable!(), // Handled by is_number() check above
            };

            // Floating point arithmetic
            let result_f = match op {
                '+' => Ok(l_f + r_f),
                '-' => Ok(l_f - r_f),
                '*' => Ok(l_f * r_f),
                '/' => {
                    if r_f.abs() < f64::EPSILON {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(l_f / r_f)
                    }
                }
                _ => Err(format!("Unknown numeric infix operator: {}", op)),
            }?;
            
            Ok(Value::Float(result_f))
        }

        // Comparison (==, !=, <, >, <=, >=, ===, !==)
        Expr::Cmp(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;
            
            let result = match op.as_str() {
                // STRICT Equality/Inequality (type must match exactly)
                "===" => left_val == right_val,
                "!==" => left_val != right_val,
                
                // NON-STRICT Equality/Inequality 
                "==" | "!=" => {
                    let strict_match = left_val == right_val;
                    if strict_match {
                        if op.as_str() == "==" { true } else { false }
                    } else {
                        // Check for non-strict equality across Integer/Float types
                        let val_match = match (&left_val, &right_val) {
                            (Value::Integer(l), Value::Float(r)) | (Value::Float(r), Value::Integer(l)) => {
                                (*l as f64) == *r
                            },
                            _ => false, 
                        };
                        
                        if op.as_str() == "==" { val_match } else { !val_match }
                    }
                },
                
                // Ordering Comparisons: require same type for ordering
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
                        (l, r) => return Err(format!(
                            "Incompatible types for ordering operator '{}': {:?} and {:?}", op, l, r
                        )),
                    }
                },
                _ => return Err(format!("Unknown comparison operator: {}", op)),
            };
            
            Ok(Value::Boolean(result))
        }
    }
}

fn execute_function(fn_name: &str, arg_exprs: &[Expr], caller_env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    debug!("Executing function '{}', args: {:?}", fn_name, arg_exprs);
    let (params, raw_body) = func_defs.get(fn_name)
        .ok_or_else(|| format!("Function '{}' is not defined", fn_name))?;
    if params.len() != arg_exprs.len() {
        return Err(format!(
            "Function '{}' expects {} arguments, but received {}",
            fn_name, params.len(), arg_exprs.len()
        ));
    }
    let evaluated_args: Vec<Value> = arg_exprs
        .iter()
        .map(|e| {
            let result = eval(e, caller_env, func_defs);
            debug!("Evaluated arg {:?} -> {:?}", e, result);
            result
        })
        .collect::<Result<Vec<Value>, String>>()?;
    let mut local_env = Environment::new();
    for (param_name, arg_value) in params.iter().zip(evaluated_args.into_iter()) {
        local_env.insert(param_name.clone(), arg_value);
    }
    debug!("Local env for '{}': {:?}", fn_name, local_env);

    // Execute the function body block
    match execute_block_body(raw_body, &mut local_env, func_defs, true) {
        // The inner function returns a Value if an explicit return was hit or if it's the last statement's value.
        Ok(Some(val)) => Ok(val), 
        Ok(None) => Ok(Value::Void), // Empty function body
        Err(e) => Err(format!("Function '{}' Execution Error: {}", fn_name, e)),
    }
}

fn run_statement_in_function(stmt: &Statement, env: &mut Environment, func_defs: &FuncDefs) -> Result<FunctionControlFlow, String> {
    debug!("Running statement in function: {:?}", stmt);
    match stmt {
        Statement::Expr(expr) => {
            let result = eval(expr, env, func_defs)?;
            Ok(FunctionControlFlow::Continue(result))
        }
        Statement::Print(opt_format_string, expressions) => {
            let results: Vec<Value> = expressions
                .iter()
                .map(|e| eval(e, env, func_defs))
                .collect::<Result<Vec<Value>, String>>()?;

            let output = if let Some(format_string) = opt_format_string {
                let mut output = format_string.clone();
                let placeholder = "{}";
                let mut current_pos = 0;
                
                for result in results.iter() {
                    let result_str = match result {
                        Value::Integer(n) => format!("{}", n),
                        Value::Float(n) => format!("{}", n),
                        Value::String(s) => s.clone(), 
                        Value::Boolean(b) => format!("{}", if *b { "true" } else { "false" }), 
                        Value::Void => String::from("void"),
                    };
                    if let Some(start) = output[current_pos..].find(placeholder) {
                        let full_start = current_pos + start;
                        let full_end = full_start + placeholder.len();
                        output.replace_range(full_start..full_end, &result_str);
                        current_pos = full_start + result_str.len();
                    } else {
                        return Err(format!("Not enough placeholders ({}) in format string: \"{}\"", placeholder, format_string));
                    }
                }
                output
            } else {
                if results.len() != 1 {
                    return Err("Simple print (without format string) expects exactly one argument".to_string());
                }
                match &results[0] {
                    Value::String(s) => s.clone(), 
                    Value::Boolean(b) => format!("{}", if *b { "true" } else { "false" }), 
                    v => format!("{}", v),         
                }
            };
            
            Ok(FunctionControlFlow::Print(output))
        }
        // UPDATED: Handle If/Else logic
        Statement::If(condition_expr, if_raw_body, else_raw_body) => {
            let condition_val = eval(condition_expr, env, func_defs)?;

            let execute_if = match condition_val {
                Value::Boolean(b) => b,
                _ => return Err(format!("'if' condition must evaluate to a Boolean, found {:?}", condition_val)),
            };

            let body_to_execute = if execute_if {
                if_raw_body
            } else if let Some(else_body) = else_raw_body {
                else_body
            } else {
                return Ok(FunctionControlFlow::Continue(Value::Void)); // No else block, condition false
            };
            
            // Execute the selected block body
            match execute_block_body(body_to_execute, env, func_defs, true) {
                // If the block execution resulted in a value (our signal for an explicit 'return'), 
                // we propagate the return signal.
                Ok(Some(val)) => Ok(FunctionControlFlow::Return(val)),
                // If the block was empty or finished normally, we continue the function's flow by returning Void.
                Ok(None) => Ok(FunctionControlFlow::Continue(Value::Void)),
                Err(e) => Err(e),
            }
        }
        Statement::Def(name, ..) => {
            Err(format!("Function definition '{}' is only allowed at the top level", name))
        }
        Statement::Return(opt_expr) => {
            let return_val = if let Some(expr) = opt_expr {
                eval(expr, env, func_defs)?
            } else {
                Value::Void
            };
            Ok(FunctionControlFlow::Return(return_val))
        }
    }
}

fn run_statement(stmt: &Statement, env: &mut Environment, func_defs: &mut FuncDefs) -> Result<String, String> {
    debug!("Running statement: {:?}", stmt);
    match stmt {
        Statement::Expr(expr) => {
            let result = eval(expr, env, func_defs)?;
            match result {
                Value::Void => Ok(String::new()),
                _ => Ok(format!("{}", result)),
            }
        }
        Statement::Print(opt_format_string, expressions) => {
            let results: Vec<Value> = expressions
                .iter()
                .map(|e| eval(e, env, func_defs))
                .collect::<Result<Vec<Value>, String>>()?;
            
            let output = if let Some(format_string) = opt_format_string {
                let mut output = format_string.clone();
                let placeholder = "{}";
                let mut current_pos = 0;
                
                for result in results.iter() {
                    let result_str = match result {
                        Value::Integer(n) => format!("{}", n),
                        Value::Float(n) => format!("{}", n),
                        Value::String(s) => s.clone(),
                        Value::Boolean(b) => format!("{}", if *b { "true" } else { "false" }), 
                        Value::Void => String::from("void"),
                    };
                    if let Some(start) = output[current_pos..].find(placeholder) {
                        let full_start = current_pos + start;
                        let full_end = full_start + placeholder.len();
                        output.replace_range(full_start..full_end, &result_str);
                        current_pos = full_start + result_str.len();
                    } else {
                        return Err(format!("Not enough placeholders ({}) in format string: \"{}\"", placeholder, format_string));
                    }
                }
                output
            } else {
                if results.len() != 1 {
                    return Err("Simple print (without format string) expects exactly one argument".to_string());
                }
                match &results[0] {
                    Value::String(s) => s.clone(), 
                    Value::Boolean(b) => format!("{}", if *b { "true" } else { "false" }), 
                    v => format!("{}", v),         
                }
            };
            
            // Output to terminal and runlog
            writeln!(io::stdout(), "{}", output).map_err(|e| format!("Failed to write to stdout: {}", e))?;
            io::stdout().flush().map_err(|e| format!("Failed to flush stdout: {}", e))?;
            let mut log_file = OpenOptions::new()
                .create(true)
                .append(true)
                .open("runlog")
                .map_err(|e| format!("Failed to open runlog: {}", e))?;
            writeln!(log_file, "Output: {}", output)
                .map_err(|e| format!("Failed to write to runlog: {}", e))?;
            log_file.flush().map_err(|e| format!("Failed to flush runlog: {}", e))?;
            Ok(output)
        }
        Statement::Def(name, params, raw_body) => {
            func_defs.insert(name.clone(), (params.clone(), raw_body.clone()));
            Ok(String::new())
        }
        Statement::Return(_) => {
            // Top-level return statements are ignored, but will throw an error if used inside a function without proper flow control
            Ok(String::new())
        }
        // UPDATED: Handle If/Else logic at the top level
        Statement::If(condition_expr, if_raw_body, else_raw_body) => {
            let condition_val = eval(condition_expr, env, func_defs)?;

            let execute_if = match condition_val {
                Value::Boolean(b) => b,
                _ => return Err(format!("'if' condition must evaluate to a Boolean, found {:?}", condition_val)),
            };

            let body_to_execute = if execute_if {
                if_raw_body
            } else if let Some(else_body) = else_raw_body {
                else_body
            } else {
                return Ok(String::new()); // No else block, condition false
            };
            
            // Execute the selected block body (top-level execution)
            match execute_block_body(body_to_execute, env, func_defs, false) {
                Ok(_) => Ok(String::new()),
                Err(e) => Err(e),
            }
        }
    }
}

fn main() {
    // Initialize env_logger to write debug! to runlog
    let debug_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("runlog")
        .expect("Failed to open runlog file for debug logging");
    let debug_writer = BufWriter::new(debug_file);
    env_logger::Builder::new()
        .filter_level(LevelFilter::Debug)
        .target(env_logger::Target::Pipe(Box::new(debug_writer)))
        .init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        eprintln!("To test, create a file (e.g., 'test.txt') and run: cargo run -- test.txt");
        return;
    }
    let filename = &args[1];
    let file_content = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            return;
        }
    };
    let mut parser = Parser::new(&file_content);
    let mut env = HashMap::new();
    let mut func_defs: FuncDefs = HashMap::new();
    // Write to runlog instead of println!
    let mut log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("runlog")
        .expect("Failed to open runlog file");
    writeln!(log_file, "--- Starting script execution from {} ---", filename)
        .expect("Failed to write to runlog");
    log_file.flush().expect("Failed to flush runlog");
    match parser.parse() {
        Ok(statements) => {
            debug!("Parsed statements: {:?}", statements);
            for (i, stmt) in statements.into_iter().enumerate() {
                // Write to runlog instead of println!
                writeln!(log_file, "\nExecuting Statement {}", i + 1)
                    .expect("Failed to write to runlog");
                log_file.flush().expect("Failed to flush runlog");
                // Clone func_defs for run_statement if it needs to execute functions
                match run_statement(&stmt, &mut env, &mut func_defs) {
                    Ok(output) => {
                        if !output.is_empty() {
                            writeln!(log_file, "Result: {}", output)
                                .expect("Failed to write to runlog");
                            log_file.flush().expect("Failed to flush runlog");
                        }
                    }
                    Err(e) => {
                        eprintln!("Runtime Error (Statement {}): {}", i + 1, e);
                        writeln!(log_file, "Runtime Error (Statement {}): {}", i + 1, e)
                            .expect("Failed to write error to runlog");
                        break;
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Parsing Error: {}", e);
            writeln!(log_file, "Parsing Error: {}", e)
                .expect("Failed to write error to runlog");
        }
    }
}
