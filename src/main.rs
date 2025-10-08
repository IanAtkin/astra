use std::fmt;
use std::env;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::{self, Write, BufWriter};
use log::{debug, LevelFilter};
use env_logger;

// --- Big Integer Imports ---
use num_bigint::BigInt;
// Imported traits to enable methods like is_positive (Signed), to_u32, and to_f64 (ToPrimitive)
use num_traits::{Zero, One, Signed, ToPrimitive}; 
// ---------------------------

// --- Value and AST Definitions ---

#[derive(Debug, Clone, PartialEq)] 
enum Value {
    // Changed i64 to BigInt to support arbitrary precision arithmetic
    Integer(BigInt), 
    Float(f64),
    String(String),
    Boolean(bool), 
    Void,
}

impl Value {
    /// Helper to check if a value is numeric (Integer or Float)
    fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            // Note: Display of Value::String includes quotes
            Value::String(s) => write!(f, "\"{}\"", s), 
            Value::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Value::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Num(String), // Stores raw number string to preserve type distinction (e.g., "1" vs "1.0")
    Str(String),
    Prefix(char, Box<Expr>),
    Infix(Box<Expr>, char, Box<Expr>),
    Cmp(Box<Expr>, String, Box<Expr>), 
    // ADDED: Logic variant for 'and' and 'or'
    Logic(Box<Expr>, String, Box<Expr>),
    Call(String, Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(id) => write!(f, "{}", id),
            Expr::Num(s) => write!(f, "{}", s), 
            Expr::Str(s) => write!(f, "\"{}\"", s),
            Expr::Prefix(op, expr) => write!(f, "({} {})", op, expr),
            Expr::Infix(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Cmp(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs), 
            // ADDED: Logic display
            Expr::Logic(lhs, op, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
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

#[derive(Debug, Clone)] // Added Clone to Statement for use in the interpreter
enum Statement {
    Expr(Expr),
    Print(Option<String>, Vec<Expr>),
    // CHANGE: Function body now Vec<Statement>
    Def(String, Vec<String>, Vec<Statement>),
    Return(Option<Expr>),
    // CHANGE: If and Else bodies now Vec<Statement>
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),
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
            // The token holds the original string representation ("1" or "1.0")
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
            // MODIFIED: Added 'and' and 'or' as keywords
            if ident == "print" || ident == "def" || ident == "fn" || ident == "return" || ident == "if" || ident == "else" || ident == "and" || ident == "or" {
                Token::Keyword(ident)
            } else {
                Token::Ident(ident)
            }
        } 
        // Compound Assignment and Single Arithmetic Operators (+, -, *, /, %, ^)
        else if "+-*/%^".contains(ch) {
            if self.peek_char() == Some('=') {
                self.next_char(); // consume '='
                // Use Cmp for compound assignment tokens to carry the string value
                return Token::Cmp(format!("{}{}", ch, '=')); 
            }
            Token::Op(ch) // Single arithmetic operator
        }
        // Comparison and Simple Assignment (=)
        else if ch == '=' {
            if self.peek_char() == Some('=') {
                self.next_char(); 
                if self.peek_char() == Some('=') {
                    self.next_char();
                    return Token::Cmp("===".to_string());
                }
                return Token::Cmp("==".to_string());
            }
            Token::Op(ch) // Simple assignment '='
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
            
            // Handle comments (';' until newline)
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
        //debug!("Advanced to token {:?}", self.current);
    }

    fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = Vec::new();
        while self.current != Token::Eof {
            //debug!("Parsing statement, current token: {:?}", self.current);
            let stmt = match self.current.clone() {
                Token::Keyword(k) if k == "print" => self.parse_print_statement(),
                Token::Keyword(k) if k == "fn" => self.parse_fn_statement(),
                Token::Keyword(k) if k == "return" => self.parse_return_statement(),
                Token::Keyword(k) if k == "if" => self.parse_if_statement(),
                // Defensive check: The assignment operator cannot start a statement.
                Token::Op(op) if op == '=' => {
                    return Err("The assignment operator '=' cannot start a statement. Assignment must follow a variable (e.g., x = 10).".to_string());
                }
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

    // CHANGE: parse_block_body now returns Vec<Statement> and directly parses tokens
    fn parse_block_body(&mut self) -> Result<Vec<Statement>, String> {
        // The calling function (parse_fn, parse_if) must ensure self.current is the token *after* '['
        let mut statements = Vec::new();

        // Loop until ']' or EOF
        while self.current != Token::Op(']') && self.current != Token::Eof {
            let stmt = match self.current.clone() {
                // Include all recognized statement types (except 'fn', which should only be top-level)
                Token::Keyword(k) if k == "print" => self.parse_print_statement(),
                Token::Keyword(k) if k == "return" => self.parse_return_statement(),
                Token::Keyword(k) if k == "if" => self.parse_if_statement(),
                // Ensure proper error handling for deprecated/misplaced keywords
                Token::Keyword(k) if k == "def" => return Err(format!("The 'def' keyword is deprecated.")),
                Token::Keyword(k) if k == "else" => return Err(format!("The 'else' keyword must immediately follow a closing ']' of an 'if' block.")),
                Token::Op(op) if op == '=' => {
                    return Err("The assignment operator '=' cannot start a statement.".to_string());
                }
                // Default: parse as an expression statement
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

        self.advance(); // consume the closing ']'
        
        Ok(statements)
    }

    fn parse_if_statement(&mut self) -> Result<Statement, String> {
        //debug!("Parsing if statement");
        self.advance(); // consume 'if'

        if self.current != Token::Op('(') {
            return Err(format!("Expected '(' after 'if', found {:?}", self.current));
        }
        self.advance(); // consume '('

        let condition = self.expr_bp(0)?;

        if self.current != Token::Op(')') {
            return Err(format!("Expected ')' after if condition, found {:?}", self.current));
        }
        self.advance(); // consume ')'

        if self.current != Token::Op('[') {
            return Err(format!("Expected '[' to start if body, found {:?}", self.current));
        }
        
        self.advance(); // CRITICAL: Consume the opening '['
        // CHANGE: if_body is now Vec<Statement>
        let if_body_statements = self.parse_block_body()?;

        let mut else_body_statements: Option<Vec<Statement>> = None;

        if let Token::Keyword(k) = self.current.clone() {
            if k == "else" {
                //debug!("Found 'else' keyword");
                self.advance(); // consume 'else'
                
                if self.current != Token::Op('[') {
                    return Err(format!("Expected '[' to start else body, found {:?}", self.current));
                }
                
                self.advance(); // CRITICAL: Consume the opening '['
                // CHANGE: else_body is now Vec<Statement>
                else_body_statements = Some(self.parse_block_body()?);
            }
        }
        
        debug!("Parsed if statement with condition {:?}, if body {:?}, and else body {:?}", condition, if_body_statements, else_body_statements);
        // CHANGE: Store the Vec<Statement>
        Ok(Statement::If(condition, if_body_statements, else_body_statements))
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
        //debug!("Parsing print statement");
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
                //debug!("Parsing print argument (formatted), current token: {:?}", self.current);
                let expr = self.expr_bp(0)?;
                expressions.push(expr);
            }

        } else if self.current != Token::Op(')') {
            //debug!("Parsing print argument (simple), current token: {:?}", self.current);
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
        //debug!("Parsing fn statement");
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
        
        self.advance(); // CRITICAL: Consume the opening '['
        // CHANGE: raw_body is now a Vec<Statement>
        let body_statements = self.parse_block_body()?;
        
        debug!("Parsed fn {}({:?}) [{:?}]", fn_name, params, body_statements);
        // CHANGE: Store the Vec<Statement>
        Ok(Statement::Def(fn_name, params, body_statements))
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
        //debug!("Parsing expression with min_bp {}, current token: {:?}", min_bp, self.current);
        let mut lhs = match self.current.clone() {
            // Store the raw number string
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
            
            // Check for logical keywords as operators
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

            // 1. Check for Compound Assignment (e.g., +=, -=) - MUST be desugared here
            if op_str.len() == 2 && op_str.ends_with('=') && "+-*/%^".contains(op_str.chars().next().unwrap()) {
                let actual_op = op_str.chars().next().unwrap(); // e.g., '+' or '-'
                
                // Compound assignment (A += B) has the same precedence (2) as simple assignment (A = B)
                if 2 < min_bp {
                    break;
                }
                
                self.advance(); // consume the compound operator token (e.g., +=)
                
                // The right hand side of the assignment
                let rhs = self.expr_bp(1)?; // Right binding power of assignment is 1

                // Left-hand side must be a variable
                let var_id = match lhs {
                    Expr::Var(ref id) => Expr::Var(id.clone()), // Clone the Var(id) for both LHS and RHS of new Infix
                    _ => return Err(format!("Left-hand side of compound assignment '{}' must be a variable", op_str)),
                };

                // Desugar: x += 5  -->  x = (x + 5)
                // 1a. Create the arithmetic expression: (x + 5)
                let arithmetic_expr = Expr::Infix(Box::new(var_id.clone()), actual_op, Box::new(rhs));
                
                // 1b. Overwrite LHS with the full assignment: x = (x + 5)
                // Use '=' as the operator for the final AST node
                lhs = Expr::Infix(Box::new(var_id), '=', Box::new(arithmetic_expr));
                continue;
            }

            // 2. Check for simple assignment, comparison, standard infix operators OR LOGIC OPS
            if let Some((l_bp, r_bp, is_cmp)) = binding_power(op_str.as_str()) {
                if l_bp < min_bp {
                    break;
                }
                self.advance();
                //debug!("Parsing infix/cmp/logic op {}, right expr with bp {}", op_str, r_bp);
                let rhs = self.expr_bp(r_bp)?;
                
                lhs = if is_cmp {
                    // Cmp covers ==, !=, <, >, <=, >=, ===, !==
                    Expr::Cmp(Box::new(lhs), op_str, Box::new(rhs))
                } else if is_logic_op {
                    // NEW: Logic covers "and" and "or"
                    Expr::Logic(Box::new(lhs), op_str, Box::new(rhs))
                }
                 else {
                    // Infix covers simple assignment (=) and standard arithmetic (+, -, *, /, %, ^)
                    let single_char_op = op_str.chars().next().unwrap(); 
                    Expr::Infix(Box::new(lhs), single_char_op, Box::new(rhs))
                };
                continue;
            }
            break;
        }
        //debug!("Parsed expression: {:?}", lhs);
        Ok(lhs)
    }
}

fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 10),
        _ => ((), 0),
    }
}

// MODIFIED binding_power to introduce 'or' and 'and', and raise precedence of Cmp
fn binding_power(op: &str) -> Option<(u8, u8, bool)> { // (l_bp, r_bp, is_comparison)
    match op {
        "=" => Some((2, 1, false)), // Simple Assignment
        "or" => Some((3, 4, false)), // Logical OR (Lowest precedence)
        "and" => Some((5, 6, false)), // Logical AND
        // Comparison (Raised to 7/8 to be higher than AND/OR)
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "===" | "!==" => Some((7, 8, true)), 
        "+" | "-" => Some((9, 10, false)), // Addition/Subtraction
        "*" | "/" | "%" => Some((11, 12, false)), // Multiplication/Division/Modulo
        "^" => Some((13, 14, false)), // Exponentiation (Highest precedence)
        _ => None,
    }
}

// --- Interpreter ---

type Environment = HashMap<String, Value>;
// CHANGE: Function definition now stores Vec<Statement>
type FuncDefs = HashMap<String, (Vec<String>, Vec<Statement>)>;

enum FunctionControlFlow {
    Continue(Value), 
    Return(Value),   
    Print(String),   
}

// REMOVED: execute_block_body function is no longer needed.

fn eval(expr: &Expr, env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    //debug!("Evaluating expr: {:?}", expr);
    match expr {
        // Logic to determine Integer vs Float from the original string
        Expr::Num(s) => {
            if s.contains('.') {
                let f = s.parse::<f64>().map_err(|e| format!("Invalid float: {}", e))?;
                Ok(Value::Float(f))
            } else {
                // Parse directly into BigInt
                let i = s.parse::<BigInt>().map_err(|e| format!("Invalid integer: {}", e))?;
                Ok(Value::Integer(i))
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
                    // Use BigInt negation
                    Value::Integer(n) => Ok(Value::Integer(-n)),
                    Value::Float(n) => Ok(Value::Float(-n)),
                    _ => Err(format!("Unary minus only works on numbers, found {:?}", val)),
                },
                _ => Err(format!("Unknown prefix operator: {}", op)),
            }
        }
        
        // Arithmetic (+, -, *, /, %, ^) - CONSOLIDATED LOGIC
        Expr::Infix(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;

            // Use a single match to cover all type combinations, preventing move errors.
            match (left_val, right_val) {
                
                // 1. Pure BigInt Arithmetic
                (Value::Integer(l), Value::Integer(r)) => {
                    return match op {
                        '+' => Ok(Value::Integer(l + r)),
                        '-' => Ok(Value::Integer(l - r)),
                        '*' => Ok(Value::Integer(l * r)),
                        '%' => {
                            if r.is_zero() {
                                Err("Modulo by zero".to_string())
                            } else {
                                Ok(Value::Integer(l % r))
                            }
                        }
                        '/' => {
                            if r.is_zero() {
                                // Integer division results in an integer, but we still check for zero
                                Err("Division by zero".to_string()) 
                            } else {
                                // Keep integer division as integer division (no float promotion)
                                Ok(Value::Integer(l / r))
                            }
                        }
                        '^' => {
                            // Exponentiation: Base is BigInt, exponent must be converted to u32
                            if r.is_positive() && r <= BigInt::from(u32::MAX) { 
                                // to_u32 is available due to ToPrimitive trait import
                                let exp: u32 = r.to_u32().ok_or("Exponent too large to convert to u32")?; 
                                Ok(Value::Integer(l.pow(exp)))
                            } else if r.is_zero() {
                                Ok(Value::Integer(BigInt::one()))
                            } else {
                                Err("Integer exponentiation only supports positive exponents up to u32 max".to_string())
                            }
                        }
                        _ => Err(format!("Unknown numeric infix operator: {}", op)),
                    };
                }

                // 2. String Concatenation (+) - only works if both are strings
                (Value::String(mut l), Value::String(r)) if *op == '+' => {
                    l.push_str(&r);
                    return Ok(Value::String(l));
                }
                
                // 3. Mixed or Float Arithmetic (Coerce to f64)
                (l, r) if l.is_number() && r.is_number() => {
                    // Coercion: l and r are guaranteed to be Int or Float.
                    // to_f64 is available due to ToPrimitive import
                    let l_f = match l {
                        Value::Float(f) => f,
                        Value::Integer(i) => i.to_f64().ok_or("Left BigInt too large for float conversion")?, 
                        _ => unreachable!(), 
                    };
                    let r_f = match r {
                        Value::Float(f) => f,
                        Value::Integer(i) => i.to_f64().ok_or("Right BigInt too large for float conversion")?,
                        _ => unreachable!(), 
                    };

                    let result_f = match op {
                        '+' => Ok(l_f + r_f),
                        '-' => Ok(l_f - r_f),
                        '*' => Ok(l_f * r_f),
                        '%' => {
                            if r_f.abs() < f64::EPSILON {
                                Err("Modulo by zero in float operation".to_string())
                            } else {
                                Ok(l_f % r_f)
                            }
                        }
                        '/' => {
                            if r_f.abs() < f64::EPSILON {
                                Err("Division by zero in float operation".to_string())
                            } else {
                                Ok(l_f / r_f)
                            }
                        }
                        '^' => Ok(l_f.powf(r_f)),
                        _ => Err(format!("Unknown numeric infix operator: {}", op)),
                    }?;
                    
                    Ok(Value::Float(result_f))
                }

                // 4. Incompatible Types (Error)
                (l, r) => Err(format!("Incompatible types for operator '{}': {:?} and {:?}", op, l, r)),
            }
        }

        // Comparison (==, !=, <, >, <=, >=, ===, !==)
        Expr::Cmp(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;
            let right_val = eval(rhs, env, func_defs)?;
            
            let result = match op.as_str() {
                // STRICT Equality/Inequality (value AND type must match exactly)
                "===" => left_val == right_val,
                "!==" => left_val != right_val,
                
                // NON-STRICT Equality/Inequality (value must match, type coercion between Int/Float)
                "==" | "!=" => {
                    let non_strict_equal = match (&left_val, &right_val) {
                        // Exact match (Value and Type)
                        (l, r) if l == r => true,
                        // Non-strict coercion for BigInt/Float
                        (Value::Integer(l), Value::Float(r)) => {
                            // to_f64 is available due to ToPrimitive trait import.
                            l.to_f64().map_or(false, |l_f| l_f == *r)
                        }
                        (Value::Float(l), Value::Integer(r)) => {
                            // to_f64 is available due to ToPrimitive trait import.
                            r.to_f64().map_or(false, |r_f| *l == r_f)
                        }
                        // All other combinations are false (String/Bool/Void != Int/Float, etc.)
                        _ => false,
                    };

                    if op.as_str() == "==" { non_strict_equal } else { !non_strict_equal }
                },
                
                // Ordering Comparisons: require same type for ordering
                "<" | ">" | "<=" | ">=" => {
                    match (&left_val, &right_val) {
                        (Value::Integer(l), Value::Integer(r)) => match op.as_str() {
                            "<" => l < r, ">" => l > r, "<=" => l <= r, ">=" => l >= r, _ => unreachable!(),
                        },
                        (Value::Float(l), Value::Float(r)) => match op.as_str() {
                            "<" => l < r, ">" => l > r, "<=" => l <= r, ">=" => r >= r, _ => unreachable!(), 
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

        // NEW: Logical Operators (AND, OR)
        Expr::Logic(lhs, op, rhs) => {
            let left_val = eval(lhs, env, func_defs)?;

            // Short-circuit evaluation
            let short_circuit_val = match (op.as_str(), &left_val) {
                // False AND anything is False
                ("and", Value::Boolean(false)) => Some(Value::Boolean(false)), 
                // True OR anything is True
                ("or", Value::Boolean(true)) => Some(Value::Boolean(true)),   
                _ => None,
            };

            if let Some(val) = short_circuit_val {
                return Ok(val);
            }
            
            // If not short-circuited, evaluate RHS
            let right_val = eval(rhs, env, func_defs)?;

            match (op.as_str(), left_val, right_val) {
                // Since we passed short-circuiting, the left must be a Boolean as well
                ("and", Value::Boolean(l_b), Value::Boolean(r_b)) => Ok(Value::Boolean(l_b && r_b)),
                ("or", Value::Boolean(l_b), Value::Boolean(r_b)) => Ok(Value::Boolean(l_b || r_b)),
                
                // Error on incompatible types (if one wasn't a boolean, or if the left was a boolean but the right wasn't)
                (op_str, l, r) => {
                    Err(format!("Logical operator '{}' only works on Booleans. Found {:?} and {:?}", op_str, l, r))
                }
            }
        }
    }
}

// CHANGE: function now uses Vec<Statement>
fn execute_function(fn_name: &str, arg_exprs: &[Expr], caller_env: &mut Environment, func_defs: &FuncDefs) -> Result<Value, String> {
    debug!("Executing function '{}', args: {:?}", fn_name, arg_exprs);
    
    // CHANGE: Retrieve Vec<Statement>
    let (params, body_statements) = func_defs.get(fn_name)
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
            //debug!("Evaluated arg {:?} -> {:?}", e, result);
            result
        })
        .collect::<Result<Vec<Value>, String>>()?;
    
    let mut local_env = Environment::new();
    for (param_name, arg_value) in params.iter().zip(evaluated_args.into_iter()) {
        local_env.insert(param_name.clone(), arg_value);
    }
    //debug!("Local env for '{}': {:?}", fn_name, local_env);

    let mut last_value = Value::Void;

    // CHANGE: Loop through the pre-parsed statements directly
    for (i, stmt) in body_statements.iter().enumerate() {
        match run_statement_in_function(stmt, &mut local_env, func_defs) {
            Ok(flow) => {
                match flow {
                    FunctionControlFlow::Return(val) => {
                        // Explicit return
                        //debug!("Explicit return triggered from block with value: {:?}", val);
                        return Ok(val);
                    }
                    FunctionControlFlow::Continue(val) => {
                        last_value = val;
                    }
                    FunctionControlFlow::Print(output) => {
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
                return Err(format!("Function '{}' Execution Error (Stmt {}): {}", fn_name, i + 1, e));
            }
        }
    }
    
    // Implicit return of the last expression value or Void
    Ok(last_value)
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
        // CHANGE: Uses Vec<Statement> for bodies
        Statement::If(condition_expr, if_statements, else_opt_statements) => {
            let condition_val = eval(condition_expr, env, func_defs)?;

            let execute_if = match condition_val {
                Value::Boolean(b) => b,
                _ => return Err(format!("'if' condition must evaluate to a Boolean, found {:?}", condition_val)),
            };

            let body_to_execute = if execute_if {
                Some(if_statements)
            } else if let Some(else_statements) = else_opt_statements {
                Some(else_statements)
            } else {
                return Ok(FunctionControlFlow::Continue(Value::Void)); 
            };
            
            let mut last_value = Value::Void;
            
            // Loop through the statements in the block
            if let Some(statements) = body_to_execute {
                for stmt in statements.iter() {
                    match run_statement_in_function(stmt, env, func_defs) {
                        Ok(flow) => {
                            match flow {
                                FunctionControlFlow::Return(val) => {
                                    // Propagate return flow up the call stack
                                    return Ok(FunctionControlFlow::Return(val)); 
                                }
                                FunctionControlFlow::Continue(val) => {
                                    last_value = val;
                                }
                                FunctionControlFlow::Print(output) => {
                                    writeln!(io::stdout(), "{}", output).map_err(|e| format!("Failed to write to stdout: {}", e))?;
                                    io::stdout().flush().map_err(|e| format!("Failed to flush stdout: {}", e))?;
                                    let mut log_file = OpenOptions::new()
                                        .create(true)
                                        .append(true)
                                        .open("runlog")
                                        .map_err(|e| format!("Failed to open runlog: {}", e))?;
                                    writeln!(log_file, "Block Output: {}", output)
                                        .map_err(|e| format!("Failed to write to runlog: {}", e))?;
                                    log_file.flush().map_err(|e| format!("Failed to flush runlog: {}", e))?;
                                }
                            }
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
            
            Ok(FunctionControlFlow::Continue(last_value))
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
        // CHANGE: Store Vec<Statement> directly in FuncDefs
        Statement::Def(name, params, body_statements) => {
            func_defs.insert(name.clone(), (params.clone(), body_statements.clone()));
            Ok(String::new())
        }
        Statement::Return(_) => {
            Ok(String::new())
        }
        // CHANGE: Execute pre-parsed Vec<Statement>
        Statement::If(condition_expr, if_statements, else_opt_statements) => {
            let condition_val = eval(condition_expr, env, func_defs)?;

            let execute_if = match condition_val {
                Value::Boolean(b) => b,
                _ => return Err(format!("'if' condition must evaluate to a Boolean, found {:?}", condition_val)),
            };

            let body_to_execute = if execute_if {
                Some(if_statements)
            } else if let Some(else_statements) = else_opt_statements {
                Some(else_statements)
            } else {
                return Ok(String::new()); 
            };
            
            // Loop through the statements in the block
            if let Some(statements) = body_to_execute {
                for stmt in statements.iter() {
                    match run_statement(stmt, env, func_defs) {
                        Ok(_) => continue,
                        Err(e) => return Err(e),
                    }
                }
            }
            
            Ok(String::new())
        }
    }
}

fn main() {
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
                writeln!(log_file, "\nExecuting Statement {}", i + 1)
                    .expect("Failed to write to runlog");
                log_file.flush().expect("Failed to flush runlog");
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