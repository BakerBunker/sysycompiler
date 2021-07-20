use colored::Colorize;

use crate::TokenType;

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::Read;
use std::rc::Rc;

pub fn tokenize(path: String) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(Rc::new(path));
    tokenizer.scan(&keyword_map(), &double_symbol_map());

    if tokenizer.is_panicked {
        panic!("Lexer panicked");
    }
    tokenizer.tokens
}

fn keyword_map() -> HashMap<String, TokenType> {
    let mut map = HashMap::new();
    map.insert("int".into(), TokenType::Int);
    map.insert("void".into(), TokenType::Void);
    map.insert("const".into(), TokenType::Const);
    map.insert("if".into(), TokenType::If);
    map.insert("while".into(), TokenType::While);
    map.insert("break".into(), TokenType::Break);
    map.insert("continue".into(), TokenType::Continue);
    map.insert("return".into(), TokenType::Return);
    map.insert("else".into(), TokenType::Else);
    map
}

fn double_symbol_map() -> HashMap<String, TokenType> {
    let mut map = HashMap::new();
    map.insert("==".into(), TokenType::Equal);
    map.insert("!=".into(), TokenType::NotEqual);
    map.insert(">=".into(), TokenType::GreaterEqual);
    map.insert("<=".into(), TokenType::LesserEqual);
    map.insert("&&".into(), TokenType::And);
    map.insert("||".into(), TokenType::Or);
    map
}
#[derive(Clone)]
pub struct Token {
    pub ttype: TokenType,

    //for error message
    pub buf: Rc<Vec<char>>,
    pub filepath: Rc<String>,
    pub lstart: Rc<usize>,
    pub start: usize,
    pub end: usize,
    pub lineno: usize,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content: String = self.buf[self.start..self.end].iter().collect();
        write!(
            f,
            "Token{{\n\ttype:{:?}\n\tcontent: {} \n\tstart:{}\n\tend:{}\n\tlineno:{}\n}}",
            self.ttype,
            content,
            self.start - *self.lstart,
            self.end - *self.lstart,
            self.lineno
        )
    }
}

impl Token {
    pub fn new(
        ttype: TokenType,
        buf: Rc<Vec<char>>,
        filepath: Rc<String>,
        lstart: Rc<usize>,
        start: usize,
        lineno: usize,
    ) -> Self {
        Token {
            ttype,
            buf,
            filepath,
            lstart,
            start,
            end: 0,
            lineno,
        }
    }
}

enum CharacterType {
    WhiteSpace, // ' ','\'t'
    NewLine,    // '\n'
    NonDigit,   // 'a-z''A-Z'
    Digit,      // '0-9'
    NonAlpha(char),
}

struct Tokenizer {
    chars: Rc<Vec<char>>,
    pos: usize,
    lineno: usize,
    lstarts: Vec<usize>,
    tokens: Vec<Token>,

    //for error message
    filepath: Rc<String>,
    is_panicked: bool,
}

impl Tokenizer {
    fn new(path: Rc<String>) -> Self {
        Tokenizer {
            chars: Rc::new(Self::read_file(&path)),
            pos: 0,
            lineno: 1,
            lstarts: vec![0],
            tokens: vec![],
            filepath: path,
            is_panicked: false,
        }
    }

    fn read_file(path: &str) -> Vec<char> {
        let mut string = String::new();
        let mut file = File::open(path).expect("File cannot be opened");
        file.read_to_string(&mut string)
            .expect("File cannot be read as string");
        string.chars().collect()
    }

    fn get_character(&self) -> Option<CharacterType> {
        self.chars.get(self.pos).map(|c| {
            if c == &' ' || c == &'\t' {
                CharacterType::WhiteSpace
            } else if c == &'\n' {
                CharacterType::NewLine
            } else if c.is_ascii_alphabetic() || c == &'_' {
                CharacterType::NonDigit
            } else if c.is_ascii_digit() {
                CharacterType::Digit
            } else {
                CharacterType::NonAlpha(*c)
            }
        })
    }

    fn scan(
        &mut self,
        keywords: &HashMap<String, TokenType>,
        double_symbols: &HashMap<String, TokenType>,
    ) {
        while let Some(head) = self.get_character() {
            match head {
                CharacterType::WhiteSpace => {
                    self.pos += 1;
                }
                CharacterType::NewLine => {
                    self.lineno += 1;
                    self.pos += 1;
                    self.lstarts.push(self.pos);
                }
                CharacterType::NonDigit => self.ident_or_keyword(keywords),
                CharacterType::Digit => self.number(),
                CharacterType::NonAlpha('/') => match self.chars.get(self.pos + 1) {
                    Some('/') => self.linecomment(),
                    Some('*') => self.blockcomment(),

                    //divide
                    _ => {
                        let mut t = self.new_token(TokenType::Divide);
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                    }
                },
                CharacterType::NonAlpha(_) => {
                    //multi-character symbols
                    if let Some(sym) = self.chars.get(self.pos..self.pos + 2) {
                        let _sym: String = sym.iter().collect();
                        if let Some(ttype) = double_symbols.get(&_sym) {
                            let mut t = self.new_token(ttype.clone());
                            self.pos += 2;
                            t.end = self.pos;
                            self.tokens.push(t);
                            continue;
                        }
                    }

                    //single-character symbols
                    if let Some(ttype) = Self::single_char_symbol_match(self.chars[self.pos]) {
                        let mut t = self.new_token(ttype.clone());
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                    } else {
                        self.error("Unknown Character", "Check if it is ascii character");
                    }
                }
            }
        }
    }

    fn new_token(&self, ttype: TokenType) -> Token {
        Token::new(
            ttype,
            self.chars.clone(),
            self.filepath.clone(),
            Rc::new(self.lstarts[self.lineno - 1]),
            self.pos,
            self.lineno,
        )
    }

    fn number(&mut self) {
        match self.chars.get(self.pos..self.pos + 2) {
            Some(&['0', 'x']) | Some(&['0', 'X']) => {
                self.pos += 2;
                self.parse_number(16);
            }
            Some(&['0', _]) => {
                self.parse_number(8);
            }
            _ => self.parse_number(10),
        }
    }

    fn parse_number(&mut self, base: u32) {
        let mut sum = 0;
        let mut len = 0;
        for c in self.chars[self.pos..].iter() {
            if let Some(val) = c.to_digit(base) {
                sum = sum * base as i32 + val as i32;
                len += 1;
            } else {
                break;
            }
        }
        let mut t = self.new_token(TokenType::Number(sum));
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    fn ident_or_keyword(&mut self, keywords: &HashMap<String, TokenType>) {
        let mut len = 1;
        while let Some(c) = self.chars.get(self.pos + len) {
            if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == &'_' {
                len += 1;
            } else {
                break;
            }
        }

        let name: String = self.chars[self.pos..self.pos + len].iter().collect();
        let mut t: Token;
        if let Some(ttype) = keywords.get(&name) {
            t = self.new_token(ttype.clone())
        } else {
            t = self.new_token(TokenType::Ident(name))
        }
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    fn linecomment(&mut self) {
        while self.chars.get(self.pos) != Some(&'\n') {
            self.pos += 1;
        }
    }

    fn blockcomment(&mut self) {
        self.pos += 2;
        while let Some(c) = self.chars.get(self.pos) {
            if c == &'*' {
                if self.chars.get(self.pos + 1) == Some(&'/') {
                    self.pos += 2;
                    return;
                }
            }
            if c == &'\n' {
                self.lineno += 1;
                self.lstarts.push(self.pos + 1);
            }
            self.pos += 1;
        }
        self.error("Unclosed comment block", "Consider close the comment block");
    }

    fn single_char_symbol_match(c: char) -> Option<TokenType> {
        use TokenType::*;
        match c {
            '=' => Some(Assign),
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Multiply),
            '/' => Some(Divide),
            '%' => Some(Modulus),
            '<' => Some(LesserThan),
            '>' => Some(GreaterThan),
            '!' => Some(Not),
            ';' => Some(Semicolon),
            ',' => Some(Comma),
            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '[' => Some(LeftBracket),
            ']' => Some(RightBracket),
            '{' => Some(LeftBrace),
            '}' => Some(RightBrace),
            _ => None,
        }
    }

    fn error(&mut self, msg: &str, suggest: &str) {
        let mut len = 0;
        let lstart = self.lstarts[self.lineno - 1];
        for c in self.chars[lstart..].iter() {
            if c == &'\n' {
                break;
            }
            len += 1;
        }
        let errline: String = self.chars[lstart..lstart + len].iter().collect();

        //Error message
        println!("{}: {}", "lexer error".red().bold(), msg.bold());
        println!(
            "  {} {}:{}:{}",
            "-->".blue().bold(),
            self.filepath,
            self.lineno,
            self.pos - lstart + 1
        );
        println!("   {}", "|".blue().bold());
        println!(
            "{:3}{} {}",
            self.lineno.to_string().blue().bold(),
            "|".blue().bold(),
            errline
        );

        //Suggestion message
        print!("   {}", "|".blue().bold());
        for _ in 0..self.pos - lstart + 1 {
            print!("{}", ' ');
        }
        println!("{} {}", "^".red().bold(), suggest.red().bold());

        println!("   {}", "|".blue().bold());
        //panic!("{}",msg);
        self.pos += 1;
        self.is_panicked = true;
    }
}
