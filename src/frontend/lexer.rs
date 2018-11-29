use failure::{self, bail, format_err};
use regex::Regex;
use std::fmt;
use std::iter::Peekable;
use std::str::{CharIndices, FromStr};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'input> {
    Integer(i64),
    Identifier(&'input str),
    Type(&'input str),
    String(String),

    // Keywords
    Class,
    Else,
    False,
    Fi,
    If,
    In,
    Inherits,
    Isvoid,
    Let,
    Loop,
    Pool,
    Then,
    While,
    Case,
    Esac,
    New,
    Of,
    Not,
    True,

    // Symbols
    LeftCurly,
    RightCurly,
    LeftParen,
    RightParen,
    Semicolon,
    Colon,
    Dot,
    Comma,
    Assign,
    At,
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    LessThan,
    LessThanEqual,
    Equal,
    RightArrow,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Integer(i) => write!(f, "{}", i),
            Token::Identifier(s) | Token::Type(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Class => write!(f, "class"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::Fi => write!(f, "fi"),
            Token::If => write!(f, "if"),
            Token::In => write!(f, "in"),
            Token::Inherits => write!(f, "inherits"),
            Token::Isvoid => write!(f, "isvoid"),
            Token::Let => write!(f, "let"),
            Token::Loop => write!(f, "loop"),
            Token::Pool => write!(f, "pool"),
            Token::Then => write!(f, "then"),
            Token::While => write!(f, "while"),
            Token::Case => write!(f, "case"),
            Token::Esac => write!(f, "esac"),
            Token::New => write!(f, "new"),
            Token::Of => write!(f, "of"),
            Token::Not => write!(f, "not"),
            Token::True => write!(f, "true"),
            Token::LeftCurly => write!(f, "{{"),
            Token::RightCurly => write!(f, "}}"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::Comma => write!(f, ","),
            Token::Assign => write!(f, "<-"),
            Token::At => write!(f, "@"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Negate => write!(f, "~"),
            Token::LessThan => write!(f, "<"),
            Token::LessThanEqual => write!(f, "<="),
            Token::Equal => write!(f, "="),
            Token::RightArrow => write!(f, "=>"),
        }
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    chars: Peekable<CharIndices<'input>>,
    true_re: Regex,
    false_re: Regex,
    class_re: Regex,
    else_re: Regex,
    fi_re: Regex,
    if_re: Regex,
    in_re: Regex,
    inherits_re: Regex,
    isvoid_re: Regex,
    let_re: Regex,
    loop_re: Regex,
    pool_re: Regex,
    then_re: Regex,
    while_re: Regex,
    case_re: Regex,
    esac_re: Regex,
    new_re: Regex,
    of_re: Regex,
    not_re: Regex,
    ident_re: Regex,
    type_re: Regex,
    int_re: Regex,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            input,
            chars: input.char_indices().peekable(),
            true_re: Regex::new(r#"^t(?i)rue\b"#).unwrap(),
            false_re: Regex::new(r#"^f(?i)alse\b"#).unwrap(),
            class_re: Regex::new(r#"^(?i)class\b"#).unwrap(),
            else_re: Regex::new(r#"^(?i)else\b"#).unwrap(),
            fi_re: Regex::new(r#"^(?i)fi\b"#).unwrap(),
            if_re: Regex::new(r#"^(?i)if\b"#).unwrap(),
            in_re: Regex::new(r#"^(?i)in\b"#).unwrap(),
            inherits_re: Regex::new(r#"^(?i)inherits\b"#).unwrap(),
            isvoid_re: Regex::new(r#"^(?i)isvoid\b"#).unwrap(),
            let_re: Regex::new(r#"^(?i)let\b"#).unwrap(),
            loop_re: Regex::new(r#"^(?i)loop\b"#).unwrap(),
            pool_re: Regex::new(r#"^(?i)pool\b"#).unwrap(),
            then_re: Regex::new(r#"^(?i)then\b"#).unwrap(),
            while_re: Regex::new(r#"^(?i)while\b"#).unwrap(),
            case_re: Regex::new(r#"^(?i)case\b"#).unwrap(),
            esac_re: Regex::new(r#"^(?i)esac\b"#).unwrap(),
            new_re: Regex::new(r#"^(?i)new\b"#).unwrap(),
            of_re: Regex::new(r#"^(?i)of\b"#).unwrap(),
            not_re: Regex::new(r#"^(?i)not\b"#).unwrap(),
            ident_re: Regex::new(r#"^[a-z][a-zA-Z0-9_]*\b"#).unwrap(),
            type_re: Regex::new(r#"^[A-Z][a-zA-Z0-9_]*\b"#).unwrap(),
            int_re: Regex::new(r#"^[0-9]+"#).unwrap(),
        }
    }
}

impl<'input> Lexer<'input> {
    fn eat_whitespace(&mut self) {
        while self
            .chars
            .peek()
            .map_or(false, |(_, ch)| ch.is_whitespace())
        {
            let _ = self.chars.next();
        }
    }

    fn eat_single_line_comment(&mut self) {
        assert_eq!(self.chars.next().unwrap().1, '-');
        assert_eq!(self.chars.next().unwrap().1, '-');
        for _ in self.chars.by_ref().take_while(|(_, c)| *c != '\n') {}
    }

    fn eat_multi_line_comment(&mut self) -> Result<(), failure::Error> {
        assert_eq!(self.chars.next().unwrap().1, '(');
        assert_eq!(self.chars.next().unwrap().1, '*');

        let mut count = 1;
        let mut last_was_star = false;
        let mut last_was_left_paren = false;

        for (_, ch) in self.chars.by_ref() {
            match ch {
                ')' if last_was_star => {
                    count -= 1;
                    if count == 0 {
                        return Ok(());
                    }
                    last_was_left_paren = false;
                    last_was_star = false;
                }
                '(' => {
                    last_was_left_paren = true;
                    last_was_star = false;
                }
                '*' if last_was_left_paren => {
                    count += 1;
                    last_was_left_paren = false;
                    last_was_star = false;
                }
                '*' => {
                    last_was_left_paren = false;
                    last_was_star = true;
                }
                _ => {
                    last_was_left_paren = false;
                    last_was_star = false;
                }
            }
        }

        bail!("EOF in multi-line comment")
    }

    fn string(&mut self) -> Spanned<Token<'input>, usize, failure::Error> {
        let (i, ch) = self.chars.next().unwrap();
        assert_eq!(ch, '"');

        let mut s = String::new();
        let mut last_was_escape = false;
        for (j, ch) in self.chars.by_ref() {
            match ch {
                '\\' if last_was_escape => {
                    s.push('\\');
                    last_was_escape = false;
                }
                '\\' => {
                    last_was_escape = true;
                }

                '\n' if last_was_escape => {
                    s.push('\n');
                    last_was_escape = false;
                }
                '\n' => bail!(
                    "found newline before end of string beginning at offset {}",
                    i
                ),

                'b' if last_was_escape => {
                    s.push('\u{8}');
                    last_was_escape = false;
                }
                't' if last_was_escape => {
                    s.push('\t');
                    last_was_escape = false;
                }
                'n' if last_was_escape => {
                    s.push('\n');
                    last_was_escape = false;
                }
                'f' if last_was_escape => {
                    s.push('\u{c}');
                    last_was_escape = false;
                }
                '"' if last_was_escape => {
                    s.push('"');
                    last_was_escape = false;
                }
                '"' => {
                    if s.len() > 1024 {
                        bail!("string constant longer than 1024");
                    }
                    if s.contains('\0') {
                        bail!("string contains null character");
                    }
                    return Ok((i, Token::String(s), j));
                }

                c => {
                    s.push(c);
                    last_was_escape = false;
                }
            }
        }

        bail!("unterminated string beginning at offset {}", i)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, failure::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.eat_whitespace();
            if self.chars.peek().is_none() {
                return None;
            }

            let i = self.chars.peek().cloned().unwrap().0;
            if self.input[i..].len() >= 2 {
                if &self.input[i..i + 2] == "--" {
                    self.eat_single_line_comment();
                    continue;
                } else if &self.input[i..i + 2] == "(*" {
                    if let Err(e) = self.eat_multi_line_comment() {
                        return Some(Err(e));
                    }
                    continue;
                }
            }

            break;
        }

        let (i, ch) = self.chars.peek().cloned().unwrap();

        if ch == '"' {
            return Some(self.string());
        }

        macro_rules! regex {
            ( $( $re:expr => |$len:ident| $tok:expr ; )* ) => {
                $(
                    if let Some(m) = $re.find(&self.input[i..]) {
                        let $len = m.end();
                        for _ in 0..$len {
                            let _ = self.chars.next();
                        }
                        let tok = $tok;
                        return Some(Ok((i, tok, i + $len)));
                    }
                )*
            }
        }

        regex! {
            self.true_re => |n| Token::True;
            self.false_re => |n| Token::False;
            self.class_re => |n| Token::Class;
            self.else_re => |n| Token::Else;
            self.fi_re => |n| Token::Fi;
            self.if_re => |n| Token::If;
            self.in_re => |n| Token::In;
            self.inherits_re => |n| Token::Inherits;
            self.isvoid_re => |n| Token::Isvoid;
            self.let_re => |n| Token::Let;
            self.loop_re => |n| Token::Loop;
            self.pool_re => |n| Token::Pool;
            self.then_re => |n| Token::Then;
            self.while_re => |n| Token::While;
            self.case_re => |n| Token::Case;
            self.esac_re => |n| Token::Esac;
            self.new_re => |n| Token::New;
            self.of_re => |n| Token::Of;
            self.not_re => |n| Token::Not;
            self.ident_re => |n| Token::Identifier(&self.input[i..i + n]);
            self.type_re => |n| Token::Type(&self.input[i..i + n]);
            self.int_re => |n| Token::Integer(match i64::from_str(&self.input[i..i + n]) {
                Ok(i) => i,
                Err(e) => return Some(Err(e.into())),
            });
        }

        macro_rules! multi_char {
            ( $( $s:expr => $tok:ident ; )* ) => {
                $(
                    let n = $s.len();
                    if i + n <= self.input.len() && &self.input[i..i + n] == $s {
                        for _ in 0..n {
                            let _ = self.chars.next();
                        }
                        return Some(Ok((i, Token::$tok, i + n)));
                    }
                )*
            }
        }

        multi_char! {
            "<-" => Assign;
            "<=" => LessThanEqual;
            "=>" => RightArrow;
        }

        macro_rules! single_char {
            ( $( $ch:expr => $tok:ident ; )* ) => {
                $(
                    if let Some((i, $ch)) = self.chars.peek().cloned() {
                        self.chars.next();
                        return Some(Ok((i, Token::$tok, i + 1)));
                    }
                )*
            }
        }

        if i + 2 < self.input.len() && &self.input[i..i + 2] == "*)" {
            let _ = self.chars.next();
            let _ = self.chars.next();
            return Some(Err(format_err!("unmatched `*)` at offset {}", i)));
        }

        single_char! {
            '{' => LeftCurly;
            '}' => RightCurly;
            '(' => LeftParen;
            ')' => RightParen;
            ';' => Semicolon;
            ':' => Colon;
            '.' => Dot;
            ',' => Comma;
            '@' => At;
            '+' => Add;
            '-' => Sub;
            '*' => Mul;
            '/' => Div;
            '~' => Negate;
            '<' => LessThan;
            '=' => Equal;
        }

        Some(Err(format_err!(
            "Unexpected token at offset {}: '{}'",
            i,
            self.chars.next().unwrap().1
        )))
    }
}
