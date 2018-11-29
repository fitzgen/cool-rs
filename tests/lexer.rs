use cool_rs::frontend::lexer;
use std::fs;
use std::path::Path;
use std::str::FromStr;

fn read_token(line: &str) -> Result<lexer::Token, ()> {
    let space = line.find(" ").unwrap();
    let line = &line[space + 1..];
    let space = line.find(" ").unwrap_or(line.len());
    match &line[..space] {
        "ERROR" => Err(()),
        "CLASS" => Ok(lexer::Token::Class),
        "'{'" => Ok(lexer::Token::LeftCurly),
        "'}'" => Ok(lexer::Token::RightCurly),
        "'('" => Ok(lexer::Token::LeftParen),
        "')'" => Ok(lexer::Token::RightParen),
        "':'" => Ok(lexer::Token::Colon),
        "';'" => Ok(lexer::Token::Semicolon),
        "','" => Ok(lexer::Token::Comma),
        "'+'" => Ok(lexer::Token::Add),
        "'-'" => Ok(lexer::Token::Sub),
        "'*'" => Ok(lexer::Token::Mul),
        "'/'" => Ok(lexer::Token::Div),
        "'.'" => Ok(lexer::Token::Dot),
        "'~'" => Ok(lexer::Token::Negate),
        "'<'" => Ok(lexer::Token::LessThan),
        "'='" => Ok(lexer::Token::Equal),
        "'@'" => Ok(lexer::Token::At),
        "LE" => Ok(lexer::Token::LessThanEqual),
        "ASSIGN" => Ok(lexer::Token::Assign),
        "CASE" => Ok(lexer::Token::Case),
        "ESAC" => Ok(lexer::Token::Esac),
        "DARROW" => Ok(lexer::Token::RightArrow),
        "OF" => Ok(lexer::Token::Of),
        "NEW" => Ok(lexer::Token::New),
        "ELSE" => Ok(lexer::Token::Else),
        "LET" => Ok(lexer::Token::Let),
        "IN" => Ok(lexer::Token::In),
        "INHERITS" => Ok(lexer::Token::Inherits),
        "IF" => Ok(lexer::Token::If),
        "FI" => Ok(lexer::Token::Fi),
        "THEN" => Ok(lexer::Token::Then),
        "WHILE" => Ok(lexer::Token::While),
        "LOOP" => Ok(lexer::Token::Loop),
        "POOL" => Ok(lexer::Token::Pool),
        "ISVOID" => Ok(lexer::Token::Isvoid),
        "NOT" => Ok(lexer::Token::Not),
        "BOOL_CONST" => {
            let val = &line[space + 1..].to_lowercase();
            let val = bool::from_str(&val).unwrap();
            Ok(if val {
                lexer::Token::True
            } else {
                lexer::Token::False
            })
        }
        "TYPEID" => {
            let tid = &line[space + 1..];
            Ok(lexer::Token::Type(tid))
        }
        "OBJECTID" => {
            let oid = &line[space + 1..];
            Ok(lexer::Token::Identifier(oid))
        }
        "STR_CONST" => {
            let s = &line[space + 1..];
            let re = regex::Regex::new(r#"(\\0\d+)"#).unwrap();
            let s = re.replace_all(&s, |caps: &regex::Captures| {
                let n = u8::from_str_radix(&caps[0][1..], 8).unwrap();
                (n as char).to_string()
            });
            let s = if let Some(Ok((_, lexer::Token::String(s), _))) = lexer::Lexer::new(&s).next()
            {
                s
            } else {
                panic!()
            };
            Ok(lexer::Token::String(s.to_string()))
        }
        "INT_CONST" => {
            let val = &line[space + 1..];
            let val = match i64::from_str(val) {
                Ok(i) => i,
                Err(_) => return Err(()),
            };
            Ok(lexer::Token::Integer(val))
        }
        token_ty => unimplemented!("no support for `{}` yet", token_ty),
    }
}

fn assert_lexer(input: &Path, expected: &Path) {
    let source = fs::read_to_string(input).unwrap();
    let expected = fs::read_to_string(expected).unwrap();
    let mut expected = expected.lines().skip(1);

    let lexer = lexer::Lexer::new(&source);
    let mut lexer = lexer.map(|r| r.map(|(_, t, _)| t).map_err(|_| ()));

    for (actual, line) in lexer.by_ref().zip(expected.by_ref()) {
        println!("---");
        println!("       line = {}", line);
        let expected = read_token(line);
        println!("parsed line = {:?}", expected);
        println!("     actual = {:?}", actual);
        assert_eq!(actual, expected, "actual == expected");
    }

    assert_eq!(lexer.count(), 0);
    assert_eq!(expected.count(), 0);
}

include!(concat!(env!("OUT_DIR"), "/lexer_tests.rs"));
