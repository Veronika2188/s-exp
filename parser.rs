#[derive(Debug, PartialEq)]
enum SExp {
    Atom(String),
    SList(Vec<SExp>),
}

impl SExp {
    fn to_string(&self) -> String {
        match self {
            SExp::Atom(v) => v.clone(),
            SExp::SList(l) => {
                let mut result = String::from("(");
                for sexp in l {
                    result.push_str(&sexp.to_string());
                }
                result.push_str(")");
                result
            }
        }
    }
}

fn tokenize(input: &str) -> Vec<String> {
    let input = input.replace("(", " ( ").replace(")", " ) ");
    input.split_whitespace().map(String::from).collect()
}

fn parse_tokens(tokens: &mut Vec<String>) -> Result<SExp, String> {
    if tokens.is_empty() {
        return Err("Unexpected end of input".to_string());
    }

    let token = tokens.remove(0);
    match token.as_str() {
        "(" => {
            let mut children = Vec::new();
            while !tokens.is_empty() && tokens[0] != ")" {
                children.push(parse_tokens(tokens)?);
            }
            if tokens.is_empty() {
                return Err("Unmatched (".to_string());
            }
            tokens.remove(0); // Remove ")"
            Ok(SExp::SList(children))
        }
        ")" => Err("Unmatched )".to_string()),
        _ => Ok(SExp::Atom(token)),
    }
}

fn parse(input: &str) -> Result<SExp, String> {
    let mut tokens = tokenize(input);
    let result = parse_tokens(&mut tokens)?;
    if !tokens.is_empty() {
        return Err("Invalid S-expression".to_string());
    }
    Ok(result)
}

fn main() {
    let input = "(+ 1 (* 2 3))";
    match parse(input) {
        Ok(result) => println!("{}", result.to_string()),
        Err(e) => println!("Error: {}", e),
    }
}
