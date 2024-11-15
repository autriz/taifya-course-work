#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // <буква>{<буква>|<цифра>}
    Ident(String), 
    // radix 10: {/ <цифра> /}[D|d]
    // radix 2: {/ 0|1 /}(B|b)
    // radix 8: {/ 0|1|2|3|4|5|6|7 /}(O|o)
    // radix 16: {/ <цифра>|A|B|C|D|E|F|a|b|c|d|e|f /}(H|h)
    Int(i64),
    // <числовая_строка><порядок>|[<числовая_строка>].<числовая_строка>[<порядок>],
    // где <порядок>::= (E|e)[+|-]<числовая_строка>
    Float(f64), 
    // этого нету в задании, но пусть будет :)
    // <строка>::= '"'{ <буква> }'"'
    String(String),
    // (* multiline comment *)
    Comment,

    // Операции группы отношения
    Equal, // ==
    NotEqual, // !=
    LessThan, // <
    LessThanOrEqual, // <=
    GreaterThan, // >
    GreaterThanOrEqual, // >=

    // Операции группы сложения
    Plus, // +
    Minus, // -
    Or, // ||

    // Операции группы умножения
    Asterisk, // *
    Slash, // /
    And, // &&

    // Унарная операция (а так же тип данных)
    Bang, // !

    // Операция присваивания
    Assign, // :=

    // Типы данных
    Percent, // %
    Dollar, // $

    // Ключевые слова
    Var, // var
    Begin, // begin
    End, // end
    If, // if
    Else, // else
    For, // for
    To, // to
    Step, // step
    While, // while
    Next, // next

    // Ввод и вывод
    Readln, // readln
    Writeln, // writeln

    // Прочие разделители
    Comma, // ,
    Colon, // :
    Semicolon, // ;
    LParen, // (
    RParen, // )

    // Логические константы
    True,
    False,

    Eof
}

impl Token {
    pub fn is_reserved_word(&self) -> bool {
        match self {
            Token::Var
            | Token::Begin
            | Token::End
            | Token::If
            | Token::Else
            | Token::For
            | Token::To
            | Token::Step
            | Token::While
            | Token::Next
            | Token::True
            | Token::False
            | Token::Readln
            | Token::Writeln => true,
            _ => false
        }
    }

    pub fn is_variable_type(&self) -> bool {
        match self {
            Token::Percent
            | Token::Dollar
            | Token::Bang => true,
            _ => false,
        }
    }

    pub fn is_ending(&self) -> bool {
        match self {
            Token::End
            | Token::Semicolon
            | Token::Next => true,
            _ => false
        }
    }

    pub fn is_operator(&self) -> bool {
        match self {
            Token::Plus
            | Token::Minus
            | Token::Or
            | Token::Asterisk
            | Token::Slash
            | Token::And
            | Token::Assign
            | Token::Bang
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual => true,
            _ => false,
        }
    }

    pub fn is_delimeter(&self) -> bool {
        match self {
            Token::LParen
            | Token::RParen
            | Token::Comma
            | Token::Semicolon
            | Token::Colon => true,
            _ => self.is_operator()
        }
    }

    pub fn as_literal(&self) -> String {
        match self {
            Token::Ident(value) => format!("{}", value),
            Token::Int(value) => format!("{}", value),
            Token::Float(value) => format!("{}", value),
            Token::String(value) => format!("\"{}\"", value),
            Token::Comment => "Comment".to_string(),
            Token::Equal => "==".to_string(),
            Token::NotEqual => "!=".to_string(),
            Token::LessThan => "<".to_string(),
            Token::LessThanOrEqual => "<=".to_string(),
            Token::GreaterThan => ">".to_string(),
            Token::GreaterThanOrEqual => ">=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Or => "||".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::And => "&&".to_string(),
            Token::Bang => "!".to_string(),
            Token::Assign => ":=".to_string(),
            Token::Percent => "%".to_string(),
            Token::Dollar => "$".to_string(),
            Token::Var => "var".to_string(),
            Token::Begin => "begin".to_string(),
            Token::End => "end".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::For => "for".to_string(),
            Token::To => "to".to_string(),
            Token::Step => "step".to_string(),
            Token::While => "while".to_string(),
            Token::Next => "next".to_string(),
            Token::Readln => "readln".to_string(),
            Token::Writeln => "writeln".to_string(),
            Token::Comma => ",".to_string(),
            Token::Colon => ":".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::Eof => "EOF".to_string()
        }.to_string()
    }
}