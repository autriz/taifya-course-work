#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // <буква>{<буква>|<цифра>}
    Ident(String), 
    // {/ <цифра> /}[D|d]
    Int(String), 
    // {/ 0|1 /}(B|b)
    Binary(String), 
    // {/ 0|1|2|3|4|5|6|7 /}(O|o)
    Octal(String), 
    // {/ <цифра>|A|B|C|D|E|F|a|b|c|d|e|f /}(H|h)
    Hexadecimal(String), 
    // <числовая_строка><порядок>|[<числовая_строка>].<числовая_строка>[<порядок>],
    // где <порядок>::= (E|e)[+|-]<числовая_строка>
    Float(String), 
    // этого нету в задании, но пусть будет :)
    // <строка>::= '"'{/ <буква> /}'"'
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

    // Унарная операция
    Bang, // !

    Assign, // :=
    Percent, // %
    At, // @
    Dollar, // $

    Var, // var
    Begin, // begin
    End, // end
    If, // if
    Else, // else
    For, // for
    To, // to
    Do, // do
    While, // while
    Next, // next

    // Ввод и вывод
    Readln, // readln
    Writeln, // writeln

    Colon, // :
    Semicolon, // ;
    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    True,
    False,

    Eof
}