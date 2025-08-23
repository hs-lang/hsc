#![allow(clippy::upper_case_acronyms)]

use std::fmt;

#[macro_export]
macro_rules! T {
    [EOF] => { $crate::lexer::token::TokenKind::EOF };
    [ParseError] => { $crate::lexer::token::TokenKind::ParseError };
    [ID] => { $crate::lexer::token::TokenKind::ID };
    [String] => { $crate::lexer::token::TokenKind::String };
    [CharLit] => { $crate::lexer::token::TokenKind::CharLit };
    [IntLit] => { $crate::lexer::token::TokenKind::IntLit };
    [True] => { $crate::lexer::token::TokenKind::True };
    [False] => { $crate::lexer::token::TokenKind::False };
    [Not] => { $crate::lexer::token::TokenKind::Not };
    [OEq] => { $crate::lexer::token::TokenKind::OEq };
    [CEq] => { $crate::lexer::token::TokenKind::CEq };
    [OAdd] => { $crate::lexer::token::TokenKind::OAdd };
    [CAdd] => { $crate::lexer::token::TokenKind::CAdd };
    [OSub] => { $crate::lexer::token::TokenKind::OSub };
    [CSub] => { $crate::lexer::token::TokenKind::CSub };
    [OMul] => { $crate::lexer::token::TokenKind::OMul };
    [CMul] => { $crate::lexer::token::TokenKind::CMul };
    [ODiv] => { $crate::lexer::token::TokenKind::ODiv };
    [CDiv] => { $crate::lexer::token::TokenKind::CDiv };
    [OMod] => { $crate::lexer::token::TokenKind::OMod };
    [CMod] => { $crate::lexer::token::TokenKind::CMod };
    [OGen] => { $crate::lexer::token::TokenKind::OGen };
    [CGen] => { $crate::lexer::token::TokenKind::CGen };
    [Beacon] => { $crate::lexer::token::TokenKind::Beacon };
    [Retrieve] => { $crate::lexer::token::TokenKind::Retrieve };
    [TyPtr] => { $crate::lexer::token::TokenKind::TyPtr };
    [TyInt] => { $crate::lexer::token::TokenKind::TyInt };
    [TyStr] => { $crate::lexer::token::TokenKind::TyStr };
    [TyBool] => { $crate::lexer::token::TokenKind::TyBool };
    [OAssign] => { $crate::lexer::token::TokenKind::OAssign };
    [Assign] => { $crate::lexer::token::TokenKind::Assign };
    [CAssign] => { $crate::lexer::token::TokenKind::CAssign };
    [If] => { $crate::lexer::token::TokenKind::If };
    [IfEnd] => { $crate::lexer::token::TokenKind::IfEnd};
    [Else] => { $crate::lexer::token::TokenKind::Else };
    [Let] => { $crate::lexer::token::TokenKind::Let };
    [OFnDecl1] => { $crate::lexer::token::TokenKind::OFnDecl1 };
    [OFnDecl2] => { $crate::lexer::token::TokenKind::OFnDecl2 };
    [CFnDecl] => { $crate::lexer::token::TokenKind::CFnDecl };
    [OFnParams] => { $crate::lexer::token::TokenKind::OFnParams };
    [CFnParams] => { $crate::lexer::token::TokenKind::CFnParams };
    [OFnCallReturn] => { $crate::lexer::token::TokenKind::OFnCallReturn };
    [OFnReturn] => { $crate::lexer::token::TokenKind::OFnReturn };
    [CFnReturn] => { $crate::lexer::token::TokenKind::CFnReturn };
    [OFnCall] => { $crate::lexer::token::TokenKind::OFnCall };
    [CFnCall] => { $crate::lexer::token::TokenKind::CFnCall };
    [Variadic] => { $crate::lexer::token::TokenKind::Variadic };
    [OExtrnFn] => { $crate::lexer::token::TokenKind::OExtrnFn};
    [CExtrnFn] => { $crate::lexer::token::TokenKind::CExtrnFn};
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Self { kind, span }
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} - <{}, {}>",
            self.kind, self.span.start, self.span.end
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenKind {
    // Terminal
    EOF,
    ParseError,

    // Values
    ID,
    String,
    CharLit,
    IntLit,
    True,
    False,

    // Puncts
    Not,
    OEq,
    CEq,
    OAdd,
    CAdd,
    OSub,
    CSub,
    OMul,
    CMul,
    ODiv,
    CDiv,
    OMod,
    CMod,
    OGen,
    CGen,
    Beacon,
    Retrieve,

    // Types
    TyPtr,
    TyInt,
    TyStr,
    TyBool,

    // Keywords
    OAssign,
    Assign,
    CAssign,
    If,
    IfEnd,
    Else,
    Let,
    OFnDecl1,
    OFnDecl2,
    CFnDecl,
    OFnParams,
    CFnParams,
    OFnCallReturn,
    OFnReturn,
    CFnReturn,
    OFnCall,
    CFnCall,
    Variadic,
    OExtrnFn,
    CExtrnFn,
}

// TODO: rework this function
impl fmt::Display for TokenKind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{}",
            match self {
                T![EOF] => "<eof>",
                T![ParseError] => "<?>",
                T![ID] => "Identifier",
                T![String] => "String",
                T![CharLit] => "Char literal",
                T![IntLit] => "Integer literal",
                T![True] => "True",
                T![False] => "False",
                T![Not] => "Not",
                T![OEq] => "Open Eq",
                T![CEq] => "Close Eq",
                T![OAdd] => "Open  Add",
                T![CAdd] => "Close Add",
                T![OSub] => "Open  Sub",
                T![CSub] => "Close Sub",
                T![OMul] => "Open  Mul",
                T![CMul] => "Close Mul",
                T![ODiv] => "Open  Div",
                T![CDiv] => "Close Div",
                T![OMod] => "Open  Mod",
                T![CMod] => "Close Mod",
                T![OGen] => "<",
                T![CGen] => ">",
                T![Beacon] => "Beacon",
                T![Retrieve] => "Retrieve",
                T![TyPtr] => "Type ptr",
                T![TyInt] => "Type int",
                T![TyStr] => "Type string",
                T![TyBool] => "Type bool",
                T![OAssign] => "Open assign",
                T![Assign] => "Assign",
                T![CAssign] => "Close assign",
                T![If] => "If",
                T![IfEnd] => "IfEnd",
                T![Else] => "Else",
                T![Let] => "Let",
                T![OFnDecl1] => "Opening function declaration 1",
                T![OFnDecl2] => "Opening function declaration 2",
                T![CFnDecl] => "Closing function declaration 1",
                T![OFnCall] => "Opening function call",
                T![CFnCall] => "Closing function call",
                T![OFnParams] => "Opening function parameters",
                T![CFnParams] => "Closing function parameters",
                T![OFnReturn] => "Opening function return",
                T![OFnCallReturn] => "Opening function call return",
                T![CFnReturn] => "Closing function return",
                T![Variadic] => "Variadic declaration",
                T![OExtrnFn] => "Open extrn function declaration",
                T![CExtrnFn] => "Clone extrn function declaration",
            }
        )
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Default, Debug)]
pub struct Span {
    // inclusive
    pub start: usize,
    // exclusive
    pub end: usize,

    pub line: usize,
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<(std::ops::Range<usize>, usize)> for Span {
    fn from(range: (std::ops::Range<usize>, usize)) -> Span {
        Span {
            start: range.0.start,
            end: range.0.end,
            line: range.1,
        }
    }
}

impl std::ops::Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[std::ops::Range::<usize>::from(index)]
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    #[test]
    fn token_kind_display() {
        assert_eq!(T![String].to_string(), "String");
        assert_eq!(T![EOF].to_string(), "<eof>");
    }
}
