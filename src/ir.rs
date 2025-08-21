//! Intermediate Representation (IR) of the HSL language
use core::fmt;

pub struct Program<'prog> {
    pub func: Vec<Fn<'prog>>,
    pub extrn: Vec<Extrn<'prog>>,
}

pub struct Extrn<'prog> {
    pub id: &'prog str,

    // Tell if the function has a variadic parameter and if so the value of variadic is
    // the number of fixed parameters
    pub variadic: Option<usize>,
    pub args: Vec<Type<'prog>>,
}

pub struct Fn<'prog> {
    pub id: &'prog str,
    pub body: Vec<Expr<'prog>>,

    // Tell if the function has a variadic parameter and if so the value of variadic is
    // the number of fixed parameters
    pub variadic: Option<usize>,
    pub args: Vec<(&'prog str, Type<'prog>)>,
    pub returns: Vec<Arg<'prog>>,
}

pub enum Binop {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'prog> {
    Ptr(&'prog Type<'prog>),
    Int,
    Str,
    Bool,
    Void,
}

pub enum Expr<'prog> {
    Let {
        id: &'prog str,
        value: Arg<'prog>,
    },
    FnCall {
        id: &'prog str,
        args: Vec<Arg<'prog>>,
        returns: Vec<(&'prog str, Type<'prog>)>,
    },
    BinOp {
        binop: Binop,
        lhs: Arg<'prog>,
        rhs: Arg<'prog>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Arg<'prog> {
    Deref(&'prog str),
    Ref(&'prog str),
    Id(&'prog str),
    Lit(Lit<'prog>),
}

#[derive(Debug, Clone, Copy)]
pub enum Lit<'prog> {
    Int(i64),
    Str(&'prog str),
    Bool(bool),
}

impl<'prog> Program<'prog> {
    pub fn new() -> Self {
        Self {
            func: Vec::new(),
            extrn: Vec::new(),
        }
    }

    pub fn get_fn_variadic(&self, id: &'prog str) -> Option<usize> {
        if let Some(func) = self.func.iter().find(|f| f.id == id) {
            func.variadic
        } else if let Some(func) = self.extrn.iter().find(|f| f.id == id) {
            func.variadic
        } else {
            None
        }
    }
}

impl<'prog> fmt::Display for Type<'prog> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ptr(inner) => write!(f, "pointer({inner})"),
            Self::Int => write!(f, "Credit"),
            Self::Str => write!(f, "Holotext"),
            Self::Bool => write!(f, "Signal"),
            Self::Void => write!(f, "void"),
        }
    }
}
