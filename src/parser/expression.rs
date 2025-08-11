use crate::ir::{Arg, Expr, Lit, Type};
use crate::lexer::token::Token;
use crate::parser::Parser;

use super::slt::{Builder, SymbolLookupTable};

impl<'input, 'prog, I> Parser<'input, 'prog, I>
where
    I: Iterator<Item = Token>,
{
    pub fn expression(
        &mut self,
        _slt_builder: &mut Builder,
        slt: &mut SymbolLookupTable<'prog>,
    ) -> Option<Expr<'prog>> {
        let Some(kind) = self.peek() else {
            error!("Expected a statement and found nothing");
            self.err_cpt += 1;
            return None;
        };

        match kind {
            T![Let] => {
                self.consume(T![Let])?;
                let Some(ident) = self.next() else {
                    error!("expected identifier after `let` but found nothing");
                    self.err_cpt += 1;
                    return None;
                };

                if ident.kind != T![ID] {
                    error!(
                        "expected identifier after `let` but found {} instead",
                        ident.kind
                    );
                    self.err_cpt += 1;
                    return None;
                }

                let id = self.arena.strdup(self.text(ident));
                self.consume(T![Assign])?;
                let value = self.arg()?;

                let res = match value {
                    Arg::Lit(Lit::Str(s)) => slt.add_variable((id, Type::Str, s), ident.span),
                    Arg::Lit(Lit::Int(i)) => slt.add_variable((id, Type::Int, i), ident.span),
                    Arg::Lit(Lit::Bool(b)) => slt.add_variable((id, Type::Bool, b), ident.span),
                    _ => {
                        error!("invalid argument found");
                        self.err_cpt += 1;
                        return None;
                    }
                };

                if let Some((_, loc)) = res {
                    self.err_cpt += 1;
                    error!(
                        "variable {id} already declared, previous declaration happened on line {}",
                        loc.line
                    );
                }

                Some(Expr::Let { id, value })
            }
            T![OFnCall] => {
                self.consume(T![OFnCall])?;
                let Some(ident) = self.next() else {
                    error!("expected identifier after `fn_call` but found nothing");
                    self.err_cpt += 1;
                    return None;
                };

                if ident.kind != T![ID] {
                    error!(
                        "expected identifier after `fn_call` but found {} instead",
                        ident.kind
                    );
                    self.err_cpt += 1;
                    return None;
                }
                let id = self.arena.strdup(self.text(ident));

                let mut args = Vec::new();
                if self.check_next(T![OFnParams]) {
                    self.consume(T![OFnParams])?;

                    while !self.check_next(T![CFnParams]) {
                        args.push(self.arg()?);
                    }

                    self.consume(T![CFnParams])?;
                }

                let mut returns = Vec::new();
                if self.check_next(T![OFnCallReturn]) {
                    self.consume(T![OFnCallReturn])?;

                    while !self.check_next(T![CFnReturn]) {
                        let Some(kind) = self.peek() else {
                            error!("expected type token in function call returns");
                            self.err_cpt += 1;
                            return None;
                        };

                        let ty = match kind {
                            T![TyInt] => Type::Int,
                            T![TyString] => Type::Str,
                            T![TyBool] => Type::Bool,
                            _ => {
                                error!("unexpected token for type");
                                self.err_cpt += 1;
                                return None;
                            }
                        };
                        self.consume(kind)?;
                        let value = self.arg()?;

                        if let Arg::Id(id) = value {
                            if let Some(var) = slt.get_variable(id) {
                                returns.push((id, var.ty));
                            } else {
                                slt.add_variable((id, ty), self.span);
                            }
                        } else {
                            error!("invalid argument found");
                            self.err_cpt += 1;
                            return None;
                        }
                    }

                    self.consume(T![CFnReturn])?;
                }
                self.consume(T![CFnCall])?;

                Some(Expr::FnCall { id, args, returns })
            }
            //T![OAssign] => {
            //    self.consume(T![OAssign])?;
            //    let Some(ident) = self.next() else {
            //        error!("expected identifier after `assign` but found nothing");
            //        self.err_cpt += 1;
            //        return None;
            //    };

            //    if ident.kind != T![ID] {
            //        error!(
            //            "expected identifier after `assign` but found {} instead",
            //            ident.kind
            //        );
            //        self.err_cpt += 1;
            //        return None;
            //    }

            //    let id = self.arena.strdup(self.text(ident));

            //    let mut ops = Vec::new();
            //    while !self.check_next(T![CAssign]) {
            //        ops.push(self.unary_op()?);
            //    }

            //    self.consume(T![CAssign])?;
            //    Some(Stmt::Assign { id, ops })
            //}
            //T![if] => {
            //    self.consume(T![if]);

            //    let condition = Box::new(self.expression());
            //    let mut body = Vec::new();

            //    while !self.at(T![else]) && !self.at(T![if_end]) {
            //        body.push(self.statement());
            //    }

            //    let else_stmt = if self.at(T![else]) {
            //        self.consume(T![else]);
            //        let mut else_body = Vec::new();

            //        while !self.at(T![if_end]) {
            //            else_body.push(self.statement());
            //        }

            //        Some(else_body)
            //    } else {
            //        None
            //    };

            //    self.consume(T![if_end]);
            //    ast::Stmt::IfStmt {
            //        condition,
            //        body,
            //        else_stmt,
            //    }
            //}
            kind => {
                error!("unknown start of expression: `{kind}`");
                self.err_cpt += 1;
                None
            }
        }
    }

    //fn unary_op(&mut self) -> Option<Unop<'prog>> {
    //    let Some(kind) = self.peek() else {
    //        error!("expected an unary operator and found nothing");
    //        self.err_cpt += 1;
    //        return None;
    //    };

    //    let unop = match kind {
    //        T![Plus] => Unop {
    //            op: crate::ir::Op::Add,
    //            value: self.expression()?,
    //        },
    //        T![Minus] => Unop {
    //            op: crate::ir::Op::Sub,
    //            value: self.expression()?,
    //        },
    //        T![Div] => Unop {
    //            op: crate::ir::Op::Div,
    //            value: self.expression()?,
    //        },
    //        T![Mul] => Unop {
    //            op: crate::ir::Op::Mul,
    //            value: self.expression()?,
    //        },
    //        T![Mod] => Unop {
    //            op: crate::ir::Op::Mod,
    //            value: self.expression()?,
    //        },
    //        kind => {
    //            error!("unknown start of unary operator: `{kind}`");
    //            self.err_cpt += 1;
    //            return None;
    //        }
    //    };

    //    Some(unop)
    //}
}
