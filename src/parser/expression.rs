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
        let Some(kind) = self.peek_kind() else {
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
                        let ty = self.ty()?;
                        let value = self.arg()?;

                        if let Arg::Id(id) = value {
                            let (id, ty) = if let Some(var) = slt.get_variable(id) {
                                (id, var.ty)
                            } else {
                                slt.add_variable((id, ty), self.span);
                                (id, ty)
                            };
                            returns.push((id, ty));
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
            T![OEq] => {
                self.consume(T![OEq])?;
                let lhs = self.arg()?;
                let rhs = self.binop()?;
                self.consume(T![CEq])?;

                let rhs = self.arena.objdup(&rhs);
                let eq = crate::ir::Binop::Eq { lhs, rhs };

                Some(Expr::Binop(eq))
            }
            kind => {
                error!("unknown start of expression: `{kind}`");
                self.err_cpt += 1;
                None
            }
        }
    }
}
