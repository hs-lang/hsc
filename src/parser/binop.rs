use crate::ir::Binop;
use crate::lexer::token::Token;
use crate::parser::Parser;

impl<'input, 'prog, I> Parser<'input, 'prog, I>
where
    I: Iterator<Item = Token>,
{
    pub fn binop(&mut self) -> Option<Binop<'prog>> {
        let Some(kind) = self.peek_kind() else {
            error!("Expected a statement and found nothing");
            self.err_cpt += 1;
            return None;
        };

        match kind {
            T![String]
            | T![Not]
            | T![IntLit]
            | T![True]
            | T![False]
            | T![ID]
            | T![Beacon]
            | T![Retrieve] => {
                let arg = self.arg()?;
                Some(Binop::Arg(arg))
            }
            T![OAdd] | T![OSub] | T![OMul] | T![ODiv] | T![OMod] => {
                let closing = match kind {
                    T![OAdd] => T![CAdd],
                    T![OSub] => T![CSub],
                    T![OMul] => T![CMul],
                    T![ODiv] => T![CDiv],
                    T![OMod] => T![CMod],
                    // SAFETY: this is because of the first match
                    _ => unreachable!(),
                };
                self.consume(kind)?;
                let lhs = self.binop()?;
                let rhs = self.binop()?;
                self.consume(closing)?;

                let lhs = self.arena.objdup(&lhs);
                let rhs = self.arena.objdup(&rhs);

                let binop = match kind {
                    T![OAdd] => Binop::Add { lhs, rhs },
                    T![OSub] => Binop::Sub { lhs, rhs },
                    T![OMul] => Binop::Mul { lhs, rhs },
                    T![ODiv] => Binop::Div { lhs, rhs },
                    T![OMod] => Binop::Mod { lhs, rhs },
                    // SAFETY: this is because of the first match
                    _ => unreachable!(),
                };

                Some(binop)
            }

            kind => {
                error!("unknown start of binop: `{kind}`");
                self.err_cpt += 1;
                None
            }
        }
    }
}
