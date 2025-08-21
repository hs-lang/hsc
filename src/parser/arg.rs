use crate::ir::Arg;
use crate::lexer::token::Token;
use crate::parser::Parser;

impl<'input, 'prog, I> Parser<'input, 'prog, I>
where
    I: Iterator<Item = Token>,
{
    pub fn arg(&mut self) -> Option<Arg<'prog>> {
        let Some(kind) = self.peek_kind() else {
            error!("Expected an expression and found nothing");
            self.err_cpt += 1;
            return None;
        };

        match kind {
            T![String] | T![Not] | T![IntLit] | T![True] | T![False] => {
                Some(Arg::Lit(self.literal()?))
            }
            T![ID] => {
                // Consumes the token and retrieve the id in the parser state
                self.consume(T![ID])?;
                Some(Arg::Id(self.arena.strdup(self.id)))
            }
            T![Beacon] => {
                self.consume(T![Beacon])?;
                self.consume(T![OGen])?;
                self.consume(T![ID])?;
                let arg = Arg::Ref(self.arena.strdup(self.id));
                self.consume(T![CGen])?;
                Some(arg)
            }
            T![Retrieve] => {
                self.consume(T![Retrieve])?;
                self.consume(T![OGen])?;
                self.consume(T![ID])?;
                let arg = Arg::Deref(self.arena.strdup(self.id));
                self.consume(T![CGen])?;
                Some(arg)
            }
            kind => {
                error!("unknown start of expression: `{kind}`");
                self.err_cpt += 1;
                None
            }
        }
    }
}
