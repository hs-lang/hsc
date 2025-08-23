use crate::ir::Type;
use crate::lexer::token::Token;

use super::Parser;

impl<'input, 'prog, I> Parser<'input, 'prog, I>
where
    I: Iterator<Item = Token>,
{
    pub fn ty(&mut self) -> Option<Type<'prog>> {
        let Some(kind) = self.next() else {
            error!("expected a type but got nothing");
            self.err_cpt += 1;
            return None;
        };

        let ty = match kind.kind {
            T![TyPtr] => {
                self.consume(T![OGen])?;
                let ty = self.ty()?;
                self.consume(T![CGen])?;

                let ty = self.arena.objdup(&ty);
                Some(Type::Ptr(ty))
            }
            T![TyInt] => Some(Type::Int),
            T![TyStr] => Some(Type::Str),
            T![TyBool] => Some(Type::Bool),
            _ => None,
        };

        ty
    }
}
