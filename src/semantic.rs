use crate::ir::{Arg, Expr, Fn, Lit, Program, Type};
use crate::parser::slt::NavigableSlt;

pub fn validate(program: &Program<'_>, slt: &NavigableSlt<'_, '_>) -> usize {
    let childs = slt.childs();

    let fn_err = program
        .func
        .iter()
        .zip(childs)
        .fold(0, |acc, (f, slt)| acc + validate_fn(f, &slt));

    fn_err
}

fn validate_fn(func: &Fn<'_>, slt: &NavigableSlt<'_, '_>) -> usize {
    func.body
        .iter()
        .fold(0, |acc, expr| acc + validate_expr(expr, slt))
}

fn validate_expr(expr: &Expr<'_>, slt: &NavigableSlt<'_, '_>) -> usize {
    let mut err_cpt = 0;

    match expr {
        Expr::FnCall { id, args, returns } => {
            let Some(called_func) = slt.find_func(id) else {
                error!("cannot find function {id} in this scope");
                err_cpt += 1;
                return err_cpt;
            };

            let min_args_number = if let Some(variadic) = called_func.variadic {
                variadic
            } else {
                called_func.args.len()
            };

            if min_args_number > args.len() {
                error!("not enough arguments passed to call function {id}");
                err_cpt += 1;
                return err_cpt;
            }

            for (i, arg) in args.iter().enumerate().take(min_args_number) {
                if !check_arg_ty(arg, called_func.args[i], slt) {
                    error!("type mismatch for function {id} argument number {i}");
                    err_cpt += 1;
                }
            }

            if returns.len() > called_func.returns.len() {
                error!("try to collect more returned value than the function {id} returns");
                err_cpt += 1;
                return err_cpt;
            }

            for (i, (rid, ty)) in returns.iter().enumerate() {
                if !check_arg_ty(&called_func.returns[i], *ty, slt) {
                    error!("type mismatch for return argument {rid} in function {id}");
                    err_cpt += 1;
                }
            }
        }
        Expr::IfThenElse { cond, then, els } => {
            if !check_arg_ty(cond, Type::Int, slt) && !check_arg_ty(cond, Type::Bool, slt) {
                error!("type mismatch for if condition, this must be an integer or a boolean");
                err_cpt += 1;
            }
            let mut childs = slt.childs();
            let then_slt = childs.next().unwrap();
            err_cpt += then
                .iter()
                .fold(0, |acc, expr| acc + validate_expr(expr, &then_slt));

            if let Some(els) = els {
                let else_slt = slt.childs().next().unwrap();
                err_cpt += els
                    .iter()
                    .fold(0, |acc, expr| acc + validate_expr(expr, &else_slt));
            }
        }
        _ => (),
    }

    err_cpt
}

fn check_arg_ty<'prog>(
    arg: &Arg<'prog>,
    check: Type<'prog>,
    slt: &NavigableSlt<'_, 'prog>,
) -> bool {
    match arg {
        Arg::Lit(Lit::Int(_)) => check == Type::Int,
        Arg::Lit(Lit::Str(_)) => check == Type::Str,
        Arg::Lit(Lit::Bool(_)) => check == Type::Bool,
        Arg::Id(id) => {
            let Some(var) = slt.find_variable(id) else {
                return false;
            };

            var.ty == check
        }
        Arg::Deref(id) => {
            let Some(var) = slt.find_variable(id) else {
                return false;
            };

            match var.ty {
                Type::Ptr(&ty) => ty == check,
                _ => false,
            }
        }
        Arg::Ref(id) => {
            let Some(var) = slt.find_variable(id) else {
                return false;
            };

            match check {
                Type::Ptr(&ty) => var.ty == ty,
                _ => false,
            }
        }
    }
}
