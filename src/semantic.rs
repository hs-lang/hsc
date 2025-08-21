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
    let mut err_cpt = 0;

    for expr in &func.body {
        #[allow(clippy::single_match)]
        match expr {
            Expr::FnCall { id, args, returns } => {
                let Some(called_func) = slt.find_func(id) else {
                    error!("cannot find function {id} in this scope");
                    err_cpt += 1;
                    continue;
                };

                let min_args_number = if let Some(variadic) = called_func.variadic {
                    variadic
                } else {
                    called_func.args.len()
                };

                if min_args_number > args.len() {
                    error!("not enough arguments passed to call function {id}");
                    err_cpt += 1;
                    continue;
                }

                for i in 0..min_args_number {
                    if !check_arg_ty(&args[i], called_func.args[i], slt) {
                        error!("type mismatch for function {id} argument number {i}");
                        err_cpt += 1;
                    }
                }

                if returns.len() > called_func.returns.len() {
                    error!("try to collect more returned value than the function {id} returns");
                    err_cpt += 1;
                    continue;
                }

                for (i, (rid, ty)) in returns.iter().enumerate() {
                    if !check_arg_ty(&called_func.returns[i], *ty, slt) {
                        error!("type mismatch for return argument {rid} in function {id}");
                        err_cpt += 1;
                    }
                }
            }
            _ => (),
        }
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
