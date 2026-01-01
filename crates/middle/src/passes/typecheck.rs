use std::collections::HashMap;

use dsl_front::{
    ast::{BinaryOp, Block, BlockItem, Expr, ExprId, ExprKind, Function, Item, Program, TypeExpr},
    span::Span,
};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    Bool,
    Unit,
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug, Default, Clone)]
pub struct TypeInfo {
    expr_types: HashMap<ExprId, Type>,
}

impl TypeInfo {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_expr(&mut self, expr_id: ExprId, ty: Type) {
        self.expr_types.insert(expr_id, ty);
    }

    pub fn get(&self, expr_id: ExprId) -> Option<&Type> {
        self.expr_types.get(&expr_id)
    }

    pub fn expr_type(&self, expr: &Expr) -> Option<&Type> {
        self.get(expr.id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ExprId, &Type)> {
        self.expr_types.iter()
    }
}

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("unknown identifier `{name}` at {span}")]
    UnknownVariable { name: String, span: Span },
    #[error("type mismatch at {span}: expected {expected:?}, found {found:?}")]
    Mismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
    #[error("invalid operands for {op:?} at {span}: left {left:?}, right {right:?}")]
    InvalidBinary {
        op: BinaryOp,
        left: Type,
        right: Type,
        span: Span,
    },
    #[error("called expression is not a function at {span}")]
    NotCallable { span: Span },
    #[error("arity mismatch at {span}: expected {expected}, found {found}")]
    ArityMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },
}

pub fn typecheck(program: &Program) -> Result<TypeInfo, TypeError> {
    let mut functions = HashMap::new();
    for item in &program.items {
        match item {
            Item::Function(func) => {
                let params = func.params.iter().map(|p| type_from_ast(&p.ty)).collect();
                let ret = type_from_ast(&func.ret_type);
                functions.insert(func.name.clone(), FunctionSig { params, ret });
            }
        }
    }

    let mut types = TypeInfo::new();
    for item in &program.items {
        match item {
            Item::Function(func) => {
                typecheck_function(func, &functions, &mut types)?;
            }
        }
    }

    Ok(types)
}

fn typecheck_function(
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    types: &mut TypeInfo,
) -> Result<(), TypeError> {
    let mut env = Vec::new();
    for param in &func.params {
        env.push((param.name.clone(), type_from_ast(&param.ty)));
    }
    let ty = type_expr(&func.body, &mut env, functions, types)?;
    let expected = type_from_ast(&func.ret_type);
    if ty != expected {
        return Err(TypeError::Mismatch {
            expected,
            found: ty,
            span: func.body.span,
        });
    }
    Ok(())
}

fn type_expr(
    expr: &Expr,
    env: &mut Vec<(String, Type)>,
    functions: &HashMap<String, FunctionSig>,
    types: &mut TypeInfo,
) -> Result<Type, TypeError> {
    let ty = match &expr.kind {
        ExprKind::Int(_) => Ok(Type::I32),
        ExprKind::Bool(_) => Ok(Type::Bool),
        ExprKind::Var(name) => {
            lookup(env, functions, name).ok_or_else(|| TypeError::UnknownVariable {
                name: name.clone(),
                span: expr.span,
            })
        }
        ExprKind::Binary { op, left, right } => {
            type_binary(expr.span, *op, left, right, env, functions, types)
        }
        ExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = type_expr(cond, env, functions, types)?;
            if cond_ty != Type::Bool {
                return Err(TypeError::Mismatch {
                    expected: Type::Bool,
                    found: cond_ty,
                    span: cond.span,
                });
            }
            let then_ty = type_expr(then_branch, env, functions, types)?;
            let else_ty = type_expr(else_branch, env, functions, types)?;
            if then_ty != else_ty {
                return Err(TypeError::Mismatch {
                    expected: then_ty,
                    found: else_ty,
                    span: else_branch.span,
                });
            }
            Ok(then_ty)
        }
        ExprKind::Block(block) => type_block(block, env, functions, types),
        ExprKind::Call { callee, args } => {
            let callee_ty = type_expr(callee, env, functions, types)?;
            match callee_ty {
                Type::Function(params, ret) => {
                    if params.len() != args.len() {
                        return Err(TypeError::ArityMismatch {
                            expected: params.len(),
                            found: args.len(),
                            span: expr.span,
                        });
                    }
                    for (arg, expected) in args.iter().zip(params.iter()) {
                        let arg_ty = type_expr(arg, env, functions, types)?;
                        if &arg_ty != expected {
                            return Err(TypeError::Mismatch {
                                expected: expected.clone(),
                                found: arg_ty,
                                span: arg.span,
                            });
                        }
                    }
                    Ok(*ret)
                }
                _ => Err(TypeError::NotCallable { span: callee.span }),
            }
        }
    }?;
    types.insert_expr(expr.id, ty.clone());
    Ok(ty)
}

fn type_block(
    block: &Block,
    env: &mut Vec<(String, Type)>,
    functions: &HashMap<String, FunctionSig>,
    types: &mut TypeInfo,
) -> Result<Type, TypeError> {
    let base_len = env.len();
    let mut last_ty = Type::Unit;
    for item in &block.items {
        match item {
            BlockItem::Let(stmt) => {
                let value_ty = type_expr(&stmt.value, env, functions, types)?;
                if let Some(annotation) = &stmt.ty {
                    let annotated = type_from_ast(annotation);
                    if annotated != value_ty {
                        return Err(TypeError::Mismatch {
                            expected: annotated,
                            found: value_ty,
                            span: stmt.span,
                        });
                    }
                }
                env.push((stmt.name.clone(), value_ty));
                last_ty = Type::Unit;
            }
            BlockItem::Expr(expr) => {
                last_ty = type_expr(expr, env, functions, types)?;
            }
        }
    }
    env.truncate(base_len);
    Ok(last_ty)
}

fn type_binary(
    span: Span,
    op: BinaryOp,
    left: &Expr,
    right: &Expr,
    env: &mut Vec<(String, Type)>,
    functions: &HashMap<String, FunctionSig>,
    types: &mut TypeInfo,
) -> Result<Type, TypeError> {
    let left_ty = type_expr(left, env, functions, types)?;
    let right_ty = type_expr(right, env, functions, types)?;
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            if left_ty == Type::I32 && right_ty == Type::I32 {
                Ok(Type::I32)
            } else {
                Err(TypeError::InvalidBinary {
                    op,
                    left: left_ty,
                    right: right_ty,
                    span,
                })
            }
        }
        BinaryOp::Eq => {
            if left_ty == right_ty {
                Ok(Type::Bool)
            } else {
                Err(TypeError::InvalidBinary {
                    op,
                    left: left_ty,
                    right: right_ty,
                    span,
                })
            }
        }
        BinaryOp::Lt | BinaryOp::Gt => {
            if left_ty == Type::I32 && right_ty == Type::I32 {
                Ok(Type::Bool)
            } else {
                Err(TypeError::InvalidBinary {
                    op,
                    left: left_ty,
                    right: right_ty,
                    span,
                })
            }
        }
    }
}

fn lookup(
    env: &[(String, Type)],
    functions: &HashMap<String, FunctionSig>,
    name: &str,
) -> Option<Type> {
    for (n, ty) in env.iter().rev() {
        if n == name {
            return Some(ty.clone());
        }
    }
    functions
        .get(name)
        .map(|sig| Type::Function(sig.params.clone(), Box::new(sig.ret.clone())))
}

pub fn type_from_ast(expr: &TypeExpr) -> Type {
    match expr {
        TypeExpr::I32 => Type::I32,
        TypeExpr::Bool => Type::Bool,
        TypeExpr::Unit => Type::Unit,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub params: Vec<Type>,
    pub ret: Type,
}
