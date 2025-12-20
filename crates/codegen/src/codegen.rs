use std::collections::HashMap;

use dsl_front::{
    ast::{BinaryOp, Block, BlockItem, Expr, ExprKind, Function, Item},
    span::Span,
};
use dsl_middle::{FunctionSig, LowIrProgram, Type, type_from_ast};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("unknown identifier `{name}` at {span}")]
    UnknownVariable { name: String, span: Span },
    #[error("only boolean expressions may be used as conditions at {span}")]
    ExpectedBool { span: Span },
    #[error("invalid operands for {op:?} at {span}")]
    InvalidBinary { op: BinaryOp, span: Span },
    #[error("mismatched branch types in `if` expression at {span}")]
    BranchTypeMismatch { span: Span },
    #[error("expression is not callable at {span}")]
    NotCallable { span: Span },
    #[error("arity mismatch at {span}: expected {expected}, found {found}")]
    ArityMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },
}

pub fn emit_llvm(ir: &LowIrProgram) -> Result<String, CodegenError> {
    let program = ir.mir().hir().program();
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

    let mut out = String::new();
    for item in &program.items {
        match item {
            Item::Function(func) => {
                let sig = functions.get(&func.name).expect("function signature");
                let body = emit_function(func, sig, &functions)?;
                out.push_str(&body);
                out.push('\n');
            }
        }
    }

    Ok(out)
}

fn emit_function(
    func: &Function,
    sig: &FunctionSig,
    functions: &HashMap<String, FunctionSig>,
) -> Result<String, CodegenError> {
    let mut emitter = FunctionEmitter::new(functions);
    for (param, ty) in func.params.iter().zip(sig.params.iter()) {
        let value = ValueRef {
            repr: format!("%{}", param.name),
            ty: ty.clone(),
        };
        emitter.bind(param.name.clone(), value);
    }
    let result = emitter.emit_expr(&func.body)?;
    emitter.append_return(result, &sig.ret);

    let params = func
        .params
        .iter()
        .zip(sig.params.iter())
        .map(|(param, ty)| format!("{} %{}", llvm_type(ty), param.name))
        .collect::<Vec<_>>()
        .join(", ");
    let mut text = format!(
        "define {} @{}({}) {{\n",
        llvm_type(&sig.ret),
        func.name,
        params
    );

    for block in emitter.blocks {
        text.push_str(&format!("{}:\n", block.label));
        for inst in block.instructions {
            text.push_str("  ");
            text.push_str(&inst);
            text.push('\n');
        }
    }
    text.push_str("}\n");
    Ok(text)
}

struct FunctionEmitter<'a> {
    functions: &'a HashMap<String, FunctionSig>,
    blocks: Vec<BlockBuilder>,
    current: usize,
    tmp_counter: usize,
    block_counter: usize,
    env: Vec<Binding>,
}

struct BlockBuilder {
    label: String,
    instructions: Vec<String>,
}

#[derive(Clone)]
struct ValueRef {
    repr: String,
    ty: Type,
}

struct Binding {
    name: String,
    value: ValueRef,
}

impl<'a> FunctionEmitter<'a> {
    fn new(functions: &'a HashMap<String, FunctionSig>) -> Self {
        Self {
            functions,
            blocks: vec![BlockBuilder::new("entry")],
            current: 0,
            tmp_counter: 0,
            block_counter: 0,
            env: Vec::new(),
        }
    }

    fn bind(&mut self, name: String, value: ValueRef) {
        self.env.push(Binding { name, value });
    }

    fn lookup(&self, name: &str) -> Option<ValueRef> {
        self.env
            .iter()
            .rev()
            .find(|b| b.name == name)
            .map(|b| b.value.clone())
    }

    fn append(&mut self, inst: impl Into<String>) {
        self.blocks[self.current].instructions.push(inst.into());
    }

    fn append_return(&mut self, value: ValueRef, expected: &Type) {
        match expected {
            Type::Unit => self.append("ret void"),
            _ => self.append(format!("ret {} {}", llvm_type(expected), value.repr)),
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> Result<ValueRef, CodegenError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(ValueRef {
                repr: value.to_string(),
                ty: Type::I32,
            }),
            ExprKind::Bool(value) => Ok(ValueRef {
                repr: if *value { "1" } else { "0" }.to_string(),
                ty: Type::Bool,
            }),
            ExprKind::Var(name) => {
                if let Some(value) = self.lookup(name) {
                    Ok(value)
                } else if let Some(sig) = self.functions.get(name) {
                    Ok(ValueRef {
                        repr: format!("@{}", name),
                        ty: Type::Function(sig.params.clone(), Box::new(sig.ret.clone())),
                    })
                } else {
                    Err(CodegenError::UnknownVariable {
                        name: name.clone(),
                        span: expr.span,
                    })
                }
            }
            ExprKind::Binary { op, left, right } => self.emit_binary(*op, expr.span, left, right),
            ExprKind::If {
                cond,
                then_branch,
                else_branch,
            } => self.emit_if(expr.span, cond, then_branch, else_branch),
            ExprKind::Block(block) => self.emit_block(block),
            ExprKind::Call { callee, args } => self.emit_call(expr.span, callee, args),
        }
    }

    fn emit_block(&mut self, block: &Block) -> Result<ValueRef, CodegenError> {
        let base = self.env.len();
        let mut last = ValueRef::unit();
        for item in &block.items {
            match item {
                BlockItem::Let(stmt) => {
                    let value = self.emit_expr(&stmt.value)?;
                    self.bind(stmt.name.clone(), value);
                    last = ValueRef::unit();
                }
                BlockItem::Expr(expr) => {
                    last = self.emit_expr(expr)?;
                }
            }
        }
        self.env.truncate(base);
        Ok(last)
    }

    fn emit_binary(
        &mut self,
        op: BinaryOp,
        span: Span,
        left: &Expr,
        right: &Expr,
    ) -> Result<ValueRef, CodegenError> {
        let left_val = self.emit_expr(left)?;
        let right_val = self.emit_expr(right)?;
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                if left_val.ty != Type::I32 || right_val.ty != Type::I32 {
                    return Err(CodegenError::InvalidBinary { op, span });
                }
                let instr = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Sub => "sub",
                    BinaryOp::Mul => "mul",
                    BinaryOp::Div => "sdiv",
                    _ => unreachable!(),
                };
                let name = self.fresh_value();
                self.append(format!(
                    "{} = {} i32 {}, {}",
                    name, instr, left_val.repr, right_val.repr
                ));
                Ok(ValueRef {
                    repr: name,
                    ty: Type::I32,
                })
            }
            BinaryOp::Eq => {
                if left_val.ty != right_val.ty {
                    return Err(CodegenError::InvalidBinary { op, span });
                }
                let ty = &left_val.ty;
                let name = self.fresh_value();
                self.append(format!(
                    "{} = icmp eq {} {}, {}",
                    name,
                    llvm_type(ty),
                    left_val.repr,
                    right_val.repr
                ));
                Ok(ValueRef {
                    repr: name,
                    ty: Type::Bool,
                })
            }
            BinaryOp::Lt | BinaryOp::Gt => {
                if left_val.ty != Type::I32 || right_val.ty != Type::I32 {
                    return Err(CodegenError::InvalidBinary { op, span });
                }
                let pred = match op {
                    BinaryOp::Lt => "slt",
                    BinaryOp::Gt => "sgt",
                    _ => unreachable!(),
                };
                let name = self.fresh_value();
                self.append(format!(
                    "{} = icmp {} i32 {}, {}",
                    name, pred, left_val.repr, right_val.repr
                ));
                Ok(ValueRef {
                    repr: name,
                    ty: Type::Bool,
                })
            }
        }
    }

    fn emit_if(
        &mut self,
        span: Span,
        cond: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
    ) -> Result<ValueRef, CodegenError> {
        let cond_val = self.emit_expr(cond)?;
        if cond_val.ty != Type::Bool {
            return Err(CodegenError::ExpectedBool { span });
        }
        let then_block = self.new_block("then");
        let else_block = self.new_block("else");
        let merge_block = self.new_block("merge");
        let then_label = self.blocks[then_block].label.clone();
        let else_label = self.blocks[else_block].label.clone();
        let merge_label = self.blocks[merge_block].label.clone();

        self.append(format!(
            "br i1 {}, label %{}, label %{}",
            cond_val.repr, then_label, else_label
        ));

        self.set_current(then_block);
        let then_val = self.emit_expr(then_branch)?;
        self.append(format!("br label %{}", merge_label));

        self.set_current(else_block);
        let else_val = self.emit_expr(else_branch)?;
        self.append(format!("br label %{}", merge_label));

        self.set_current(merge_block);
        if then_val.ty != else_val.ty {
            return Err(CodegenError::BranchTypeMismatch { span });
        }
        if matches!(then_val.ty, Type::Unit) {
            Ok(ValueRef::unit())
        } else {
            let ty = then_val.ty.clone();
            let then_repr = then_val.repr.clone();
            let else_repr = else_val.repr.clone();
            let name = self.fresh_value();
            self.append(format!(
                "{} = phi {} [ {}, %{} ], [ {}, %{} ]",
                name,
                llvm_type(&ty),
                then_repr,
                then_label,
                else_repr,
                else_label
            ));
            Ok(ValueRef { repr: name, ty })
        }
    }

    fn emit_call(
        &mut self,
        span: Span,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<ValueRef, CodegenError> {
        let callee_val = self.emit_expr(callee)?;
        let (param_types, ret_ty) = match &callee_val.ty {
            Type::Function(params, ret) => (params.clone(), (**ret).clone()),
            _ => return Err(CodegenError::NotCallable { span }),
        };
        if param_types.len() != args.len() {
            return Err(CodegenError::ArityMismatch {
                expected: param_types.len(),
                found: args.len(),
                span,
            });
        }
        let mut arg_vals = Vec::new();
        for arg in args {
            arg_vals.push(self.emit_expr(arg)?);
        }
        let operands = param_types
            .iter()
            .zip(arg_vals.iter())
            .map(|(ty, val)| format!("{} {}", llvm_type(ty), val.repr))
            .collect::<Vec<_>>()
            .join(", ");
        if ret_ty == Type::Unit {
            self.append(format!(
                "call {} {}({})",
                llvm_type(&ret_ty),
                callee_val.repr,
                operands
            ));
            Ok(ValueRef::unit())
        } else {
            let name = self.fresh_value();
            self.append(format!(
                "{} = call {} {}({})",
                name,
                llvm_type(&ret_ty),
                callee_val.repr,
                operands
            ));
            Ok(ValueRef {
                repr: name,
                ty: ret_ty,
            })
        }
    }

    fn set_current(&mut self, idx: usize) {
        self.current = idx;
    }

    fn new_block(&mut self, prefix: &str) -> usize {
        self.block_counter += 1;
        let label = format!("{}{}", prefix, self.block_counter);
        self.blocks.push(BlockBuilder::new(&label));
        self.blocks.len() - 1
    }

    fn fresh_value(&mut self) -> String {
        self.tmp_counter += 1;
        format!("%tmp{}", self.tmp_counter)
    }
}

impl BlockBuilder {
    fn new(label: &str) -> Self {
        Self {
            label: label.to_string(),
            instructions: Vec::new(),
        }
    }
}

impl ValueRef {
    fn unit() -> Self {
        Self {
            repr: String::new(),
            ty: Type::Unit,
        }
    }
}

fn llvm_type(ty: &Type) -> String {
    match ty {
        Type::I32 => "i32".to_string(),
        Type::Bool => "i1".to_string(),
        Type::Unit => "void".to_string(),
        Type::Function(params, ret) => {
            let params = params
                .iter()
                .map(|t| llvm_type(t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} ({})*", llvm_type(ret), params)
        }
    }
}
