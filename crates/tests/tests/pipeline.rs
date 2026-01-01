use dsl_driver::{
    ast::{BlockItem, ExprKind, Item},
    emit_llvm,
    lexer,
    lower_to_hir,
    lower_to_lowir,
    lower_to_mir,
    parser,
    typecheck,
    Type,
    TypeError,
    Compiler,
};

const SAMPLE: &str = r#"
fn add(x: i32, y: i32) -> i32 {
    let sum: i32 = x + y;
    sum
}

fn main_dummy() -> i32 {
    if true then add(1, 2) else 0
}
"#;

const INFER_SAMPLE: &str =
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../examples/infer.dsl"));

#[test]
fn sample_program_typechecks() {
    let tokens = lexer::lex(SAMPLE).expect("lexing sample");
    let program = parser::parse(&tokens).expect("parsing sample");
    let types = typecheck(&program).expect("typechecking sample");
    assert!(types.iter().next().is_some(), "expected inferred types");
}

#[test]
fn detects_type_mismatch() {
    let src = "fn bad(x: i32) -> bool { x }";
    let tokens = lexer::lex(src).unwrap();
    let program = parser::parse(&tokens).unwrap();
    let err = typecheck(&program).unwrap_err();
    match err {
        TypeError::Mismatch { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn emits_llvm_ir() {
    let tokens = lexer::lex(SAMPLE).expect("lex sample");
    let program = parser::parse(&tokens).expect("parse sample");
    let _types = typecheck(&program).expect("typecheck sample");
    let hir = lower_to_hir(&program);
    let mir = lower_to_mir(&hir);
    let low_ir = lower_to_lowir(&mir);
    let ir = emit_llvm(&low_ir).expect("codegen sample");
    assert!(ir.contains("define i32 @add"));
    assert!(ir.contains("call i32 @add(i32 1, i32 2)"));
}

#[test]
fn compiler_driver_runs_full_pipeline() {
    let compiler = Compiler::default();
    let compilation = compiler.compile(SAMPLE).expect("compile sample");
    assert!(!compilation.tokens.is_empty());
    assert_eq!(compilation.program.items.len(), 2);
    let ir = compiler.emit_llvm(&compilation).expect("emit llvm");
    assert!(ir.contains("define i32 @add"));
}

#[test]
fn infers_unannotated_bindings() {
    let tokens = lexer::lex(INFER_SAMPLE).expect("lex infer sample");
    let program = parser::parse(&tokens).expect("parse infer sample");
    let types = typecheck(&program).expect("typecheck infer sample");
    let infer_fn = program
        .items
        .iter()
        .find_map(|item| match item {
            Item::Function(func) if func.name == "infer_sum" => Some(func),
            _ => None,
        })
        .expect("function infer_sum exists");
    let ExprKind::Block(block) = &infer_fn.body.kind else {
        panic!("function body must be a block");
    };
    assert!(
        block.items.len() >= 3,
        "infer_sum block should contain bindings and return expression"
    );
    let BlockItem::Let(total) = &block.items[0] else {
        panic!("first item should be a let binding");
    };
    assert_eq!(
        types.expr_type(&total.value),
        Some(&Type::I32),
        "total binding should infer to i32"
    );
    let BlockItem::Let(doubled) = &block.items[1] else {
        panic!("second item should be the doubled binding");
    };
    assert_eq!(
        types.expr_type(&doubled.value),
        Some(&Type::I32),
        "doubled binding should infer to i32"
    );
    let BlockItem::Expr(return_expr) = &block.items[2] else {
        panic!("third item should be the return expression");
    };
    assert_eq!(
        types.expr_type(return_expr),
        Some(&Type::I32),
        "return expression should produce i32"
    );
}
