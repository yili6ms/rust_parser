use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use anyhow::Context;
use clap::Parser;
use dsl_driver::Compiler;
use tempfile::NamedTempFile;

#[derive(Debug, Parser)]
#[command(name = "dslc", about = "Experimental typed DSL compiler pipeline")]
struct Cli {
    /// Path to the input DSL file
    #[arg(value_name = "FILE")]
    input: PathBuf,

    /// Dump lexed tokens as debug output
    #[arg(long)]
    dump_tokens: bool,

    /// Dump the parsed AST using Debug formatting
    #[arg(long)]
    dump_ast: bool,

    /// Explicitly print a success message after typechecking
    #[arg(long)]
    typecheck: bool,

    /// Emit textual LLVM IR for the input program
    #[arg(long)]
    emit_llvm: bool,

    /// Pipe LLVM IR through `llvm-as` + `lli` and execute it
    #[arg(long)]
    run: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let source = std::fs::read_to_string(&cli.input)
        .with_context(|| format!("reading {}", cli.input.display()))?;

    let compiler = Compiler::default();
    let compilation = compiler.compile(&source)?;

    if cli.dump_tokens {
        println!("{}", compilation.dump_tokens());
    }

    if cli.dump_ast {
        println!("{}", compilation.dump_ast());
    }

    let ir_output = if cli.emit_llvm || cli.run {
        Some(compiler.emit_llvm(&compilation)?)
    } else {
        None
    };

    if cli.emit_llvm {
        if let Some(ir) = &ir_output {
            println!("{}", ir);
        }
    }

    if cli.run {
        if let Some(ir) = &ir_output {
            let exit_code = run_with_llvm_tools(ir)?;
            std::process::exit(exit_code);
        }
    }

    let default_success = !cli.dump_tokens && !cli.dump_ast && !cli.emit_llvm && !cli.run;
    if cli.typecheck || default_success {
        println!("typecheck passed for {}", cli.input.display());
    }

    Ok(())
}

fn run_with_llvm_tools(ir: &str) -> anyhow::Result<i32> {
    let bc_file = NamedTempFile::new().context("creating temporary bitcode file")?;
    let bc_path = bc_file.path().to_owned();

    let mut child = Command::new("llvm-as")
        .arg("-o")
        .arg(&bc_path)
        .stdin(Stdio::piped())
        .spawn()
        .context("spawning llvm-as (is LLVM installed?)")?;
    child
        .stdin
        .as_mut()
        .context("grabbing llvm-as stdin")?
        .write_all(ir.as_bytes())
        .context("writing IR into llvm-as")?;
    let status = child.wait().context("waiting on llvm-as")?;
    if !status.success() {
        anyhow::bail!("llvm-as failed with status {status}");
    }

    let status = Command::new("lli")
        .arg(&bc_path)
        .status()
        .context("running lli")?;
    match status.code() {
        Some(code) => Ok(code),
        None => anyhow::bail!("lli terminated by signal"),
    }
}
