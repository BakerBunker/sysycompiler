use clap::{App, Arg};
use colored::Colorize;
use std::{
    fs::{self, File},
    io::Write,
    path::Path,
    process::{Command, Stdio},
};
use sysycompiler::{asm::{self, asmgen::asmgen}, ir::{self, bb_optim::bb_optim, dce::dead_code_elimination, irgen::irgen}, lexical::tokenize, parse::parse, semantic::semantic, utils};

fn main() {
    let matches = App::new("SysY Compiler")
        .version("1.0")
        .author("bakerbunker@mail.nwpu.edu.cn")
        .about("Compile SysY file to LLVM IR or MIPS assembly")
        .arg(Arg::new("INPUT").about("SysY file").required(true))
        .arg(
            Arg::new("TOKEN")
                .long("token")
                .about("Print tokens to [output_dir]/[filename].tokens"),
        )
        .arg(
            Arg::new("AST")
                .long("ast")
                .about("Print ast tree to [output_dir]/[filename].ast"),
        )
        .arg(
            Arg::new("SEMANTIC")
                .long("semantic")
                .about("Print optimized ast tree to [output_dir]/[filename].optim_ast"),
        )
        .arg(
            Arg::new("IR")
                .long("ir")
                .about("Output LLVM IR to [output_dir]/[filename].ir"),
        )
        .arg(Arg::new("DCE")
            .long("dce")
            .about("Dead code elimination"))
        .arg(Arg::new("CFG")
            .long("cfg")
            .about("Simplify cfg"))
        .arg(
            Arg::new("IR PROGRAM")
                .long("test_ir")
                .about("Generate program with LLVM IR and output test result")
                .requires("IR"),
        )
        .arg(
            Arg::new("ASM")
                .long("asm")
                .about("Print mips asm to [output_dir]/[filename].asm"),
        )
        .arg(Arg::new("MARS")
            .long("mars")
            .about("Path of mars")
            .default_value("/usr/share/java/mars-mips/Mars.jar"))
        .arg(
            Arg::new("ASM PROGRAM")
                .long("test_asm")
                .about("Generate program with MIPS assembly and output test result (Needs Mars-a MIPS simulator)")
                .requires("ASM"),
        )
        .arg(
            Arg::new("OUTPUT")
                .long("output_dir")
                .about("Set the path of output file")
                .takes_value(true)
                .default_value("./output"),
        )
        .get_matches();

    let path_str = matches.value_of("INPUT").unwrap();
    let output_dir = Path::new(matches.value_of("OUTPUT").unwrap());
    let input_path = Path::new(path_str);
    let output_path = output_dir.join(input_path.file_name().unwrap());
    let ir_test_path = Path::new("./test_ir");
    let asm_test_path = Path::new("./test_asm");
    let sysy_lib_path = Path::new("./sysyruntime");
    let mars_path = Path::new(matches.value_of("MARS").unwrap());

    //Lexical Analysis
    let tokens = tokenize(path_str.to_string());
    if matches.is_present("TOKEN") {
        utils::print_tokens(&tokens, &output_path)
    }

    //Parsing
    let ast = parse(tokens);
    if matches.is_present("AST") {
        utils::print_tree(&ast, &output_path, "ast", false);
    }

    //Semantic Analysis
    let optim_ast = semantic(&ast,&path_str.to_string());
    if matches.is_present("SEMANTIC") {
        utils::print_tree(&optim_ast, &output_path, "optim_ast", true);
    }

    //Intermediate Representation
    let ir_program = irgen(&optim_ast);
    if matches.is_present("DCE"){
        dead_code_elimination(&ir_program);
    }
    if matches.is_present("CFG"){
        bb_optim(&ir_program);
    }
    if matches.is_present("IR") {
        ir::util::print_ir(&ir_program, &output_path);
        if matches.is_present("IR PROGRAM") {
            let llvm_as = Command::new("llvm-as")
                .arg(output_path.with_extension("ir"))
                .arg("-o")
                .arg(output_path.with_extension("bc"))
                .output()
                .unwrap();
            if !llvm_as.stderr.is_empty() {
                println!("{}", String::from_utf8(llvm_as.stderr).unwrap().red());
                panic!()
            }

            let llvm_link = Command::new("llvm-link")
                .arg(output_path.with_extension("bc"))
                .arg(sysy_lib_path.join("sylib.bc"))
                .arg("-o")
                .arg(output_path.with_extension("bc"))
                .output()
                .unwrap();
            if !llvm_link.stderr.is_empty() {
                println!("{}", String::from_utf8(llvm_link.stderr).unwrap().red());
                panic!()
            }

            let opt = Command::new("opt")
                //.args("-gvn -instcombine".split(" "))
                .arg(output_path.with_extension("bc"))
                .arg("-o")
                .arg(output_path.with_extension("bc"))
                .output()
                .unwrap();
            if !opt.stderr.is_empty() {
                println!("{}", String::from_utf8(opt.stderr).unwrap().red());
            }

            let clang = Command::new("clang")
                //.arg("-O1")
                .arg(output_path.with_extension("bc"))
                .arg("-o")
                .arg(output_path.with_extension(""))
                .output()
                .unwrap();
            if !clang.stderr.is_empty() {
                println!("{}", String::from_utf8(clang.stderr).unwrap().red());
                panic!()
            }

            let mut comm = Command::new(output_path.with_extension(""));
            if input_path.with_extension("in").exists() {
                let cat = Command::new("cat")
                    .stdout(Stdio::piped())
                    .arg(input_path.with_extension("in"))
                    .spawn()
                    .unwrap();
                comm.stdin(cat.stdout.unwrap());
            }
            let output = comm.output().expect("Execution failed");
            let mut result = File::create(
                ir_test_path.join(input_path.with_extension("txt").file_name().unwrap()),
            )
            .expect("Cannot open result file");
            let answer = fs::read(input_path.with_extension("out"))
                .unwrap_or(format!("No {:?} found", input_path.with_extension("out")).into_bytes());
            result
                .write_fmt(format_args!("# Program output\n"))
                .unwrap();
            result.write(output.stdout.as_slice()).unwrap();
            result.write(output.status.to_string().as_bytes()).unwrap();
            result
                .write_fmt(format_args!(
                    "\n# {}\n",
                    input_path.with_extension("out").to_str().unwrap()
                ))
                .unwrap();
            result.write(answer.as_slice()).unwrap();
        }
    }

    let asm_program = asmgen(&ir_program);
    if matches.is_present("ASM") {
        asm::util::print_mi(&asm_program, &output_path);
        if matches.is_present("ASM PROGRAM") {
            let mut comm = Command::new("java");
            if input_path.with_extension("in").exists() {
                let cat = Command::new("cat")
                    .stdout(Stdio::piped())
                    .arg(input_path.with_extension("in"))
                    .spawn()
                    .unwrap();
                comm.stdin(cat.stdout.unwrap());
            }
            let output = comm
                .args(&["-jar", mars_path.to_str().unwrap()])
                .arg("sm")
                .arg("nc")
                .arg(output_path.with_extension("s"))
                .output()
                .expect("Execution failed");
            let mut result = File::create(
                asm_test_path.join(input_path.with_extension("txt").file_name().unwrap()),
            )
            .expect("Cannot open result file");
            let answer = fs::read(input_path.with_extension("out"))
                .unwrap_or(format!("No {:?} found", input_path.with_extension("out")).into_bytes());
            result
                .write_fmt(format_args!("# Program output\n"))
                .unwrap();
            result.write(output.stdout.as_slice()).unwrap();
            result.write(output.status.to_string().as_bytes()).unwrap();
            result
                .write_fmt(format_args!(
                    "\n# {}\n",
                    input_path.with_extension("out").to_str().unwrap()
                ))
                .unwrap();
            result.write(answer.as_slice()).unwrap();
        }
    }
}
