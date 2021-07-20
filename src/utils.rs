use std::{fs::File, io::Write, path::Path};

use crate::{lexical::Token, parse::Node, NodeType};

pub fn print_tokens(tokens: &Vec<Token>, path: &Path) {
    let mut output = File::create(path.with_extension("tokens")).unwrap();
    let mut i = 0;
    for token in tokens {
        output
            .write_fmt(format_args!("TokenNo:{}\n{:?}\n", i, token))
            .expect("");
        i += 1;
    }
}

pub fn print_tree(ast: &Vec<Node>, path: &Path, extension: &str, with_type: bool) {
    let mut output = File::create(path.with_extension(extension)).unwrap();
    for n in ast {
        visit(&n, 0, &mut output, with_type);
    }
    fn visit(node: &Node, level: u32, output: &mut File, with_type: bool) {
        match &node.ntype {
            NodeType::DeclStmt(nodes) => {
                print_len(level, format!("DeclStmt"), output);
                for n in nodes {
                    visit(&n, level + 1, output, with_type);
                }
            }
            NodeType::Func(ret, name, args, body) => {
                print_len(level, format!("Func {},returns {:?}", name, ret), output);
                //output.write(b"//args\n");
                for arg in args {
                    visit(&arg, level + 1, output, with_type);
                }
                //output.write(b"//body\n");
                visit(&body, level + 1, output, with_type);
            }
            NodeType::Number(num) => {
                let mut str = format!("Number {}", num);
                if with_type {
                    str.push_str(&format!(" with type {:?}", node.btype));
                }
                print_len(level, str, output);
            }
            NodeType::Nil => print_len(level, "Nil".into(), output),
            NodeType::Declare(btype, name, dims, init, scope) => {
                print_len(
                    level,
                    format!("Declare of {}({:?}) in {:?} scope", name, btype, scope),
                    output,
                );
                //output.write(b"//dims\n");
                if let Some(dimslist) = dims {
                    for dim in dimslist {
                        visit(&dim, level + 1, output, with_type);
                    }
                }
                //output.write(b"//init\n");
                if let Some(initlist) = init {
                    for init1 in initlist {
                        visit(&init1, level + 1, output, with_type);
                    }
                }
            }
            NodeType::InitList(list) => {
                print_len(level, "Initlist".into(), output);
                for i in list {
                    visit(&i, level + 1, output, with_type);
                }
            }
            NodeType::Access(name, indexes, _) => {
                let mut str = format!("Access {}", name);
                if with_type {
                    str.push_str(&format!(" with type {:?}", node.btype));
                }
                print_len(level, str, output);
                if let Some(indexeslist) = indexes {
                    for index in indexeslist {
                        visit(&index, level + 1, output, with_type);
                    }
                }
            }
            NodeType::BinOp(ttype, lhs, rhs) => {
                let mut str = format!("Binop {:?}", ttype);
                if with_type {
                    str.push_str(&format!(" with type {:?}", node.btype));
                }
                print_len(level, str, output);
                //output.write(b"//lhs\n");
                visit(&lhs, level + 1, output, with_type);
                //output.write(b"//rhs\n");
                visit(&rhs, level + 1, output, with_type);
            }
            NodeType::Call(name, args, _) => {
                let mut str = format!("Function call {}", name);
                if with_type {
                    str.push_str(&format!(" with type {:?}", node.btype));
                }
                print_len(level, str, output);
                for arg in args {
                    visit(&arg, level + 1, output, with_type);
                }
            }
            NodeType::Assign(name, indexes, rhs, _) => {
                print_len(level, format!("Assign {}", name), output);
                //output.write(b"//indexes\n");
                if let Some(indexlist) = indexes {
                    for index in indexlist {
                        visit(&index, level + 1, output, with_type);
                    }
                }
                //output.write(b"//rhs\n");
                visit(&rhs, level + 1, output, with_type);
            }
            NodeType::ExprStmt(expr) => {
                print_len(level, "ExprStmt".into(), output);
                visit(&expr, level + 1, output, with_type);
            }
            NodeType::Block(stmts) => {
                print_len(level, "Block".into(), output);
                for stmt in stmts {
                    visit(&stmt, level + 1, output, with_type);
                }
            }
            NodeType::If(cond, on_true, on_false) => {
                print_len(level, "If".into(), output);
                //output.write(b"//Cond\n");
                visit(&cond, level + 1, output, with_type);
                //output.write(b"//True\n");
                visit(&on_true, level + 1, output, with_type);
                if let Some(f) = on_false {
                    //output.write(b"//False\n");
                    visit(&f, level + 1, output, with_type);
                }
            }
            NodeType::While(cond, body) => {
                print_len(level, "While".into(), output);
                //output.write(b"//Cond\n");
                visit(&cond, level + 1, output, with_type);
                //output.write(b"//Body\n");
                visit(&body, level + 1, output, with_type);
            }
            NodeType::Break => {
                print_len(level, "Break".into(), output);
            }
            NodeType::Continue => {
                print_len(level, "Continue".into(), output);
            }
            NodeType::Return(ret) => {
                print_len(level, "Return".into(), output);
                if let Some(r) = ret {
                    // output.write(b"//Return expr\n");
                    visit(&r, level + 1, output, with_type);
                }
            }
        }
    }

    fn print_len(level: u32, msg: String, output: &mut File) {
        output.write(b"|").expect("write error");
        for _ in 0..level {
            output.write(b"--").expect("write error");
        }
        output
            .write_fmt(format_args!("{}\n", msg))
            .expect("write error");
    }
}
