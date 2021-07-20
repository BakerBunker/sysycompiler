use std::{
    fmt::Debug,
    fs::{self, File},
    io::Write,
    path::Path,
};

use super::{Instruction, Program};

trait ToRegString {
    fn to_reg_string(&self) -> String;
}

impl ToRegString for i32 {
    fn to_reg_string(&self) -> String {
        match *self {
            0 => String::from("$zero"),
            2 => String::from("$v0"),
            4..=7 => format!("$a{}", self - 4),
            8..=15 => format!("$t{}", self - 8),
            16..=23 => format!("$s{}", self - 16),
            29 => String::from("$sp"),
            30 => String::from("$fp"),
            31 => String::from("$ra"),
            _ => format!("${}", self),
        }
    }
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Deleted=>unreachable!(),
            Instruction::Nop => f.write_str("nop"),
            Instruction::Add(dst, lhs, rhs) => f.write_fmt(format_args!(
                "add {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Addi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "addi {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Addiu(dst, lhs, rhs) => f.write_fmt(format_args!(
                "addiu {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sub(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sub {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Subi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sub {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Mul(dst, lhs, rhs) => f.write_fmt(format_args!(
                "mul {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Muli(dst, lhs, rhs) => f.write_fmt(format_args!(
                "mul {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sll(dst, lhs, inst) => f.write_fmt(format_args!(
                "sll {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                inst
            )),
            Instruction::Div(dst, lhs, rhs) => f.write_fmt(format_args!(
                "div {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Divi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "div {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sra(dst, lhs, inst)=> f.write_fmt(format_args!(
                "sra {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                inst
            )),
            Instruction::Rem(dst, lhs, rhs) => f.write_fmt(format_args!(
                "rem {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Remi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "rem {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::And(dst, lhs, rhs) => f.write_fmt(format_args!(
                "and {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Andi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "andi {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Or(dst, lhs, rhs) => f.write_fmt(format_args!(
                "or {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Ori(dst, lhs, rhs) => f.write_fmt(format_args!(
                "ori {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Lw(dst, bias, reg) => f.write_fmt(format_args!(
                "lw {}, {}({})",
                dst.to_reg_string(),
                bias,
                reg.to_reg_string()
            )),
            Instruction::LwGlob(dst, label) => {
                f.write_fmt(format_args!("lw {}, {}", dst.to_reg_string(), label))
            }
            Instruction::La(dst, bias, reg) => f.write_fmt(format_args!(
                "la {},{}({})",
                dst.to_reg_string(),
                bias,
                reg.to_reg_string()
            )),
            Instruction::LaGlob(dst, label) => {
                f.write_fmt(format_args!("la {}, {}", dst.to_reg_string(), label))
            }
            Instruction::Li(dst, inst) => {
                f.write_fmt(format_args!("li {}, {}", dst.to_reg_string(), inst))
            }
            Instruction::Sw(dst, bias, reg) => f.write_fmt(format_args!(
                "sw {},{}({})",
                dst.to_reg_string(),
                bias,
                reg.to_reg_string()
            )),
            Instruction::SwGlob(dst, label) => {
                f.write_fmt(format_args!("sw {}, {}", dst.to_reg_string(), label))
            }
            Instruction::Move(dst, src) => f.write_fmt(format_args!(
                "move {}, {}",
                dst.to_reg_string(),
                src.to_reg_string()
            )),
            Instruction::Xor(dst, lhs, rhs) => f.write_fmt(format_args!(
                "xor {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Xori(dst, lhs, rhs) => f.write_fmt(format_args!(
                "xori {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Seq(dst, lhs, rhs) => f.write_fmt(format_args!(
                "seq {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Seqi(dst, lhs, rhs) => f.write_fmt(format_args!(
                "seq {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sne(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sne {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Snei(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sne {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sgt(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sgt {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Sgti(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sgt {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sge(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sge {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Sgei(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sge {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Slt(dst, lhs, rhs) => f.write_fmt(format_args!(
                "slt {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Slti(dst, lhs, rhs) => f.write_fmt(format_args!(
                "slti {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::Sle(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sle {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs.to_reg_string()
            )),
            Instruction::Slei(dst, lhs, rhs) => f.write_fmt(format_args!(
                "sle {}, {}, {}",
                dst.to_reg_string(),
                lhs.to_reg_string(),
                rhs
            )),
            Instruction::J(label) => f.write_fmt(format_args!("j {}", label)),
            Instruction::Jal(label) => f.write_fmt(format_args!("jal {}", label)),
            Instruction::Beq(lhs, rhs, label) => f.write_fmt(format_args!(
                "beq {}, {}, {}",
                lhs.to_reg_string(),
                rhs.to_reg_string(),
                label
            )),
            Instruction::Bne(lhs, rhs, label) => f.write_fmt(format_args!(
                "bne {}, {}, {}",
                lhs.to_reg_string(),
                rhs.to_reg_string(),
                label
            )),
            Instruction::Jr => f.write_str("jr $ra"),
            Instruction::Syscall => f.write_str("syscall"),
        }
    }
}

pub fn print_mi(mi_program: &Program, path: &Path) {
    let sylib = fs::read("./sysyruntime/sylib.s").unwrap();
    let mut output = File::create(path.with_extension("s")).expect("failed create asm file");

    output.write_fmt(format_args!(".data\n")).unwrap();
    for glob in &mi_program.globals {
        output.write_fmt(format_args!("{}:\n", glob.name)).unwrap();
        //Zeroinitializer
        if glob.inits.is_empty() {
            output
                .write_fmt(format_args!("  .space {}\n", glob.size * 4))
                .unwrap();
        } else {
            for init in &glob.inits {
                output
                    .write_fmt(format_args!("  .word {}\n", *init))
                    .unwrap();
            }
        }
    }

    output.write(".text\n".as_bytes()).unwrap();
    output.write(".globl main\n".as_bytes()).unwrap();
    for block in &mi_program.blocks {
        let name = if block.name == "main" {
            "main".to_string()
        } else {
            block.name.clone()
        };
        output.write_fmt(format_args!("{}:\n", name)).unwrap();

        for inst in &block.insts {
            output.write_fmt(format_args!("  {:?}\n", inst)).unwrap();
        }
    }
    output.write("\n".as_bytes()).unwrap();
    output.write(&sylib).unwrap();
}
