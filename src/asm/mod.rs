pub mod asmgen;
pub mod util;

const ZERO: i32 = 0;
const RET: i32 = 2;
const ARGS: &[i32; 4] = &[4, 5, 6, 7];
const TEMPS: &[i32; 8] = &[8, 9, 10, 11, 12, 13, 14, 15];
const SAVED: &[i32; 8] = &[16, 17, 18, 19, 20, 21, 22, 23];
const SP: i32 = 29;
const FP: i32 = 30;
const RA: i32 = 31;

pub struct Program {
    globals: Vec<Global>,
    blocks: Vec<Block>,
}

pub struct Global {
    name: String,
    size: i32,
    inits: Vec<i32>,
}

pub struct Block {
    name: String,
    insts: Vec<Instruction>,
}

#[derive(Clone)]
pub enum Instruction {
    //placeholder
    Deleted,
    Nop,

    Add(i32, i32, i32),
    Addi(i32, i32, i32),
    Addiu(i32, i32, i32),

    Sub(i32, i32, i32),
    Subi(i32, i32, i32),

    Mul(i32, i32, i32),
    Muli(i32, i32, i32),
    Sll(i32, i32, i32),

    Div(i32, i32, i32),
    Divi(i32, i32, i32),
    Sra(i32,i32,i32),

    //Mflo(i32),
    Rem(i32, i32, i32),
    Remi(i32, i32, i32),

    //Mfhi(i32),
    And(i32, i32, i32),
    Andi(i32, i32, i32),

    Or(i32, i32, i32),
    Ori(i32, i32, i32),

    Lw(i32, i32, i32),
    LwGlob(i32, String),

    La(i32, i32, i32),
    LaGlob(i32, String),

    Li(i32, i32),
    Sw(i32, i32, i32),
    SwGlob(i32, String),

    Move(i32, i32),

    Xor(i32, i32, i32),
    Xori(i32, i32, i32),

    Seq(i32, i32, i32),
    Seqi(i32, i32, i32),
    Sne(i32, i32, i32),
    Snei(i32, i32, i32),
    Sgt(i32, i32, i32),
    Sgti(i32, i32, i32),
    Sge(i32, i32, i32),
    Sgei(i32, i32, i32),
    Slt(i32, i32, i32),
    Slti(i32, i32, i32),
    Sle(i32, i32, i32),
    Slei(i32, i32, i32),

    //Terminator
    J(String),
    Jal(String),
    Beq(i32, i32, String),
    Bne(i32, i32, String),
    Jr,

    //Special
    Syscall,
}
