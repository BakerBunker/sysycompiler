use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

pub mod load_store_map;
pub mod bb_optim;
pub mod dce;
pub mod irgen;
pub mod util;

#[derive(Clone)]
pub enum Type {
    Void,
    Bool,
    Integer,
    Pointer(Box<Type>),
    Array {
        element_type: Box<Type>,
        element_num: usize,
    },
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Bool => "i1".to_string(),
            Type::Integer => "i32".to_string(),
            Type::Array {
                element_type,
                element_num,
            } => {
                format!(
                    "[ {} x {} ]",
                    element_num.to_string(),
                    element_type.to_string()
                )
            }
            Type::Pointer(ty) => {
                format!("{}*", ty.to_string())
            }
        }
    }
}

impl Type {
    pub fn deref(&self) -> Type {
        match self {
            Type::Void | Type::Bool | Type::Integer => unreachable!(),
            Type::Pointer(inner) => *inner.clone(),
            Type::Array { element_type, .. } => *element_type.clone(),
        }
    }
}

pub enum Var {
    Function(Rc<RefCell<Function>>),
    Instruction(Rc<RefCell<Instruction>>),
    GlobalVar(Rc<RefCell<GlobalVariable>>),
    Parameter(Rc<RefCell<Parameter>>),
}

pub struct Program {
    pub global_vars: Vec<Rc<RefCell<GlobalVariable>>>,
    pub functions: Vec<Rc<RefCell<Function>>>,
}
impl Program {
    fn new() -> Self {
        Program {
            global_vars: vec![],
            functions: vec![],
        }
    }
}

pub struct GlobalVariable {
    pub name: String,
    pub is_constant: bool,
    pub ty: Type,
    pub init: Vec<i32>,
    pub users: Vec<Rc<RefCell<Instruction>>>,
}

pub struct Function {
    pub return_type: Type,
    pub name: String,
    pub parameters: Vec<Rc<RefCell<Parameter>>>,
    pub basic_blocks: Vec<Rc<RefCell<BasicBlock>>>,
    pub users: HashMap<String, Rc<RefCell<Function>>>,
    pub uses: HashMap<String, Rc<RefCell<Function>>>,
    pub builtin: bool,
}

pub struct Parameter {
    pub name: String,
    pub ty: Type,
    pub no: i32,
    pub users: Vec<Rc<RefCell<Instruction>>>,
}

pub struct BasicBlock {
    pub no: i32,
    pub name: String,
    pub users: Vec<Rc<RefCell<BasicBlock>>>,
    pub uses: Vec<Rc<RefCell<BasicBlock>>>,
    pub instructions: Vec<Rc<RefCell<Instruction>>>,
}

#[derive(Clone)]
pub struct Instruction {
    pub no: i32,
    pub users: Vec<Rc<RefCell<Instruction>>>,
    pub bb: Rc<RefCell<BasicBlock>>,
    pub inst: InstructionKind,
}

#[derive(Clone)]
pub enum InstructionKind {
    Number {
        num: i32,
    },
    Binary {
        tag: BinOp,
        lhs: Rc<RefCell<Var>>,
        rhs: Rc<RefCell<Var>>,
    },
    GetElementPtr {
        arr: Rc<RefCell<Var>>,
        index: Rc<RefCell<Var>>,
    },
    Load {
        var: Rc<RefCell<Var>>,
    },
    Store {
        var: Rc<RefCell<Var>>,
        data: Rc<RefCell<Var>>,
        side_effect:bool
    },
    Call {
        func: Rc<RefCell<Var>>,
        args: Vec<Rc<RefCell<Var>>>,
    },
    Alloca {
        name: String,
        ty: Type,
    },
    //Terminators
    Ret {
        ret: Option<Rc<RefCell<Var>>>,
    },
    Jmp {
        next_bb: Rc<RefCell<BasicBlock>>,
    },
    Br {
        cond: Rc<RefCell<Var>>,
        on_true: Rc<RefCell<BasicBlock>>,
        on_false: Rc<RefCell<BasicBlock>>,
    },
}
impl Debug for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionKind::Number { .. } => f.write_str("number"),
            InstructionKind::Binary { .. } => f.write_str("binary"),
            InstructionKind::GetElementPtr { .. } => f.write_str("getelementptr"),
            InstructionKind::Load { .. } => f.write_str("load"),
            InstructionKind::Store { .. } => f.write_str("store"),
            InstructionKind::Call { .. } => f.write_str("call"),
            InstructionKind::Alloca { .. } => f.write_str("alloca"),
            InstructionKind::Ret { .. } => f.write_str("ret"),
            InstructionKind::Jmp { .. } => f.write_str("jmp"),
            InstructionKind::Br { .. } => f.write_str("br"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}
