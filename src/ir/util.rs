use std::{cell::RefCell, collections::HashMap, fs::File, io::Write, path::Path, rc::Rc};

use crate::ir::InstructionKind;

use super::{BasicBlock, BinOp, Function, Instruction, Program, Type, Var};

struct Env {
    bb_map: HashMap<i32, String>,
    temp_count: i32,
    inst_map: HashMap<i32, (String, Type)>,

    name_times: HashMap<String, i32>,
}

impl Env {
    fn new() -> Self {
        Env {
            bb_map: HashMap::new(),
            temp_count: 0,
            inst_map: HashMap::new(),
            name_times: HashMap::new(),
        }
    }

    fn get_bb_tag(&mut self, bb: &Rc<RefCell<BasicBlock>>) -> String {
        let bb_no = bb.borrow().no;
        if let Some(tag) = self.bb_map.get(&bb_no) {
            tag.clone()
        } else {
            unreachable!()
        }
    }

    fn get_var_tag(&mut self, var_ptr: &Rc<RefCell<Var>>) -> (String, Type) {
        match &*var_ptr.borrow() {
            Var::Function(func_ptr) => {
                let func = func_ptr.borrow();
                (func.name.clone(), func.return_type.clone())
            }
            Var::Instruction(inst_ptr) => {
                let inst = inst_ptr.borrow();
                self.get_inst_tag(&inst, None, None)
            }
            Var::GlobalVar(glob_ptr) => {
                let glob = glob_ptr.borrow();
                (
                    format!("@{}", glob.name),
                    Type::Pointer(Box::new(glob.ty.clone())),
                )
            }
            Var::Parameter(param_ptr) => {
                let param = param_ptr.borrow();
                (format!("%{}", param.name), param.ty.clone())
            }
        }
    }

    fn get_inst_tag(
        &mut self,
        inst: &Instruction,
        op_name: Option<String>,
        op_ty: Option<Type>,
    ) -> (String, Type) {
        match &inst.inst {
            InstructionKind::Number { num } => return (format!("{}", num), Type::Integer),
            _ => {}
        }
        if let Some((tag, t)) = self.inst_map.get(&inst.no) {
            (tag.clone(), t.clone())
        } else {
            let new_tag = if let Some(name) = op_name {
                let time = self.name_times.get(&name).unwrap_or(&0).clone();
                self.name_times.insert(name.clone(), time + 1);
                format!("%{}{}", name, time)
            } else {
                self.temp_count += 1;
                format!("%t{}", self.temp_count)
            };
            let ty = op_ty.unwrap();
            self.inst_map.insert(inst.no, (new_tag.clone(), ty.clone()));
            (new_tag, ty.clone())
        }
    }

    fn init_bb_map(&mut self,function:&Rc<RefCell<Function>>){
        let mut bb_no=0;
        for bb in &function.as_ref().borrow().basic_blocks{
            bb_no+=1;
            self.bb_map.insert(bb.as_ref().borrow().no, format!("bb{}",bb_no));
        }
    }
}

impl ToString for BinOp {
    fn to_string(&self) -> String {
        match self {
            BinOp::Add => String::from("add"),
            BinOp::Sub => String::from("sub"),
            BinOp::Mul => String::from("mul"),
            BinOp::Div => String::from("sdiv"),
            BinOp::Rem => String::from("srem"),
            BinOp::And => String::from("and"),
            BinOp::Or => String::from("or"),
            BinOp::Eq => String::from("icmp eq"),
            BinOp::Ne => String::from("icmp ne"),
            BinOp::Lt => String::from("icmp slt"),
            BinOp::Le => String::from("icmp sle"),
            BinOp::Gt => String::from("icmp sgt"),
            BinOp::Ge => String::from("icmp sge"),
        }
    }
}

impl Type {
    fn reference(&self) -> Type {
        Type::Pointer(Box::new(self.clone()))
    }
    fn decay(&self) -> Type {
        match self {
            Type::Array { element_type, .. } => Type::Pointer(element_type.clone()),
            _ => self.clone(),
        }
    }
}

fn global_init(init: &Vec<i32>, ty: &Type, start: usize, end: usize) -> String {
    match ty {
        Type::Array {
            element_type,
            element_num,
        } => {
            let mut list: Vec<String> = vec![];
            for i in (start..end).step_by((end - start) / element_num) {
                list.push(global_init(init, element_type, i, i + element_num));
            }
            let blocks = list
                .into_iter()
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap();
            format!("{} [ {} ]", ty.to_string(), blocks)
        }
        Type::Integer => format!("i32 {}", init[start]),
        _ => unreachable!(),
    }
}

fn inst_string(inst: &Instruction, env: &mut Env) -> String {
    match &inst.inst {
        InstructionKind::Number { .. } => unreachable!(),
        InstructionKind::Binary { tag, lhs, rhs } => {
            let (lhs_tag, lhs_ty) = env.get_var_tag(lhs);
            let (rhs_tag, _) = env.get_var_tag(rhs);

            let l_val_ty = match tag {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => Type::Integer,
                _ => Type::Bool,
            };
            let (l_val, _) = env.get_inst_tag(inst, None, Some(l_val_ty));
            let op = tag.to_string();

            format!(
                "    {} = {} {} {}, {} ",
                l_val,
                op,
                lhs_ty.to_string(),
                lhs_tag,
                rhs_tag
            )
        }
        InstructionKind::GetElementPtr { arr, index } => {
            let (arr_tag, arr_ty) = env.get_var_tag(arr);
            let (index_tag, _) = env.get_var_tag(index);
            let l_val_ty;
            let is_param_first = match &*arr.borrow() {
                Var::Instruction(inst) => {
                    matches!(inst.borrow().inst, InstructionKind::Load { .. })
                }
                _ => false,
            };
            let decay = if !is_param_first && matches!(arr_ty.deref(), Type::Array { .. }) {
                l_val_ty = arr_ty.deref().decay();
                "i32 0, "
            } else {
                l_val_ty = arr_ty.clone();
                ""
            };

            let (l_val, _) = env.get_inst_tag(inst, None, Some(l_val_ty));
            format!(
                "    {} = getelementptr {}, {} {},{}i32 {}",
                l_val,
                arr_ty.deref().to_string(),
                arr_ty.to_string(),
                arr_tag,
                decay,
                index_tag
            )
        }
        InstructionKind::Load { var } => {
            let (var_tag, var_type) = env.get_var_tag(var);
            let l_val_type = var_type.deref();
            let (l_val_tag, ty) = env.get_inst_tag(inst, None, Some(l_val_type));

            format!(
                "    {} = load {}, {} {}",
                l_val_tag,
                ty.to_string(),
                var_type.decay().to_string(),
                var_tag
            )
        }
        InstructionKind::Store { var, data,.. } => {
            let (var_tag, var_ty) = env.get_var_tag(var);
            let (data_tag, data_ty) = env.get_var_tag(data);

            format!(
                "    store {} {}, {} {}",
                data_ty.to_string(),
                data_tag,
                var_ty.to_string(),
                var_tag
            )
        }
        InstructionKind::Call { func, args } => {
            let (func_tag, ret_ty) = env.get_var_tag(func);
            let args_str = args
                .iter()
                .map(|arg| {
                    let (arg_tag, arg_ty) = env.get_var_tag(arg);
                    format!("{} {}", arg_ty.decay().to_string(), arg_tag)
                })
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap_or(String::new());

            if matches!(ret_ty, Type::Void) {
                format!("    call void @{}({})", func_tag, args_str)
            } else {
                let (l_val_tag, _) = env.get_inst_tag(inst, None, Some(ret_ty.clone()));
                format!(
                    "    {} = call {} @{}({})",
                    l_val_tag,
                    ret_ty.to_string(),
                    func_tag,
                    args_str
                )
            }
        }
        InstructionKind::Alloca { name, ty } => {
            let (l_val_tag, l_val_ty) =
                env.get_inst_tag(inst, Some(name.clone()), Some(ty.reference()));
            format!(
                "    {} = alloca {}",
                l_val_tag,
                l_val_ty.deref().to_string()
            )
        }
        InstructionKind::Ret { ret } => {
            if let Some(ret_var_ptr) = ret {
                let (ret_tag, _) = env.get_var_tag(ret_var_ptr);
                format!("    ret i32 {}", ret_tag)
            } else {
                String::from("    ret void")
            }
        }
        InstructionKind::Jmp { next_bb } => {
            let bb_tag = env.get_bb_tag(next_bb);
            format!("    br label %{}", bb_tag)
        }
        InstructionKind::Br {
            cond,
            on_true,
            on_false,
        } => {
            let (cond_tag, _) = env.get_var_tag(cond);
            let then_bb_tag = env.get_bb_tag(on_true);
            let else_bb_tag = env.get_bb_tag(on_false);
            format!(
                "    br i1 {}, label %{}, label %{}",
                cond_tag, then_bb_tag, else_bb_tag
            )
        }
    }
}

pub fn print_ir(program: &Program, path: &Path) {
    let mut output = File::create(path.with_extension("ir")).unwrap();
    for glob_ptr in &program.global_vars {
        let glob = glob_ptr.borrow();
        let glob_or_const = if glob.is_constant {
            "constant"
        } else {
            "global"
        };
        let mut ty = glob.ty.to_string();
        let init = if glob.init.is_empty() {
            "zeroinitializer".to_string()
        } else {
            if matches!(glob.ty, Type::Integer) {
                format!("{}", glob.init[0])
            } else {
                let list = &glob.init;
                ty = String::new();
                global_init(list, &glob.ty, 0, list.len())
            }
        };
        output
            .write_fmt(format_args!(
                "@{} = {} {} {}\n",
                glob.name, glob_or_const, ty, init
            ))
            .unwrap();
    }

    for func_ptr in &program.functions {
        let func = func_ptr.borrow();
        let mut env = Env::new();
        env.init_bb_map(func_ptr);
        if func.builtin {
            let args_str = func
                .parameters
                .iter()
                .map(|arg| format!("{}", arg.borrow().ty.to_string()))
                .reduce(|a, b| format!("{}, {}", a, b))
                .unwrap_or(String::new());
            output
                .write_fmt(format_args!(
                    "declare {} @{}({})\n",
                    func.return_type.to_string(),
                    func.name,
                    args_str
                ))
                .unwrap();
            continue;
        }

        let ret_type = func.return_type.to_string();
        let param_list = &func.parameters;
        let params = param_list
            .into_iter()
            .map(|x| {
                let param = x.borrow();
                format!("{} %{}", param.ty.to_string(), param.name)
            })
            .reduce(|a, b| format!("{} ,{}", a, b))
            .unwrap_or(String::new());

        output
            .write_fmt(format_args!(
                "define {} @{}({}){{\n",
                ret_type, func.name, params
            ))
            .unwrap();

        for basic_block_ptr in &func.basic_blocks {
            let basic_block = basic_block_ptr.borrow();
            let prev=basic_block.users.iter().map(|block|{
                format!("{}",env.get_bb_tag(block))
            }).reduce(|a,b|{
                format!("{}, {}",a,b)
            }).unwrap_or("".to_string());
            let next=basic_block.uses.iter().map(|block|{
                format!("{}",env.get_bb_tag(block))
            }).reduce(|a,b|{
                format!("{}, {}",a,b)
            }).unwrap_or("".to_string());
            output
                .write_fmt(format_args!(
                    "{}:\t\t;{} pred=[{}] next=[{}]\n",
                    env.get_bb_tag(basic_block_ptr),
                    basic_block.name,
                    prev,
                    next
                ))
                .unwrap();
            for inst_ptr in &basic_block.instructions {
                let inst = inst_ptr.borrow();
                output
                    .write_fmt(format_args!("{}\n", inst_string(&inst, &mut env)))
                    .unwrap();
            }
        }

        output.write_fmt(format_args!("}}\n")).unwrap();
    }
}
