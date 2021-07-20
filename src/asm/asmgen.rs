use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use crate::{
    asm::Block,
    ir::{self, Type},
};

use super::*;

struct Env {
    function_name: String,
    arg_inst_set: HashSet<i32>,
    inst_type_map: HashMap<i32, ir::Type>,
    ///InstructionNo->(Bias,RegNo)
    inst_reg_map: HashMap<i32, (i32, i32)>,
    constant_arg_sets: VecDeque<HashSet<i32>>,
    reg_avalibie: [bool; 32],
    stack_size: i32,
    used_saved_reg_set:HashSet<i32>,
    arg_count: i32,
}

impl Env {
    fn next_arg_reg(&mut self) -> i32 {
        self.arg_count += 1;
        if let Some(set) = self.constant_arg_sets.front() {
            while set.contains(&self.arg_count) {
                self.arg_count += 1;
            }
        }
        self.arg_count
    }
    fn get_temp_reg(&mut self) -> i32 {
        for num in TEMPS {
            if self.reg_avalibie[*num as usize] == true {
                self.reg_avalibie[*num as usize] = false;
                return *num;
            }
        }
        unreachable!();
    }

    fn get_saved_reg(&mut self) -> i32 {
        for num in SAVED {
            if self.reg_avalibie[*num as usize] == true {
                self.reg_avalibie[*num as usize] = false;
                self.used_saved_reg_set.insert(*num);
                return *num;
            }
        }
        unreachable!();
    }

    fn free_reg(&mut self, reg: i32) {
        self.reg_avalibie[reg as usize] = true;
    }

    fn get_inst_reg(&self, var_ptr: &Rc<RefCell<ir::Var>>) -> (i32, i32) {
        match &*var_ptr.as_ref().borrow() {
            ir::Var::Instruction(inst_ptr) => {
                let inst_no = inst_ptr.as_ref().borrow().no;
                if let Some(reg) = self.inst_reg_map.get(&inst_no) {
                    *reg
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }
}

impl Type {
    fn size(&self) -> i32 {
        match self {
            Type::Void | Type::Bool => unreachable!(),
            Type::Pointer(..) | Type::Integer => 1,
            Type::Array {
                element_type,
                element_num,
            } => (*element_num as i32) * element_type.size(),
        }
    }
}

fn get_num(ptr: &Rc<RefCell<ir::Var>>) -> Option<i32> {
    match &*ptr.as_ref().borrow() {
        ir::Var::Instruction(inst_ptr) => match inst_ptr.as_ref().borrow().inst {
            ir::InstructionKind::Number { num } => Some(num),
            _ => None,
        },
        _ => unreachable!(),
    }
}

impl ir::Instruction {
    fn to_machine_insts(&self, env: &mut Env) -> Vec<Instruction> {
        match &self.inst {
            ir::InstructionKind::Number { .. } => unreachable!(),
            ir::InstructionKind::Alloca { .. } => vec![],
            ir::InstructionKind::Binary { tag, lhs, rhs } => {
                let mut inst_list = vec![];

                let saved_reg = env.get_saved_reg();

                match tag {
                    ir::BinOp::Add => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Addi(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Addi(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Add(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Sub => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            let l_reg=if l_num==0{
                                ZERO
                            }else{
                                inst_list.push(Instruction::Li(saved_reg, l_num));
                                saved_reg
                            };
                            inst_list.push(Instruction::Sub(saved_reg, l_reg, r_reg));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Subi(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sub(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Mul => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            if l_num.abs().count_ones() == 1 {
                                let pow = 32 - l_num.abs().leading_zeros() - 1;
                                inst_list.push(Instruction::Sll(saved_reg, r_reg, pow as i32));
                                if l_num.is_negative(){
                                    inst_list.push(Instruction::Sub(saved_reg,ZERO,saved_reg));
                                }
                            } else {
                                inst_list.push(Instruction::Muli(saved_reg, r_reg, l_num));
                            }
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            if r_num.abs().count_ones() == 1 {
                                let pow = 32 - r_num.abs().leading_zeros() - 1;
                                inst_list.push(Instruction::Sll(saved_reg, l_reg, pow as i32));
                                if r_num.is_negative(){
                                    inst_list.push(Instruction::Sub(saved_reg,ZERO,saved_reg));
                                }
                            } else {
                                inst_list.push(Instruction::Muli(saved_reg, l_reg, r_num));
                            }
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Mul(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Div => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            let l_reg=if l_num==0{
                                ZERO
                            }else{
                                inst_list.push(Instruction::Li(saved_reg, l_num));
                                saved_reg
                            };
                            inst_list.push(Instruction::Div(saved_reg, l_reg, r_reg));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            if r_num.abs().count_ones() == 1 {
                                let pow = 32 - r_num.abs().leading_zeros() - 1;
                                inst_list.push(Instruction::Sra(saved_reg, l_reg, pow as i32));
                                if r_num.is_negative(){
                                    inst_list.push(Instruction::Sub(saved_reg,ZERO,saved_reg));
                                }
                            }else{
                                inst_list.push(Instruction::Divi(saved_reg, l_reg, r_num));
                            }
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Div(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Rem => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Li(saved_reg, l_num));
                            inst_list.push(Instruction::Rem(saved_reg, saved_reg, r_reg));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Remi(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Rem(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::And => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Andi(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Andi(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::And(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Or => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Ori(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Ori(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Or(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Eq => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Seqi(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Seqi(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Seq(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Ne => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Snei(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Snei(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sne(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Lt => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sgti(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Slti(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Slt(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Le => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sgei(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Slei(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sle(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Gt => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Slti(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Sgti(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sgt(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                    ir::BinOp::Ge => {
                        if let Some(l_num) = get_num(lhs) {
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Slei(saved_reg, r_reg, l_num));
                            env.free_reg(r_reg);
                        } else if let Some(r_num) = get_num(rhs) {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            inst_list.push(Instruction::Sgei(saved_reg, l_reg, r_num));
                            env.free_reg(l_reg);
                        } else {
                            let (_, l_reg) = env.get_inst_reg(lhs);
                            let (_, r_reg) = env.get_inst_reg(rhs);
                            inst_list.push(Instruction::Sge(saved_reg, l_reg, r_reg));
                            env.free_reg(l_reg);
                            env.free_reg(r_reg);
                        }
                    }
                }

                let is_param = env.arg_inst_set.contains(&self.no);
                if is_param {
                    if env.arg_count < 4 {
                        inst_list.push(Instruction::Move(ARGS[env.arg_count as usize], saved_reg));
                        env.next_arg_reg();
                        env.free_reg(saved_reg);
                    } else {
                        inst_list.push(Instruction::Sw(saved_reg, (env.arg_count - 4) * 4, SP));
                        env.next_arg_reg();
                        env.free_reg(saved_reg);
                    }
                }

                env.inst_reg_map.insert(self.no, (0, saved_reg));

                inst_list
            }
            ir::InstructionKind::GetElementPtr { arr, index } => {
                let mut inst_list = vec![];

                let (ty, arr_reg) = match &*arr.as_ref().borrow() {
                    ir::Var::Instruction(inst_ptr) => {
                        match &inst_ptr.as_ref().borrow().inst {
                            //First time need to load address
                            ir::InstructionKind::Alloca { ty, .. } => {
                                let saved_reg = env.get_saved_reg();
                                let (bias, r) = env.get_inst_reg(arr);
                                assert_eq!(r, SP);
                                inst_list.push(Instruction::La(saved_reg, bias, SP));
                                (ty.clone(), saved_reg)
                            }
                            _ => {
                                let (_, reg) = env.get_inst_reg(arr);
                                let ty = env.inst_type_map[&inst_ptr.as_ref().borrow().no].clone();
                                (ty, reg)
                            }
                        }
                    }
                    ir::Var::GlobalVar(glob_ptr) => {
                        let saved_reg = env.get_saved_reg();
                        let glob_name = glob_ptr.as_ref().borrow().name.clone();
                        let ty = glob_ptr.as_ref().borrow().ty.clone();
                        inst_list.push(Instruction::LaGlob(saved_reg, glob_name));
                        (ty, saved_reg)
                    }
                    _ => unreachable!(),
                };

                let child_ty = ty.deref();
                match &*index.as_ref().borrow() {
                    ir::Var::Instruction(inst_ptr) => match &inst_ptr.as_ref().borrow().inst {
                        ir::InstructionKind::Number { num } => {
                            let index = child_ty.size() * num * 4;
                            inst_list.push(Instruction::Addi(arr_reg, arr_reg, index));
                        }
                        _ => {
                            let (_, index_reg) = env.get_inst_reg(index);
                            inst_list.push(Instruction::Muli(
                                index_reg,
                                index_reg,
                                4 * child_ty.size(),
                            ));
                            inst_list.push(Instruction::Add(arr_reg, arr_reg, index_reg));
                            env.free_reg(index_reg);
                        }
                    },
                    _ => unreachable!(),
                }

                let is_param = env.arg_inst_set.contains(&self.no);
                if is_param {
                    if env.arg_count < 4 {
                        inst_list.push(Instruction::Move(ARGS[env.arg_count as usize], arr_reg));
                        env.next_arg_reg();
                        env.free_reg(arr_reg);
                    } else {
                        inst_list.push(Instruction::Sw(arr_reg, (env.arg_count - 4) * 4, SP));
                        env.next_arg_reg();
                        env.free_reg(arr_reg);
                    }
                }

                env.inst_reg_map.insert(self.no, (0, arr_reg));
                env.inst_type_map.insert(self.no, child_ty);
                inst_list
            }
            ir::InstructionKind::Load { var } => {
                let mut inst_list = vec![];
                let load_reg = env.get_saved_reg();
                env.inst_reg_map.insert(self.no, (0, load_reg));

                match &*var.as_ref().borrow() {
                    ir::Var::Instruction(inst_ptr) => {
                        let ty = env.inst_type_map[&inst_ptr.as_ref().borrow().no].clone();
                        env.inst_type_map.insert(self.no, ty);
                        let (bias, reg) = env.get_inst_reg(var);
                        inst_list.push(Instruction::Lw(load_reg, bias, reg));
                        env.free_reg(reg);
                    }
                    ir::Var::GlobalVar(glob_ptr) => {
                        let glob_name = glob_ptr.as_ref().borrow().name.clone();
                        inst_list.push(Instruction::LwGlob(load_reg, glob_name));
                    }
                    _ => unreachable!(),
                }

                let is_param = env.arg_inst_set.contains(&self.no);
                if is_param {
                    if env.arg_count < 4 {
                        inst_list.push(Instruction::Move(ARGS[env.arg_count as usize], load_reg));
                        env.next_arg_reg();
                        env.free_reg(load_reg);
                    } else {
                        inst_list.push(Instruction::Sw(load_reg, (env.arg_count - 4) * 4, SP));
                        env.next_arg_reg();
                        env.free_reg(load_reg);
                    }
                }
                inst_list
            }
            ir::InstructionKind::Store { var, data,.. } => {
                let mut inst_list = vec![];
                let data_reg = match &*data.as_ref().borrow() {
                    ir::Var::Parameter(param_ptr) => {
                        let param = param_ptr.as_ref().borrow();
                        if param.no < ARGS.len() as i32 {
                            ARGS[param.no as usize]
                        } else {
                            let bias = env.stack_size + (param.no - ARGS.len() as i32) * 4;
                            let reg = env.get_temp_reg();
                            inst_list.push(Instruction::Lw(reg, bias, SP));
                            reg
                        }
                    }
                    ir::Var::Instruction(inst_ptr) => match inst_ptr.as_ref().borrow().inst {
                        ir::InstructionKind::Number { num } => {
                            let temp_reg = env.get_temp_reg();
                            inst_list.push(Instruction::Li(temp_reg, num));
                            temp_reg
                        }
                        _ => {
                            let (_, reg) = env.get_inst_reg(data);
                            reg
                        }
                    },
                    _ => unreachable!(),
                };

                match &*var.as_ref().borrow() {
                    ir::Var::Instruction(..) => {
                        let (bias, reg) = env.get_inst_reg(var);
                        inst_list.push(Instruction::Sw(data_reg, bias, reg));
                        env.free_reg(reg);
                    }
                    ir::Var::GlobalVar(glob_ptr) => {
                        inst_list.push(Instruction::SwGlob(
                            data_reg,
                            glob_ptr.as_ref().borrow().name.clone(),
                        ));
                    }
                    _ => unreachable!(),
                }
                env.free_reg(data_reg);
                inst_list
            }
            ir::InstructionKind::Call { func, args } => {
                let mut inst_list = vec![];

                //Constant Parameters
                if let Some(set) = env.constant_arg_sets.clone().front() {
                    for i in set {
                        let num = match &*args[*i as usize].as_ref().borrow() {
                            ir::Var::Instruction(inst_ptr) => {
                                match inst_ptr.as_ref().borrow().inst {
                                    ir::InstructionKind::Number { num } => num,
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        };
                        if *i < 4 {
                            inst_list.push(Instruction::Li(ARGS[*i as usize], num));
                        } else {
                            let temp = env.get_temp_reg();
                            inst_list.push(Instruction::Li(temp, num));
                            inst_list.push(Instruction::Sw(temp, (i - 4) * 4, SP));
                            env.next_arg_reg();
                            env.free_reg(temp);
                        }
                    }
                }

                let (have_return, func_name) = match &*func.as_ref().borrow() {
                    ir::Var::Function(func_ptr) => {
                        let func = func_ptr.as_ref().borrow();
                        (!matches!(func.return_type, Type::Void), func.name.clone())
                    }
                    _ => unreachable!(),
                };
                inst_list.push(Instruction::Jal(func_name));
                env.arg_count = 0;

                if have_return&&self.users.len()!=0 {
                    let reg = env.get_saved_reg();
                    inst_list.push(Instruction::Move(reg, RET));
                    env.inst_reg_map.insert(self.no, (0, reg));
                    let is_param = env.arg_inst_set.contains(&self.no);
                    if is_param {
                        if env.arg_count < 4 {
                            inst_list.push(Instruction::Move(ARGS[env.arg_count as usize], reg));
                            env.next_arg_reg();
                            env.free_reg(reg);
                        } else {
                            inst_list.push(Instruction::Sw(reg, (env.arg_count - 4) * 4, SP));
                            env.next_arg_reg();
                            env.free_reg(reg);
                        }
                    }
                }

                env.constant_arg_sets.pop_front();
                inst_list
            }
            ir::InstructionKind::Ret { ret } => {
                let mut inst_list = vec![];

                if let Some(ret_ptr) = ret {
                    match &*ret_ptr.as_ref().borrow() {
                        ir::Var::Instruction(inst_ptr) => match &inst_ptr.as_ref().borrow().inst {
                            ir::InstructionKind::Number { num } => {
                                inst_list.push(Instruction::Li(RET, *num));
                            }
                            _ => {
                                let (_, ret_reg) = env.get_inst_reg(ret_ptr);
                                inst_list.push(Instruction::Move(RET, ret_reg));
                                env.free_reg(ret_reg);
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                inst_list.push(Instruction::Lw(RA, env.stack_size - 4, SP));
                inst_list.push(Instruction::Lw(FP, env.stack_size - 8, SP));
                for (i, reg) in SAVED.iter().enumerate() {
                    inst_list.push(Instruction::Lw(
                        *reg,
                        env.stack_size - 12 - (i * 4) as i32,
                        SP,
                    ));
                }
                inst_list.push(Instruction::Addiu(SP, SP, env.stack_size));

                if env.function_name == "main" {
                    inst_list.push(Instruction::Move(ARGS[0], RET));
                    inst_list.push(Instruction::Li(RET, 17));
                    inst_list.push(Instruction::Syscall);
                }
                inst_list.push(Instruction::Jr);

                inst_list
            }
            ir::InstructionKind::Jmp { next_bb } => {
                let block_name = format!("${}_{}", env.function_name, next_bb.as_ref().borrow().no);
                vec![Instruction::J(block_name)]
            }
            ir::InstructionKind::Br {
                cond,
                on_true,
                on_false,
            } => {
                let mut inst_list = vec![];

                let cond_reg = match &*cond.as_ref().borrow() {
                    ir::Var::Instruction(inst_ptr) => match &inst_ptr.as_ref().borrow().inst {
                        ir::InstructionKind::Number { num } => {
                            let reg = env.get_temp_reg();
                            inst_list.push(Instruction::Li(reg, *num));
                            reg
                        }
                        _ => {
                            let (_, reg) = env.get_inst_reg(cond);
                            reg
                        }
                    },
                    _ => unreachable!(),
                };
                let true_name = format!("${}_{}", env.function_name, on_true.as_ref().borrow().no);
                let false_name =
                    format!("${}_{}", env.function_name, on_false.as_ref().borrow().no);

                inst_list.push(Instruction::Bne(cond_reg, ZERO, true_name));
                inst_list.push(Instruction::Beq(cond_reg, ZERO, false_name));

                env.free_reg(cond_reg);

                inst_list
            }
        }
    }
}

pub fn asmgen(ir_program: &ir::Program) -> Program {
    let mut program = Program {
        globals: vec![],
        blocks: vec![],
    };

    for glob_ptr in &ir_program.global_vars {
        let glob = glob_ptr.as_ref().borrow();
        let miglob = Global {
            name: glob.name.clone(),
            size: glob.ty.size(),
            inits: glob.init.clone(),
        };
        program.globals.push(miglob);
    }

    for func in &ir_program.functions {
        if func.as_ref().borrow().builtin {
            continue;
        }
        let mut env = Env {
            function_name: func.as_ref().borrow().name.clone(),
            inst_type_map: HashMap::new(),
            arg_inst_set: HashSet::new(),
            inst_reg_map: HashMap::new(),
            constant_arg_sets: VecDeque::new(),
            reg_avalibie: [true; 32],
            used_saved_reg_set:HashSet::new(),
            stack_size: 0,
            arg_count: 0,
        };
        //Count stack size
        let mut stack_size = 4    //Return Address
                            +4   //Frame Pointer 
                            +32; //Saved Registers
        let mut longest_arg = 0;
        for bb in &func.as_ref().borrow().basic_blocks {
            for inst_ptr in &bb.as_ref().borrow().instructions {
                let inst = inst_ptr.as_ref().borrow();
                match &inst.inst {
                    ir::InstructionKind::Alloca { ty, .. } => {
                        env.inst_type_map.insert(inst.no, ty.clone());
                        stack_size += ty.size() * 4;
                        env.inst_reg_map.insert(inst.no, (stack_size, SP));
                    }
                    ir::InstructionKind::Call { args, .. } => {
                        let mut const_arg_set = HashSet::new();
                        for (i, arg) in args.iter().enumerate() {
                            match &*arg.as_ref().borrow() {
                                ir::Var::Instruction(inst_ptr) => {
                                    let arg_no = inst_ptr.as_ref().borrow().no;
                                    env.arg_inst_set.insert(arg_no);
                                    match inst_ptr.as_ref().borrow().inst {
                                        ir::InstructionKind::Number { .. } => {
                                            const_arg_set.insert(i as i32);
                                        }
                                        _ => {}
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }
                        env.constant_arg_sets.push_back(const_arg_set);
                        if args.len() > longest_arg {
                            longest_arg = args.len();
                        }
                    }
                    _ => {}
                }
            }
        }
        if let Some(set) = env.constant_arg_sets.front() {
            while set.contains(&env.arg_count) {
                env.arg_count += 1;
            }
        }
        stack_size += if longest_arg > 4 {
            ((longest_arg - 4) * 4) as i32
        } else {
            0
        };
        env.stack_size = stack_size;
        for (no, (bias, _)) in env.inst_reg_map.clone() {
            env.inst_reg_map.insert(no, (env.stack_size - bias, SP));
        }

        let mut bb_count = 0;
        for bb in &func.as_ref().borrow().basic_blocks {
            bb_count += 1;
            let mut block = Block {
                name: format!("${}_{}", env.function_name, bb.as_ref().borrow().no),
                insts: vec![],
            };

            //Open stack at the start of function
            if bb_count == 1 {
                block.name = func.as_ref().borrow().name.clone();
                block.insts.push(Instruction::Addiu(SP, SP, -stack_size));
                block
                    .insts
                    .push(Instruction::Sw(RA, env.stack_size - 4, SP));
                block
                    .insts
                    .push(Instruction::Sw(FP, env.stack_size - 8, SP));
                for (i, reg) in SAVED.iter().enumerate() {
                    block.insts.push(Instruction::Sw(
                        *reg,
                        env.stack_size - 12 - (i * 4) as i32,
                        SP,
                    ));
                }
            }
            for inst_ptr in &bb.as_ref().borrow().instructions {
                for machine_inst in inst_ptr.as_ref().borrow().to_machine_insts(&mut env) {
                    //println!("{:?}",machine_inst);
                    block.insts.push(machine_inst);
                }
            }
            program.blocks.push(block);
        }

        for bb in &mut program.blocks{
            let mut delete_index=vec![];
            for (index,inst) in bb.insts.iter().enumerate(){
                match inst {
                    Instruction::Lw(dst,..)=>{
                        if SAVED.contains(dst)&&!env.used_saved_reg_set.contains(dst){
                            delete_index.push(index);
                        }
                    }
                    Instruction::Sw(dst,..)=>{
                        if SAVED.contains(dst)&&!env.used_saved_reg_set.contains(dst){
                            delete_index.push(index);
                        }
                    }
                    _=>{}
                }
            }
            for index in delete_index.iter().rev(){
                bb.insts.remove(*index);
            }
        }
    }
    program
}
