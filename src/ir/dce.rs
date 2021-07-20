use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::ir::Var;

use super::load_store_map::init_load_store_map;
use super::InstructionKind;
use super::{Instruction, Program};

macro_rules! traverse_var {
    ($var:expr,$reachable:expr,$load_store_map:expr) => {
        if let Var::Instruction(var_ptr) = &*$var.as_ref().borrow() {
            traverse(var_ptr, $reachable, $load_store_map);
        }
    };
}

pub fn dead_code_elimination(program: &Program) {
    for function in &program.functions {
        if function.as_ref().borrow().builtin {
            continue;
        }

        let load_store_map = init_load_store_map(function);
        let mut reachable: HashSet<i32> = HashSet::new();
        //initialization
        for block in &function.as_ref().borrow().basic_blocks {
            for inst in &block.as_ref().borrow().instructions {
                match &inst.as_ref().borrow().inst {
                    //Terminators
                    InstructionKind::Ret { ret: op_ret } => {
                        reachable.insert(inst.as_ref().borrow().no);
                        if let Some(ret) = op_ret {
                            traverse_var!(ret, &mut reachable, &load_store_map);
                        }
                    }
                    InstructionKind::Br { cond, .. } => {
                        reachable.insert(inst.as_ref().borrow().no);
                        traverse_var!(cond, &mut reachable, &load_store_map);
                    }
                    InstructionKind::Jmp { .. } => {
                        reachable.insert(inst.as_ref().borrow().no);
                    }
                    //Instructions with side effect
                    InstructionKind::Call { args, .. } => {
                        reachable.insert(inst.as_ref().borrow().no);
                        for arg in args {
                            traverse_var!(arg, &mut reachable, &load_store_map);
                        }
                    }
                    InstructionKind::Store {
                        var,
                        data,
                        side_effect,
                    } => {
                        if *side_effect {
                            reachable.insert(inst.as_ref().borrow().no);
                            traverse_var!(var, &mut reachable, &load_store_map);
                            traverse_var!(data, &mut reachable, &load_store_map);
                        }
                    }
                    _ => {}
                }
            }
        }

        //delete
        for block in &function.as_ref().borrow().basic_blocks {
            block
                .as_ref()
                .borrow_mut()
                .instructions
                .retain(|inst| reachable.contains(&inst.as_ref().borrow().no));

            for inst in block.as_ref().borrow_mut().instructions.iter_mut() {
                let mut delete_index = vec![];
                for (index, user) in inst.as_ref().borrow().users.iter().enumerate() {
                    if !reachable.contains(&user.as_ref().borrow().no) {
                        delete_index.push(index);
                    }
                }
                for index in delete_index.iter().rev() {
                    inst.as_ref().borrow_mut().users.remove(*index);
                }
            }
        }
    }
}

fn traverse(
    inst: &Rc<RefCell<Instruction>>,
    reachable: &mut HashSet<i32>,
    load_store_map: &HashMap<i32, Vec<Rc<RefCell<Instruction>>>>,
) {
    if reachable.contains(&inst.as_ref().borrow().no) {
        return;
    }
    reachable.insert(inst.as_ref().borrow().no);
    match &inst.as_ref().borrow().inst {
        InstructionKind::Binary { lhs, rhs, .. } => {
            traverse_var!(lhs, reachable, load_store_map);
            traverse_var!(rhs, reachable, load_store_map);
        }
        InstructionKind::GetElementPtr { arr, index } => {
            traverse_var!(arr, reachable, load_store_map);
            traverse_var!(index, reachable, load_store_map);
        }
        InstructionKind::Load { var } => {
            traverse_var!(var, reachable, load_store_map);
            if let Some(load_srcs) = load_store_map.get(&inst.as_ref().borrow().no) {
                for load_src in load_srcs {
                    traverse(load_src, reachable, load_store_map);
                }
            }
        }
        InstructionKind::Call { args, .. } => {
            for arg in args {
                traverse_var!(arg, reachable, load_store_map);
            }
        }
        InstructionKind::Store { var, data, .. } => {
            traverse_var!(var, reachable, load_store_map);
            traverse_var!(data, reachable, load_store_map);
        }
        _ => {}
    }
}
