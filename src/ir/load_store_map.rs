use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{BasicBlock, Function, Instruction, InstructionKind, Var};

pub fn init_load_store_map(
    function: &Rc<RefCell<Function>>,
) -> HashMap<i32, Vec<Rc<RefCell<Instruction>>>> {
    let mut block_map = HashMap::new();
    for block in &function.as_ref().borrow().basic_blocks {
        let alloc_store_map = init_alloc_store_map(block);
        block_map.insert(block.as_ref().borrow().no, alloc_store_map);
    }

    let local_block_map = block_map.clone();
    let mut is_converged = false;

    while !is_converged {
        is_converged = true;
        for block in function.as_ref().borrow().basic_blocks.iter().rev() {
            let mut in_map = HashMap::new();
            for user in &block.as_ref().borrow().users {
                let user_map = block_map.get(&user.as_ref().borrow().no).unwrap().clone();
                in_map = merge_map(&in_map, &user_map);
            }
            let map = block_map.get_mut(&block.as_ref().borrow().no).unwrap();
            let prev = map.clone();
            in_map.extend(
                local_block_map
                    .get(&block.as_ref().borrow().no)
                    .unwrap()
                    .clone()
                    .into_iter(),
            );
            *map = in_map;
            for key in map.keys() {
                if let Some(val) = prev.get(key) {
                    let new_val = map.get(key).unwrap();
                    let mut val_set = HashSet::new();
                    for inst in val {
                        val_set.insert(inst.as_ref().borrow().no);
                    }
                    for inst in new_val {
                        if !val_set.contains(&inst.as_ref().borrow().no) {
                            is_converged = false;
                            break;
                        }
                    }
                    if !is_converged {
                        break;
                    }
                } else {
                    is_converged = false;
                    break;
                }
            }
        }
    }

    let mut load_store_map: HashMap<i32, Vec<Rc<RefCell<Instruction>>>> = HashMap::new();
    for block in &function.as_ref().borrow().basic_blocks {
        let mut alloc_store_map = HashMap::new();
        for user in &block.as_ref().borrow().users {
            let user_map = block_map.get(&user.as_ref().borrow().no).unwrap();
            alloc_store_map = merge_map(&alloc_store_map, user_map);
        }
        let mut local_map = HashMap::new();
        for inst_ptr in &block.as_ref().borrow().instructions {
            let inst = inst_ptr.as_ref().borrow();
            match &inst.inst {
                InstructionKind::Alloca { ty, .. } => {
                    if matches!(ty, &crate::ir::Type::Integer) {
                        local_map.insert(inst.no, vec![inst_ptr.clone()]);
                    }
                }
                InstructionKind::Store { var, .. } => match &*var.as_ref().borrow() {
                    Var::Instruction(store_dst_ptr) => {
                        match &store_dst_ptr.as_ref().borrow().inst {
                            InstructionKind::Alloca { .. } => {
                                local_map.insert(
                                    store_dst_ptr.as_ref().borrow().no,
                                    vec![inst_ptr.clone()],
                                );
                            }
                            _ => {}
                        }
                    }
                    Var::Function(_) | Var::GlobalVar(_) | Var::Parameter(_) => {}
                },
                InstructionKind::Load { var } => match &*var.as_ref().borrow() {
                    Var::Instruction(load_src_ptr) => {
                        if let Some(local_store) = local_map.get(&load_src_ptr.as_ref().borrow().no)
                        {
                            if let Some(store_list) = load_store_map.get_mut(&inst.no) {
                                store_list.push(local_store[0].clone());
                            } else {
                                load_store_map.insert(inst.no, vec![local_store[0].clone()]);
                            }
                        } else if let Some(possible_last_stores) =
                            alloc_store_map.get(&load_src_ptr.as_ref().borrow().no)
                        {
                            for possible_last_store in possible_last_stores {
                                if let Some(store_list) = load_store_map.get_mut(&inst.no) {
                                    store_list.push(possible_last_store.clone());
                                } else {
                                    load_store_map
                                        .insert(inst.no, vec![possible_last_store.clone()]);
                                }
                            }
                        }
                    }
                    Var::Function(_) | Var::GlobalVar(_) | Var::Parameter(_) => {}
                },
                _ => {}
            }
        }
    }
    load_store_map
}

fn init_alloc_store_map(
    block: &Rc<RefCell<BasicBlock>>,
) -> HashMap<i32, Vec<Rc<RefCell<Instruction>>>> {
    let mut alloc_store_map = HashMap::new();
    for inst_ptr in &block.as_ref().borrow().instructions {
        let inst = inst_ptr.as_ref().borrow();
        match &inst.inst {
            InstructionKind::Alloca { ty, .. } => {
                if matches!(ty, &crate::ir::Type::Integer) {
                    alloc_store_map.insert(inst.no, vec![inst_ptr.clone()]);
                }
            }
            InstructionKind::Store { var, .. } => match &*var.as_ref().borrow() {
                Var::Instruction(store_dst_ptr) => match &store_dst_ptr.as_ref().borrow().inst {
                    InstructionKind::Alloca { .. } => {
                        alloc_store_map
                            .insert(store_dst_ptr.as_ref().borrow().no, vec![inst_ptr.clone()]);
                    }
                    _ => {}
                },
                Var::Function(_) | Var::GlobalVar(_) | Var::Parameter(_) => {}
            },
            _ => {}
        }
    }
    alloc_store_map
}

fn merge_map(
    mapa: &HashMap<i32, Vec<Rc<RefCell<Instruction>>>>,
    mapb: &HashMap<i32, Vec<Rc<RefCell<Instruction>>>>,
) -> HashMap<i32, Vec<Rc<RefCell<Instruction>>>> {
    let mut map;
    map = mapa.clone();
    for key in mapb.keys() {
        map.entry(*key).or_default().append(&mut mapb[key].clone());
        let mut inst_set = HashSet::new();
        map.get_mut(key)
            .unwrap()
            .retain(|inst_ptr| inst_set.insert(inst_ptr.as_ref().borrow().no));
    }
    map
}
