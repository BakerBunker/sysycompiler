use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use super::InstructionKind;
use super::Program;

pub fn bb_optim(ir_program: &Program) {
    for func_ptr in &ir_program.functions {
        if func_ptr.borrow().builtin == true {
            continue;
        }
        let mut remove_index = vec![];
        for (bb_index, bb_ptr) in func_ptr.borrow_mut().basic_blocks.iter().enumerate() {
            let bb = bb_ptr.as_ref().borrow();
            if bb.instructions.len() == 1 {
                if let InstructionKind::Jmp { next_bb } = &bb.instructions[0].as_ref().borrow().inst
                {
                    //assert_eq!(bb.uses.len(), 1);

                    for prev_bb in &bb.users {
                        let user_terminator_ptr =
                            prev_bb.borrow().instructions.last().unwrap().clone();
                        let user_terminator = user_terminator_ptr.borrow_mut();
                        let mut new_terminator = user_terminator.clone();
                        match &user_terminator.inst {
                            InstructionKind::Jmp { .. } => {
                                if prev_bb.as_ref().borrow().no!=next_bb.as_ref().borrow().no{
                                    next_bb.borrow_mut().users.push(prev_bb.clone());
                                    prev_bb.as_ref().borrow_mut().uses.push(next_bb.clone());
                                }
                                new_terminator.inst = InstructionKind::Jmp {
                                    next_bb: next_bb.clone(),
                                }
                            }
                            InstructionKind::Br {
                                cond,
                                on_true,
                                on_false,
                            } => {
                                let new_on_true = if on_true.borrow().no == bb.no {
                                    if prev_bb.as_ref().borrow().no!=next_bb.as_ref().borrow().no{
                                        next_bb.borrow_mut().users.push(prev_bb.clone());
                                        prev_bb.as_ref().borrow_mut().uses.push(next_bb.clone());
                                    }
                                    next_bb.clone()
                                } else {
                                    on_true.clone()
                                };
                                let new_on_false = if on_false.borrow().no == bb.no {
                                    if prev_bb.as_ref().borrow().no!=next_bb.as_ref().borrow().no{
                                        next_bb.borrow_mut().users.push(prev_bb.clone());
                                        prev_bb.as_ref().borrow_mut().uses.push(next_bb.clone());
                                    }
                                    next_bb.clone()
                                } else {
                                    on_false.clone()
                                };
                                new_terminator.inst =
                                    if new_on_true.borrow().no == new_on_false.borrow().no {
                                        InstructionKind::Jmp {
                                            next_bb: new_on_true,
                                        }
                                    } else {
                                        InstructionKind::Br {
                                            cond: cond.clone(),
                                            on_true: new_on_true,
                                            on_false: new_on_false,
                                        }
                                    }
                            }
                            _ => unreachable!(),
                        }
                        if let Some(last) = prev_bb.borrow_mut().instructions.last_mut() {
                            *last = Rc::new(RefCell::new(new_terminator));
                        } else {
                            unreachable!()
                        }
                    }
                    remove_index.push(bb_index);
                    //func_ptr.as_ref().borrow_mut().basic_blocks.remove(bb_index);
                }
            }
        }
        for index in remove_index.iter().rev() {
            func_ptr.as_ref().borrow_mut().basic_blocks.remove(*index);
        }
        let mut block_set=HashSet::new();
        for block in &func_ptr.as_ref().borrow().basic_blocks{
            block_set.insert(block.as_ref().borrow().no);
        }
        //remove duplicate 
        for block in &func_ptr.as_ref().borrow().basic_blocks{
            let mut uniques=HashSet::new();
            block.as_ref().borrow_mut().users.retain(|b|{
                uniques.insert(b.as_ref().borrow().no)
            });
            let mut uniques=HashSet::new();
            block.as_ref().borrow_mut().uses.retain(|b|{
                uniques.insert(b.as_ref().borrow().no)
            });
            block.as_ref().borrow_mut().users.retain(|block|{
                block_set.contains(&block.as_ref().borrow().no)
            });
            block.as_ref().borrow_mut().uses.retain(|block|{
                block_set.contains(&block.as_ref().borrow().no)
            });
        }
    }
}
