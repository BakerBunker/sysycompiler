use std::collections::HashMap;

use crate::{builtin_funcs, BasicType, Node, NodeType, TokenType};

use super::*;

macro_rules! rc_ref {
    ($exp:expr) => {
        Rc::new(RefCell::new($exp))
    };
}

macro_rules! inst_var {
    ($ins:expr) => {
        rc_ref!(Var::Instruction($ins))
    };
}

macro_rules! add_user {
    ($host:expr,$user:expr) => {
        $host.borrow_mut().add_user($user.clone())
    };
}

macro_rules! add_inst {
    ($bb:expr,$inst:expr) => {
        $bb.borrow_mut().instructions.push($inst.clone())
    };
}

macro_rules! add_bb {
    ($func:expr,$bb:expr) => {
        $func.borrow_mut().basic_blocks.push($bb.clone())
    };
}

macro_rules! add_bb_link {
    ($src:expr,$dst:expr) => {
        $src.borrow_mut().uses.push($dst.clone());
        $dst.borrow_mut().users.push($src.clone())
    };
}

struct Env {
    curr_func: Rc<RefCell<Function>>,
    curr_bb: Rc<RefCell<BasicBlock>>,
    global: HashMap<String, Rc<RefCell<Var>>>,
    local: Vec<HashMap<String, Rc<RefCell<Var>>>>,
    //continue,break
    loop_stack: Vec<(Rc<RefCell<BasicBlock>>, Rc<RefCell<BasicBlock>>)>,
    bb_num: i32,
    inst_num: i32,
}
impl Env {
    fn new() -> Self {
        Env {
            curr_func: Rc::new(RefCell::new(Function {
                name: String::new(),
                parameters: vec![],
                return_type: Type::Void,
                basic_blocks: vec![],
                users: HashMap::new(),
                uses: HashMap::new(),
                builtin: false,
            })),
            curr_bb: rc_ref!(BasicBlock {
                no: -1,
                name: "placeholder".to_string(),
                users: vec![],
                uses: vec![],
                instructions: vec![],
            }),
            global: HashMap::new(),
            local: vec![],
            loop_stack: vec![],
            bb_num: 0,
            inst_num: 0,
        }
    }

    fn start_scope(&mut self) {
        self.local.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.local.pop();
    }

    fn insert(&mut self, name: &String, var: Var) {
        if self.local.is_empty() {
            self.global.insert(name.clone(), rc_ref!(var));
        } else {
            self.local
                .last_mut()
                .unwrap()
                .insert(name.clone(), rc_ref!(var));
        }
    }

    fn find(&self, name: &String) -> Rc<RefCell<Var>> {
        for map in self.local.iter().rev() {
            if let Some(var) = map.get(name) {
                return var.clone();
            }
        }
        if let Some(var) = self.global.get(name) {
            var.clone()
        } else {
            unreachable!();
        }
    }

    fn get_inst_no(&mut self) -> i32 {
        self.inst_num += 1;
        self.inst_num
    }
    fn get_bb_no(&mut self) -> i32 {
        self.bb_num += 1;
        self.bb_num
    }
}

impl Var {
    fn add_user(&mut self, user: Rc<RefCell<Instruction>>) {
        match self {
            Var::Instruction(inst) => {
                inst.borrow_mut().users.push(user);
            }
            Var::GlobalVar(glob_var) => {
                glob_var.borrow_mut().users.push(user);
            }
            Var::Parameter(param) => {
                param.borrow_mut().users.push(user);
            }
            _ => unreachable!(),
        }
    }
}

impl BasicBlock {
    fn new(name: &str, env: &mut Env) -> Self {
        BasicBlock {
            no: env.get_bb_no(),
            name: name.to_string(),
            users: vec![],
            uses: vec![],
            instructions: vec![],
        }
    }

    fn completed(&self) -> bool {
        if self.instructions.is_empty() {
            return false;
        }
        match self.instructions.last().unwrap().borrow().inst {
            InstructionKind::Br { .. }
            | InstructionKind::Jmp { .. }
            | InstructionKind::Ret { .. } => true,
            _ => false,
        }
    }
}

impl Instruction {
    fn new_alloca(
        name: &String,
        ty: Type,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let alloca = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Alloca {
                name: name.clone(),
                ty
            }
        });
        env.insert(name, Var::Instruction(alloca.clone()));
        add_inst!(bb, alloca);
        alloca
    }
    fn new_store(
        side_effect:bool,
        var: &mut Rc<RefCell<Var>>,
        data: &mut Rc<RefCell<Var>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let store = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Store {
                side_effect,
                var: var.clone(),
                data: data.clone()
            }
        });
        add_user!(var, store);
        add_user!(data, store);
        add_inst!(bb, store);
        store
    }
    fn new_load(
        var: &mut Rc<RefCell<Var>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let load = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Load { var: var.clone() }
        });
        add_user!(var, load);
        add_inst!(bb, load);
        load
    }
    fn new_call(
        func_called: &mut Rc<RefCell<Var>>,
        func_caller: &mut Rc<RefCell<Function>>,
        args: Vec<Rc<RefCell<Var>>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let call = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Call {
                func: func_called.clone(),
                args: args.clone()
            }
        });
        match &*func_called.borrow_mut() {
            Var::Function(func) => {
                let called_name = func.borrow().name.clone();
                func.borrow_mut().users.insert(called_name, func.clone());
            }
            _ => unreachable!(),
        }
        for arg in args {
            add_user!(arg, call);
        }
        let caller_name = func_caller.borrow().name.clone();
        func_caller
            .borrow_mut()
            .uses
            .insert(caller_name, func_caller.clone());
        add_inst!(bb, call);
        call
    }
    fn new_bin_op(
        tag: BinOp,
        lhs: &mut Rc<RefCell<Var>>,
        rhs: &mut Rc<RefCell<Var>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let bin = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Binary {
                tag,
                lhs: lhs.clone(),
                rhs: rhs.clone()
            }
        });
        add_user!(lhs, bin);
        add_user!(rhs, bin);
        add_inst!(bb, bin);
        bin
    }
    fn new_get_element_ptr(
        arr: &mut Rc<RefCell<Var>>,
        index: &mut Rc<RefCell<Var>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let gep = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::GetElementPtr {
                arr: arr.clone(),
                index: index.clone(),
            }
        });
        add_user!(arr, gep);
        add_user!(index, gep);
        add_inst!(bb, gep);
        gep
    }
    fn new_ret(
        op_ret: &mut Option<Rc<RefCell<Var>>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let inst = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Ret {
                ret: op_ret.clone()
            }
        });
        if let Some(ret) = op_ret {
            add_user!(ret, inst);
        }
        add_inst!(bb, inst);
        inst
    }
    fn new_jmp(
        src_bb: &mut Rc<RefCell<BasicBlock>>,
        dst_bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let jmp = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: src_bb.clone(),
            inst: InstructionKind::Jmp {
                next_bb: dst_bb.clone()
            }
        });
        add_inst!(src_bb, jmp);
        add_bb_link!(src_bb, dst_bb);
        jmp
    }
    fn new_br(
        cond: &mut Rc<RefCell<Var>>,
        then_bb: &mut Rc<RefCell<BasicBlock>>,
        else_bb: &mut Rc<RefCell<BasicBlock>>,
        bb: &mut Rc<RefCell<BasicBlock>>,
        env: &mut Env,
    ) -> Rc<RefCell<Instruction>> {
        let br = rc_ref!(Instruction {
            no: env.get_inst_no(),
            users: vec![],
            bb: bb.clone(),
            inst: InstructionKind::Br {
                cond: cond.clone(),
                on_true: then_bb.clone(),
                on_false: else_bb.clone()
            }
        });
        add_user!(cond, br);
        add_inst!(bb, br);
        add_bb_link!(bb, then_bb);
        add_bb_link!(bb, else_bb);
        br
    }
    fn new_const(num: i32, bb: Rc<RefCell<BasicBlock>>) -> Rc<RefCell<Instruction>> {
        rc_ref!(Instruction {
            no: 0,
            users: vec![],
            bb,
            inst: InstructionKind::Number { num }
        })
    }
}

impl From<&BasicType> for Type {
    fn from(btype: &BasicType) -> Self {
        match btype {
            BasicType::Const | BasicType::Int => Type::Integer,
            BasicType::IntArray(dims) | BasicType::ConstArray(dims) => {
                let mut ty = Type::Integer;
                for dim in dims.iter().rev() {
                    if dim != &0 {
                        ty = Type::Array {
                            element_type: Box::new(ty.clone()),
                            element_num: *dim,
                        };
                    } else {
                        ty = Type::Pointer(Box::new(ty));
                    }
                }
                ty
            }
            BasicType::Void => Type::Void,
            _ => unreachable!(),
        }
    }
}

impl From<&TokenType> for BinOp {
    fn from(ttype: &TokenType) -> Self {
        use super::BinOp::*;
        match ttype {
            TokenType::Plus => Add,
            TokenType::Minus => Sub,
            TokenType::Multiply => Mul,
            TokenType::Divide => Div,
            TokenType::Modulus => Rem,
            TokenType::And => And,
            TokenType::Or => Or,
            TokenType::Equal => Eq,
            TokenType::NotEqual => Ne,
            TokenType::LesserThan => Lt,
            TokenType::LesserEqual => Le,
            TokenType::GreaterThan => Gt,
            TokenType::GreaterEqual => Ge,
            _ => unreachable!(),
        }
    }
}

impl BasicType {
    fn is_const(&self) -> bool {
        match self {
            &BasicType::Const | BasicType::ConstArray(_) => true,
            BasicType::Int | BasicType::IntArray(_) => false,
            _ => unreachable!(),
        }
    }
}

fn gen_expr(expr: &Node, env: &mut Env) -> Rc<RefCell<Instruction>> {
    match &expr.ntype {
        NodeType::Number(num) => Instruction::new_const(*num, env.curr_bb.clone()),
        NodeType::Access(name, op_index_nodes, decl) => {
            let mut last = env.find(&format!("{}.addr", name));
            let mut last_ins = Instruction::new_const(0, env.curr_bb.clone());

            match &decl.btype {
                //Access to Integer
                BasicType::Const | BasicType::Int => {
                    Instruction::new_load(&mut last, &mut env.curr_bb.clone(), env)
                }
                //Access to array
                BasicType::IntArray(dims) | BasicType::ConstArray(dims) => {
                    match &decl.ntype {
                        NodeType::Declare(_, _, _, _, scope) => {
                            if matches!(scope, &crate::Scope::Param) {
                                last = inst_var!(Instruction::new_load(
                                    &mut last,
                                    &mut env.curr_bb.clone(),
                                    env
                                ));
                            }
                        }
                        _ => unreachable!(),
                    }

                    //Access array with index
                    if let Some(index_nodes) = op_index_nodes {
                        for index_node in index_nodes {
                            let index = gen_expr(index_node, env);
                            last_ins = Instruction::new_get_element_ptr(
                                &mut last,
                                &mut inst_var!(index),
                                &mut env.curr_bb.clone(),
                                env,
                            );
                            last = inst_var!(last_ins.clone());
                        }
                        if index_nodes.len() == dims.len() {
                            Instruction::new_load(&mut last, &mut env.curr_bb.clone(), env)
                        } else {
                            last_ins.clone()
                        }
                    }
                    //Access to array itself
                    else {
                        Instruction::new_get_element_ptr(
                            &mut last,
                            &mut inst_var!(Instruction::new_const(0, env.curr_bb.clone())),
                            &mut env.curr_bb.clone(),
                            env,
                        )
                    }
                }
                _ => unreachable!(),
            }
        }
        NodeType::BinOp(ttype, lhs_node, rhs_node) => {
            let lhs = gen_expr(&lhs_node, env);
            match ttype.clone() {
                //TODO:Add short curcit in and/or
                TokenType::And | TokenType::Or => {
                    let rhs = gen_expr(&rhs_node, env);
                    let zero = inst_var!(Instruction::new_const(0, env.curr_bb.clone()));
                    let left = Instruction::new_bin_op(
                        BinOp::Ne,
                        &mut inst_var!(lhs.clone()),
                        &mut zero.clone(),
                        &mut env.curr_bb.clone(),
                        env,
                    );
                    let right = Instruction::new_bin_op(
                        BinOp::Ne,
                        &mut inst_var!(rhs.clone()),
                        &mut zero.clone(),
                        &mut env.curr_bb.clone(),
                        env,
                    );
                    Instruction::new_bin_op(
                        BinOp::from(ttype),
                        &mut inst_var!(left.clone()),
                        &mut inst_var!(right.clone()),
                        &mut env.curr_bb.clone(),
                        env,
                    )
                }
                _ => {
                    let rhs = gen_expr(&rhs_node, env);
                    Instruction::new_bin_op(
                        BinOp::from(ttype),
                        &mut inst_var!(lhs),
                        &mut inst_var!(rhs),
                        &mut env.curr_bb.clone(),
                        env,
                    )
                }
            }
        }
        NodeType::Call(name, arg_nodes, _) => {
            let mut var = env.find(&format!("{}.func", name));
            let mut arg_exprs = vec![];
            for arg_node in arg_nodes {
                arg_exprs.push(inst_var!(gen_expr(arg_node, env)));
            }
            Instruction::new_call(
                &mut var,
                &mut env.curr_func.clone(),
                arg_exprs,
                &mut env.curr_bb.clone(),
                env,
            )
        }
        _ => unreachable!(),
    }
}

fn gen_stmt(stmt: &Node, env: &mut Env) {
    match &stmt.ntype {
        NodeType::DeclStmt(decl_nodes) => {
            for decl_node in decl_nodes {
                if let NodeType::Declare(_, name, op_dims, inits, _) = &decl_node.ntype {
                    let mut dims = vec![];
                    if let Some(dim_nodes) = op_dims {
                        for dim_node in dim_nodes {
                            if let NodeType::Number(num) = dim_node.ntype {
                                dims.push(num);
                            }
                        }
                    }
                    let mut ty = Type::Integer;
                    for dim in dims.iter().rev() {
                        if dim == &0 {
                            ty = Type::Pointer(Box::new(ty));
                        } else {
                            ty = Type::Array {
                                element_type: Box::new(ty),
                                element_num: *dim as usize,
                            };
                        }
                    }
                    let alloca = Instruction::new_alloca(
                        &format!("{}.addr", name),
                        ty,
                        &mut env.curr_bb.clone(),
                        env,
                    );
                    if let Some(init_nodes) = inits {
                        //Not Array
                        if dims.is_empty() {
                            let init = gen_expr(&init_nodes[0], env);
                            Instruction::new_store(
                                false,
                                &mut inst_var!(alloca.clone()),
                                &mut inst_var!(init),
                                &mut env.curr_bb.clone(),
                                env,
                            );
                        }
                        //Array
                        else {
                            let mut total = 1;
                            let mut sizes = vec![];
                            for dim in dims.iter().rev() {
                                sizes.push(total);
                                total *= dim;
                            }
                            for (i, init_node) in init_nodes.iter().enumerate() {
                                let init = gen_expr(init_node, env);
                                let mut index = i as i32;
                                let mut pointer = alloca.clone();

                                for size in sizes.iter().rev() {
                                    let ind =
                                        Instruction::new_const(index / size, env.curr_bb.clone());
                                    pointer = Instruction::new_get_element_ptr(
                                        &mut inst_var!(pointer.clone()),
                                        &mut inst_var!(ind),
                                        &mut env.curr_bb.clone(),
                                        env,
                                    );
                                    index = index % size;
                                }
                                Instruction::new_store(
                                    true,
                                    &mut inst_var!(pointer.clone()),
                                    &mut inst_var!(init),
                                    &mut env.curr_bb.clone(),
                                    env,
                                );
                            }
                        }
                    }
                }
            }
        }
        NodeType::Assign(name, op_indexs, rhs_node, decl) => {
            let lhs = env.find(&format!("{}.addr", name));
            let rhs = gen_expr(&rhs_node, env);

            let mut indexs = vec![];
            if let Some(index_nodes) = op_indexs {
                for index_node in index_nodes {
                    let index = gen_expr(index_node, env);
                    indexs.push(index);
                }
            }

            let side_effect= match &decl.ntype {
                NodeType::Declare(_, _, _, _, scope) => {
                    match scope {
                        crate::Scope::Global => true,
                        crate::Scope::Local|
                        crate::Scope::Param => {
                            if indexs.is_empty(){
                                false
                            }else{
                                true
                            }
                        },
                    }
                }
                _ => unreachable!(),
            };

            if indexs.is_empty() {
                Instruction::new_store(
                    side_effect,
                    &mut lhs.clone(),
                    &mut inst_var!(rhs),
                    &mut env.curr_bb.clone(),
                    env,
                );
            } else {
                let mut last = lhs.clone();
                match &decl.ntype {
                    NodeType::Declare(_, _, _, _, scope) => {
                        if matches!(scope, &crate::Scope::Param) {
                            last = inst_var!(Instruction::new_load(
                                &mut lhs.clone(),
                                &mut env.curr_bb.clone(),
                                env
                            ));
                        }
                    }
                    _ => unreachable!(),
                }
                for index in indexs {
                    let inst = Instruction::new_get_element_ptr(
                        &mut last,
                        &mut inst_var!(index.clone()),
                        &mut env.curr_bb.clone(),
                        env,
                    );
                    last = inst_var!(inst);
                }
                Instruction::new_store(
                    side_effect,
                    &mut last,
                    &mut inst_var!(rhs),
                    &mut env.curr_bb.clone(),
                    env,
                );
            }
        }
        NodeType::ExprStmt(expr) => {
            gen_expr(expr, env);
        }
        NodeType::Block(stmt_nodes) => {
            env.start_scope();
            for stmt_node in stmt_nodes {
                gen_stmt(&stmt_node, env);
            }
            env.end_scope();
        }
        NodeType::If(cond, on_true, op_on_false) => {
            let cond_expr = gen_expr(&cond, env);
            let then_bb = rc_ref!(BasicBlock::new("if.then", env));
            let else_bb = rc_ref!(BasicBlock::new("if.else", env));
            let end_bb = rc_ref!(BasicBlock::new("if.end", env));
            add_bb!(&mut env.curr_func, then_bb);
            add_bb!(&mut env.curr_func, else_bb);
            add_bb!(&mut env.curr_func, end_bb);

            Instruction::new_br(
                &mut inst_var!(cond_expr),
                &mut then_bb.clone(),
                &mut else_bb.clone(),
                &mut env.curr_bb.clone(),
                env,
            );

            env.curr_bb = then_bb.clone();
            gen_stmt(&on_true, env);
            if !env.curr_bb.borrow().completed() {
                Instruction::new_jmp(&mut env.curr_bb.clone(), &mut end_bb.clone(), env);
            }

            env.curr_bb = else_bb.clone();
            if let Some(on_false) = op_on_false {
                gen_stmt(&on_false, env);
            }
            if !env.curr_bb.borrow().completed() {
                Instruction::new_jmp(&mut env.curr_bb.clone(), &mut end_bb.clone(), env);
            }

            env.curr_bb = end_bb.clone();
        }
        NodeType::While(cond, body) => {
            let cond_bb = rc_ref!(BasicBlock::new("while.cond", env));
            let body_bb = rc_ref!(BasicBlock::new("while.body", env));
            let end_bb = rc_ref!(BasicBlock::new("while.end", env));
            add_bb!(&mut env.curr_func, cond_bb);
            add_bb!(&mut env.curr_func, body_bb);
            add_bb!(&mut env.curr_func, end_bb);

            Instruction::new_jmp(&mut env.curr_bb.clone(), &mut cond_bb.clone(), env);

            env.curr_bb = cond_bb.clone();
            let expr = gen_expr(&cond, env);
            Instruction::new_br(
                &mut inst_var!(expr),
                &mut body_bb.clone(),
                &mut end_bb.clone(),
                &mut env.curr_bb.clone(),
                env,
            );

            env.curr_bb = body_bb.clone();
            env.loop_stack.push((cond_bb.clone(), end_bb.clone()));
            gen_stmt(&body, env);
            if !env.curr_bb.borrow().completed() {
                Instruction::new_jmp(&mut env.curr_bb.clone(), &mut cond_bb.clone(), env);
            }
            env.loop_stack.pop();

            env.curr_bb = end_bb.clone();
        }
        NodeType::Break => {
            let mut end = env.loop_stack.last().unwrap().1.clone();
            Instruction::new_jmp(&mut env.curr_bb.clone(), &mut end, env);
        }
        NodeType::Continue => {
            let mut next = env.loop_stack.last().unwrap().0.clone();
            Instruction::new_jmp(&mut env.curr_bb.clone(), &mut next, env);
        }
        NodeType::Return(op_ret_node) => {
            if let Some(ret_node) = op_ret_node {
                let ret = gen_expr(&ret_node, env);
                Instruction::new_ret(&mut Some(inst_var!(ret)), &mut env.curr_bb.clone(), env);
            } else {
                Instruction::new_ret(&mut None, &mut env.curr_bb.clone(), env);
            }
        }
        _ => unreachable!(),
    }
}

pub fn irgen(ast: &Vec<Node>) -> Program {
    let mut program = Program::new();
    let mut env = Env::new();
    for builtin in builtin_funcs() {
        if let NodeType::Func(ret_type, name, arg_nodes, _) = &builtin.ntype {
            let mut parameters = vec![];
            for (i, arg_node) in arg_nodes.iter().enumerate() {
                if let NodeType::Declare(btype, name, _, _, _) = &arg_node.ntype {
                    let parameter = Parameter {
                        name: name.clone(),
                        no: i as i32,
                        ty: Type::from(btype),
                        users: vec![],
                    };
                    parameters.push(rc_ref!(parameter));
                }
            }
            let func = rc_ref!(Function {
                return_type: Type::from(ret_type),
                name: name.clone(),
                parameters: parameters,
                basic_blocks: vec![],
                users: HashMap::new(),
                uses: HashMap::new(),
                builtin: true
            });
            env.global.insert(
                format!("{}.func", name),
                rc_ref!(Var::Function(func.clone())),
            );
            program.functions.push(func.clone())
        }
    }
    for node in ast {
        match &node.ntype {
            NodeType::DeclStmt(decl_nodes) => {
                for decl_node in decl_nodes {
                    if let NodeType::Declare(btype, name, _, op_init_nodes, _) = &decl_node.ntype {
                        let mut inits = vec![];
                        if let Some(init_nodes) = op_init_nodes {
                            for init_node in init_nodes {
                                if let NodeType::Number(num) = init_node.ntype {
                                    inits.push(num);
                                }
                            }
                        }

                        let glob_var = Rc::new(RefCell::new(GlobalVariable {
                            name: name.clone(),
                            is_constant: btype.is_const(),
                            ty: Type::from(btype),
                            init: inits,
                            users: vec![],
                        }));
                        program.global_vars.push(glob_var.clone());
                        env.insert(&format!("{}.addr", name), Var::GlobalVar(glob_var.clone()));
                    }
                }
            }
            NodeType::Func(ret_type, name, arg_nodes, body) => {
                let entry = rc_ref!(BasicBlock::new("entry", &mut env));
                env.curr_bb = entry.clone();
                let mut parameters = vec![];
                for (i, arg_node) in arg_nodes.iter().enumerate() {
                    if let NodeType::Declare(btype, name, _, _, _) = &arg_node.ntype {
                        let parameter = Parameter {
                            name: name.clone(),
                            no: i as i32,
                            ty: Type::from(btype),
                            users: vec![],
                        };
                        parameters.push(rc_ref!(parameter));
                    }
                }
                let func = Rc::new(RefCell::new(Function {
                    name: name.clone(),
                    parameters: parameters.clone(),
                    return_type: Type::from(ret_type),
                    basic_blocks: vec![entry.clone()],
                    users: HashMap::new(),
                    uses: HashMap::new(),
                    builtin: false,
                }));
                env.curr_func = func.clone();

                env.insert(&format!("{}.func", name), Var::Function(func.clone()));
                program.functions.push(func.clone());
                env.start_scope();
                for param in parameters {
                    env.insert(&param.borrow().name, Var::Parameter(param.clone()));
                    let inst = Instruction::new_alloca(
                        &format!("{}.addr", param.borrow().name),
                        param.borrow().ty.clone(),
                        &mut entry.clone(),
                        &mut env,
                    );
                    Instruction::new_store(
                        false,
                        &mut inst_var!(inst),
                        &mut rc_ref!(Var::Parameter(param.clone())),
                        &mut entry.clone(),
                        &mut env,
                    );
                }
                gen_stmt(&body, &mut env);
                if !env.curr_bb.borrow().completed() {
                    if ret_type == &BasicType::Void {
                        Instruction::new_ret(&mut None, &mut env.curr_bb.clone(), &mut env);
                    } else {
                        Instruction::new_ret(
                            &mut Some(inst_var!(Instruction::new_const(0, env.curr_bb.clone()))),
                            &mut env.curr_bb.clone(),
                            &mut env,
                        );
                    }
                }
                env.end_scope();
            }
            _ => unreachable!(),
        }
    }
    program
}
