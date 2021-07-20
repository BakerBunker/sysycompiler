use std::{collections::HashMap, fs::File, io::Read, path::Path, usize};

use colored::Colorize;

use crate::{builtin_funcs, parse::Node, BasicType, NodeType, Scope, TokenType};

static mut FILEPATH:String=String::new();

#[derive(Clone)]
pub struct Var {
    btype: BasicType,
    node: Node,
}

impl Var {
    pub fn new(btype: BasicType, node: Node) -> Self {
        Var { btype, node }
    }
}

struct Env {
    global: HashMap<String, Var>,
    local: Vec<HashMap<String, Var>>,
    loop_count: i32,
    curr_return_type: BasicType,
    curr_function_name: String,
}
impl Env {
    fn new() -> Self {
        Env {
            global: HashMap::new(),
            local: vec![],
            loop_count: 0,
            curr_return_type: BasicType::Nil,
            curr_function_name: String::new(),
        }
    }

    fn insert(&mut self, name: String, btype: BasicType, node: Node) {
        if matches!(node.ntype, NodeType::Declare(..)) {
            if self.local.is_empty() {
                if let Some(var) = self.global.get(&name) {
                    if matches!(var.node.ntype, NodeType::Declare(..)) {
                        node.wrong(format!("{} had already been defined in global scope", name))
                    }
                }
            } else {
                if self.local.last().unwrap().contains_key(&name) {
                    node.wrong(format!("{} had already been defined in this scope", name));
                }
            }
        }
        if self.local.is_empty() || matches!(node.ntype, NodeType::Func(..)) {
            self.global.insert(name, Var::new(btype, node));
        } else {
            self.local
                .last_mut()
                .unwrap()
                .insert(name, Var::new(btype, node));
        }
    }

    fn start_scope(&mut self) {
        self.local.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.local.pop();
    }

    fn find(&self, name: &String,n:&Node) -> (BasicType, Node) {
        for map in self.local.iter().rev() {
            if let Some(var) = map.get(name) {
                return (var.btype.clone(), var.node.clone());
            }
        }
        if let Some(var) = self.global.get(name) {
            (var.btype.clone(), var.node.clone())
        } else {
            n.wrong(format!("Cannot find {} in its scope", name));
            unreachable!()
        }
    }

    fn start_loop(&mut self) {
        self.loop_count += 1;
    }
    fn end_loop(&mut self) {
        self.loop_count -= 1;
    }
    fn is_in_loop(&self) -> bool {
        if self.loop_count == 0 {
            false
        } else if self.loop_count > 0 {
            true
        } else {
            unreachable!()
        }
    }

    fn set_curr_func(&mut self, name: &String, ret: &BasicType) {
        self.curr_function_name = name.clone();
        self.curr_return_type = ret.clone();
    }
    fn get_curr_func(&self) -> (String, BasicType) {
        (
            self.curr_function_name.clone(),
            self.curr_return_type.clone(),
        )
    }
}

pub fn semantic(ast: &Vec<Node>,path:&String) -> Vec<Node> {
    unsafe {FILEPATH=path.clone()}
    let mut env = Env::new();
    for builtin in builtin_funcs() {
        if let NodeType::Func(ret, name, args, body) = builtin.ntype {
            env.insert(
                name.clone(),
                BasicType::Func(Box::new(ret.clone())),
                Node::new(NodeType::Func(ret.clone(), name, args, body)),
            );
        }
    }
    let mut new_nodes = vec![];
    for node in ast {
        match &node.ntype {
            NodeType::DeclStmt(_) => {
                let new = walk(node, &mut env);
                new_nodes.push(new);
            }
            _ => {}
        }
    }
    for node in ast {
        match &node.ntype {
            NodeType::DeclStmt(_) => {}
            _ => {
                let new = walk(node, &mut env);
                new_nodes.push(new);
            }
        }
    }
    new_nodes
}

fn walk(node: &Node, env: &mut Env) -> Node {
    use NodeType::*;
    match &node.ntype {
        Break => {
            if !env.is_in_loop() {
                node.wrong(format!("Break should in a loop"));
            }
            node.clone()
        }
        Continue => {
            if !env.is_in_loop() {
                node.wrong(format!("Continue should in a loop"));
            }
            node.clone()
        }
        Number(_) => {
            let mut new_node = node.clone();
            new_node.btype = BasicType::Const;
            new_node
        }
        DeclStmt(decls) => {
            let mut new_node = vec![];
            for decl in decls {
                new_node.push(walk(&decl, env));
            }
            Node::new(DeclStmt(new_node))
        }
        Declare(btype, name, dims, inits, scope) => {
            let mut ty = btype.clone();
            let new_dims = if let Some(dim) = dims {
                let mut new = vec![];
                let mut n = vec![];
                for dim_node in dim {
                    let result = eval(&dim_node, env);
                    if result <= 0 && !matches!(dim_node.ntype, NodeType::Nil) {
                        dim_node.wrong(format!("Dimension of {} should > 0", name));
                    }
                    new.push(Node {
                        start: dim_node.start,
                        end: dim_node.end,
                        ntype: Number(result),
                        btype: BasicType::Const,
                    });
                    n.push(result as usize);
                }
                if ty == BasicType::Int || matches!(ty, BasicType::IntArray(_)) {
                    ty = BasicType::IntArray(n);
                } else if ty == BasicType::Const || matches!(ty, BasicType::ConstArray(_)) {
                    ty = BasicType::ConstArray(n);
                }
                Some(new)
            } else {
                None
            };
            let mut new_inits = vec![];
            if let Some(init_nodes) = inits {
                if new_dims.is_none() && init_nodes.len() == 1 {
                    let mut new_node;
                    new_node = walk(&init_nodes[0], env);
                    if btype == &BasicType::Const || scope == &Scope::Global {
                        new_node = Node {
                            start: init_nodes[0].start,
                            end: init_nodes[0].end,
                            ntype: Number(eval(&init_nodes[0], env)),
                            btype: BasicType::Const,
                        };
                    }
                    new_inits.push(new_node);
                } else if let Some(ref n_dims) = new_dims {
                    if scope == &Scope::Global {
                        new_inits = expand_inits(&n_dims, &init_nodes, true, env, 0);
                    } else {
                        new_inits = expand_inits(&n_dims, &init_nodes, false, env, 0);
                    }
                } else {
                    node.wrong(format!("Wrong initializer for {}", name));
                    unreachable!()
                }
            }
            let n_inits = if new_inits.is_empty() {
                None
            } else {
                Some(new_inits)
            };
            let new_node = Node::new(Declare(
                ty.clone(),
                name.clone(),
                new_dims,
                n_inits,
                scope.clone(),
            ));
            env.insert(name.clone(), ty, new_node.clone());
            new_node
        }
        Access(name, indexes, _) => {
            let (btype, n) = env.find(name,node);
            if let NodeType::Declare(_, _, _, _, _) = n.ntype {
                match &btype {
                    BasicType::Const => {
                        let num = eval(node, env);
                        let mut new_node = Node {
                            start: node.start,
                            end: node.end,
                            ntype: Number(num),
                            btype: BasicType::Const,
                        };
                        new_node.btype = BasicType::Const;
                        return new_node;
                    }
                    BasicType::Int => {
                        let mut nn = n.clone();
                        nn.btype = btype.clone();
                        Node {
                            start: node.start,
                            end: node.end,
                            ntype: Access(name.clone(), indexes.clone(), Box::new(nn)),
                            btype: BasicType::Int,
                        }
                    }
                    BasicType::IntArray(dims) | BasicType::ConstArray(dims) => {
                        if indexes.is_none() {
                            let mut nn = n.clone();
                            nn.btype = btype.clone();
                            return Node {
                                start: node.start,
                                end: node.end,
                                ntype: Access(name.clone(), None, Box::new(nn)),
                                btype: btype.clone(),
                            };
                        }
                        let mut new_indexes = vec![];
                        for index in indexes.as_ref().unwrap() {
                            let new_index = walk(&index, env);
                            if new_index.btype != BasicType::Int
                                && new_index.btype != BasicType::Const
                            {
                                node.wrong(format!("Index of {} should be int or const", name));
                            }
                            new_indexes.push(new_index);
                        }
                        let dim_len = dims.len();
                        let index_len = new_indexes.len();
                        let bty = if matches!(&btype, BasicType::IntArray(_)) {
                            if index_len == dim_len {
                                BasicType::Int
                            } else {
                                let arr = dims[index_len..dim_len].to_vec();
                                BasicType::IntArray(arr)
                            }
                        } else {
                            if index_len == dim_len {
                                BasicType::Const
                            } else {
                                let arr = dims[index_len..dim_len].to_vec();
                                BasicType::ConstArray(arr)
                            }
                        };
                        let mut nn = n.clone();
                        nn.btype = btype.clone();
                        Node {
                            start: node.start,
                            end: node.end,
                            ntype: Access(name.clone(), Some(new_indexes), Box::new(nn)),
                            btype: bty,
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                node.wrong(format!(
                    "{} cannot be accessed since it is a function",
                    name
                ));
                unreachable!()
            }
        }
        BinOp(ttype, lhs, rhs) => {
            let new_lhs = walk(&lhs, env);
            if new_lhs.btype != BasicType::Int && new_lhs.btype != BasicType::Const {
                lhs.wrong(format!(
                    "Expression at the left of the operator should be int or const"
                ));
            }
            let new_rhs = walk(&rhs, env);
            if new_rhs.btype != BasicType::Int && new_rhs.btype != BasicType::Const {
                rhs.wrong(format!(
                    "Expression at the right of the operator should be int or const"
                ));
            }
            if new_lhs.btype == BasicType::Const && new_rhs.btype == BasicType::Const {
                return Node {
                    start: node.start,
                    end: node.end,
                    ntype: Number(eval(node, env)),
                    btype: BasicType::Const,
                };
            }
            Node {
                start: node.start,
                end: node.end,
                ntype: BinOp(ttype.clone(), Box::new(new_lhs), Box::new(new_rhs)),
                btype: BasicType::Int,
            }
        }
        Call(name, call_args, _) => {
            let (_, n) = env.find(&name,node);
            if let Func(ret, _, def_args, _) = &n.ntype {
                if call_args.len() != def_args.len() {
                    node.wrong(format!(
                        "Argument length of {} should be {} instead of {}",
                        name,
                        def_args.len(),
                        call_args.len()
                    ));
                }
                let mut new_call_args = vec![];
                for (call_arg, def_arg) in call_args.iter().zip(def_args.iter()) {
                    let new_call_arg = walk(&call_arg, env);
                    new_call_args.push(new_call_arg.clone());
                    //Both int/const
                    if let Declare(def_btype, _, _, _, _) = &def_arg.ntype {
                        if def_btype == &BasicType::Int
                            && (new_call_arg.btype == BasicType::Int
                                || new_call_arg.btype == BasicType::Const)
                        {
                            continue;
                        }
                    }
                    //Both array
                    if let Declare(def_btype, _, _, _, _) = &def_arg.ntype {
                        if let BasicType::IntArray(def_dims) = def_btype {
                            if let BasicType::IntArray(call_dims) = &new_call_arg.btype {
                                for (call_dim, def_dim) in
                                    call_dims.iter().zip(def_dims.iter()).skip(1)
                                {
                                    if call_dim != def_dim {
                                        call_arg.wrong(format!(
                                            "Wrong dimension in function call {}",
                                            name
                                        ));
                                    }
                                }
                                continue;
                            }
                        }
                    }
                    //Others
                    call_arg.wrong(format!("Unmatched type in function call {}", name));
                }
                Node {
                    start: node.start,
                    end: node.end,
                    ntype: Call(name.clone(), new_call_args, Box::new(n.clone())),
                    btype: ret.clone(),
                }
            } else {
                node.wrong(format!("{} is not a function", name));
                unreachable!();
            }
        }
        Assign(name, indexes, expr, _) => {
            let (btype, n) = env.find(name,node);
            if let Declare(_, _, _, _, _) = n.ntype {
                match &btype {
                    BasicType::Const | BasicType::ConstArray(_) => {
                        node.wrong(format!("Cannot assign to constant {}", name));
                        unreachable!()
                    }
                    BasicType::Int => {
                        if indexes.is_some() {
                            node.wrong(format!(
                                "Integer {} should not have indexes in assign",
                                name
                            ));
                        }
                        let new_expr = walk(expr, env);
                        if new_expr.btype != BasicType::Int && new_expr.btype != BasicType::Const {
                            node.wrong(format!("Should assign int/const to int"))
                        }
                        Node {
                            start: node.start,
                            end: node.end,
                            ntype: Assign(
                                name.clone(),
                                None,
                                Box::new(new_expr),
                                Box::new(n.clone()),
                            ),
                            btype: BasicType::Nil,
                        }
                    }
                    BasicType::IntArray(dims) => {
                        if indexes.is_none() {
                            node.wrong(format!(
                                "Integer array {} should have indexes in assign",
                                name
                            ));
                        }
                        let new_expr = walk(expr, env);
                        if new_expr.btype != BasicType::Int && new_expr.btype != BasicType::Const {
                            node.wrong(format!("Should assign int/const to int"));
                        }
                        if indexes.as_ref().unwrap().len() != dims.len() {
                            node.wrong(format!(
                                "Indexes of {} should be {} instead of {}",
                                name,
                                dims.len(),
                                indexes.as_ref().unwrap().len()
                            ))
                        }
                        let mut new_indexes = vec![];
                        for index in indexes.as_ref().unwrap() {
                            let new_index = walk(&index, env);
                            if new_index.btype != BasicType::Int
                                && new_index.btype != BasicType::Const
                            {
                                node.wrong(format!("Index of array {} should be int/const", name));
                            }
                            new_indexes.push(new_index);
                        }

                        let mut decl_node = n.clone();
                        decl_node.btype = btype;
                        Node {
                            start: node.start,
                            end: node.end,
                            ntype: Assign(
                                name.clone(),
                                Some(new_indexes),
                                Box::new(new_expr),
                                Box::new(decl_node),
                            ),
                            btype: BasicType::Nil,
                        }
                    }
                    _ => unreachable!(),
                }
            } else {
                node.wrong(format!("Cannot assign to function {}", name));
                unreachable!()
            }
        }
        ExprStmt(expr) => Node {
            start: node.start,
            end: node.end,
            ntype: ExprStmt(Box::new(walk(expr, env))),
            btype: BasicType::Nil,
        },
        Block(stmts) => {
            env.start_scope();
            let mut new_stmts = vec![];
            for stmt in stmts {
                new_stmts.push(walk(&stmt, env));
            }
            env.end_scope();
            Node {
                start: node.start,
                end: node.end,
                ntype: Block(new_stmts),
                btype: BasicType::Nil,
            }
        }
        If(cond, on_true, on_false) => {
            let new_cond = walk(cond, env);
            if new_cond.btype != BasicType::Int && new_cond.btype != BasicType::Const {
                node.wrong(format!("Condition of if statement should be int/const"));
            }
            let new_on_false = if let Some(on_false_block) = on_false {
                Some(Box::new(walk(on_false_block, env)))
            } else {
                None
            };
            Node {
                start: node.start,
                end: node.end,
                ntype: If(
                    Box::new(new_cond),
                    Box::new(walk(on_true, env)),
                    new_on_false,
                ),
                btype: BasicType::Nil,
            }
        }
        While(cond, body) => {
            let new_cond = walk(cond, env);
            if new_cond.btype != BasicType::Int && new_cond.btype != BasicType::Const {
                node.wrong(format!("Condition of if statement should be int/const"));
            }
            env.start_loop();
            let new_body = Box::new(walk(body, env));
            env.end_loop();
            Node {
                start: node.start,
                end: node.end,
                ntype: While(Box::new(new_cond), new_body),
                btype: BasicType::Nil,
            }
        }
        Return(expr) => {
            let new_expr: Option<Box<Node>>;
            let mut ret_type: BasicType;
            let (name, ret) = env.get_curr_func();
            if let Some(exp) = expr {
                let new_exp = walk(exp, env);
                ret_type = new_exp.btype.clone();
                new_expr = Some(Box::new(new_exp));
            } else {
                ret_type = BasicType::Void;
                new_expr = None;
            }
            if ret_type == BasicType::Const {
                ret_type = BasicType::Int;
            }
            if ret_type != ret {
                node.wrong(format!("Return type of {} does not match", name));
            }
            Node {
                start: node.start,
                end: node.end,
                ntype: Return(new_expr),
                btype: BasicType::Nil,
            }
        }
        Func(ret, name, args, body) => {
            env.set_curr_func(name, ret);
            let mut new_args = vec![];
            env.start_scope();
            for arg in args {
                new_args.push(walk(arg, env));
            }
            env.insert(
                name.clone(),
                BasicType::Func(Box::new(ret.clone())),
                Node::new(NodeType::Func(
                    ret.clone(),
                    name.clone(),
                    new_args.clone(),
                    body.clone(),
                )),
            );
            let new_body = walk(body, env);
            env.end_scope();
            Node {
                start: node.start,
                end: node.end,
                ntype: Func(ret.clone(), name.clone(), new_args, Box::new(new_body)),
                btype: BasicType::Nil,
            }
        }
        _ => unreachable!(),
    }
}

fn expand_inits(
    dims: &Vec<Node>,
    inits: &Vec<Node>,
    need_eval: bool,
    env: &mut Env,
    level: usize,
) -> Vec<Node> {
    if level == dims.len() {
        inits
            .last()
            .unwrap()
            .wrong(format!("Dimension of initializer exceeded"));
    }
    let mut max = 1;
    for dim_node in dims.get(level..).unwrap() {
        if let NodeType::Number(dim) = dim_node.ntype {
            max *= dim;
        }
    }
    let mut expanded = vec![];
    for init_node in inits {
        if let NodeType::InitList(inits2) = &init_node.ntype {
            for new_init in expand_inits(dims, &inits2, need_eval, env, level + 1) {
                expanded.push(new_init);
            }
        } else {
            let new_init = if need_eval {
                Node {
                    start: init_node.start,
                    end: init_node.end,
                    ntype: NodeType::Number(eval(init_node, env)),
                    btype: BasicType::Const,
                }
            } else {
                let ini = walk(init_node, env);
                ini
            };
            expanded.push(new_init);
        }
    }
    if expanded.len() > max as usize {
        inits
            .last()
            .unwrap()
            .wrong(format!("Length of initializer exceeded"));
    } else {
        for _ in expanded.len()..(max as usize) {
            expanded.push(Node {
                start: 0,
                end: 0,
                ntype: NodeType::Number(0),
                btype: BasicType::Const,
            });
        }
    }
    expanded
}

fn eval(node: &Node, env: &Env) -> i32 {
    impl TokenType {
        fn calc(&self, lhs: i32, rhs: i32) -> i32 {
            use TokenType::*;
            match self {
                Plus => lhs + rhs,
                Minus => lhs - rhs,
                Multiply => lhs * rhs,
                Divide => lhs / rhs,
                Modulus => lhs % rhs,
                Equal => (lhs == rhs) as i32,
                NotEqual => (lhs != rhs) as i32,
                LesserThan => (lhs < rhs) as i32,
                GreaterThan => (lhs > rhs) as i32,
                LesserEqual => (lhs <= rhs) as i32,
                GreaterEqual => (lhs >= rhs) as i32,
                And => (lhs != 0 && rhs != 0) as i32,
                Or => (lhs != 0 || rhs != 0) as i32,
                _ => unreachable!(),
            }
        }
    }
    use NodeType::*;
    match &node.ntype {
        Nil => return 0,
        Call(name, _, _) => {
            node.wrong(format!(
                "Cannot call function {} in constant expression",
                name
            ));
            unreachable!()
        }
        Number(num) => num.clone(),
        BinOp(ttype, lhs, rhs) => {
            let l = eval(&lhs, env);
            let r = eval(&rhs, env);
            ttype.calc(l, r)
        }
        Access(name, indexes, _) => {
            let (btype, def_node) = env.find(&name,node);
            match btype {
                BasicType::Const => {
                    //Access a const with index
                    if indexes.is_some() {
                        node.wrong(format!("Access constant {} with index", name));
                    }
                    if let NodeType::Declare(_, _, _, initlist, _) = def_node.ntype.clone() {
                        if let NodeType::Number(num) = initlist.unwrap()[0].ntype {
                            return num;
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
                BasicType::ConstArray(dims) => {
                    if let Some(index) = indexes {
                        if index.len() == dims.len() {
                            let mut offset = 0;
                            for (i, indexnode) in index.iter().enumerate() {
                                let id = eval(indexnode, env);
                                if let Some(n) = dims.get(i + 1) {
                                    offset += id * (*n as i32);
                                } else {
                                    offset += id;
                                }
                            }
                            if let NodeType::Declare(_, _, _, initlist, _) = node.ntype.clone() {
                                if let Some(n) = initlist.unwrap().get(offset as usize) {
                                    if let NodeType::Number(num) = n.ntype {
                                        return num;
                                    } else {
                                        unreachable!()
                                    }
                                } else {
                                    node.wrong(format!("Index of {} out of range", name));
                                    unreachable!()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            node.wrong(format!(
                                "Dimension of {} should be {} instead of {}",
                                name,
                                dims.len(),
                                index.len()
                            ));
                            unreachable!()
                        }
                    } else {
                        node.wrong(format!("{} should be accessed with index", name));
                        unreachable!()
                    }
                }
                BasicType::Int | BasicType::IntArray(_) => {
                    node.wrong(format!("{} should be a constant", name));
                    unreachable!()
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

impl Node {
    fn wrong(&self, msg: String) {
        let path=unsafe {Path::new(&FILEPATH)};
        let mut code=String::new();
        File::open(path).expect("failed to read code").read_to_string(&mut code).expect("read code to string failed");

        let code_chars:Vec<char> =code.chars().collect();
        let mut line_start=self.start;
        while line_start!=0&&code_chars[line_start]!='\n'{
            line_start-=1;
        }
        let mut line_end=self.end;
        while line_end!=code.len()&&code_chars[line_end]!='\n'{
            line_end+=1;
        }

        let mut start_line=1;
        let mut index=0;
        while index!=line_start{
            if code_chars[index]=='\n'{
                start_line+=1;
            }
            index+=1;
        }

        let code_lines=code[line_start..line_end].to_string();
        let mut sign_lines=String::new();
        for i in line_start..line_end{
            if code_chars[i]=='\n'{
                sign_lines.push('\n');
                continue;
            }
            if self.start<=i&&i<self.end{
                sign_lines.push('^');
            }else{
                sign_lines.push(' ');
            }
        }
        //Error message
        println!("{}: {}","sementic error".red().bold(),msg.bold());
        println!("  {} {}:{}:{}","-->".blue().bold(),path.display(),start_line+1,self.start-line_start);
        for (i,(code_line,sign_line)) in code_lines.split('\n').into_iter().zip(sign_lines.split('\n').into_iter()).enumerate(){
            if code_line.trim().is_empty(){
                continue;
            }
            println!("     {}","|".blue().bold());
            println!("  {3:3}{2} {}\n     {2} {}\n",code_line,sign_line.red().bold(),"|".blue().bold(),(start_line+i).to_string().blue().bold());
        }
        panic!("{}", msg);
    }
}
