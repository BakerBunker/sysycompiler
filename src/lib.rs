use parse::Node;

pub mod asm;
pub mod ir;
pub mod lexical;
pub mod parse;
pub mod semantic;
pub mod utils;

pub fn builtin_funcs() -> Vec<Node> {
    impl Node {
        fn null() -> Self {
            Node::new(NodeType::Nil)
        }
    }
    use BasicType::*;
    use NodeType::Func;
    vec![
        Node::new(Func(Int, "getint".into(), vec![], Box::new(Node::null()))),
        Node::new(Func(Int, "getch".into(), vec![], Box::new(Node::null()))),
        Node::new(Func(
            Int,
            "getarray".into(),
            vec![Node::new(NodeType::Declare(
                IntArray(vec![0]),
                "a".into(),
                Some(vec![Node::new(NodeType::Nil)]),
                None,
                Scope::Local,
            ))],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "putint".into(),
            vec![Node::new(NodeType::Declare(
                Int,
                "n".into(),
                None,
                None,
                Scope::Local,
            ))],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "putch".into(),
            vec![Node::new(NodeType::Declare(
                Int,
                "c".into(),
                None,
                None,
                Scope::Local,
            ))],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "putarray".into(),
            vec![
                Node::new(NodeType::Declare(Int, "n".into(), None, None, Scope::Local)),
                Node::new(NodeType::Declare(
                    IntArray(vec![0]),
                    "a".into(),
                    Some(vec![Node::new(NodeType::Nil)]),
                    None,
                    Scope::Local,
                )),
            ],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "starttime".into(),
            vec![],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "stoptime".into(),
            vec![],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "_sysy_starttime".into(),
            vec![Node::new(NodeType::Declare(
                Int,
                "lineno".into(),
                None,
                None,
                Scope::Local,
            ))],
            Box::new(Node::null()),
        )),
        Node::new(Func(
            Void,
            "_sysy_stoptime".into(),
            vec![Node::new(NodeType::Declare(
                Int,
                "lineno".into(),
                None,
                None,
                Scope::Local,
            ))],
            Box::new(Node::null()),
        )),
    ]
}

//Tokens
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    //Literals
    Number(i32),
    Ident(String),

    //Keywords
    /// "int"
    Int,
    /// "void"
    Void,
    /// "const"
    Const,
    /// "if"
    If,
    ///"Else"
    Else,
    /// "while"
    While,
    /// "break"
    Break,
    /// "continue"
    Continue,
    /// "return"
    Return,

    //Arithmetical operators
    /// =
    Assign,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Multiply,
    /// /
    Divide,
    /// %
    Modulus,

    //Relational operators
    /// ==
    Equal,
    /// !=
    NotEqual,
    /// <
    LesserThan,
    /// >
    GreaterThan,
    /// <=
    LesserEqual,
    /// >=
    GreaterEqual,

    //Logical operators
    /// !
    Not,
    /// &&
    And,
    /// ||
    Or,

    //symbols
    /// ;
    Semicolon,
    /// ,
    Comma,
    /// (
    LeftParen,
    /// )
    RightParen,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
    /// {
    LeftBrace,
    /// }
    RightBrace,
}

//Nodes
#[derive(Clone)]
pub enum NodeType {
    Nil,
    //Expressions
    ///Value
    Number(i32),

    ///#### Value declaration
    ///Type,Name,Dimensions,Init list,Scope
    Declare(
        BasicType,
        String,
        Option<Vec<Node>>,
        Option<Vec<Node>>,
        Scope,
    ),
    ///#### Initialization list
    ///List of `Expr` or `InitList`
    InitList(Vec<Node>),

    ///#### Value access
    ///Array name,Indexes,Decl
    Access(String, Option<Vec<Node>>, Box<Node>),

    ///#### Binary operation
    ///Op,Lhs,Rhs
    BinOp(TokenType, Box<Node>, Box<Node>),

    ///#### Function call
    ///Name,Args,Func_decl
    Call(String, Vec<Node>, Box<Node>),

    //Statements
    ///#### Declaration sequence
    ///List of `Declare`
    DeclStmt(Vec<Node>),

    ///Name,Indexes,Expr,Lhs_decl
    Assign(String, Option<Vec<Node>>, Box<Node>, Box<Node>),

    ///Expression
    ExprStmt(Box<Node>),

    ///List of statement
    Block(Vec<Node>),

    ///Condition,If block,Else block
    If(Box<Node>, Box<Node>, Option<Box<Node>>),

    ///Condition,Body
    While(Box<Node>, Box<Node>),
    Break,
    Continue,
    ///Return value
    Return(Option<Box<Node>>),
    ///#### Function definition
    ///Return type,Name,Args(List of declares),Body
    Func(BasicType, String, Vec<Node>, Box<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BasicType {
    Nil,
    Int,
    Const,
    Void,
    IntArray(Vec<usize>),
    ConstArray(Vec<usize>),
    Func(Box<BasicType>), //return type
}
#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Local,
    Global,
    Param,
}
