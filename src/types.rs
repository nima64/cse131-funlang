/** Type Checker **/ 

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo{
    Num,
    Bool,
    Nothing,
    Any
}

// T ≤ T
pub fn is_subtype(t1: &TypeInfo, t2: &TypeInfo) -> bool {
    match (t1, t2) {
        (TypeInfo::Num,     TypeInfo::Num)
        | (TypeInfo::Bool,  TypeInfo::Bool)
        | (TypeInfo::Any,   TypeInfo::Any)
        | (TypeInfo::Nothing, TypeInfo::Nothing) => true,

        (_, TypeInfo::Any) => true,
        (TypeInfo::Nothing, _) => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum ExprT {
    Number(i64, TypeInfo),
    Boolean(bool, TypeInfo),
    Let(Vec<(String, ExprT)>, Box<ExprT>, TypeInfo),
    Id(String, TypeInfo),
    UnOp(Op1, Box<ExprT>, TypeInfo),
    Define(String, Box<ExprT>, TypeInfo),
    Block(Vec<ExprT>, TypeInfo),
    BinOp(Op2, Box<ExprT>, Box<ExprT>, TypeInfo),
    If(Box<ExprT>, Box<ExprT>, Box<ExprT>, TypeInfo),
    Loop(Box<ExprT>, TypeInfo),
    Break(Box<ExprT>, TypeInfo),
    Set(String, Box<ExprT>, TypeInfo),
    FunCall(String, Vec<ExprT>, TypeInfo),
    Print(Box<ExprT>, TypeInfo),
    Cast(TypeInfo, Box<ExprT>, TypeInfo)
}

impl ExprT {
    pub fn get_type_info(&self) -> &TypeInfo {
        match self {
            ExprT::Number(_, ti) => ti,
            ExprT::Boolean(_, ti) => ti,
            ExprT::Let(_, _, ti) => ti,
            ExprT::Id(_, ti) => ti,
            ExprT::UnOp(_, _, ti) => ti,
            ExprT::Define(_, _, ti) => ti,
            ExprT::Block(_, ti) => ti,
            ExprT::BinOp(_, _, _, ti) => ti,
            ExprT::If(_, _, _, ti) => ti,
            ExprT::Loop(_, ti) => ti,
            ExprT::Break(_, ti) => ti,
            ExprT::Set(_, _, ti) => ti,
            ExprT::FunCall(_, _, ti) => ti,
            ExprT::Print(_, ti) => ti,
            ExprT::Cast(_, _, ti) => ti,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Rax = 0,
    Rbx = 3,
    Rcx = 1,
    Rdx = 2,
    Rsi = 6,
    Rdi = 7,
    Rsp = 4,
    Rbp = 5,
}

impl Reg {
    pub fn to_num(&self) -> u8 {
        *self as u8
    }
}

#[derive(Debug, Clone)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug, Clone)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
}

// Re-export constants from common module
pub use crate::common::{
    TRUE_TAGGED, FALSE_TAGGED, BOOL_TAG, 
    get_tag, tag_number, untag_number, format_result, parse_input,
};

#[derive(Debug, Clone)]
pub enum Expr {
    // Num(i64),
    Number(i64),
    Boolean(bool),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),
    Define(String, Box<Expr>),
    Block(Vec<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    FunCall(String, Vec<Expr>),
    Print(Box<Expr>),
    Cast(TypeInfo, Box<Expr>)

}

#[derive(Debug, Clone)]
pub struct Defn {
    pub name: String,
    pub params: Vec<(String, Option<TypeInfo>)>, // (name, optional type annotation)
    pub return_type: Option<TypeInfo>, // optional return type annotation
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Prog {
    pub defns: Vec<Defn>,
    pub main: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Condition {
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    Overflow,
    Uncond,
}

pub enum RuntimeErr {
    TypeMismatch,
    Overflow,
    ArithmeticType,
    BadCast,
}

impl ToString for RuntimeErr {
    fn to_string(&self) -> String {
        match self {
            RuntimeErr::TypeMismatch => "type_mismatch_error".to_string(),
            RuntimeErr::Overflow => "overflow_error".to_string(),
            RuntimeErr::ArithmeticType => "type_error_arithmetic".to_string(),
            RuntimeErr::BadCast => "bad_cast_error".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    // IMov(Reg, RegOrImm),
    Comment(String),
    Mov(Reg, i64), // mov register, immediate
    Add(Reg, i32), // add register, immediate
    Sub(Reg, i32), // sub register, immediate
    IMul(Reg, Reg),
    AddReg(Reg, Reg),
    MinusReg(Reg, Reg),
    MovReg(Reg, Reg),
    XorReg(Reg, Reg),
    Push(Reg),            // push reg - push register onto stack
    Pop(Reg),             // pop reg - pop from stack into register
    MovToStack(Reg, i32), // Register, stackdepth => mov [rsp - offset], register
    MovFromStack(Reg, i32), // mov register, [rsp - offset]
    MovDeref(Reg, Reg),   // mov dest_reg, [src_reg] - dereference src_reg and put in dest_reg
    MovToMem(Reg, Reg),   // mov [dest_reg], src_reg - store src_reg value to memory at address in dest_reg
    Cmp(Reg, Reg),        // cmp reg1, reg2
    CmpImm(Reg, i64),     // cmp reg, immediate
    SetL(Reg),            // setl reg - set if less
    SetG(Reg),            // setg reg - set if greater
    SetLE(Reg),           // setle reg - set if less or equal
    SetGE(Reg),           // setge reg - set if greater or equal
    SetEq(Reg),           // sete reg - set if equal
    Shl(Reg, i32),        // shl reg, immediate - shift left
    Sar(Reg, i32),        // sar reg, immediate - shift arithmetic right
    Or(Reg, i64),         // or reg, immediate - for setting bits
    Test(Reg, i64),       // test reg, immediate - for checking bits
    Call(String),         // call label - call function
    Ret,                  // ret - return from function
    Label(String),        // label: - assembly label
    CMov(Condition, Reg, Reg), // conditional move
    Jump(Condition, String),
}

// Helper to check if a value is a number (tag bit is 0)
pub fn is_number_tag(val: i64) -> bool {
    (val & 1) == 0
}

// Helper to check if a value is a boolean (tag bit is 1)
pub fn is_bool_tag(val: i64) -> bool {
    (val & 1) == 1
}