# SysYcompiler使用文档

## 介绍

SysYcompiler 是一个用于编译SysY语言(一个C语言的子集)的编译器，目标汇编语言是MIPS，并使用MARS进行模拟。本编译器使用Rust进行开发，是编译原理试点班中要求的作业作品。

## 编译与运行

### 所需环境

- Rust (edition2018+)
- LLVM (用于编译并测试生成的LLVM IR)
- MARS (用于测试MIPS生成的汇编语言)

### 编译

进入项目根目录并运行：

```shell
cargo build
```

即可生成debug版本的二进制文件，生成的二进制文件会在`./target/debug/sysycompiler`

进入项目根目录并运行：

```shell
cargo build --release
```

即可生成release版本的二进制文件，生成的二进制文件会在`./target/release/sysycompiler`

### 运行

运行二进制文件即可将SysY文件编译为MIPS汇编文件

```shell
./target/debug/sysycompiler --asm test.sy
```

生成的test.s会默认保存在`./output/test.s`

编译器还提供其它选项，`./target/debug/sysycompiler --help`可以进行查看，具体内容如下

```
SysY Compiler 1.0
bakerbunker@nwpu.edu.cn
Compile SysY file to LLVM IR or MIPS assembly

USAGE:
    sysycompiler [FLAGS] [OPTIONS] <INPUT>

ARGS:
    <INPUT>    SysY file

FLAGS:
        --asm         Print mips asm to [output_dir]/[filename].asm
        --test_asm    Generate program with MIPS assembly and output test result (Needs Mars-a MIPS
                      simulator)
        --ast         Print ast tree to [output_dir]/[filename].ast
        --cfg         Simplify cfg
        --dce         Dead code elimination
        --ir          Output LLVM IR to [output_dir]/[filename].ir
        --test_ir     Generate program with LLVM IR and output test result
        --semantic    Print optimized ast tree to [output_dir]/[filename].optim_ast
        --token       Print tokens to [output_dir]/[filename].tokens
    -h, --help        Prints help information
    -V, --version     Prints version information

OPTIONS:
        --mars <MARS>            Path of mars [default: /usr/share/java/mars-mips/Mars.jar]
        --output_dir <OUTPUT>    Set the path of output file [default: ./output]
```



#### 参数含义

| 参数       | 含义                                                         |
| ---------- | ------------------------------------------------------------ |
| --token    | 输出Token到[output_dir]/[filename].tokens                    |
| --ast      | 输出AST到[output_dir]/[filename].ast                         |
| --semantic | 输出语法分析后的到[output_dir]/[filename].optim_ast          |
| --ir       | 输出LLVM IR到[output_dir]/[filename].ir                      |
| --test_ir  | 使用llvm测试LLVM IR的运行结果，结果将会输出到 test_ir，会使用[filename].in作为默认输入 |
| --cfg      | 简化控制流图                                                 |
| --dce      | 死代码消除                                                   |
| --asm      | [output_dir]/[filename].s                                    |
| --test_asm | 使用mars测试MIPS汇编的运行结果，结果将会输出到 test_ast，会使用[filename].in作为默认输入 |
| --mars     | mars.jar的路径                                               |
| --output   | 设置输出文件夹                                               |

<div STYLE="page-break-after: always;"></div>


### SysY语言定义与运行时库

包含在`sysyruntime`文件夹中，其中`sylib.s`是手写的系统库，官方的定义如下

- [SysY语言定义](https://gitlab.eduxiji.net/nscscc/compiler2021/-/blob/master/SysY%E8%AF%AD%E8%A8%80%E5%AE%9A%E4%B9%89.pdf)
- [SysY运行时库介绍](https://gitlab.eduxiji.net/nscscc/compiler2021/-/blob/master/SysY%E8%BF%90%E8%A1%8C%E6%97%B6%E5%BA%93.pdf)
- [SysY运行时库地址](https://gitlab.eduxiji.net/windcome/sysyruntimelibrary)

### SysY语言测试用例

使用的是[大赛](https://compiler.educg.net/)官网的[测试用例](https://gitlab.eduxiji.net/windcome/sysyruntimelibrary/-/tree/master/section1)

### 辅助工具

辅助工具在`utils`文件夹下，分别为:

#### add_line.py

由于Mars在读入数字时读入到空格会导致错误，所以该脚本可以将测试样例中的空格替换为换行符，避免Mars出现错误

#### clean.sh

会将output中的文件全部删除

#### test.sh

会测试`test`文件夹中的所有`*.sy`，但不会生成任何文件

#### testir.sh

会将`test`文件夹中的所有`*.sy`编译并生成LLVM IR并测试其结果

#### testasm.sh

会将`test`文件夹中的所有`*.sy`编译并生成MIPS汇编并测试其结果

## 编译器各部分

本部分将用一段简短的程序来作为案例，程序如下：

```c
int globl=0;

int func(int a){
    return 1;
}

int main(){
    int c[30][30];
    int b;
    int a=1;
    {

        int a;
        a = 3;
        b = a;
    }
    {

        int a;
        a = 2;
        b = a;
    }
    return c[a][b]+a;
}
```



### 词法分析

采用状态机进行判断并将输入的字符分隔为Token，并记录每个Token的位置信息和其它信息，方便后面的语法分析和错误显示，词法分析后Token流的信息如下：

```
TokenNo:0
Token{
	type:Int
	content: int 
	start:0
	end:3
	lineno:1
}
TokenNo:1
Token{
	type:Ident("globl")
	content: globl 
	start:4
	end:9
	lineno:1
}
TokenNo:2
Token{
	type:Assign
	content: = 
	start:9
	end:10
	lineno:1
}
TokenNo:3
Token{
	type:Number(0)
	content: 0 
	start:10
	end:11
	lineno:1
}
TokenNo:4
Token{
	type:Semicolon
	content: ; 
	start:11
	end:12
	lineno:1
}
TokenNo:5
Token{
	type:Int
	content: int 
	start:0
	end:3
	lineno:3
}
......
```



### 语法分析

采用LL(1)文法，使用递归下降的方法进行语法分析，将词法分析器中输入的Token流转化为AST，其中代码是根据官方文法进行编写的，官方文法如下：

> 编译单元	 CompUnit  → [ CompUnit ] ( Decl | FuncDef ) 
> 声明 			Decl  → ConstDecl | VarDecl 
> 常量声明 	ConstDecl  → 'const' BType ConstDef { ',' ConstDef } ';' 
> 基本类型 	BType  → 'int' 
> 常数定义 	ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal 
> 常量初值 	ConstInitVal → ConstExp   
> 												| '{' [ ConstInitVal { ',' ConstInitVal } ] '}' 
> 变量声明 	VarDecl  → BType VarDef { ',' VarDef } ';' 
> 变量定义	 VarDef  → Ident { '[' ConstExp ']' } 
> 										| Ident { '[' ConstExp ']' } '=' InitVal   
> 变量初值 	InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}' 
> 函数定义 	FuncDef  → FuncType Ident '(' [FuncFParams] ')' Block 
> 函数类型 	FuncType  → 'void' | 'int' 
> 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam } 
> 函数形参 	FuncFParam  → BType Ident ['[' ']' { '[' Exp ']' }] 
> 语句块 	Block  → '{' { BlockItem } '}' 
> 语句块项 BlockItem  → Decl | Stmt 
> 语句 		Stmt  → LVal '=' Exp ';' | [Exp] ';'    | Block  
> 								| 'if' '( Cond ')' Stmt [ 'else' Stmt ] 
> 								| 'while' '(' Cond ')' Stmt 
> 								| 'break' ';'    | 'continue' ';' 
> 								| 'return' [Exp] ';' 
> 表达式 	Exp  → AddExp      注：SysY 表达式是int 型表达式
> 条件表达式 Cond  → LOrExp    
> 左值表达式 LVal  → Ident {'[' Exp ']'} 
> 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number 
> 数值 		Number → IntConst 
>
> 一元表达式 UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')'   
> 											| UnaryOp UnaryExp 
> 单目运算符 UnaryOp  → '+' | '−' | '!'        注：'!'仅出现在条件表达式中
> 函数实参表 FuncRParams → Exp { ',' Exp } 
> 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp 
> 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp 
> 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
> 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp 
> 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp 
> 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp 
> 常量表达式 ConstExp → AddExp          注：使用的Ident 必须是常量

经过语法分析后的AST表示如下：

```
|DeclStmt
|--Declare of globl(Int) in Global scope
|----Number 0
|Func func,returns Int
|--Declare of a(Int) in Param scope
|--Block
|----Return
|------Number 1
|Func main,returns Int
|--Block
|----DeclStmt
|------Declare of b(Int) in Local scope
|----DeclStmt
|------Declare of a(Int) in Local scope
|--------Number 1
|----Block
|------DeclStmt
|--------Declare of a(Int) in Local scope
|------Assign a
|--------Number 3
|------Assign b
|--------Access a
|----Block
|------DeclStmt
|--------Declare of a(Int) in Local scope
|------Assign a
|--------Number 2
|------Assign b
|--------Access a
|----Return
|------Binop Plus
|--------Access a
|--------Access b

```

### 语义分析

此阶段将会检查代码中符合语法，但不符合语义的片段，例如：

```
sementic error: Dimension of initializer exceeded
  --> test/test.sy:8:27
     |
  8  |     int k[3] = {1, 2, {3, 4}};
     |                           ^   
```

此阶段检查的错误有如下几种：

- 变量重复定义
- 使用未定义的变量
- 在循环之外使用`break`/`continue`
- 数组定义时下标非法(非常数、小于等于0等)
- 数组定义时初始化值非法(非常数、维度过大等)
- 数组下标越界
- 赋值语句左值非法(非变量/常数、维度错误等)
- 赋值语句右值类型错误(是数组)
- 函数调用参数列表过长、类型不匹配
- 函数返回类型错误
- `if`/`while`语句中条件类型错误

本阶段同时会将定义的常数在AST中替换，并尝试计算出能够计算出的值

### 中间代码

在本阶段会将输入的AST生成IR，IR使用了LLVM IR的格式，最后可以生成LLVM IR的文本形式如下：

```
@globl = global i32 0
declare i32 @getint()
declare i32 @getch()
declare i32 @getarray(i32*)
declare void @putint(i32)
declare void @putch(i32)
declare void @putarray(i32, i32*)
declare void @starttime()
declare void @stoptime()
declare void @_sysy_starttime(i32)
declare void @_sysy_stoptime(i32)
define i32 @func(i32 %a){
bb1:		;entry pred=[] next=[]
    %a.addr0 = alloca i32
    store i32 %a, i32* %a.addr0
    ret i32 1
}
define i32 @main(){
bb1:		;entry pred=[] next=[]
    %c.addr0 = alloca [ 30 x [ 30 x i32 ] ]
    %b.addr0 = alloca i32
    %a.addr0 = alloca i32
    store i32 1, i32* %a.addr0
    %a.addr1 = alloca i32
    store i32 3, i32* %a.addr1
    %t1 = load i32, i32* %a.addr1
    store i32 %t1, i32* %b.addr0
    %a.addr2 = alloca i32
    store i32 2, i32* %a.addr2
    %t2 = load i32, i32* %a.addr2
    store i32 %t2, i32* %b.addr0
    %t3 = load i32, i32* %a.addr0
    %t4 = getelementptr [ 30 x [ 30 x i32 ] ], [ 30 x [ 30 x i32 ] ]* %c.addr0,i32 0, i32 %t3
    %t5 = load i32, i32* %b.addr0
    %t6 = getelementptr [ 30 x i32 ], [ 30 x i32 ]* %t4,i32 0, i32 %t5
    %t7 = load i32, i32* %t6
    %t8 = load i32, i32* %a.addr0
    %t9 = add i32 %t7, %t8 
    ret i32 %t9
}
```

此后可以进行两个优化的Pass

- 死代码消除
- 简化控制流图

经过死代码消除之后的LLVM IR如下：

```
@globl = global i32 0
declare i32 @getint()
declare i32 @getch()
declare i32 @getarray(i32*)
declare void @putint(i32)
declare void @putch(i32)
declare void @putarray(i32, i32*)
declare void @starttime()
declare void @stoptime()
declare void @_sysy_starttime(i32)
declare void @_sysy_stoptime(i32)
define i32 @func(i32 %a){
bb1:		;entry pred=[] next=[]
    ret i32 1
}
define i32 @main(){
bb1:		;entry pred=[] next=[]
    %c.addr0 = alloca [ 30 x [ 30 x i32 ] ]
    %b.addr0 = alloca i32
    %a.addr0 = alloca i32
    store i32 1, i32* %a.addr0
    %a.addr1 = alloca i32
    store i32 2, i32* %a.addr1
    %t1 = load i32, i32* %a.addr1
    store i32 %t1, i32* %b.addr0
    %t2 = load i32, i32* %a.addr0
    %t3 = getelementptr [ 30 x [ 30 x i32 ] ], [ 30 x [ 30 x i32 ] ]* %c.addr0,i32 0, i32 %t2
    %t4 = load i32, i32* %b.addr0
    %t5 = getelementptr [ 30 x i32 ], [ 30 x i32 ]* %t3,i32 0, i32 %t4
    %t6 = load i32, i32* %t5
    %t7 = load i32, i32* %a.addr0
    %t8 = add i32 %t6, %t7 
    ret i32 %t8
}
```

### 汇编生成

该阶段会将上一阶段输入的LLVM IR代码生成MIPS汇编代码，其中会进行寄存器的分配，生成后的汇编代码如下：

```assembly
.data
globl:
  .word 0
.text
.globl main
func:
  addiu $sp, $sp, -44
  sw $ra,40($sp)
  sw $fp,36($sp)
  sw $a0,0($sp)
  li $v0, 1
  lw $ra, 40($sp)
  lw $fp, 36($sp)
  addiu $sp, $sp, 44
  jr $ra
main:
  addiu $sp, $sp, -56
  sw $ra,52($sp)
  sw $fp,48($sp)
  sw $s0,44($sp)
  sw $s1,40($sp)
  sw $s2,36($sp)
  li $t0, 1
  sw $t0,8($sp)
  li $t0, 3
  sw $t0,4($sp)
  lw $s0, 4($sp)
  sw $s0,12($sp)
  li $t0, 2
  sw $t0,0($sp)
  lw $s0, 0($sp)
  sw $s0,12($sp)
  lw $s0, 8($sp)
  lw $s1, 12($sp)
  add $s2, $s0, $s1
  move $v0, $s2
  lw $ra, 52($sp)
  lw $fp, 48($sp)
  lw $s0, 44($sp)
  lw $s1, 40($sp)
  lw $s2, 36($sp)
  addiu $sp, $sp, 56
  move $a0, $v0
  li $v0, 17
  syscall
  jr $ra

getint:
  li $v0,5
  syscall
  jr $ra
  
getch:
  li $v0,12
  syscall
  jr $ra
  
putint:
  li $v0,1
  syscall
  jr $ra
  
putch:
  li $v0,11
  syscall
  jr $ra
  
getarray:
  jal getint
  move $t0,$v0
  move $t1,$zero
  j $getarray_cond
$getarray_cond:
  bne $t1,$t0,$getarray_body
  beq $t1,$t0,$getarray_end
$getarray_body:
  jal getint
  mul $t2,$t1,4
  add $t3,$a0,$t2
  sw $v0,($t3)
  add $t1,$t1,1
  j $getarray_cond
$getarray_end:
  move $v0,$t0
  jr $ra
  
putarray:
  li $v0,1
  syscall
  move $t0,$a0
  move $t1,$zero
  j $putarray_cond
$putarray_cond:
  bne $t1,$t0,$putarray_body
  beq $t1,$t0,$putarray_end
$putarray_body:
  mul $t2,$t1,4
  add $t3,$a1,$t2
  lw $a0,($t3)
  li $v0,1
  syscall
  add $t1,$t1,1
  j $putarray_cond
$putarray_end:
  jr $ra
  
starttime:
  jr $ra

stoptime:
  jr $ra
```

