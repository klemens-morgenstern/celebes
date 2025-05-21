grammar celebes;

Import: 'import';
Include : 'include';
Namespace :'namespace';
Par_open : '(';
Par_close : ')';
Comma : ',';
SemiColon : ';';
Curly_open : '{';
Curly_close : '}';
Attr_open : '[[';
Attr_close : ']]';
Square_open : '[';
Square_close : ']';
Void : 'void';

Inc : '++';
Dec : '--';

Add_assign : '+=';
Sub_assign : '-=';
Mul_assign : '*=';
Div_assign : '/=';
Mod_assign : '%=';
And_assign : '&=';
 Or_assign : '|=';
Xor_assign : '^=';
Lsh_assign : '<<=';
Rsh_assign : '>>=';
Sfl_assign : '<~>=';
Arrow : '=>';

Dot_ref : '.*';
Drf : '->';
Opt : '?.';
Plus  : '+';
Minus : '-';
Mul : '*';
Div : '/';
Mod : '%';
And : '&';
 Or : '|';
Xor : '^';
Not : '~';
Lsh : '<<';
Rsh : '>>';

Sfl : '<~>';

Cc : '$';

Equal : '==';
Not_equal : '!=';
Gt : '>';
Lt : '<';
Ge : '>:';
Le : '<:';
Cmp : '<:>';

Log_not : '!';
Log_and : '&&';
Log_or  : '||';
Log_xor : '^^';
Qm : '?';
At : '@';
Variadic: '...';
Rng : '..';
Colon : ':';
Dot : '.';
Str : '#';

Assign : '=';

Await:    'await';
Bool:     'bool';
Break:    'break';
Case:     'case';
Yield:    'yield';
Const:    'const';
Goto:     'goto';
Volatile: 'volatile';
Atomic:   'atomic';
Macro:    'macro';
Mutable:  'mutable';
Module:   'module';
Thread_local : 'thread_local';
Default : 'default';
Explicit : 'explicit';
Else     : 'else';
Enum     : 'enum';
Extern   : 'extern';
Expr_    : 'expr';
For   : 'for';
Final : 'final';
If   : 'if';
In   : 'in';
Inline : 'inline';
Is   : 'is';
Len  : 'len';
Let  : 'let';
Little : 'little';
Big : 'big';
Interface : 'interface';
Sizeof : 'sizeof';
Alignof : 'alignof';
Alignas : 'alignas';
Operator : 'operator';
Register : 'register';
Partial : 'partial';
Move : 'move';
New : 'new';
Nothrow : 'nothrow';
Noreturn : 'noreturn';
Null : 'null';
Return : 'return';
Struct : 'struct';
Switch : 'switch';
True : 'true';
Throw : 'throw';
False :'false';
Fast : 'fast';
Safe : 'safe';
Nan : 'nan';
Then : 'then';
Type : 'type';
This : 'this';
Var : 'var';
Try : 'try';
Union : 'union';
Catch : 'catch';
While : 'while';
Class : 'class';
Small : 'small';

// char c : "a";
PlaceHolder: '\\' [0-9]+ ;
Builtin:     '\\' [a-z][0-9a-z]* ;

EmptyString   : '""' ;
StringLiteral : '"'  (~["\\] | [\\]. )* '"' ;
CharLiteral   : '\'' (~['\\] | [\\]. )* '\'' ;

// this is incorrect. it will be easier to override this after gen, then mucking about here.
RawStringLiteral: [Rr]'"(' ~[)"]*  ')"';

InterpolationStringSingle : [`] ~[`{}]* [`];
InterpolationStringStart :  [`] ~[`{}]* '${';
InterpolationStringMid   :  [}] ~[`{}]* '${';
InterpolationStringEnd   :  [}] ~[`{}]* '`';

// helper, so 1..3

fragment Exponent : [eE][+-]?['0-9]+ ;
fragment Fp1 : [0-9]['0-9]* '.' ['0-9]+ Exponent?;
fragment Fp2 : [1-9]['0-9]* Exponent ; // This seems to work with flex, as there's potential ambiguity with 1..2

FloatingPoint :  Fp1 | Fp2 ;

fragment IntNull: [0];
fragment IntDec : [1-9]['0-9]* ;
fragment IntBin : [0][bB][01]['01]* ;
fragment IntOct : [0][oO][0-7]['0-7]* ;
fragment IntHex : [0][xX][0-9a-fA-F]['0-9a-fA-F]* ;

Int :  IntDec | IntBin | IntOct | IntHex  | IntNull;

FloatType :  'bf16' | 'f16' | 'f32' | 'f64' | 'f128' ;
IntType : [ui][0-9]+ ;

IdentifierRaw : [_a-z][_a-z0-9]* ;

Space: (' ' | '\r' | '\n' | '\t') -> skip;
LineComment: '//' ~[\n]* [\n] -> skip;
BlockComment: '/*' .*? '*/' -> skip;

Private  : 'private';
Public   : 'public';
Protected: 'protected';

root_rule: module_decl ? (include_statement | import_statement | namespace_content | SemiColon )* EOF ;

module_decl : Module Private? identifier SemiColon ;

namespace_ : namespace_definition | namespace_alias;
namespace_definition : Namespace scoped_identifier Curly_open namespace_content* Curly_close;
namespace_alias : Namespace scoped_identifier Assign scoped_identifier SemiColon;
namespace_content : namespace_ | global_statement;


include_statement : Include identifier? string SemiColon;
//include_op : include Par_open string (Comma string)? Par_close
import_statement : Import identifier? scoped_identifier SemiColon;


interpolation : InterpolationStringSingle
              | (InterpolationStringStart expr (InterpolationStringMid expr)* InterpolationStringEnd);


identifier : IdentifierRaw | CharLiteral | interpolation | Builtin ;

scoped_identifier : Dot? identifier (Dot identifier) * ;

floating_point_literal : FloatingPoint (identifier | FloatType)? ;
int_literal : Int (identifier | IntType )? ;
string : RawStringLiteral | CharLiteral | StringLiteral | EmptyString | interpolation | RawStringLiteral ;

literal : floating_point_literal | int_literal | string | True | False | Null ;


//invocation : absolute_identifier Par_open (( dot ? identifier assign) ? expression (Comma ( dot ? identifier assign) ? expression  )*)? Par_close

designator : Dot identifier Assign ;
struct_init : Curly_open designator? expr (Comma designator? expr)* Curly_close ;

//  ( designator? primary_expr ( Comma designator? primary_expr)* )? Curly_close

primary_expr : PlaceHolder | scoped_identifier | literal | string | ((Little|Big)? IntType) | FloatType | Bool | CharLiteral | Type
             | Square_open (expr (Comma expr)*)? Square_close | struct_init | Par_open expr Par_close | /*IntNull |*/ Variadic ;

argument_list : (identifier Colon)?  expr ((identifier Colon)? Comma expr)* ;

castable_qualifier : Mutable | Const | Volatile | Atomic | Fast | Safe
                     | Log_not Volatile | Log_not Fast | Log_not Safe
                     | Big | Little ;

qualifier : Const | non_const_qualifier ;
non_const_qualifier : Volatile | Atomic | Fast | Safe | Thread_local ;

postfix_expr :
             primary_expr
             (
                Square_open argument_list? Square_close
              | Par_open argument_list?    Par_close
              | Drf identifier
              | Inc
              | Dec
              | Qm
             ) *  ;

unary_operator : And | Mul | Plus | Minus | Not | Xor | Log_not | Await ;

unary_expr : (
            Inc            |
            Dec            |
            unary_operator |
            Sizeof         |
            Alignof        |
            Len            |
            Cc             |
            New            |
            Qm             |
            Throw          |
            Str            |
            Move           ) * postfix_expr  ;

cast_expr : (Par_open (expr | Void | castable_qualifier) Par_close)* unary_expr ;

mul_expr   : cast_expr ((Mul|Div|Mod) cast_expr)*    ;
add_expr   : mul_expr  ((Plus|Minus)  mul_expr)*     ;
shift_expr : add_expr  ((Lsh|Rsh)  add_expr)*        ;
rel_expr   : shift_expr ((Gt|Lt|Ge|Le) shift_expr)*  ;
eq_exp : rel_expr  ((Equal|Not_equal)  rel_expr)*    ;


// and_expr :
and_exp     : eq_exp      (And     eq_exp)*      ;
exc_exp     : and_exp     (Xor     and_exp)*     ;
inc_exp     : exc_exp     (Or      exc_exp)*     ;
log_and_exp : inc_exp     (Log_and inc_exp)*     ;
log_or_exp  : log_and_exp (Log_or  log_and_exp)* ;
rng_exp     : log_or_exp  (Rng     log_or_exp)*  ;
at_exp      : rng_exp     (At      rng_exp)*     ;

str_exp     : at_exp (Str at_exp)*    ;
sfl_exp     : str_exp (Sfl str_exp)*  ;

is_exp      : sfl_exp (Is (Nan | sfl_exp))*  ;
in_exp      : is_exp (In is_exp)*            ;
cond_expr   : is_exp ((Qm | Colon) is_exp) * ;

assign_op   : Add_assign | Sub_assign | Mul_assign | Div_assign | Mod_assign |
              And_assign | Or_assign  | Xor_assign |
              Lsh_assign | Rsh_assign | Sfl_assign | Assign  ;

//assign_expr : (cond_expr assign_op )* cond_expr
assign_expr : unary_expr assign_op assign_expr
              | cond_expr  ;

expr :  assign_expr | block_statement | lambda  ;

lambda : function_decl_tail Arrow expr  ;

block_statement : Curly_open statement* Curly_close ;

align_as : Alignas Par_open expr Par_close  ;

type_mod : Square_open (expr (Comma expr)* )? Square_close | Xor | And | Mul
         | Small (Qm?) Par_open expr Par_close | align_as | Qm  ;

type_decl_inner : (IntType | FloatType | Bool | Void | Let | Var | scoped_identifier (Par_open  argument_list? Par_close) ?
        | Type (Par_open expr Par_close) ?
        | struct_inline
        | interface_inline
        | enum_inline )  (Or type_decl_inner)*  ;

type_tuple :
        Square_open
            type_decl_inner ( Comma type_decl_inner)*
        Square_close  ;


type_decl :
        (qualifier*
        Cc ?
        (type_decl_inner | type_tuple )
        type_mod* qualifier*)  ;


tuple_bind  : Square_open (identifier (Comma identifier)*)? Square_close  ;
struct_bind : Curly_open  (identifier (Comma identifier)*)? Curly_close   ;

bind_decl : (qualifier | Let | Var)+
           (Xor | And | Mul)? Cc ? (struct_bind | tuple_bind) (Assign expr) ?
           (Comma (struct_bind | tuple_bind) (Assign expr)?)*  ;

var_decl  : bind_decl
            | type_decl ((Dot_ref type_decl) | Par_open (type_decl (Comma type_decl)*)? Par_close ) ?
              identifier (Assign expr )? (Comma identifier (Assign expr )?)* SemiColon ;


function_decl_args : type_decl Variadic? (identifier|This)? (Assign expr )?
                     (Comma type_decl Variadic ? identifier? (Assign expr )?)* ;
macro_decl_args    : Expr_? type_decl Variadic ? (identifier|This)? (Assign expr )?
                     (Comma Expr_? type_decl Variadic ? identifier? (Assign expr )?)* ;



//operator_decl : 'operator'

// int operator[](int, int);
// int operator..(int, int);
// operator type_decl();
op_decl : Operator (Qm|unary_operator|Inc|Dec|Len|Mul|Div|Mod|Plus|Str|Minus|Lsh|Rsh|Gt|Lt|Ge|Le|Equal|Not_equal|And|Xor|Or|Log_and|Log_or|Rng|In|Is|Qm|Colon|Move|assign_op|(EmptyString identifier))
        | Explicit? Operator Type

          ;

attribute : Attr_open (identifier (Par_open expr Par_close)? (Comma identifier (Par_open expr Par_close)?)*) ? Attr_close  ;
function_decl_spec : Nothrow? attribute*  ;

function_decl_tail : Par_open function_decl_args? Par_close  ;
function_decl : type_decl (op_decl | scoped_identifier (Dot op_decl )? ) function_decl_tail function_decl_spec ;
macro_def : Macro scoped_identifier Par_open macro_decl_args ? Par_close
            (block_statement | Arrow expr SemiColon) ;


constructor_decl : This Qm? function_decl_tail function_decl_spec ;
destructor_decl : Not This Par_open Par_close ;
method_decl  : type_decl (op_decl | scoped_identifier (Dot op_decl )? ) function_decl_tail qualifier* function_decl_spec ;
vmethod_decl : type_decl (op_decl | scoped_identifier (Dot op_decl )? ) function_decl_tail qualifier* function_decl_spec Assign Null ;

interface_content : constructor_decl SemiColon
                    | destructor_decl SemiColon
                    | method_decl SemiColon  ;

interface_decl    : Interface attribute* identifier SemiColon  ;
interface_def     : Interface attribute* identifier Curly_open interface_content* Curly_close  ;
interface_inline  : Interface attribute*            Curly_open interface_content* Curly_close ;

struct_content : constructor_decl (SemiColon | ctor_init? block_statement ctor_init? | Arrow expr SemiColon)
               | destructor_decl (SemiColon |           block_statement | Arrow expr SemiColon)
               | method_decl (SemiColon | block_statement)
               | Inline? var_decl ;


struct_inline_content : constructor_decl block_statement
                        | destructor_decl block_statement
                        | method_decl  block_statement
                        | Inline? var_decl  ;

union_content : constructor_decl (SemiColon | ctor_init? block_statement)
                | destructor_decl (SemiColon | block_statement)
                | method_decl (SemiColon | block_statement)
                | var_decl  ;


struct_decl   : Partial? Struct align_as? attribute* identifier SemiColon ;
struct_def    : Partial? Struct align_as? attribute* identifier Curly_open struct_content* Curly_close ;
struct_inline : Partial? Struct align_as? attribute* Curly_open struct_inline_content* Curly_close ;


strong_type_decl : Type identifier type_decl SemiColon ;

enum_content : identifier (Assign expr)? (Comma identifier (Assign expr)?)* ;

integral : Bool | IntType | scoped_identifier ;

enum_decl   : Inline? Enum attribute * identifier (Colon integral)? SemiColon ;
enum_def    : Inline? Enum attribute * identifier (Colon integral)? Curly_open enum_content? Curly_close ;
enum_inline :         Enum (Colon integral) ? attribute * Curly_open enum_content? Curly_close ;

union_decl   : Union align_as? attribute* identifier SemiColon ;
union_def    : Union align_as? attribute* identifier Curly_open union_content* Curly_close ;

register_content : integral identifier ? Colon expr SemiColon ;
register_decl : Register identifier (Colon integral)? ;
register_def  : Register identifier (Colon integral)? Curly_open register_content* Curly_close ;

constructor_def: scoped_identifier Dot This function_decl_tail function_decl_spec
                (block_statement | Assign Default SemiColon | Arrow expr SemiColon) ;

destructor_def: scoped_identifier Dot Not This Par_open Par_close
                (block_statement | Assign Default SemiColon | Arrow expr SemiColon) ;

global_statement : Extern? var_decl
           | function_decl SemiColon
           | function_decl block_statement
           | function_decl Arrow expr SemiColon
           | interface_decl
           | interface_def
           | strong_type_decl
           | macro_def
           | struct_decl
           | struct_def
           | enum_decl
           | enum_def
           | class_decl
           | class_def
           | union_decl
           | union_def
           | register_decl
           | register_def
           | constructor_def
           | destructor_def
           ;


inline_namespace : Inline Namespace identifier SemiColon ;

statement : global_statement
           | expr SemiColon | Return expr SemiColon | Yield expr SemiColon | block_statement SemiColon?
           | SemiColon
           | attribute SemiColon
           | while_loop
           | for_loop
           | if_
           | switch_
           | Goto scoped_identifier SemiColon
           | Goto Case expr SemiColon
           | identifier Colon
           | Break SemiColon
           | try_catch
           | inline_namespace
           ;

ctor_init : Colon scoped_identifier Par_open (expr (Comma expr)*)? Par_close (Comma scoped_identifier Par_open (expr (Comma expr)*)? Par_close) * ;

visibility : Public | Private | Protected  ;

class_content : visibility? constructor_decl (SemiColon | ctor_init? block_statement  | Assign Default)
              | visibility?  destructor_decl (SemiColon | block_statement | Assign Default)
              | visibility? method_decl Final? (SemiColon | block_statement | Assign Null | Assign Default)
              | visibility? Inline? var_decl
              | visibility Colon ;


class_def  : Partial? Class identifier SemiColon ;
class_decl : Partial? Class identifier (Colon visibility? scoped_identifier (Comma visibility? scoped_identifier) *)? Final? attribute* Curly_open class_content Curly_close ;


then_ : Then Par_open (type_decl identifier?) Par_close statement ;
while_loop : While Par_open expr Par_close statement then_ ? ;
for_loop: For Par_open (type_decl identifier? In expr) Par_close statement then_ ? ;
else_if : Cc? Else If Par_open (var_decl SemiColon)? expr Par_close statement ;
else_   : Cc? Else statement ;
if_     : Cc? If Par_open var_decl? expr Par_close statement else_if * else_ ? ;

case_block : Case Par_open type_decl identifier? Par_close statement ;

switch_ : Switch identifier Par_open expr Par_close Curly_open (statement | case_block | Default (Colon | statement) | Case expr (Comma expr)* (Colon | statement)) Curly_close ;
try_catch : Try statement (Catch Par_open type_decl identifier? Par_close statement )+ ;

