//
// Created by klemens on 5/21/25.
//

#ifndef CELEBES_AST_NODES_HPP
#define CELEBES_AST_NODES_HPP

#include <memory_resource>
#include <optional>
#include <string>
#include <variant>

namespace celebes::ast
{
struct expr;

struct interpolation
{
  std::pmr::vector<std::pmr::string> segments;
  std::pmr::vector<expr*> values;
};

struct identifier
{
  enum { raw, char_, interpolation, builtin  } type;
  std::pmr::string value;
  struct interpolation * ip;
};

struct scoped_identifier
{
  bool is_absolute;
  std::pmr::vector<identifier> ids;
};

enum class float_type {bf16 = 1, f16, f32, f64, f128};

struct floating_point_literal
{
  long double value;
  identifier id;
  float_type tp;
};

struct int_literal
{
  std::string value;
  identifier id;
  // instead of id
  bool signed_ = false;
  std::size_t bits;
};

struct string
{
  std::pmr::string str;
  interpolation ip;
};

using literal = std::variant<floating_point_literal, int_literal, string, bool, std::nullptr_t>;

struct module_decl
{
  bool private_ = false;
  identifier name;
};

struct global_statement;
struct namespace_definition;
struct namespace_alias
{
  scoped_identifier id;
  scoped_identifier aliased;
};


using namespace_content = std::variant<namespace_definition, namespace_alias, global_statement>;
struct namespace_definition
{
  scoped_identifier id;
  std::pmr::vector<namespace_content*> content;
};

struct include_statement
{
  identifier id;
  string value;
};

struct import_statement
{
  identifier id;
  scoped_identifier import_;
};

struct struct_init_elem
{
  identifier designator;
  expr* init;
};

using struct_init = std::pmr::vector<struct_init_elem>;

enum placeholder : std::int64_t {};

struct int_type
{
  bool signed_ = false;
  std::size_t bits;
  std::endian endianess;
};


enum class type {};
enum class variadic {};

struct tuple_expr
{
  std::pmr::vector<expr*> elements;
};


using  primary_expr = std::variant<placeholder, scoped_identifier, literal, string, int_type, float_type,
                                   bool, type, tuple_expr, struct_init, expr*, variadic>;

struct argument
{
  identifier designator;
  expr* type;
};

// no need to encode all of that in the type system
enum class any_qualifier
{
  mutable_, const_, volatile_, atomic, fast, safe, not_volatile, not_fast, not_safe, little, big,
  thread_local_
};

struct postfix_expr
{
  primary_expr expr;

  struct op
  {
    enum {square, par, drf, inc, dec, qm} type;
    std::pmr::vector<argument> args;
    identifier id;
  };
  std::pmr::vector<op> ops;
};

struct unary_expr
{
  postfix_expr expr;
  enum op {and_, mul, plus, minus, not_, xor_, log_not, await_, inc, dec, sizeof_, alignof_, len, cc, new_, qm, throw_, str, move};
  std::pmr::vector<op> ops;
};

struct cast_expr
{
  unary_expr expr;
  std::pmr::vector<std::variant<std::monostate, struct expr*, any_qualifier>> ops;
};

struct mul_expr
{
  cast_expr expr;
  enum type {mul, div, mod};
  std::pmr::vector<std::pair<type, cast_expr>> tail;
};

struct add_expr
{
  mul_expr expr;
  enum type {plus, minus};
  std::pmr::vector<std::pair<type, mul_expr>> tail;
};

struct shift_expr
{
  add_expr expr;
  enum type {left, right};
  std::pmr::vector<std::pair<type, add_expr>> tail;
};

struct rel_expr
{
  shift_expr expr;
  enum type {gt, lt, ge, le};
  std::pmr::vector<std::pair<type, shift_expr>> tail;
};

struct eq_expr
{
  rel_expr expr;
  enum type {equal, ne};
  std::pmr::vector<std::pair<type, rel_expr>> tail;
};

template<typename T>
struct simple_expr { T expr; std::pmr::vector<T> tail;};

using and_expr     = simple_expr<eq_expr>;
using exc_expr     = simple_expr<and_expr>;
using inc_expr     = simple_expr<exc_expr>;
using log_and_expr = simple_expr<inc_expr>;
using log_or_expr  = simple_expr<log_and_expr>;
using rng_expr     = simple_expr<log_or_expr>;
using at_expr      = simple_expr<rng_expr>;
using str_expr     = simple_expr<at_expr>;
using sfl_expr     = simple_expr<str_expr>;

struct is_expr
{
  sfl_expr expr;
  std::pmr::vector<std::pair<bool, sfl_expr>> tail;
};

using in_expr     = simple_expr<is_expr>;

struct cond_expr
{
  in_expr expr;
  enum type {qm, colon} ;
  std::pmr::vector<std::pair<type, in_expr>> tail;
};

enum class assign_op
{
  add, sub, mul, div, mod, and_, or_, xor_, lsh, rsh, shl, assign
};


struct assign_expr
{
  std::variant<unary_expr, cond_expr> expr;
  assign_op op;
  assign_expr * tail;
};

using block_statement = std::pmr::vector<struct statement>;

struct lambda
{
  bool async = false;
  struct function_decl_type * func;
  struct function_decl_spec *  spec;
  expr * body;
};

struct expr
{
  std::variant<assign_expr,  block_statement, struct lambda*> inner;
};

struct align_as
{
  expr value;
};

struct type_mod
{
  struct array_mod
  {
    std::pmr::vector<expr> inner;
  };

  enum tmod {xor_, and_, mul_, qm};
  struct small
  {
    bool qm = false;
    expr * value;
  };

  std::variant<array_mod, tmod, small, align_as> mod;
};

struct type_decl_inner
{
  enum tp {void_, let, var};
  struct invocation
  {
    identifier id;
    std::pmr::vector<argument> args;
  };

  struct type_deduction
  {
    expr * ex;
  };

  std::variant<int_type, float_type, bool, tp, invocation,
               type_deduction, struct struct_inline*, struct interface_inline*, struct enum_inline*> inner;

  type_decl_inner * alternative;
};

struct type_decl
{
  std::pmr::vector<any_qualifier> qualifiers;
  bool cc;
  std::variant<type_decl_inner*, std::pmr::vector<type_decl_inner>> tp;
  std::pmr::vector<type_mod> modifier;
};

struct tuple_bind
{
  std::pmr::vector<std::variant<identifier,variadic>> ids;
};

struct struct_bind
{
  std::pmr::vector<std::variant<identifier,variadic>> ids;
};

struct bind_decl
{
  bool extern_;
  bool is_const;
  std::pmr::vector<any_qualifier> qualifiers;
  enum {none, xor_, and_, mul_} mod;

  std::variant<tuple_bind, struct_bind> binding;

  bool cc;
  expr * default_value;
};


struct var_decl
{
  bool extern_;
  type_decl type;

  std::optional<type_decl> offset;
  std::pmr::vector<type_decl> func_type;

  identifier id;
  expr * init;
};

struct function_decl_arg
{
  bool is_this, is_variadic;
  type_decl type;
  expr * default_value;
};

struct macro_decl_arg : function_decl_arg
{
  bool is_expr = false;
};

struct op_decl
{
  enum {
    none,
    qm,
    and_,
    mul, plus, minus, not_, xor_, log_not, await,
    inc, dec, len, div, mod,  str,  lsh, rsh, gt, lt, ge, le, equal, not_equal,  log_and, log_or, rng, in, is,
    colon, move, add_assign, sub_assign, mul_assign, div_assign, mod_assign, and_assign, or_assign, xor_assign, lsh_assign, rhs_assign, shl, assign,
    cast, literal,
  } op = none;

  identifier id;
  bool explicit_;
  type_decl type;
};

struct attribute
{
  identifier id;
  std::pmr::vector<expr> exprs;
};

struct function_decl_spec
{
  std::pmr::vector<attribute> attributes;
  bool noexcept_;
};

struct function_def
{
  bool async;
  type_decl type;
  scoped_identifier id;
  op_decl op;

  std::pmr::vector<function_decl_arg> args;
  function_decl_spec spec;

  expr * body;
};

struct macro_def
{
  scoped_identifier id;
  op_decl op; // an op can be a macro too
  std::pmr::vector<macro_decl_arg> args;
  expr * body;
};

struct statement;

struct inline_namespace
{
  scoped_identifier identifier;
};


struct register_entry
{
    int_type type;
    identifier name;
    expr * width;
};


struct constructor_def_base
{
  std::pmr::vector<function_decl_arg> args;
  function_decl_spec spec;
  std::pmr::vector<std::pair<scoped_identifier, std::pmr::vector<expr*>>> inits;
  expr * body;
};


struct constructor_
{
  bool optional = false;
  bool defaulted = false;
  constructor_def_base * def;
};


struct destructor_
{
  bool is_noexcept = false;
  expr * body;
};

struct method_decl
{
  bool is_async;
  bool is_final;
  bool is_pure_virtual;
  bool is_default;

  std::variant<scoped_identifier, op_decl> id;

  std::pmr::vector<any_qualifier> qualifiers;
  std::pmr::vector<function_decl_arg> args;
  function_decl_spec spec;
  expr * body;

};

struct union_
{
  identifier name;
  expr * alignas_;

  std::pmr::vector<attribute> attributes;
  std::pmr::vector<constructor_> constructors;
  std::optional<destructor_> dtor;
  std::pmr::vector<var_decl> members;
  std::pmr::vector<method_decl> methods;
};

struct register_
{
  identifier name;
  std::optional<int_type> base_int;
  bool is_def = true;
  std::pmr::vector<register_entry> content;
};

enum visibility {public_, private_, protected_};


struct interface_def
{
  identifier name;
  std::pmr::vector<std::pair<visibility, scoped_identifier>> inheritance;
  std::pmr::vector<attribute> attributes;
  std::pmr::vector<method_decl> methods;
};


struct struct_def
{
  identifier name;


  std::pmr::vector<attribute> attributes;
  std::pmr::vector<constructor_> constructors;
  std::optional<destructor_> dtor;
  std::pmr::vector<var_decl> members;
  std::pmr::vector<method_decl> methods;
};

struct struct_
{
  bool partial = false;
  identifier id;

  struct_def * definition;
};

struct enum_
{
  bool inline_ = false;
  std::pmr::vector<attribute> attributes;
  int_type base_type;
  identifier id;

  std::pmr::vector<std::pair<identifier, expr*>> values;
};

struct class_def
{
  identifier name;
  std::pmr::vector<std::pair<visibility, scoped_identifier>> inheritance;
  bool is_final = false;

  std::pmr::vector<attribute> attributes;
  std::pmr::vector<constructor_> constructors;
  std::optional<destructor_> dtor;
  std::pmr::vector<var_decl> members;
  std::pmr::vector<method_decl> methods;

};

struct class_
{
  bool partial = false;
  identifier id;

  class_def * definition;
};

struct then_
{
  type_decl * type_;
  identifier id;

  statement * body;
  then_ * next;
};

struct while_loop
{
  expr * source;
  statement * body;
  then_ * next;
};

struct for_loop
{
  type_decl * type_;
  identifier decl_id;
  expr * source;

  statement * body;

  then_ * next;
};


struct else_
{
  bool cc = false;
  var_decl * decl;
  expr * cond;

  statement * body;

  else_ * next;
};

struct if_
{
  bool cc = false;
  var_decl * decl;
  expr * cond;

  statement * body;

  else_ * next;
};

struct switch_
{
  identifier id;
  expr * switch_expr;
  struct case_
  {
    bool is_default() const {return value == nullptr && type == nullptr;}
    bool is_block() const {return value == nullptr;}
    // if not block
    expr * value = nullptr;

    // if block
    type_decl * type;
    identifier id;

    // always present
    statement * body;
  };

  std::pmr::vector<case_> cases;
};

struct try_catch
{
  statement * try_exprs;
  struct catch_clause
  {
    type_decl * type;
    identifier id;
    statement * stmt;
  };
  std::pmr::vector<catch_clause> catch_exprs;
};


struct return_ { expr * value; };
struct yield_ { expr * value; };
struct goto_ {identifier target;};
struct goto_case {expr * target;};

struct strong_type_def
{
  identifier id;
  type_decl type;
};

struct global_statement :
    std::variant<var_decl,
                 function_def,
                 interface_def,
                 strong_type_def,
                 macro_def,
                 struct_,
                 enum_,
                 class_,
                 union_,
                 register_,
                 constructor_,
                 destructor_>
{

};

struct statement : std::variant<global_statement,
                                expr, return_, yield_, block_statement,
                                attribute,
                                while_loop,
                                for_loop,
                                if_,
                                switch_,
                                goto_,
                                goto_case,
                                identifier,
                                try_catch,
                                inline_namespace
                                > {};

}

#endif //CELEBES_AST_NODES_HPP
