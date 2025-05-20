//
// Created by klemens on 3/19/25.
//

#ifndef CELEBES_PARSER_HPP
#define CELEBES_PARSER_HPP

#include <memory>
#include <memory_resource>
#include <string>
#include <variant>
#include <vector>

#include <celebes/lexer.hpp>

namespace celebes
{
namespace ast
{

struct scoped_identifier
{
  bool absolute;
  std::vector<token> segments; // at least one !
};

struct placeholder : token {};

//struct expr ;

struct interpolation
{
  std::pmr::vector<std::string_view> segments;
  std::pmr::vector<struct expr*> expressions;
};

using literal = std::variant<token, interpolation>;

struct int_type
{
  std::endian endian = std::endian::native;
  token type;
};

using array_expr = std::vector<expr>;
struct struct_init_step
{
  std::optional<token> designator;
  //std::unique_ptr<expr> expr;
};

using primary_expr = std::variant<placeholder, scoped_identifier, token, interpolation, int_type, array_expr, std::vector<struct_init_step>, std::unique_ptr<expr>>;

struct postfix_expr
{
  primary_expr pe;
  token tk;
};


struct unary_expr
{
  std::pmr::vector<token> ops;
  postfix_expr * next;
};

struct castable_qualifier
{
  bool invert = false;
  token qual;
};

struct cast_expr
{
  struct inner
  {
    std::variant<std::unique_ptr<expr>, token, castable_qualifier> cast_op;
    std::unique_ptr<cast_expr> cast;
  };
  std::variant<struct inner, unary_expr> value;
};

// binary_expression ::= binary_expression binary_op cast_expr | cast_expr
struct binary_expression
{
  std::unique_ptr<binary_expression> lhs;
  token op;
  cast_expr rhs;
};

struct cond_expr
{

};

// assign_expr ::= binary_expression assign_op assign_expr | binary_expression
struct assign_expr
{
  std::variant<cond_expr, unary_expr> * lhs;
  token op; // multi-char
  assign_expr* rhs;
};

using block_statement = std::vector<struct statement>;

struct attribute
{

};

struct global_statement
{
  //
};

struct statement : global_statement
{

};

struct lambda;

//using expr = std::variant<std::monostate, assign_expr*, block_statement*, lambda*>;

/*struct expr : std::variant<assign_expr, block_statement>
{
  using std::variant<assign_expr, block_statement>::variant;
  using std::variant<assign_expr, block_statement>::operator=;
};*/


using identifier = std::variant<std::monostate, std::string_view, interpolation>;

struct namespace_content
{
  namespace_content * parent = nullptr;
  std::vector<std::pair<std::string, std::string>> namespace_aliases;

  std::vector<std::unique_ptr<global_statement>> statements;
};

struct module_decl
{
  identifier id;
  bool private_ = false;
};

struct import_statement;
struct include_statement;

struct root_content : namespace_content
{
  identifier module_name;
  bool module_private = false;

  std::vector<std::pair<std::string, std::string>> includes, imports;
};


}


struct error
{
  enum type_t {
    none,
    unexpected_eof,
    unexpected_token,
    missing_token,
    expected_expr,
    expected_primary_expr
  };


  type_t type = none;
  token pos;

  std::vector<token::type_t> expected_tokens;

  operator bool() const { return type != none;}
};


/*
  the itr is to be set to the first non-consumed token, if there is no error.
  if there is an error it points to the errnous token

  required will cause an error if the rule isn't matched.


 */

// state, tokens that can be consumed
ast::identifier parse_identifier(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::module_decl * parse_module_decl(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::expr parse_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());


std::optional<ast::interpolation> parse_interpolation(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::assign_expr * parse_assign_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::block_statement * parse_block_statement(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::lambda * parse_lambda(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::unary_expr * parse_unary_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());

ast::postfix_expr * parse_postfix_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required = false, // require the
    std::pmr::memory_resource * res = std::pmr::get_default_resource());



ast::root_content parse(tokenizer tk)
{
  auto itr = tk.begin();

  if (itr == tk.end())
    return {};

  auto get_next = [](tokenizer::iterator itr){ while ((++itr).current.whitespace()); return itr;};

  auto current = *itr;
  auto next = *get_next(itr);


  switch (current.type)
  {
    case token::module_: // module decl
    case token::include_: // include_statement
    case token::import_: // import statement
    // namespace content
    case token::namespace_: // namespace decl
      break;
  }

}


}

#endif //CELEBES_PARSER_HPP
