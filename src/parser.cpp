//
// Created by klemens on 3/19/25.
//

#include <celebes/parser.hpp>

#define CHECK_NOT_EOF(itr, err, res) if (itr.eof()) {err = {error::unexpected_eof, *itr}; return res;}

void* operator new  ( std::size_t count,                      std::pmr::memory_resource * res)
{
  return res->allocate(count);
}

void* operator new  ( std::size_t count, std::align_val_t al, std::pmr::memory_resource * res)
{
  return res->allocate(count, static_cast<std::size_t>(al));
}

namespace celebes
{


inline std::string_view unescape_literal(std::string_view data, std::pmr::memory_resource * res)
{
  char * s = static_cast<char*>(res->allocate(data.size(), alignof(char)));
  std::size_t pos = 0u;

  for (std::size_t n = 0u; n < data.size(); n++)
  {
    if (data[n] == '\\')
    {
      n++;
      assert(n != pos); // precondition, should be caught by the lexer
      switch (data[n])
      {
        case 'n': s[pos++] = '\n';
        case 'r': s[pos++] = '\r';
        case 'v': s[pos++] = '\v';
        default:
          s[pos++] = data[n];
          break;
      }
    }
    s[pos++] = data[n];
  }
  pos++;
  return {s, pos};
}




ast::module_decl * parse_module_decl(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, nullptr);

  if (itr->type != token::module_) // no match
  {
    if (required)
      err = {error::unexpected_token, *itr};
    return nullptr;
  }

  // allocate the return thingy

  auto r = new (res) ast::module_decl{};

  itr ++;
  if (itr->type == token::private_)
  {
    itr++;
    r->private_ = true;
  }

  r->id = parse_identifier(itr, err, true, res);

  if (!err && itr++->type != token::semicolon)
    err = {error::missing_token, *itr, {token::semicolon}};
  return r;
}


ast::identifier parse_identifier(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, std::monostate());

  if (itr->type == token::identifier_raw)
    return ast::identifier {itr++->value};

  if (itr->type == token::char_literal)
    return ast::identifier {unescape_literal(itr++->value, res)};

  auto ip = parse_interpolation(itr, err, required, res);
  if (!ip)
    return std::monostate{};
  else
    return {*ip};
}


std::optional<ast::interpolation> parse_interpolation(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, std::nullopt);

  ast::interpolation r {
    .segments{std::pmr::polymorphic_allocator<std::string_view>(res)},
    .expressions{std::pmr::polymorphic_allocator<ast::expr>(res)}
  };


  if (itr->type == token::interpolation_string_single)
  {
    r.segments.push_back(unescape_literal(itr->value, res));
    return r;
  }
  else if (itr->type == token::interpolation_string_start)
  {
    r.segments.push_back(unescape_literal(itr->value, res));
    itr ++;

    while (!itr.eof())
    {
      // parse an expression!
      auto e = parse_expr(itr, err, true, res);
      if (!e)
        return r;

      r.expressions.push_back(e);
      if (itr->type == token::interpolation_string_end)
      {
        r.segments.push_back(unescape_literal(itr->value, res));
        itr ++;
        return r;
      }

      if (itr->type == token::interpolation_string_mid)
      {
        r.segments.push_back(unescape_literal(itr->value, res));
        itr ++;
      }
      else
      {
        err = {error::unexpected_token, *itr, {token::interpolation_string_mid, token::interpolation_string_end}};
        return r;
      }
    }
  }
  else if (required)
    err  = {error::unexpected_token, *itr, {token::interpolation_string_single, token::interpolation_string_start}};
  return std::nullopt;
}


ast::expr parse_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, std::monostate{});

  if (auto ae = parse_assign_expr(itr, err, false, res); ae || err)
    return ae;
  else if (auto bs = parse_block_statement(itr, err, false, res); bs || err)
    return bs;
  else if (auto l = parse_lambda(itr, err, false, res); l || err)
    return l;

  err = {error::expected_expr, *itr};

  return std::monostate{};
}

ast::assign_expr * parse_assign_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
    CHECK_NOT_EOF(itr, err, nullptr);

    auto ue = parse_unary_expr(itr, err, false, res);

    // any expression starts with a unary expression
    if (!ue || err)
      return nullptr;

    auto ae = new (res) ast::assign_expr{.lhs=ce};

    using t = token::type_t;
    constexpr t assign_ops[]
          = {
            t::add_assign,
            t::sub_assign,
            t::mul_assign,
            t::div_assign,
            t::mod_assign,
            t::and_assign,
            t::or_assign,
            t::xor_assign,
            t::lsh_assign,
            t::rsh_assign,
            t::sfl_assign,
            t::assign
    };

    static_assert(std::is_sorted(std::begin(assign_ops), std::end(assign_ops)));

    if (auto[b, e] = std::equal_range(std::begin(assign_ops), std::end(assign_ops), itr->type); b != e) // assign op
    {
      ae->lhs = ue;
      // is assignment expr - requires Rhs
      ae->op = itr++->type;
      ae->rhs = parse_assign_expr(itr, err, true, res);
      return ae;
    }
    else // not assignment expr
    {
      ae.lhs = parse_cond_expr(itr, err, true, res, ue);
      return ae;
    }
}

ast::postfix_expr * parse_postfix_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, nullptr);

  auto pe = parse_primary_expression(itr, err, required, res);

  if (!pe && !err && required)
    err = {error::expected_primary_expr, *itr};

  auto po = new (res) ast::postfix_expr{
    pe
  };

  if (err)
    return nullptr;

  if (itr->type == token::deref
   || itr->type == token::dec
   || itr->type == token::qm) // simple postfix-expr
  {
    pe->tk = *itr;
    itr ++;
  }

}

ast::unary_expr * parse_unary_expr(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    std::pmr::memory_resource * res)
{
  CHECK_NOT_EOF(itr, err, nullptr);

  using t = token::type_t;
  constexpr t unary_ops[]
      = {
          t::inc,
          t::dec,
          t::minus,
          t::mul,
          t::and_,
          t::xor_,
          t::cc,
          t::not_,
          t::log_not,
          t::qm,
          t::str,
          t::await,
          t::throw_,
          t::move,
          t::len,
          t::sizeof_,
          t::alignof_,
          t::new_
      };
  static_assert(std::is_sorted(std::begin(unary_ops), std::end(unary_ops)));

  auto is_op =
      [&](token::type_t t)
      {
        auto[b, e] = std::equal_range(std::begin(unary_ops), std::end(unary_ops), itr->type);
        return b != e;
      };

  std::pmr::vector<token> tks{res};

  while (is_op(itr->type))
    tks.push_back(*itr++);

  auto pe = parse_postfix_expr(itr, err, true, res);
  if (!pe)
    return nullptr;

  if ()
}

}