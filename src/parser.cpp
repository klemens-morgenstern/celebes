//
// Created by klemens on 3/19/25.
//

#include <celebes/parser.hpp>

#define CHECK_NOT_EOF(itr, err, res) if (itr.eof()) {err = {error::unexpected_eof, *itr}; return res;}

using mem_res = std::pmr::monotonic_buffer_resource;


void* operator new  ( std::size_t count,                      mem_res* res)
{
  return res->allocate(count);
}

void* operator new  ( std::size_t count, std::align_val_t al, mem_res* res)
{
  return res->allocate(count, static_cast<std::size_t>(al));
}

namespace celebes
{


inline std::string_view unescape_literal(std::string_view data, mem_res* res)
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




struct rule {};

template<typename T>
concept is_rule = true;

constexpr
struct identifier_t : rule
{
  ast::identifier reduce(token tk, mem_res * res) const
  {
    if (tk.type == token::identifier_raw )
      return tk.value;

    if (tk.type == token::char_literal)
      return unescape_literal(tk.value, res);
  }

  ast::identifier reduce(std::optional<ast::interpolation> ip, mem_res * res) const
  {
    return ast::identifier {std::move(*ip)};
  }

  ast::identifier error() const {return {}; }


} identifier;

constexpr
struct scoped_identifier_t : rule
{

} scoped_identifier;

constexpr struct interpolation_tail_t : rule
{
  std::optional<ast::interpolation> error() const {return std::nullopt;}

} interpolation_tail;

constexpr struct interpolation_t      : rule
{
  std::optional<ast::interpolation> reduce(celebes::token tk, mem_res* res) const
  {
    assert(tk.type == token::interpolation_string_single);
    return ast::interpolation{.segments={unescape_literal(tk.value, res)}};
  }

  std::optional<ast::interpolation> reduce(celebes::token tk, ast::expr* ex, std::optional<ast::interpolation> ip, mem_res* res) const;/*
  {
    assert(tk.type == token::interpolation_string_single);
    return ast::interpolation{.segments={unescape_literal(tk.value, res)}};
  }
*/

  std::optional<ast::interpolation> error() const {return std::nullopt;}
}
interpolation;

constexpr struct expr_t      : rule
{
  ast::expr* error() const {return nullptr;}
}  expr;

constexpr struct root_rule_t : rule
{
  ast::root_content * error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...) const;
} root_rule;

constexpr struct root_tail_t : rule
{
  ast::root_content * error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...) const;
} root_tail;

constexpr struct module_decl_t : rule
{
  ast::module_decl * error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...) const;

} module_decl;

constexpr struct include_statement_t : rule
{
  ast::include_statement * error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...)  const;
} include_statement;


constexpr struct import_statement_t : rule
{
  ast::import_statement * error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...) const;
} import_statement;

constexpr struct namespace_content_t : rule
{
  ast::namespace_content  *error() const;
  template<typename ... Args>
  ast::root_content * reduce(Args && ...) const;
} namespace_content;

template<typename ... Args>
struct parse_tuple
{
  std::tuple<Args...> steps;

  constexpr parse_tuple(Args ... args) : steps(args...) {}
  constexpr parse_tuple(std::tuple<Args ...> args) : steps(args) {}

};


template<typename RuleId, typename ... Alternatives>
struct parse_rule
{
  using rule_id_t = RuleId;

  RuleId id;
  std::tuple<Alternatives...> alternatives;

  constexpr parse_rule(RuleId id, std::tuple<Alternatives...> alternatives) :
             id(id), alternatives(alternatives) {}

};

template<typename ... Rules>
struct parse_table
{
  std::tuple<Rules...> rules;

  constexpr parse_table(Rules ... rules) : rules(std::move(rules)...) {}

  template<typename RuleId>
  constexpr auto get_rule() const
  {
    return get_rule_impl<0u, RuleId>();
    /*constexpr bool same[] = {std::is_same_v<typename Rules::rule_id_t, RuleId>...};
    constexpr auto itr = std::find(std::begin(same), std::end(same), true);
    constexpr auto pos = itr - std::begin(same);
    return std::get<pos>(rules);*/
  }

  template<typename T>
  auto foo() const;

  template<std::size_t N, typename RuleId>
  constexpr auto get_rule_impl() const
  {
    static_assert(N < sizeof...(Rules), "No matching rule found");

    if constexpr (std::is_same_v<typename std::tuple_element_t<N, std::tuple<Rules...>>::rule_id_t, RuleId>)
      return std::get<N>(rules);
    else if constexpr (sizeof ... (Rules) > N)
      return get_rule_impl<N+1, RuleId>();
  }
};

// mini DSL time - just to get the parser up quickly. it allows you to override single rules if you so desire later on.
template<typename T>
concept is_rule_element = is_rule<T> || std::same_as<T, token::type_t>;

template<is_rule Rule, typename ... Alternatives >
constexpr parse_rule<Rule, Alternatives...> operator>>=(Rule rl, parse_tuple<Alternatives...> alts)
{
  return parse_rule<Rule, Alternatives...>{rl, alts.steps};
}


template<is_rule_element Lhs, is_rule_element Rhs>
constexpr auto operator|(Lhs lhs, Rhs rhs)
{
  return parse_tuple<Lhs, Rhs>{lhs, rhs};
}

template<typename ...Ts, is_rule_element Rhs>
constexpr auto operator|(parse_tuple<Ts...> lhs, Rhs rhs)
{
  return parse_tuple<Ts..., Rhs>{std::tuple_cat(lhs.steps, std::make_tuple(rhs))};
}

template<typename ... Ts>
using r = parse_tuple<Ts...>;
using t = token;


template<typename ... Alts>
constexpr bool operator!(const std::variant<std::monostate, Alts...> & var)
{
  return var.index() == 0u;
}

constexpr parse_table table =
{
    root_rule >>= r{module_decl, root_tail},
    root_tail >>= include_statement | import_statement | namespace_content | r{},

    module_decl >>= r{t::module_, t::private_, identifier, t::semicolon}
                  | r{t::module_,              identifier, t::semicolon},

    namespace_ >>= namespace_definition | namespace_alias,
    namespace_definition >>= {t::namespace_, scoped_identifier, t::curly_open, namespace_content_tail, t::curly_close},
    namespace_content_tail >>= r{namespace_content namespace_content_tail} | namespace_content_tail
    namespace_content >>= namespace_ | global_statement,
    namespace_alias >>= r{t::namespace_ , scoped_identifier_t, t::assign, scoped_identifier, t::semicolon},

    include_statement >>= r{t::include_, identifier, string, t::semicolon}
                        | r{t::include_, string, t::semicolon},
    interpolation     >>= t::interpolation_string_start |
                          r{t::interpolation_string_start, expr, interpolation_tail},
    interpolation_tail>>= t::interpolation_string_end |
                          r{t::interpolation_string_mid, expr, interpolation_tail},

    identifier        >>= t::identifier_raw | t::char_literal | interpolation,
    scoped_identifier_tail >>= r{t::dot, identifier},
    scoped_identifier >>= r{t::dot, identifier} |
                          r{        identifier} |
                          r{        identifier, scoped_identifier_tail} |
                          r{t::dot, identifier, scoped_identifier_tail},

    struct_decl >>= r{t::struct_, identifier_t, t::semicolon}
    floating_point_literal >>= float_ | r{float_, identifier},
    int_literal >>= t::int_ | r{int_, identifier},
    string >>= t::raw_string | t::char_literal | t::string_literal | interpolation | t::raw_string_literal,
    literal >>= floating_point_literal | int_literal | string | t::true | t::false | t::null,

    designator >>= r{t::dot_, identifier, t::assign},
    struct_init_tail >>= r{t::comma, struct_init_step} | r{},
    struct_init_step >>= r{designator expr} | expr
    struct_init >>= r{t::curly_open, struct_init_step, struct_init_tail, t::curly_close},
                    r{t::curly_open, t::curly_close},


    expr_list_tail >>= r{t::comma, expr} | r{};
    expr_list  >>= r{expr, expr_list_tail
    tuple_expr >>=
                 | r{t::square_open, t::square_close}
                 | r{t::square_open, expr_list, t::square_close },
    primary_expr >>= t::placeholder | scoped_identifier | literal | string
                   | r{t::little, t::int_t} | r{t::big, t::int_t} | t::int_t
                   | t::float_t | t::bool_ | t::char_literal | t::type |
                   | tuple_expr
                   | struct_init | r{t::par_open, expr, t::par_close}
                   | t::int_null,

    argument_list_tail >>= r{comma, argument_list} |  r{},
    argument_list >>= r{identifier, t::colon, expr, argument_list_tail} | r{expr, argument_list_tail}

    castable_qualifier >>= t::mutable_ | t::const_ | t::volatile_ | t::atomic | t::fast | t::safe |
                         | r{t::log_not, t::volatile_} | r{t::log_not, t::fast} | r{t::log_not, t::safe}
                         | t::big | t::little,

    non_const_qualifier >>= t::volatile | t::atomic | t::fast | t::safe | t::thread_local_,
    qualifier >>= non_const_qualifier | t::const_,

    postfix_expr>>=
                  | r{postfix_expr, t::square_open,                t::square_close}
                  | r{postfix_expr, t::square_open, argument_list, t::square_close}
                  | r{postfix_expr, t::par_open,                   t::par_close}
                  | r{postfix_expr, t::par_open, argument_list,    t::par_close}
                  | r{postfix_expr, t::drf, identifier}
                  | r{postfix_expr, t::inc}
                  | r{postfix_expr, t::dec}
                  | r{postfix_expr, t::qm}
                  | primary_expression

    unary_operator >>=
        t::and_ | t::mul | t::plus | t::minus | t::not_ | t::xor_ | t::log_not,

    unary_lhs >>=
        t::inc | t::dec | unary_operator | t::sizeof_ | t::alignof_ | t::len | t::cc | t::new_
        t::qm | t::throw_ | t::str | t::move,

    unary_expr >>= r{unary_lhs, unary_expr} | postfix_exp,
    cast_expr >>= r{t::par_open, expr, t::par_close}
                | r{t::par_open, t::void_, t::par_close}
                | r{t::par_open, castable_qualifier, t::par_close}
                | unary_expr,

    mul_expr >>= cast_expr
                 r{mul_expr, t::mul, cast_expr}
               | r{mul_expr, t::div, cast_expr}
               | r{mul_expr, t::mod, cast_expr}

    expr >>= r{t::type_t(-2)}
};

// any function must have a return value that's truthy-

template<typename RuleId, typename ... Steps>
auto parse_alternative(const RuleId & rule,
                       const parse_tuple<Steps...>  & alt,
                       tokenizer::iterator & itr,
                       error & old_err,
                       bool required,
                       mem_res* res)
{
  error err;

  auto step =
      [&]<typename T>(const T & a )
      {
        if constexpr(std::is_same_v<T, token::type_t>)
        {
          if (err)
            return token{};

          if (a == itr->type)
            return *itr++;
          else
            err = error{.type=error::unexpected_token, .pos = *itr};
        }
        else
        {
          if (err)
            return a.error();
          return parse(a, itr, err, required, res);
        }
      };

  auto val = std::apply([&](const auto & ... as) {return std::make_tuple(step(as)...);}, alt.steps);
  if (err)
  {
    old_err = err;
    return rule.error();
  }
  return std::apply(
      [&](auto ... as) {return rule.reduce(std::move(as)..., res);},
      std::move(val));
}

template<typename RuleId>
auto parse_alternative(const RuleId & rule,
                       token::type_t tp,
                       tokenizer::iterator & itr,
                       error & err,
                       bool required,
                       mem_res* res)
{
  if (tp == itr->type)
    return rule.reduce(*itr++, res);
  else
  {
    err = error{.type=error::unexpected_token, .pos = *itr};
    return rule.error();
  }
}

template<is_rule RuleId, is_rule Alt>
auto parse_alternative(const RuleId & rule,
                       Alt inner,
                       tokenizer::iterator & itr,
                       error & err,
                       bool required,
                       mem_res* res)
{
  auto r = parse(inner, itr, err, required, res);
  if (!!r)
    return rule.reduce(r, res);
  else
    return rule.error();
}


template<typename RuleId, typename ... Alternatives>
auto parse_impl(
           tokenizer::iterator & itr,
           error & pre_err,
           bool required,
           mem_res* res,
           const Alternatives & ... alts) -> decltype(RuleId().error())
{
  std::optional<decltype(RuleId().error())> opt;
  auto err = pre_err;
  bool done = false;
  auto do_the_thing =
      [&](auto alt)
      {
        if (done)
          return false;
        error err2;
        auto itr2 = itr;
        auto rr =  parse_alternative(RuleId{}, alt, itr2, err2, required, res);

        if (err2) // check if we've gotten further before the first error.
          if (err2.pos.value.data() > err.pos.value.data())
            itr2 = itr;

        if (!!rr)
        {
          done |= !!rr;
          itr2 = itr;
          opt = std::move(rr);
          return true;
        }
        return false;

      };

  if ((do_the_thing(alts) && ...))
    return *opt;
  else
  {
    if (pre_err != err)
      pre_err = err;
    else
      pre_err = error{.type=error::unexpected_token, .pos=*itr};
    return RuleId().error();
  }

  return opt.value_or(RuleId().error());

}

template<typename RuleId>
auto parse(const RuleId & r,
           tokenizer::iterator & itr,
           error & err,
           bool required, // require the
           mem_res* res) -> decltype(r.error())
{
  constexpr auto a = table.get_rule<RuleId>();
  return std::apply(
      [&](auto ... a)
      {
        return parse_impl<RuleId>(itr, err, required, res, a...);
      }, a.alternatives);

}

template
auto parse(const identifier_t & r,
           tokenizer::iterator & itr,
           error & err,
           bool required, // require the
           mem_res* res) -> ast::identifier ;


template
auto parse(const root_rule_t & r,
           tokenizer::iterator & itr,
           error & err,
           bool required, // require the
           mem_res* res) -> ast::root_content *;

/*
ast::module_decl * parse_module_decl(
    tokenizer::iterator & itr,
    error & err,
    bool required, // require the
    mem_res* res)
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
    mem_res* res)
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
    mem_res* res)
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
    mem_res* res)
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
    mem_res* res)
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
    mem_res* res)
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
    mem_res* res)
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
*/
}