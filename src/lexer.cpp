#include <celebes/lexer.hpp>

namespace celebes
{

using std::operator""sv;
constexpr auto npos = std::string_view::npos;



token::type_t interpret_alphanum_token(std::string_view sv)
{
  if (sv.empty()) [[unlikely]]
    return token::type_t::eof;

  constexpr std::pair<std::string_view, token::type_t> keywords[]
      = {
          {"alignof",      token::alignof_      },
          {"atomic",       token::atomic        },
          {"await",        token::await         },
          {"class",        token::class_        },
          {"const",        token::const_        },
          {"else",         token::else_         },
          {"extern",       token::extern_       },
          {"false",        token::false_        },
          {"fast",         token::fast_         },
          {"if",           token::if_           },
          {"import",       token::import_       },
          {"in",           token::in            },
          {"include",      token::include_      },
          {"is",           token::is            },
          {"len",          token::len           },
          {"let",          token::let           },
          {"module",       token::module_       },
          {"move",         token::move_         },
          {"mutable",      token::mutable_      },
          {"namespace",    token::namespace_    },
          {"nan",          token::nan_          },
          {"new",          token::new_          },
          {"null",         token::null          },
          {"private",      token::private_      },
          {"protected",    token::protected_    },
          {"public",       token::public_       },
          {"safe",         token::safe_         },
          {"sizeof",       token::sizeof_       },
          {"struct",       token::struct_       },
          {"this",         token::this_         },
          {"thread_local", token::thread_local_ },
          {"throw",        token::throw_        },
          {"true",         token::true_         },
          {"type",         token::type_         },
          {"void",         token::void_         },
          {"volatile",     token::volatile_     },
          {"yield",        token::yield         }
      };

  static_assert(std::is_sorted(std::begin(keywords), std::end(keywords),
                               [](const auto & p, const auto & q ) { return p.first < q.first;}));

  constexpr std::string_view fl[] = {"bf16", "f16", "f32", "f64", "f128"};

  if (std::find(std::begin(fl), std::end(fl), sv) != std::end(fl))
    return token::float_t;

  if (sv.starts_with('u') || sv.starts_with('i')) //
  {
    using std::operator""sv;
    constexpr auto npos = std::string_view::npos;
    // rest needs to be numerics
    if (std::all_of(std::next(sv.begin()), sv.end(), [](char c){return "0123456789"sv.find(c) != npos;}))
      return token::int_t;
  }

  // check for keywords
  auto [b, e] = std::equal_range(std::begin(keywords), std::end(keywords),
                                 std::pair<std::string_view, token::type_t>(sv, {}),
                                 [](std::pair<std::string_view, token::type_t> lhs,
                                    std::pair<std::string_view, token::type_t> rhs)
                                 {return lhs.first.compare(rhs.first);});
  if (b != e)
    return b->second;

  return token::identifier_raw;
}


token get_whitespace(std::string_view input, source_location & loc)
{
  auto itr = input.begin();
  const auto ll = loc;
  for (std::size_t pos = "\n\v\r\t "sv.find(*itr);
       itr != input.end()  && pos != npos;
       ++itr != input.end(), pos = "\n\v\r\t "sv.find(*itr))
    if (pos == 0u)
      loc.line++, loc.column = 0;
    else if (pos == 1u)
      loc.line++;
    else if (pos == 2u)
      loc.column = 0;
    else
      loc.column++;

  return {.type=token::whitespace_, .value={input.begin(), itr}, .loc=ll};
}


token get_alphanum(std::string_view input, source_location & loc)
{
  const auto e = input.find_first_not_of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_01234567890");
  const auto val = input.substr(0, e);
  const auto ll = loc;
  loc.column += e;
  return {.type=interpret_alphanum_token(val), .value=val, .loc=ll};
}

token get_placeholder_or_builtin(std::string_view input, source_location & loc)
{
  const auto ll = loc;
  auto itr = input.begin();
  itr ++;
  if (itr == input.end())// incomplete, just break
    return {token::incomplete_, input, loc};

  token res{.loc=loc};

  if ("0123456789"sv.find(*itr) != npos) // placeholder
    res.type = token::placeholder;
  else if ("_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"sv.find(*itr) !=  npos)
    res.type = token::intrinsic_;

  const auto e = input.find_first_not_of("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijlkmnopqrstuvwxyz_", 1u);
  loc.column += (std::min)(e, input.size());
  res.value = input.substr(0u, e);
  return res;
}

token get_if_comment(std::string_view input, source_location & loc)
{
  printf("Is comment ?\n");
  if (input.size() < 2u)
    return token{token::incomplete_, input, loc};

  const auto ll = loc;

  if (input[1] == '/') // line comment
  {
    auto e = input.find('\n');
    loc.line++;
    loc.column = 0u;
    return token{token::comment, input.substr(0, e), ll};
  }
  else if (input[1] == '*')
  {
    std::size_t depth = 1u;
    auto itr = input.begin() + 2u;

    while (depth > 0)
    {
      if (itr == input.end() || std::next(itr) == input.end()) //
        return token{ token::invalid_, input, ll};

      if (*itr == '*' && *(itr + 1) == '/')
        depth--, itr += 2u, loc.column += 2u;
      if (*itr == '/' && *(itr + 1) == '*')
        depth++, itr += 2u, loc.column += 2u;
      else
      {
        if (*itr == '\n')
          loc.line ++, loc.column = 0u;
        else if (*itr == '\r')
          loc.column = 0u;
        else if (*itr == '\v')
          loc.line++;
        else
          loc.column ++;
        itr ++;
      }
    }
    return token{token::comment, std::string_view(input.begin(), itr), ll};
  }
  return token{};
}

token get_string_literal(std::string_view input, source_location & loc)
{
  if (input.size() < 2u)
    return token{token::incomplete_, input, loc};

  const auto ll = loc;

  for (auto itr = input.begin() + 1; itr != input.end(); itr++, loc.column ++)
  {
    if (*itr == '\\')
      itr ++,  loc.column ++;
    else if (*itr == '"')
    {
      loc.column ++; itr++;
      return {token::string_literal, {input.begin(), itr}, ll};
    }
    else if (*itr == '\n')
      loc.line ++, loc.column = 0u;
    else if (*itr == '\r')
      loc.column = 0u;
    else if (*itr == '\v')
      loc.line++;
  }

  return token{token::incomplete_, input, ll};
}

token get_char_literal(std::string_view input, source_location & loc)
{
  if (input.size() < 2u)
    return token{token::incomplete_, input, loc};

  const auto ll = loc;

  for (auto itr = input.begin() + 1; itr != input.end(); itr++, loc.column++)
  {
    if (*itr == '\\')
      itr ++,  loc.column ++;
    else if (*itr == '\'')
    {
      loc.column ++; itr++;
      return {token::char_literal, {input.begin(), itr}, ll};
    }
    else if (*itr == '\n')
      loc.line ++, loc.column = 0u;
    else if (*itr == '\r')
      loc.column = 0u;
    else if (*itr == '\v')
      loc.line++;
  }

  return token{token::incomplete_, input, ll};
}

token get_raw_string_literal(std::string_view input, source_location & loc)
{
  if (input.size() < 2u || input[1] != '"') // not different thing, e.g. alphanum
    return {};
  if (input.size() < 5u) // must be at least R"()"
    return {token::incomplete_, input, loc};

  const auto ll = loc;

  const auto s = input.find('(');
  if (s == npos)
    return token{token::invalid_, input, loc};

  std::string pattern = ')' + std::string{input.substr(2u, s - 2)} + '"';

  const auto e = input.find(pattern);
  if (e == npos)
    return {token::incomplete_, input, loc};

  const auto len = e + pattern.size();


  const auto res = input.substr(0, len);
  loc.line += std::count(res.begin(), res.end(), '\v');
  const auto ln = std::count(res.begin(), res.end(), '\n');
  if (ln > 0)
    loc.column += res.size();
  else
  {
    loc.line += ln;
    loc.column =  res.rfind('\n') - 1;
  }

  return {token::raw_string_literal, res, ll};
}

token get_interpolation_piece(std::string_view  input, source_location & loc)
{
  if (input.size() < 2u)
    return {token::incomplete_, input, loc};

  const auto ll = loc;

  for (auto itr = input.begin() + 1; itr != input.end(); itr++, loc.column ++)
  {
    if (*itr == '\\')
      itr ++,  loc.column ++;
    else if (*itr == '{')
    {
      loc.column ++; itr++;
      return token{input[0] == '`' ? token::interpolation_string_start  : token::interpolation_string_mid,
                   {input.begin(), itr}, ll};
    }
    else if (*itr == '`')
    {
      loc.column ++; itr++;
      return token{input[0] == '`' ? token::interpolation_string_single : token::interpolation_string_end,
                   {input.begin(), itr}, ll};
    }
    else if (*itr == '\n')
      loc.line ++, loc.column = 0u;
    else if (*itr == '\r')
      loc.column = 0u;
    else if (*itr == '\v')
      loc.line++;

  }

  return token{token::incomplete_, input, ll};
}

token get_numeric_token(std::string_view input, source_location & loc)
{
  auto itr = input.begin();
  const auto in = [&itr](std::string_view set) { return set.find(*itr) != npos;};
  const auto ll = loc;

  if (input[0] == '.')  // double
  {
    itr++, loc.column++;
    if (itr == input.end() )
      return {token::dot, input.substr(0, 1), ll};

    if (*itr == '.')
    {
      loc.column ++;
      return {token::rng, input.substr(0, 2), ll};
    }

   after_comma:
    while (itr != input)
    {
      if (in("1234567890'"))
        itr++, loc.column++;
      else if (*itr == 'e') has_e:
      {
        itr ++, loc.column++;
        if (itr == input.end())
          return {token::incomplete_, input, loc};
        if (*itr == '-' || *itr == '+')
          itr++, loc.column++;
      }
    }
    return {token::float_, input, ll};
  }
  else if (input[0] == '0') // type prefix
  {
    itr ++, loc.column++;
    if (itr == input.end() || !in("0123456789box")) // '0' is a valid number
      return {token::int_, input, ll};

    std::size_t e;
    if (*itr == 'b')
      e = input.find_first_not_of("01'", 2u);
    else if (*itr == 'o')
      e = input.find_first_not_of("01234567", 2u);
    else if (*itr == 'x')
      e = input.find_first_not_of("0123456789ABCDEFabcdef", 2u);
    else
    {
      e = input.find_first_not_of("0123456789", 2u);
      loc.column += (e - 1);
      return {token::int_, input.substr(0, e), ll};
    }

    return {e == 2 ? token::invalid_ : token::int_, input.substr(0, e), ll};
  }
  else if (in("123456789")) // reg number
  {
    for (itr++, loc.line++; itr != input.end(); itr++, loc.line++)
    {
      if (in("01234567890'"))
        continue;
      else if (*itr == '.')
      {
        if (itr +1 != input.end() && *(itr + 1) == '.')
          break;
        itr++, loc.column++;

        goto after_comma;
      }
      else if (*itr == 'e')
        goto has_e;
      else
        break;

    }
    return {token::int_, {input.begin(), itr}, ll};
  }
  return {token::invalid_, input, ll};
}

token get_operator(std::string_view input, source_location & loc)
{
  std::pair<std::string_view, token::type_t> operators[] =
      {
          {"+=",   token::add_assign},
          {"-=",   token::sub_assign},
          {"*=",   token::mul_assign},
          {"/=",   token::div_assign},
          {"%=",   token::mod_assign},
          {"&=",   token::and_assign},
          {"|=",   token:: or_assign},
          {"^=",   token::xor_assign},
          {">>=",  token::rsh_assign},
          {"<<=",  token::lsh_assign},
          {"<~>=", token::sfl_assign},
          {"=>",   token::arrow},
          {".*",   token::dot_deref},
          {"->",   token::deref},
          {"?.",   token::opt_mem},
          {"++",   token::inc},
          {"--",   token::dec},
          {"+",    token::plus},
          {"-",    token::minus},
          {"*",    token::mul},
          {"/",    token::div},
          {"%",    token::mod},
          {"&",    token::and_},
          {"|",    token::or_},
          {"^",    token::xor_},
          {"~",    token::not_},
          {"<<",   token::lsh},
          {">>",   token::rsh},
          {"/",    token::div},
          {"<~>",  token::sfl},
          {"$",    token::cc},
          {"==",   token::equal},
          {">=",   token::ge},
          {"<=",   token::le},
          {">",    token::gt},
          {"<",    token::lt},
          {"<=>",  token::cmp},
          {"!",    token::log_not},
          {"&&",   token::log_and},
          {"||",   token::log_or},
          {"^^",   token::log_xor},
          {"?",    token::qm},
          {"@",    token::at},
          {"..",   token::rng},
          {":",    token::colon},
          {".",    token::dot},
          {"#",    token::str},
          {"=",    token::assign},
          {",",    token::comma},
          {";",    token::semicolon},
          {"(",    token::par_open},
          {")",    token::par_close},
          {"{",    token::curly_open},
          {"}",    token::curly_close},
          {"[[",   token::attr_open},
          {"]]",   token::attr_close},
          {"[",    token::square_open},
          {"]",    token::square_close}
      };

  const auto ll = loc;
  for (auto & [s, t] : operators)
  {
    if (input.starts_with(s))
    {
      loc.column += s.size();
      return {t, input.substr(0, s.size()), ll};
    }

  }
  return token{};
}

token get_token(std::string_view input, source_location & loc)
{
  if (input.empty())
    return token{.type=token::eof};

  const auto c = input.front();

  if ("\n\v\r\t "sv.find(c) != npos)
    return get_whitespace(input, loc);

  if ("rR"sv.find(c) != npos) // can be R"(raw-string)"
  {
    auto tk = get_raw_string_literal(input, loc);
    if (tk)
      return tk;
  }
  if ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"sv.find(c) != npos)
    return get_alphanum(input, loc);

  if ("0123456789."sv.find(c) != npos)
    return get_numeric_token(input, loc);

  if (c == '\\')
    return get_placeholder_or_builtin(input, loc);

  if (c == '"')
    return get_string_literal(input, loc);

  if (c == '\'')
    return get_char_literal(input, loc);

  if (c == '`' || c == '}')
    if (const auto tk = get_interpolation_piece(input, loc);
        tk && !(c == '}' && tk.bad()))
      return tk;

  if (c == '/')
    if (auto tk = get_if_comment(input, loc); tk)
      return tk;

  auto tk = get_operator(input, loc);
  if (tk)
    return tk;
  return {token::invalid_, input, loc};
}




}
