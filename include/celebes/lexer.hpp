#ifndef CELEBES_LEXER_HPP
#define CELEBES_LEXER_HPP

#include <algorithm>
#include <array>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>
#include <cassert>
#include <cstdint>

namespace celebes
{

struct source_location
{
  std::string_view filename;
  std::string_view function;
  std::uint32_t line{0u}, column{0u};

  friend bool operator !=(const source_location &, const source_location &) = default;
  friend bool operator ==(const source_location &, const source_location &) = default;
};

struct token
{
  enum type_t
  {
    none_,
    invalid_,
    incomplete_,
    whitespace_,
    comment,
    class_,
    struct_,
    import_,
    include_,
    intrinsic_,
    namespace_,
    par_open,
    par_close,
    comma,
    semicolon,
    curly_open,
    curly_close,
    attr_open,
    attr_close,
    square_open,
    square_close,
    void_,
    null,
    inc,
    dec,
    add_assign,
    sub_assign,
    mul_assign,
    div_assign,
    mod_assign,
    and_assign,
     or_assign,
    xor_assign,
    lsh_assign,
    rsh_assign,
    sfl_assign,
    arrow,
    dot_deref,
    deref,
    opt_mem,
    plus,
    minus,
    mul, div, mod,
    and_, or_,
    xor_,
    lsh, rsh,
    sfl,
    cc,
    equal,
    ne,
    gt, lt, ge, le, cmp,
    not_, log_not, log_and, log_or,
    log_xor, qm,
    at, rng, colon, dot,
    assign,
    str,
    if_,
    else_,
    await,
    yield,
    const_,
    volatile_,
    atomic,
    public_,
    private_,
    protected_,
    mutable_, thread_local_,
    extern_, in, is,
    len, sizeof_, alignof_,
    new_, true_, false_, fast_, safe_, nan_, type_,
    this_,
    placeholder,
    string_literal,
    char_literal,
    raw_string_literal,
    interpolation_string_single,
    interpolation_string_start,
    interpolation_string_mid,
    interpolation_string_end,
    int_, float_, float_t, int_t,
    identifier_raw,
    eof
  } type{none_};

  std::string_view value;
  source_location loc;

  [[nodiscard]]
  operator bool () const {return type != none_;}

  bool bad() const {return type <= incomplete_;}

  friend bool operator !=(const token &, const token &) = default;
  friend bool operator ==(const token &, const token &) = default;

};



token get_token(std::string_view input, source_location & loc);

struct tokenizer
{
  struct iterator
  {
    std::string_view input;
    source_location loc;

    token current;

          token & operator*()       {return current;}
    const token & operator*() const {return current;}
    iterator & operator++()
    {
      current = input.empty() ? token{} : get_token(input, loc);
      input.remove_prefix(current.value.size());
      return *this;
    }

    iterator operator++(int)
    {
      auto pre = *this;
      current = input.empty() ? token{} : get_token(input, loc);
      input.remove_prefix(current.value.size());
      return pre;
    }
    friend bool operator !=(const iterator & lhs, const iterator &rhs)
    {
      return lhs.current != rhs.current;
    }
    friend bool operator ==(const iterator & lhs, const iterator &rhs)
    {
      return lhs.current == rhs.current;
    }
  };

  std::string_view input;
  source_location loc;

  iterator begin() { iterator itr{input, loc}; itr++; return itr;}
  iterator   end() { return iterator{"", loc}; }
};

inline tokenizer tokenize(std::string_view input, source_location loc)
{
  return tokenizer{input, loc};
}

}

#endif //CELEBES_LEXER_HPP
