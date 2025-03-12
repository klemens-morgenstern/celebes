#include <cstdio>

#include <celebes/lexer.hpp>

#include "doctest.h"

TEST_CASE("alphanum")
{
  auto data =
R"(foo bar /* asd */ // asdlhks
struct foo {};)";

  for (auto tk : celebes::tokenize(data, {__FILE__, __FUNCTION__, __LINE__ - 3, 3}))
  {
    printf("tk %i '%s'\n", tk.type, std::string(tk.value).c_str());
  }


}