#include "token.h"
#include <string>

namespace lexer {
Token::Token(TokenType type) : type(type) {}

// Operators
bool const Token::operator==(const Token &obj) { return type == obj.type; }

bool const Token::operator!=(const Token &obj) { return !(*this == obj); }

#define branch(c, o)                                                           \
  case c:                                                                      \
    return o;
Operator char_to_op(char c) {

  // macros bc i miss rust and their nice macros
  // also makes the code look cleaner

  switch (c) {
    branch('(', op_openparen);
    branch(')', op_closeparen);
    branch('{', op_opencurly);
    branch('}', op_closecurly);
    branch('[', op_openbracket);
    branch(']', op_closebracket);
    branch('+', op_plus);
    branch('-', op_minus);
    branch('/', op_div);
    branch('=', op_eq);
    branch('|', op_pipe);
    branch('^', op_caret);
    branch('&', op_ampersand);
    branch('!', op_not);
    branch('%', op_mod);
  default:
    return op_invalid;
  }
}

Keyword string_to_keyword(string str) {
#define ifbranch(c, t)                                                         \
  if (str == c)                                                                \
    return t;

  ifbranch("def", keyword_def);
  ifbranch("def", keyword_def);

  return keyword_invalid;
}

}; // namespace lexer
