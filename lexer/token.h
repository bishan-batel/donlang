#pragma once
#include <string>

using namespace std;
namespace lexer {

enum TokenType : char {
  tok_eof = -1,
  tok_identifier,
  tok_keyword,
  tok_def,
  tok_strliteral,
  tok_op
};

enum Keyword : char {
  keyword_invalid=-1,
  keyword_def,
};

enum Operator : char {
  op_invalid=-1,
  op_openparen,
  op_closeparen,
  op_opencurly,
  op_closecurly,
  op_openbracket,
  op_closebracket,
  op_plus,
  op_minus,
  op_div,
  op_eq,
  op_pipe,
  op_caret,
  op_ampersand,
  op_not,
  op_mod,
};

Operator char_to_op(char c);
Keyword string_to_keyword(string c);

class Token {
public:
  Token(TokenType type);

  TokenType type;

  string str;
  int i32;
  long i64;
  float f32;
  double f64;
  char u8;

  bool const operator==(Token const &obj);

  bool const operator!=(Token const &obj);
};

}; // namespace lexer
