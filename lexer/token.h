#pragma once
#include <string>

using namespace std;

enum tok_type {
  tok_eof = -1,
  tok_identifier,
  tok_def,
  tok_strliteral,
};

class Token {
public:
  Token(tok_type type);

  tok_type type;

  string str;
  int i32;
  long i64;
  float f32;
  double f64;

  bool const operator==(Token const &obj);

  bool const operator!=(Token const &obj);
};
