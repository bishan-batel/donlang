#include "token.h"
#include <string>

Token::Token(tok_type type)
    : type(type) {}

// Operators
bool const Token::operator==(const Token &obj) {
  return type == obj.type;
}

bool const Token::operator!=(const Token &obj) { return !(*this == obj); }
