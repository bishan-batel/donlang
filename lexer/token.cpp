#include "token.h"
#include <iostream>
#include <string>
#include <tuple>

namespace lexer {

Token::Token(TokenType type) : type(type) {}
string Token::name() const { return "NULL Token"; }

EOFToken::EOFToken() : Token(tok_eof) {}
string EOFToken::name() const { return "EOF"; }

IdentifierToken::IdentifierToken(string ident)
    : Token(tok_identifier), ident(ident) {}
string IdentifierToken::name() const { return ident; }

KeywordToken::KeywordToken(Keyword type) : Token(tok_keyword), word(type) {}
string KeywordToken::name() const {
  for (auto keyword : KEYWORD_MAP) {
    if (get<1>(keyword) == word) {
      return get<0>(keyword);
    }
  }
  return "[NULL Keyword]";
}

StringLiteralToken::StringLiteralToken(string str)
    : Token(tok_strliteral), literal(str) {}
string StringLiteralToken::name() const { return literal; }

OperatorToken::OperatorToken(Operator op) : Token(tok_op), op(op) {}
string OperatorToken::name() const {
  for (auto op : OPERATOR_MAP) {
    if (get<1>(op) == this->op) {
      return get<0>(op);
    }
  }
  return "NULL Operator";
}

F64Literal::F64Literal(double f) : Token(tok_double), literal(f) {}

string F64Literal::name() const { return to_string(literal) + "f"; }
}; // namespace lexer
