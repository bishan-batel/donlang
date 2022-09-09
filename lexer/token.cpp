#include "token.h"
#include <iostream>
#include <string>
#include <tuple>

namespace lexer {

bool is_operator_char(char c) {
  auto op = OPERATOR_MAP.find(c);
  return op != OPERATOR_MAP.end();
}

Token::Token(TokenType type) : type(type) {}
Token::operator string() const { return "NULL Token"; }

EOFToken::EOFToken() : Token(tok_eof) {}
EOFToken::operator string() const { return "EOF"; }

IdentifierToken::IdentifierToken(string ident)
    : Token(tok_identifier), ident(ident) {}
IdentifierToken::operator string() const { return ident; }

KeywordToken::KeywordToken(Keyword type) : Token(tok_keyword), word(type) {}
KeywordToken::operator string() const {
  for (auto keyword : KEYWORD_MAP) {
    if (get<1>(keyword) == word) {
      return get<0>(keyword);
    }
  }
  return "[NULL Keyword]";
}

StringLiteralToken::StringLiteralToken(string str)
    : Token(tok_strliteral), literal(str) {}
StringLiteralToken::operator string() const { return "\"" + literal + "\""; }

OperatorToken::OperatorToken(Operator op) : Token(tok_op), op(op) {}
OperatorToken::operator string() const {
  for (auto &p : OPERATOR_MAP) {
    if (p.second == op) {
      return string(1, p.first);
    }
  }
  return "NULL Operator";
}

F64Literal::F64Literal(double f) : Token(tok_double), literal(f) {}

F64Literal::operator string() const { return to_string(literal) + "f"; }
}; // namespace lexer
