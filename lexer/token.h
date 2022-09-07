#pragma once
#include <string>
#include <tuple>

using namespace std;
namespace lexer {

enum Keyword : char {
  keyword_invalid = -1,
  keyword_def,
  keyword_return,
  keyword_i32,
  keyword_f32,
  keyword_f64,
  keyword_char,
  keyword_void,
  keyword_var,
  keyword_let,
};

static const tuple<string, Keyword> KEYWORD_MAP[] = {
    make_tuple("def", keyword_def),   make_tuple("return", keyword_return),
    make_tuple("i32", keyword_i32),   make_tuple("f32", keyword_f32),
    make_tuple("f64", keyword_f64),   make_tuple("char", keyword_char),
    make_tuple("void", keyword_void), make_tuple("var", keyword_var),
    make_tuple("let", keyword_let),
};

enum Operator : char {
  op_invalid = -1,
  op_openparen,
  op_closeparen,
  op_opencurly,
  op_closecurly,
  op_openbracket,
  op_closebracket,
  op_plus,
  op_minus,
  op_div,
  op_mul,
  op_eq,
  op_pipe,
  op_caret,
  op_ampersand,
  op_not,
  op_mod,
  op_colon,
  op_dot,
};

static const tuple<string, Operator> OPERATOR_MAP[] = {
    make_tuple("(", op_openparen),   make_tuple(")", op_closeparen),
    make_tuple("{", op_opencurly),   make_tuple("}", op_closecurly),
    make_tuple("[", op_openbracket), make_tuple("]", op_closebracket),
    make_tuple("+", op_plus),        make_tuple("-", op_minus),
    make_tuple("/", op_div),         make_tuple("=", op_eq),
    make_tuple("*", op_mul),         make_tuple("|", op_pipe),
    make_tuple("^", op_caret),       make_tuple("&", op_ampersand),
    make_tuple("!", op_not),         make_tuple("%", op_mod),
    make_tuple(":", op_colon),       make_tuple(".", op_dot)};

enum TokenType : char {
  tok_eof = -1,
  tok_identifier,
  tok_keyword,
  tok_strliteral,
  tok_number,
  tok_op
};
class Token {

public:
  Token(TokenType type);
  TokenType type;
  virtual string name() const;
};

class EOFToken : public Token {
public:
  EOFToken();
  string name() const;
};

class IdentifierToken : public Token {
public:
  IdentifierToken(string ident);
  string ident;
  string name() const;
};

class KeywordToken : public Token {
public:
  KeywordToken(Keyword keyword);
  Keyword word;
  string name() const;
};

class StringLiteralToken : public Token {
public:
  StringLiteralToken(string literal);
  string literal;
  string name() const;
};

class OperatorToken : public Token {
public:
  OperatorToken(Operator op);
  Operator op;
  string name() const;
};

class F64Literal : public Token {
public:
  F64Literal(double literal);
  double literal;
  string name() const;
};
}; // namespace lexer
