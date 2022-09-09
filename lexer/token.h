#pragma once
#include <map>
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
  keyword_extern
};

static const map<string, Keyword> KEYWORD_MAP{
    {"def", keyword_def},   {"return", keyword_return},
    {"i32", keyword_i32},   {"f32", keyword_f32},
    {"f64", keyword_f64},   {"char", keyword_char},
    {"void", keyword_void}, {"var", keyword_var},
    {"let", keyword_let},   {"extern", keyword_extern}};

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
  op_comma,
};

static const int MAX_OPERATOR_LENGTH = 2;
static const map<char, Operator> OPERATOR_MAP{
    {'(', op_openparen},  {')', op_closeparen},  {'{', op_opencurly},
    {'}', op_closecurly}, {'[', op_openbracket}, {']', op_closebracket},
    {'+', op_plus},       {'-', op_minus},       {'/', op_div},
    {'=', op_eq},         {'*', op_mul},         {'|', op_pipe},
    {'^', op_caret},      {'&', op_ampersand},   {'!', op_not},
    {'%', op_mod},        {':', op_colon},       {'.', op_dot},
    {',', op_comma}};

bool is_operator_char(char c);

enum TokenType : char {
  tok_eof = -1,
  tok_identifier,
  tok_keyword,
  tok_strliteral,
  tok_op,
  tok_double,
  tok_int,
  tok_char,
};
class Token {

public:
  Token(TokenType type);
  TokenType type;
  virtual operator std::string() const;
};

class EOFToken : public Token {
public:
  EOFToken();
  operator string() const override;
};

class IdentifierToken : public Token {
public:
  IdentifierToken(string ident);
  string ident;
  operator string() const override;
};

class KeywordToken : public Token {
public:
  KeywordToken(Keyword keyword);
  Keyword word;
  operator string() const override;
};

class StringLiteralToken : public Token {
public:
  StringLiteralToken(string literal);
  string literal;
  operator string() const override;
};

class OperatorToken : public Token {
public:
  OperatorToken(Operator op);
  Operator op;
  operator string() const override;
};

class F64Literal : public Token {
public:
  F64Literal(double literal);
  double literal;
  operator string() const override;
};
}; // namespace lexer
