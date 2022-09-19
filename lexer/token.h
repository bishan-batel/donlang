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
  keyword_string,
  keyword_extern,
  keyword_if,
  keyword_else,
  keyword_is,
  keyword_ptr,
  keyword_ref,
  keyword_as,
  keyword_while,
};

static const map<string, Keyword> KEYWORD_MAP{
    {"def", keyword_def},       {"return", keyword_return},
    {"i32", keyword_i32},       {"f32", keyword_f32},
    {"i8", keyword_char},       {"f64", keyword_f64},
    {"char", keyword_char},     {"void", keyword_void},
    {"var", keyword_var},       {"let", keyword_let},
    {"extern", keyword_extern}, {"string", keyword_string},
    {"if", keyword_if},         {"else", keyword_else},
    {"ptr", keyword_ptr},       {"ref", keyword_ref},
    {"is", keyword_is},         {"as", keyword_as},
    {"while", keyword_while}};

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
  op_greater_than,
  op_less_than,
  op_ptr,
  op_ref,
  op_set
};

static const map<char, Operator> OPERATOR_MAP{
    {'(', op_openparen},  {')', op_closeparen},   {'{', op_opencurly},
    {'}', op_closecurly}, {'[', op_openbracket},  {']', op_closebracket},
    {'+', op_plus},       {'-', op_minus},        {'/', op_div},
    {'=', op_eq},         {'*', op_mul},          {'|', op_pipe},
    {'^', op_caret},      {'&', op_ampersand},    {'!', op_not},
    {'%', op_mod},        {':', op_colon},        {'.', op_dot},
    {',', op_comma},      {'>', op_greater_than}, {'<', op_less_than}};

bool is_operator_char(char c);

enum TokenType : char {
  tok_eof = EOF,
  tok_identifier,
  tok_keyword,
  tok_strliteral,
  tok_op,
  tok_double,
  tok_int,
  tok_char,
  tok_float,
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

class F32Literal : public Token {
public:
  F32Literal(float literal);
  float literal;
  operator string() const override;
};

class I32Literal : public Token {
public:
  I32Literal(int literal);
  int literal;
  operator string() const override;
};

class CharLiteral : public Token {
public:
  CharLiteral(char literal);
  char literal;
  operator string() const override;
};

}; // namespace lexer
