#pragma once
#include <map>
#include <string>
#include <tuple>

using namespace std;
namespace lexer {

enum Keyword : char {
  keyword_invalid = -1,

  // Function
  keyword_def,
  keyword_extern,
  keyword_varargs,

  // Types
  keyword_i32,
  keyword_f32,
  keyword_f64,
  keyword_char,
  keyword_void,
  keyword_string,
  keyword_bool,

  // Variables
  keyword_var,
  keyword_let,

  // operations
  keyword_is,
  keyword_and,
  keyword_not,
  keyword_or,

  // pointers / casting
  keyword_ptr,
  keyword_ref,
  keyword_deref,
  keyword_as,

  // Control Flow
  keyword_if,
  keyword_else,
  keyword_return,
  keyword_unsafe,

  // loops
  keyword_while,
  keyword_for,

  // boolean
  keyword_true,
  keyword_false,

  // classes
  keyword_class,
  keyword_data,
  keyword_constructor,
  keyword_self,
  keyword_public,
  keyword_private,
  keyword_static,

  // modules
  keyword_package,
  keyword_import,
  keyword_export,
  keyword_from
};

static const map<string, Keyword> KEYWORD_MAP{
    {"def", keyword_def},
    {"extern", keyword_extern},
    {"i32", keyword_i32},
    {"f32", keyword_f32},
    {"f64", keyword_f64},
    {"char", keyword_char},
    {"void", keyword_void},
    {"string", keyword_string},
    {"bool", keyword_bool},
    {"var", keyword_var},
    {"let", keyword_let},
    {"is", keyword_is},
    {"and", keyword_and},
    {"not", keyword_not},
    {"ptr", keyword_ptr},
    {"ref", keyword_ref},
    {"as", keyword_as},
    {"if", keyword_if},
    {"else", keyword_else},
    {"return", keyword_return},
    {"unsafe", keyword_unsafe},
    {"while", keyword_while},
    {"for", keyword_for},
    {"true", keyword_true},
    {"false", keyword_false},
    {"class", keyword_class},
    {"data", keyword_data},
    {"constructor", keyword_constructor},
    /*{"self", keyword_self},*/ {"pub", keyword_public},
    {"priv", keyword_private},
    {"static", keyword_static},
    {"package", keyword_package},
    {"import", keyword_import},
    {"export", keyword_export},
    {"from", keyword_from},
    {"varargs", keyword_varargs},
    {"deref", keyword_deref}};

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
  op_assign,
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
  op_deref,
  op_equal,
  op_and,
  op_or,
};

static const map<char, Operator> OPERATOR_MAP{
    {'(', op_openparen},  {')', op_closeparen},   {'{', op_opencurly},
    {'}', op_closecurly}, {'[', op_openbracket},  {']', op_closebracket},
    {'+', op_plus},       {'-', op_minus},        {'/', op_div},
    {'=', op_assign},     {'*', op_mul},          {'|', op_pipe},
    {'^', op_caret},      {'&', op_ampersand},    {'!', op_not},
    {'%', op_mod},        {':', op_colon},        {'.', op_dot},
    {',', op_comma},      {'>', op_greater_than}, {'<', op_less_than},
};

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
