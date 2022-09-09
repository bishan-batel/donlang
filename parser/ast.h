#pragma once
#include "lexer/token.h"
#include <memory>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

namespace parser {
namespace ast {

class Expression {
public:
  virtual ~Expression();
  virtual operator string() const;
};

class NumberExpression : public Expression {
  double value;

public:
  NumberExpression(double val);
  operator string() const override;
};

class VariableDefinition : public Expression {
  string name;
  unique_ptr<Expression> expression;

public:
  VariableDefinition(string, unique_ptr<Expression>);
  operator string() const override;
};

class VariableExpression : public Expression {
  string name;

public:
  VariableExpression(string name);
  operator string() const override;
};

class BinaryExpresion : public Expression {
  lexer::Operator op;
  unique_ptr<Expression> lhs, rhs;

public:
  BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                  unique_ptr<Expression> rhs);
  operator string() const override;
};

class CallExpression : public Expression {
  string name;
  vector<unique_ptr<Expression>> args;

public:
  CallExpression(string name, vector<unique_ptr<Expression>> args);
  operator string() const override;
};

enum Primitive : char {
  primitive_f32,
  primitive_f64,
  primitive_i32,
  primitive_i64,
  primitive_char,
};

/**
 * NON EXPRESSIONS
 */

class Prototype {
  string name;
  vector<tuple<string, Primitive>> args;

public:
  Prototype(string name, vector<tuple<string, Primitive>> args);
  const string getName() const;
  operator string() const;
};

class Function {
  unique_ptr<Prototype> proto;
  vector<unique_ptr<Expression>> body;

public:
  Function(unique_ptr<Prototype> proto, vector<unique_ptr<Expression>> body);
  operator string() const;
};

}; // namespace ast

}; // namespace parser
