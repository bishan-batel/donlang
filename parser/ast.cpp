#include "ast.h"
#include "lexer/token.h"
#include <iostream>
#include <tuple>

using namespace std;
namespace parser {
namespace ast {
Expression::~Expression() {}
Expression::operator string() const { return "NULL Expression"; }

/**
 * Number Expression
 */
NumberExpression::NumberExpression(double val) : value(val) {}
NumberExpression::operator string() const { return to_string(value); }

/**
 * Variable Expression
 */
VariableExpression::VariableExpression(string name) : name(name) {}
VariableExpression::operator string() const { return name; }

/**
 * VariableDefinition
 */
VariableDefinition::VariableDefinition(string name, unique_ptr<Expression> expr)
    : name(name), expression(move(expr)) {}

VariableDefinition::operator string() const {
  auto expr_str =
      expression == nullptr ? "uninitialized" : string(*expression.get());
  return "let " + name + " = " + expr_str;
}

/**
 * Binary Expression
 */
BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                 unique_ptr<Expression> rhs)
    : op(op), lhs(move(lhs)), rhs(move(rhs)) {}

BinaryExpresion::operator string() const {
  return string(*lhs.get()) + string(lexer::OperatorToken(op)) +
         string(*rhs.get());
}

/**
 * Call Expression
 */
CallExpression::CallExpression(string name, vector<unique_ptr<Expression>> args)
    : name(name), args(move(args)) {}

CallExpression::operator string() const {
  auto str = name + "(";
  for (auto &arg : args) {
    str += string(*arg.get());
  }
  return str + ")";
}

/**
 * Function Prototype
 */
Prototype::Prototype(string name, vector<tuple<string, Primitive>> args)
    : name(name), args(move(args)) {}

const string Prototype::getName() const { return name; }

Prototype::operator string() const {
  string str = "Name: \"" + name + "\" Args: (";
  for (auto arg : args) {
    str += string(get<0>(arg)) + ":" + to_string((int)get<1>(arg)) + ",";
  }

  str += ")";
  return str;
}

/**
 * Function
 */
Function::Function(unique_ptr<Prototype> proto,
                   vector<unique_ptr<Expression>> body)
    : proto(move(proto)), body(move(body)) {}

Function::operator string() const {
  return "Function:\n\t" + string(*proto.get());
};

}; // namespace ast
}; // namespace parser
