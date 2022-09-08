#include "ast.h"

namespace ast {
Expression::~Expression() {}

NumberExpression::NumberExpression(double val) : value(val) {}

VariableExpression::VariableExpression(string name) : name(name) {}

BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                 unique_ptr<Expression> rhs)
    : op(op), lhs(move(lhs)), rhs(move(rhs)) {}

CallExpression::CallExpression(string name, vector<unique_ptr<Expression>> args)
    : name(name), args(move(args)) {}

FunctionPrototype::FunctionPrototype(string name, vector<string> args)
    : name(name), args(move(args)) {}

const string FunctionPrototype::getName() const { return name; }

Function::Function(unique_ptr<FunctionPrototype> proto,
                   unique_ptr<Expression> body)
    : proto(move(proto)), body(move(body)) {}

}; // namespace ast
