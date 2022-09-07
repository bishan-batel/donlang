#include "ast.h"

namespace ast {
Expression::~Expression() {}

NumberExpression::NumberExpression(double val) : value(val) {}

VariableExpression::VariableExpression(string name) : name(name) {}

BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                 unique_ptr<Expression> rhs)
    : op(op), lhs(move(lhs)), rhs(move(rhs)) {}



}; // namespace ast
