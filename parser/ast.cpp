#include "ast.h"
#include "lexer/token.h"
#include <iostream>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include <tuple>

using namespace std;
using namespace llvm;
namespace parser {
namespace ast {
Expression::operator string() const { return "NULL Expression"; }

/**
 * Number Expression
 */
NumberExpression::NumberExpression(double val) : value(val) {}
NumberExpression::operator string() const { return to_string(value); }
Value *NumberExpression::codegen(LLVMContext &ctx) {
  return ConstantFP::get(ctx, APFloat(value));
}

/**
 * String Expression
 */
StringExpression::StringExpression(string val) : value(val) {}
StringExpression::operator string() const { return '"' + (value) + '"'; }
Value *StringExpression::codegen(LLVMContext &ctx) {
  return ConstantDataArray::getString(ctx, value);
}

/**
 * Variable Expression
 */
VariableExpression::VariableExpression(string name) : name(name) {}
VariableExpression::operator string() const { return name; }
Value *VariableExpression::codegen(LLVMContext &ctx) { return nullptr; }

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

Value *VariableDefinition::codegen(LLVMContext &ctx) {
  auto expr = expression->codegen(ctx);
  return expr;
}

/**
 * Binary Expression
 */
BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                 unique_ptr<Expression> rhs)
    : op(op), lhs(move(lhs)), rhs(move(rhs)) {}

BinaryExpresion::operator string() const {
  return "(" + string(*lhs.get()) + string(lexer::OperatorToken(op)) +
         string(*rhs.get()) + ")";
}
Value *BinaryExpresion::codegen(LLVMContext &ctx) {
  auto lhs_val = lhs->codegen(ctx);
  auto rhs_val = rhs->codegen(ctx);

  if (lhs_val == nullptr || rhs_val == nullptr) {
    return nullptr;
  }
  switch (op) {
  case lexer::op_plus:
    return BinaryOperator::CreateFAdd(lhs_val, rhs_val, "addtmp");
  case lexer::op_minus:
    return BinaryOperator::CreateFSub(lhs_val, rhs_val, "subtmp");
  case lexer::op_mul:
    return BinaryOperator::CreateFMul(lhs_val, rhs_val, "multmp");
  case lexer::op_div:
    return BinaryOperator::CreateFDiv(lhs_val, rhs_val, "divtmp");
  case lexer::op_mod:
    return BinaryOperator::CreateFRem(lhs_val, rhs_val, "modtmp");
  default:
    // TODO err;
    return nullptr;
  }
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

Value *CallExpression::codegen(LLVMContext &ctx) { return nullptr; }

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
  string str = "Function:\n\t" + string(*proto.get());
  str += "\nBody:\n";
  for (auto &expr : body) {
    if (expr != nullptr) {
      str += "\t" + string(*expr.get()) + "\n";
    }
  }
  return str;
}

Value *Function::codegen(LLVMContext &ctx) {
  auto func =
      llvm::Function::Create(FunctionType::get(Type::getDoubleTy(ctx), false),
                             llvm::Function::ExternalLinkage, proto->getName());
  auto bb = BasicBlock::Create(ctx, "entry", func);
  IRBuilder<> builder(ctx);
  builder.SetInsertPoint(bb);

  for (auto &expr : body) {
    if (expr != nullptr) {
      expr->codegen(ctx);
    }
  }

  builder.CreateRet(ConstantFP::get(ctx, APFloat(0.0)));
  return func;
}

}; // namespace ast
}; // namespace parser
