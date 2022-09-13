#include "ast.h"
#include "lexer/token.h"
#include <iostream>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include <tuple>
#include <utility>

using namespace std;
using namespace llvm;

namespace codegen {

// constructo that takes context and creates IR builder
CGContext::CGContext(unique_ptr<LLVMContext> &ctx,
                     unique_ptr<IRBuilder<>> &builder,
                     unique_ptr<Module> &module) {
  this->llvm = &ctx;
  this->builder = &builder;
  this->module = &module;
  this->namedValues = map<string, Value *>();
}

} // namespace codegen

namespace parser {

namespace ast {
Expression::operator string() const { return "NULL Expression"; }

/**
 * Number Expression
 */
NumberExpression::NumberExpression(double val) : value(val) {}

NumberExpression::operator string() const { return to_string(value); }

Value *NumberExpression::codegen(codegen::CGContext &ctx) {
  // return ConstantFP::get(ctx.ctx, APFloat(value));
  //
  return ConstantFP::get(**ctx.llvm, APFloat(value));
}

/**
 * String Expression
 */
StringExpression::StringExpression(string val) : value(std::move(val)) {}

StringExpression::operator string() const { return '"' + (value) + '"'; }

Value *StringExpression::codegen(codegen::CGContext &ctx) {
  return ConstantDataArray::getString(**ctx.llvm, value);
}

/**
 * Variable Expression
 */
VariableExpression::VariableExpression(string name) : name(std::move(name)) {}

VariableExpression::operator string() const { return "variable[" + name + "]"; }

Value *VariableExpression::codegen(codegen::CGContext &ctx) {
  auto val = ctx.namedValues[name];
  if (val == nullptr) {
    throw string("Unknown variable used: '") + name + '\'';
  }
  return val;
}

/**
 * VariableDefinition
 */
VariableDefinition::VariableDefinition(string name, unique_ptr<Expression> expr)
    : name(std::move(name)), expression(std::move(expr)) {}

VariableDefinition::operator string() const {
  auto expr_str = expression == nullptr ? "uninitialized" : string(*expression);
  return "variable_init[" + name + " = " + expr_str + "]";
}

Value *VariableDefinition::codegen(codegen::CGContext &ctx) {
  // get double type
  Type *doubleType = Type::getDoubleTy(**ctx.llvm);

  // create variable
  AllocaInst *variable =
      ctx.builder->get()->CreateAlloca(doubleType, nullptr, name);

  // store value
  if (expression != nullptr) {
    ctx.builder->get()->CreateStore(expression->codegen(ctx), variable);
  }

  // add variable to named values
  ctx.namedValues[name] = variable;

  return variable;
}

/**
 * Return Expression
 */
ReturnExpression::ReturnExpression(unique_ptr<Expression> expr)
    : expr(std::move(expr)) {}

ReturnExpression::operator string() const {
  return "return[" + string(*expr) + "]";
}

Value *ReturnExpression::codegen(codegen::CGContext &ctx) {
  auto expr = this->expr->codegen(ctx);
  return ctx.builder->get()->CreateRet(expr);
}

/**
 * Binary Expression
 */
BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                 unique_ptr<Expression> rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

BinaryExpresion::operator string() const {
  return "(" + string(*lhs) + string(lexer::OperatorToken(op)) + string(*rhs) +
         ")";
}

Value *BinaryExpresion::codegen(codegen::CGContext &ctx) {
  auto lhs_val = lhs->codegen(ctx);
  auto rhs_val = rhs->codegen(ctx);

  if (lhs_val == nullptr) {
    throw runtime_error("Invalid binary expression: lhs is null");
  }

  if (rhs_val == nullptr) {
    throw runtime_error("Invalid binary expression: rhs is null");
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
  case lexer::op_eq:
    // check of lhs is a variable expression
    if (auto var = dynamic_cast<VariableExpression *>(lhs.get())) {
      ctx.builder->get()->CreateStore(rhs_val, var->codegen(ctx));
      break;
    } else {
      throw string("Invalid binary expression, lhs is not a variable");
    }
  default:
    throw string("Invalid binary expression, operator not supported (" +
                 string(lexer::OperatorToken(op)) + ")");
  }
}

/**
 * Call Expression
 */
CallExpression::CallExpression(string name, vector<unique_ptr<Expression>> args)
    : name(std::move(name)), args(std::move(args)) {}

CallExpression::operator string() const {
  auto str = "call " + name + " [args: (";
  for (auto &arg : args) {
    str += string(*arg);
  }
  return str + ")]";
}

Value *CallExpression::codegen(codegen::CGContext &ctx) {

  // Look up the name in the global module table.
  auto func = ctx.module->get()->getFunction(name);
  if (!func) {
    throw string("Undefined function: " + name);
  }

  // If argument mismatch error.
  if (func->arg_size() != args.size()) {
    throw string("Incorrect # arguments passed");
  }

  vector<Value *> arg_vals;
  for (auto &arg : args) {
    arg_vals.push_back(arg->codegen(ctx));
  }
  return ctx.builder->get()->CreateCall(func, arg_vals, "calltmp");
}

/**
 * Function Prototype
 */
Prototype::Prototype(string name, Primitive returntype,
                     vector<tuple<string, Primitive>> args)
    : name(std::move(name)), args(std::move(args)), returntype(returntype) {}

string Prototype::getName() const { return name; }

Prototype::operator string() const {
  string str = "Name: \"" + name + "\" Args: (";
  for (auto arg : args) {
    str += string(get<0>(arg)) + ":" + to_string((int)get<1>(arg)) + ",";
  }

  str += ")";
  return str;
}

llvm::Type *primitive_to_type(codegen::CGContext &ctx, Primitive prim) {
  switch (prim) {
  case Primitive::primitive_f64:
    return Type::getDoubleTy(**ctx.llvm);
  case Primitive::primitive_f32:
    return Type::getFloatTy(**ctx.llvm);
  case Primitive::primitive_char:
    return Type::getInt8Ty(**ctx.llvm);
  case Primitive::primitive_string:
    return Type::getInt8PtrTy(**ctx.llvm);
  case Primitive::primitive_void:
    return Type::getVoidTy(**ctx.llvm);
  case Primitive::primitive_i64:
    return Type::getInt64Ty(**ctx.llvm);
  case Primitive::primitive_i32:
    return Type::getInt32Ty(**ctx.llvm);
  }
}

llvm::Function *Prototype::codegen(codegen::CGContext &ctx) {
  vector<Type *> arg_types;

  for (auto arg : args) {
    arg_types.push_back(primitive_to_type(ctx, get<1>(arg)));
  }

  auto func_type =
      FunctionType::get(primitive_to_type(ctx, returntype), arg_types, false);
  auto func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                     name, *ctx.module->get());
  int idx = 0;
  for (auto &arg : func->args()) {
    arg.setName(get<0>(args[idx]));
    idx++;
  }

  return func;
}

Prototype::~Prototype() {}

/**
 * Function
 */
Function::Function(unique_ptr<Prototype> proto,
                   vector<unique_ptr<Expression>> body)
    : proto(std::move(proto)), body(std::move(body)) {}

Function::operator string() const {
  string str = "Function:\n\t" + string(*proto);
  str += "\nBody:\n";
  for (auto &expr : body) {
    if (expr != nullptr) {
      str += "\t" + string(*expr) + "\n";
    }
  }
  return str;
}

Value *Function::codegen(codegen::CGContext &ctx) {
  cout << "Parsing Function" << endl;

  auto func = proto->codegen(ctx);
  if (func == nullptr) {
    return nullptr;
  }

  auto bb = BasicBlock::Create(**ctx.llvm, "entry", func);
  ctx.builder->get()->SetInsertPoint(bb);

  // add arguments to namedValues
  for (auto &arg : func->args()) {
    ctx.namedValues[arg.getName().str()] = &arg;
  }

  for (auto &expr : body) {
    if (expr != nullptr) {
      expr->codegen(ctx);
    }
  }

  // remove arguments from named values
  for (auto &arg : func->args()) {
    ctx.namedValues[arg.getName().str()] = nullptr;
  }

  return func;
}

/**
 * ExternFunction
 */

ExternFunction::ExternFunction(unique_ptr<Prototype> proto)
    : proto(std::move(proto)) {}

ExternFunction::operator string() const {
  return "Extern Function:\n\t" + string(*proto);
}

// TODO
Value *ExternFunction::codegen(codegen::CGContext &ctx) {
  auto func = proto->codegen(ctx);

  if (func == nullptr) {
    return nullptr;
  }

  return func;
}

} // namespace ast
} // namespace parser
