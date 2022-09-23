#include "ast.h"
#include "lexer/token.h"
#include <iostream>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>
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

#include <streambuf>
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
  this->namedValues = map<string, AllocaInst *>();
}

bool CGContext::isUnsafe() { return unsafe; }

} // namespace codegen

namespace parser::ast {
Expression::operator string() const { return "NULL Expression"; }

/**
 * Variable Expression
 */
VariableExpression::VariableExpression(string name) : name(std::move(name)) {}

VariableExpression::operator string() const { return "variable[" + name + "]"; }

Value *VariableExpression::codegen(codegen::CGContext &ctx) {
  AllocaInst *alloca = ctx.namedValues[name];
  if (alloca == nullptr) {
    throw "Unknown variable used: \"" + name + '\"';
  }

  return ctx.builder->get()->CreateLoad(alloca->getAllocatedType(), alloca,
                                        alloca->getName());
  // return alloca;
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
  if (expression == nullptr) {
    throw string("Variable '" + name + "' is uninitialized");
  }

  auto expr_val = expression->codegen(ctx);

  if (expr_val == nullptr) {
    throw string("Variable '" + name + "' is uninitialized");
  }

  AllocaInst *alloca =
      ctx.builder->get()->CreateAlloca(expr_val->getType(), nullptr, name);
  ctx.namedValues[name] = alloca;

  ctx.builder->get()->CreateStore(expr_val, alloca);
  return alloca;
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
  if (lhs == nullptr) {
    throw string("LHS is null");
  }

  Value *lhs_val = lhs->codegen(ctx);

  if (rhs == nullptr) {
    return lhs_val;
  }

  // variable value storing
  if (op == lexer::op_assign) {
    if (auto var = dynamic_cast<VariableExpression *>(lhs.get())) {
      return ctx.builder->get()->CreateStore(rhs->codegen(ctx),
                                             ctx.namedValues[var->name]);
    }
  }

  if (lhs_val == nullptr) {
    throw string("Invalid binary expression: lhs is null");
  }

  Value *rhs_val = rhs->codegen(ctx);
  if (rhs_val == nullptr) {
    throw string("Invalid binary expression: rhs is null");
  }

  if (rhs_val->getType() != lhs_val->getType()) {
    throw string("Invalid binary expression: types mismatch");
  }

  /**
   * INTEGER OPERATIONS
   */
  if (lhs_val->getType()->isIntegerTy()) {
    switch (op) {
    case lexer::op_plus:
      return ctx.builder->get()->CreateAdd(lhs_val, rhs_val, "addtmp");
    case lexer::op_minus:
      return ctx.builder->get()->CreateSub(lhs_val, rhs_val, "subtmp");
    case lexer::op_mul:
      return ctx.builder->get()->CreateMul(lhs_val, rhs_val, "multmp");
    case lexer::op_div:
      return ctx.builder->get()->CreateSDiv(lhs_val, rhs_val, "divtmp");
    case lexer::op_mod:
      return ctx.builder->get()->CreateSRem(lhs_val, rhs_val, "modtmp");
    case lexer::op_greater_than:
      return ctx.builder->get()->CreateICmpSGT(lhs_val, rhs_val, "gttmp");
    case lexer::op_less_than:
      return ctx.builder->get()->CreateICmpSLT(lhs_val, rhs_val, "lttmp");
    case lexer::op_equal:
      return ctx.builder->get()->CreateICmpEQ(lhs_val, rhs_val, "eqtmp");
    case lexer::op_or:
      return ctx.builder->get()->CreateOr(lhs_val, rhs_val, "ortmp");
    case lexer::op_and:
      return ctx.builder->get()->CreateAnd(lhs_val, rhs_val, "andtmp");
    default:
      throw string("Invalid binary expression, operator not supported for "
                   "integer values (" +
                   string(lexer::OperatorToken(op)) + ")");
    }
  } else if (lhs_val->getType()->isFloatingPointTy()) {
    /**
     * FLOAT OPERATIONS
     */
    switch (op) {
    case lexer::op_plus:
      return ctx.builder->get()->CreateFAdd(lhs_val, rhs_val, "addtmp");
    case lexer::op_minus:
      return ctx.builder->get()->CreateFSub(lhs_val, rhs_val, "subtmp");
    case lexer::op_mul:
      return ctx.builder->get()->CreateFMul(lhs_val, rhs_val, "multmp");
    case lexer::op_div:
      return ctx.builder->get()->CreateFDiv(lhs_val, rhs_val, "divtmp");
    case lexer::op_mod:
      return ctx.builder->get()->CreateFRem(lhs_val, rhs_val, "modtmp");
    case lexer::op_greater_than:
      return ctx.builder->get()->CreateFCmpOGT(lhs_val, rhs_val, "gttmp");
    case lexer::op_less_than:
      return ctx.builder->get()->CreateFCmpOLT(lhs_val, rhs_val, "lttmp");
    case lexer::op_equal:
      return ctx.builder->get()->CreateFCmpOEQ(lhs_val, rhs_val, "eqtmp");
    default:
      throw string("Invalid binary expression, operator not supported for "
                   "floating point values (" +
                   string(lexer::OperatorToken(op)) + ")");
    }
  } else {
    throw string("Invalid binary expression, unsupported type");
  }
  return nullptr;
}

/**
 * Unary Expression
 */
UnaryExpression::UnaryExpression(lexer::Operator op,
                                 unique_ptr<Expression> expr)
    : op(op), expression(std::move(expr)) {}

UnaryExpression::operator string() const {
  return "(" + string(lexer::OperatorToken(op)) + string(*expression) + ")";
}

Value *parser::ast::UnaryExpression::codegen(codegen::CGContext &ctx) {
  if (expression == nullptr) {
    throw string("Unary expression is null");
  }

  Value *expr_val = expression->codegen(ctx);

  if (expr_val == nullptr) {
    throw string("Invalid unary expression: expr is null");
  }

  switch (op) {
  case lexer::op_minus:
    return ctx.builder->get()->CreateFNeg(expr_val, "negtmp");
  case lexer::op_not:
    return ctx.builder->get()->CreateNot(expr_val, "nottmp");
  case lexer::op_plus:
    return expr_val;
  case lexer::op_ref:
    // create pointer to value
    return ctx.builder->get()->CreateAlloca(expr_val->getType());
  case lexer::op_deref:
    
  default:
    throw string("Invalid unary expression, operator not supported (" +
                 string(lexer::OperatorToken(op)) + ")");
  }
  return nullptr;
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
  for (auto &arg : func->args()) {
  }

  vector<Value *> arg_vals;
  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    arg_vals.push_back(args[i]->codegen(ctx));
    if (!arg_vals.back()) {
      throw string("Invalid argument passed to function " + string(name));
    }
  }

  if (func->getReturnType()->isVoidTy()) {
    return ctx.builder->get()->CreateCall(func, arg_vals);
  } else {
    return ctx.builder->get()->CreateCall(func, arg_vals, "calltmp");
  }
}

/**
 * If Expression
 */

IfExpression::IfExpression(unique_ptr<Expression> cond,
                           vector<unique_ptr<Expression>> then_expr,
                           vector<unique_ptr<Expression>> else_expr)
    : condition(std::move(cond)), then(std::move(then_expr)),
      otherwise(std::move(else_expr)) {}

IfExpression::operator string() const {
  auto str = "if (" + string(*condition) + ") then (";

  for (auto &expr : then) {
    str += string(*expr);
  }

  str += ") else (";

  for (auto &expr : otherwise) {
    str += string(*expr);
  }

  return str + ")";
}

Value *IfExpression::codegen(codegen::CGContext &ctx) {
  Value *cond = condition->codegen(ctx);
  if (cond == nullptr) {
    throw string("Invalid if expression: condition is null");
  }

  // convert condition to a bool by comparing equal to 0
  cond = ctx.builder->get()->CreateICmpNE(
      cond,
      ConstantInt::get(ctx.builder->get()->getContext(), APInt(1, 0, true)),
      "ifcond");

  llvm::Function *func = ctx.builder->get()->GetInsertBlock()->getParent();

  // create blocks for then and else
  BasicBlock *then_block = BasicBlock::Create(**ctx.llvm, "then", func);

  BasicBlock *else_block = BasicBlock::Create(**ctx.llvm, "else");
  BasicBlock *merge_block = BasicBlock::Create(**ctx.llvm, "ifcont");

  ctx.builder->get()->CreateCondBr(cond, then_block, else_block);

  ctx.builder->get()->SetInsertPoint(then_block);

  Value *then_val = nullptr;
  for (auto &expr : then) {
    then_val = expr->codegen(ctx);
  }

  if (then_val == nullptr) {
    throw string("Invalid if expression: then expression is null");
  }

  // create an unconditional branch to the merge block and insert it
  ctx.builder->get()->CreateBr(merge_block);

  then_block = ctx.builder->get()->GetInsertBlock();
  func->getBasicBlockList().push_back(else_block);
  ctx.builder->get()->SetInsertPoint(else_block);

  Value *else_val = nullptr;
  for (auto &expr : otherwise) {
    else_val = expr->codegen(ctx);
  }

  ctx.builder->get()->CreateBr(merge_block);
  else_block = ctx.builder->get()->GetInsertBlock();

  func->getBasicBlockList().push_back(merge_block);
  ctx.builder->get()->SetInsertPoint(merge_block);

  PHINode *phi =
      ctx.builder->get()->CreatePHI(Type::getDoubleTy(**ctx.llvm), 2, "iftmp");

  return phi;
  phi->addIncoming(then_val, then_block);
  phi->addIncoming(else_val, else_block);
  return phi;
}

/**
 * While Expression
 */

WhileExpression::WhileExpression(unique_ptr<Expression> cond,
                                 vector<unique_ptr<Expression>> body)
    : condition(std::move(cond)), body(std::move(body)) {}

WhileExpression::operator string() const {
  auto str = "while (" + string(*condition) + ") do (";

  for (auto &expr : body) {
    str += string(*expr);
  }

  return str + ")";
}

Value *WhileExpression::codegen(codegen::CGContext &ctx) {
  auto func = ctx.builder->get()->GetInsertBlock()->getParent();

  // create blocks for then and else
  BasicBlock *cond_block = BasicBlock::Create(**ctx.llvm, "whilecond", func);
  BasicBlock *body_block = BasicBlock::Create(**ctx.llvm, "whilebody");
  BasicBlock *merge_block = BasicBlock::Create(**ctx.llvm, "whilecont");

  ctx.builder->get()->CreateBr(cond_block);
  ctx.builder->get()->SetInsertPoint(cond_block);

  Value *cond = condition->codegen(ctx);
  if (cond == nullptr) {
    throw string("Invalid while expression: condition is null");
  }

  // convert condition to a bool by comparing equal to 0
  cond = ctx.builder->get()->CreateICmpNE(
      cond,
      ConstantInt::get(ctx.builder->get()->getContext(), APInt(1, 0, true)),
      "whilecond");

  ctx.builder->get()->CreateCondBr(cond, body_block, merge_block);

  func->getBasicBlockList().push_back(body_block);
  ctx.builder->get()->SetInsertPoint(body_block);

  Value *body_val = nullptr;
  for (auto &expr : body) {
    body_val = expr->codegen(ctx);
  }

  ctx.builder->get()->CreateBr(cond_block);
  body_block = ctx.builder->get()->GetInsertBlock();

  func->getBasicBlockList().push_back(merge_block);
  ctx.builder->get()->SetInsertPoint(merge_block);

  return Constant::getNullValue(Type::getDoubleTy(**ctx.llvm));
}
/**
 * Function Prototype
 */
Prototype::Prototype(string name, Primitive returntype,
                     vector<tuple<string, Primitive>> args, bool varargs)
    : name(std::move(name)), args(std::move(args)), returntype(returntype),
      varargs(varargs) {}

string Prototype::getName() const { return name; }

Prototype::operator string() const {
  string str = "Name: \"" + name + "\" Args: (";
  for (auto arg : args) {
    str += string(get<0>(arg)) + ":" + to_string((int)get<1>(arg)) + ",";
  }

  str += ")";
  return str;
}

Primitive primitive_from_type(Type *type) {
  if (type->isDoubleTy()) {
    return primitive_f64;
  } else if (type->isFloatTy()) {
    return primitive_f32;
  } else if (type->isVoidTy()) {
    return primitive_void;
  } else if (type->isIntegerTy(64)) {
    return primitive_i64;
  } else if (type->isIntegerTy(32)) {
    return primitive_i32;
  } else if (type->isIntegerTy(8)) {
    return primitive_char;
  } else if (type->isIntegerTy(1)) {
    return primitive_bool;
  } else {
    throw string("Unknown primitive type");
  }
}

llvm::Type *primitive_to_type(codegen::CGContext &ctx, Primitive prim) {
  auto isptr = ast::is_primitive_ptr(prim);
  if (isptr)
    prim = ast::primitive_flip_ptr(prim);

  Type *ty;
  switch (prim) {
  case Primitive::primitive_f64:
    ty = Type::getDoubleTy(**ctx.llvm);
    break;
  case Primitive::primitive_f32:
    ty = Type::getFloatTy(**ctx.llvm);
    break;
  case Primitive::primitive_char:
    ty = Type::getInt8Ty(**ctx.llvm);
    break;
  case Primitive::primitive_string:
    ty = Type::getInt8PtrTy(**ctx.llvm);
    break;
  case Primitive::primitive_void:
    ty = Type::getVoidTy(**ctx.llvm);
    break;
  case Primitive::primitive_i64:
    ty = Type::getInt64Ty(**ctx.llvm);
    break;
  case Primitive::primitive_i32:
    ty = Type::getInt32Ty(**ctx.llvm);
    break;
  case Primitive::primitive_bool:
    ty = Type::getInt1Ty(**ctx.llvm);
    break;
  default:
    throw "Invalid primitive type: " + to_string(prim) + ", " +
        to_string(parser::ast::primitive_flip_ptr(prim));
  }

  if (isptr) {
    ty = ty->getPointerTo();
  }
  return ty;
}

llvm::Function *Prototype::codegen(codegen::CGContext &ctx) {
  vector<Type *> arg_types;

  for (auto arg : args) {
    arg_types.push_back(primitive_to_type(ctx, get<1>(arg)));
  }

  auto func_type =
      FunctionType::get(primitive_to_type(ctx, returntype), arg_types, varargs);
  auto func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                     name, *ctx.module->get());

  int idx = 0;
  for (auto &arg : func->args()) {
    arg.setName(get<0>(args[idx++]));
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
  auto func = proto->codegen(ctx);
  if (func == nullptr) {
    return nullptr;
  }

  auto bb = BasicBlock::Create(**ctx.llvm, "entry", func);
  ctx.builder->get()->SetInsertPoint(bb);

  // add arguments to namedValues
  for (auto &arg : func->args()) {
    // ctx.namedValues[arg.getName().str()] = &arg;

    // create an alloca for this variable
    AllocaInst *alloca = ctx.builder->get()->CreateAlloca(
        arg.getType(), 0, arg.getName().str().c_str());

    // store the initial value into the alloca
    ctx.builder->get()->CreateStore(&arg, alloca);

    // add the alloca to the symbol table
    ctx.namedValues[arg.getName().str()] = alloca;
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

  if (ctx.builder->get()->GetInsertBlock()->getTerminator() == nullptr) {
    ctx.builder->get()->CreateRetVoid();
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

Value *ExternFunction::codegen(codegen::CGContext &ctx) {
  return proto->codegen(ctx);
}

void add_default_functions(codegen::CGContext &ctx) {}

} // namespace parser::ast
