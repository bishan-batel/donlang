#pragma once

#include "lexer/token.h"
#include <iostream>
#include <lexer/token.h>
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
#include <memory>
#include <string>
#include <tuple>
#include <vector>

using namespace std;
using namespace llvm;

namespace codegen {

class CGContext {
public:
  unique_ptr<LLVMContext> *llvm;
  unique_ptr<IRBuilder<>> *builder;
  unique_ptr<Module> *module;
  // named variables
  map<string, AllocaInst *> namedValues;

  explicit CGContext(unique_ptr<LLVMContext> &, unique_ptr<IRBuilder<>> &,
                     unique_ptr<Module> &);
};
}; // namespace codegen

namespace parser::ast {

enum Primitive : int {
  primitive_invalid = -1,
  primitive_void,
  primitive_f32,
  primitive_f64,
  primitive_i32,
  primitive_i64,
  primitive_char,
  primitive_string,
  primitive_bool,
  primitive_struct,
};

static const int PRIMITIVE_POINTER_FLAG = 1 << 31;

inline Primitive primitive_flip_ptr(Primitive primitive) {
  return (Primitive)(primitive ^ PRIMITIVE_POINTER_FLAG);
}

inline Primitive primitive_to_ptr(Primitive primitive) {
  return primitive_flip_ptr(primitive);
}

inline bool is_primitive_ptr(Primitive primitive) {
  return primitive & PRIMITIVE_POINTER_FLAG;
}

void add_default_functions(codegen::CGContext &ctx);

class Expression {
public:
  virtual ~Expression() = default;

  virtual explicit operator string() const;

  virtual Value *codegen(codegen::CGContext &ctx) = 0;
};

// Double Expression
class NumberF64Expression : public Expression {
  double value;

public:
  explicit NumberF64Expression(double val);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

// Float Expression
class NumberF32Expression : public Expression {
  float value;

public:
  explicit NumberF32Expression(float val);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

// Integer Expression
class NumberI32Expression : public Expression {
  int value;

public:
  explicit NumberI32Expression(int val);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

// I8/Char Expression
class NumberI8Expression : public Expression {
  char value;

public:
  explicit NumberI8Expression(char val);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class StringExpression : public Expression {
  string value;

public:
  explicit StringExpression(string val);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class VariableDefinition : public Expression {
  string name;
  unique_ptr<Expression> expression;

public:
  VariableDefinition(string, unique_ptr<Expression>);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class VariableExpression : public Expression {
public:
  string name;
  explicit VariableExpression(string name);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class CastExpression : public Expression {
  unique_ptr<Expression> expression;
  Primitive type;

public:
  CastExpression(unique_ptr<Expression> expression, Primitive primitive);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class BinaryExpresion : public Expression {
  lexer::Operator op;
  unique_ptr<Expression> lhs, rhs;

public:
  BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                  unique_ptr<Expression> rhs);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class UnaryExpression : public Expression {
  lexer::Operator op;
  unique_ptr<Expression> expression;

public:
  UnaryExpression(lexer::Operator op, unique_ptr<Expression> expression);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class ReturnExpression : public Expression {
  unique_ptr<Expression> expr;

public:
  ReturnExpression(unique_ptr<Expression>);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class CallExpression : public Expression {
  string name;
  vector<unique_ptr<Expression>> args;

public:
  CallExpression(string name, vector<unique_ptr<Expression>> args);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class IfExpression : public Expression {
  unique_ptr<Expression> condition;
  vector<unique_ptr<Expression>> then;
  vector<unique_ptr<Expression>> otherwise;

public:
  IfExpression(unique_ptr<Expression> condition,
               vector<unique_ptr<Expression>> then,
               vector<unique_ptr<Expression>> otherwise);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

/**
 * While loop expression
 */

class WhileExpression : public Expression {
  unique_ptr<Expression> condition;
  vector<unique_ptr<Expression>> body;

public:
  WhileExpression(unique_ptr<Expression> condition,
                  vector<unique_ptr<Expression>> body);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

/**
 * NON EXPRESSIONS
 */

class Prototype {
public:
  string name;
  Primitive returntype;
  vector<tuple<string, Primitive>> args;

  Prototype(string name, Primitive returntype,
            vector<tuple<string, Primitive>> args);

  ~Prototype();

  string getName() const;

  explicit operator string() const;

  llvm::Function *codegen(codegen::CGContext &ctx);
};

class Function : public Expression {
public:
  unique_ptr<Prototype> proto;
  vector<unique_ptr<Expression>> body;

  Function(unique_ptr<Prototype> proto, vector<unique_ptr<Expression>> body);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class ExternFunction : public Expression {
  unique_ptr<Prototype> proto;

public:
  explicit ExternFunction(unique_ptr<Prototype> proto);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

llvm::Type *primitive_to_type(codegen::CGContext &ctx, Primitive prim);
Primitive primitive_from_type(Type *type);

// Classes
class ClassDefinition : public Expression {
  string name;
  vector<unique_ptr<Expression>> body;

public:
  ClassDefinition(string name, vector<unique_ptr<Expression>> body);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class ClassAttribute : public Expression {
  string name;
  unique_ptr<Expression> expression;
  Primitive type;

public:
  ClassAttribute(string name, unique_ptr<Expression> expression);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class ClassConstructor : public Expression {
  string name;
  vector<unique_ptr<Expression>> args;

public:
  ClassConstructor(string name, vector<unique_ptr<Expression>> args);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

} // namespace parser::ast
