#pragma once

#include "lexer/token.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
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
  map<string, Value *> namedValues;

  explicit CGContext(unique_ptr<LLVMContext> &, unique_ptr<IRBuilder<>> &,
                     unique_ptr<Module> &);
};
}; // namespace codegen

namespace parser {

namespace ast {
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
  bool mut;

public:
  VariableDefinition(string, unique_ptr<Expression>);

  explicit operator string() const override;

  Value *codegen(codegen::CGContext &ctx) override;
};

class VariableExpression : public Expression {
  string name;

public:
  explicit VariableExpression(string name);

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

enum Primitive : char {
  primitive_void = -1,
  primitive_f32,
  primitive_f64,
  primitive_i32,
  primitive_i64,
  primitive_char,
  primitive_string,
  primitive_bool,
  primitive_struct,
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

}; // namespace ast

}; // namespace parser
