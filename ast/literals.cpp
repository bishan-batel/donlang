#include "ast.h"

namespace parser::ast {
/**
 * Number Expressions
 */

// Doubles
NumberF64Expression::NumberF64Expression(double val) : value(val) {}

NumberF64Expression::operator string() const { return to_string(value); }

Value *NumberF64Expression::codegen(codegen::CGContext &ctx) {
  return ConstantFP::get(**ctx.llvm, APFloat(value));
}

// Floats
NumberF32Expression::NumberF32Expression(float val) : value(val) {}

NumberF32Expression::operator string() const { return to_string(value); }

Value *NumberF32Expression::codegen(codegen::CGContext &ctx) {
  return ConstantFP::get(**ctx.llvm, APFloat(value));
}

// Integer (32 bit)
NumberI32Expression::NumberI32Expression(int val) : value(val) {}

NumberI32Expression::operator string() const { return to_string(value); }

Value *NumberI32Expression::codegen(codegen::CGContext &ctx) {
  return ConstantInt::get(**ctx.llvm, APInt(8 * sizeof(int), value));
}

// Integer (8 bit)
NumberI8Expression::NumberI8Expression(char val) : value(val) {}

NumberI8Expression::operator string() const { return to_string(value); }

Value *NumberI8Expression::codegen(codegen::CGContext &ctx) {
  return ConstantInt::get(**ctx.llvm, APInt(8 * sizeof(char), value));
}

/**
 *  String Expression
 */
StringExpression::StringExpression(string val) : value(std::move(val)) {}

StringExpression::operator string() const { return '"' + (value) + '"'; }

Value *StringExpression::codegen(codegen::CGContext &ctx) {
  return ctx.builder->get()->CreateGlobalStringPtr(value);
}
} // namespace parser::ast
