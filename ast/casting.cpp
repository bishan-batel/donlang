#include "ast.h"

namespace parser::ast {
/**
 * Cast Expression
 */
CastExpression::CastExpression(unique_ptr<Expression> expr, Primitive type)
    : expression(std::move(expr)), type(std::move(type)) {}

CastExpression::operator string() const {
  return "cast[" + string(*expression) + " to " + to_string(type) + "]";
}

Value *CastExpression::codegen(codegen::CGContext &ctx) {
  auto expr = expression->codegen(ctx);
  if (expr == nullptr) {
    throw string("Cannot cast null expression");
  }

  Type *valtype = expr->getType();

  // ignore casts to the same type
  if (valtype == primitive_to_type(ctx, type)) {
    return expr;
  }

  if (valtype->isFloatingPointTy()) {

    /**
     * FLOATING POINT CASTING
     */
    switch (type) {
    case primitive_i64:
      return ctx.builder->get()->CreateFPToSI(expr,
                                              Type::getInt64Ty(**ctx.llvm));
    case primitive_i32:
      return ctx.builder->get()->CreateFPToSI(expr,
                                              Type::getInt32Ty(**ctx.llvm));
    case primitive_bool:
      return ctx.builder->get()->CreateFCmpONE(
          expr, ConstantFP::get(**ctx.llvm, APFloat(0.0)));
    case primitive_f64:
      if (valtype->isDoubleTy()) {
        return expr;
      } else {
        return ctx.builder->get()->CreateFPExt(expr,
                                               Type::getDoubleTy(**ctx.llvm));
      }
    case primitive_f32:
      if (valtype->isFloatTy()) {
        return expr;
      } else {
        return ctx.builder->get()->CreateFPTrunc(expr,
                                                 Type::getFloatTy(**ctx.llvm));
      }
    default:
      throw runtime_error("Cannot cast to unknown type");
    }
  } else if (valtype->isIntegerTy()) {
    /**
     * INTEGER CASTING
     */

    switch (type) {
    case primitive_f64:
      return ctx.builder->get()->CreateSIToFP(expr,
                                              Type::getInt64Ty(**ctx.llvm));
    case primitive_f32:
      return ctx.builder->get()->CreateSIToFP(expr,
                                              Type::getInt32Ty(**ctx.llvm));

    case primitive_bool:
      return ctx.builder->get()->CreateICmpNE(
          expr, ConstantInt::get(**ctx.llvm, APInt(1, 0, true)));

    case primitive_i64:
      if (valtype->isIntegerTy(64)) {
        return expr;
      } else {
        return ctx.builder->get()->CreateSExt(expr,
                                              Type::getInt64Ty(**ctx.llvm));
      }

    case primitive_i32:
      if (valtype->isIntegerTy(32)) {
        return expr;
      } else {
        return ctx.builder->get()->CreateTrunc(expr,
                                               Type::getInt32Ty(**ctx.llvm));
      }

    case parser::ast::primitive_char:
      return ctx.builder->get()->CreateTrunc(expr, Type::getInt8Ty(**ctx.llvm));

    default:
      throw runtime_error("Cannot cast to unknown type");
    }
  } else if (valtype->isPointerTy()) {
    // load pointer valtype
    return ctx.builder->get()->CreateLoad(valtype->getContainedType(0), expr);
  }

  throw runtime_error("Cannot cast unknown type");
}
} // namespace parser::ast
