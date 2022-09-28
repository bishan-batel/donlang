#include "types.h"
#include "ast/ast.h"

namespace parser::ast {

string I32Type::getName() const { return "i32"; }
llvm::Type *I32Type::getType(codegen::CGContext &ctx) const {
  return llvm::Type::getInt32Ty(**ctx.llvm);
}
size_t I32Type::getSize() const { return sizeof(int); }
bool I32Type::isIntegerTy() const { return true; }
bool I32Type::isFloating() const { return false; }
bool I32Type::isObject() const { return false; }
bool I32Type::isPointer() const { return false; }
bool I32Type::getPointerTo() const { return false; }

string I64Type::getName() const { return "i64"; }
llvm::Type *I64Type::getType(codegen::CGContext &ctx) const {
  return llvm::Type::getInt64Ty(**ctx.llvm);
}
size_t I64Type::getSize() const { return sizeof(long); }
bool I64Type::isIntegerTy() const { return true; }
bool I64Type::isFloating() const { return false; }
bool I64Type::isObject() const { return false; }
bool I64Type::isPointer() const { return false; }
bool I64Type::getPointerTo() const { return false; }

string F32Type::getName() const { return "f32"; }
llvm::Type *F32Type::getType(codegen::CGContext &ctx) const {
  return llvm::Type::getFloatTy(**ctx.llvm);
}
size_t F32Type::getSize() const { return sizeof(float); }
bool F32Type::isIntegerTy() const { return false; }
bool F32Type::isFloating() const { return true; }
bool F32Type::isObject() const { return false; }
bool F32Type::isPointer() const { return false; }
bool F32Type::getPointerTo() const { return false; }

string F64Type::getName() const { return "f64"; }
llvm::Type *F64Type::getType(codegen::CGContext &ctx) const {
  return llvm::Type::getDoubleTy(**ctx.llvm);
}
size_t F64Type::getSize() const { return sizeof(double); }
bool F64Type::isIntegerTy() const { return false; }
bool F64Type::isFloating() const { return true; }
bool F64Type::isObject() const { return false; }
bool F64Type::isPointer() const { return false; }
bool F64Type::getPointerTo() const { return false; }

PointerType::PointerType(DonType *type) : containingType(type) {}
string PointerType::getName() const { return containingType->getName() + "*"; }
llvm::Type *PointerType::getType(codegen::CGContext &ctx) const {
  return containingType->getType(ctx)->getPointerTo();
}
size_t PointerType::getSize() const { return sizeof(void *); }
bool PointerType::isIntegerTy() const { return false; }
bool PointerType::isFloating() const { return false; }
bool PointerType::isObject() const { return false; }
bool PointerType::isPointer() const { return true; }
bool PointerType::getPointerTo() const { return false; }
} // namespace parser::ast
