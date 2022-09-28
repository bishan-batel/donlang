#pragma once
#include "ast.h"
#include <cstddef>
#include <llvm/IR/Type.h>

namespace parser::ast {

class DonType {
public:
  virtual string getName() const;
  virtual llvm::Type *getType(codegen::CGContext &ctx) const;
  virtual size_t getSize() const;
  virtual bool isIntegerTy() const;
  virtual bool isFloating() const;
  virtual bool isObject() const;
  virtual bool isPointer() const;
  virtual DonType getContainingType() const;
  virtual bool getPointerTo() const;

  static DonType getI32Type();
  static DonType getI64Type();
  static DonType getF32Type();
  static DonType getF64Type();
  static DonType getVoidType();
  static DonType getPointerType(DonType type);
};

class I32Type : DonType {
public:
  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  bool getPointerTo() const override;
};

class I64Type : DonType {
public:
  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  bool getPointerTo() const override;
};

class F32Type : DonType {
public:
  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  bool getPointerTo() const override;
};

class F64Type : DonType {
public:
  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  bool getPointerTo() const override;
};

class ObjectType : DonType {
public:
  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  bool getPointerTo() const override;
};

class PointerType : DonType {
  DonType *containingType;

public:
  PointerType(DonType *containingType);

  string getName() const override;
  llvm::Type *getType(codegen::CGContext &ctx) const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
  DonType getContainingType() const override;
  bool getPointerTo() const override;
};

} // namespace parser::ast
