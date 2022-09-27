#pragma once
#include "ast.h"
#include <cstddef>

namespace parser::ast {

class DonType {
  virtual string getName() const;
  virtual llvm::Type getType() const;
  virtual size_t getSize() const;
  virtual bool isIntegerTy() const;
  virtual bool isFloating() const;
  virtual bool isObject() const;
  virtual bool isPointer() const;
  virtual DonType getContainingType() const;
};

class I32Type : DonType {
  string getName() const override;
  llvm::Type getType() const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
};

class I64Type : DonType {
  string getName() const override;
  llvm::Type getType() const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
};

class F32Type : DonType {
  string getName() const override;
  llvm::Type getType() const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
};

class F64Type : DonType {
  string getName() const override;
  llvm::Type getType() const override;
  size_t getSize() const override;
  bool isIntegerTy() const override;
  bool isFloating() const override;
  bool isObject() const override;
  bool isPointer() const override;
};

} // namespace parser::ast
