#pragma once
#include "parser/ast.h"
#include <llvm/IR/LLVMContext.h>
#include <map>
#include <string.h>

namespace parser {

enum SymbolType {
  symbol_variable,
  symbol_function,
};

class SymbolData {
public:
  SymbolType type;
  SymbolData(SymbolType type);
};

class VariableSymbolData : public SymbolData {
public:
  ast::Primitive type;

  VariableSymbolData(ast::Primitive type);
};

class FunctionSymbolData : public SymbolData {
public:
  ast::Primitive type;
  vector<ast::Primitive> args;

  FunctionSymbolData(ast::Primitive type, vector<ast::Primitive> args);
};

class ScopedSymbolTable {
  map<string, unique_ptr<SymbolData>> table;
  ScopedSymbolTable *parent;

public:
  ScopedSymbolTable(ScopedSymbolTable *parent = nullptr);
  SymbolData *operator[](string name);
  void add(string name, unique_ptr<SymbolData> data);

  void remove(string name);
};
}; // namespace parser
