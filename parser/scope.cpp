#include "scope.h"

namespace parser {
SymbolData::SymbolData(SymbolType type) : type(type) {}

VariableSymbolData::VariableSymbolData(ast::Primitive type)
    : SymbolData(symbol_variable), type(type) {}

FunctionSymbolData::FunctionSymbolData(ast::Primitive type,
                                       vector<ast::Primitive> args)
    : SymbolData(symbol_function), type(type), args(args) {}

ScopedSymbolTable::ScopedSymbolTable(ScopedSymbolTable *parent)
    : parent(parent) {}

SymbolData *ScopedSymbolTable::operator[](string name) {
  auto it = table.find(name);
  if (it != table.end()) {
    return it->second.get();
  }
  if (parent != nullptr) {
    return (*parent)[name];
  }
  return nullptr;
}

void ScopedSymbolTable::add(string name, unique_ptr<SymbolData> data) {
  table[name] = move(data);
}

void ScopedSymbolTable::remove(string name) { table.erase(name); }


}; // namespace parser
