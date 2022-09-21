#include "ast.h"
#include <llvm/IR/DerivedTypes.h>

namespace parser::ast {

ClassDefinition::ClassDefinition(string name,
                                 vector<unique_ptr<Expression>> body)
    : name(name), body(move(body)) {}

ClassDefinition::operator string() const {
  string str = "class " + name + " {\n";
  for (auto &expr : body) {
    str += "  " + (string)*expr + "\n";
  }
  str += "}";
  return str;
}

Value *ClassDefinition::codegen(codegen::CGContext &ctx) {
  StructType *struct_type = StructType::create(**ctx.llvm, name);

  struct_type->setBody(primitive_to_type(ctx, Primitive::primitive_void));
  struct_type->setName(name);
}

  //return struct_type;
}
