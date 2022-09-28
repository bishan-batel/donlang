#include "ast.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>

using namespace llvm;

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
  struct_type->setName(name);

  vector<Type *> members;
  for (auto &expr : body) {
    if (auto attr = dynamic_cast<ClassAttribute *>(expr.get())) {
      members.push_back(primitive_to_type(ctx, attr->type));
      continue;
    }

    if (auto method = dynamic_cast<ClassMethod *>(expr.get())) {
      method->proto->name = name + "." + method->proto->name;

      // append pointer to self to the beginning of the argument list if not
      // static
      //
      if (!(method->flags & CF_STATIC)) {
        auto ty = ast::primitive_to_ptr(Primitive::primitive_i32);
        method->proto->args.insert(method->proto->args.begin(),
                                   make_tuple("self", ty));
      }
      continue;
    }

    if (auto constructor = dynamic_cast<ClassConstructor *>(expr.get())) {
      // append pointer to self to the beginning of the argument list
      constructor->args.insert(
          constructor->args.begin(),
          make_tuple("self", ast::primitive_to_ptr(Primitive::primitive_i32)));

      continue;
    }
  }
  struct_type->setBody(members);

  ctx.classes[name] = struct_type;
  return nullptr;
}

parser::ast::ClassConstructor::ClassConstructor(
    int flags, vector<tuple<string, Primitive>> args,
    vector<unique_ptr<Expression>> body, bool varargs)
    : flags(flags), args(args), body(move(body)), varargs(varargs) {}

parser::ast::ClassConstructor::operator string() const {
  string str = "constructor(";
  for (auto &arg : args) {
    str += get<0>(arg) + ": " + to_string(get<1>(arg)) + ", ";
  }
  str += ") {\n";
  for (auto &expr : body) {
    str += "  " + (string)*expr + "\n";
  }
  str += "}";
  return str;
}

Value *parser::ast::ClassConstructor::codegen(codegen::CGContext &ctx) {
  return nullptr;
}

ClassMethod::ClassMethod(int flags, unique_ptr<Prototype> proto,
                         vector<unique_ptr<Expression>> body)
    : flags(flags), proto(move(proto)), body(move(body)) {}

ClassMethod::operator string() const {
  string str =
      string(flags & CF_PUBLIC ? "pub " : "priv ") + (string)*proto + " {\n";
  for (auto &expr : body) {
    str += "  " + (string)*expr + "\n";
  }
  str += "}";
  return str;
}

Value *ClassMethod::codegen(codegen::CGContext &ctx) {
  auto func = proto->codegen(ctx);
  ctx.builder->get()->SetInsertPoint(
      BasicBlock::Create(**ctx.llvm, "entry", func));

  for (auto &arg : func->args()) {
    // create an alloca for this variable
    AllocaInst *alloca =
        ctx.builder->get()->CreateAlloca(arg.getType(), 0, arg.getName());

    // store the initial value into the alloca
    ctx.builder->get()->CreateStore(&arg, alloca);

    // add the alloca to the symbol table
    ctx.namedValues[arg.getName().str()] = alloca;
  }

  for (auto &expr : body) {
    expr->codegen(ctx);
  }

  for (auto &arg : func->args()) {
    ctx.namedValues[arg.getName().str()] = nullptr;
  }

  if (ctx.builder->get()->GetInsertBlock()->getTerminator() == nullptr) {
    ctx.builder->get()->CreateRetVoid();
  }

  return func;
}

ClassAttribute::ClassAttribute(int flags, string name, Primitive type,
                               unique_ptr<Expression> expression)
    : flags(flags), name(name), type(type), expression(move(expression)) {}

ClassAttribute::operator string() const {
  return name + " = " + (string)*expression;
}

Value *ClassAttribute::codegen(codegen::CGContext &ctx) { return nullptr; }

// return struct_type;
} // namespace parser::ast
