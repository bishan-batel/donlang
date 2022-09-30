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

  // get variabels
  for (auto &expr : body) {
    if (auto attr = dynamic_cast<ClassAttribute *>(expr.get())) {
      if (attr->flags & CF_STATIC) {
        // TODO
        throw string("static variables not supported yet");
        continue;
      } else {
        members.push_back(primitive_to_type(ctx, attr->type));
      }
      continue;
    }
  }
  cout << "members: " << members.size() << endl;
  struct_type->setBody(members);

  for (auto &expr : body) {
    if (auto method = dynamic_cast<ClassMethod *>(expr.get())) {
      method->proto->name = name + "." + method->proto->name;

      // append pointer to self to the beginning of the argument list if not
      // static
      if (!(method->flags & CF_STATIC)) {
        auto ty = ast::primitive_to_ptr(Primitive::primitive_i32);
        method->proto->args.insert(method->proto->args.begin(),
                                   make_tuple("self", ty));
      }
      method->codegen(ctx);
      continue;
    }

    if (auto constructor = dynamic_cast<ClassConstructor *>(expr.get())) {
      // append pointer to self to the beginning of the argument list
      constructor->struct_type = struct_type;
      constructor->codegen(ctx);
      continue;
    }
  }
  ctx.classes[name] = struct_type;
  return nullptr;
}

parser::ast::ClassConstructor::ClassConstructor(
    int flags, vector<tuple<string, Primitive>> args,
    vector<unique_ptr<Expression>> body, bool varargs)
    : flags(flags), args(args), body(move(body)), varargs(varargs),
      struct_type(nullptr) {}

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
  vector<Type *> args;
  for (auto &arg : this->args) {
    args.push_back(primitive_to_type(ctx, get<1>(arg)));
  }

  auto functype = FunctionType::get(struct_type, args, varargs);

  auto func = llvm::Function::Create(functype, llvm::Function::ExternalLinkage,
                                     struct_type->getName(), ctx.module->get());

  int idx = 0;
  for (auto &arg : func->args()) {
    arg.setName(get<0>(this->args[idx++]));
  }

  auto bb = BasicBlock::Create(**ctx.llvm, "entry", func);
  ctx.builder->get()->SetInsertPoint(bb);

  // create alloca for each argument
  for (auto &arg : func->args()) {
    // set name of argument
    auto alloca =
        ctx.builder->get()->CreateAlloca(arg.getType(), nullptr, arg.getName());

    ctx.builder->get()->CreateStore(&arg, alloca);
    ctx.namedValues[arg.getName().str()] = alloca;
  }

  // make constructor return new instance of the class
  auto self = ctx.builder->get()->CreateAlloca(struct_type);
  ctx.namedValues["self"] = self;

  for (auto &expr : body) {
    expr->codegen(ctx);
  }

  ctx.builder->get()->CreateRet(self);
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
