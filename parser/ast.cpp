#include "ast.h"
#include "lexer/token.h"
#include <iostream>
#include <llvm/IR/Argument.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include <tuple>
#include <utility>

using namespace std;
using namespace llvm;

namespace codegen {

// constructo that takes context and creates IR builder
    CGContext::CGContext(unique_ptr<LLVMContext> &ctx,
                         unique_ptr<IRBuilder<>> &builder,
                         unique_ptr<Module> &module) {
        this->llvm = &ctx;
        this->builder = &builder;
        this->module = &module;
        this->namedValues = map<string, AllocaInst *>();
    }

} // namespace codegen

namespace parser::ast {
    Expression::operator string() const { return "NULL Expression"; }

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

/**
 * Variable Expression
 */
    VariableExpression::VariableExpression(string name) : name(std::move(name)) {}

    VariableExpression::operator string() const { return "variable[" + name + "]"; }

    Value *VariableExpression::codegen(codegen::CGContext &ctx) {
        AllocaInst *v = ctx.namedValues[name];
        if (v == nullptr) {
            throw runtime_error("Unknown variable used");
        }

        LoadInst *a = ctx.builder->get()->CreateLoad(v->getType(), v, name);

        return a;
    }

/**
 * VariableDefinition
 */
    VariableDefinition::VariableDefinition(string name, unique_ptr<Expression> expr)
            : name(std::move(name)), expression(std::move(expr)) {}

    VariableDefinition::operator string() const {
        auto expr_str = expression == nullptr ? "uninitialized" : string(*expression);
        return "variable_init[" + name + " = " + expr_str + "]";
    }

    Value *VariableDefinition::codegen(codegen::CGContext &ctx) {
        if (expression == nullptr) {
            throw runtime_error("Variable '" + name + "' is uninitialized");
        }

        auto expr_val = expression->codegen(ctx);

        if (expr_val == nullptr) {
            throw runtime_error("Variable '" + name + "' is uninitialized");
        }

        auto ty = expr_val->getType();
        AllocaInst *alloca = ctx.builder->get()->CreateAlloca(ty, nullptr, name);
        ctx.namedValues[name] = alloca;


        ctx.builder->get()->CreateStore(expr_val, alloca);
        return alloca;
    }

/**
 * Return Expression
 */
    ReturnExpression::ReturnExpression(unique_ptr<Expression> expr)
            : expr(std::move(expr)) {}

    ReturnExpression::operator string() const {
        return "return[" + string(*expr) + "]";
    }

    Value *ReturnExpression::codegen(codegen::CGContext &ctx) {
        auto expr = this->expr->codegen(ctx);
        return ctx.builder->get()->CreateRet(expr);
    }

/**
 * Binary Expression
 */
    BinaryExpresion::BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                                     unique_ptr<Expression> rhs)
            : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    BinaryExpresion::operator string() const {
        return "(" + string(*lhs) + string(lexer::OperatorToken(op)) + string(*rhs) +
               ")";
    }

    Value *BinaryExpresion::codegen(codegen::CGContext &ctx) {
        if (lhs == nullptr) {
            throw runtime_error("LHS is null");
        }
        Value *lhs_val = lhs->codegen(ctx);

        if (rhs == nullptr) {
            return lhs_val;
        }

        Value *rhs_val = rhs->codegen(ctx);

        if (lhs_val == nullptr) {
            throw runtime_error("Invalid binary expression: lhs is null");
        }

        if (rhs_val == nullptr) {
            throw runtime_error("Invalid binary expression: rhs is null");
        }

        switch (op) {
            case lexer::op_plus:
                return ctx.builder->get()->CreateAdd(lhs_val, rhs_val, "addtmp");
            case lexer::op_minus:
                return ctx.builder->get()->CreateSub(lhs_val, rhs_val, "subtmp");
            case lexer::op_mul:
                return ctx.builder->get()->CreateMul(lhs_val, rhs_val, "multmp");
            case lexer::op_div:
                return ctx.builder->get()->CreateFDiv(lhs_val, rhs_val, "divtmp");
            case lexer::op_mod:
                return ctx.builder->get()->CreateFRem(lhs_val, rhs_val, "modtmp");
            case lexer::op_greater_than:
                return ctx.builder->get()->CreateFCmpUGT(lhs_val, rhs_val, "gttmp");
            case lexer::op_less_than:
                return ctx.builder->get()->CreateFCmpULT(lhs_val, rhs_val, "lttmp");
            case lexer::op_eq:
                // check of lhs is a variable expression
                if (auto var = dynamic_cast<VariableExpression *>(lhs.get())) {
                    ctx.builder->get()->CreateStore(rhs_val, var->codegen(ctx));
                    break;
                } else {
                    throw string("Invalid binary expression, lhs is not a variable");
                }
            default:
                throw string("Invalid binary expression, operator not supported (" +
                             string(lexer::OperatorToken(op)) + ")");
        }
        return nullptr;
    }

/**
 * Call Expression
 */
    CallExpression::CallExpression(string name, vector<unique_ptr<Expression>> args)
            : name(std::move(name)), args(std::move(args)) {}

    CallExpression::operator string() const {
        auto str = "call " + name + " [args: (";
        for (auto &arg: args) {
            str += string(*arg);
        }
        return str + ")]";
    }

    Value *CallExpression::codegen(codegen::CGContext &ctx) {

        // Look up the name in the global module table.
        auto func = ctx.module->get()->getFunction(name);
        if (!func) {
            throw string("Undefined function: " + name);
        }

        // If argument mismatch error.
        if (func->arg_size() != args.size()) {
            throw runtime_error("Incorrect # arguments passed for function " + name);
        }

        vector<Value *> arg_vals;
        for (unsigned i = 0, e = args.size(); i != e; ++i) {
            arg_vals.push_back(args[i]->codegen(ctx));
            if (!arg_vals.back()) {
                throw runtime_error("Invalid argument passed to function");
            }
        }

        if (func->getReturnType()->isVoidTy()) {
            return ctx.builder->get()->CreateCall(func, arg_vals);
        } else {
            return ctx.builder->get()->CreateCall(func, arg_vals, "calltmp");
        }
    }

/**
 * If Expression
 */

    IfExpression::IfExpression(unique_ptr<Expression> cond,
                               vector<unique_ptr<Expression>> then_expr,
                               vector<unique_ptr<Expression>> else_expr)
            : condition(std::move(cond)), then(std::move(then_expr)),
              otherwise(std::move(else_expr)) {}

    IfExpression::operator string() const {
        auto str = "if (" + string(*condition) + ") then (";

        for (auto &expr: then) {
            str += string(*expr);
        }

        str += ") else (";

        for (auto &expr: otherwise) {
            str += string(*expr);
        }

        return str + ")";
    }

    Value *IfExpression::codegen(codegen::CGContext &ctx) {
        Value *cond = condition->codegen(ctx);
        if (cond == nullptr) {
            throw runtime_error("Invalid if expression: condition is null");
        }

        // convert condition to a bool by comparing equal to 0.0
        cond = ctx.builder->get()->CreateFCmpONE(
                cond, ConstantFP::get(**ctx.llvm, APFloat(0.0)), "ifcond");

        llvm::Function *func = ctx.builder->get()->GetInsertBlock()->getParent();

        // create blocks for then and else
        BasicBlock *then_block = BasicBlock::Create(**ctx.llvm, "then", func);

        BasicBlock *else_block = BasicBlock::Create(**ctx.llvm, "else");
        BasicBlock *merge_block = BasicBlock::Create(**ctx.llvm, "ifcont");

        ctx.builder->get()->CreateCondBr(cond, then_block, else_block);

        ctx.builder->get()->SetInsertPoint(then_block);

        Value *then_val = nullptr;
        for (auto &expr: then) {
            then_val = expr->codegen(ctx);
        }

        if (then_val == nullptr) {
            throw runtime_error("Invalid if expression: then expression is null");
        }

        // create an unconditional branch to the merge block and insert it
        ctx.builder->get()->CreateBr(merge_block);

        then_block = ctx.builder->get()->GetInsertBlock();
        func->getBasicBlockList().push_back(else_block);
        ctx.builder->get()->SetInsertPoint(else_block);

        Value *else_val = nullptr;
        for (auto &expr: otherwise) {
            else_val = expr->codegen(ctx);
        }

        ctx.builder->get()->CreateBr(merge_block);
        else_block = ctx.builder->get()->GetInsertBlock();

        func->getBasicBlockList().push_back(merge_block);
        ctx.builder->get()->SetInsertPoint(merge_block);

        PHINode *phi =
                ctx.builder->get()->CreatePHI(Type::getDoubleTy(**ctx.llvm), 2, "iftmp");

        return phi;
        phi->addIncoming(then_val, then_block);
        phi->addIncoming(else_val, else_block);
        return phi;
    }

/**
 * Function Prototype
 */
    Prototype::Prototype(string name, Primitive returntype,
                         vector<tuple<string, Primitive>> args)
            : name(std::move(name)), args(std::move(args)), returntype(returntype) {}

    string Prototype::getName() const { return name; }

    Prototype::operator string() const {
        string str = "Name: \"" + name + "\" Args: (";
        for (auto arg: args) {
            str += string(get<0>(arg)) + ":" + to_string((int) get<1>(arg)) + ",";
        }

        str += ")";
        return str;
    }

    llvm::Type *primitive_to_type(codegen::CGContext &ctx, Primitive prim) {
        switch (prim) {
            case Primitive::primitive_f64:
                return Type::getDoubleTy(**ctx.llvm);
            case Primitive::primitive_f32:
                return Type::getFloatTy(**ctx.llvm);
            case Primitive::primitive_char:
                return Type::getInt8Ty(**ctx.llvm);
            case Primitive::primitive_string:
                return Type::getInt8PtrTy(**ctx.llvm);
            case Primitive::primitive_void:
                return Type::getVoidTy(**ctx.llvm);
            case Primitive::primitive_i64:
                return Type::getInt64Ty(**ctx.llvm);
            case Primitive::primitive_i32:
                return Type::getInt32Ty(**ctx.llvm);
            case Primitive::primitive_bool:
                return Type::getInt1Ty(**ctx.llvm);
            default:
                throw string("Invalid primitive type");
        }
    }

    llvm::Function *Prototype::codegen(codegen::CGContext &ctx) {
        vector<Type *> arg_types;

        for (auto arg: args) {
            arg_types.push_back(primitive_to_type(ctx, get<1>(arg)));
        }

        auto func_type =
                FunctionType::get(primitive_to_type(ctx, returntype), arg_types, false);
        auto func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                           name, *ctx.module->get());

        int idx = 0;
        for (auto &arg: func->args()) {
            arg.setName(get<0>(args[idx++]));
        }

        return func;
    }

    Prototype::~Prototype() {}

/**
 * Function
 */
    Function::Function(unique_ptr<Prototype> proto,
                       vector<unique_ptr<Expression>> body)
            : proto(std::move(proto)), body(std::move(body)) {}

    Function::operator string() const {
        string str = "Function:\n\t" + string(*proto);
        str += "\nBody:\n";
        for (auto &expr: body) {
            if (expr != nullptr) {
                str += "\t" + string(*expr) + "\n";
            }
        }
        return str;
    }

    Value *Function::codegen(codegen::CGContext &ctx) {
        auto func = proto->codegen(ctx);
        if (func == nullptr) {
            return nullptr;
        }

        auto bb = BasicBlock::Create(**ctx.llvm, "entry", func);
        ctx.builder->get()->SetInsertPoint(bb);

        // add arguments to namedValues
        for (auto &arg: func->args()) {
            // ctx.namedValues[arg.getName().str()] = &arg;

            // create an alloca for this variable
            AllocaInst *alloca = ctx.builder->get()->CreateAlloca(
                    arg.getType(), 0, arg.getName().str().c_str());

            // store the initial value into the alloca
            ctx.builder->get()->CreateStore(&arg, alloca);

            // add the alloca to the symbol table
            ctx.namedValues[arg.getName().str()] = alloca;
        }

        for (auto &expr: body) {
            if (expr != nullptr) {
                expr->codegen(ctx);
            }
        }

        // remove arguments from named values
        for (auto &arg: func->args()) {
            ctx.namedValues[arg.getName().str()] = nullptr;
        }

        if (ctx.builder->get()->GetInsertBlock()->getTerminator() == nullptr) {
            ctx.builder->get()->CreateRetVoid();
        }
        return func;
    }

/**
 * ExternFunction
 */

    ExternFunction::ExternFunction(unique_ptr<Prototype> proto)
            : proto(std::move(proto)) {}

    ExternFunction::operator string() const {
        return "Extern Function:\n\t" + string(*proto);
    }

// TODO
    Value *ExternFunction::codegen(codegen::CGContext &ctx) {
        auto func = proto->codegen(ctx);

        if (func == nullptr) {
            return nullptr;
        }

        return func;
    }

    void add_default_functions(codegen::CGContext &ctx) {}

} // namespace parser::ast
