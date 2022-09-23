#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <fstream>
#include <iostream>
#include <jit/JIT.h>
#include <lexer/lexer.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <parser/parser.h>
#include <sstream>
#include <stdio.h>
#include <string>
#include <utils/stringutils.h>

using namespace std;

static cl::opt<bool> PrintIR("p", cl::desc("Print the IR"), cl::init(false));

static cl::opt<std::string> InputFileName(cl::Positional,
                                          cl::desc("<input file>"));

static cl::opt<std::string> OutputFileName("o", cl::desc("<output ll file>"),
                                           cl::init("a.ll"));

static cl::opt<bool> JIT("jit", cl::desc("Runs file with JIT"),
                         cl::init(false));

int main(int nargs, const char *argv[]) {
  errs().enable_colors(true);
  outs().enable_colors(true);

  // init CL
  cl::ParseCommandLineOptions(nargs, argv, "Donlang Compiler");

  // change LLVM errs

  if (InputFileName.empty()) {
    errs() << "No input file specified, use --help see options\n";
    return 1;
  }

  // check if exists
  ifstream file(InputFileName);

  stringstream stream;
  file >> stream.rdbuf();
  file.close();

  string contents = stream.str();

  lexer::Lexer lexer(contents);
  lexer.tokenize();

  vector<unique_ptr<parser::ast::Expression>> ast;
  try {
    ast = parser::Parser(lexer.getTokens()).parse();
  } catch (string ex) {
    errs().changeColor(errs().RED) << "[PARSER ERROR] " << ex << '\n';
    errs().resetColor();
    return 1;
  }

  // print out the first element (a function)'s prototype
  auto llvm_ctx = make_unique<llvm::LLVMContext>();
  auto irbuilder = make_unique<llvm::IRBuilder<>>(*llvm_ctx);
  auto module = make_unique<llvm::Module>("JIT", *llvm_ctx);
  module->setTargetTriple(LLVMGetDefaultTargetTriple());
  module->setSourceFileName(InputFileName);

  auto ctx = codegen::CGContext(llvm_ctx, irbuilder, module);

  parser::ast::add_default_functions(ctx);
  try {
    for (auto &expr : ast) {
      expr->codegen(ctx);
    }
  } catch (string ex) {
    errs().changeColor(errs().RED) << ex << "\n";
    errs().resetColor();
    return 1;
  }

  if (PrintIR) {
    module->print(llvm::outs(), nullptr);
  }

  // set target triple

  if (JIT) {
    // error: unsupported

    errs().changeColor(errs().RED) << "JIT not supported yet :(\n";
    errs().resetColor();
    return 1;
  }

  std::error_code ec;
  llvm::raw_fd_stream outfile(OutputFileName, ec);
  llvm::WriteBitcodeToFile(*module, outfile);
}
