#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <fstream>
#include <iostream>
#include <jit/JIT.h>
#include <lexer/lexer.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Analysis/LoopInfo.h>
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
#include <llvm/Bitcode/BitcodeWriter.h>
#include <parser/parser.h>
#include <sstream>
#include <string>
#include <utils/stringutils.h>

using namespace std;

static cl::opt<bool> PrintIR("p", cl::desc("Print the IR"), cl::init(false));

static cl::opt<std::string> InputFileName(cl::Positional,
  cl::desc("<input file>"));

static cl::opt<std::string> OutputFileName("o", cl::desc("<output ll file>"),
  cl::init("a.ll"));

int main(int nargs, const char* argv[]) {
  // init CL

  llvm::cl::ParseCommandLineOptions(nargs, argv, "Donlang Compiler");

  if (InputFileName == "") {
    errs() << "No input file specified, use --help see options\n";
    abort();
  }

  // check if exists
  ifstream file(InputFileName);

  stringstream stream;
  file >> stream.rdbuf();
  file.close();

  string contents = stream.str();

  lexer::Lexer lexer(contents);
  lexer.tokenize();

  auto ast = parser::Parser(lexer.getTokens()).parse();

  // print out the first element (a function)'s prototype
  auto llvm_ctx = make_unique<llvm::LLVMContext>();
  auto irbuilder = make_unique<llvm::IRBuilder<>>(*llvm_ctx);
  auto module = make_unique<llvm::Module>("JIT", *llvm_ctx);

  auto ctx = codegen::CGContext(llvm_ctx, irbuilder, module);

  parser::ast::add_default_functions(ctx);
  try {
    for (auto& expr : ast) {
      expr->codegen(ctx);
    }
  }
  catch (string ex) {
    cout << "[ERROR CODEGEN]: " << ex << endl;
    return 1;
  }

  if (PrintIR) {
    module->print(llvm::outs(), nullptr);
  }

  // TODO remove
  module->setTargetTriple("x86_64-pc-linux-gnu");

  std::error_code ec;
  llvm::raw_fd_stream outfile(OutputFileName, ec);
  llvm::WriteBitcodeToFile(*module, outfile);

  return 0;
}
