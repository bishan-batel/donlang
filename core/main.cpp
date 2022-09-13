#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <fstream>
#include <iostream>
#include <lexer/lexer.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetParser.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <parser/ast.h>
#include <parser/parser.h>
#include <sstream>
#include <string>
#include <utils/stringutils.h>

using namespace std;

int main(int nargs, const char *argv[]) {
  if (nargs == 1) {
    cout << "ERROR: Requires argument for filename" << endl;
    return 1;
  }
  ifstream file(argv[1]);

  if (file) {
    cout << "Compiling file " << argv[1] << endl;
    stringstream stream;
    file >> stream.rdbuf();
    file.close();

    string contents = stream.str();
    cout << "File byte length: " << contents.length() << endl;
    cout << endl << "======================================" << endl;

    lexer::Lexer lexer(contents);
    lexer.tokenize();
    printf("Finished Tokenization\n");

    auto ast = parser::Parser(lexer.getTokens()).parse();

    // print out AST
    cout << "AST: " << endl;
    for (auto &node : ast) {
      cout << string(*node) << endl;
    }
    // print out the first element (a function)'s prototype
    auto llvm_ctx = make_unique<llvm::LLVMContext>();
    auto irbuilder = make_unique<llvm::IRBuilder<>>(*llvm_ctx);
    auto module = make_unique<llvm::Module>("JIT", *llvm_ctx);

    auto ctx = codegen::CGContext(llvm_ctx, irbuilder, module);

    try {

      for (auto &expr : ast) {
        expr->codegen(ctx);
        cout << endl;
      }
    } catch (string ex) {
      cout << "[ERROR CODEGEN]: " << ex << endl;
      return 1;
    }

    cout << "\n\n\n";

    module->print(errs(), nullptr);

    cout << "\n\n\n";

    llvm::Function *mainFunc = module->getFunction("main");

    // run the main function
    // fpm.run(Function &IR, AnalysisManager<Function> &AM)

  } else {
    cout << "ERROR: File \"" << argv[0] << "\" does not exist" << endl;
    return 1;
  }
  return 0;
}
