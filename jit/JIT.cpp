#include "JIT.h"

#include <fstream>
#include <iostream>
#include <llvm-c/Target.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <parser/ast.h>

using namespace llvm;
namespace jit {

void run_module(unique_ptr<llvm::Module> module) {
  // InitLLVM __X(argc, argv);

  // InitializeNativeTarget();
  // InitializeNativeTargetAsmPrinter();
  //  InitializeNativeTargetAsmParser();

  // writes module bitcode to file

  std::error_code ec;
  llvm::raw_fd_stream file("main.ll", ec);

  llvm::WriteBitcodeToFile(*module, file);

  return;
}
} // namespace jit
