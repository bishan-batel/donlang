#pragma once

#include <fstream>
#include <iostream>
#include <llvm-c/Target.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <parser/ast.h>

using namespace std;

namespace jit {
void run_module(unique_ptr<llvm::Module> module);
}
