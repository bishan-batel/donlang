cmake_minimum_required(VERSION 3.22.1)
project(donlang VERSION 1.0.1)

set(CMAKE_CXX_STANDARD_20)
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(
        llvm_libs
        support
        core
        irreader
        object
        native
        MCJIT
)

# add multiple subdirectories
add_subdirectory(utils)
add_subdirectory(lexer)
add_subdirectory(jit)
add_subdirectory(parser)
add_subdirectory(ast)
add_subdirectory(core)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # does not produce the json file
set(CMAKE_EXPORT_COMPILE_COMMANDS
        ON
        CACHE INTERNAL "") # works
