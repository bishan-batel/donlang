add_library(
  ast
  ast.h
  ast.cpp
  literals.cpp
  casting.cpp
)

target_include_directories(ast PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(ast PUBLIC ${llvm_libs} utils lexer )
