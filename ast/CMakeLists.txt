add_library(
  ast
  literals.cpp
  classes.cpp
  casting.cpp
  types.h
  types.cpp
  ast.cpp
  ast.h
)

target_include_directories(ast PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(ast PUBLIC ${llvm_libs} utils lexer )
