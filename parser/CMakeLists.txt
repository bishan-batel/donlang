add_library(
  parser
  parser.h
  parser.cpp
)

target_include_directories(parser PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(parser PUBLIC ${llvm_libs} ast utils lexer )
