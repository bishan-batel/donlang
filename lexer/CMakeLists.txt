add_library(
  lexer
  lexer.h
  lexer.cpp
  token.h
  token.cpp
)

target_include_directories(lexer PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(lexer PUBLIC utils)
