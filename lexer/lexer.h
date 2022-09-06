#pragma once

#include "token.h"
#include <string>
#include <vector>

namespace lexer {
class Lexer {
private:
  std::string src;
  int tokenCount;
  std::vector<Token> tokens;
  int idx;

  char currentChar();
  char prevChar();
  char nextChar();

  bool identifier();
  bool comment();
  bool stringliteral();
  bool operator_();

public:
  Lexer(std::string input);

  void tokenize();

  std::vector<Token> getTokens();
  int getTokenCount();
};
}; // namespace lexer
