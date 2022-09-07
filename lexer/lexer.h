#pragma once

#include "token.h"
#include <string>
#include <vector>

namespace lexer {
class Lexer {
private:
  std::string src;
  int tokenCount;
  std::vector<const Token*> tokens;
  int idx;

  char currentChar();
  char prevChar();
  char nextChar();

  bool keyword();
  bool identifier();
  bool comment();
  bool stringliteral();
  bool numberliteral();
  bool op();

public:
  Lexer(std::string input);
  ~Lexer();

  void tokenize();

  std::vector<const Token*> getTokens();
  int getTokenCount();
};
}; // namespace lexer
