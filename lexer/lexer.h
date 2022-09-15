#pragma once

#include "token.h"
#include <string>
#include <vector>

namespace lexer {
class Lexer {
private:
  std::string src;
  int tokenCount;
  std::vector<Token *> tokens;
  int idx;

  char currentChar();
  char prevChar();
  char nextChar();

  bool identifier();
  bool comment();
  bool stringliteral();
  char escape_char();
  bool charliteral();
  bool numberliteral();
  bool op();

public:
  Lexer(std::string input);
  ~Lexer();

  void tokenize();

  std::vector<Token *> getTokens();
  int getTokenCount();
};

// ERROR
}; // namespace lexer
   //
