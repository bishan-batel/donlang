#pragma once

#include <string>
#include <vector>
#include "token.h"

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

public:
  Lexer(std::string input);

  void tokenize();

  std::vector<Token> getTokens();
  int getTokenCount();
};
