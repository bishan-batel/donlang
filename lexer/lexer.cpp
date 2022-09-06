#include "lexer.h"
#include "token.h"
#include <cctype>
#include <cstdio>
#include <iostream>
#include <string>

using namespace std;
Lexer::Lexer(string input) : src(input) {}

// Index Control
char Lexer::currentChar() {
  return idx >= 0 && idx < src.size() ? src.at(idx) : EOF;
}

char Lexer::nextChar() {
  ++idx;
  return currentChar();
}

char Lexer::prevChar() {
  --idx;
  return currentChar();
}

// Tokenizer Functions
void Lexer::tokenize() {
  while (currentChar() != EOF) {

    if (comment())
      continue;

    if (stringliteral())
      continue;

    if (identifier())
      continue;

    nextChar();
  }
  printf("EOF\n");
  tokens.push_back(Token(tok_eof));
  printf("EOF2\n");
}

bool Lexer::identifier() { return false; }

bool Lexer::comment() {
  // comments
  if (currentChar() == '#') {
    while (nextChar() != '\n' && currentChar() != EOF) {
    }
  }
  return false;
}

bool Lexer::stringliteral() {
  if (currentChar() != '\"') {
    return false;
  }

  int start = idx + 1;

  while (nextChar() != EOF && currentChar() != '\"') {
  }
  nextChar();

  // printf("B3\n");
  Token tok(tok_strliteral);
  tok.str = src.substr(start, idx - 1);

  tokens.push_back(tok);
  return true;
}

int Lexer::getTokenCount() { return tokenCount; }
vector<Token> Lexer::getTokens() { return tokens; }
