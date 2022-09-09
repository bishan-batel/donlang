#include "lexer.h"
#include "token.h"
#include <iostream>
#include <string>
#include <utils/stringutils.h>

using namespace std;
namespace lexer {
Lexer::Lexer(string input) : src(input), idx(0) {}

Lexer::~Lexer() {
  for (auto tok : tokens) {
    delete tok;
  }
  tokens.clear();
}

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
    if (utils::is_whitespace(currentChar())) {
      nextChar();
      continue;
    }

    if (comment())
      continue;

    if (numberliteral())
      continue;

    if (stringliteral())
      continue;

    if (identifier())
      continue;

    if (op())
      continue;

    // cerr << "Invalid Character: '" << currentChar() << "'" << endl;
    throw string("Unexpected Character '") + currentChar() + '\'';
    // throw -1;
  }
  tokens.push_back(new EOFToken());
}

bool Lexer::op() {
  auto op = OPERATOR_MAP.find(currentChar());

  if (op != OPERATOR_MAP.end()) {
    tokens.push_back(new OperatorToken(op->second));
    idx++;
    return true;
  }

  return false;
}

bool Lexer::identifier() {
  if (!utils::is_alpha(currentChar()))
    return false;

  int start = idx;
  int length = 1;
  while (nextChar() != EOF && utils::is_alphanumeric(currentChar())) {
    length++;
  }

  string ident = src.substr(start, length);

  auto keyword = KEYWORD_MAP.find(ident);
  if (keyword != KEYWORD_MAP.end()) {
    tokens.push_back(new KeywordToken(keyword->second));
  } else {
    tokens.push_back(new IdentifierToken(ident));
  }

  return true;
}

bool Lexer::comment() {
  if (currentChar() != '#')
    return false;

  while (nextChar() != EOF && currentChar() != '\n') {
  }
  idx++;
  return true;
}

bool Lexer::numberliteral() {
  if (!utils::is_numeric(currentChar())) {

    return false;
  };

  int length = 1;
  int start = idx - 1;

  bool decimal;

  char c;
  while (utils::is_numeric(c = nextChar()) || c == '.') {
    if (c == '.') {
      if (decimal)
        break;
      else
        decimal = true;
    }
    length++;
  }
  double val = stod(src.substr(start, start + length));

  tokens.push_back(new F64Literal(val));
  return true;
}

bool Lexer::stringliteral() {
  if (currentChar() != '\"') {
    return false;
  }

  int start = idx + 1;
  int length = 0;

  while (nextChar() != EOF && currentChar() != '\"') {
    length++;
  }
  idx++;

  tokens.push_back(new StringLiteralToken(src.substr(start, length)));
  return true;
}

int Lexer::getTokenCount() { return tokenCount; }
vector<Token *> Lexer::getTokens() { return tokens; }
}; // namespace lexer
   //
