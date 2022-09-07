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

    if (keyword())
      continue;

    if (numberliteral())
      continue;

    if (stringliteral())
      continue;

    if (identifier())
      continue;

    if (op())
      continue;

    cerr << "Invalid Character: '" << currentChar() << "'" << endl;
    throw -1;
  }
  tokens.push_back(new EOFToken());
}

bool Lexer::op() {
  int src_len = src.size();
  for (auto operator_t : OPERATOR_MAP) {
    auto op_str = get<0>(operator_t);
    auto len = op_str.size();

    if (idx + len >= src_len)
      continue;

    auto substr = src.substr(idx, len);
    if (substr == op_str) {
      tokens.push_back(new OperatorToken(get<1>(operator_t)));
      idx += len;
      return true;
    }
  }
  return false;
}

bool Lexer::keyword() {
  int src_len = src.size();
  for (auto keyword : KEYWORD_MAP) {
    auto word = get<0>(keyword);
    auto len = word.size();

    if (idx + len >= src_len)
      continue;

    auto substr = src.substr(idx, len);
    if (substr == word && !utils::is_whitespace(src.at(idx + len))) {
      tokens.push_back(new KeywordToken(get<1>(keyword)));
      idx += len;
      return true;
    }
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
  tokens.push_back(new IdentifierToken(ident));

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
  idx--;

  int length = 1;
  int start = idx;

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
vector<const Token *> Lexer::getTokens() { return tokens; }
}; // namespace lexer
