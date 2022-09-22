#include "lexer.h"
#include "token.h"
#include <iostream>
#include <stdexcept>
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

    if (charliteral())
      continue;

    if (identifier())
      continue;

    if (op())
      continue;

    // cerr << "Invalid Character: '" << currentChar() << "'" << endl;
    throw runtime_error(string("Unexpected Character '") + currentChar() +
                        '\'');
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

  if (nextChar() == ':') {
    while (nextChar() != EOF) {
      if (currentChar() == ':' && nextChar() == '#') {
        nextChar();
        return true;
      }
    }
    return true;
  }

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

  auto substr = src.substr(start, start + length);

  switch (currentChar()) {
  case 'd':
    tokens.push_back(new F64Literal(stod(substr)));
    nextChar();
    break;

  case 'f':
    tokens.push_back(new F32Literal(stof(substr)));
    nextChar();
    break;

  case 'c':
    tokens.push_back(new CharLiteral(stoi(substr)));
    nextChar();
    break;

  case 'i':
    nextChar();
  default:
    tokens.push_back(new I32Literal(stoi(substr)));
    break;
  }

  return true;
}

bool Lexer::stringliteral() {
  if (currentChar() != '\"') {
    return false;
  }

  string str;
  while (nextChar() != EOF && currentChar() != '\"') {
    if (currentChar() == '\\') {
      str += escape_char();
      idx--;
    } else {
      str += currentChar();
    }
  }
  nextChar();

  tokens.push_back(new StringLiteralToken(str));
  return true;
}

char Lexer::escape_char() {
  char c = currentChar();

  if (c == '\\') {
    nextChar();

    c = currentChar();
    nextChar();
    switch (c) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case 'r':
      return '\r';
    case '0':
      return '\0';
    case '\'':
      return '\'';
    case '\\':
      return '\\';
    default:
      throw runtime_error("Invalid escape sequence");
    }
  }
  return c;
}

bool Lexer::charliteral() {
  if (currentChar() != '\'') {
    return false;
  }

  // read character literal until ' or EOF, parsing escape sequences
  if (nextChar() == EOF) {
    throw runtime_error("Invalid character literal");
  }

  char c = escape_char();

  if (currentChar() != '\'') {
    throw runtime_error("Expected ', found " + string(1, currentChar()));
  }
  nextChar();

  tokens.push_back(new CharLiteral(c));

  return true;
}

int Lexer::getTokenCount() { return tokenCount; }
vector<Token *> Lexer::getTokens() { return tokens; }
}; // namespace lexer
   //
