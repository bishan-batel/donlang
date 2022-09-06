#include "lexer.h"
#include "token.h"
#include <iostream>
#include <string>
#include <utils/stringutils.h>

using namespace std;
namespace lexer {
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

    if (op())
      continue;

    nextChar();
  }
  printf("EOF\n");
  tokens.push_back(Token(tok_eof));
  printf("EOF2\n");
}

bool Lexer::op() {
  Operator op = char_to_op(currentChar());
  if (op != op_invalid) {
    nextChar();
    Token tok(tok_op);
    tok.u8 = op;
    tokens.push_back(tok);
    return true;
  }
  return false;
}

bool Lexer::identifier() {
  if (!utils::is_alpha(currentChar()))
    return false;
  prevChar();

  string ident;

  while (nextChar() != EOF) {
    if (!utils::is_alphanumeric(currentChar()))
      break;
    ident.push_back(currentChar());
  }

  Keyword keyword = string_to_keyword(ident);
  if (keyword == keyword_invalid) {
    Token tok(tok_keyword);
    tok.u8 = keyword;
    tokens.push_back(tok);
  } else {
    cout << "Identifier: \"" << ident << '\"' << endl;
    Token tok(tok_identifier);
    tok.str = ident;
    tokens.push_back(tok);
  }

  return true;
}

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
  int length = 0;

  while (nextChar() != EOF && currentChar() != '\"') {
    length++;
  }
  nextChar();

  // printf("B3\n");
  Token tok(tok_strliteral);
  tok.str = src.substr(start, length);

  tokens.push_back(tok);
  return true;
}

int Lexer::getTokenCount() { return tokenCount; }
vector<Token> Lexer::getTokens() { return tokens; }
}; // namespace lexer
