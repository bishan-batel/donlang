#pragma once
#include "ast.h"
#include "lexer/token.h"
#include <iostream>

using namespace std;
using namespace lexer;
using namespace std;

namespace parser {

class Parser {
  vector<Token *> tokens;

  Token *currTok;
  int idx;

  bool is_curr(TokenType);
  bool is_keyword(Keyword);
  bool is_op(Operator);

  Token *advance();
  unique_ptr<ast::Prototype> parse_prototype();
  unique_ptr<ast::Function> parse_function();
  unique_ptr<ast::Expression> parse_var_def();

  unique_ptr<ast::Expression> parse_expression();
  unique_ptr<ast::Expression> parse_expression_add();
  unique_ptr<ast::Expression> parse_expression_mul();
  unique_ptr<ast::Expression> parse_exppression_factor();
  unique_ptr<ast::Expression> parse_expression_paren();

  void err_unexpected_tok(string expected);

public:
  Parser(vector<Token *> tokens);

  void parse();
};
}; // namespace parser
