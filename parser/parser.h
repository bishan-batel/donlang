#pragma once
#include <ast/ast.h>
#include <iostream>
#include <lexer/token.h>

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
  bool is_identifier();
  bool is_op(lexer::Operator);
  string require_identifier();
  void require_keyword(Keyword);
  void require_op(lexer::Operator);

  Token *advance();
  unique_ptr<ast::Prototype> parse_prototype();
  unique_ptr<ast::Expression> parse_function();
  unique_ptr<ast::Expression> parse_extern();

  // class parsing
  unique_ptr<ast::Expression> parse_class_constructor(int flags = 0);
  unique_ptr<ast::Expression> parse_class_method(int flags = 0);
  unique_ptr<ast::Expression> parse_class_attr(int flags = 0);
  unique_ptr<ast::Expression> parse_class();

  unique_ptr<ast::Expression> parse_inner_function();
  unique_ptr<ast::Expression> parse_control_if();
  vector<unique_ptr<ast::Expression>> parse_control_else();
  unique_ptr<ast::Expression> parse_control_while();

  unique_ptr<ast::Expression> parse_var_def();
  unique_ptr<ast::Expression> parse_return();

  unique_ptr<ast::Expression> parse_expression();
  unique_ptr<ast::Expression> parse_expression_and_or();
  unique_ptr<ast::Expression> parse_expression_is();
  unique_ptr<ast::Expression> parse_expression_compare();
  unique_ptr<ast::Expression> parse_expression_eq();
  unique_ptr<ast::Expression> parse_expression_add();
  unique_ptr<ast::Expression> parse_expression_mul();
  unique_ptr<ast::Expression> parse_expression_cast();
  unique_ptr<ast::Expression> parse_exppression_factor();
  ast::Primitive parse_type();

  inline void err_unexpected_tok(const string &expected);

public:
  Parser(vector<Token *> tokens);

  vector<unique_ptr<ast::Expression>> parse();
};

ast::Primitive keyword_to_primitive(Keyword word, bool isptr);

}; // namespace parser
