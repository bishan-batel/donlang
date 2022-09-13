#include "parser.h"
#include "ast.h"
#include "lexer/token.h"
#include <memory>
#include <tuple>
#include <utility>
#include <vector>

namespace parser {
using namespace std;

Parser::Parser(vector<lexer::Token *> toks) : tokens(std::move(toks)) {
  currTok = nullptr;
  idx = -1;
  advance();
}

lexer::Token *Parser::advance() {
  currTok = tokens.at(++idx);
  return (idx >= tokens.size() || idx < 0) ? nullptr : currTok;
}

bool Parser::is_curr(lexer::TokenType toktype) {
  return currTok->type == toktype;
}

bool Parser::is_keyword(lexer::Keyword keyword) {
  return is_curr(lexer::tok_keyword) &&
         ((KeywordToken *)currTok)->word == keyword;
}

bool Parser::is_op(lexer::Operator op) {
  return is_curr(lexer::tok_op) && ((OperatorToken *)currTok)->op == op;
}

void Parser::err_unexpected_tok(const string &unexpected) {
  throw "Unexpected Token, expected '" + unexpected + "', found '" +
      string(*currTok) + '\'';
}

unique_ptr<ast::Expression> Parser::parse_extern() {
  if (!is_keyword(lexer::keyword_extern)) {
    return nullptr;
  }
  advance();

  auto proto = parse_prototype();
  return make_unique<ast::ExternFunction>(std::move(proto));
}

unique_ptr<ast::Expression> Parser::parse_return() {
  if (!is_keyword(lexer::keyword_return)) {
    return nullptr;
  }
  advance();

  auto expr = parse_expression();
  return make_unique<ast::ReturnExpression>(std::move(expr));
}

unique_ptr<ast::Expression> Parser::parse_function() {
  if (!is_keyword(lexer::keyword_def)) {
    return nullptr;
  }
  advance();

  auto proto = parse_prototype();
  if (proto == nullptr)
    throw "Unable to parse function signature";

  // multiline functions

  if (!is_op(lexer::op_opencurly)) {
    err_unexpected_tok("{");
  }
  advance();

  vector<unique_ptr<ast::Expression>> body;
  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = nullptr;

    if ((expr = parse_var_def()) != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }
    if ((expr = parse_return()) != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }
    if ((expr = parse_expression()) != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }

    // throw "Unexpected Token, '" + string(*currTok) + '\'';
  }
  advance();

  auto func = make_unique<ast::Function>(std::move(proto), std::move(body));
  cout << "Created prototype for function '" << func->proto->getName() << endl;
  return func;
}

unique_ptr<ast::Expression> Parser::parse_var_def() {
  if (!is_keyword(lexer::keyword_let)) {
    return nullptr;
  }
  advance();

  if (!is_curr(lexer::tok_identifier)) {
    throw "Expected variable name, found '" + string(*currTok) + '\'';
  }

  auto ident = ((lexer::IdentifierToken *)currTok)->ident;
  advance();

  if (is_op(lexer::op_eq)) {
    advance();
    return make_unique<ast::VariableDefinition>(ident, parse_expression());
  } else {
    err_unexpected_tok("=");
    return nullptr;
  }
}

unique_ptr<ast::Expression> Parser::parse_expression() {
  return parse_expression_eq();
}

unique_ptr<ast::Expression> Parser::parse_expression_eq() {
  auto node = parse_expression_add();

  while (is_op(lexer::op_eq)) {
    auto op = ((OperatorToken *)currTok)->op;
    advance();
    node = make_unique<ast::BinaryExpresion>(op, std::move(node),
                                             parse_expression_add());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_add() {
  auto node = parse_expression_mul();

  while (is_op(lexer::op_plus) || is_op(lexer::op_minus)) {
    auto op = ((OperatorToken *)currTok)->op;
    advance();
    node = make_unique<ast::BinaryExpresion>(op, std::move(node),
                                             parse_expression_mul());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_mul() {
  auto node = parse_exppression_factor();

  while (is_op(lexer::op_mul) || is_op(lexer::op_div) || is_op(lexer::op_mod)) {
    auto op = ((OperatorToken *)currTok)->op;
    advance();
    node = make_unique<ast::BinaryExpresion>(op, std::move(node),
                                             parse_exppression_factor());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_exppression_factor() {
  if (is_curr(lexer::tok_double)) {
    auto val = ((lexer::F64Literal *)currTok)->literal;
    advance();
    return make_unique<ast::NumberExpression>(val);
  } else if (is_curr(lexer::tok_strliteral)) {
    auto val = ((lexer::StringLiteralToken *)currTok)->literal;
    advance();
    return make_unique<ast::StringExpression>(val);
  } else if (is_curr(lexer::tok_identifier)) {
    auto val = ((lexer::IdentifierToken *)currTok)->ident;
    advance();

    if (!is_op(lexer::op_openparen)) {
      return make_unique<ast::VariableExpression>(val);
    }

    advance();
    vector<unique_ptr<ast::Expression>> args;

    while (!is_op(lexer::op_closeparen)) {
      args.push_back(parse_expression());
      if (is_op(lexer::op_comma)) {
        advance();
      }
    }

    advance();
    return make_unique<ast::CallExpression>(val, std::move(args));
  } else if (is_op(lexer::op_openparen)) {
    advance();
    auto expr = parse_expression();
    if (!is_op(lexer::op_closeparen)) {
      err_unexpected_tok(")");
    }
    advance();
    return expr;
  } else {
    err_unexpected_tok("expression");
    return nullptr;
  }
}

unique_ptr<ast::Prototype> Parser::parse_prototype() {
  if (!is_curr(lexer::tok_identifier)) {
    err_unexpected_tok("function name");
  }

  auto name = string(((IdentifierToken *)currTok)->ident);

  advance();
  if (!is_op(lexer::op_openparen)) {
    err_unexpected_tok("(");
  }

  vector<tuple<string, ast::Primitive>> args;
  // read each argument
  advance();
  while (is_curr(lexer::tok_identifier)) {
    auto argname = ((IdentifierToken *)currTok)->ident;

    advance();
    if (!is_op(lexer::op_colon)) {
      err_unexpected_tok(":");
    }

    advance();
    if (!is_curr(lexer::tok_keyword)) {
      throw string("Unexpected variable type: ") + string(*currTok);
    }

    // reads value type
    ast::Primitive valtype;
    switch (((KeywordToken *)currTok)->word) {
    case lexer::keyword_f64:
      valtype = ast::primitive_f64;
      break;
    case lexer::keyword_f32:
      valtype = ast::primitive_f32;
      break;
    case lexer::keyword_i32:
      valtype = ast::primitive_i32;
      break;
    case lexer::keyword_char:
      valtype = ast::primitive_char;
      break;
    case lexer::keyword_string:
      valtype = ast::primitive_string;
      break;
    case lexer::keyword_void:
      valtype = ast::primitive_void;
    default:
      throw "Unknown type: " + string(*currTok);
    }

    args.emplace_back(argname, valtype);

    advance();
    if (is_op(lexer::op_comma)) {
      advance();
    } else {
      break;
    }
  }
  if (!is_op(lexer::op_closeparen)) {
    err_unexpected_tok(")");
  }
  advance();

  ast::Primitive returntype = ast::primitive_void;

  if (is_op(lexer::op_colon)) {
    advance();

    if (!is_curr(lexer::tok_keyword)) {
      err_unexpected_tok("Keyword");
    }
    switch (((KeywordToken *)currTok)->word) {
    case lexer::keyword_f64:
      returntype = ast::primitive_f64;
      break;
    case lexer::keyword_f32:
      returntype = ast::primitive_f32;
      break;
    case lexer::keyword_i32:
      returntype = ast::primitive_i32;
      break;
    case lexer::keyword_char:
      returntype = ast::primitive_char;
      break;
    case lexer::keyword_string:
      returntype = ast::primitive_string;
      break;
    case lexer::keyword_void:
      returntype = ast::primitive_void;
      break;
    default:
      throw "Unknown type: " + string(*currTok);
    }
    advance();
  }

  return make_unique<ast::Prototype>(name, returntype, args);
}

vector<unique_ptr<ast::Expression>> Parser::parse() {
  try {
    vector<unique_ptr<ast::Expression>> expressions;
    while (!is_curr(lexer::tok_eof)) {
      unique_ptr<ast::Expression> expr;

      if ((expr = parse_function()) != nullptr) {
        expressions.push_back(std::move(expr));

      } else if ((expr = parse_extern()) != nullptr) {
        expressions.push_back(std::move(expr));
      } else {
        throw "Unknown token: " + string(*currTok);
      }
    }

    return expressions;
  } catch (string err) {
    cout << "[PARSER ERROR]: " << err << endl;
    return {};
  }
}

}; // namespace parser
