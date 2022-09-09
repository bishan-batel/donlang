#include "parser.h"
#include "ast.h"
#include "lexer/token.h"
#include <memory>
#include <tuple>
#include <vector>

namespace parser {

using namespace std;

Parser::Parser(vector<lexer::Token *> toks) : tokens(toks) {
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

void Parser::err_unexpected_tok(string unexpected) {
  throw "Unexpected Token, expected '" + unexpected + "', found '" +
      string(*currTok) + '\'';
}

unique_ptr<ast::Function> Parser::parse_function() {
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

  vector<unique_ptr<ast::Expression>> body;

  advance();
  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = nullptr;

    if ((expr = parse_var_def()) == nullptr) {
      body.push_back(move(expr));
      continue;
    }

    if ((expr = parse_expression()) == nullptr) {
      body.push_back(move(expr));
      continue;
    }

    throw "Unexpected Token, '" + string(*currTok) + '\'';
  }

  // TODO parse body
  return make_unique<ast::Function>(move(proto), move(body));
}

unique_ptr<ast::Expression> Parser::parse_var_def() {
  if (!is_keyword(lexer::keyword_let)) {
    return nullptr;
  }

  if (!is_curr(lexer::tok_identifier)) {
    throw "Expected variable name, found '" + string(*currTok) + '\'';
  }

  auto ident = ((lexer::IdentifierToken *)currTok)->ident;
  advance();
  if (is_op(lexer::op_eq)) {
    return make_unique<ast::VariableDefinition>(ident, parse_expression());
  } else {
    err_unexpected_tok("=");
    return nullptr;
  }
}

unique_ptr<ast::Expression> Parser::parse_expression() {
  return parse_expression_add();
}

unique_ptr<ast::Expression> Parser::parse_expression_add() {
  auto node = parse_expression_mul();

  while (is_op(lexer::op_plus) || is_op(lexer::op_minus)) {
    advance();
    auto op = ((OperatorToken *)currTok)->op;
    node = make_unique<ast::BinaryExpresion>(op, move(node),
                                             parse_expression_mul());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_mul() {
  auto node = parse_exppression_factor();

  while (is_op(lexer::op_mul) || is_op(lexer::op_div) || is_op(lexer::op_mod)) {
    advance();
    auto op = ((OperatorToken *)currTok)->op;
    node = make_unique<ast::BinaryExpresion>(op, move(node),
                                             parse_exppression_factor());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_exppression_factor() {
  return nullptr;
}

unique_ptr<ast::Prototype> Parser::parse_prototype() {
  if (!is_curr(lexer::tok_identifier)) {
    err_unexpected_tok("function name");
  }

  string name = ((IdentifierToken *)currTok)->ident;
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
      throw string("Unexpected token: ") + string(*currTok);
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
    default:
      throw string("Unknown type: ") + string(*currTok);
    }

    args.push_back(make_tuple(argname, valtype));

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

  return make_unique<ast::Prototype>(name, args);
}

void Parser::parse() {
  auto proto = parse_function();
  cout << (string(*proto.get())) << endl;
}

}; // namespace parser
