#include "parser.h"
#include "lexer/token.h"
#include <ast/ast.h>
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

void Parser::require_keyword(lexer::Keyword key) {
  if (!is_keyword(key)) {
    err_unexpected_tok(string(KeywordToken(key)));
  }
}

bool Parser::is_op(lexer::Operator op) {
  return is_curr(lexer::tok_op) && ((OperatorToken *)currTok)->op == op;
}

void Parser::require_op(lexer::Operator op) {
  if (!is_op(op)) {
    err_unexpected_tok(string(OperatorToken(op)));
  }
}

inline void Parser::err_unexpected_tok(const string &unexpected) {
  throw string("Unexpected Token, expected '" + unexpected + "', found '" +
               string(*currTok) + '\'');
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
    throw string("Unable to parse function signature");

  // multiline functions

  if (!is_op(lexer::op_opencurly)) {
    err_unexpected_tok("{");
  }
  advance();

  vector<unique_ptr<ast::Expression>> body;
  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = parse_inner_function();

    if (expr != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }

    // throw "Unexpected Token, '" + string(*currTok) + '\'';
  }
  advance();

  auto func = make_unique<ast::Function>(std::move(proto), std::move(body));
  return func;
}

unique_ptr<ast::Expression> Parser::parse_inner_function() {
  unique_ptr<ast::Expression> expr = nullptr;

  if ((expr = parse_control_if()) != nullptr) {
    return std::move(expr);
  }

  if ((expr = parse_control_while()) != nullptr) {
    return std::move(expr);
  }

  if ((expr = parse_var_def()) != nullptr) {
    return std::move(expr);
  }
  if ((expr = parse_return()) != nullptr) {
    return std::move(expr);
  }

  if ((expr = parse_expression()) != nullptr) {
    return std::move(expr);
  }

  return nullptr;
}

unique_ptr<ast::Expression> Parser::parse_control_if() {
  if (!is_keyword(lexer::keyword_if)) {
    return nullptr;
  }
  advance();

  auto expr = parse_expression();

  if (!is_op(lexer::op_opencurly)) {
    err_unexpected_tok("{");
  }
  advance();

  vector<unique_ptr<ast::Expression>> body;
  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = parse_inner_function();

    if (expr != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }

    throw string("Unexpected Token, '" + string(*currTok) + '\'');
  }
  advance();

  return make_unique<ast::IfExpression>(std::move(expr), std::move(body),
                                        parse_control_else());
}

vector<unique_ptr<ast::Expression>> Parser::parse_control_else() {
  if (!is_keyword(lexer::keyword_else)) {
    return {};
  }
  advance();

  if (is_keyword(lexer::keyword_if)) {
    auto body = vector<unique_ptr<ast::Expression>>();
    body.push_back(parse_control_if());
    return body;
  }

  if (!is_op(lexer::op_opencurly)) {
    err_unexpected_tok("{");
  }
  advance();

  vector<unique_ptr<ast::Expression>> body;

  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = parse_inner_function();

    if (expr != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }

    throw string("Unexpected Token, '" + string(*currTok) + '\'');
  }
  advance();

  return body;
}

unique_ptr<ast::Expression> Parser::parse_control_while() {
  if (!is_keyword(lexer::keyword_while)) {
    return nullptr;
  }
  advance();

  auto expr = parse_expression();

  if (!is_op(lexer::op_opencurly)) {
    err_unexpected_tok("{");
  }
  advance();

  vector<unique_ptr<ast::Expression>> body;
  while (!is_op(lexer::op_closecurly)) {
    unique_ptr<ast::Expression> expr = parse_inner_function();

    if (expr != nullptr) {
      body.push_back(std::move(expr));
      continue;
    }

    throw string("Unexpected Token, '" + string(*currTok) + '\'');
  }
  advance();

  return make_unique<ast::WhileExpression>(std::move(expr), std::move(body));
}

unique_ptr<ast::Expression> Parser::parse_var_def() {
  auto let = is_keyword(lexer::keyword_let);
  auto var = is_keyword(lexer::keyword_var);

  if (!let && !var) {
    return nullptr;
  }
  advance();

  if (!is_curr(lexer::tok_identifier)) {
    throw string("Expected variable name, found '" + string(*currTok) + '\'');
  }

  auto ident = ((lexer::IdentifierToken *)currTok)->ident;
  advance();

  if (is_op(lexer::op_assign)) {
    advance();
    return make_unique<ast::VariableDefinition>(ident, parse_expression());
  } else {
    err_unexpected_tok("=");
    return nullptr;
  }
}

unique_ptr<ast::Expression> Parser::parse_expression() {
  return parse_expression_and_or();
}

unique_ptr<ast::Expression> Parser::parse_expression_and_or() {
  auto node = parse_expression_is();

  bool isand = is_keyword(lexer::keyword_and);
  bool isor = is_keyword(lexer::keyword_or);

  while (is_keyword(lexer::keyword_and)) {
    advance();
    node = make_unique<ast::BinaryExpresion>(op_and, std::move(node),
                                             parse_expression_is());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_is() {
  auto node = parse_expression_compare();

  while (is_keyword(lexer::keyword_is)) {
    advance();
    node = make_unique<ast::BinaryExpresion>(op_equal, std::move(node),
                                             parse_expression_compare());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_compare() {
  auto node = parse_expression_eq();

  while (is_op(lexer::op_greater_than) || is_op(lexer::op_less_than)) {
    auto op = ((OperatorToken *)currTok)->op;
    advance();
    node = make_unique<ast::BinaryExpresion>(op, std::move(node),
                                             parse_expression_eq());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_eq() {
  auto node = parse_expression_add();

  while (is_op(lexer::op_assign)) {
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
  auto node = parse_expression_cast();

  while (is_op(lexer::op_mul) || is_op(lexer::op_div) || is_op(lexer::op_mod)) {
    auto op = ((OperatorToken *)currTok)->op;
    advance();
    node = make_unique<ast::BinaryExpresion>(op, std::move(node),
                                             parse_expression_cast());
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_expression_cast() {
  auto node = parse_exppression_factor();

  while (is_keyword(keyword_as)) {
    advance();
    auto prim = parser::keyword_to_primitive(
        ((lexer::KeywordToken *)currTok)->word, false);
    node = make_unique<ast::CastExpression>(std::move(node), prim);
    advance();
  }
  return node;
}

unique_ptr<ast::Expression> Parser::parse_exppression_factor() {
  if (is_curr(lexer::tok_double)) {
    auto val = ((lexer::F64Literal *)currTok)->literal;
    advance();
    return make_unique<ast::NumberF64Expression>(val);
  } else if (is_curr(lexer::tok_float)) {
    auto val = ((lexer::F32Literal *)currTok)->literal;
    advance();
    return make_unique<ast::NumberF32Expression>(val);
  } else if (is_curr(lexer::tok_int)) {
    auto val = ((lexer::I32Literal *)currTok)->literal;
    advance();
    return make_unique<ast::NumberI32Expression>(val);
  } else if (is_curr(lexer::tok_char)) {
    auto val = ((lexer::CharLiteral *)currTok)->literal;
    advance();
    return make_unique<ast::NumberI8Expression>(val);
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
  } else if (is_op(lexer::op_minus)) {
    advance();
    return make_unique<ast::UnaryExpression>(lexer::op_minus,
                                             parse_exppression_factor());
  } else if (is_op(lexer::op_not) || is_keyword(lexer::keyword_not)) {
    // Unary not
    advance();
    return make_unique<ast::UnaryExpression>(lexer::op_not,
                                             parse_exppression_factor());
  } else if (is_keyword(lexer::keyword_ref)) {
    advance();
    return make_unique<ast::UnaryExpression>(lexer::op_ref,
                                             parse_exppression_factor());
  } else if (is_keyword(lexer::keyword_deref)) {
    advance();
    return make_unique<ast::UnaryExpression>(lexer::op_deref,
                                             parse_exppression_factor());
  }else if (is_op(lexer::op_openparen)) {
    advance();
    auto expr = parse_expression();
    if (!is_op(lexer::op_closeparen)) {
      err_unexpected_tok(")");
    }
    advance();
    return std::move(expr);
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

  bool varargs = false;
  while (!varargs && is_curr(lexer::tok_identifier)) {

    auto argname = ((IdentifierToken *)currTok)->ident;

    advance();
    if (!is_op(lexer::op_colon)) {
      err_unexpected_tok(":");
    }

    advance();

    if (!is_curr(lexer::tok_keyword)) {
      throw string("Unexpected variable type: " + string(*currTok));
    }

    args.emplace_back(argname, parse_type());

    if (is_op(lexer::op_comma)) {
      advance();

      if (is_keyword(lexer::keyword_varargs)) {
        varargs = true;
        advance();
      }
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

    returntype = parse_type();
    if (returntype == ast::primitive_invalid) {
      returntype = ast::primitive_void;
    }
  }

  return make_unique<ast::Prototype>(name, returntype, args, varargs);
}

ast::Primitive Parser::parse_type() {
  int isPtr = 0;
  while (is_keyword(lexer::keyword_ptr)) {
    isPtr++;
    advance();
    if (!is_curr(lexer::tok_keyword)) {
      return ast::primitive_invalid;
    }
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
    return ast::Primitive::primitive_invalid;
  }
  advance();

  if (isPtr) {
    valtype = ast::primitive_to_ptr(valtype);
  }
  return valtype;
}

vector<unique_ptr<ast::Expression>> Parser::parse() {
  vector<unique_ptr<ast::Expression>> expressions;

  while (!is_curr(lexer::tok_eof)) {
    unique_ptr<ast::Expression> expr;

    if ((expr = parse_function()) != nullptr) {
      expressions.push_back(std::move(expr));

    } else if ((expr = parse_extern()) != nullptr) {
      expressions.push_back(std::move(expr));
    } else {
      throw string("Unknown token: " + string(*currTok));
    }
  }

  return expressions;
}

ast::Primitive keyword_to_primitive(Keyword word, bool isptr) {
  ast::Primitive prim;

  switch (word) {
  case lexer::keyword_f64:
    prim = ast::primitive_f64;
    break;
  case lexer::keyword_f32:
    prim = ast::primitive_f32;
    break;
  case lexer::keyword_i32:
    prim = ast::primitive_i32;
    break;
  case lexer::keyword_char:
    prim = ast::primitive_char;
    break;
  case lexer::keyword_string:
    prim = ast::primitive_string;
    break;
  case lexer::keyword_void:
    prim = ast::primitive_void;
    break;
  case lexer::keyword_bool:
    prim = ast::primitive_bool;
    break;
  default:
    throw string("Unknown type: " + string(KeywordToken(word)));
  }

  if (isptr)
    prim = ast::primitive_to_ptr(prim);

  return prim;
}

}; // namespace parser
