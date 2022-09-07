#include "lexer/token.h"
#include <memory>
#include <string>
#include <vector>

using namespace std;

namespace ast {

class Expression {
public:
  virtual ~Expression();
};

class NumberExpression : public Expression {
  double value;

public:
  NumberExpression(double val);
};

class VariableExpression : public Expression {
  string name;

public:
  VariableExpression(string name);
};

class BinaryExpresion : public Expression {
  lexer::Operator op;
  unique_ptr<Expression> lhs, rhs;

public:
  BinaryExpresion(lexer::Operator op, unique_ptr<Expression> lhs,
                  unique_ptr<Expression> rhs);
};

class CallExpression : public Expression {
  vector<unique_ptr<Expression>> args;
};

}; // namespace ast
