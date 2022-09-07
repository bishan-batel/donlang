//#include "../lexer/lexer.h"
#include "lexer/token.h"
#include <fstream>
#include <iostream>
#include <lexer/lexer.h>
#include <sstream>
#include <string>
#include <utils/stringutils.h>

using namespace std;

int main(int nargs, const char *argv[]) {
  if (nargs == 1) {
    cout << "ERROR: Requires argument for filename" << endl;
    return 1;
  }

  ifstream file(argv[1]);

  if (file) {
    cout << "Compiling file " << argv[1] << endl;
    stringstream stream;
    file >> stream.rdbuf();
    file.close();

    string contents = stream.str();
    cout << "File byte length: " << contents.length() << endl;
    cout << endl << "======================================" << endl;

    lexer::Lexer lexer(contents);
    lexer.tokenize();
    printf("Finished Tokenization\n");
    auto tokens = lexer.getTokens();
    cout << "Tokens: " << endl;
    for (auto tok : tokens) {
      cout << tok->name() << ' ' << endl;
    }
    cout << endl;
  } else {
    cout << "ERROR: File \"" << argv[0] << "\" does not exist" << endl;
    return 1;
  }

  return 0;
}
