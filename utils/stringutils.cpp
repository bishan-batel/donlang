#include "stringutils.h"

namespace utils {
bool is_alpha(char c) { return (c >= 65 && c <= 90) || (c >= 97 && c <= 122); }

bool is_numeric(char c) { return 48 <= c && c <= 57; };

bool is_alphanumeric(char c) { return is_alpha(c) || is_numeric(c); }
}; // namespace utils
