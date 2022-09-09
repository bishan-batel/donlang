add_library(
  utils
  stringutils.cpp
  stringutils.h
)

target_include_directories(utils PUBLIC "${CMAKE_SOURCE_DIR}")
