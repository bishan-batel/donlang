add_library(
  utils
  stringutils.cpp
  stringutils.h
)

target_include_directories(utils PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(utils PUBLIC ${llvm_libs})
