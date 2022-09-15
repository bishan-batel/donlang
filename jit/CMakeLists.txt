add_library(
  jit
  JIT.h
  JIT.cpp
)

target_include_directories(jit PUBLIC "${CMAKE_SOURCE_DIR}")
target_link_libraries(jit PUBLIC ${llvm_libs} parser)
