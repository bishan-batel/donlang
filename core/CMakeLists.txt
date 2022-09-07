add_executable(donlang main.cpp)
target_link_libraries(donlang PUBLIC parser lexer utils ${llvm_libs})

set_target_properties( donlang PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build )
