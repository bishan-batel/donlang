add_executable(Piss main.cpp)
target_link_libraries(Piss PUBLIC lexer ${llvm_libs})

set_target_properties( Piss PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build )
