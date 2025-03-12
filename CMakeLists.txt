cmake_minimum_required(VERSION 3.29)
project(celebes)

set(CMAKE_CXX_STANDARD 23)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

add_library(celebes_lexical STATIC src/lexer.cpp)

target_include_directories(celebes_lexical PUBLIC ${LLVM_INCLUDE_DIRS})
target_compile_definitions(celebes_lexical PUBLIC ${LLVM_DEFINITIONS})
target_include_directories(celebes_lexical PUBLIC include)

add_executable(llvm_playground main.cpp)
target_link_libraries(llvm_playground  ${llvm_libs} celebes_lexical)

add_subdirectory(test)