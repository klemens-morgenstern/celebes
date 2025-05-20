cmake_minimum_required(VERSION 3.29)
project(celebes)

set(CMAKE_CXX_STANDARD 23)

if (LLVM_DIR)
    find_package(LLVM REQUIRED CONFIG PATHS NO_DEFAULT_PATH)
else()
    find_package(LLVM REQUIRED CONFIG PATHS)
endif()

if (Clang_DIR)
    find_package(Clang REQUIRED CONFIG PATHS NO_DEFAULT_PATH)
else()
    find_package(Clang REQUIRED CONFIG PATHS)
endif()

add_library(celebes_lexical STATIC src/lexer.cpp src/parser.cpp)

target_include_directories(celebes_lexical PUBLIC ${LLVM_INCLUDE_DIRS})
target_compile_definitions(celebes_lexical PUBLIC ${LLVM_DEFINITIONS})
target_include_directories(celebes_lexical PUBLIC include)
get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)

message("LLVM Path ${LLVM_DIR}")
message("Clang Path ${Clang_DIR}")
# add_compile_options(-fno-rtti)

add_executable(llvm_playground main.cpp)
target_include_directories(llvm_playground PUBLIC ${LLVM_INCLUDE_DIRS})
target_link_directories(llvm_playground PUBLIC ${LLVM_PATH}/lib)
target_link_libraries(llvm_playground  ${LLVM_AVAILABLE_LIBS} clang-cpp)

add_subdirectory(test)

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test.bc
        COMMAND llvm-as ${CMAKE_CURRENT_SOURCE_DIR}/test.ll -o ${CMAKE_CURRENT_BINARY_DIR}/test.bc
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS test.ll
        COMMENT "Test bc target"
        VERBATIM
)

add_custom_command(OUTPUT test.o
        COMMAND llc test.bc -filetype=obj -o test.o
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/test.bc
        COMMENT "Test.o target"
        VERBATIM
)


add_executable(test_ll test.o test.c)
set_target_properties(test_ll PROPERTIES LINKER_LANGUAGE CXX)
