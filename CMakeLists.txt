cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_USE_RELATIVE_PATHS TRUE)
set(CMAKE_INCLUDE_CURRENT_DIR TRUE)

project("cpp-cas")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

set(source_list
    "main.cpp"
)
set(target_name "cpp-cas")
add_executable(${target_name} ${source_list})