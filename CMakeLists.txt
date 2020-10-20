cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_USE_RELATIVE_PATHS TRUE)
set(CMAKE_INCLUDE_CURRENT_DIR TRUE)

project("cpp-cas")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

enable_testing()

set(source_list
    "main.cpp"
)
set(target_name "cpp-calc")
add_executable(${target_name} ${source_list})
add_test(NAME BasicAdd COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5+7" "12")
add_test(NAME BasicSub COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5-7" "-2")
add_test(NAME BasicMul COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5*7" "35")
add_test(NAME BasicDiv COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5/2" "2.5")
add_test(NAME BasicPow COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5^2" "25")
add_test(NAME BasicNeg COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5--7" "12")
add_test(NAME BasicPos COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5++7" "12")
add_test(NAME Advanced COMMAND $<TARGET_FILE:${target_name}> "--ctest" "2^3-4/5*6" "3.2")
# add_test(NAME Variable COMMAND $<TARGET_FILE:${target_name}> "--ctest" "5x^2-5x^2" "0")