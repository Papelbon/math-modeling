cmake_minimum_required(VERSION 3.17)

project(queueing_system_simulation)

enable_language(Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

file(GLOB MAIN "modules/*.f90" "modules/queueing_system/*.f90" "modules/csv_file/csv_file.f90")

add_executable(qss_ex1 "example1.f90" ${MAIN})
add_executable(qss_ex2 "example2.f90" ${MAIN})
add_executable(qss_ex3 "example3.f90" ${MAIN})
add_executable(qss_ex4 "example4.f90" ${MAIN})
add_executable(qss_ex5 "example5.f90" ${MAIN})
