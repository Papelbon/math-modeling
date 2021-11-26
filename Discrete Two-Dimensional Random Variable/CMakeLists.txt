cmake_minimum_required(VERSION 3.17)

project(d2Drv)

enable_language(Fortran)

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

file(GLOB MAIN "main.f90" "modules/*.f90")

# Include LIBCDF90
find_library(LIBCDF90 libcdf90.a "${CMAKE_SOURCE_DIR}/libs/libcdf90")
include_directories("${CMAKE_SOURCE_DIR}/libs/libcdf90/modules")

add_executable(d2Drv ${MAIN})

target_link_libraries(d2Drv ${LIBCDF90})













