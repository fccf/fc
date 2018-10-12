cmake_minimum_required(VERSION 2.10)
project(fc)

enable_language(Fortran)
enable_testing()

add_compile_options(-fdefault-real-8)
add_compile_options(-std=f2008 -fall-intrinsics -ffree-line-length-none -cpp)
add_compile_options(-o3)

add_subdirectory(src)
