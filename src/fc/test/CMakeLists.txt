################################################################################
#
# CMake File for fc unit tests
#
# Description: Adds the subdirectories for the unit tests
#
# Author: Fang Chao
#   Date: 20181012
#
################################################################################
include_directories(${CMAKE_CURRENT_BINARY_DIR}/../../) # .mod file
# List the names of all the tests (same as subdirectory)
set(test_names
  test_debug
  test_face
  test_file
  test_option
  test_string
  test_time
)

foreach(test ${test_names})
  file(GLOB src ${CMAKE_CURRENT_SOURCE_DIR}/${test}/*.f90)
  add_executable(${test} ${src})
  target_link_libraries(${test} fc)
  add_test(${test} ${test})
endforeach()
