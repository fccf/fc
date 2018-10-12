# library of fc
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/fc/)

file(GLOB src ${CMAKE_CURRENT_SOURCE_DIR}/fc/*.f90
              ${CMAKE_CURRENT_SOURCE_DIR}/fc/*.h)

add_library(fc STATIC ${src})

if(WITH_NO_TEST)
  set(enable_test_fc OFF CACHE BOOL OFF)
else()
  set(enable_test_fc ON CACHE BOOL OFF)
endif()

if(enable_test_fc)
  add_subdirectory(fc/test)
endif()

#
