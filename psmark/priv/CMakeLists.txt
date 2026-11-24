cmake_minimum_required(VERSION 4.0)

project(PSMark-DDS-NIFs LANGUAGES C CXX)
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# Base requirements
option(ERTS_ROOT "Root path for the ERTS Installation" "/usr/lib/erlang/erts-15.2.7")

# OpenDDS
option(BUILD_OPENDDS "Build with OpenDDS support" ON)
option(OPENDDS_ROOT "Root path for OpenDDS" "~/OpenDDS-33.3.0")

# CycloneDDS
option(BUILD_CYCLONEDDS "Build with CycloneDDS support" ON)

# CycloneDDS
option(BUILD_FASTDDS "Build with FastDDS support" ON)


set(INSTALL_DIR "${CMAKE_BINARY_DIR}/install")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

# The device harness speaks whichever protocol the observer was built for.
if(BUILD_OPENDDS)
    add_subdirectory(opendds_cplusplus)
endif()

if(BUILD_CYCLONEDDS)
    add_subdirectory(cyclonedds_cplusplus)
endif()

if(BUILD_FASTDDS)
    add_subdirectory(fastdds_cplusplus)
endif()