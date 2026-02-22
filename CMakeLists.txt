cmake_minimum_required(VERSION 3.10)
project(SquareLang LANGUAGES C)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

if (MSVC)
    add_compile_options(
        /W4
        /wd4100
        /wd4189
        /wd4201
        /wd4127
    )
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
    set(CMAKE_C_FLAGS_DEBUG   "/Zi /Od")
    set(CMAKE_C_FLAGS_RELEASE "/O2 /DNDEBUG")
else()
    add_compile_options(
        -Wall
        -Wextra
        -Wno-unused-parameter
        -Wno-unused-variable
    )
    set(CMAKE_C_FLAGS_DEBUG   "-g -O0")
    set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")
endif()

file(GLOB_RECURSE SQUARE_SOURCES CONFIGURE_DEPENDS
    ${CMAKE_SOURCE_DIR}/src/*.c
)
add_executable(square ${SQUARE_SOURCES} ${COMMON_SOURCES})
target_compile_features(square PRIVATE c_std_11)
