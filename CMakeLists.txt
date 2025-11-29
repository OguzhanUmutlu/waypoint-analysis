cmake_minimum_required(VERSION 4.0)
project(waypoint_analysis)

set(CMAKE_CXX_STANDARD 20)

find_package(SFML COMPONENTS Network Graphics Window Audio System CONFIG REQUIRED)

add_executable(waypoint_analysis
        src/main.cpp
        src/main.hpp
        src/plane.hpp
        src/plane.cpp
        src/Unit.hpp
        src/Vector2.hpp
        src/Rect.hpp
        src/safe_mutex.hpp)

target_link_libraries(waypoint_analysis PRIVATE SFML::Network SFML::Graphics SFML::Window SFML::Audio SFML::System)

if(MSVC)
    add_compile_options(/O2)
else()
    add_compile_options(-O3)
endif()
