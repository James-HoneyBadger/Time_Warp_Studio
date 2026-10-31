# SPDX-License-Identifier: MIT
#
# The warning policy, in one place.
#
# Every target in this tree is compiled with the same warnings and the same
# warnings-as-errors switch. That policy used to be written out at each of the 49
# targets -- six lines each, and an edit to the policy was 49 edits, which is the
# kind of change that stops being made. It is a function now.

function(timewarp_enable_warnings target)
    target_compile_options(${target} PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    # `TIME_WARP_EXPR_WERROR` is the historical name -- the expression library grew the
    # flag first -- and it still decides whether a warning stops a build. Renaming it
    # would invalidate every documented `cmake` line for no gain.
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(${target} PRIVATE -Werror)
    endif()
endfunction()

# A suite is an executable, the warnings, and a CTest entry: the three things every
# test in this tree is. Extra libraries and include directories follow the name.
#
#     timewarp_add_suite(sql_tests timewarp::sql)
function(timewarp_add_suite suite)
    add_executable(${suite} tests/${suite}.cpp)
    target_include_directories(${suite} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    if(ARGC GREATER 1)
        target_link_libraries(${suite} PRIVATE ${ARGN})
    endif()
    timewarp_enable_warnings(${suite})
    add_test(NAME ${suite} COMMAND ${suite})
endfunction()
