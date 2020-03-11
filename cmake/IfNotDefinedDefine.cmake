# Set compile definitions
get_directory_property(${PROJECT_NAME}_COMPILE_DEFINITIONS COMPILE_DEFINITIONS)

function(if_not_defined_define key)
  if(NOT ("${${PROJECT_NAME}_COMPILE_DEFINITIONS}" MATCHES "^${key}"
          OR "${${PROJECT_NAME}_COMPILE_DEFINITIONS}" MATCHES ";${key}"))
    if(${ARGC} GREATER 1)
      add_compile_definitions(${key}=${ARGV1})
    else()
      add_compile_definitions(${key})
    endif()
  endif()
endfunction()
