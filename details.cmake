if(WIN32)
  set(_library_dir bin) # .dll are in PATH, like executables
else(WIN32)
  set(_library_dir lib) # .so and .dyn
endif(WIN32)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin" CACHE INTERNAL 
  "Where to put the executables"
)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${_library_dir}" 
  CACHE INTERNAL "Where to put the libraries"
)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib" CACHE INTERNAL 
  "Where to put the archives"
)

set(DEFAULT_INSTALL_DESTINATIONS
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION ${_library_dir} 
  ARCHIVE DESTINATION lib
)


# Maybe useful for C++ application and module writers, thus a reusable function
function(generate_doxygen_documentation _target_name _doxygen_config_file)
  find_package(Doxygen)
  if(NOT DOXYGEN_FOUND )
    message(STATUS "Doxygen not found - Doxygen based docs won't be generated")
    return()
  endif()

  if(NOT EXISTS ${_doxygen_config_file})
    message( STATUS "Doxygen configuration file not found - Doxygen based docs won't be generated")
    return()
  endif()

  add_custom_target(${_target_name}
    COMMAND ${DOXYGEN_EXECUTABLE} "${DOXYGEN_CONFIG_FILE}"
  )
endfunction(generate_doxygen_documentation)
