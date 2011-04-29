
find_package(HTMLHelp)
if(HTML_HELP_COMPILER)
  set(GENERATE_HTMLHELP YES)
else()
  set(GENERATE_HTMLHELP NO)
endif()

set(output_dir ${CMAKE_CURRENT_BINARY_DIR}/html)
configure_file(
  Doxyfile.in
  ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  @ONLY
)

#
generate_doxygen_documentation(
  f2k3_lua_simple_docs # make target
  ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
)
# #
# install(
#    DIRECTORY ${output_dir} DESTINATION ${Falcon_DOC_DIR}
#    OPTIONAL # if docs haven't been generated
# )
