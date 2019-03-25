cmake_minimum_required(VERSION 3.9)

project(aermod)

project(aermod VERSION 18081
               DESCRIPTION "AMS/EPA Regulatory Model (AERMOD)"
               LANGUAGES Fortran)

include(CheckSymbolExists)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR})
set(CMAKE_Fortran_FORMAT FIXED)

# Set default build type to RELEASE.
if(NOT CMAKE_BUILD_TYPE)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# Define AERMOD specific options.
option(AERMOD_NETCDF_SUPPORT "Build AERMOD with netCDF POSTFILE support." OFF)
option(AERMOD_IPC_SUPPORT "Build AERMOD with interprocess communication support." OFF)

# Add netCDF and dependencies if enabled.
if(AERMOD_NETCDF_SUPPORT)
  # HDF5, C and HL
  option(AERMOD_SHARED_HDF5 "Use shared HDF5 library." OFF)
  if(AERMOD_SHARED_HDF5)
    set(HDF5_USE_STATIC_LIBRARIES OFF)
    set(SZIP_LIBRARY_NAME "szip")
    set(ZLIB_LIBRARY_NAME "zlib")
  else()
    set(HDF5_USE_STATIC_LIBRARIES ON)
    set(SZIP_LIBRARY_NAME "libszip")
    set(ZLIB_LIBRARY_NAME "libzlib")
  endif()
  set(HDF5_FIND_DEBUG OFF)
  find_package(HDF5 COMPONENTS C HL REQUIRED)
  
  message(STATUS "Found HDF5: ${HDF5_C_LIBRARY}")
  message(STATUS "Found HDF5-HL: ${HDF5_C_HL_LIBRARY}")
  set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${HDF5_C_LIBRARY})
  set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${HDF5_C_HL_LIBRARY})
  
  # Use HDF5 C library path as hint for find_library.
  get_filename_component(SEARCH_PATH ${HDF5_C_LIBRARY} DIRECTORY)

  # Set include search path for H5pubconf.h.
  set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${HDF5_INCLUDE_DIRS})
  
  # Check whether HDF5 is configured with SZIP.
  check_symbol_exists(H5_HAVE_FILTER_SZIP H5pubconf.h HAVE_SZIP)
  if(HAVE_SZIP)
    find_library(SZIP_LIBRARY NAMES ${SZIP_LIBRARY_NAME} HINTS ${SEARCH_PATH})
    if(SZIP_LIBRARY)
      message(STATUS "Found szip: ${SZIP_LIBRARY}")
      set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${SZIP_LIBRARY})
    else()
      message(FATAL_ERROR "HDF5 built with SZIP support, but cannot find ${SZIP_LIBRARY_NAME}.")
    endif()
  endif()

  # Check whether HDF5 is configured with DEFLATE (requires zlib).
  check_symbol_exists(H5_HAVE_FILTER_DEFLATE H5pubconf.h HAVE_DEFLATE)
  if(HAVE_DEFLATE)
    find_library(ZLIB_LIBRARY NAMES ${ZLIB_LIBRARY_NAME} HINTS ${SEARCH_PATH})
    if(ZLIB_LIBRARY)
      message(STATUS "Found zlib: ${ZLIB_LIBRARY}")
      set(ZLIB_LIBRARY ${ZLIB_LIBRARY})
      set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${ZLIB_LIBRARY})
    else()
      message(FATAL_ERROR "netCDF support requires zlib.")
    endif()
  else()
    message(FATAL_ERROR "HDF5 must be built with DEFLATE support.")
  endif()

  # netCDF, C and F90
  set(NETCDF_F90 "YES")
  find_package(NetCDF REQUIRED)
  message(STATUS "Found netCDF-C: ${NETCDF_C_LIBRARIES}")
  message(STATUS "Found netCDF-Fortran: ${NETCDF_F90_LIBRARIES}")
  set(AERMOD_INCLUDE_DIRS ${AERMOD_INCLUDE_DIRS} ${NETCDF_INCLUDE_DIRS})
  set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${NETCDF_LIBRARIES})
  
  # Set include search path for netcdf_meta.h.
  set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${NETCDF_INCLUDE_DIRS})
  
  # Make sure netCDF has API version 4 support.
  check_symbol_exists(NC_HAS_NC4 netcdf_meta.h HAVE_NC4)
  if(NOT HAVE_NC4)
    message(FATAL_ERROR "netCDF-C must be built with API version 4 support.")
  endif()
  
  # Check whether netCDF is configured with DAP (requires libcurl).
  check_symbol_exists(NC_HAS_DAP2 netcdf_meta.h HAVE_DAP2)
  check_symbol_exists(NC_HAS_DAP4 netcdf_meta.h HAVE_DAP4)
  if(HAVE_DAP2 OR HAVE_DAP4)
    find_package(CURL REQUIRED)
    message(STATUS "Found libcurl: ${CURL_LIBRARIES}")
    set(AERMOD_LINK_LIBS ${AERMOD_LINK_LIBS} ${CURL_LIBRARIES})
  endif()
endif()

# Compiler flags.
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fpp -check:format -Qipo -Qprec-div- -QaxSSE2 -trace -Qdiag-disable:8291")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -fpp -check:format -Qipo -Qprec-div- -QaxSSE2 -trace -Qdiag-disable:8291")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fbounds-check -Wuninitialized -static")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -fbounds-check -Wuninitialized -static")
else()
  get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
  message("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message(WARNING "Using default Fortran compiler flags.")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0")
endif()

# Add binary directory to includes for Fortran modules.
set(AERMOD_INCLUDE_DIRS ${AERMOD_INCLUDE_DIRS} ${PROJECT_BINARY_DIR})

add_executable(aermod
               ipc.f
               modules.f
               ncpost.f
               aermod.f
               setup.f
               coset.f
               soset.f
               reset.f
               meset.f
               ouset.f
               inpsum.f
               metext.f
               iblval.f
               siggrid.f
               tempgrid.f
               windgrid.f
               calc1.f
               calc2.f
               prise.f
               prime.f
               sigmas.f
               pitarea.f
               uninam.f
               output.f
               evset.f
               evcalc.f
               evoutput.f)

if(AERMOD_NETCDF_SUPPORT)
  target_compile_definitions(aermod PRIVATE ENABLE_NETCDF=1)
endif()

if(AERMOD_IPC_SUPPORT)
  target_compile_definitions(aermod PRIVATE ENABLE_IPC=1)
endif()

target_include_directories(aermod PRIVATE ${AERMOD_INCLUDE_DIRS})
target_link_libraries(aermod PRIVATE ${AERMOD_LINK_LIBS})

