cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

################### Variables. ####################
# Change if you want modify path or other values. #
###################################################

set(PROJECT_NAME ModflowLib)
set(CMAKE_DEBUG_POSTFIX _d)


# Output Variables
set(OUTPUT_DEBUG Debug/bin)
set(OUTPUT_RELEASE Release/bin)

# Build Options
SET(BUILD_TESTING NO CACHE BOOL "Build an executable to test the library built")
SET(USE_DBLPREC_AS_REAL NO CACHE BOOL "Define Fortran real as double")

############## CMake Project ################
#        The main options of project        #
#############################################

project(${PROJECT_NAME} CXX)

# Conan
include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)
set(EXT_INCLUDE_DIRS ${CONAN_INCLUDE_DIRS})
set(EXT_LIB_DIRS ${CONAN_LIB_DIRS})
set(EXT_LIBS ${CONAN_LIBS})
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/ModflowLib/source/")
include_directories(${EXT_INCLUDE_DIRS})
link_directories(${EXT_LIB_DIRS})

# Define Release by default.
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release")
  message(STATUS "Build type not specified: Use Release by default.")
endif(NOT CMAKE_BUILD_TYPE)

# Definition of Macros
add_definitions(
   -DSTATICLIB 
   -DNDEBUG 
   -D_CONSOLE 
   -DDBLPREC 
   -D_SECURE_SCL=0 
)

############## Artefacts Output #################
# Defines outputs , depending Debug or Release. #
#################################################

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_DEBUG}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_DEBUG}")
  set(CMAKE_EXECUTABLE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_DEBUG}")
else()
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_REL}")
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_REL}")
  set(CMAKE_EXECUTABLE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${OUTPUT_REL}")
endif()

################# Flags ################
# Defines Flags for Windows and Linux. #
########################################

if(MSVC)
   set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /W4 /EHsc")
   set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /W4 /EHsc")
endif(MSVC)
if(NOT MSVC)
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
   if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
       set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
   endif()
endif(NOT MSVC)

################ Files ################
#   --   Add files to project.   --   #
#######################################

SET(MFLIB_SOURCES
    ModflowLib/source/private/Parameters/MultArray.cpp
    ModflowLib/source/private/Parameters/EReadAsciiFile.cpp
    ModflowLib/source/private/Parameters/ParamFileReader.cpp
    ModflowLib/source/private/Parameters/ParamUtil.cpp
    ModflowLib/source/private/Parameters/ParamList.cpp
    ModflowLib/source/private/Parameters/PilotPoints.cpp
    ModflowLib/source/private/Parameters/SenFileReader.cpp
    ModflowLib/source/private/MfData.cpp
    ModflowLib/source/private/samg/samg.cpp
    ModflowLib/source/private/ArrayReader/ArrayReaderParser.cpp
    ModflowLib/source/private/MfLibAsserts.cpp
    ModflowLib/source/private/ListReader/ListReaderH5.cpp
    ModflowLib/source/private/ListReader/ListReaderParser.cpp
    ModflowLib/source/private/Parameters.cpp
    ModflowLib/source/private/util/EReadAsciiFile.cpp
    ModflowLib/source/private/util/CToken.cpp
    ModflowLib/source/private/util/H5Initialize.cpp
    ModflowLib/source/private/util/util.cpp
    ModflowLib/source/private/ListReaderStr.cpp
    ModflowLib/source/private/Sfr2Reader.cpp
    ModflowLib/source/private/ListReader.cpp
    ModflowLib/source/private/SQLite/CppSQLite3.cpp
    ModflowLib/source/private/SQLite/sqlite3.c
    ModflowLib/source/private/CmdLine.cpp
    ModflowLib/source/private/H5DataReader/H5DataSetReader.cpp
    ModflowLib/source/private/H5DataReader/H5DataSetWriter.cpp
    ModflowLib/source/private/H5DataReader/H5DataReaderUtil.cpp
    ModflowLib/source/private/H5DataReader/H5VecCStrReader.cpp
    ModflowLib/source/private/ArrayReader.cpp
    ModflowLib/source/private/MNWReader.cpp
    ModflowLib/source/private/MfData/MfGlobal.cpp
    ModflowLib/source/private/MfData/MfExport/private/MfExportUtil.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLstObs.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativePackExp.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpRch.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLak.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSwi.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpGag.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpBas.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpBct.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSTP.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSfr.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpDis.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpVsc.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpMnw2.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Sto.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Lak.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Rch.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Disu.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Npf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Sfr.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Uzf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Tdis.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Dis.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Ic.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Evt.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Oc.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Maw.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Mfsim.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6Nam.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/mf6/NativeExpMf6LstPack.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpPes.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpMlt.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSub.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLgr.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpDisu.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpZon.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpHuf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpDpt.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpOc.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpEts.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpMnw1.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpEvt.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpMnwi.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpDdf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/Mnw1PropList.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeUtil.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpArr1d.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLstPack.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpGnc.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpStr.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLpf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSen.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpCln.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpUzf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpHfb.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpNam.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NeArealPar.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpVdf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpSolver.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpArr2d.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpBcf.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpObs.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpLstPar.cpp
    ModflowLib/source/private/MfData/MfExport/private/Native/NativeExpPval.cpp
    ModflowLib/source/private/MfData/MfExport/private/H5/H5LstPack.cpp
    ModflowLib/source/private/MfData/MfExport/private/H5/H5Util.cpp
    ModflowLib/source/private/MfData/MfExport/private/H5/H5ArrayWriter.cpp
    ModflowLib/source/private/MfData/MfExport/private/H5/H5UseLastWriter.cpp
    ModflowLib/source/private/MfData/MfExport/private/H5/H5BcList.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqFile.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqArrayWriter.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqDisu.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqMfSchema.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqBcList.cpp
    ModflowLib/source/private/MfData/MfExport/private/Sqlite/SqExporter.cpp
    ModflowLib/source/private/MfData/MfExport/private/MfExporterImpl.cpp
    ModflowLib/source/private/MfData/MfExport/private/Mf2kNative.cpp
    ModflowLib/source/private/MfData/MfExport/private/TxtExporter.cpp
    ModflowLib/source/private/MfData/MfExport/private/ExpGeoDb.cpp
    ModflowLib/source/private/MfData/MfExport/private/Gdb.cpp
    ModflowLib/source/private/MfData/MfExport/private/CellNumbering.cpp
    ModflowLib/source/private/MfData/MfExport/MfExporter.cpp
    ModflowLib/source/private/MfData/MfPackageUtil.cpp
    ModflowLib/source/private/MfData/Packages/MfPackage.cpp
    ModflowLib/source/private/MfData/Packages/ObsHd.cpp
    ModflowLib/source/private/Gms2Mf2k/Converter.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/ProcessorSFR2.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/ProcessorUtil.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/Processor.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/ProcessorMNW2.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/ProcessorMNW.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/Processor/ProcessorStream.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/DisFileReader.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/FileProcessor.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/LgrFileReader.cpp
    ModflowLib/source/private/Gms2Mf2k/Files/NameFileReader.cpp
    ModflowLib/source/RunTest.cpp
    ModflowLib/source/Export.cpp
    ModflowLib/source/ModflowLib.cpp
)

if(BUILD_TESTING)
    add_definitions(-DCXX_TEST -DCXXTEST4)

    LIST(APPEND MFLIB_SOURCES
      ModflowLib/source/private/ArrayReader/ArrayReaderParser.t.cpp
      ModflowLib/source/private/Gms2Mf2k/Gms2Mf2k.t.cpp
      ModflowLib/source/private/H5DataReader/H5DataSetReader.t.cpp
      ModflowLib/source/private/H5DataReader/H5VecCStrReader.t.cpp
      ModflowLib/source/private/ListReader/CellIdToIJK.t.cpp
      ModflowLib/source/private/ListReader/ListReaderH5.t.cpp
      ModflowLib/source/private/ListReader/ListReaderParser.t.cpp
      ModflowLib/source/private/Parameters/Param.t.cpp
    )

    find_package(CxxTest)
    if(CXXTEST_FOUND)
      include_directories(${CXXTEST_INCLUDE_DIRS})
      enable_testing()

      set(CXXTEST_TESTGEN_ARGS --xunit-printer)
      file(GLOB_RECURSE test_headers ${CMAKE_CURRENT_LIST_DIR}/ModflowLib/*.t.h)
      CXXTEST_ADD_TEST(
        runner runner.cpp ${test_headers}
      )
      target_link_libraries(runner ${PROJECT_NAME})

      if(WIN32)
        set(RUNNERNAME "runner.exe")
      else()
        set(RUNNERNAME "runner")
      endif()
    endif()
endif(BUILD_TESTING)
# Add library to build.
add_library(${PROJECT_NAME} STATIC
    ${MFLIB_SOURCES}
)
target_link_libraries(${PROJECT_NAME}
  ${EXT_LIBS}
)
