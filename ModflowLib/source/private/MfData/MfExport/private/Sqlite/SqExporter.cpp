//------------------------------------------------------------------------------
// FILE      SqExporter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private/MfData/MfExport/private/Sqlite/SqExporter.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

////////////////////////////////////////////////////////////////////////////////
/// \class SqExporter
/// \brief Base class for exporting data to sqlite.
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqExporter::~SqExporter ()
{
} // SqExporter::~SqExporter
//------------------------------------------------------------------------------
/// \brief Exports an array to SQLite.
///
///        Not a template to avoid putting all the code
///        into the header file, and because we're using a polymorphism
///        approach. Not pure virtual because I don't want to force child
///        classes to define it.
//------------------------------------------------------------------------------
void SqExporter::ExportArray(MfData::Export::NativePackExp* /*a_package*/,
                             const std::string& /*a_arrayName*/, int /*a_size*/,
                             int /*a_iprn*/, const float* /*a_array*/,
                             Real /*a_mult*/, int /*a_layer*/)
{
  ASSERT(false); // You must override this if you want to use it.
} // SqExporter::ExportArray
//------------------------------------------------------------------------------
/// \brief Exports an array to SQLite.
///
///        Not a template to avoid putting all the code
///        into the header file, and because we're using a polymorphism
///        approach. Not pure virtual because I don't want to force child
///        classes to define it.
//------------------------------------------------------------------------------
void SqExporter::ExportArray(MfData::Export::NativePackExp* /*a_package*/,
                             const std::string& /*a_arrayName*/, int /*a_size*/,
                             int /*a_iprn*/, const double* /*a_array*/,
                             Real /*a_mult*/, int /*a_layer*/)
{
  ASSERT(false); // You must override this if you want to use it.
} // SqExporter::ExportArray
//------------------------------------------------------------------------------
/// \brief Exports an array to SQLite.
///
///        Not a template to avoid putting all the code
///        into the header file, and because we're using a polymorphism
///        approach. Not pure virtual because I don't want to force child
///        classes to define it.
//------------------------------------------------------------------------------
void SqExporter::ExportArray(MfData::Export::NativePackExp* /*a_package*/,
                             const std::string& /*a_arrayName*/, int /*a_size*/,
                             int /*a_iprn*/, const int* /*a_array*/,
                             Real /*a_mult*/, int /*a_layer*/)
{
  ASSERT(false); // You must override this if you want to use it.
} // SqExporter::ExportArray
