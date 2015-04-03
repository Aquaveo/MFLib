//------------------------------------------------------------------------------
// FILE      H5UseLastWriter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Native\H5UseLastWriter.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------
#define MFBC_USELAST   "01. Use Last"
#define MFBC_NETSEG    "20. Number of Segments"

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
namespace
{
} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5UseLastWriter::H5UseLastWriter (NativePackExp* a_) :
   m_pack(a_)
{
} // H5UseLastWriter::H5UseLastWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5UseLastWriter::~H5UseLastWriter ()
{
} // H5UseLastWriter::~H5UseLastWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5UseLastWriter::WriteData (std::vector<int>& a_data)
{
  int spIdx(m_pack->GetGlobal()->GetCurrentPeriod()-1);
  CStr fname(m_pack->GetNative()->GetExp()->GetBaseFileName());
  fname += ".h5";
  CStr path(GetH5DataPath());
  H5DataSetWriterSetup setup(fname, path, H5T_NATIVE_INT, 2);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);

  std::vector<hsize_t> start(2,0), n2write(2,1);
  start[1] = spIdx;
  n2write[0] = a_data.size();
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  h.WriteData(&a_data[0], a_data.size());
} // H5ArrayWriter::WriteData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5UseLastWriter::GetH5DataPath ()
{
  CStr rval;
  CStr packName(m_pack->GetPackage()->PackageName());
  if (packName.Find(Packages::RCH) != -1)      rval = "Recharge/";
  else if (packName.Find(Packages::EVT) != -1) rval = "ET/";
  else if (packName.Find(Packages::ETS) != -1) rval = "ETS/";
  else if (packName.Find(Packages::UZF) != -1) rval = "UZF/";
  rval += MFBC_USELAST;
  return rval;
} // H5UseLastWriter::GetH5DataPath
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5UseLastWriter::WriteEtsNetSeg (int a_)
{
  CStr fname(m_pack->GetNative()->GetExp()->GetBaseFileName());
  fname += ".h5";
  CStr path = "ETS/";
  path += MFBC_NETSEG;
  H5DataSetWriterSetup setup(fname, path, H5T_NATIVE_INT, 1);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  h.WriteData(&a_, 1);
} // H5UseLastWriter::WriteEtsNetSeg




