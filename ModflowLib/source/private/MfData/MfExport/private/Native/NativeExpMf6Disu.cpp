//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Disuu.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Disu.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>


using namespace MfData;
using namespace MfData::Export;


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::NativeExpMf6Disu (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Disu::MfNativeExpMf6Disu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::~NativeExpMf6Disu ()
{
} // MfNativeExpMf6Disu::~MfNativeExpMf6Disu
//------------------------------------------------------------------------------
/// \brief
/// BEGIN OPTIONS
///     [LENGTH_UNITS <length_units>]
///     [NOGRB]
///     [XORIGIN <xorigin>]
///     [YORIGIN <yorigin>]
///     [ANGROT <angrot>]
/// END OPTIONS
/// 
/// BEGIN DIMENSIONS
///     NODES <nodes>
///     NJA <nja>
///     [NVERT <nvert>]
/// END DIMENSIONS
/// 
/// BEGIN GRIDDATA
///     TOP [LAYERED]
///     <top(nodes)> -- READARRAY
///     BOT [LAYERED]
///     <bot(nodes)> -- READARRAY
///     AREA [LAYERED]
///     <area(nodes)> -- READARRAY
/// END GRIDDATA
/// 
/// BEGIN CONNECTIONDATA
///     IAC [LAYERED]
///     <iac(nodes)> -- READARRAY
///     JA [LAYERED]
///     <ja(nja)> -- READARRAY
///     IHC [LAYERED]
///     <ihc(nja)> -- READARRAY
///     CL12 [LAYERED]
///     <cl12(nja)> -- READARRAY
///     HWVA [LAYERED]
///     <hwva(nja)> -- READARRAY
///     [ANGLDEGX [LAYERED]
///     <angldegx(nja)> -- READARRAY]
/// END CONNECTIONDATA
/// 
/// BEGIN VERTICES
///     <iv> <xv> <yv>
///     <iv> <xv> <yv>
///     ...
/// END VERTICES
/// 
/// BEGIN CELL2D
///     <icell2d> <xc> <yc> <ncvert> <icvert(ncvert)>
///     <icell2d> <xc> <yc> <ncvert> <icvert(ncvert)>
///     ...
/// END CELL2D
//------------------------------------------------------------------------------
bool NativeExpMf6Disu::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, desc;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g)
    return false;
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  desc.assign(lines.size(), "");
  m_pack->AddToStoredLinesDesc(lines, desc);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Disu::ExportMf6Dis



///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif