//------------------------------------------------------------------------------
// FILE      NativeExpNam.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>

#include <sstream>
//
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfPackageUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpNam::NativeExpNam ()
{
} // MfNativeExpNam::MfNativeExpNam
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpNam::~NativeExpNam ()
{
} // MfNativeExpNam::~MfNativeExpNam
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpNam::Export ()
{
  using namespace MfData::Packages;
  const char *t(0), *n(0);
  const int *iu;
  if (!GetPackage()->GetField(NameFile::FNAME, &n) || !n ||
      !GetPackage()->GetField(NameFile::FTYPE, &t) || !t ||
      !GetPackage()->GetField(NameFile::NIU, &iu) || !iu)
  {
    ASSERT(0);
    return false;
  }

  CStr type = t;
  if (type.CompareNoCase("BCF6") == 0)
  {
    GetGlobal()->SetIntVar("Uses BCF", 1);
  }
  std::vector<CStr>& lines(GetPackage()->StringsToWrite());
  std::vector<CStr>& desc(GetPackage()->StringDescriptions());

  desc.push_back(" 1. Ftype Nunit Fname [Option]");

  CStr str = type;
  while (str.size() < 6) str += " ";
  CStr unit;
  unit.Format("%5d  ", *iu);
  str += unit;
  str += n;
  lines.push_back(str);

  return true;
} // MfNativeExpNam::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpNam::WriteFileStp ()
{
  // read the ftype and unit numbers
  std::vector<CStr>& lines(GetPackage()->StringsToWrite());
  std::vector<CStr>& desc(GetPackage()->StringDescriptions());
  std::vector<CStr> ftype(lines.size(), ""), fname(lines.size(), "");
  std::vector<int> unit(lines.size(), 0);
  bool multExists(0), zoneExists(0);
  for (size_t i=0; i<lines.size(); ++i)
  {
    std::stringstream ss;
    ss << lines[i];
    ss >> ftype[i] >> unit[i] >> fname[i];
    if (ftype[i].CompareNoCase("MULT") == 0) multExists = true;
    if (ftype[i].CompareNoCase("ZONE") == 0) zoneExists = true;
  }
  if (!multExists && GetGlobal()->GetPackage(Packages::MLT))
  {
    ftype.push_back("MULT");
    unit.push_back(*std::max_element(unit.begin(), unit.end())+1);
    fname.push_back("mult.mlt");
  }
  if (!zoneExists && GetGlobal()->GetPackage(Packages::ZON))
  {
    ftype.push_back("ZONE");
    unit.push_back(*std::max_element(unit.begin(), unit.end())+1);
    fname.push_back("zone.zon");
  }

  lines.resize(0);
  desc.resize(0);
  TxtExporter* exp = GetNative()->GetExp();
  std::set<CStr> uniqueNames, setNativeSupported;
  setNativeSupported.insert("OBS");
  setNativeSupported.insert("HOB");
  setNativeSupported.insert("CHOB");
  setNativeSupported.insert("DROB");
  setNativeSupported.insert("DTOB");
  setNativeSupported.insert("GBOB");
  setNativeSupported.insert("RVOB");
  setNativeSupported.insert("STOB");

  CStr file, baseName = exp->GetBaseFileName();
  util::StripPathFromFilename(baseName, baseName);
  for (size_t i=0; i<ftype.size(); ++i)
  {
    CStr type = ftype[i];
    type.ToLower();

    if (!ExportFileType(type)) continue;

    if (type.find("data") != -1)
    {
      file = fname[i];
      //CStr extension(fname[i]);
      //util::StripAllButExtension(extension, extension);
      //GetNative()->BuildUniqueName(baseName, extension, unit[i],
      //                             uniqueNames, file);
    }
    else if (exp->IsTypeSupported(ftype[i]) ||
             setNativeSupported.find(ftype[i]) != setNativeSupported.end())
    {
      GetNative()->BuildUniqueName(baseName, exp->GetExtension(ftype[i]),
                                   unit[i], uniqueNames, file);
    }
    else
    {
      GetNative()->BuildUniqueName(baseName, type, unit[i], uniqueNames, file);
      // copy unsupported file
      CStr sourceFile(fname[i]);
      CStr destPath;
      util::StripFileFromFilename(exp->GetBaseFileName(), destPath);
      CStr destFile(destPath + file);
      util::FileCopy(sourceFile, destFile);
    }
    CStr unitStr, fline;
    unitStr.Format("%d", unit[i]);
    fline = ftype[i];
    while (fline.GetLength() < 20) fline += " ";
    fline += unitStr;
    while (fline.GetLength() < 30) fline += " ";
    fline += file;
    lines.push_back(fline);
    desc.push_back(" 1. Ftype Nunit Fname [Option]");
  }

  WriteComments();
  WriteStoredLines();
} // NativeExpNam::WriteFileStp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpNam::ExportFileType (const CStr& a_type)
{
  if ("asp" == a_type) return false;

  CStr varStr;
  if ("pval" == a_type) varStr = "PVAL_Exported";
  else if ("sen" == a_type) varStr = "SEN_Exported";

  if (!varStr.IsEmpty())
  {
    int val;
    if (!GetGlobal()->GetIntVar(varStr, val) || !val) return false;
  }
  return true;
} // NativeExpNam::ExportFileType

