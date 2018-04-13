//------------------------------------------------------------------------------
// FILE      MfExportUtil.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\MfExportUtil.h>

#include <sstream>

#include <private\util\util.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\MfExporter.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\MfExport\private\ExpGeoDb.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\Parameters.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief returns the right type of MfExporter.
//------------------------------------------------------------------------------
MfExporterImpl* MfExportUtil::CreateExporter (const char *a_type)
{
  MfExporterImpl *ret(NULL);
  CStr type(a_type);
  type.ToLower();
  if (type == "-exportgeodb")
  {
    ret = new ExpGeoDb;
  }
  else if (type == "-exportgeodbfree")
  {
    ret = new ExpGeoDbFree;
  }
  else if (type == "-exportsqlite")
  {
    ret = new ExpGeoDbSQLite;
  }
  else
  {
    Mf2kNative *n(new Mf2kNative());
    if (!n) return ret;
    if (type == "-exportgmsh5" || type == "-exportgmscompressedh5")
    {
      bool compress(false);
      if (type == "-exportgmscompressedh5") compress = true;
      n->SetUseH5(true, compress);
    }
    else if (type == "-exporttext_arraysinfolder")
    {
      n->SetArraysInternal(false);
      n->SetArraysInFolder(true);
    }
    else if ("-exportmf6" == type )
    {
      n->SetExportMf6(true);
    }
    else if ("-exportgmssqlite" == type)           n->SetUseSQLite(true);
    else if ("-exporttext" == type)                n->SetArraysInternal(false);
    else if ("-exporttext_arraysinternal" == type) {}
    else if ("-exporttextai" == type)              {}
    ret = n;
  }
  return(ret);
} // MfExportUtil::GetExporter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfExportUtil::IsDataArray (const CStr &a_name,
                                std::map<CStr, CStr>& a_map)
{
  std::map<CStr,CStr> &m(a_map);
  std::map<CStr,CStr>::iterator it(m.find(a_name));
  
  if (it != m.end()) return true; // it is in our map return true

  if (   a_name.find("MULT. ARRAY:") != -1              // multiplier array
      || a_name.find("ZONE ARRAY:") != -1               // zone array
     )
  {
    return true;
  }
  if (   a_name.find("TOP ELEVATN: ") != -1             // HGU top
      || a_name.find("THICKNESS: ") != -1               // HUG thick
     )
  { // add the HUF arrays to the map
    CStr str(a_name);
    str.Replace("TOP ELEVATN: ", "top_");
    str.Replace("THICKNESS: ", "thick_");
    m.insert(std::make_pair(a_name, str));
    return true;
  }
  // SWI
  else if (a_name.find("ZETA SURFACE") != -1) {
    // The number is in a field 3 wide, which makes it harder to remove
    int pos = a_name.ReverseFind(' ');
    CStr s = "zeta_" + a_name.Right(a_name.GetLength() - pos - 1);
    m.insert(std::make_pair(a_name, s));
    return true;
  }

  return false;
} // MfExportUtil::IsDataArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfExportUtil::Is1dArray (const CStr &a_name)
{
  using namespace MfData::Packages;
  if (a_name == "DELR" || a_name == "DELC" ||
      a_name == "COLUMN TO ROW ANISOTROPY" ||
      a_name == Disu::LAYCBD || a_name == Disu::NODLAY ||
      a_name == Disu::TOP || a_name == Disu::BOT ||
      a_name == Disu::AREA || a_name == Disu::IA ||
      a_name == Disu::JA || a_name == Disu::IVC ||
      a_name == Disu::CL1 || a_name == Disu::CL2 ||
      a_name == Disu::CL12 || a_name == Disu::FAHL ||
      a_name == Cln::NNDCLN || a_name == Cln::IAC_CLN ||
      a_name == Cln::JA_CLN || a_name == Cln::IBOUND ||
      a_name == Cln::IB0 || a_name == Cln::STRT ||
      a_name == Swi::NUZONE || a_name == Swi::NUSURF)
  {
    return true;
  }
  return false;
} // MfExportUtil::Is1dArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfExportUtil::IsSolver (const CStr &a_name)
{
  using namespace MfData::Packages;
  if (SIP == a_name  || DE4Line2 == a_name ||
      SOR == a_name  || PCG == a_name ||
      PCGN == a_name || LMG == a_name ||
      GMG == a_name  || NWT == a_name ||
      SMS == a_name)
  {
    return true;
  }
  return false;
} // MfExportUtil::IsSolver
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::set<CStr> MfExportUtil::LpfParamTypes ()
{
  std::set<CStr> types;
  types.insert("hk");
  types.insert("hani");
  types.insert("vk");
  types.insert("vani");
  types.insert("ss");
  types.insert("sy");
  types.insert("vkcb");
  return types;
} // MfExportUtil::LpfParamTypes
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::set<CStr> MfExportUtil::HufParamTypes ()
{
  std::set<CStr> a_types;
  a_types.insert("hk");
  a_types.insert("hani");
  a_types.insert("vk");
  a_types.insert("vani");
  a_types.insert("ss");
  a_types.insert("sy");
  a_types.insert("sytp");
  return a_types;
} // MfExportUtil::HufParamTypes
//------------------------------------------------------------------------------
/// \brief Inserts single quotes in a name when necessary.
//------------------------------------------------------------------------------
void MfExportUtil::InsertSingleQuotesInName (CStr &a_name)
{
  if (a_name.Find(" ") != -1 || a_name.Find(",") != -1)
  {
    int idx = a_name.GetLength();

    if (a_name.Find("\n") != -1)
    {
      idx = a_name.Find("\n");
    }

    a_name.Insert(idx, "'");
    a_name = "'" + a_name;
  }
} // MfExportUtil::InsertSingleQuotesInName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
std::vector<Param> MfExportUtil::GetParamsOfType (const char * const a_type)
{
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  std::vector<Param> params;
  if (list != NULL)
  {
    for (size_t i = 0; i < list->Size(); ++i)
    {
      Param param;
      list->At(i, &param);
      if (param.m_type.CompareNoCase(a_type) == 0)
        params.push_back(param);
    }
  }
  return params;
} // MfExportUtil::GetParamsOfType
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfExportUtil::ArrayWriteNextLineInternal (
  Mf2kNative* a_native
, const CStr& a_line)
{
  if (a_native->GetArraysInternal() &&
      a_line.Find("CONSTANT") == -1 &&
      a_line.Find("HDF5 ") == -1)
  {
    return true;
  }
  return false;
} // bool MfExportUtil::ArrayWriteNextLineInternal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iConvertToOneLayerArray (
  std::vector<std::vector<Real>>& a_array,
  std::vector<Real>& a_arrayMult)
{
  size_t nVals(0);
  for (size_t i=0; i<a_array.size(); ++i) nVals += a_array[i].size();
  std::vector<Real> vals(nVals, 0);
  int cnt(0);
  for (size_t i=0; i<a_array.size(); ++i)
  {
    for (size_t j=0; j<a_array[i].size(); ++j)
    {
      vals[cnt] = a_arrayMult[i] * a_array[i][j];
      cnt++;
    }
  }
  a_array.assign(1, vals);
  a_arrayMult.assign(1, 1);
} // iConvertToOneLayerArray
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static void iModifyArrayForDisu (MfData::MfGlobal* a_g,
  MfData::Export::Mf2kNative* a_native, MfData::MfPackage* a_p, CStr a_packName)
{
  std::map<CStr, std::vector< std::vector<Real> > >& mymap(a_native->SavedRealArrays());
  std::map<CStr, std::vector<Real> >& mymapMult(a_native->SavedRealArraysMult());
  std::map<CStr, std::vector<int> >& mymapJj(a_native->SavedRealArraysJj());

  if (mymap.find(a_packName) == mymap.end()) return;

  a_g->SetIntVar("SAVE_REAL_ARRAYS", 0);
  std::vector< std::vector<Real> > myVec2d(mymap[a_packName]);
  std::vector<Real> myMult(mymapMult[a_packName]);
  std::vector<int> myJj(mymapJj[a_packName]);

  iConvertToOneLayerArray(myVec2d, myMult);

  int IPRN = -1;
  int JJ = (int)myVec2d[0].size();
  int LAYER = 1;
  Real rArrayMult = 1;
  std::vector<Real> tmpRealArray = myVec2d[0];
  a_p->StringsToWrite().clear();
  a_p->StringDescriptions().clear();
  a_p->SetField("JJ", &JJ);
  a_p->SetField(MfData::Packages::Array::LAYER, &LAYER);
  a_p->SetField("K", &LAYER);
  a_p->SetField(MfData::Packages::Array::IPRN, &IPRN);
  a_p->SetField(MfData::Packages::Array::MULT, &rArrayMult);
  a_p->SetField(MfData::Packages::Array::ARRAY, &tmpRealArray[0]);
  a_p->SetField("ARR", &tmpRealArray[0]);
  MfData::Get().Export(a_packName);
  a_g->SetIntVar("SAVE_REAL_ARRAYS", 1);
} // iModifyArrayForDisu
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
CStr MfExportUtil::GetMf6ArrayString (MfData::MfGlobal* a_g,
  MfData::Export::Mf2kNative* a_native, CStr a_packName)
{
  CStr rval = "";
  MfData::MfPackage* p = a_g->GetPackage(a_packName);
  if (!p)
  {
    ASSERT(0);
    return rval;
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  if (lines.empty()) return rval;

  int layered(true);
  a_g->GetIntVar("ARRAYS_LAYERED", layered);
  if (!layered)
  {
    iModifyArrayForDisu(a_g, a_native, p, a_packName);
  }

  bool onlyOneLayer(false);
  if (a_packName == MfData::Packages::Disu::TOP) onlyOneLayer = true;

  CStr pad("    ");

  if (1 == lines.size())
  {
    rval = pad + lines.front();
    lines.clear();
    return rval;
  }

  CStr pad2(pad + "  ");
  for (size_t i=0; i<lines.size(); ++i)
  {
    CStr factor;
    CStr l = lines[i];
    if (l.Find("INTERNAL") != -1)
    {
      std::stringstream ss, ss1;
      ss << l;
      ss >> l >> factor;
      ss1 << pad << "INTERNAL FACTOR " << factor << "\n";
      rval += ss1.str();
      i++;
      CStr l2 = lines[i];
      l2.Replace("\n", "\n"+pad2);
      rval += pad2;
      rval += l2;
    }
    else
    {
      rval += pad;
      rval += lines[i];
    }

    if (onlyOneLayer) break;

    if (i+1 < lines.size()) rval += "\n";
  }
  lines.clear();
  return rval;
} // MfExportUtil::GetMf6ArrayString
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
CStr MfExportUtil::GetMf6CommentHeader ()
{
  CStr rval = "# Exported by MODFLOW Exporter from Aquaveo, the GMS developers.\n"
              "# www.aquaveo.com/gms\n"
              "\n";
  return rval;
} // MfExportUtil::GetMf6CommentHeader

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\MfExportUtil.t.h>

//------------------------------------------------------------------------------
void MfExportUtilT::testGetExporter_WrongString ()
{
  MfData::Export::MfExporterImpl *ex;
  ex = MfData::Export::MfExportUtil::CreateExporter("stuff");
  TS_ASSERT(ex);
  if (ex)
    delete(ex);
  // TODO
  //TS_FAIL("implement other exporters");
}

#endif
