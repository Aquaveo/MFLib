//------------------------------------------------------------------------------
// FILE      SqBcList.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Sqlite\SqBcList.h>

// 3. Standard library headers
#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/ListReader/CellIdToIJK.h>
#include <private/MfData/MfExport/private/Native/NativeExpLstPack.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/Sqlite/SqFile.h>
#include <private/MfData/MfExport/private/Sqlite/SqMfSchema.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/SQLite/CppSQLite3.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------
class SqBcList::impl
{
public:
  impl(NativeExpLstPack* a_)
    : m_pack(a_)
    , m_stmt_insert()
    , m_grid(m_pack->GetGlobal()->NumRow(), m_pack->GetGlobal()->NumCol())
  {}

  void CreateTables();
  std::string SpTableSql();
  void CreateInsertStmt();

  void AddStressPeriodData();
  int  CellId(int a_idx);

  NativeExpLstPack     *m_pack;
  CppSQLite3Statement   m_stmt_insert;
  CellIdToIJK           m_grid;
};

static const char* SQFT = "SQLITE_FILE_TIME";
typedef std::pair<std::vector<CStr>, std::vector<CStr>> pVecCStr;
typedef std::map<CStr, pVecCStr> ColMap;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static ColMap &iSpTableCols ()
{
  static ColMap m_;
  if (m_.empty())
  {
    std::vector<CStr> baseCols(3);
    baseCols[0] = "OID"; baseCols[1] = "CELLID"; baseCols[2] = "SPID";
    std::vector<CStr> baseType(3, "INTEGER");
    baseType[0] = "INTEGER PRIMARY KEY";

    { // DRN
      std::vector<CStr> drn(baseCols), drnType(baseType);
      drn.push_back("Elevation"); drnType.push_back("REAL");
      drn.push_back("Cond");      drnType.push_back("REAL");
      drn.push_back("IFACE");     drnType.push_back("INTEGER");
      drn.push_back("CONDFACT");  drnType.push_back("REAL");
      drn.push_back("CELLGRP");   drnType.push_back("INTEGER");
      drn.push_back("DRNBELEV");  drnType.push_back("REAL");
      pVecCStr pDrn(drn, drnType);
      m_.insert(std::make_pair("DRN", pDrn));
    }
    { // DRT
      std::vector<CStr> drt(baseCols), drtType(baseType);
      drt.push_back("Elevation"); drtType.push_back("REAL");
      drt.push_back("Cond");      drtType.push_back("REAL");
      drt.push_back("LayR");      drtType.push_back("INTEGER");
      drt.push_back("RowR");      drtType.push_back("INTEGER");
      drt.push_back("ColR");      drtType.push_back("INTEGER");
      drt.push_back("Rfprop");    drtType.push_back("REAL");
      drt.push_back("IFACE");     drtType.push_back("INTEGER");
      drt.push_back("CONDFACT");  drtType.push_back("REAL");
      drt.push_back("CELLGRP");   drtType.push_back("INTEGER");
      pVecCStr pDrt(drt, drtType);
      m_.insert(std::make_pair("DRT", pDrt));
    }
    { // RIV
      std::vector<CStr> riv(baseCols), rivType(baseType);
      riv.push_back("Stage");       rivType.push_back("REAL");
      riv.push_back("Cond");        rivType.push_back("REAL");
      riv.push_back("Rbot");        rivType.push_back("REAL");
      riv.push_back("IFACE");       rivType.push_back("INTEGER");
      riv.push_back("CONDFACT");    rivType.push_back("REAL");
      riv.push_back("CELLGRP");     rivType.push_back("INTEGER");
      riv.push_back("RBDTHK");      rivType.push_back("REAL");
      riv.push_back("RIVDEN");      rivType.push_back("REAL");
      pVecCStr pRiv(riv, rivType);
      m_.insert(std::make_pair("RIV", pRiv));
    }
    { // GHB
      std::vector<CStr> ghb(baseCols), ghbType(baseType);
      ghb.push_back("BHead");       ghbType.push_back("REAL");
      ghb.push_back("Cond");        ghbType.push_back("REAL");
      ghb.push_back("IFACE");       ghbType.push_back("INTEGER");
      ghb.push_back("CONDFACT");    ghbType.push_back("REAL");
      ghb.push_back("CELLGRP");     ghbType.push_back("INTEGER");
      ghb.push_back("GHBELEV");     ghbType.push_back("REAL");
      ghb.push_back("GHBDENS");     ghbType.push_back("REAL");
      pVecCStr pGhb(ghb, ghbType);
      m_.insert(std::make_pair("GHB", pGhb));
    }
    { // CHD
      std::vector<CStr> chd(baseCols), chdType(baseType);
      chd.push_back("SHead");       chdType.push_back("REAL");
      chd.push_back("EHead");       chdType.push_back("REAL");
      chd.push_back("SHEADFACT");   chdType.push_back("REAL");
      chd.push_back("EHEADFACT");   chdType.push_back("REAL");
      chd.push_back("CELLGRP");     chdType.push_back("INTEGER");
      chd.push_back("CHDDENSOPT");  chdType.push_back("REAL");
      chd.push_back("CHDDEN");      chdType.push_back("REAL");
      pVecCStr pChd(chd, chdType);
      m_.insert(std::make_pair("CHD", pChd));
    }
    { // WEL
      std::vector<CStr> wel(baseCols), welType(baseType);
      wel.push_back("Q");         welType.push_back("REAL");
      wel.push_back("IFACE");     welType.push_back("INTEGER");
      wel.push_back("QFACT");     welType.push_back("REAL");
      wel.push_back("CELLGRP");   welType.push_back("INTEGER");
      wel.push_back("WELDENS");   welType.push_back("REAL");
      pVecCStr pWel(wel, welType);
      m_.insert(std::make_pair("WEL", pWel));
    }
  }
  return m_;
} // iSpTableCols
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqBcList::SqBcList (NativeExpLstPack* a_) :
  m_pack(a_)
, m_p(new SqBcList::impl(a_))
{
  m_p->CreateTables();
} // SqBcList::SqBcList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqBcList::~SqBcList ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // SqBcList::~SqBcList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::AddVariable (
  const char* a_var
  , const char* a_val
  )
{
  std::string var = a_var;
  std::string val = a_val;
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  sqAddVariable(f, a_var, a_val);
} // SqBcList::AddVariable
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::AddItmp (
  int a_sp
  , int a_itmp
  )
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  sqAddLstItmp(f, a_sp, a_itmp);
} // SqBcList::AddItmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::AddStressPeriodData ()
{
  m_p->AddStressPeriodData();
} // SqBcList::AddStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::EndWriteFile ()
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  CStr tStr;
  m_pack->GetGlobal()->GetStrVar(SQFT, tStr);
  sqSetLastEditTime(f, tStr);
} // SqBcList::AddStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::AddSqComment ()
{
  sqAddSqliteComment(m_pack);
} // SqBcList::AddSqComment
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::impl::CreateTables ()
{
  // make get database
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  if (sqDbHasTables(f)) return;

  // see if the database has already been created
  CStr packName = m_pack->GetPackage()->PackageName();

  std::string str(SpTableSql());
  std::vector<std::string> sql;
  if (!str.empty()) sql.push_back(str);
  sqCreatePackageTables(f, packName, sql);
  sqStoreLastEditTime(f, m_pack);
} // SqBcList::impl::CreateTables
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::string SqBcList::impl::SpTableSql ()
{
  std::string str;
  CStr packName = m_pack->GetPackage()->PackageName();
  ColMap &colMap(iSpTableCols());
  if (colMap.find(packName) == colMap.end()) return str;
  std::vector<CStr> &colStrs = colMap[packName].first;
  std::vector<CStr> &colTypes = colMap[packName].second;
  // find any aux variables that we don't create by default
  std::set<CStr> strs;
  strs.insert("K");
  strs.insert("I");
  strs.insert("J");
  for (size_t i=3; i<colStrs.size(); ++i) // skip OID, CELLID, SPID
  {
    CStr tmp = colStrs[i];
    tmp.ToUpper();
    strs.insert(tmp);
  }
  std::vector<CStr> fieldStr = m_pack->m_fieldStrings;
  std::vector<CStr> aux;
  for (size_t i=0; i<fieldStr.size(); ++i)
  {
    CStr tmp = fieldStr[i];
    tmp.ToUpper();
    if (strs.find(tmp) == strs.end())
    {
      colStrs.push_back(fieldStr[i]);
      colTypes.push_back("REAL");
    }
  }

  std::stringstream ss;
  ss << "CREATE TABLE LST_STRESS_PERIODS ("
     << colStrs.front() << " " << colTypes.front();
  for (size_t i=1; i<colStrs.size(); ++i)
    ss << ", " << colStrs[i] << " " << colTypes[i];
  ss << ")";
  return ss.str();
} // SqBcList::impl::SpTableSql
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::impl::CreateInsertStmt ()
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  std::stringstream ss;
  ColMap &colMap(iSpTableCols());
  CStr tabName = m_pack->GetPackage()->PackageName();
  std::vector<CStr> &colStrs = colMap[tabName].first;
  tabName = "LST_STRESS_PERIODS";
  ss << "INSERT INTO " << tabName << " VALUES(?";
  for (size_t i=1; i<colStrs.size(); ++i) ss << ", ?";
  ss << ");";
  m_stmt_insert = f->compileStatement(ss.str().c_str());
} // SqBcList::impl::CreateInsertStmt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::impl::AddStressPeriodData ()
{
  ColMap &colMap(iSpTableCols());
  CStr packName = m_pack->GetPackage()->PackageName();
  if (colMap.find(packName) == colMap.end()) return;

  const int *itmp(0);
  {
    using namespace MfData::Packages::ListPack;
    if (!m_pack->GetPackage()->GetField(ITMP, &itmp) || !itmp) return;
  }
  CreateInsertStmt();
  // get the stress period
  int sp = m_pack->GetGlobal()->GetCurrentPeriod();
  std::vector<CStr> &colStrs = colMap[packName].first;
  std::map<CStr, int> colIdx, dataIdx;
  for (size_t i=0; i<colStrs.size(); ++i) colIdx[colStrs[i]] = (int)i+1;
  for (size_t i=0; i<colStrs.size(); ++i)
  {
    for (size_t j=0; j<m_pack->m_fieldStrings.size(); ++j)
    {
      if (m_pack->m_fieldStrings[j] == colStrs[i])
        dataIdx[colStrs[i]] = (int)j;
    }
  }

  std::vector<Real> vals(colIdx.size()+1, 0.0);
  vals[colIdx["SPID"]] = (Real)sp;
  auto itEnd = colIdx.end();
  if (colIdx.find("IFACE") != itEnd)    vals[colIdx["IFACE"]] = 1;
  if (colIdx.find("CELLGRP") != itEnd)  vals[colIdx["CELLGRP"]] = -1;
  if (colIdx.find("CONDFACT") != itEnd) vals[colIdx["CONDFACT"]] = 1.0;
  if (colIdx.find("QFACT") != itEnd)    vals[colIdx["QFACT"]] = 1.0;

  colIdx.erase("OID");
  int nDataFields(*m_pack->m_nDataFields);
  const Real *data(m_pack->m_data);
  auto itEnd2 = dataIdx.end();
  for (int i=0; i<*itmp; ++i)
  {
    m_stmt_insert.bind(colIdx["SPID"], sp);
    vals[colIdx["CELLID"]] = (Real)CellId(i);
    auto it = colIdx.begin();
    for (; it != itEnd; ++it)
    {
      auto it2 = dataIdx.find(it->first);
      if (it2 != itEnd2)
      {
        vals[it->second] = data[i*nDataFields+it2->second];
      }
      m_stmt_insert.bind(it->second, vals[it->second]);
    }
    m_stmt_insert.execQuery();
    m_stmt_insert.reset();
  }
} // SqBcList::impl::AddStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int SqBcList::impl::CellId (int a_idx)
{
  int i(a_idx);
  int nDataFields(*m_pack->m_nDataFields), cellIdOffset(0);
  const Real *data(m_pack->m_data);
  int ck, ci, cj, cellId(-1);
  ck = static_cast<int>(data[i*(nDataFields)+0]);
  ci = static_cast<int>(data[i*(nDataFields)+1]);
  cj = static_cast<int>(data[i*(nDataFields)+2]);
  if (m_pack->m_usg) cellId = (int)data[i*nDataFields+0] - cellIdOffset;
  else cellId = m_grid.IdFromIJK(ci, cj, ck);
  return cellId;
} // SqBcList::impl::CellId
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::WriteMapIdsForListBcs ()
{
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //h.WriteMapIds();
} // SqBcList::WriteMapIdsForListBcs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqBcList::LstPar ()
{
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //h.LstPar();
} // SqBcList::LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::Str (
  int& //a_itmp
  )
{
  CStr rval;
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //rval = h.StrPack(a_itmp);
  return rval;
} // SqBcList::Str
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::SfrLn2 ()
{
  CStr rval;
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //rval = h.SfrLn2();
  return rval;
} // SqBcList::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::SfrLn6 (
  int& //a_itmp
  )
{
  CStr rval;
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //rval = h.SfrLn6(a_itmp);
  return rval;
} // SqBcList::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::Mnw2 ()
{
  //using namespace MfData::Packages;
  //using util::ForElement;

  CStr rval;
  //int a_sp = m_pack->GetGlobal()->GetCurrentPeriod();
  //const int* ITMP(0),* NMNWVL(0),* MNWMAX(0),* NAUX(0);
  //const double* MNW2d(0);
  //MfPackage* a_p=m_pack->GetPackage();
  //if (!a_p->GetField(MNW2pack::ITMP, &ITMP) || !ITMP ||
  //    !a_p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
  //    !a_p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
  //    !a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
  //    !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  //{
  //  ASSERT(0);
  //  return rval;
  //}

  //CStr f(m_pack->GetNative()->GetExp()->GetBaseFileName()), path;
  //f += ".h5";
  //{// write the use last flag
  //  path.Format("%s/%s", "MNW2", MFBC_USELAST);
  //  H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
  //  std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
  //  H5DSWriterDimInfo dim(start, n2write);
  //  H5DataSetWriter w(&s);
  //  w.SetDimInfoForWriting(&dim);
  //  int tmpItmp(*ITMP < 0 ? 1 : 0);
  //  w.WriteData(&tmpItmp, 1);
  //}
  //CAR_DBL2D bcData;
  //bcData.SetSize(13, *MNWMAX, 0);
  //if (*ITMP > 0)
  //{ // get the data from the MNW2 array
  //  for (int i=0; i<*MNWMAX; i++)
  //  {
  //    bcData.at(0,i) = ForElement(MNW2d,  1, i+1, *NMNWVL); // active
  //    bcData.at(1,i) = ForElement(MNW2d,  5, i+1, *NMNWVL); // Qdes
  //    bcData.at(2,i) = ForElement(MNW2d, 24, i+1, *NMNWVL); // CapMult
  //    bcData.at(3,i) = ForElement(MNW2d, 12, i+1, *NMNWVL); // Cprime
  //    bcData.at(4,i) = ForElement(MNW2d,  7, i+1, *NMNWVL); // Hlim
  //    bcData.at(5,i) = ForElement(MNW2d,  8, i+1, *NMNWVL); // QCUT
  //    bcData.at(6,i) = ForElement(MNW2d,  9, i+1, *NMNWVL); // Qfrcmn
  //    bcData.at(7,i) = ForElement(MNW2d, 10, i+1, *NMNWVL); // Qfrcmx
  //    for (int j=0; j<*NAUX; j++) // AUX
  //    {
  //      bcData.at(8+j,i) = ForElement(MNW2d, 31+j, i+1, *NMNWVL);
  //    }
  //  }
  //}
  //else if (*ITMP < 0 && a_sp > 1)
  //{ // get the previous stress period
  //  path.Format("%s/%s", "MNW2", MFBC_DATA);
  //  std::pair<int, int> p(0,1);
  //  VEC_INT_PAIR indices(3,p);
  //  indices[0].second = bcData.GetSize1();
  //  indices[1].second = bcData.GetSize2();
  //  indices[2].first = a_sp-2;
  //  H5DataSetReader r(f, path, indices);
  //  r.GetData(&bcData.at(0,0),
  //    static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
  //}

  //std::vector<int> cellids(*MNWMAX, 0), iface(*MNWMAX, 0);
  //for (size_t q=0; q<cellids.size(); q++)
  //{
  //  cellids[q] = static_cast<int>(q+1);
  //}
  //// write the data for the stress period
  //int sp = m_pack->GetGlobal()->GetCurrentPeriod();
  //CStr file1, line, mStr = "MNW2";
  //WriteList(sp, 0, mStr, f, cellids, bcData, iface);
  //util::StripPathFromFilename(f, file1);
  //line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, "MNW2", sp);
  //if (*ITMP > 0)
  //{
  //  rval = line;
  //}
  return rval;
} // SqBcList::Mnw2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::Mnw1 ()
{
  CStr rval;

  //using namespace MfData::Packages;
  //using util::ForElement;

  //int modIdx = (int)m_pack->GetGlobal()->CurModIdx();
  //const int *itmp(0), *nwell2(0);
  //const double *well2(0), *mnwflgs(0);
  //const char *mnwsite(0);
  //const int WELL2_SIZE = 18;
  //MfPackage* a_p = m_pack->GetPackage();
  //int sp = m_pack->GetGlobal()->GetCurrentPeriod();
  //if (a_p->GetField(MNWpack::ITMP, &itmp) && itmp &&
  //    a_p->GetField(MNWpack::NWELL2, &nwell2) && nwell2 &&
  //    a_p->GetField(MNWpack::WELL2, &well2) && well2 &&
  //    a_p->GetField(MNWpack::MNWSITE, &mnwsite) && mnwsite &&
  //    a_p->GetField(MNWpack::MNWFLGS, &mnwflgs) && mnwflgs)
  //{
  //  CStr line, f(m_pack->GetNative()->GetExp()->GetBaseFileName()), fileName;
  //  const char *type = "Multi-Node Well";
  //  CStr path;
  //  f += ".h5";
  //  util::StripPathFromFilename(f, fileName);

  //  // line 4. ITMP (ADD)
  //  // not handling ADD so use nwell2 if itmp > 0
  //  int useLast(*itmp);
  //  if (useLast > 0)
  //    useLast = *nwell2;

  //  // line 5. as HDF5
  //  if (*itmp > 0)
  //  {
  //    rval.Format("GMS_HDF5_MNW \"%s\" \"Multi-Node Well\" %d", fileName, sp);
  //  }

  //  // update use last
  //  path.Format("%s/%s", type, MFBC_USELAST);
  //  Write1DH5Value(f, path, sp-1, useLast < 0 ? 1 : 0);

  //  Mnw1PropList& wpl = iGet_Mnw1PropList(modIdx);
  //  wpl.NewStressPeriod();
  //  std::vector<int> properties;

  //  if (*nwell2 != 0)
  //  { // get a list of indicees into the properties array for each line 5
  //    // row from well2 and append to properties array
  //    std::vector<int> wellCellIds;
  //    std::vector<int> wellProperties;
  //    int currWellNum = static_cast<int>(ForElement(well2, 18, 1, 18));
  //    int firstWell2Index = 1;
  //    for (int i = 1; i <= *nwell2; ++i)
  //    {
  //      int cell = static_cast<int>(ForElement(well2, 1, i, 18));
  //      int wellNum = static_cast<int>(ForElement(well2, 18, i, 18));
  //      if (wellNum != currWellNum)
  //      { // write properties for well
  //        CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
  //        siteName.Trim();
  //        if (siteName == "NO-PRINT")
  //          siteName = "";
  //        wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
  //        properties.insert(properties.end(), wellProperties.begin(),
  //                          wellProperties.end());

  //        wellCellIds.clear();
  //        wellProperties.clear();
  //        currWellNum = wellNum;
  //        firstWell2Index = i;
  //      }
  //      wellCellIds.push_back(cell);
  //    }

  //    // get properties for last well
  //    CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
  //    siteName.Trim();
  //    if (siteName == "NO-PRINT")
  //      siteName = "";
  //    wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
  //    properties.insert(properties.end(), wellProperties.begin(),
  //                      wellProperties.end());
  //  }

  //  if (wpl.GetCellIds().size() != 0)
  //  {
  //    // save mnwflgs in case using last next stress period
  //    std::vector<double>& mnwFlgsCopy = iGet_Mnw1Flags(modIdx);
  //    if (sp == 1 || *itmp > 0)
  //    {
  //      mnwFlgsCopy.clear();
  //      mnwFlgsCopy.insert(mnwFlgsCopy.begin(), mnwflgs, 
  //                         mnwflgs+mnw::MNWFLGS_SIZE*(*nwell2));
  //    }

  //    // set property values
  //    CAR_DBL2D v;
  //    v.SetSize(mnw::H5_SIZE, (int)wpl.GetCellIds().size(), 0.0);
  //    for (int i = 1; i <= *nwell2; ++i)
  //    {
  //      int j = properties.at(i-1);
  //      double *flgs = &mnwFlgsCopy.at(0);
  //      v.at(mnw::H5_ACTIVE, j) = mnw::ACTIVE;
  //      double q = ForElement(flgs, mnw::MNWFLGS_QDES, i, mnw::MNWFLGS_SIZE);
  //      v.at(mnw::H5_QDES, j)   = q;
  //      v.at(mnw::H5_WELLID, j) = wpl.GetWellIds().at(j);
  //      v.at(mnw::H5_QWVAL, j)  = ForElement(well2, 4, i, WELL2_SIZE);
  //      v.at(mnw::H5_RW, j)     = ForElement(well2, 5, i, WELL2_SIZE);
  //      v.at(mnw::H5_SKIN, j)   = ForElement(well2, 6, i, WELL2_SIZE);
  //      if (ForElement(flgs, mnw::MNWFLGS_DD, i, mnw::MNWFLGS_SIZE) ==
  //          mnw::DD_NONE)
  //      {
  //        v.at(mnw::H5_HLIM, j) = 0.0;
  //        v.at(mnw::H5_HREF, j) = 0.0;
  //      }
  //      else
  //      {
  //        v.at(mnw::H5_HLIM, j)   = ForElement(flgs, mnw::MNWFLGS_HLIM, i, 
  //                                             mnw::MNWFLGS_SIZE);
  //        v.at(mnw::H5_HREF, j)   = ForElement(flgs, mnw::MNWFLGS_HREF, i,
  //                                             mnw::MNWFLGS_SIZE);
  //      }
  //      v.at(mnw::H5_DD, j)     = ForElement(flgs, mnw::MNWFLGS_DD, i,
  //                                           mnw::MNWFLGS_SIZE);
  //      if (util::lrint(ForElement(flgs, mnw::MNWFLGS_IERR, i,
  //                                 mnw::MNWFLGS_SIZE)) >= 1)
  //        v.at(mnw::H5_IWGRP, j) = -1;
  //      else
  //        v.at(mnw::H5_IWGRP, j) = ForElement(well2, 9, i, WELL2_SIZE);
  //      v.at(mnw::H5_C, j)      = ForElement(well2, 16, i, WELL2_SIZE);
  //      int qcut = util::lrint(ForElement(flgs, mnw::MNWFLGS_QCUT, i,
  //                                        mnw::MNWFLGS_SIZE));
  //      v.at(mnw::H5_QCUT, j)   = qcut;
  //      //if (v.at(mnw::H5_QDES, j) == 0)
  //      //{ // if the Q is zero then the MNW packages doesn't read these values
  //      //  // the values may have been set if the user specifies "DEFAULT" but
  //      //  // the package code ignores the values
  //      //  v.at(mnw::H5_QFRCMN, j) = 0;
  //      //  v.at(mnw::H5_QFRCMX, j) = 0;
  //      //}
  //      //else if (qcut == 1)
  //      if (qcut == 1)
  //      {
  //        v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*q;
  //        v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*q;
  //      }
  //      else if (qcut == 2)
  //      {
  //        v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*100;
  //        v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*100;
  //      }
  //      v.at(mnw::H5_SITE, j)   = wpl.GetIsSiteName().at(j) ? mnw::SITE_PRINT : 
  //                                                        mnw::SITE_DONT_PRINT;
  //    }

  //    // write properties
  //    {
  //      path.Format("%s/%s", type, MFBC_DATA);
  //      H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
  //      std::vector<hsize_t> start(3, 0), n2write(3,1);
  //      n2write[0] = v.GetSize1();
  //      n2write[1] = v.GetSize2();
  //      start[2] = sp - 1;
  //      H5DSWriterDimInfo dim(start, n2write);
  //      H5DataSetWriter w(&s);
  //      w.SetDimInfoForWriting(&dim);
  //      w.WriteData(&v.at(0,0),
  //                  static_cast<size_t>(v.GetSize1()*v.GetSize2()));
  //    }

  //    // update number of boundary conditions
  //    path.Format("%s/%s", type, MFBC_NUMBC);
  //    WriteSingleH5IntValue(f, path, (int)wpl.GetCellIds().size());

  //    // update cellids
  //    path.Format("%s/%s", type, MFBC_CELLIDS);
  //    Write1DIntArray(f, path, &wpl.GetCellIds()[0], wpl.GetCellIds().size());

  //    // update names
  //    path.Format("%s/%s", type, MFBC_NAME);
  //    WriteH5StringArray(f, path, wpl.GetWellNames());

  //    // update iface
  //    std::vector<int> iface(wpl.GetCellIds().size(), 0);
  //    path.Format("%s/%s", type, MFBC_IFACE);
  //    Write1DIntArray(f, path, &iface[0], iface.size());

  //    // update mapids
  //    std::vector<CStr> mapids(wpl.GetCellIds().size(), CStr());
  //    path.Format("%s/%s", type, MFBC_MAPIDSTR);
  //    WriteH5StringArray(f, path, mapids);
  //  }
  //}
  return rval;
} // SqBcList::Mnw1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr SqBcList::ClnWel (
  int& //a_itmp
  )
{
  CStr rval;
  //H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
  //            m_pack->GetNative(), this);
  //rval = h.ClnWel(a_itmp);
  return rval;
} // SqBcList::ClnWel


