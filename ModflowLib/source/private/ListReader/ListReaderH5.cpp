//------------------------------------------------------------------------------
// FILE      ListReaderH5.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\ListReader\ListReaderH5.h>

#include <map>
#include <private\ListReader.h>

#include <Export.h>
#include <private\H5DataReader\H5DataSetReader.h>
#include <private\H5DataReader\H5VecCStrReader.h>
#include <private\ListReader\CellIdToIJK.h>
#include <private\ListReader\ListReaderParser.h>
#include <private\MfData\MfGlobal.h>
#include <private\Parameters.h>

#define CELLIDS   "02. Cell IDs"
#define MAPIDSTR  "04. Map ID"
#define FACTOR    "05. Factor"
#define IFACE     "06. IFACE"
#define PROPERTY  "07. Property"

std::map<CStr, Real>& listReader_GetMapId ()
{
  static std::map<CStr, Real> m_map; // ok to leave static
  return m_map;
} // listReader_GetMapId
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ListReaderH5::ListReaderH5 (const ListReaderParser &a_parser,
                            const ListReaderSetUp &a_setup) :
m_file(a_parser.GetFileName()),
m_path(a_parser.GetPath()),
m_stress(a_parser.GetStressPeriod()),
m_nRows(a_setup.m_nRows),
m_nFields(a_setup.m_nFields),
m_nAuxFields(a_setup.m_nAuxFields),
m_ial(a_setup.m_ial),
m_nGridRows(a_setup.m_nGridRows),
m_nGridCols(a_setup.m_nGridCols),
m_fileVersion(1.0),
m_auxNames(a_setup.m_auxNames),
m_nCbcFields(0)
{
  int modType = MfData::Get().ModelType();
  if (modType != MfData::MF2K && modType != MfData::SEAWAT &&
      m_path.Find("Specified Head") == -1)
  {
    m_nCbcFields = 1;
    if (m_path.find("Drain Return") != -1) m_nCbcFields = 2;
  }
} // ListReaderH5::ListReaderH5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool ListReaderH5::FillInData (double *a_)
{
  return (FillInDataT(a_));
} // ListReaderH5::FillInData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool ListReaderH5::FillInData (float *a_)
{
  return (FillInDataT(a_));
} // ListReaderH5::FillInData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::FillInDataT (T *a_)
{
  listReader_GetMapId().clear();
  // we only have to do this the first time
  GetVersion();
  // need to read this in case they have 0 in the first stress period
  //if (m_stress == 1)
  //{
    for (int i=0; i<m_nRows*m_nFields; i++)
      a_[i] = 0.0;
    // fill in cell k i j
    if (!GetCellKIJ(a_))
      return false;
    // fill in the auxilary values like IFACE and 
    // cellgrp (for post processing CCF)
    if (m_nAuxFields > 0)
    {
      if (GetAuxIdx("IFACE") > -1)
      {
        if (!GetIface(a_))
          return false;
      }
      char* factStr[4] = { "CONDFACT", "QFACT", "SHEADFACT", "EHEADFACT"};
      int   factFlg[4] = {          0,       0,           1,           2};
      for (int i = 0; i < 4; ++i)
      {
        int index = GetAuxIdx(factStr[i]);
        if (index > -1)
        {
          if (!GetFactor(a_, index, factFlg[i]))
            return false;
        }
      }
      if (GetAuxIdx("CELLGRP") > -1)
      {
        if (!GetCellGroup(a_))
          return false;
      }
      GetSeawatAux(a_);
      GetUsgTransportAux(a_);
    }
    ASSERT(_CrtCheckMemory());
  //}

  // fill in the boundary condition values like head, conductance, elevation
  return (GetStressData(a_));
} // ListReaderH5::FillInData
//------------------------------------------------------------------------------
/// \brief Fills in the stress period data into the passed in array
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetStressData (T *a_) const
{
  // set up the indices to read the portion of the boundary conditions that
  // we are going to read
  int nVals = m_nFields - m_nAuxFields - 3 - m_ial;
  std::pair<int, int> myPair(0, nVals);
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = m_nRows;
  indices.at(2).first = m_stress - 1;
  indices.at(2).second = 1;

  CStr str(m_path), parType;

  str += "/";
  str += PROPERTY;
  H5DataSetReader r(m_file, str, indices);
  r.AllowTypeConversions(true);
  std::vector<Real> d, fact;
  r.GetData(d);
  GetFactor(fact, 0);

  Parameters::ParTypeFromH5Path(m_path, parType);
  if (Parameters::CheckListSubstituteOk())
  {
    Parameters::SubstituteList(d, fact, parType);
  }
  else if (parType == "STR")
  {
    mfLibExp_StrCondFact(&fact[0], (int)fact.size());
  }
  try
  {
    for (int i=0; i<m_nRows; i++)
    {
      for (int j=0; j<nVals; j++)
      {
        a_[i*m_nFields+3+j] = (T)(d.at(j*m_nRows+i));
      }
      //ASSERT(_CrtCheckMemory());
    }
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // ListReaderH5::GetStressData
//------------------------------------------------------------------------------
/// \brief Fills in the IJK into the passed in array
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetCellKIJ (T *a_) const
{
  std::pair<int, int> myPair(0, m_nRows);
  VEC_INT_PAIR indices(1, myPair);

  CStr str(m_path);
  str += "/";
  str += CELLIDS;

  H5DataSetReader r(m_file, str, indices);
  CellIdToIJK g(m_nGridRows, m_nGridCols);

  std::vector<int> cellIds(m_nRows, 0);
  if (!r.GetData(&cellIds[0], cellIds.size()))
    return false;

  for (size_t i=0; i<cellIds.size(); i++)
  {
    a_[i*m_nFields] = (T)g.KFromId(cellIds.at(i));
    a_[i*m_nFields+1] = (T)g.IFromId(cellIds.at(i));
    a_[i*m_nFields+2] = (T)g.JFromId(cellIds.at(i));
  }

  return true;
} // ListReaderH5::GetCellKIJ
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetIface (T *a_) const
{
  std::pair<int, int> myPair(0, m_nRows);
  VEC_INT_PAIR indices(1, myPair);

  CStr str(m_path);
  str += "/";
  str += IFACE;

  int auxIdx(GetAuxIdx("iface"));
  if (auxIdx < 0)
  {
    auxIdx = 0;
  }
  int index;
  index = m_nFields - m_nAuxFields + auxIdx - m_nCbcFields;

  H5DataSetReader r(m_file, str, indices);
  std::vector<int> iface(m_nRows, 0);
  r.GetData(&iface[0], iface.size());
  for (size_t i=0; i<iface.size(); i++)
  {
    a_[i*m_nFields+index] = (T)iface.at(i);
  }
  return true;
} // ListReaderH5::GetIface
//------------------------------------------------------------------------------
/// \brief Gets the factor (a multiplier) associated with this BC.
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetFactor (T *a_,
                              const int a_auxIdx,
                              const int a_sheadFact) const
{
  std::vector<Real> fact;
  if (GetFactor(fact, a_sheadFact))
  {
    int index;
    index = m_nFields - m_nAuxFields + a_auxIdx - m_nCbcFields;
    for (size_t i=0; i<fact.size(); i++)
    {
      a_[i*m_nFields+index] = (T)fact.at(i);
    }
  }
  else
    return false;
  return true;
} // ListReaderH5::GetFactor
//------------------------------------------------------------------------------
/// \brief Gets the factor (a multiplier) associated with this BC.
//------------------------------------------------------------------------------
bool ListReaderH5::GetFactor (std::vector<Real> &a_,
                              const int a_sheadFact) const
{
  if (m_fileVersion == 1.0)
    return(GetFactorVer1(a_));

  // factors are now stored with the bcs per stress period

  // set up the indices to read the portion of the boundary conditions that
  // we are going to read

  //m_nFields    - size of array from MODFLOW
  //m_nAuxFields - number of AUX variables
  //3            - k, i, j
  //m_ial        - variable from MODFLOW
  int nVals = m_nFields - m_nAuxFields - 3 - m_ial;
  std::pair<int, int> myPair(0, 1);
  VEC_INT_PAIR indices(3, myPair);
  indices.at(0).first = nVals;
  indices.at(0).second = GetNumFactors();
  indices.at(1).second = m_nRows;
  indices.at(2).first = m_stress - 1;
  indices.at(2).second = 1;

  if (1 == a_sheadFact)
  { // just reading SHEADFACT
    indices.at(0).second = 1;
  }
  else if (2 == a_sheadFact)
  { // just reading EHEADFACT
    indices.at(0).first++;
    indices.at(0).second = 1;
  }

  CStr str(m_path);

  str += "/";
  str += PROPERTY;
  H5DataSetReader r(m_file, str, indices);
  r.AllowTypeConversions(true);
  return (r.GetData(a_));
} // ListReaderH5::GetFactor
//------------------------------------------------------------------------------
/// \brief Gets the factor (a multiplier) associated with this BC.
//------------------------------------------------------------------------------
bool ListReaderH5::GetFactorVer1 (std::vector<Real> &a_) const
{
  std::pair<int, int> myPair(0, m_nRows);
  VEC_INT_PAIR indices(1, myPair);

  CStr str(m_path);
  str += "/";
  str += FACTOR;

  H5DataSetReader r(m_file, str, indices);
  r.AllowTypeConversions(true);
  return (r.GetData(a_));
} // ListReaderH5::GetFactorVer1
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetCellGroup (T *a_) const
{
  CStr str(m_path);
  str += "/";
  str += MAPIDSTR;
  H5VecCStrReader r(m_file, str);

  std::vector<CStr> vStr;
  r.FillInStrings(vStr);
  if (vStr.empty())
    return false;
  vStr.resize(m_nRows);

  Real f;
  int auxIdx(GetAuxIdx("cellgrp"));
  if (auxIdx < 0)
  {
    auxIdx = 0;
  }
  int   index;
  index = m_nFields - m_nAuxFields + auxIdx - m_nCbcFields;

  std::map<CStr, Real> myMap;
  std::map<CStr, Real>::iterator it;
  for (size_t i=0; i<vStr.size(); i++)
  {
    if (vStr.at(i).IsEmpty())
      f = -1;
    else
    {
      it = myMap.find(vStr.at(i));
      if (it == myMap.end())
      {
        f = (Real)(myMap.size() + 1);
        it = myMap.insert(myMap.begin(), std::make_pair(vStr.at(i),f));
      }
      f = it->second;
    }

    a_[i*m_nFields+index] = (T)f;
  }

  listReader_GetMapId() = myMap;
  return true;
} // ListReaderH5::GetCellGroup
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::ReadAux (const std::vector<std::pair<int, int> >& a_vIdx, T* a_) const
{
  // set up the indices to read the portion of the boundary conditions that
  // we are going to read
  for (size_t i=0; i<a_vIdx.size(); i++)
  {
    std::pair<int, int> myPair(0, 1);
    VEC_INT_PAIR indices(3, myPair);
    indices.at(0).first = a_vIdx.at(i).first;
    indices.at(0).second = 1;
    indices.at(1).second = m_nRows;
    indices.at(2).first = m_stress - 1;
    indices.at(2).second = 1;

    CStr str(m_path);

    str += "/";
    str += PROPERTY;
    H5DataSetReader r(m_file, str, indices);
    r.AllowTypeConversions(true);
    std::vector<Real> vDat;
    if (!r.GetData(vDat))
    {
      ASSERT(0);
      return false;
    }

    int index = m_nFields - m_nAuxFields + a_vIdx.at(i).second;
    for (size_t j=0; j<vDat.size(); j++)
    {
      a_[j*m_nFields+index] = (T)vDat.at(j);
    }
  }
  return true;
} // ListReaderH5::ReadAux
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetSeawatAux (T* a_) const
{
  // the file version must be 3 or greater
  if (m_fileVersion < 3)
    return false;
  // look at the aux names
  // RBDTHK=4, RIVDEN=5
  // DRNBELEV=3
  // GHBELEV=3, GHBDENS=4
  // CHDDENSOPT=4, CHDDEN=5
  // WELDENS=2

  std::vector<std::pair<int, int> > vIdx; // first: GMS H5 index, second: MODFLOW index
  GetH5IndicesForSeawatAux(m_auxNames, vIdx);
  if (vIdx.empty() || vIdx.size() > 2) // no seawat data to read or error
  {
    if (!vIdx.empty())
    {
      ASSERT(0);
    }
    return false;
  }

  return ReadAux(vIdx, a_);
} // ListReaderH5::GetSeawatAux
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
template <class T>
bool ListReaderH5::GetUsgTransportAux (T* a_) const
{
  // the file version must be 4 or greater
  if (m_fileVersion < 4)
    return false;

  std::vector<std::pair<int, int> > vIdx; // first: GMS H5 index, second: MODFLOW index
  GetH5IndicesForUsgTransportAux(m_auxNames, vIdx);
  if (vIdx.empty() || vIdx.size() > 5) // no aux data to read or error
  {
    if (!vIdx.empty())
    {
      ASSERT(0);
    }
    return false;
  }

  return ReadAux(vIdx, a_);
} // ListReaderH5::GetUsgTransportAux
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool ListReaderH5::GetVersion ()
{
  // get the file version
  std::pair<int, int> myPair(0, 1);
  VEC_INT_PAIR indices(1, myPair);
  H5DataSetReader r(m_file, "MFH5 Version", indices);
  if (r.DataSetExists())
    return(r.GetData(&m_fileVersion, 1));
  else
    return false;
} // ListReaderH5::GetVersion
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ListReaderH5::GetNumFactors () const
{
  CStr parType;
  Parameters::ParTypeFromH5Path(m_path, parType);

  if (parType == "CHD")
    return 2;
  else
    return 1;
} // ListReaderH5::GetNumFactors
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ListReaderH5::GetAuxIdx (const char *a_name) const
{
  for (int i=0; i<(int)m_auxNames.size(); i++)
  {
    if (m_auxNames.at(i).CompareNoCase(a_name) == 0)
      return i;
  }
  return -1;
} // ListReaderH5::GetAuxIdx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static std::map<CStr, int> iSeawatH5Idx()
{
  // look at the aux names
  // see enums in gms/source/modflow/bc/mfbc.h
  // RBDTHK=4, RIVDEN=5
  // DRNBELEV=3
  // GHBELEV=3, GHBDENS=4
  // CHDDENSOPT=4, CHDDEN=5
  // WELDENS=2
  std::map<CStr, int> m_map;
  m_map["rbdthk"] = 4;
  m_map["rivden"] = 5;
  m_map["drnbelev"] = 3;
  m_map["ghbelev"] = 3;
  m_map["ghbdens"] = 4;
  m_map["chddensopt"] = 4;
  m_map["chdden"] = 5;
  m_map["weldens"] = 2;
  return m_map;
} // iSeawatH5Idx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static std::set<CStr> iGmsAux()
{
  // look at the aux names
  // see enums in gms/source/modflow/bc/mfbc.h
  // RBDTHK=4, RIVDEN=5
  // DRNBELEV=3
  // GHBELEV=3, GHBDENS=4
  // CHDDENSOPT=4, CHDDEN=5
  // WELDENS=2
  static std::set<CStr> m_setGmsAux;
  m_setGmsAux.insert("rbdthk");
  m_setGmsAux.insert("rivden");
  m_setGmsAux.insert("drnbelev");
  m_setGmsAux.insert("ghbelev");
  m_setGmsAux.insert("ghbdens");
  m_setGmsAux.insert("chddensopt");
  m_setGmsAux.insert("chdden");
  m_setGmsAux.insert("weldens");
  m_setGmsAux.insert("iface");
  m_setGmsAux.insert("condfact");
  m_setGmsAux.insert("qfact");
  m_setGmsAux.insert("sheadfact");
  m_setGmsAux.insert("eheadfact");
  m_setGmsAux.insert("cellgrp");
  return m_setGmsAux;
} // iGmsAux
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int ListReaderH5::GetSeawatAuxH5Idx (const char *a_name) const
{
  CStr name(a_name);
  name.MakeLower();
  std::map<CStr, int> aMap(iSeawatH5Idx());
  if (aMap.find(name) != aMap.end())
  {
    return aMap[name];
  }
  return -1;
} // ListReaderH5::GetSeawatAuxH5Idx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void ListReaderH5::GetH5IndicesForSeawatAux (const std::vector<CStr>& a_auxNames,
                                             std::vector<std::pair<int, int> >& a_indices) const
{
  size_t i;
  int idx;
  for (i=0; i<a_auxNames.size(); i++)
  {
    idx = GetSeawatAuxH5Idx(a_auxNames.at(i));
    if (idx > 0)
      a_indices.push_back(std::make_pair(idx, (int)i));
  }
} // ListReaderH5::GetH5IndicesForSeawatAux
//------------------------------------------------------------------------------
/// \brief
/// \param[out] a_indices: Vector of pairs where first = GMS H5 index,
///                        second = MODFLOW index.
//------------------------------------------------------------------------------
void ListReaderH5::GetH5IndicesForUsgTransportAux (const std::vector<CStr>& a_auxNames,
                                             std::vector<std::pair<int, int> >& a_indices) const
{
  int idx = 0;
  std::set<CStr> setGmsAux(iGmsAux());
  const int MAX_USER_AUX_COUNT = 5; ///< Max number of user AUX fields (that GMS supports)
  for (size_t i=0; i<a_auxNames.size(); i++)
  {
    CStr name(a_auxNames[i]);
    name.MakeLower();
    if (setGmsAux.find(name) != setGmsAux.end())
    {
      // This is a GMS hardwired aux variable, not a user aux.
    }
    else
    {
      // This must be a user aux
      int h5Idx = m_nFields - MAX_USER_AUX_COUNT + idx;
      a_indices.push_back(std::make_pair(h5Idx, (int)i));
      ++idx;
    }
  }
} // ListReaderH5::GetH5IndicesForUsgTransportAux

