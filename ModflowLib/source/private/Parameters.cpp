//------------------------------------------------------------------------------
// FILE      Parameters.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\Parameters.h>

#include <hash_map>
#include <map>
#include <math.h>
#include <set>
#include <sstream>

#include <Export.h>
#include <private\H5DataReader\H5DataReaderUtil.h>
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\H5DataReader\H5DataSetWriterSetup.h>
#include <private\MfData\MfExport\private\ExpGmsH5.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\Parameters\MultArray.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\Parameters\ParamFileReader.h>
#include <private\Parameters\PilotPoints.h>
#include <private\Parameters\SenFileReader.h>
#include <private\util\EReadAsciiFile.h>

static ParamList& GetList(void);
static bool ParametersExist(void);

struct pData
{
public:
  pData() : m_val(0), m_arrVals() {}
  Real m_val;
  std::vector<Real> m_arrVals;
};
class ParPub
{
public:
  ParPub() {}
  ~ParPub() {}
  std::map<CStr, std::vector<Real> > m_arraysToWrite;
  CStr                               m_fname;
  std::map<CStr, std::set<int> >     m_ArrayNameKeyMap;
  stdext::hash_map<int, pData>       m_parData;
};

ParPub* New_ParPub ()
{
  ParPub* p = new ParPub;
  return p;
} // New_ParPub
void Delete_ParPub (ParPub* a_)
{
  if (a_) delete(a_);
} // Delete_ParPub
//------------------------------------------------------------------------------
/// \brief file global map to hold arrays
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<Real> >& iArraysToWrite()
{
  return GetList().m_public->m_arraysToWrite;
  //static std::map<CStr, std::vector<Real> > m_;
  //return m_;
} // iArraysToWrite
//------------------------------------------------------------------------------
/// \brief file global map to hold the filename
//------------------------------------------------------------------------------
static CStr& iNameFileName ()
{
  return GetList().m_public->m_fname;
  //static CStr m_;
  //return m_;
} // iNameFileName
//------------------------------------------------------------------------------
/// \brief file global map to associate an array name with a parameter
//------------------------------------------------------------------------------
static std::map<CStr, std::set<int> >& iGetArrayNameKeyMap ()
{
  return GetList().m_public->m_ArrayNameKeyMap;
  //static std::map<CStr, std::set<int> > m_;
  //return m_;
} // iGetArrayNameKeyMap
//------------------------------------------------------------------------------
/// \brief Gets a map with parameter data associated with arrays
//------------------------------------------------------------------------------
static stdext::hash_map<int, pData>& GetPData ()
{
  return GetList().m_public->m_parData;
  //static stdext::hash_map<int, pData> m_parData;
  //return m_parData;
} // GetPData
//------------------------------------------------------------------------------
/// \brief Fills in the pointer to the parameter list
//------------------------------------------------------------------------------
void Parameters::GetParameterList (ParamList **a_)
{
  if (!a_)
    return;
  *a_ = &GetList();
} // Parameters::GetParameterList
//------------------------------------------------------------------------------
/// \brief Returns a reference to the parameter list
//------------------------------------------------------------------------------
static ParamList& GetList()
{
  return MfData::Get().GetParamList();
  //static ParamList list;
  //return(list);
} // GetList
//------------------------------------------------------------------------------
/// \brief Returns true if parameters exist
//------------------------------------------------------------------------------
static bool ParametersExist ()
{
  if (GetList().Size() > 0)
    return true;
  return false;
} // ParametersExist
//------------------------------------------------------------------------------
/// \brief used when running tests
//------------------------------------------------------------------------------
void Parameters::test_ClearParData ()
{
  GetPData().clear();
} // test_ClearParData
//------------------------------------------------------------------------------
/// \brief Fills in a map with parameter data associated with arrays
//------------------------------------------------------------------------------
static bool FillInPData (const ParamList &a_l,
                         stdext::hash_map<int, pData> &a_map)
{
  a_map.clear();

  std::pair<stdext::hash_map<int, pData>::iterator, bool> retVal;
  stdext::hash_map<int, pData>::iterator it;
  Param p;
  for (size_t i=0; i<a_l.Size(); i++)
  {
    if (!a_l.At(i, &p))
      continue;
    // only do array based parameters
    if (p.m_type.CompareNoCase("hk") == 0 ||
        p.m_type.CompareNoCase("hani") == 0 ||
        p.m_type.CompareNoCase("vk") == 0 ||
        p.m_type.CompareNoCase("vani") == 0 ||
        p.m_type.CompareNoCase("ss") == 0 ||
        p.m_type.CompareNoCase("sy") == 0 ||
        p.m_type.CompareNoCase("vkcb") == 0 ||
        p.m_type.CompareNoCase("rch") == 0 ||
        // we changed "et" to be "evt" to be consisten with MODFLOW
        p.m_type.CompareNoCase("et") == 0 ||
        p.m_type.CompareNoCase("evt") == 0 ||
        p.m_type.CompareNoCase("ets") == 0)
    {
      retVal = a_map.insert(std::make_pair(static_cast<int>(p.m_key), pData()));
      it = retVal.first;

      it->second.m_val = static_cast<Real>(p.m_value);
      CStr fName(GetList().GetSourceFile());
      const int loc(fName.ReverseFind("."));
      fName.Delete(loc+1, (fName.GetLength()-loc)+1);
      fName += "h5";
      if (p.m_pilotPoints)
      {
        // get the file where the pp weights are stored
        std::vector<double> ppVals;
        // make the pp object
        PilotPoints pp(fName, p);
        // see if we need to set the start values from the SEN file
        if (GetList().GetPilotPtValues(p.m_scatIndex, ppVals))
        {
          pp.SetPPStartVals(ppVals);
        }
        // do the interpolation
        if (!pp.DoInterpolation(it->second.m_arrVals))
        {
          return false;
        }
        // put the ppVals in the paramList if needed
        if (ppVals.empty())
        {
          pp.GetPPStartVals(ppVals);
          std::vector<int> isens(ppVals.size(), 0);
          GetList().SetPPValsIsens(p.m_scatIndex, ppVals, isens);
        }
      }
      if (p.m_multArray)
      {
        // get the multiplier array
        std::vector<Real> mult;
        MultArray mArray(fName, p.m_name);
        if (!mArray.GetArray(mult))
          return false;
        if (it->second.m_arrVals.empty())
        {
          it->second.m_arrVals.assign(mult.size(), it->second.m_val);
        }
        for (size_t j=0; j<mult.size() && j<it->second.m_arrVals.size(); j++)
          it->second.m_arrVals.at(j) *= mult.at(j);
      }

      Real *fptr(NULL);
      if (!it->second.m_arrVals.empty())
        fptr = &it->second.m_arrVals.at(0);
      const Real fmin(static_cast<Real>(p.m_min)),
                  fmax(static_cast<Real>(p.m_max));
      for (size_t j=0; j<it->second.m_arrVals.size(); j++, fptr++)
      {
        if (*fptr < fmin)
          *fptr = fmin;
        else if (*fptr > fmax)
          *fptr = fmax;
      }
    }
  }
  return true;
} // FillInPData
//------------------------------------------------------------------------------
/// \brief Returns a pilot point value at a particular index
//------------------------------------------------------------------------------
template <class T>
static bool GetMultiplierArrayValueAtIdx (int a_key,
                                          size_t a_arrayIndex,
                                          T &a_val,
                                          std::map<int, std::vector<Real> > &a_mult)
{
  bool retval(true);

  a_val = 1;

  std::map<int, std::vector<Real> >::iterator it;
  it = a_mult.find(a_key);
  if (it == a_mult.end())
  {
    // put the array into the map
    a_mult.insert(std::make_pair(a_key, std::vector<Real>()));
    it = a_mult.find(a_key);

    Param p;
    if (!GetList().FindByKey(a_key, &p))
      return false;
    if (!p.m_multArray)
      return false;

    // get the file where the pp weights are stored
    CStr fName(GetList().GetSourceFile());
    int loc(fName.ReverseFind("."));
    fName.Delete(loc+1, fName.GetLength()-loc+1);
    fName += "h5";
    // get the multiplier array
    MultArray mArray(fName, p.m_name);
    mArray.GetArray(it->second);
  }

  try
  {
    if (!it->second.empty())
      a_val = (T)it->second.at(a_arrayIndex);
  }
  catch (std::out_of_range)
  {
    retval = false;
  }
  return (retval);
} // GetMultiplierArrayValueAtIdx
//------------------------------------------------------------------------------
/// \brief Gets a set doubles that are the keys for the parameters
//------------------------------------------------------------------------------
template<class T>
static void GetMapOfParamKeysValues (const ParamList &a_l,
                                     std::map<T, T> &a_m)
{
  Param p;
  for (size_t i=0; i<a_l.Size(); i++)
  {
    a_l.At(i, &p);
    if (!p.m_pilotPoints)
      a_m.insert(std::make_pair((T)p.m_key, (T)p.m_value));
  }
} // GetSetOfParamKeys
//------------------------------------------------------------------------------
/// \brief Gets a set doubles that are the keys for the parameters
//------------------------------------------------------------------------------
template<class T>
static void GetMapOfParamKeysValuesPilot (const ParamList &a_l,
                                          std::map<T, T> &a_m)
{
  Param p;
  for (size_t i=0; i<a_l.Size(); i++)
  {
    a_l.At(i, &p);
    if (p.m_pilotPoints)
      a_m.insert(std::make_pair((T)p.m_key, (T)p.m_value));
  }
} // GetSetOfParamKeys
//------------------------------------------------------------------------------
/// \brief Gets a map of parameter keys and values that goes with conductance
/// parameters.
//------------------------------------------------------------------------------
template<class T>
static void GetMapOfParamKeysValuesMultByFactor (const ParamList &a_l,
                                                 std::map<T, T> &a_m)
{
  Param p;
  for (size_t i=0; i<a_l.Size(); i++)
  {
    a_l.At(i, &p);
    CStr type(p.m_type);
    type.ToLower();
    if ("riv" == type || "str" == type || "drn" == type || "ghb" == type ||
        "q" == type || "drt" == type || "sfr" == type || "chd" == type)
      a_m.insert(std::make_pair((T)p.m_key, (T)p.m_value));
  }
} // GetMapOfConductanceParamKeysValues
//------------------------------------------------------------------------------
/// \brief Gets a map of parameter keys and values that goes with conductance
/// parameters.
//------------------------------------------------------------------------------
template<class T>
static void GetMapOfParamKeysValuesByType (const ParamList &a_l,
                                           std::map<T, T> &a_m,
                                           const char * const a_type)
{
  CStr parType(a_type);
  parType.ToLower();
  Param p;
  for (size_t i=0; i<a_l.Size(); i++)
  {
    a_l.At(i, &p);
    CStr type(p.m_type);
    type.ToLower();
    if (parType == type)
      a_m.insert(std::make_pair((T)p.m_key, (T)p.m_value));
  }
} // GetMapOfConductanceParamKeysValues
//------------------------------------------------------------------------------
/// \brief Sets the filename where the parameter definitions are stored
/// the string is formatted like this MPARAM "filename.param"
//------------------------------------------------------------------------------
bool Parameters::SetFileName (const char * const a_fName)
{
  CStr f(a_fName);
  iNameFileName() = f;

  f.Replace("MPARAM", "");
  f.Replace("mparam", "");
  f.Replace("\"", "");
  f.TrimLeft();
  f.TrimRight();
  if (f.IsEmpty())
    return false;
  FILE *fp = fopen(f, "r");
  if (!fp)
  {
    CStr base, ext;
    util::StripExtensionFromFilename(f, base);
    util::StripAllButExtension(f, ext);
    // remove the last 3 characters from the base
    base = base.Left(base.GetLength()-3);
    f = base + "." + ext;
    fp = fopen(f, "r");
    if (!fp) return false;
  }
  fclose(fp);

  //iParamFileName() = f;
  ParamFileReader r(f);
  return (r.FillInListFromFile(&GetList()));
} // Parameters::SetFileName
//------------------------------------------------------------------------------
/// \brief Sets the SEN filename where we read the current value for the 
/// parameters. The line should be formatted like this
/// SEN   57 "filename.snn"
//------------------------------------------------------------------------------
bool Parameters::SetSenFileName (const char * const a_fName)
{
  bool retval(true);
  CStr line(a_fName);
  try 
  {
    bool isPVAL(0);
    EReadAsciiFile r;
    r.UseExceptions();
    r.SetLine(line);
    // this should read the SEN
    r.ReadData(line);
    if (line.CompareNoCase("PVAL") == 0)
      isPVAL = true;
    // this should read the unit number
    r.ReadData(line);
    // this should read the filename
    r.ReadData(line);
    SenFileReader sR(line);
    ParamList& list = GetList();
    if (list.Size() > 0)
      retval = sR.FillInStartingVals(&GetList(), isPVAL);
  }
  catch (const ioexception &e)
  {
    CStr msg(e.what());
    if (!msg.IsEmpty())
    {
      ErrorStack::Get().PutError(msg);
      retval = false;
    }
  }

  return retval;
} // Parameters::SetSenFileName
//------------------------------------------------------------------------------
/// \brief Fills in the PARTYP array
//------------------------------------------------------------------------------
bool Parameters::FillInParType (const int* a_NPVAL,
                                const char* a_PARNAM,
                                char* a_PARTYP)
{
  if (!ParametersExist())
    return true;

  ParamList& list = GetList();
  // loop through the passed in names
  char name[11];
  name[10] = '\0';
  for (int i=0; i<*a_NPVAL; i++)
  {
    for (int j=0; j<10; j++)
    {
      name[j] = a_PARNAM[(i*10)+j];
    }
    CStr pnam = name;
    pnam.Trim();
    Param par;
    if (list.FindByName(pnam, &par))
    {
      if (!par.m_clustInParamFile && par.m_type != "HFB")
      {
        for (int j=0; j < 4; j++)
        {
          a_PARTYP[(i*4)+j] = ' ';
        }
        for (int j=0; j<par.m_type.GetLength() && j < 4; j++)
        {
          a_PARTYP[(i*4)+j] = par.m_type.GetAt(j);
        }
      }
    }
    else if (pnam.Find("sc") != -1 &&
             pnam.Find("v") != -1)
    { // check pilot point names
      CStr tmpNam(pnam);
      tmpNam.Replace("v", " ");
      tmpNam.Replace("sc", "");
      if (!tmpNam.IsEmpty())
      {
        // read the scatter index
        std::stringstream str;
        str << tmpNam.c_str();
        int idx;
        str >> idx;
        // loop through parameters and find scatindex
        for (size_t q=0; q<list.Size(); ++q)
        {
          list.At(q, &par);
          if (par.m_scatIndex == idx)
          {
            for (int j=0; j < 4; j++)
            {
              a_PARTYP[(i*4)+j] = ' ';
            }
            for (int j=0; j<par.m_type.GetLength() && j < 4; j++)
            {
              a_PARTYP[(i*4)+j] = par.m_type.GetAt(j);
            }
          }
        }
      }
    }

  }
  return true;
} // Parameters::FillInParType
//------------------------------------------------------------------------------
/// \brief Gets the number of cells in the unstructured grid
//------------------------------------------------------------------------------
int iGetNumCellsUnstructured ()
{
  int rval = 0;
  MfData::MfPackage *p = MfData::Get().GetPackage(MfData::Packages::DISU);
  ASSERT(p);
  if (p)
  {
    const int* nodlay(0);
    p->GetField(MfData::Packages::Disu::NODLAY, &nodlay);
    if (nodlay)
    {
      for (int i=0; i<MfData::Get().NumLay(); ++i) rval += nodlay[i];
    }
  }
  return rval;
} // iGetNumCellsUnstructured
//------------------------------------------------------------------------------
/// \brief figures out the starting position in the pilot point array for
/// unstructured grids.
//------------------------------------------------------------------------------
int iGetStart (int a_lay)
{
  int rval = 0;
  if (!MfData::Get().Unstructured()) return rval;
  if (a_lay < 2) return rval;

  // get the number of nodes per layer
  MfData::MfPackage* p = MfData::Get().GetPackage(MfData::Packages::DISU);
  if (p)
  {
    const int* nodlay(0);
    p->GetField(MfData::Packages::Disu::NODLAY, &nodlay);
    if (nodlay)
    {
      for (int i=0; i<a_lay-1; ++i) rval += nodlay[i];
    }
  }

  return rval;
} // iGetStart
//------------------------------------------------------------------------------
/// \brief Substitutes the parameter value for the key value in the array.
//------------------------------------------------------------------------------
template <class T>
bool SubstituteArrayT (T *a_, size_t a_size, int a_layer, const CStr& a_name)
{
  if (!a_ || a_size < 1)
    return false;

  // see if parameters exist
  if (!ParametersExist())
    return true;

  if (GetPData().empty())
  {
    FillInPData(GetList(), GetPData());
  }
  stdext::hash_map<int, pData>::iterator itHash, itHashEnd;
  itHashEnd = GetPData().end();

  int start = iGetStart(a_layer);
  double intPart, decimalPart;
  for (size_t i=0; i<a_size; i++)
  {
    if (a_[i] < 0)
    {
      decimalPart = modf(a_[i], &intPart);
      if (decimalPart == 0)
      {
        itHash = GetPData().find((int)a_[i]);
        if (itHash != itHashEnd)
        {
          iGetArrayNameKeyMap()[a_name].insert((int)a_[i]);
          if (itHash->second.m_arrVals.empty())
          {
            a_[i] = (T)itHash->second.m_val;
          }
          else
          {
            a_[i] = (T)itHash->second.m_arrVals.at(i+start);
          }
        }
      }
    }
  }
  return true;
} // Parameters::SubstituteArrayT
//------------------------------------------------------------------------------
/// \brief Substitutes the parameter value for the key value in the array.
//------------------------------------------------------------------------------
bool Parameters::SubstituteArray (double *a_, size_t a_size, int a_layer,
                                  const CStr& a_name)
{
  return SubstituteArrayT(a_, a_size, a_layer, a_name);
} // Parameters::SubstituteArray
//------------------------------------------------------------------------------
/// \brief Substitutes the parameter value for the key value in the array.
//------------------------------------------------------------------------------
bool Parameters::SubstituteArray (float *a_, size_t a_size, int a_layer,
                                  const CStr& a_name)
{
  return SubstituteArrayT(a_, a_size, a_layer, a_name);
} // Parameters::SubstituteArray
void Parameters::SubstituteArray (int* /*a_*/, size_t /*a_size*/,int, const CStr& )
{
  // we don't do anything with int arrays and parameters
  return;
} // Parameters::SubstituteArray
//------------------------------------------------------------------------------
/// \brief Substitutes the parameter value for the key value in the passed in
/// variable.
//------------------------------------------------------------------------------
bool Parameters::SubstituteValue (double *a_)
{
  if (!a_)
    return false;
  // see if parameters exist
  if (!ParametersExist())
    return true;

  std::map<double, double> map;
  std::map<double, double>::iterator it;
  GetMapOfParamKeysValues(GetList(), map);

  it = map.find(*a_);
  if (it != map.end())
    *a_ = it->second;
  return true;
} // Parameters::SubstituteValue
//------------------------------------------------------------------------------
/// \brief Substitutes the parameter value for the key value for data read by
/// the list reader (ULSTRD) in MODFLOW.
//------------------------------------------------------------------------------
bool Parameters::SubstituteList (std::vector<Real> &a_,
                                 const std::vector<Real> &a_fact,
                                 const char * const a_parType)
{
  // see if parameters exist
  if (!ParametersExist())
    return true;

  // the size of a_fact should be the number of rows in the data
  size_t nRow, nCol;

  nRow = a_fact.size();
  // there should be no remainder when the size of a_ is divided by nRow
  if (a_.size() % nRow)
  {
    ErrorStack::Get().PutError("Invalid arrays passed to SubstituteList.");
    return false;
  }
  nCol = a_.size() / nRow;

  // loop through the list
  try
  {
    std::map<Real, Real> condMap1, pMap2;
    std::map<Real, Real>::iterator it1, end1, it2, end2;
    GetMapOfParamKeysValuesByType(GetList(), pMap2, a_parType);
    GetMapOfParamKeysValuesMultByFactor(GetList(), condMap1);
    end1 = condMap1.end();
    end2 = pMap2.end();

    size_t i, j, cnt(0);
    for (i=0; i<nCol; i++)
    {
      for (j=0; j<nRow; j++, cnt++)
      {
        if (a_.at(cnt) < 0)
        {
          // see if this is a parameter that we should be looking at
          it2 = pMap2.find(a_.at(cnt));
          if (it2 != pMap2.end())
          {
            // see if this is a parameter that should be multiplied by the
            // factor (like conductance).
            it1 = condMap1.find(a_.at(cnt));
            if (it1 != end1)
            {
              a_.at(cnt) = it1->second*a_fact.at(j);
            }
            else // do a regular substitution
            {
              a_.at(cnt) = it2->second;
            }
          }
        }
      }
    }
  }
  catch (std::out_of_range&)
  {
    ErrorStack::Get().PutError("Accessing outside of array range.");
    return false;
  }
  return true;
} // Parameters::SubstituteList
//------------------------------------------------------------------------------
/// \brief Substitutes a factor based parameter value into an array.
//------------------------------------------------------------------------------
bool Parameters::SubstituteProperty(CAR_DBL2D &a_,
                                    const CStr &a_parType,
                                    const int a_valueIndex,
                                    const int a_factorIndex)
{
  // see if parameters exist
  if (!ParametersExist())
    return true;

  // value and factor index should be in bounds of first a_ index
  if (a_valueIndex < 0 || a_valueIndex >= a_.GetSize1() ||
      a_factorIndex < 0 || a_factorIndex >= a_.GetSize1())
  {
    ErrorStack::Get().PutError("Invalid arrays passed to SubstituteProperty.");
    return false;
  }

  // loop through the array
  try
  {
    std::map<Real, Real> keyValues;
    GetMapOfParamKeysValuesByType(GetList(), keyValues, a_parType);

    for (int j = 0; j < a_.GetSize2(); ++j)
    {
      Real value = static_cast<Real>(a_.at(a_valueIndex, j));
      if (value < 0)
      {
        // see if this is a parameter that we should be looking at
        std::map<Real, Real>::iterator keyValue = keyValues.find(value);
        if (keyValue != keyValues.end())
          a_.at(a_valueIndex, j) = a_.at(a_factorIndex, j) * keyValue->second;
      }
    }
  }
  catch (std::out_of_range&)
  {
    ErrorStack::Get().PutError("Accessing outside of array range in "
                               "SubstituteProperty.");
    return false;
  }
  return true;
} // Parameters::SubstituteProperty
//------------------------------------------------------------------------------
/// \brief Using the path to the boundary condition this function will figure
/// out what the parameter type should be.
//------------------------------------------------------------------------------
bool Parameters::ParTypeFromH5Path (const char * const a_path,
                                    CStr &a_type)
{
  std::map<CStr, CStr> m_map;
  m_map.insert(std::make_pair("drain", "DRN"));
  m_map.insert(std::make_pair("drain return", "DRT"));
  m_map.insert(std::make_pair("general head", "GHB"));
  m_map.insert(std::make_pair("river", "RIV"));
  m_map.insert(std::make_pair("specified head", "CHD"));
  m_map.insert(std::make_pair("stream", "STR"));
  m_map.insert(std::make_pair("well", "Q"));

  a_type = "";

  CStr path(a_path);
  path.ToLower();
  std::map<CStr, CStr>::iterator it;
  it = m_map.find(path);
  if (it == m_map.end())
    return false;

  a_type = it->second;
  return true;
} // Parameters::ParTypeFromH5Path
//------------------------------------------------------------------------------
/// \brief Check to see if it is ok to parameter substitute into a list
//------------------------------------------------------------------------------
bool Parameters::CheckListSubstituteOk ()
{
  if (mfLibExp_Exporting())
    return false;
  return true;
} // Parameters::CheckListSubstituteOk
//------------------------------------------------------------------------------
/// \brief Using the path to the array. See if it is ok to substitute in this
/// array. We don't want to change the top, bottom, or starting head arrays
//------------------------------------------------------------------------------
bool Parameters::CheckArraySubstituteOk (const char * const a_h5Path)
{
  CStr path(a_h5Path);
  path.ToLower();
  if (mfLibExp_Exporting() ||
      path.find("top") != -1 ||
      path.find("bot") != -1 ||
      path.find("vcb") != -1 ||
      path.find("starthead") != -1 ||
      path.find("anglex") != -1 ||
      path.find("ibound") != -1)
    return false;
  return true;
} // Parameters::CheckArraySubstituteOk
//------------------------------------------------------------------------------
/// \brief This gets array parameter data
//------------------------------------------------------------------------------
void Parameters::FillArrayParamData ()
{
  if (GetPData().empty())
  {
    FillInPData(GetList(), GetPData());
  }
} // Parameters::FillArrayParamData
//------------------------------------------------------------------------------
/// \brief copies data into a map
//------------------------------------------------------------------------------
template <class T, class U>
static void iCopyArray (std::vector<Real>& a_vec,
                        const T* a_ARR,
                        const U* a_MULT,
                        const int* a_K,
                        const int* a_JJ,
                        const int* a_II)
{
  int nCellsLay( (*a_JJ)*(*a_II) );
  int k(*a_K);
  if (k < 1) k = 1;
  int start(iGetStart(k));
  int nVal( (k) * nCellsLay );
  if (MfData::Get().Unstructured())
  {
    nVal = iGetNumCellsUnstructured();
  }

  // make sure vector is big enough
  a_vec.resize(nVal);
  for (int i=0; i<nCellsLay; ++i)
  {
    a_vec[start+i] = (Real)(a_ARR[i] * (*a_MULT));
  }
} // iCopyArray
//------------------------------------------------------------------------------
/// \brief Gets the active array for a data set
//------------------------------------------------------------------------------
static void iGetActiveArray (const CStr& a_dsName,
                             std::vector<char>& a_act,
                             const int* a_JJ,
                             const int* a_II)
{
  std::map<CStr, std::vector<Real> >& aMap(iArraysToWrite());
  std::vector<Real>& ibound(aMap[ARR_BAS_IBND]);
  if (a_dsName.find("RCH ") != -1 && 
      !MfData::Get().Unstructured())
  {
    int nCellsLay = *a_JJ * *a_II;
    a_act.assign(nCellsLay, 0);
    // assign the first layer of the ibound to the array
    for (int i=0; i<nCellsLay; i++)
    {
      if (0 != ibound[i])
      {
        a_act[i] = 1;
      }
    }

    // get recharge cell assignment option
    int opt(1);
    MfData::Get().GetIntVar("NRCHOP", opt);

    try
    {
      if (2 == opt && aMap.find("RECHARGE LAYER INDEX") != aMap.end())
      {
        int pos;
        std::vector<Real> layInd(ibound);
        layInd = aMap["RECHARGE LAYER INDEX"];
        for (int i=0; i<nCellsLay; i++)
        {
          a_act[i] = 0;
          pos = i + ( ((int)layInd[i] - 1) * nCellsLay );
          if (0 != ibound.at(pos))
          {
            a_act[i] = 1;
          }
        }
      }
      else if (opt == 3)
      {
        for (int i=0; i<nCellsLay; i++)
        {
          for (int k=1; !a_act[i] && k<MfData::Get().NumLay(); k++)
          {
            if (0 != ibound.at((k*nCellsLay) + i))
            {
              a_act[i] = 1;
            }
          }
        }
      }
    }
    catch (std::out_of_range&) { ASSERT(0); }
  }
  else //if (a_dsName.Find("HK ") != -1)
  {
    a_act.assign(ibound.size(), 0);
    for (size_t i=0; i<ibound.size(); i++)
    {
      if (0 != ibound[i])
      {
        a_act[i] = 1;
      }
    }
  }
} // iGetActiveArray
//------------------------------------------------------------------------------
/// \brief Writes a MODFLOW array that had parameters to a data set
//------------------------------------------------------------------------------
static void iWriteDataSet (std::vector<Real>& a_vec,
                           const CStr& a_dsName,
                           const int* a_JJ,
                           const int* a_II)
{
  using namespace H5DataReader;

  // get file name
  CStr fname = iNameFileName();
  util::StripExtensionFromFilename(fname, fname);
  fname += ".ParameterArray.h5";
  //{ // write data set values
  //  CStr path1 = a_dsName;
  //  H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
  //                             Get_H5_nativetype(&a_vec[0]));
  //  H5DataSetWriter w(&setup);
  //  w.WriteData(&a_vec[0], a_vec.size());
  //}
  //return;

  // get activity array
  std::vector<char> act;
  iGetActiveArray(a_dsName, act, a_JJ, a_II);
  // get data set min and max
  Real dmin;
  Real dmax;

  if (act.empty())
  {
    dmin= *std::min_element(a_vec.begin(), a_vec.end());
    dmax = *std::max_element(a_vec.begin(), a_vec.end());
  }
  else
  {
    dmax = -FLT_MAX;
    dmin = FLT_MAX;
    for (size_t i=1; i<a_vec.size(); i++)
    {
      if (act[i])
      {
        if (a_vec[i] < dmin) dmin = a_vec[i];
        if (a_vec[i] > dmax) dmax = a_vec[i];
      }
    }
  }

  // if the array is not the size of the grid then duplicate the layer
  // to multiple layers
  std::vector<Real> vec(a_vec);
  int nCells = MfData::Get().NumRow() *
               MfData::Get().NumCol() *
               MfData::Get().NumLay();
  if (MfData::Get().Unstructured())
  {
    nCells = iGetNumCellsUnstructured();
  }

  if (a_vec.size() < (size_t)nCells)
  {
    std::vector<Real> tmpReal;
    std::vector<char> tmpChar;
    for (int i=0; i<MfData::Get().NumLay(); i++)
    {
      tmpReal.insert(tmpReal.end(), a_vec.begin(), a_vec.end());
      if (!act.empty())
      {
        tmpChar.insert(tmpChar.end(), act.begin(), act.end());
      }
    }
    if (!act.empty())
    {
      act = tmpChar;
    }
    vec = tmpReal;
  }

  CStr path;
  path.Format("Datasets/%s/", a_dsName);
  { // create group for the data set
    H5DataSetWriterSetup setup(fname.c_str());
    H5DataSetWriter w(&setup);
    w.CreateGroup("Datasets");
    w.CreateGroup(path.c_str());
  }
  { // put atts on groups
    hid_t fid, gid;
    fid = H5DataReader::GetFileId(fname.c_str());
    if (fid > -1)
    {
      gid = H5Gopen(fid, "Datasets");
      if (gid > -1)
      {
        xfpWriteAttributeString(gid, "Grouptype", "MULTI DATASETS");
        H5Gclose(gid);
      }
      gid = H5Gopen(fid, path.c_str());
      if (gid > -1)
      {
        int iVal(0);
        xfpWriteAttributeInt(gid, "Data Type", 1, &iVal);
        iVal = 1;
        xfpWriteAttributeInt(gid, "DatasetCompression", 1, &iVal);
        xfpWriteAttributeString(gid, "DatasetUnits", "None");
        xfpWriteAttributeString(gid, "Grouptype", "DATASET SCALAR");
        xfpWriteAttributeString(gid, "TimeUnits", "None");
        double t(2);
        xfpWriteAttributeDouble(gid, "Reftime", 1, &t);
        H5Gclose(gid);
      }
    }
  }

  { // write the min/max data sets
    CStr path1 = path;
    path1 += "Mins";
    H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
                               Get_H5_nativetype(&dmin));
    H5DataSetWriter w(&setup);
    w.WriteData(&dmin, 1);
  }
  {
    CStr path1 = path;
    path1 += "Maxs";
    H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
                               Get_H5_nativetype(&dmax));
    H5DataSetWriter w(&setup);
    w.WriteData(&dmax, 1);
  }
  { // write data set times
    Real time(0);
    CStr path1 = path;
    path1 += "Times";
    H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
                               Get_H5_nativetype(&time));
    H5DataSetWriter w(&setup);
    w.WriteData(&time, 1);
  }
  { // write data set values
    CStr path1 = path;
    path1 += "Values";
    H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
                               Get_H5_nativetype(&vec[0]), 2);
    H5DataSetWriter w(&setup);
    std::vector<hsize_t> start(2,0), n2write(2,1);
    n2write[1] = vec.size();
    H5DSWriterDimInfo dim(start, n2write);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vec[0], vec.size());
  }
  { // write data set name
    CStr path1 = path;
    path1 += "Name";
    hid_t fid(H5DataReader::GetFileId(fname));
    xfpWriteDatasetString(fid, path1.c_str(), a_dsName.c_str());
  }
  if (!act.empty())
  {
    CStr path1 = path;
    path1 += "Active";
    H5DataSetWriterSetup setup(fname.c_str(),path1.c_str(),
                               Get_H5_nativetype(&act[0]), 2);
    H5DataSetWriter w(&setup);
    std::vector<hsize_t> start(2,0), n2write(2,1);
    n2write[1] = act.size();
    H5DSWriterDimInfo dim(start, n2write);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&act[0], act.size());
  }
  { // write info that file is XMDF
    hid_t fid(H5DataReader::GetFileId(fname));
    xfpWriteDatasetString(fid, "File Type", "Xmdf");
  }
} // iWriteArray
//------------------------------------------------------------------------------
/// \brief Updates a data set name with the parameter key values
//------------------------------------------------------------------------------
static void iUpdateDataSetName (CStr& a_dName,
                                const CStr& a_NAME)
{
  std::map<CStr, std::set<int> >& aMap(iGetArrayNameKeyMap());
  if (aMap.find(a_NAME) == aMap.end())
    return;

  a_dName += " Parameter";
  std::set<int> vI(aMap[a_NAME]);
  std::set<int>::iterator it(vI.begin());
  for (; it!=vI.end(); ++it)
  {
    if (it != vI.begin())
    {
      a_dName += ",";
    }
    CStr s;
    s.Format("%d", *it);
    a_dName += " ";
    a_dName += s;
  }
} // iUpdateDataSetName
//------------------------------------------------------------------------------
static void iGetDataSetNameFromArrayName (const CStr& a_NAME,
                                          CStr& dataName)
{
  dataName = "";
  if (ARR_LPF_HK == a_NAME) dataName = "HK";
  else if (ARR_LPF_SS == a_NAME) dataName = "SS";
  else if (ARR_LPF_SY == a_NAME) dataName = "SY";
  else if (ARR_LPF_HANI == a_NAME) dataName = "HANI";
  else if (ARR_LPF_VANI == a_NAME) dataName = "VANI";
  else if (ARR_LPF_VK == a_NAME) dataName = "VK";
  else if (ARR_LPF_VKCBD == a_NAME) dataName = "VKCB";
  
} // iGetDataSetNameFromArrayName
//------------------------------------------------------------------------------
/// \brief Exports an array that uses pilot points to a data set
//------------------------------------------------------------------------------
template <class T>
static void iExportParameterArrayToDataSetT (
  CStr& a_NAME,
  const T* a_ARR,
  const Real* a_MULT,
  const int* a_K,
  const int* a_JJ,
  const int* a_II)
{
  std::map<CStr, std::set<int> >& aMap(iGetArrayNameKeyMap());
  if (aMap.find(a_NAME) == aMap.end())
    return;

  // copy the data
  iCopyArray(iArraysToWrite()[a_NAME],a_ARR,a_MULT,a_K,a_JJ,a_II);

  bool delArray(false);
  CStr dataName;
  iGetDataSetNameFromArrayName(a_NAME, dataName);
  if (!dataName.IsEmpty())
  {
    // don't write the data set until we have the IBOUND
    if (iArraysToWrite().find(ARR_BAS_IBND) != iArraysToWrite().end() &&
      a_K && *a_K == MfData::Get().NumLay())
    {

      iUpdateDataSetName(dataName, a_NAME);
      iWriteDataSet(iArraysToWrite()[a_NAME], dataName, a_JJ, a_II);
      delArray = true;
    }
  }
  else if ("RECHARGE" == a_NAME)
  {
    CStr dName;
    dName.Format("RCH SP_%d", MfData::Get().GetCurrentPeriod());
    iUpdateDataSetName(dName, a_NAME);
    int opt(1);
    MfData::Get().GetIntVar("NRCHOP", opt);
    if (2 != opt)
    {
      iWriteDataSet(iArraysToWrite()[a_NAME], dName, a_JJ, a_II);
      delArray = true;
    }
  }

  // write the data set
  if (delArray)
  {
    iArraysToWrite().erase(a_NAME);
    aMap.erase(a_NAME);
  }

} // iExportParameterArrayToDataSetT
//------------------------------------------------------------------------------
/// \brief Exports an array that uses pilot points to a data set
//------------------------------------------------------------------------------
void Parameters::ExportParameterArrayToDataSet (CStr& a_NAME,
                                                const Real* a_ARR,
                                                const Real* a_MULT,
                                                const int* a_K,
                                                const int* a_JJ,
                                                const int* a_II)
{
  iExportParameterArrayToDataSetT(a_NAME, a_ARR, a_MULT, a_K, a_JJ, a_II);
} // Parameters::ExportParameterArrayToDataSet
//------------------------------------------------------------------------------
/// \brief Exports an array that uses pilot points to a data set
//------------------------------------------------------------------------------
void Parameters::ExportParameterArrayToDataSet8 (CStr& a_NAME,
                                                 const double* a_ARR,
                                                 const Real* a_MULT,
                                                 const int* a_K,
                                                 const int* a_JJ,
                                                 const int* a_II)
{
  iExportParameterArrayToDataSetT(a_NAME, a_ARR, a_MULT, a_K, a_JJ, a_II);
} // Parameters::ExportParameterArrayToDataSet8
//------------------------------------------------------------------------------
/// \brief Exports an array that uses pilot points to a data set
//------------------------------------------------------------------------------
void Parameters::ExportParameterArrayToDataSet (CStr& a_NAME,
                                                const int* a_ARR,
                                                const int* a_MULT,
                                                const int* a_K,
                                                const int* a_JJ,
                                                const int* a_II)
{
  using namespace H5DataReader;


  bool doExport(true);
  CStr nm(a_NAME), dname, arrayName;
  CStr dataName;
  nm.MakeUpper();
  if (ARR_BAS_IBND == nm)
  {
    if (!a_K || *a_K != MfData::Get().NumLay())
    {
      doExport = false;
    }
  }
  else if ("RECHARGE LAYER INDEX" == nm)
  {
    dataName.Format("RCH SP_%d", MfData::Get().GetCurrentPeriod());
    arrayName = "RECHARGE";
  }
  else
  {
    return;
  }

  // copy the data
  iCopyArray(iArraysToWrite()[a_NAME],a_ARR,a_MULT,a_K,a_JJ,a_II);

  if (!doExport)
  {
    return;
  }

  if (ARR_BAS_IBND == nm)
  { // export all data that is not associated with a stress period
    std::map<CStr, std::vector<Real> >::iterator it(iArraysToWrite().begin());
    for (; it != iArraysToWrite().end(); ++it)
    {
      if (it->first.Find("SP_") == -1 &&
          it->first != ARR_BAS_IBND)
      {
        iGetDataSetNameFromArrayName(it->first, dataName);
        iUpdateDataSetName(dataName, it->first);
        iWriteDataSet(iArraysToWrite()[it->first], dataName, a_JJ, a_II);
      }
    }
  }
  else
  {
    iUpdateDataSetName(dataName, arrayName);
    if (iArraysToWrite().find(arrayName) != iArraysToWrite().end())
    {
      iWriteDataSet(iArraysToWrite()[arrayName], dataName, a_JJ, a_II);
    }
  }

} // Parameters::ExportParameterArrayToDataSet
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Parameters.t.h>

//------------------------------------------------------------------------------
void ParametersT::setUp ()
{
  char c[5000];
  GetModuleFileName(NULL, c, 5000);
  CStr str(c);
  // strip off the file name
  int pos(str.ReverseFind("\\"));
  CStr exeName = str.Left(str.GetLength() - (str.GetLength() - pos));
  m_file = util::GetTestFilesDirectory() + "\\Parameter\\pest.param";
  m_file1 = util::GetTestFilesDirectory() + "\\Parameter\\pest.snn";
}
//------------------------------------------------------------------------------
void ParametersT::tearDown ()
{
  GetPData().clear();
  GetList().Clear();
}
//------------------------------------------------------------------------------
void ParametersT::testSetFileName ()
{
  CStr empty, wrong, correct;
  TS_ASSERT(!Parameters::SetFileName(empty));

  wrong = "wrong";
  TS_ASSERT(!Parameters::SetFileName(wrong));
  wrong = "mparam noFile.param";
  TS_ASSERT(!Parameters::SetFileName(wrong));

  correct.Format("mparam%s", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct));
  correct.Format("MPARAM\"%s\"", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct));
  correct.Format("MPARAM \"%s\"", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct));
  correct.Format("MPARAM %s", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct));
  correct.Format("mparam %s", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct));
}
//------------------------------------------------------------------------------
void ParametersT::testSetSenFileName ()
{
  CStr empty, wrong, correct;
  TS_ASSERT(!Parameters::SetSenFileName(empty.c_str()));

  wrong = "wrong";
  TS_ASSERT(!Parameters::SetSenFileName(wrong.c_str()));
  wrong = "SEN wrong";
  TS_ASSERT(!Parameters::SetSenFileName(wrong.c_str()));

  correct.Format("SEN 57 \"%s\"", m_file1.c_str());
  // this will not fail (empty param list allowed)
  TS_ASSERT(Parameters::SetSenFileName(correct.c_str()));

  wrong = "SEN 57 wrong";
  TS_ASSERT(!Parameters::SetFileName(wrong.c_str()));
  correct.Format("mparam %s", m_file.c_str());
  TS_ASSERT(Parameters::SetFileName(correct.c_str()));
  correct.Format("SEN 57 \"%s\"", m_file1.c_str());
  TS_ASSERT(Parameters::SetSenFileName(correct.c_str()));
  Param p;
  TS_ASSERT(GetList().FindByName("RCH_150", &p));
  TS_ASSERT_EQUALS(p.m_value, 150.0);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteList_WrongSizeArrays ()
{
  std::vector<Real> data(8, 5), factor(3, 1);
  // no paramters
  TS_ASSERT(Parameters::SubstituteList(data, factor, "DRN"));
  // make a parameter
  Param p("drn_1", -1, "other", 19, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(!Parameters::SubstituteList(data, factor, "DRN"));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteList_NoKeys ()
{
  // here is the data this would be like a drain file
  // 1 3 2 19.0 10.0 6 2.0 -1
  // 1 4 3 19.0 10.0 4 2.0 -1
  // 1 5 4 19.0 10.0 5 2.0 -1
  // so the array would be 19.0 19.0 19.0 10.0 10.0 10.0 and
  // the factors would be 2.0 2.0 2.0

  Real a[6] = {19.0,19.0,19.0,10.0,10.0,10.0};
  Real b[3] = {2,2,2};

  std::vector<Real> data(&a[0], &a[6]), factor(&b[0], &b[3]);

  // returns true - there are no parameters
  TS_ASSERT(Parameters::SubstituteList(data, factor, "DRN"));

  // make some parameters
  Param p("drn_1", -1, "DRN", 10, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteList(data, factor, "DRN"));
  // the array should be the same
  for (size_t i=0; i<6; i++)
    TS_ASSERT_EQUALS(a[i], data.at(i));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteList_Keys ()
{
  // here is the data this would be like a drain file
  // 1 3 2 19.0 10.0 6 2.0 -1
  // 1 4 3 19.0 10.0 4 2.0 -1
  // 1 5 4 19.0 10.0 5 2.0 -1
  // so the array would be 19.0 19.0 19.0 10.0 10.0 10.0 and
  // the factors would be 2.0 2.0 2.0
  // In this case we will change the 19.0 to -1 and then
  // substitute. This would be like it drain elev was a parameter.
  Real a[6] = {-1,-1,-1,10.0,10.0,10.0};
  Real result[6] = {19.0,19.0,19.0,10.0,10.0,10.0};
  Real b[3] = {2,2,2};

  std::vector<Real> data(&a[0], &a[6]), factor(&b[0], &b[3]);

  // make a parameter
  Param p("drn_1", -1, "other", 19, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteList(data, factor, "other"));
  // the array should be the same
  for (size_t i=0; i<6; i++)
    TS_ASSERT_EQUALS(result[i], data.at(i));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteList_KeysFactors ()
{
  // here is the data this would be like a drain file
  // 1 3 2 19.0 20.0 6 2.0 -1
  // 1 4 3 19.0 20.0 4 2.0 -1
  // 1 5 4 19.0 20.0 5 2.0 -1
  // so the array would be 19.0 19.0 19.0 10.0 10.0 10.0 and
  // the factors would be 2.0 2.0 2.0
  // In this case we will change the 20.0 to -1 and then
  // substitute. This would be like drain cond was a parameter.
  Real a[6] = {19,19,19,-10,-10,-10};
  Real result[6] = {19.0,19.0,19.0,20.0,20.0,20.0};
  Real b[3] = {2,2,2};

  std::vector<Real> data(&a[0], &a[6]), factor(&b[0], &b[3]);

  // make a parameter
  Param p("drn_10", -10, "DRN", 10, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteList(data, factor, "DRN"));
  // the array should be the same
  for (size_t i=0; i<6; i++)
    TS_ASSERT_EQUALS(result[i], data.at(i));

}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteProperty_WrongSizeArrays ()
{
  CAR_DBL2D data;
  data.SetSize(4, 5, 0);
  // no paramters
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", -1, 0));
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 0, -1));
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 4, 0));
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 0, 4));
  // make a parameter
  Param p("drn_1", -1, "other", 19, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(!Parameters::SubstituteProperty(data, "DRN", -1, 0));
  TS_ASSERT(!Parameters::SubstituteProperty(data, "DRN", 0, -1));
  TS_ASSERT(!Parameters::SubstituteProperty(data, "DRN", 4, 0));
  TS_ASSERT(!Parameters::SubstituteProperty(data, "DRN", 0, 4));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteProperty_NoKeys ()
{
  CAR_DBL2D data;
  data.SetSize(4, 5, 1.0);

  // returns true - there are no parameters
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 1, 2));

  // make some parameters
  Param p("drn_1", -1, "DRN", 10, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 1, 2));
  // the array should be the same
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 5; ++j)
      TS_ASSERT_EQUALS(data.at(i, j), 1.0);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteProperty_KeysFactors ()
{
  CAR_DBL2D data;
  data.SetSize(3, 3, 2.0);
  data.at(0, 0) = -10;
  data.at(0, 1) = -10;

  // make a parameter
  Param p("drn_10", -10, "DRN", 10, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteProperty(data, "DRN", 0, 2));
  // keys should change
  TS_ASSERT_EQUALS(data.at(0, 1), 20.0);
  TS_ASSERT_EQUALS(data.at(0, 1), 20.0);

  // everything else shouldn't
  TS_ASSERT_EQUALS(data.at(0, 2), 2.0);
  for (int i = 1; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      TS_ASSERT_EQUALS(data.at(i, j), 2.0);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteValue_NoParam ()
{
  TS_ASSERT(!Parameters::SubstituteValue(NULL));
  double d;
  // no paramters
  TS_ASSERT(Parameters::SubstituteValue(&d));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteValue_NoKey ()
{
  double d(9);
  // make a param
  Param p("drn_10", -10, "DRN", 10, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteValue(&d));
  TS_ASSERT_EQUALS(d, 9);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteValue ()
{
  double d(-10);
  // make a param
  Param p("drn_10", -10, "DRN", 30, .01, 50);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteValue(&d));
  TS_ASSERT_EQUALS(d, 30);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteArray_NoParam ()
{
  Real f, *fptr(NULL);
  int k(-1);
  CStr str;
  TS_ASSERT(!Parameters::SubstituteArray(fptr, 1, k, str));
  TS_ASSERT(!Parameters::SubstituteArray(&f, 0, k, str));
  TS_ASSERT(Parameters::SubstituteArray(&f, 1, k, str));
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteArray_NoKey ()
{
  Real a[6] = {19.0,19.0,19.0,10.0,10.0,10.0};
  Real result[6] = {19.0,19.0,19.0,10.0,10.0,10.0};
  int k(-1);
  CStr str;
  // no parameters
  TS_ASSERT(Parameters::SubstituteArray(a, 6, k, str));
  // make a param
  Param p("hk_1", -1, "HK", 5, .01, 100);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteArray(a, 6, k, str));
  for (int i=0; i<6; i++)
    TS_ASSERT_EQUALS(a[i], result[i]);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteArray_NoPilot ()
{
  Real a[6] = {-1,19.0,-1,10.0,-1,10.0};
  Real result[6] = {5,19.0,5,10.0,5,10.0};
  int k(-1);
  CStr str;
  // no parameters
  TS_ASSERT(Parameters::SubstituteArray(a, 6, k, str));
  // make a param
  Param p("hk_1", -1, "HK", 5, .01, 100);
  GetList().PushBack(&p);
  TS_ASSERT(Parameters::SubstituteArray(a, 6, k, str));
  for (int i=0; i<6; i++)
    TS_ASSERT_EQUALS(a[i], result[i]);
}
//------------------------------------------------------------------------------
void ParametersT::testSubstituteArray_NoPilot_WithMultArray ()
{
  //TS_FAIL("ParametersT::testSubstituteArray_NoPilot_WithMultArray");

  Real a[6] = {-1,19.0,-1,10.0,-1,10.0};
  Real result[6] = {3,19.0,(Real)2.2,10.0,(Real)2.2,10.0};
  CStr str;
  // make a param
  Param p("rch_150", -1, "RCH", 2, 1.0e-010, 3);
  p.m_multArray = true;
  GetList().PushBack(&p);
  int k(-1);
  TS_ASSERT(Parameters::SubstituteArray(a, 6, k, str));
  for (int i=0; i<6; i++)
    TS_ASSERT_DELTA(a[i], result[i], 1e-5);
}
//------------------------------------------------------------------------------
void ParametersT::testParTypeFromH5Path ()
{
  CStr type;
  type = "stuff";
  TS_ASSERT(!Parameters::ParTypeFromH5Path("stuff", type));
  TS_ASSERT(type == "");

  TS_ASSERT(Parameters::ParTypeFromH5Path("DRAIN", type));
  TS_ASSERT(type == "DRN");
  TS_ASSERT(Parameters::ParTypeFromH5Path("drain", type));
  TS_ASSERT(type == "DRN");
  TS_ASSERT(Parameters::ParTypeFromH5Path("dRaIn", type));
  TS_ASSERT(type == "DRN");

  TS_ASSERT(Parameters::ParTypeFromH5Path("DRAIN RETURN", type));
  TS_ASSERT(type == "DRT");
  TS_ASSERT(Parameters::ParTypeFromH5Path("drain return", type));
  TS_ASSERT(type == "DRT");
  TS_ASSERT(Parameters::ParTypeFromH5Path("dRaIn rEtUrN", type));
  TS_ASSERT(type == "DRT");

  TS_ASSERT(Parameters::ParTypeFromH5Path("RIVER", type));
  TS_ASSERT(type == "RIV");
  TS_ASSERT(Parameters::ParTypeFromH5Path("river", type));
  TS_ASSERT(type == "RIV");
  TS_ASSERT(Parameters::ParTypeFromH5Path("rIvEr", type));
  TS_ASSERT(type == "RIV");

  TS_ASSERT(Parameters::ParTypeFromH5Path("GENERAL HEAD", type));
  TS_ASSERT(type == "GHB");
  TS_ASSERT(Parameters::ParTypeFromH5Path("general head", type));
  TS_ASSERT(type == "GHB");
  TS_ASSERT(Parameters::ParTypeFromH5Path("gEnErAl HeAd", type));
  TS_ASSERT(type == "GHB");

  TS_ASSERT(Parameters::ParTypeFromH5Path("SPECIFIED HEAD", type));
  TS_ASSERT(type == "CHD");
  TS_ASSERT(Parameters::ParTypeFromH5Path("specified head", type));
  TS_ASSERT(type == "CHD");
  TS_ASSERT(Parameters::ParTypeFromH5Path("sPeCiFiEd HeAd", type));
  TS_ASSERT(type == "CHD");

  TS_ASSERT(Parameters::ParTypeFromH5Path("STREAM", type));
  TS_ASSERT(type == "STR");
  TS_ASSERT(Parameters::ParTypeFromH5Path("stream", type));
  TS_ASSERT(type == "STR");
  TS_ASSERT(Parameters::ParTypeFromH5Path("sTrEaM", type));
  TS_ASSERT(type == "STR");

  TS_ASSERT(Parameters::ParTypeFromH5Path("WELL", type));
  TS_ASSERT(type == "Q");
  TS_ASSERT(Parameters::ParTypeFromH5Path("well", type));
  TS_ASSERT(type == "Q");
  TS_ASSERT(Parameters::ParTypeFromH5Path("wElL", type));
  TS_ASSERT(type == "Q");
}
//------------------------------------------------------------------------------
void ParametersT::testCheckListSubstituteOk ()
{
  TS_ASSERT(Parameters::CheckListSubstituteOk());
  mfLibExp_Exporting() = true;
  TS_ASSERT(!Parameters::CheckListSubstituteOk());
  mfLibExp_Exporting() = false;
}
//------------------------------------------------------------------------------
void ParametersT::testCheckArraySubstituteOk ()
{
  TS_ASSERT(!Parameters::CheckArraySubstituteOk("top1"));
  TS_ASSERT(!Parameters::CheckArraySubstituteOk("bot2"));
  TS_ASSERT(!Parameters::CheckArraySubstituteOk("StartHead2"));
  TS_ASSERT(Parameters::CheckArraySubstituteOk("stuff"));
  mfLibExp_Exporting() = true;
  TS_ASSERT(!Parameters::CheckArraySubstituteOk("stuff"));
  mfLibExp_Exporting() = false;
}
//------------------------------------------------------------------------------
void ParametersT::testFillInParType ()
{
  ParamList& list(GetList());
  list.Clear();
  std::vector<Param> pList;
  pList.push_back(Param("HK_1", -1, "HK", 1));
  pList.push_back(Param("HANI_2", -2, "HANI", 2));
  pList.push_back(Param("VK_3", -3, "VK", 3));
  pList.push_back(Param("VANI_4", -4, "VANI", 4));
  pList.push_back(Param("SS_5", -5, "SS", 5));
  pList.push_back(Param("SY_6", -6, "SY", 6));
  pList.push_back(Param("VKCB_7", -7, "VKCB", 7));
  pList.push_back(Param("RIV_8", -8, "RIV", 8));
  pList.push_back(Param("RCH_9", -9, "RCH", 9));
  pList.push_back(Param("EVT_10", -10, "EVT", 10));
  pList.push_back(Param("WEL_11", -11, "Q", 11));
  pList.push_back(Param("DRN_12", -12, "DRN", 12));
  pList.push_back(Param("DRT_13", -13, "DRT", 13));
  pList.push_back(Param("ETS_14", -14, "ETS", 14));
  pList.push_back(Param("GHB_15", -15, "GHB", 15));
  pList.push_back(Param("CHD_16", -16, "CHD", 16));
  pList.push_back(Param("STR_17", -17, "STR", 17));
  pList.push_back(Param("SFR_18", -18, "SFR", 18));
  pList.push_back(Param("HFB_19", -19, "HFB", 19));
  for (size_t i=0; i<pList.size(); i++)
    list.PushBack(&pList[i]);

  char nam[181] = {"HK_1      "
                   "HANI_2    "
                   "VK_3      "
                   "VANI_4    "
                   "SS_5      "
                   "SY_6      "
                   "VKCB_7    "
                   "RIV_8     "
                   "RCH_9     "
                   "EVT_10    "
                   "WEL_11    "
                   "DRN_12    "
                   "DRT_13    "
                   "ETS_14    "
                   "GHB_15    "
                   "CHD_16    "
                   "STR_17    "
                   "SFR_18    "};
  char ptyp[73];
  ptyp[72] = '\0';
  int nPar(18);
  Parameters::FillInParType(&nPar, &nam[0], &ptyp[0]);
  char ptypBas[73] = {"HK  "
                      "HANI"
                      "VK  "
                      "VANI"
                      "SS  "
                      "SY  "
                      "VKCB"
                      "RIV "
                      "RCH "
                      "EVT "
                      "Q   "
                      "DRN "
                      "DRT "
                      "ETS "
                      "GHB "
                      "CHD "
                      "STR "
                      "SFR "};
  TS_ASSERT(strcmp(&ptyp[0], &ptypBas[0]) == 0);
}

#endif // CXX_TEST


