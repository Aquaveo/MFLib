//------------------------------------------------------------------------------
// FILE      ParamList.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\Parameters\ParamList.h>

#include <map>

#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\PilotPoints.h>

class ParamList::impl
{
public:
  impl() {}
  impl(const impl &rhs) : m_params(rhs.m_params) {};
  const impl& operator=(const impl &rhs)
  {
    if (this != &rhs) m_params = rhs.m_params;
    return *this;
  }

  CStr m_srcFileName;
  std::vector<Param> m_params;
  std::map<CStr, int> m_findByName;
  std::map<double, int> m_findByKey;
  std::map<int, std::vector<double> > m_pilotVals;
  std::map<int, std::vector<int> > m_pilotIsens;
};

//------------------------------------------------------------------------------
/// /brief Constructor
//------------------------------------------------------------------------------
ParamList::ParamList () :
  m_p(new ParamList::impl)
, m_public(0)
{
  m_public = New_ParPub();
} // ParamList::ParamList
//------------------------------------------------------------------------------
/// /brief Destructor
//------------------------------------------------------------------------------
ParamList::~ParamList ()
{
  if (m_p) delete(m_p);
  m_p = 0;
  if (m_public) Delete_ParPub(m_public);
  m_public = 0;
} // ParamList::~ParamList
//------------------------------------------------------------------------------
/// /brief Returns the number of parameters in the list.
//------------------------------------------------------------------------------
size_t ParamList::Size () const
{
  return (m_p->m_params.size());
} // ParamList::Size
//------------------------------------------------------------------------------
/// /brief Clears out the parameter list.
//------------------------------------------------------------------------------
void ParamList::Clear ()
{
  m_p->m_params.clear();
  m_p->m_findByName.clear();
  m_p->m_findByKey.clear();
  m_p->m_pilotVals.clear();
  m_p->m_pilotIsens.clear();
} // ParamList::Size
//------------------------------------------------------------------------------
/// /brief Adds a parameter to the end of the list.
//------------------------------------------------------------------------------
bool ParamList::PushBack (Param *a_)
{
  if (!a_ || a_->m_name.IsEmpty() || a_->m_key > -1)
    return false;
  Param p;
  if (FindByName(a_->m_name, &p))
    return false;
  if (FindByKey(a_->m_key, &p) && a_->m_key != -999)
    return false;
  m_p->m_params.push_back(*a_);
  int pos((int)m_p->m_params.size()-1);
  CStr pnameLower(a_->m_name);
  pnameLower.ToLower();
  m_p->m_findByName.insert(std::make_pair(pnameLower, pos));
  m_p->m_findByKey.insert(std::make_pair(a_->m_key, pos));
  return true;
} // ParamList::PushBack
//------------------------------------------------------------------------------
/// /brief Adds a parameter to the end of the list.
//------------------------------------------------------------------------------
bool ParamList::At (size_t a_i, Param *a_) const
{
  if (!a_)
    return false;
  try
  {
    *a_ = m_p->m_params.at(a_i);
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // ParamList::At
//------------------------------------------------------------------------------
/// /brief Finds a parameter in the list based on the parameter name.
//------------------------------------------------------------------------------
bool ParamList::FindByName (const char *a_name, Param *a_) const
{
  if (!a_)
    return false;

  *a_ = Param();
  CStr n(a_name);
  n.ToLower();
  n.Trim();
  if (m_p->m_findByName.find(n) == m_p->m_findByName.end())
    return false;

  try
  {
    *a_ = m_p->m_params.at(m_p->m_findByName[n]);
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // ParamList::FindByName
//------------------------------------------------------------------------------
/// /brief Finds a parameter in the list based on the parameter name.
//------------------------------------------------------------------------------
bool ParamList::FindByKey (double a_key, Param *a_) const
{
  if (!a_)
    return false;

  *a_ = Param();
  if (m_p->m_findByKey.find(a_key) == m_p->m_findByKey.end())
    return false;

  try
  {
    *a_ = m_p->m_params.at(m_p->m_findByKey[a_key]);
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // ParamList::FindByKey
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool ParamList::IsPilotParName (const char *PNAM,
                                const char *PTYP,
                                int *a_scatIndex)
{
  CStr name(PNAM), type(PTYP);
  bool pilot(false);
  // don't add pilot point parameters
  // they have this type of name sc1v1 or sc3v15...
  if (type.CompareNoCase("HK") == 0 ||
      type.CompareNoCase("RCH") == 0)
  {
    int val, scatIndex(0), valIndex(0);
    name.Replace("sc", "");
    name.Replace("v", " ");
    // now we should just have 2 ints in the string separated by a space
    val = sscanf(name.c_str(), "%d %d", &scatIndex, &valIndex);
    if (2 == val)
    {
      pilot = true;
      if (a_scatIndex != NULL)
        *a_scatIndex = scatIndex;
    }
    else if (a_scatIndex)
    {
      name = PNAM;
      name.Replace("pp", "");
      name.Replace("_", " ");
      // now we should just have 2 ints in the string separated by a space
      val = sscanf(name.c_str(), "%d %d", &scatIndex, &valIndex);
      if (2 == val)
      {
        pilot = true;
        if (a_scatIndex != NULL)
          *a_scatIndex = scatIndex;
      }
    }
  }
  return(pilot);
} // ParamList::IsPilotParName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool ParamList::FindFromPilotName (const char *a_name,
                                   const char *a_type,
                                   Param *a_) const
{
  bool found = false;
  int scatIndex;
  if (IsPilotParName(a_name, a_type, &scatIndex))
  {
    CStr type(a_type);
    type.ToLower();
    std::vector<Param>::const_iterator curr = m_p->m_params.begin();
    for ( ; !found && curr != m_p->m_params.end(); ++curr)
    {
      CStr currType(curr->m_type);
      currType.ToLower();
      if (curr->m_scatIndex == scatIndex && currType == type)
      {
        found = true;
        *a_ = *curr;
      }
    }
  }

  return found;
} // ParamList::FindFromPilotName
//------------------------------------------------------------------------------
/// /brief Sets the value for a pilot point. The name that is passed in will 
/// look something like this "sc1v4". There are 2 indices represented
/// in the name of the pilot point. The first '1' is an index related to the
/// pilot point set. The '4' refers to the position of the point in that set
/// of pilot points.
//------------------------------------------------------------------------------
bool ParamList::SetPilotPtVal (const char *a_name,
                               double a_val,
                               int a_isens)
{
  int ppSetIndex, ptIndex, retval;
  // make sure the name is formated correctly
  // the first two letters are 'sc' then a number then 'v' and then
  // another number
  CStr str(a_name), str1, numberStr;
  str1 = str.Left(2);
  if (str1.CompareNoCase("sc") != 0)
    return false;

  // now find the location of 'v'
  int loc(str.Find("v"));
  if (loc == -1)
    return false;

  // get a string that has the first number
  // get a string to the left of 'v'
  CStr ppSetIndexStr(str.Left(loc));
  // remove the first 2 characters
  ppSetIndexStr.Delete(0, 2);
  // read the index
  retval = sscanf(ppSetIndexStr.c_str(), "%d", &ppSetIndex);
  if (retval == EOF || retval == 0)
    return false;

  // get a string that has the second number
  CStr ptIndexStr(str);
  ptIndexStr.Delete(0, loc+1);
  // read the point index
  retval = sscanf(ptIndexStr.c_str(), "%d", &ptIndex);
  if (retval == EOF || retval == 0)
    return false;
  ptIndex--;

  // get the vector of values for this pilot point set
  {
    std::map<int, std::vector<double> >::iterator it;
    it = m_p->m_pilotVals.find(ppSetIndex);
    if (it == m_p->m_pilotVals.end())
    {
      m_p->m_pilotVals.insert(std::make_pair(ppSetIndex,
                                             std::vector<double>()));
      it = m_p->m_pilotVals.find(ppSetIndex);
    }
    if (ptIndex >= (int)it->second.size())
    {
      it->second.reserve(ptIndex+1);
      int c(static_cast<int>(it->second.size()));
      for (; c<ptIndex+1; c++)
        it->second.push_back(0);
    }
    it->second.at(ptIndex) = a_val;
  }
  // get the vector of values for this pilot point set
  {
    std::map<int, std::vector<int> >::iterator it;
    it = m_p->m_pilotIsens.find(ppSetIndex);
    if (it == m_p->m_pilotIsens.end())
    {
      m_p->m_pilotIsens.insert(std::make_pair(ppSetIndex,
                                              std::vector<int>()));
      it = m_p->m_pilotIsens.find(ppSetIndex);
    }
    if (ptIndex >= (int)it->second.size())
    {
      it->second.reserve(ptIndex+1);
      int c(static_cast<int>(it->second.size()));
      for (; c<ptIndex+1; c++)
        it->second.push_back(0);
    }
    it->second.at(ptIndex) = a_isens;
  }

  return true;
} // ParamList::SetPilotPtVal
//------------------------------------------------------------------------------
/// /brief 
//------------------------------------------------------------------------------
bool ParamList::SetPPValsIsens (int a_scatIdx,
                                const std::vector<double> a_vals,
                                const std::vector<int> a_isens)
{
  if (a_vals.empty() || a_isens.empty())
    return false;

  std::map<int, std::vector<double> >::iterator it;
  it = m_p->m_pilotVals.find(a_scatIdx);
  if (it != m_p->m_pilotVals.end() ||
      a_vals.size() != a_isens.size())
    return false;

  m_p->m_pilotVals.insert(std::make_pair(a_scatIdx, a_vals));
  m_p->m_pilotIsens.insert(std::make_pair(a_scatIdx, a_isens));
  return true;
} // ParamList::SetPilotPtVal
//------------------------------------------------------------------------------
/// /brief Gets the values associated with a pilot point
//------------------------------------------------------------------------------
bool ParamList::GetPilotPtValues (int a_ptSetIndex,
                                  std::vector<double> &a_vals) const
{
  std::map<int, std::vector<double> >::iterator it;
  it = m_p->m_pilotVals.find(a_ptSetIndex);
  if (it == m_p->m_pilotVals.end())
  {
    Param p;
    // get the parameter with this index
    for (size_t i=0; i<m_p->m_params.size(); ++i)
    {
      if (m_p->m_params[i].m_pilotPoints &&
          m_p->m_params[i].m_scatIndex == a_ptSetIndex)
      {
        p = m_p->m_params[i];
        i = m_p->m_params.size();
      }
    }
    CStr fname = GetSourceFile();
    util::StripExtensionFromFilename(fname, fname);
    fname += ".h5";
    PilotPoints pp(fname, p);
    pp.GetPPStartValsReadFromH5IfNeeded(m_p->m_pilotVals[a_ptSetIndex]);
    it = m_p->m_pilotVals.find(a_ptSetIndex);
  }
  a_vals = it->second;
  return true;
} // ParamList::GetPilotPtValues
//------------------------------------------------------------------------------
/// /brief Gets the values associated with a pilot point
//------------------------------------------------------------------------------
bool ParamList::GetPilotPtIsens (int a_ptSetIndex,
                                 std::vector<int> &a_vals) const
{
  std::map<int, std::vector<int> >::iterator it;
  it = m_p->m_pilotIsens.find(a_ptSetIndex);
  if (it == m_p->m_pilotIsens.end())
    return false;
  a_vals = it->second;
  return true;
} // ParamList::GetPilotPtIsens
//------------------------------------------------------------------------------
/// /brief Sets the name of the file where the parameters were read
//------------------------------------------------------------------------------
void ParamList::SetSourceFile (const char *a_fName)
{
  m_p->m_srcFileName = a_fName;
} // ParamList::SetSourceFile
//------------------------------------------------------------------------------
/// /brief Sets the name of the file where the parameters were read
//------------------------------------------------------------------------------
const char * ParamList::GetSourceFile () const
{
  return(m_p->m_srcFileName.c_str());
} // ParamList::GetSourceFile
//------------------------------------------------------------------------------
/// /brief Updates the parameter in the list with the data in the parameter
/// is passed to this function
//------------------------------------------------------------------------------
bool ParamList::UpdateParameter (Param *a_)
{
  if (!a_)
    return false;

  // the key and the name must be unique so don't let that get messed up
  Param p1, p2;
  if (!FindByKey(a_->m_key, &p1))
    return false;
  if (!FindByName(a_->m_name, &p2))
    return false;
  if (p1.m_key != p2.m_key)
    return false;
  if (p1.m_name != p2.m_name)
    return false;

  try
  {
    m_p->m_params.at(m_p->m_findByKey[a_->m_key]) = *a_;
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // ParamList::UpdateParameter
//------------------------------------------------------------------------------
/// /brief returns an unused key
//------------------------------------------------------------------------------
double ParamList::UnusedParamKey ()
{
  double key = -100;
  Param par;
  while (FindByKey(key, &par))
  {
    --key;
  }
  return key;
} // ParamList::UnusedParamKey
//------------------------------------------------------------------------------
/// /brief returns an unused key
//------------------------------------------------------------------------------
bool ParamList::ParamOfTypeExists (const char* a_type)
{
  CStr type(a_type);
  if (type.empty()) return false;
  Param par;
  for (size_t i=0; i<m_p->m_params.size(); ++i)
  {
    if (0 == type.CompareNoCase(m_p->m_params[i].m_type)) return true;
  }
  return false;
} // ParamList::UnusedParamKey

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Parameters\ParamList.t.h>

//------------------------------------------------------------------------------
void ParamListT::setUp ()
{
  m_p = new ParamList;
  Param p("p1", -100.0);
  m_p->PushBack(&p);
  p.m_name = "p2";
  p.m_key = -101;
  m_p->PushBack(&p);
  p.m_name = "p3";
  p.m_key = -102;
  m_p->PushBack(&p);
}
//------------------------------------------------------------------------------
void ParamListT::tearDown ()
{
  if (m_p)
    delete(m_p);
}
//------------------------------------------------------------------------------
void ParamListT::testCreateClass ()
{
  ParamList *l = new ParamList();
  TS_ASSERT(l);
  if (l)
    delete(l);
}
//------------------------------------------------------------------------------
void ParamListT::testSize_PushBack_Clear ()
{
  ParamList l;
  TS_ASSERT(l.Size() == 0);
  Param p;
  TS_ASSERT(!l.PushBack(&p));
  TS_ASSERT(l.Size() == 0);
  p.m_name = "not empty";
  p.m_key = -1;
  TS_ASSERT(l.PushBack(&p));
  TS_ASSERT(l.Size() == 1);
  p.m_name = "NOT EMPTY";
  p.m_key = -1;
  TS_ASSERT(!l.PushBack(&p));
  l.Clear();
  TS_ASSERT(l.Size() == 0);
  p.m_key = -1;
  p.m_name = "p1";
}
//------------------------------------------------------------------------------
void ParamListT::testAt ()
{
  ParamList l;
  Param p;
  TS_ASSERT(!l.At(0, &p));
  l.PushBack(&p);
  TS_ASSERT(!l.At(0, &p));
  p.m_name = "p1";
  p.m_key = -1;
  l.PushBack(&p);
  TS_ASSERT(l.Size() == 1);
  l.PushBack(&p);
  TS_ASSERT(l.Size() == 1);

  p.m_name = "p2";
  p.m_key = -1;
  l.PushBack(&p);
  TS_ASSERT(l.Size() == 1);
  p.m_key = -2;
  l.PushBack(&p);
  TS_ASSERT(l.Size() == 2);
  p = Param();
  TS_ASSERT(l.At(1, &p));
  TS_ASSERT("p2" == p.m_name);
  TS_ASSERT(!l.At(2, &p));
  TS_ASSERT(l.At(0, &p));
  TS_ASSERT("p1" == p.m_name);
}
//------------------------------------------------------------------------------
void ParamListT::testFindByName ()
{
  Param p;

  TS_ASSERT(!m_p->FindByName("p1", NULL));
  TS_ASSERT(!m_p->FindByName("", &p));
  TS_ASSERT(!m_p->FindByName("crap", &p));
  TS_ASSERT(p.m_key == 0);
  TS_ASSERT(m_p->FindByName("p1", &p));
  TS_ASSERT(p.m_key == -100);
  TS_ASSERT(m_p->FindByName("p2", &p));
  TS_ASSERT(p.m_key == -101);
  TS_ASSERT(m_p->FindByName("p3", &p));
  TS_ASSERT(p.m_key == -102);
  TS_ASSERT(!m_p->FindByName("p4", &p));
  TS_ASSERT(p.m_key == 0);
  TS_ASSERT(m_p->FindByName("P1", &p));
  TS_ASSERT(p.m_key == -100);
}
//------------------------------------------------------------------------------
void ParamListT::testFindByKey ()
{
  Param p;

  TS_ASSERT(!m_p->FindByKey(-100, NULL));
  TS_ASSERT(!m_p->FindByKey(0, &p));
  TS_ASSERT(!m_p->FindByKey(1, &p));
  TS_ASSERT(p.m_name == "");
  TS_ASSERT(m_p->FindByKey(-100, &p));
  TS_ASSERT(p.m_name == "p1");
  TS_ASSERT(m_p->FindByKey(-101, &p));
  TS_ASSERT(p.m_name == "p2");
  TS_ASSERT(m_p->FindByKey(-102, &p));
  TS_ASSERT(p.m_name == "p3");
  TS_ASSERT(!m_p->FindByKey(-103, &p));
  TS_ASSERT(p.m_name == "");
}
//------------------------------------------------------------------------------
void ParamListT::testSetPilotPtVal ()
{
  ParamList pl;
  TS_ASSERT(!pl.SetPilotPtVal("crap", 0, 1));
  TS_ASSERT(!pl.SetPilotPtVal("scv", 0, 1));
  TS_ASSERT(!pl.SetPilotPtVal("scv1", 0, 1));
  TS_ASSERT(!pl.SetPilotPtVal("sc1v", 0, 1));
  TS_ASSERT(pl.SetPilotPtVal("sc1v4", 2, 1));
  TS_ASSERT(pl.SetPilotPtVal("sc1v5", 3, 1));
  TS_ASSERT(pl.m_p->m_pilotVals.size() == 1);
  TS_ASSERT(pl.m_p->m_pilotIsens.size() == 1);
  TS_ASSERT(pl.SetPilotPtVal("sc5v3", 2, 2));
  TS_ASSERT(pl.m_p->m_pilotVals.size() == 2);
  TS_ASSERT(pl.m_p->m_pilotIsens.size() == 2);
  {
    std::map<int, std::vector<double> >::iterator it;
    it = pl.m_p->m_pilotVals.begin();
    TS_ASSERT(it->first == 1);
    TS_ASSERT(it->second.size() == 5);
    TS_ASSERT(it->second.at(0) == 0);
    TS_ASSERT(it->second.at(1) == 0);
    TS_ASSERT(it->second.at(2) == 0);
    TS_ASSERT(it->second.at(3) == 2);
    TS_ASSERT(it->second.at(4) == 3);
    it++;
    TS_ASSERT(it->first == 5);
    TS_ASSERT(it->second.size() == 3);
    TS_ASSERT(it->second.at(0) == 0);
    TS_ASSERT(it->second.at(1) == 0);
    TS_ASSERT(it->second.at(2) == 2);
  }
  {
    std::map<int, std::vector<int> >::iterator it;
    it = pl.m_p->m_pilotIsens.begin();
    TS_ASSERT(it->first == 1);
    TS_ASSERT(it->second.size() == 5);
    TS_ASSERT(it->second.at(0) == 0);
    TS_ASSERT(it->second.at(1) == 0);
    TS_ASSERT(it->second.at(2) == 0);
    TS_ASSERT(it->second.at(3) == 1);
    TS_ASSERT(it->second.at(4) == 1);
    it++;
    TS_ASSERT(it->first == 5);
    TS_ASSERT(it->second.size() == 3);
    TS_ASSERT(it->second.at(0) == 0);
    TS_ASSERT(it->second.at(1) == 0);
    TS_ASSERT(it->second.at(2) == 2);
  }
}
//------------------------------------------------------------------------------
void ParamListT::testSetPPValsIsens ()
{
  ParamList pl;
  std::vector<double> vals;
  std::vector<int> isens;
  // pass empty vectors
  TS_ASSERT(false == pl.SetPPValsIsens(1, vals, isens));
  vals.assign(3, 5);
  // pass vectors of different sizes
  TS_ASSERT(false == pl.SetPPValsIsens(1, vals, isens));
  isens.assign(4, 1);
  TS_ASSERT(false == pl.SetPPValsIsens(1, vals, isens));
  isens.pop_back();
  // this should work
  TS_ASSERT(true == pl.SetPPValsIsens(1, vals, isens));

  // try to put a scat index in that already exists
  TS_ASSERT(false == pl.SetPPValsIsens(1, vals, isens));
}
//------------------------------------------------------------------------------
void ParamListT::testGetPilotPtValues ()
{
  ParamList pl;
  TS_ASSERT(pl.SetPilotPtVal("sc1v5", 3, 1));
  TS_ASSERT(pl.m_p->m_pilotVals.size() == 1);
  TS_ASSERT(pl.SetPilotPtVal("sc5v3", 2, 1));
  TS_ASSERT(pl.m_p->m_pilotVals.size() == 2);
  double md[5] = {0,0,0,0,3};
  std::vector<double> d;
  TS_ASSERT(pl.GetPilotPtValues(1, d));
  TS_ASSERT(d.size() == 5);
  for (int i=0; i<5; i++)
    TS_ASSERT_EQUALS(md[i], d.at(i));
  TS_ASSERT(pl.GetPilotPtValues(5, d));
  TS_ASSERT(d.size() == 3);
  md[2] = 2;
  for (int i=0; i<3; i++)
    TS_ASSERT_EQUALS(md[i], d.at(i));
}
//------------------------------------------------------------------------------
void ParamListT::testGetPilotPtIsens ()
{
  ParamList pl;
  TS_ASSERT(pl.SetPilotPtVal("sc1v5", 3, 1));
  TS_ASSERT(pl.m_p->m_pilotIsens.size() == 1);
  TS_ASSERT(pl.SetPilotPtVal("sc5v3", 2, 1));
  TS_ASSERT(pl.m_p->m_pilotIsens.size() == 2);
  double md[5] = {0,0,0,0,1};
  std::vector<int> d;
  TS_ASSERT(pl.GetPilotPtIsens(1, d));
  TS_ASSERT(d.size() == 5);
  for (int i=0; i<5; i++)
    TS_ASSERT_EQUALS(md[i], d.at(i));
  TS_ASSERT(pl.GetPilotPtIsens(5, d));
  TS_ASSERT(d.size() == 3);
  md[2] = 1;
  for (int i=0; i<3; i++)
    TS_ASSERT_EQUALS(md[i], d.at(i));
}
//------------------------------------------------------------------------------
void ParamListT::testUpdateParameter ()
{
  ParamList pL;
  Param *nullPtr(NULL), p;

  TS_ASSERT(!pL.UpdateParameter(nullPtr));
  TS_ASSERT(!pL.UpdateParameter(&p));
  p.m_key = -10;
  p.m_name = "stuff";
  p.m_value = 10.0;
  pL.PushBack(&p);
  p.m_value = 9.0;
  p.m_key = -9;
  TS_ASSERT(!pL.UpdateParameter(&p));
  p.m_key = -10;
  p.m_name = "stuff1";
  TS_ASSERT(!pL.UpdateParameter(&p));
  p.m_name = "stuff";
  TS_ASSERT(pL.UpdateParameter(&p));
  TS_ASSERT(pL.FindByKey(-10, &p));
  TS_ASSERT_EQUALS(p.m_value, 9.0);
}
//------------------------------------------------------------------------------
void ParamListT::testIsPilotParName ()
{
  const char *p0="sc2v45", *t0="RCH";
  const char *p1="sc1v1",  *t1="HK";
  const char *p2="sc1v1",  *t2="HANI";
  const char *p3="hk1",    *t3="HK";

  TS_ASSERT(ParamList::IsPilotParName(p0, t0));
  TS_ASSERT(ParamList::IsPilotParName(p1, t1));
  TS_ASSERT(!ParamList::IsPilotParName(p2, t2));
  TS_ASSERT(!ParamList::IsPilotParName(p3, t3));
  int scatterIndex;
  TS_ASSERT(ParamList::IsPilotParName(p0, t0, &scatterIndex));
  TS_ASSERT_EQUALS(2, scatterIndex);
  TS_ASSERT(ParamList::IsPilotParName(p1, t1, &scatterIndex));
  TS_ASSERT_EQUALS(1, scatterIndex);
}
//------------------------------------------------------------------------------
void ParamListT::testUnusedParamKey ()
{
  double key(-100);
  ParamList pL;
  TS_ASSERT_EQUALS(key, pL.UnusedParamKey());
  Param p("par", key);
  pL.PushBack(&p);
  TS_ASSERT_EQUALS(-101, pL.UnusedParamKey());
}
//------------------------------------------------------------------------------
void ParamListT::testParmOfTypeExists ()
{
  ParamList pL;
  TS_ASSERT(!pL.ParamOfTypeExists("HK"));
  Param p("par", -1, "hk");
  pL.PushBack(&p);
  TS_ASSERT(pL.ParamOfTypeExists("hk"));
  TS_ASSERT(pL.ParamOfTypeExists("HK"));
}
#endif
