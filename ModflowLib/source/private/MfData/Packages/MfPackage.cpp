//------------------------------------------------------------------------------
// FILE      MfPackage.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\MfData\Packages\MfPackage.h>

#include <map>
#include <set>

#include <private\util\util.h>

using namespace MfData;

class dataPack
{
public:
  dataPack(): i(0),f(0),d(0),c(0) {}
  ~dataPack() {}
  dataPack(const dataPack &rhs) : i(rhs.i),f(rhs.f),d(rhs.d),c(rhs.c) {}
  const dataPack& operator=(const dataPack &rhs)
  {
    if (this != &rhs)
    {
      i = rhs.i;
      f = rhs.f;
      d = rhs.d;
      c = rhs.c;
    }
    return(*this);
  }
  const int* i;
  const float* f;
  const double* d;
  const char*  c;
};
////////////////////////////////////////////////////////////////////////////////
/// class MfPackage::impl
////////////////////////////////////////////////////////////////////////////////
class MfPackage::impl 
{
public:
  impl(const char *a_);
  impl(const impl &rhs);
  const impl& operator=(const impl &rhs);

  void RemoveField(const char* a_field)
  {
    m_data.erase(a_field);
  }

  template <class T>
  bool SetField(const char *a_field,
                const T *a_);
  template <class T>
  bool GetField(const char *a_field,
                const T **a_) const;

  void SetStructVal(const int* a_, dataPack &a_p) const
  {
    a_p.i=a_;
    a_p.f=0;
    a_p.d=0;
    a_p.c=0;
  }
  void SetStructVal(const float* a_, dataPack &a_p) const
  {
    a_p.i=0;
    a_p.f=a_;
    a_p.d=0;
    a_p.c=0;
  }
  void SetStructVal(const double* a_, dataPack &a_p) const
  {
    a_p.i=0;
    a_p.f=0;
    a_p.d=a_;
    a_p.c=0;
  }
  void SetStructVal(const char* a_, dataPack &a_p) const
  {
    a_p.i=0;
    a_p.f=0;
    a_p.d=0;
    a_p.c=a_;
  }
  void GetStructVal(const int** a_, const dataPack &a_p) const
  {
    *a_=a_p.i;
  }
  void GetStructVal(const float** a_, const dataPack &a_p) const
  {
    *a_=a_p.f;
  }
  void GetStructVal(const double** a_, const dataPack &a_p) const
  {
    *a_=a_p.d;
  }
  void GetStructVal(const char** a_, const dataPack &a_p) const
  {
    *a_=a_p.c;
  }

  CStr m_packName, m_lineNumber;
  std::map<CStr, dataPack > m_data;
  std::vector<CStr> m_stringToWrite;
  std::vector<CStr> m_strDescription;
};


//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
MfPackage::MfPackage (const char *a_) :
m_p(new MfPackage::impl(a_))
{
} // MfPackage::MfPackage
//------------------------------------------------------------------------------
/// \brief Copy constructor.
//------------------------------------------------------------------------------
MfPackage::MfPackage (const MfPackage &rhs) :
m_p(new MfPackage::impl(*rhs.m_p))
{
} // MfPackage::MfPackage
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
MfPackage::~MfPackage ()
{
  if (m_p)
    delete(m_p);
} // MfPackage::~MfPackage
//------------------------------------------------------------------------------
/// \brief Operator=.
//------------------------------------------------------------------------------
const MfPackage &MfPackage::operator = (const MfPackage &rhs)
{
  if (this != &rhs)
  {
    *m_p = *rhs.m_p;
  }
  return(*this);
} // MfPackage::operator =
//------------------------------------------------------------------------------
/// \brief Gets the name of the package.
//------------------------------------------------------------------------------
CStr MfPackage::PackageName () const
{
  return (m_p->m_packName);
} // MfPackage::PackageName
//------------------------------------------------------------------------------
/// \brief Removes a field from the class
//------------------------------------------------------------------------------
void MfPackage::RemoveField (const char* a_field)
{
  m_p->RemoveField(a_field);
} // MfPackage::RemoveField
//------------------------------------------------------------------------------
/// \brief Stores a pointer to MODFLOW data. If the field already exists
/// then it returns false.
//------------------------------------------------------------------------------
bool MfPackage::SetField (const char* a_field,
                          const float *a_)
{
  return (m_p->SetField(a_field, a_));
} // MfPackage::SetField
bool MfPackage::SetField (const char* a_field,
                          const double *a_)
{
  return (m_p->SetField(a_field, a_));
} // MfPackage::SetField
bool MfPackage::SetField (const char* a_field,
                          const int *a_)
{
  return (m_p->SetField(a_field, a_));
} // MfPackage::SetField
bool MfPackage::SetField (const char* a_field,
                          const char *a_)
{
  return (m_p->SetField(a_field, a_));
} // MfPackage::SetField
//------------------------------------------------------------------------------
/// \brief Gets a pointer to MODFLOW data. If the field doesn't exist
/// then it returns false.
//------------------------------------------------------------------------------
bool MfPackage::GetField (const char* a_field,
                          const float **a_) const
{
  return(m_p->GetField(a_field, a_));
} // MfPackage::GetField
bool MfPackage::GetField (const char* a_field,
                          const double **a_) const
{
  return(m_p->GetField(a_field, a_));
} // MfPackage::GetField
bool MfPackage::GetField (const char* a_field,
                          const int **a_) const
{
  return(m_p->GetField(a_field, a_));
} // MfPackage::GetField
bool MfPackage::GetField (const char* a_field,
                          const char **a_) const
{
  return(m_p->GetField(a_field, a_));
} // MfPackage::GetField
//------------------------------------------------------------------------------
/// \brief accesses vector
//------------------------------------------------------------------------------
std::vector<CStr> MfPackage::FieldNames ()
{
  std::vector<CStr> v;
  std::map<CStr, dataPack>::iterator it = m_p->m_data.begin();
  for (; it != m_p->m_data.end(); ++it)
    v.push_back(it->first);
  return v;
} // MfPackage::FieldNames
//------------------------------------------------------------------------------
/// \brief accesses vector
//------------------------------------------------------------------------------
std::vector<CStr>& MfPackage::StringsToWrite ()
{
  return m_p->m_stringToWrite;
} // MfPackage::StringsToWrite
//------------------------------------------------------------------------------
/// \brief accesses vector
//------------------------------------------------------------------------------
std::vector<CStr>& MfPackage::StringDescriptions ()
{
  return m_p->m_strDescription;
} // MfPackage::StringDescriptions
//------------------------------------------------------------------------------
/// \brief sets member
//------------------------------------------------------------------------------
void MfPackage::SetLineNumber (const char* a_)
{
  m_p->m_lineNumber = a_;
} // MfPackage::SetLineNumber
//------------------------------------------------------------------------------
/// \brief sets member
//------------------------------------------------------------------------------
const char* MfPackage::GetLineNumber () const
{
  return(LPCTSTR)m_p->m_lineNumber;
} // MfPackage::GetLineNumber

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
MfPackage::impl::impl (const char *a_) :
m_packName(a_)
{
} // MfPackage::impl::impl
//------------------------------------------------------------------------------
/// \brief Copy constructor.
//------------------------------------------------------------------------------
MfPackage::impl::impl (const MfPackage::impl &rhs) :
m_packName(rhs.m_packName),
m_lineNumber(rhs.m_lineNumber),
m_data(rhs.m_data),
m_stringToWrite(rhs.m_stringToWrite),
m_strDescription(rhs.m_strDescription)
{
} // MfPackage::impl::impl
//------------------------------------------------------------------------------
/// \brief Operator=.
//------------------------------------------------------------------------------
const MfPackage::impl &MfPackage::impl::operator= (const MfPackage::impl &rhs)
{
  if (this != &rhs)
  {
    m_packName = rhs.m_packName;
    m_lineNumber = rhs.m_lineNumber;
    m_data = rhs.m_data;
    m_stringToWrite = rhs.m_stringToWrite;
    m_strDescription = rhs.m_strDescription;
  }
  return (*this);
} // MfPackage::impl::operator=
//------------------------------------------------------------------------------
/// \brief Template for setting the data from a field.
//------------------------------------------------------------------------------
template <class T>
bool MfPackage::impl::SetField (const char *a_field,
                                const T *a_)
{
  if (!a_)
    return false;

  CStr str(a_field);
  std::map<CStr, dataPack >::iterator it;
  it = m_data.find(str);
  dataPack p;
  SetStructVal(a_, p);
  if (it == m_data.end())
    m_data.insert(std::make_pair(str, p));
  else
    m_data[str] = p;
  return true;
} // MfPackage::impl::SetField
//------------------------------------------------------------------------------
/// \brief Template for setting the data from a field.
//------------------------------------------------------------------------------
template <class T>
bool MfPackage::impl::GetField (const char *a_field,
                                const T **a_) const
{
  if (!a_)
    return false;

  CStr str(a_field);
  std::map<CStr, dataPack >::const_iterator it;
  it = m_data.find(str);
  if (it == m_data.end())
    return false;
  std::pair<int*,float*> p;
  GetStructVal(a_, it->second);
  return true;
} // MfPackage::impl::GetField
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
TmpPackageNameChanger::TmpPackageNameChanger (MfPackage* a_pack,
                                              const char *a_tmpName) :
m_pack(a_pack)
{
  if (m_pack)
  {
    m_origName = m_pack->PackageName();
    m_pack->m_p->m_packName = a_tmpName;
  }
} // TmpPackageNameChanger::TmpPackageNameChanger
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
TmpPackageNameChanger::~TmpPackageNameChanger ()
{
  if (m_pack)
  {
    m_pack->m_p->m_packName = m_origName;
  }
} // TmpPackageNameChanger::~TmpPackageNameChanger

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\Packages\MfPackage.t.h>

//------------------------------------------------------------------------------
void MfPackageT::setUp ()
{
  m_p = new MfPackage("test");
  m_f[0] = 0;
  m_f[1] = 1;
  m_f[2] = 2;
  m_i[0] = 0;
  m_i[1] = 1;
  m_i[2] = 2;
  m_p->SetField("f0", &m_f[0]);
  m_p->SetField("f1", &m_f[1]);
  m_p->SetField("f2", &m_f[2]);
  m_p->SetField("i0", &m_i[0]);
  m_p->SetField("i1", &m_i[1]);
  m_p->SetField("i2", &m_i[2]);
}
//------------------------------------------------------------------------------
void MfPackageT::tearDown ()
{
  if (m_p)
    delete(m_p);
}
//------------------------------------------------------------------------------
void MfPackageT::CheckAgainstMember (MfData::MfPackage *a_)
{
  const float *fp;
  const int *ip;
  CStr str(a_->PackageName());
  TS_ASSERT(str == "test");
  TS_ASSERT(a_->GetField("f0", &fp));
  TS_ASSERT(fp == &m_f[0]);
  TS_ASSERT(a_->GetField("f1", &fp));
  TS_ASSERT(fp == &m_f[1]);
  TS_ASSERT(a_->GetField("f2", &fp));
  TS_ASSERT(fp == &m_f[2]);
  TS_ASSERT(a_->GetField("i0", &ip));
  TS_ASSERT(ip == &m_i[0]);
  TS_ASSERT(a_->GetField("i1", &ip));
  TS_ASSERT(ip == &m_i[1]);
  TS_ASSERT(a_->GetField("i2", &ip));
  TS_ASSERT(ip == &m_i[2]);
}
//------------------------------------------------------------------------------
void MfPackageT::testCopyConstructor ()
{
  MfPackage p1(*m_p);
  CheckAgainstMember(&p1);
}
//------------------------------------------------------------------------------
void MfPackageT::testOperatorEqual ()
{
  MfPackage p1("");
  p1 = *m_p;
  CheckAgainstMember(&p1);
}
//------------------------------------------------------------------------------
void MfPackageT::testPackageName ()
{
  MfPackage *p = new MfPackage("stuff");
  TS_ASSERT(p);
  if (p)
  {
    CStr str(p->PackageName());
    TS_ASSERT(str == "stuff");
  }
}
//------------------------------------------------------------------------------
void MfPackageT::testSetField ()
{
  MfPackage p("stuff");
  int *iptr(0);
  float *fptr(0);
  TS_ASSERT(!p.SetField("1", fptr));
  TS_ASSERT(!p.SetField("1", iptr));
  float f(1), f1(2);
  TS_ASSERT(p.SetField("1", &f));
  TS_ASSERT(p.SetField("1", &f1));
  TS_ASSERT(p.SetField("2", &f1));

  int i(1), i2;
  TS_ASSERT(p.SetField("1", &i));
  TS_ASSERT(p.SetField("2", &i));
  TS_ASSERT(p.SetField("3", &i2));
}
//------------------------------------------------------------------------------
void MfPackageT::testGetField ()
{
  const float **fpp;
  const float *fp;
  fpp = 0;
  TS_ASSERT(!m_p->GetField("1", fpp));
  TS_ASSERT(!m_p->GetField("doesn't exist", fpp));
  TS_ASSERT(!m_p->GetField("doesn't exist", &fp));
  TS_ASSERT(m_p->GetField("f0", &fp));
  TS_ASSERT(&m_f[0] == fp);
  TS_ASSERT(m_p->GetField("f1", &fp));
  TS_ASSERT(&m_f[1] == fp);
  TS_ASSERT(m_p->GetField("f2", &fp));
  TS_ASSERT(&m_f[2] == fp);

  const int **ipp;
  const int *ip;
  ipp = 0;
  TS_ASSERT(!m_p->GetField("1", ipp));
  TS_ASSERT(!m_p->GetField("doesn't exist", ipp));
  TS_ASSERT(!m_p->GetField("doesn't exist", &ip));
  TS_ASSERT(m_p->GetField("i0", &ip));
  TS_ASSERT(&m_i[0] == ip);
  TS_ASSERT(m_p->GetField("i1", &ip));
  TS_ASSERT(&m_i[1] == ip);
  TS_ASSERT(m_p->GetField("i2", &ip));
  TS_ASSERT(&m_i[2] == ip);
}
#if 0
//------------------------------------------------------------------------------
void MfPackageT::testStoreArrayPtr ()
{
  MfPackage p("DIS");
  float *f1(NULL);
  TS_ASSERT(!p.SetField(f1, "stuff"));
  std::vector<float> f(10, 0);
  TS_ASSERT(p.StoreArrayPtr(&f.at(0), "Top1"));
  TS_ASSERT(!p.StoreArrayPtr(&f.at(0), "Top1"));

  std::vector<int> i(10, 0);
  // won't work because an array already exists with this name
  TS_ASSERT(!p.StoreArrayPtr(&i.at(0), "Top1"));
  TS_ASSERT(p.StoreArrayPtr(&i.at(0), "IBound1"));
  TS_ASSERT(!p.StoreArrayPtr(&i.at(0), "IBound1"));
}
//------------------------------------------------------------------------------
void MfPackageT::testGetArrayPtr ()
{
  MfPackage p("DIS");
  std::vector<float> f(10, 0);
  TS_ASSERT(p.StoreArrayPtr(&f.at(0), "Top1"));
  float **fptr;
  fptr = NULL;
  TS_ASSERT(!p.GetArrayPtr(fptr, "Top1"));
  float *p1;
  TS_ASSERT(!p.GetArrayPtr(&p1, "stuff"));
  TS_ASSERT(p.GetArrayPtr(&p1, "Top1"));
  TS_ASSERT(p1 == &f.at(0));
}
//------------------------------------------------------------------------------
void MfPackageT::testStoreValue ()
{
  MfPackage p("DIS");
  int i(3);
  float f((float)2.1);
  TS_ASSERT(p.StoreValue(f, "field1"));
  TS_ASSERT(!p.StoreValue(f, "field1"));

  TS_ASSERT(!p.StoreValue(i, "field1"));
  TS_ASSERT(p.StoreValue(i, "field2"));
  TS_ASSERT(!p.StoreValue(i, "field2"));
}
//------------------------------------------------------------------------------
void MfPackageT::testGetValue ()
{
  MfPackage p("DIS");
  int i(3);
  float f((float)2.1);
  TS_ASSERT(p.StoreValue(f, "field1"));
  TS_ASSERT(p.StoreValue(i, "field2"));

  int i1;
  float f1;
  float *f3(NULL);
  int *i3(NULL);

  // test sending in NULL
  TS_ASSERT(!p.GetValue(f3, "field1"));
  TS_ASSERT(!p.GetValue(i3, "field2"));

  TS_ASSERT(!p.GetValue(&f1, "wrong"));
  TS_ASSERT(!p.GetValue(&i1, "wrong"));
  TS_ASSERT(p.GetValue(&f1, "field1"));
  TS_ASSERT(p.GetValue(&i1, "field2"));
  TS_ASSERT_EQUALS(f, f1);
  TS_ASSERT_EQUALS(i, i1);
}
#endif

#endif
