//------------------------------------------------------------------------------
// FILE      TxtExporter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\TxtExporter.h>

#include <map>
#include <set>
#include <fstream>
#include <sstream>

#include <private\MfData\MfExport\private\ExpGmsH5.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\util\util.h>

#define TESTBASE "4814dfa0-51de-11dd-ae16-0800200c9a66"

class TxtExporterT;
class TxtExporter::impl
{
friend TxtExporterT;
public:
  impl(const char * a_) :
      m_base(a_),
      m_lastStream(0),
      m_lastType(""),
      m_map(),
      m_AtLeastOneTransientSPExists(false),
      m_steadyStateStressPeriods(),
      m_ext() {}
  ~impl();

  void SetBaseFileName(const char *a_base) { m_base = a_base; }
  const char * const GetBaseFileName() { return m_base; }
  bool WriteLineToFile(const char *a_type,
                       const char *a_line);
  bool WriteStringToFile(const char *a_type,
                        const char *a_string);
  bool IsTypeSupported(const char *a_type);
  bool FileTypeExists(const char *a_type);
  CStr GetExtension(const char *a_type);
  void SetTypesToExtensions(const std::map<CStr, CStr> &a_);
  void GetFileContents(const char *a_type,
                       CStr &a_str);
  bool WriteLinesAndDescriptionsToFile (const char* a_type,
                                        std::vector<CStr>& a_lines,
                                        std::vector<CStr>& a_desc);
  bool ClearFile (const char* const a_);
  bool m_AtLeastOneTransientSPExists;
  std::set<int> m_steadyStateStressPeriods;
  std::map<CStr, int> m_lineLen;

private:
  std::ostream *GetStream(const char *a_type);
  std::ostream *GetNewStream(const char *a_);
  CStr GetFileName(const char *a_type);

  CStr m_base;
  std::ostream *m_lastStream;
  CStr m_lastType;
  std::map<CStr, std::ostream *> m_map;
  std::map<CStr, CStr> m_ext;
};

//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
TxtExporter::TxtExporter (const char * a_) :
  m_p(new TxtExporter::impl(a_))
, m_public(0)
{
  m_public = New_ExpGmsH5Public();
} // TxtExporter::TxtExporter
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
TxtExporter::~TxtExporter ()
{
  try
  {
    if (m_p) delete(m_p);
    m_p = 0;
    if (m_public) Delete_ExpGmsH5Public(m_public);
    m_public = 0;
  }
  catch (...) {}
} // TxtExporter::~TxtExporter
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::WriteLineToFile (const char *a_type,
                                   const char *a_line)
{
  return(m_p->WriteLineToFile(a_type, a_line));
} // TxtExporter::WriteLineToFile
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::WriteLineToFile (const char *a_type,
                                   const std::string& a_line)
{
  return(m_p->WriteLineToFile(a_type, a_line.c_str()));
} // TxtExporter::WriteLineToFile
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::WriteStringToFile (const char *a_type,
                                   const char *a_line)
{
  return(m_p->WriteStringToFile(a_type, a_line));
} // TxtExporter::WriteStringToFile
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::WriteLinesAndDescriptionsToFile (const char* a_type,
                                                   std::vector<CStr>& a_lines,
                                                   std::vector<CStr>& a_desc)
{
  return(m_p->WriteLinesAndDescriptionsToFile(a_type, a_lines, a_desc));
} // TxtExporter::WriteLinesAndDescriptionsToFile
//------------------------------------------------------------------------------
/// \brief sets the base filename
//------------------------------------------------------------------------------
void TxtExporter::SetBaseFileName (const char *a_base)
{
  m_p->SetBaseFileName(a_base);
} // TxtExporter::SetBaseFileName
//------------------------------------------------------------------------------
/// \brief gets the base filename
//------------------------------------------------------------------------------
const char * const TxtExporter::GetBaseFileName ()
{
  return (m_p->GetBaseFileName());
} // TxtExporter::GetBaseFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TxtExporter::IsTypeSupported (const char *a_type)
{
  return m_p->IsTypeSupported(a_type);
}
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TxtExporter::FileTypeExists (const char *a_type)
{
  return m_p->FileTypeExists(a_type);
}
//------------------------------------------------------------------------------
/// \brief gets the standard extension based on the passed in type
//------------------------------------------------------------------------------
CStr TxtExporter::GetExtension (const char *a_type)
{
  return(m_p->GetExtension(a_type));
} // TxtExporter::GetExtension
//------------------------------------------------------------------------------
/// \brief Sets the allowed types and the matching extensions
//------------------------------------------------------------------------------
void TxtExporter::SetTypesToExtensions (const std::map<CStr, CStr> &a_)
{
  m_p->SetTypesToExtensions(a_);
} // TxtExporter::SetTypesToExtensions
//------------------------------------------------------------------------------
/// \brief Gets the stream from the type. Used in testing.
//------------------------------------------------------------------------------
void TxtExporter::GetFileContents (const char *a_type,
                                   CStr &a_str)
{
  m_p->GetFileContents(a_type, a_str);
} // TxtExporter::GetFileContents
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool& TxtExporter::AtLeastOneTransientSPExists ()
{
  return m_p->m_AtLeastOneTransientSPExists;
} // TxtExporter::AtLeastOneTransientSPExists
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::set<int>& TxtExporter::SetOfSteadyStateStressPeriods ()
{
  return m_p->m_steadyStateStressPeriods;
} // TxtExporter::SetOfSteadyStateStressPeriods
bool TxtExporter::FirstStressIsSteadyState ()
{
  return(m_p->m_steadyStateStressPeriods.find(1) !=
         m_p->m_steadyStateStressPeriods.end());
} // TxtExporter::FirstStressIsSteadyState
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int TxtExporter::GetMaxLineLengthFromType (const char * const a_)
{
  int rval(0);
  if (m_p->m_lineLen.find(a_) != m_p->m_lineLen.end())
    rval = m_p->m_lineLen[a_];
  return rval;
} // TxtExporter::GetMaxLineLengthFromType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TxtExporter::ClearFile (const char * const a_)
{
  return m_p->ClearFile(a_);
} // TxtExporter::ClearFile

//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
TxtExporter::impl::~impl ()
{
  try
  {
    std::map<CStr, std::ostream*>::iterator it(m_map.begin());
    while (it != m_map.end())
    {
      if (it->second)
        delete(it->second);
      it->second = 0;
      it++;
    }
  }
  catch (...) {}
} // TxtExporter::impl::~impl
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::impl::WriteLineToFile (const char *a_type,
                                         const char *a_line)
{
  std::ostream *os(GetStream(a_type));
  if (!os)
    return false;
  (*os) << a_line << std::endl;
  return true;
} // TxtExporter::impl::WriteLineToFile
//------------------------------------------------------------------------------
/// \brief Writes a line to the specified file
//------------------------------------------------------------------------------
bool TxtExporter::impl::WriteStringToFile (const char *a_type,
                                           const char *a_string)
{
  std::ostream *os(GetStream(a_type));
  if (!os)
    return false;
  (*os) << a_string;
  return true;
} // TxtExporter::impl::WriteStringToFile
//------------------------------------------------------------------------------
/// \brief Gets the ostream from the map
//------------------------------------------------------------------------------
std::ostream *TxtExporter::impl::GetStream (const char *a_type)
{
  if (m_lastType == a_type && m_lastStream)
    return m_lastStream;

  std::ostream *ret(0);
  m_lastType = a_type;
  std::map<CStr, std::ostream*>::iterator it(m_map.find(CStr(a_type)));
  if (it == m_map.end())
  {
    CStr name = GetFileName(a_type);
    std::ostream *f(GetNewStream(name));
    if (f)
    {
      m_map.insert(std::make_pair(CStr(a_type), f));
    }
    ret = f;
  }
  else
    ret = it->second;
  m_lastStream = ret;
  return (ret);
} // TxtExporter::impl::GetStream
//------------------------------------------------------------------------------
/// \brief Gets the file name for a given type
//------------------------------------------------------------------------------
CStr TxtExporter::impl::GetFileName (const char *a_type)
{
    CStr name, ext(GetExtension(a_type));
    name.Format("%s.%s", m_base, ext);
    return name;
} // TxtExporter::impl::GetFileName
//------------------------------------------------------------------------------
/// \brief Gets a new ostream for use. If the base name = TESTBASE then we 
/// return a std::ostringstream if not then we return a std::ofstream
//------------------------------------------------------------------------------
std::ostream *TxtExporter::impl::GetNewStream (const char *a_)
{
  std::ostream *f(0);
  if (m_base == TESTBASE)
  {
    f = new std::ostringstream;
  }
  else
  {
    f = new std::ofstream(a_);
  }
  return f;
} // TxtExporter::impl::GetNewStream
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TxtExporter::impl::IsTypeSupported (const char *a_type)
{
  return m_ext.find(a_type) != m_ext.end();
} // TxtExporter::impl::IsTypeSupported
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TxtExporter::impl::FileTypeExists (const char *a_type)
{
  std::map<CStr, std::ostream*>::iterator it(m_map.find(CStr(a_type)));
  if (it == m_map.end())
    return false;
  return true;
} //TxtExporter::impl::FileTypeExists
//------------------------------------------------------------------------------
/// \brief Gets the extension from the type of file
//------------------------------------------------------------------------------
CStr TxtExporter::impl::GetExtension (const char *a_type)
{
  CStr result;
  std::map<CStr, CStr>::iterator it(m_ext.find(a_type));
  if (it != m_ext.end())
    result = it->second;
  else
    result = a_type;
  return result.ToLower();
} // TxtExporter::impl::GetExtension
//------------------------------------------------------------------------------
/// \brief Sets the allowed types and the matching extensions
//------------------------------------------------------------------------------
void TxtExporter::impl::SetTypesToExtensions (const std::map<CStr, CStr> &a_)
{
  m_ext = a_;
} // TxtExporter::impl::SetTypesToExtensions
//------------------------------------------------------------------------------
/// \brief Gets the stream from the type. Used in testing.
//------------------------------------------------------------------------------
void TxtExporter::impl::GetFileContents (const char *a_type,
                                         CStr &a_str)
{
  std::map<CStr, std::ostream*>::iterator it(m_map.find(a_type));
  if (it != m_map.end())
  {
    std::ostringstream *osString(0);
    osString = dynamic_cast<std::ostringstream*>(it->second);
    if (osString)
    {
      a_str = osString->str();
    }
    else
    {
      std::ofstream *osFile(0);
      osFile = dynamic_cast<std::ofstream*>(it->second);
      if (osFile)
      {
        osFile->flush();
        a_str = "";
        //a_str.clear();
        std::ifstream is(GetFileName(a_type).c_str());
        std::string line;
        while (getline(is, line))
        {
          a_str += line;
          a_str += "\n";
        }
      }
    }
  }
} // TxtExporter::impl::GetStream
//------------------------------------------------------------------------------
/// \brief Gets the stream from the type. Used in testing.
//------------------------------------------------------------------------------
bool TxtExporter::impl::WriteLinesAndDescriptionsToFile (const char* a_type,
                                                         std::vector<CStr>& a_lines,
                                                         std::vector<CStr>& a_desc)
{
  CStr sep = " # ";
  CStr type(a_type);
  if (MfData::Packages::MNW == type) sep = " !! ";
  size_t len=40;
  if (a_lines.size() != a_desc.size())
  {
    ASSERT(0);
    return false;
  }

  // see what the max string length is
  for (size_t i=0; i<a_lines.size(); i++)
  {
    if (len < a_lines[i].size() && !a_desc[i].empty())
      len = a_lines[i].size() + 4;
  }
  if (m_lineLen.find(a_type) == m_lineLen.end()) m_lineLen[a_type] = (int)len;
  if ((int)len < m_lineLen[a_type]) len = m_lineLen[a_type];

  CStr line;
  for (size_t i=0; i<a_lines.size(); i++)
  {
    // write the line
    line = a_lines[i];
    int diff = (int)(len - line.size());
    if (diff > 0)
    {
      // buffer the text out to the max
      CStr buff(diff, ' ');
      line += buff;
    }

    // write the line description
    if (!a_desc[i].empty())
    {
      line += sep;
      line += a_desc[i];
    }
    WriteLineToFile(a_type, line);
  }
  return true;
} // TxtExporter::impl::WriteLinesAndDescriptionsToFile
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool TxtExporter::impl::ClearFile (const char* const a_type)
{
  bool rval(0);
  std::map<CStr, std::ostream*>::iterator it(m_map.find(a_type));
  if (it != m_map.end())
  {
    if (it->second)
      delete(it->second);
    m_map.erase(it);
    CStr name = GetFileName(a_type);
    remove(name);
    m_lastStream = nullptr;
  }
  return rval;
} // TxtExporter::impl::ClearFile

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\TxtExporter.t.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
static std::map<CStr, CStr>& DefaultTypesToExtensions ()
{
  using namespace MfData;
  static std::map<CStr, CStr> m_ext;
  
  if (m_ext.empty())
  {
    m_ext.insert(std::make_pair(Packages::NAM, "mfn"));
    m_ext.insert(std::make_pair(Packages::GLOBAL, "glo"));
    m_ext.insert(std::make_pair(Packages::LIST, "out"));
    m_ext.insert(std::make_pair(Packages::BAS6, "ba6"));
    m_ext.insert(std::make_pair(Packages::LMT6, "lmt"));
    m_ext.insert(std::make_pair(Packages::GAGE, "gag"));
    m_ext.insert(std::make_pair(Packages::BCF, "bc6"));
    m_ext.insert(std::make_pair(Packages::BCF6, "bc6"));
  }
  return m_ext;
}
//------------------------------------------------------------------------------
void TxtExporterT::testCreateClass ()
{
  TxtExporter *t = new TxtExporter("junk");
  TS_ASSERT(t);
  if (t)
  {
    delete(t);
    t = 0;
  }
}
//------------------------------------------------------------------------------
void TxtExporterT::testGetStream ()
{
  CStr tmp, f;
  util::GetTempDirectory(tmp);
  tmp += "\\mybase";
  f = tmp + ".mfn";
  {
    TxtExporter t(tmp);
    t.SetTypesToExtensions(DefaultTypesToExtensions());
    std::ostream *os, *os1, *os2;
    os = t.m_p->GetStream(MfData::Packages::NAM);
    os1 = t.m_p->GetStream(MfData::Packages::NAM);
    os2 = t.m_p->GetStream("ABC");
    TS_ASSERT(*os);
    TS_ASSERT(*os1);
    TS_ASSERT(os == os1);
    TS_ASSERT(*os2);
  }
  TS_ASSERT(!remove(f));
  f = tmp + ".abc";
  TS_ASSERT(!remove(f));
}
//------------------------------------------------------------------------------
void TxtExporterT::testGetExtension ()
{
  using namespace MfData::Packages;
  CStr tmp;
  util::GetTempDirectory(tmp);
  tmp += "\\mybase";
  TxtExporter t(tmp);
  CStr types[9] = {NAM, GLOBAL, LIST, BAS6, LMT6, GAGE, BCF, BCF6,
                   "ABC"};
  CStr exts[9] = {"mfn", "glo", "out", "ba6", "lmt", "gag", "bc6", "bc6", 
                  "ABC"};
  std::map<CStr, CStr> typesToExtensions;
  CStr ext;

  for (int i=0; i<9; i++)
    typesToExtensions.insert(std::pair<CStr, CStr>(types[i], exts[i]));
  t.SetTypesToExtensions(typesToExtensions);

  for (int i=0; i<9; i++)
  {
    TS_ASSERT(t.IsTypeSupported(types[i]));
    ext = t.m_p->GetExtension(types[i]);
    TS_ASSERT_EQUALS2(ext, exts[i].ToLower());
  }
  TS_ASSERT(t.m_p->m_ext.size() == 9);
}
//------------------------------------------------------------------------------
void TxtExporterT::testWriteLineToFile ()
{
  CStr tmp, f, s;
  util::GetTempDirectory(tmp);
  tmp += "\\mybase";
  f = tmp + ".nam";
  {
    TxtExporter t(tmp);
    t.WriteStringToFile(MfData::Packages::NAM, "here is a string");
    t.GetFileContents(MfData::Packages::NAM, s);
    CStr expected = "here is a string\n";  // GetFileContents tacks on \n
    TS_ASSERT_EQUALS2(expected, s);
  }
  {
    std::ifstream is(f);
    TS_ASSERT(is);
    if (is)
    {
      is >> s; TS_ASSERT(s == "here");
      is >> s; TS_ASSERT(s == "is");
      is >> s; TS_ASSERT(s == "a");
      is >> s; TS_ASSERT(s == "string");
    }
  }
  TS_ASSERT(!remove(f));
}
//------------------------------------------------------------------------------
void TxtExporterT::testWriteStringToFile ()
{
  CStr tmp, f, s;
  util::GetTempDirectory(tmp);
  tmp += "\\mybase";
  f = tmp + ".nam";
  {
    TxtExporter t(tmp);
    t.WriteLineToFile(MfData::Packages::NAM, "here is a line");
    t.GetFileContents(MfData::Packages::NAM, s);
    CStr expected = "here is a line\n";
    TS_ASSERT_EQUALS2(expected, s);
  }
  {
    std::ifstream is(f);
    TS_ASSERT(is);
    if (is)
    {
      is >> s; TS_ASSERT(s == "here");
      is >> s; TS_ASSERT(s == "is");
      is >> s; TS_ASSERT(s == "a");
      is >> s; TS_ASSERT(s == "line");
      is >> s; TS_ASSERT(s == "line");
    }
  }
  TS_ASSERT(!remove(f));
}
//------------------------------------------------------------------------------
void TxtExporterT::testGetNewStream ()
{
  {
    TxtExporter t(TESTBASE);
    t.WriteLineToFile(MfData::Packages::NAM, "here is a line");
    std::map<CStr, std::ostream*>::iterator it;
    it = t.m_p->m_map.find(MfData::Packages::NAM);
    std::ostringstream *s(dynamic_cast<std::ostringstream*>(it->second));
    TS_ASSERT(s);
  }
  CStr f;
  {
    CStr tmp;
    util::GetTempDirectory(tmp);
    tmp += "\\mybase";
    f = tmp + ".mfn";
    TxtExporter t(tmp);
    t.SetTypesToExtensions(DefaultTypesToExtensions());
    t.WriteLineToFile(MfData::Packages::NAM, "here is a line");
    std::map<CStr, std::ostream*>::iterator it;
    it = t.m_p->m_map.find(MfData::Packages::NAM);
    std::ofstream *s(dynamic_cast<std::ofstream*>(it->second));
    TS_ASSERT(s);
  }
  TS_ASSERT(!remove(f));
}

#endif
