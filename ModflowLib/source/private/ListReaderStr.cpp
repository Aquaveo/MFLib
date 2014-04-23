//------------------------------------------------------------------------------
// FILE      ListReaderStr.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\ListReaderStr.h>

#include <map>

#include <private\ListReader\ListReaderParser.h>
#include <private\H5DataReader\H5DataSetReader.h>

#define STRSEGID  "08. Str reach segment ID"
#define SEGID     "09. Segment ID"
#define SEGFLW    "10. Segment Flow"
#define ITRIB     "11. ITRIB"
#define UPID      "12. Upstream ID"

static bool GetSegReach(int a_nRows,
                        const CStr &a_fileName,
                        const CStr &a_path,
                        std::vector<int> &a_vSeg,
                        std::vector<int> &a_vReach);
static bool GetFlow(int a_stressPer,
                    const CStr &a_fileName,
                    const CStr &a_path,
                    const std::vector<int> &a_vSeg,
                    std::vector<Real> &a_vFlow);
static bool GetItrib(const CStr &a_fileName,
                     const CStr &a_path,
                     int a_NSS,
                     int a_NTRIB,
                     int *a_ITRBAR);
static bool GetIdivar(const CStr &a_fileName,
                      const CStr &a_path,
                      int a_NSS,
                      int *a_IDIVAR);


//------------------------------------------------------------------------------
/// \brief Gets the data for the stream
//------------------------------------------------------------------------------
bool ListReaderStr::GetDataStr (Real *a_STRM,
                                int *a_ISTRM,
                                int *a_ITRBAR,
                                int *a_IDIVAR,
                                int a_NSS,
                                int a_NTRIB) const
{
  bool rVal(true);
  try
  {
    {
      std::vector<Real> vFlt, fact, vFlow;
      std::vector<int>   vReach, vSeg;
      int nRows(SetUp().m_nRows), nFields(SetUp().m_nFields);
      int stressPer(Parser().GetStressPeriod());

      if (1 == stressPer)
      {
        // init the arrays to zero
        int j;
        for (j=0; j<nRows*11; j++)
          a_STRM[j] = 0.0;
        for (j=0; j<nRows*5; j++)
          a_ISTRM[j] = 0;
        for (j=0; j<a_NSS*a_NTRIB; j++)
          a_ITRBAR[j] = 0;
        for (j=0; j<a_NSS; j++)
          a_IDIVAR[j] = 0;
      }

      GetData(vFlt);

      GetSegReach(nRows, Parser().GetFileName(),
                  Parser().GetPath(), vSeg, vReach);
      GetFlow(Parser().GetStressPeriod(), Parser().GetFileName(),
              Parser().GetPath(), vSeg, vFlow);
      if (vSeg.empty() || vReach.empty() || vFlow.empty())
        throw EException("Unable to get segement and reach ids.");

      for (int i=0; i<nRows; i++)
      {
        if (1 == stressPer)
        {
          a_ISTRM[i*5+0] = (int)vFlt.at(i*nFields+0); // k
          a_ISTRM[i*5+1] = (int)vFlt.at(i*nFields+1); // i
          a_ISTRM[i*5+2] = (int)vFlt.at(i*nFields+2); // j
          a_ISTRM[i*5+3] = vSeg.at(i); // Segment id
          a_ISTRM[i*5+4] = vReach.at(i); // Reach id
        }

        a_STRM[i*11+0] = vFlow.at(i); // flow
        a_STRM[i*11+1] = vFlt.at(i*nFields+3); // stage
        a_STRM[i*11+2] = vFlt.at(i*nFields+4); // conductance
        a_STRM[i*11+3] = vFlt.at(i*nFields+5); // bot. elev.
        a_STRM[i*11+4] = vFlt.at(i*nFields+6); // top elev.
        a_STRM[i*11+5] = vFlt.at(i*nFields+7); // width
        a_STRM[i*11+6] = vFlt.at(i*nFields+8); // slope
        a_STRM[i*11+7] = vFlt.at(i*nFields+9); // rough
        //ASSERT(_CrtCheckMemory());
      }
    }

    // get the ITRIB data
    GetItrib(Parser().GetFileName(), Parser().GetPath(),
             a_NSS, a_NTRIB, a_ITRBAR);

    // Get the IDIVAR data. This is for diversions.
    GetIdivar(Parser().GetFileName(), Parser().GetPath(), a_NSS, a_IDIVAR);
  }
  catch (EException &e)
  {
    CStr msg(e.what());
    if (!msg.IsEmpty())
    {
      ErrorStack::Get().PutError(msg);
    }
    rVal = false;
  }
  catch (std::out_of_range)
  {
    rVal = false;
  }
  return rVal;
} // ListReaderStr::GetDataStr
//------------------------------------------------------------------------------
/// \brief Gets the segment and reach ids for each stream boundary condition
//------------------------------------------------------------------------------
static bool GetSegReach (int a_nRows,
                         const CStr &a_fileName,
                         const CStr &a_path,
                         std::vector<int> &a_vSeg,
                         std::vector<int> &a_vReach)
{
  // get the reach and segment ids
  std::pair<int, int> myPair(0, a_nRows);
  VEC_INT_PAIR indices(1, myPair);
  CStr path(a_path + "/" + STRSEGID);
  H5DataSetReader r(a_fileName, path, indices);
  r.AllowTypeConversions(true);
  if (!r.GetData(a_vSeg))
    return false;

  // fill in the vector for the reaches
  a_vReach.assign(a_vSeg.size(), 1);
  for (size_t i=1; i<a_vReach.size(); i++)
  {
    if (a_vSeg.at(i) == a_vSeg.at(i-1))
      a_vReach.at(i) = a_vReach.at(i-1) + 1;
  }
  return true;
} // GetSegAndReach
//------------------------------------------------------------------------------
/// \brief Gets the flow for each stream boundary condition in a stress period
//------------------------------------------------------------------------------
static bool GetFlow (int a_stressPer,
                     const CStr &a_fileName,
                     const CStr &a_path,
                     const std::vector<int> &a_vSeg,
                     std::vector<Real> &a_vFlow)
{
  if (a_stressPer < 1)
    return false;

  static std::vector<int> seg; // ok to leave static
  if (1 == a_stressPer)
  {
    seg = a_vSeg;
  }
  if (seg.empty())
    return false;

  // get the segment ids
  std::vector<int> segments;
  {
    VEC_INT_PAIR indices;
    CStr path(a_path + "/" + SEGID);
    H5DataSetReader r(a_fileName, path, indices);
    if (!r.GetAllData(segments))
      return false;
    if (segments.empty())
      return false;
  }

  // read the flow for this stress period
  std::vector<double> flow;
  {
    std::pair<int, int> myPair(0, (int)segments.size());
    VEC_INT_PAIR indices(2, myPair);
    indices.at(1).first = a_stressPer - 1;
    indices.at(1).second = 1;
    CStr path(a_path + "/" + SEGFLW);
    H5DataSetReader r(a_fileName, path, indices);
    r.AllowTypeConversions(true);
    if (!r.GetData(flow))
      return false;
    if (flow.empty() || flow.size() != segments.size())
      return false;
  }

  size_t i;
  std::map<int, double> mapSegFlow;
  for (i=0; i<flow.size(); i++)
  {
    mapSegFlow.insert(std::make_pair(segments.at(i), flow.at(i)));
  }

  a_vFlow.assign(seg.size(), 0);
  for (i=0; i<seg.size(); i++)
  {
    a_vFlow.at(i) = (Real)mapSegFlow[seg.at(i)];
    mapSegFlow[seg.at(i)] = 0.0;
  }
  return true;
} // GetFlow
//------------------------------------------------------------------------------
/// \brief Gets ITRIB array
//------------------------------------------------------------------------------
static bool GetItrib (const CStr &a_fileName,
                      const CStr &a_path,
                      int a_NSS,
                      int a_NTRIB,
                      int *a_ITRBAR)
{
  if (!a_ITRBAR || a_NSS < 1 || a_NTRIB < 1)
    return false;

  bool rVal(true);
  for (int i=0; i<a_NTRIB && rVal; i++)
  {
    std::pair<int, int> myPair(0, a_NSS);
    VEC_INT_PAIR indices(2, myPair);
    indices.at(1).first = i;
    indices.at(1).second = 1;
    CStr path(a_path + "/" + ITRIB);
    H5DataSetReader r(a_fileName, path, indices);
    r.AllowTypeConversions(true);
    if (!r.GetData(&a_ITRBAR[i*a_NSS], (size_t)a_NSS))
      rVal = false;
  }
  return rVal;
} // GetItrib
//------------------------------------------------------------------------------
/// \brief Gets IDIVAR array
//------------------------------------------------------------------------------
static bool GetIdivar (const CStr &a_fileName,
                       const CStr &a_path,
                       int a_NSS,
                       int *a_IDIVAR)
{
  std::pair<int, int> myPair(0, a_NSS);
  VEC_INT_PAIR indices(1, myPair);
  CStr path(a_path + "/" + UPID);
  H5DataSetReader r(a_fileName, path, indices);
  r.AllowTypeConversions(true);
  return(r.GetData(a_IDIVAR,(size_t)a_NSS));
} // GetIdivar
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\ListReaderStr.t.h>

//------------------------------------------------------------------------------
void ListReaderStrT::setUp ()
{
  H5Initialize::Init();
  m_file = util::GetTestFilesDirectory() +
           "\\HDF5_InputFiles\\smallGrid_Trans.h5";
}
//------------------------------------------------------------------------------
void ListReaderStrT::testCreateClass ()
{
  ListReaderSetUp s;
  ListReaderStr *tmp = new ListReaderStr(s);
  TS_ASSERT(tmp);
  if (tmp)
    delete(tmp);
}
//------------------------------------------------------------------------------
void ListReaderStrT::testGetSegReach ()
{
  CStr emptyStr, path("Stream");
  std::vector<int> vSeg, vReach;
  std::vector<Real> vFlow;
  TS_ASSERT(!GetSegReach(17, emptyStr, path, vSeg, vReach));
  TS_ASSERT(!GetSegReach(17, m_file, emptyStr, vSeg, vReach));
  TS_ASSERT(!GetSegReach(18, m_file, path, vSeg, vReach));

  TS_ASSERT(GetSegReach(17, m_file, path, vSeg, vReach));

  int seg[17] = {1,1,1,2,2,3,3,4,4,4,5,5,5,6,6,6,6};
  for (size_t i=0; i<vSeg.size(); i++)
    TS_ASSERT_EQUALS(vSeg.at(i), seg[i]);
  int reach[17] = {1,2,3,1,2,1,2,1,2,3,1,2,3,1,2,3,4};
  for (size_t i=0; i<vReach.size(); i++)
    TS_ASSERT_EQUALS(vReach.at(i), reach[i]);
}
//------------------------------------------------------------------------------
void ListReaderStrT::testGetFlow ()
{
  CStr emptyStr, path("Stream");
  std::vector<int> vSeg, vReach;
  std::vector<Real> vFlow;
  TS_ASSERT(!GetFlow(0, m_file, path, vSeg, vFlow));
  TS_ASSERT(!GetFlow(1, emptyStr, path, vSeg, vFlow));
  TS_ASSERT(!GetFlow(1, m_file, emptyStr, vSeg, vFlow));
  TS_ASSERT(!GetFlow(1, m_file, path, vSeg, vFlow));
  TS_ASSERT(!GetFlow(2, m_file, path, vSeg, vFlow));

  TS_ASSERT(GetSegReach(17, m_file, path, vSeg, vReach));
  TS_ASSERT(GetFlow(1, m_file, path, vSeg, vFlow));
  TS_ASSERT(vFlow.size() == 17);
  {
    Real a((Real)0.01);
    Real f[17] = {1,0,0,1,0,-1,0,1,0,0,-1,0,0,a,0,0,0};
    for (size_t i=0; i<vFlow.size(); i++)
      TS_ASSERT_EQUALS(vFlow.at(i), f[i]);
  }

  TS_ASSERT(GetFlow(2, m_file, path, vSeg, vFlow));
  TS_ASSERT(vFlow.size() == 17);
  {
    Real a((Real)0.01);
    Real f[17] = {2,0,0,2,0,-1,0,2,0,0,-1,0,0,a,0,0,0};
    for (size_t i=0; i<vFlow.size(); i++)
      TS_ASSERT_EQUALS(vFlow.at(i), f[i]);
  }
}
//------------------------------------------------------------------------------
void ListReaderStrT::testGetItrib ()
{
  CStr emptyStr, path("Stream");
  std::vector<int> vItrib(12, 0);
  int NSS(6), NTRIB(2);
  TS_ASSERT(!GetItrib(emptyStr, path, NSS, NTRIB, &vItrib.at(0)));
  TS_ASSERT(!GetItrib(m_file, emptyStr, NSS, NTRIB, &vItrib.at(0)));
  TS_ASSERT(!GetItrib(m_file, path, 0, NTRIB, &vItrib.at(0)));
  TS_ASSERT(!GetItrib(m_file, path, NSS, 0, &vItrib.at(0)));
  TS_ASSERT(!GetItrib(m_file, path, NSS, NTRIB, NULL));
  TS_ASSERT(GetItrib(m_file, path, NSS, NTRIB, &vItrib.at(0)));
  TS_ASSERT(vItrib.size() == 12);
  int i[12] = {0,0,1,0,3,0,0,0,2,0,4,0};
  for (int j=0; j<12; j++)
    TS_ASSERT_EQUALS(vItrib.at(j), i[j]);
}
//------------------------------------------------------------------------------
void ListReaderStrT::testGetIdivar ()
{
  CStr emptyStr, path("Stream");
  std::vector<int> vIdivar(6, 0);
  int NSS(6);
  TS_ASSERT(!GetIdivar(emptyStr, path, NSS, &vIdivar.at(0)));
  TS_ASSERT(!GetIdivar(m_file, emptyStr, NSS, &vIdivar.at(0)));
  TS_ASSERT(!GetIdivar(m_file, path, 0, &vIdivar.at(0)));
  TS_ASSERT(!GetIdivar(m_file, path, 7, &vIdivar.at(0)));
  TS_ASSERT(GetIdivar(m_file, path, NSS, &vIdivar.at(0)));
  TS_ASSERT(vIdivar.size() == 6);
  int i, j[6] = {0,0,0,0,0,1};
  for (i=0; i<6; i++)
    TS_ASSERT_EQUALS(vIdivar.at(i), j[i]);
}
//------------------------------------------------------------------------------
void ListReaderStrT::testGetDataStr ()
{
  CStr str;
  str.Format("GMS_HDF5_01 \"%s\" \"Stream\" 1", m_file);
  ListReaderSetUp setUp(17, 10, 0, 0, 5, 6, str);
  ListReaderStr reader(setUp);

  std::vector<Real> strm(187, 0.0);
  std::vector<int> iStrm(85, 0), iTrbar(12, 0), iDivar(6, 0);
  TS_ASSERT(reader.GetDataStr(&strm[0], &iStrm[0], &iTrbar[0], &iDivar[0], 6, 2));

  {
    int   idx[10] = {0,186,8,21,35,70,83,121,145,151};
    Real a[10] = {1,0,0,0,(Real)5.1994166,20,0,0,(Real)6.4922776,0};
    for (int i=0; i<10; i++)
      TS_ASSERT_DELTA(strm.at(idx[i]), a[i], CXXDELTA);
  }
  {
    int   idx[10] = {0,84,3,9,17,26,33,47,51,68};
    int a[10] = {1,4,1,2,3,2,3,3,3,6};
    for (int i=0; i<10; i++)
      TS_ASSERT_EQUALS(iStrm.at(idx[i]), a[i]);
  }
  {
    int a[12] = {0,0,1,0,3,0,0,0,2,0,4,0};
    for (int i=0; i<12; i++)
      TS_ASSERT_EQUALS(iTrbar.at(i), a[i]);
  }
  {
    int a[6] = {0,0,0,0,0,1,};
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(iDivar.at(i), a[i]);
  }
}
#endif // CXX_TEST

