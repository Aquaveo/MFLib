//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Sfr.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Sfr.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\CellNumbering.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfExport\private\Native\NativeExpSfr.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>



using namespace MfData::Export;


class segment
{
public:
  segment() : m_roughch(-1), m_startReachId(-1), m_nReaches(-1), m_icalc(-1) {}

  std::vector<int> m_upStreamSegIds;
  std::vector<int> m_downStreamSegIds;
  std::vector<CStr> m_cprior;
  Real m_roughch;
  int m_startReachId;
  int m_nReaches;
  int m_icalc;
};

class reach
{
public:
  reach() : m_rlen(0), m_rwid(0), m_rgrd(0), m_rtp(0), m_rbth(0),
    m_rhk(0), m_man(0), m_ustrf(1.0), m_cellid(-1), m_ncon(0),
    m_segId(-1) {}

  Real m_rlen, m_rwid, m_rgrd, m_rtp, m_rbth, m_rhk, m_man, m_ustrf;
  int m_cellid, m_ncon, m_segId;
  std::vector<int> m_connections;
  std::vector<int> m_ndv;
};

class diversion
{
public:
  diversion() : m_rno(-1), m_iconr(-1), m_segId(-1), m_cprior() {}
  int m_rno;     // reach number for this diversion
  int m_iconr;   // reach number for down stream reach
  int m_segId;   // id of segment that is a diversion
  CStr m_cprior; // prioritization system for diversion
};

class sfrData
{
public:
  bool                   m_unstructured;
  int                    m_nstrm;
  std::vector<segment>   m_segments;
  std::vector<reach>     m_reaches;
  std::vector<diversion> m_diversions;

};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static sfrData& data()
{
  static sfrData m_;
  return m_;
} // data

class NativeExpMf6Sfr::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_)
  {
    data().m_unstructured = m_pack->GetGlobal()->Unstructured() ? 1 : 0;
  }

  void WriteOptions();
  void WriteDimensions();
  void WritePackageData();
  void WriteConnections();
  void WriteDiversions();
  void WriteStressPeriodData();

  void WriteToFile();

  void BuildConnections();

  void WriteReachStressPeriodData (const char *a_txt, const CStr& a_rnoStr,
    Real a_val);
  void SegmentsFromPackageData();
  void ReachConnectionsFromSegments();
  void FillReachesFromPackageData();

  CStr ConvertSfrIpriorToMf6Cprior(int a_iprior);

  NativePackExp* m_pack;
  std::vector<CStr> m_lines;
};



//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Sfr::NativeExpMf6Sfr (NativePackExp* a_) :
m_p(new impl(a_))
{
} // MfNativeExpMf6Sfr::MfNativeExpMf6Sfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Sfr::~NativeExpMf6Sfr ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // MfNativeExpMf6Sfr::~MfNativeExpMf6Sfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Sfr::Export ()
{
  if (!m_p) return false;
  if (m_p->m_pack->GetGlobal()->GetCurrentPeriod() == 1)
  {
    m_p->WriteOptions();
    m_p->WriteDimensions();

    m_p->BuildConnections();

    m_p->WritePackageData();
    m_p->WriteConnections();
    m_p->WriteDiversions();
  }

  m_p->WriteStressPeriodData();

  m_p->WriteToFile();
  return true;
} // NativeExpMf6Sfr::ExportMf6sfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteOptions ()
{
  using namespace MfData::Packages;
  MfGlobal* g = m_pack->GetGlobal();
  MfPackage* p = g->GetPackage(SFRLine1);
  const int *istcb1(0), *istcb2(0);  

  m_lines.push_back(MfExportUtil::GetMf6CommentHeader());

  m_lines.push_back("BEGIN OPTIONS");
  p->GetField(SFRpack::ISTCB1, &istcb1);
  if (istcb1 && *istcb1 > 0)
  {
    m_pack->GetGlobal()->SetIntVar("MF6_SAVE_FLOWS", 1);
    m_lines.push_back("  SAVE_FLOWS");
  }

  p->GetField(SFRpack::ISTCB2, &istcb2);
  if (istcb2 && *istcb2 > 0)
  {
    std::stringstream ss;
    CStr baseName;
    util::StripExtensionFromFilename(baseName, baseName);
    baseName += "_sfr.ccf";
    ss << "  BUDGET FILEOUT " << baseName;
    m_lines.push_back(ss.str());
  }

  // need to write unit conversion.
  const Real* constv(0),* dleak(0);
  p->GetField(SFRpack::CONSTV, &constv);
  p->GetField(SFRpack::DLEAK, &dleak);
  if (constv)
  {
    std::stringstream ss;
    ss << "  UNIT_CONVERSION " << STR(*constv);
    m_lines.push_back(ss.str());
  }
  if (dleak)
  {
    std::stringstream ss;
    ss << "  MAXIMUM_DEPTH_CHANGE " << STR(*dleak);
    m_lines.push_back(ss.str());
  }

  m_lines.push_back("END OPTIONS");
  m_lines.push_back("");
} // NativeExpMf6Sfr::impl::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteDimensions ()
{

  m_lines.push_back("BEGIN DIMENSIONS");

  m_pack->GetGlobal()->GetIntVar(Packages::SFRpack::NSTRM, data().m_nstrm);
  std::stringstream ss;
  ss << "  NREACHES " << data().m_nstrm;
  m_lines.push_back(ss.str());

  m_lines.push_back("END DIMENSIONS");
  m_lines.push_back("");
} // NativeExpMf6Sfr::impl::WriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WritePackageData ()
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  int w = util::RealWidth();
  m_lines.push_back("BEGIN PACKAGEDATA");
  CStr comment = "#        rno     k     i     j          rlen         "
    "rwid         rgrd          rtp         rbth          rhk          "
    "man  ncon        ustrf   ndv";
  m_lines.push_back(comment);
  for (size_t i=0; i<data().m_reaches.size(); ++i)
  {
    reach &r(data().m_reaches[i]);
    std::stringstream ss;
    CStr rnoStr, nconStr, ndvStr;
    Real man = data().m_segments[r.m_segId-1].m_roughch;
    if (0 == man)
    {
      man = (Real).0298765;
      CStr msg = "WARNING: 0.0 specified for ROUGHCH (Manning's n). Value has "
        "been changed to .0298765.";
      printf("%s\n", msg.c_str());
    }
    rnoStr.Format("%10d", i+1);
    nconStr.Format("%5d", r.m_connections.size());
    ndvStr.Format("%5d", r.m_ndv.size());
    ss << "  " << rnoStr << " " << cn->CellIdStringFromId(r.m_cellid) << " "
       << STR(r.m_rlen,-1,w,STR_FULLWIDTH) << " "
       << STR(r.m_rwid,-1,w,STR_FULLWIDTH) << " "
       << STR(r.m_rgrd,-1,w,STR_FULLWIDTH) << " "
       << STR(r.m_rtp,-1,w,STR_FULLWIDTH) << " "
       << STR(r.m_rbth,-1,w,STR_FULLWIDTH) << " "
       << STR(r.m_rhk,-1,w,STR_FULLWIDTH) << " "
       << STR(man,-1,w,STR_FULLWIDTH) << " "
       << nconStr << " "
       << STR(r.m_ustrf,-1,w,STR_FULLWIDTH) << " "
       << ndvStr;
    m_lines.push_back(ss.str());
  }
  m_lines.push_back("END PACKAGEDATA");
  m_lines.push_back("");
} //NativeExpMf6Sfr::impl::WritePackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteConnections ()
{
  m_lines.push_back("BEGIN CONNECTIONDATA");
  for (size_t i=0; i<data().m_reaches.size(); ++i)
  {
    std::stringstream ss;
    CStr str;
    str.Format("%10d", i+1);
    ss << "  " << str << " ";
    for (size_t j=0; j<data().m_reaches[i].m_connections.size(); ++j)
    {
      str.Format("%10d", data().m_reaches[i].m_connections[j]);
      ss << str << " ";
    }
    m_lines.push_back(ss.str());
  }
  m_lines.push_back("END CONNECTIONDATA");
  m_lines.push_back("");
} // NativeExpMf6Sfr::impl::WriteConnections
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteDiversions ()
{
  m_lines.push_back("BEGIN DIVERSIONS");
  for (size_t i=0; i<data().m_diversions.size(); ++i)
  {
    CStr rnoStr, idvStr, iconrStr;
    rnoStr.Format("%10d", data().m_diversions[i].m_rno);
    idvStr.Format("%10d", i+1);
    iconrStr.Format("%10d", data().m_diversions[i].m_iconr);
    std::stringstream ss;
    ss << "  " << rnoStr << " " << idvStr << " " << iconrStr << " "
       << data().m_diversions[i].m_cprior;
    m_lines.push_back(ss.str());
  }
  m_lines.push_back("END DIVERSIONS");
  m_lines.push_back("");
} // NativeExpMf6Sfr::impl::WriteDiversions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteStressPeriodData ()
{
  using namespace MfData::Packages;
  using namespace util;

  MfGlobal* g = m_pack->GetGlobal();
  int nstrmd(0), segSize(0);
  g->GetIntVar(SFRpack::NSTRMD, nstrmd);
  g->GetIntVar("SFR_SEG_SIZE", segSize);

  const Real* seg(0),* strm(0);
  MfPackage* pSFRLine2 = g->GetPackage(SFRLine2);
  pSFRLine2->GetField(SFRpack::STRM, &strm);
  MfPackage* pSFRLine6 = g->GetPackage(SFRLine6);
  pSFRLine6->GetField(SFRpack::SEG, &seg);

  {
    std::stringstream ss;
    ss << "BEGIN PERIOD " << m_pack->GetGlobal()->GetCurrentPeriod();
    m_lines.push_back(ss.str());
  }

  // get a set of segments that are diversions
  std::set<int> diversionSegments;
  for (size_t i=0; i<data().m_diversions.size(); ++i)
  {
    diversionSegments.insert(data().m_diversions[i].m_segId);
  }

  int w = util::RealWidth();
  for (int i=0; i<data().m_nstrm; ++i)
  {
    // inflow, rainfall, evaporation, runoff
    CStr rnoStr;
    rnoStr.Format("%10d", i+1);
    reach& rchRef(data().m_reaches[i]);
    int segId = rchRef.m_segId;
    segment& segRef(data().m_segments[segId-1]);
    Real inflow(0), rainfall(0), evaporation(0), runoff(0);

    // get the inflow for the first reach on a segment if it is specified
    // and the segment is not a diversion
    if (i+1 == segRef.m_startReachId &&
        diversionSegments.find(segId) == diversionSegments.end())
    {
      inflow = ForElement(seg, 2, segId, segSize);
    }

    runoff = ForElement(strm, 12, i+1, nstrmd);
    evaporation = ForElement(seg, 4, segId, segSize);
    rainfall = ForElement(seg, 5, segId, segSize);

    WriteReachStressPeriodData(" inflow      ", rnoStr, inflow);
    WriteReachStressPeriodData(" rainfall    ", rnoStr, rainfall);
    WriteReachStressPeriodData(" evaporation ", rnoStr, evaporation);
    WriteReachStressPeriodData(" runoff      ", rnoStr, runoff);

    if (segRef.m_icalc <= 0)
    {
      std::stringstream ss;
      Real stage = ForElement(strm, 15, i+1, nstrmd);
      ss << "  " << rnoStr << " status            simple\n";
      ss << "  " << rnoStr << " stage       " << STR(stage,-1,w,STR_FULLWIDTH);
      m_lines.push_back(ss.str());
    }
    for (size_t d=0; d<rchRef.m_ndv.size(); ++d)
    {
      std::stringstream ss;
      int divId = rchRef.m_ndv[d];
      CStr divIdStr;
      divIdStr.Format("%5d", divId);
      int diversionSegId = data().m_diversions[divId-1].m_segId;
      Real diversionFlow = ForElement(seg, 2, diversionSegId, segSize);
      ss << "  " << rnoStr << " diversion   " << divIdStr << " "
         << STR(diversionFlow,-1,w,STR_FULLWIDTH);
      m_lines.push_back(ss.str());
    }
  }
  m_lines.push_back("END PERIOD");
  m_lines.push_back("");
} // NativeExpMf6Sfr::impl::WriteStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteToFile ()
{
  NativeExpSfr* sfr = dynamic_cast<NativeExpSfr*>(m_pack);
  MfPackage* p = sfr->GetGlobal()->GetPackage(Packages::SFR);
  std::vector<CStr> desc(m_lines.size(), "");
  p->StringsToWrite() = m_lines;
  p->StringDescriptions() = desc;
  sfr->WriteStoredLinesSfr();
} // NativeExpMf6Sfr::impl::WriteToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::BuildConnections ()
{
  SegmentsFromPackageData();
  ReachConnectionsFromSegments();
  FillReachesFromPackageData();
} // NativeExpMf6Sfr::impl::BuildConnections
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::WriteReachStressPeriodData (
  const char *a_txt, const CStr& a_rnoStr, Real a_val)
{
  int w = util::RealWidth();
  if (0 != a_val)
  {
    std::stringstream ss;
    ss << "  " << a_rnoStr << a_txt << STR(a_val,-1,w,STR_FULLWIDTH);
    m_lines.push_back(ss.str());
  }
} // iWriteReachStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::SegmentsFromPackageData ()
{
  using namespace MfData::Packages;
  using namespace util;

  MfGlobal* g = m_pack->GetGlobal();
  MfPackage *p1 = g->GetPackage(SFRLine1);
  MfPackage *p2 = g->GetPackage(SFRLine2);

  const int* nss(0);
  p1->GetField(SFRpack::NSS, &nss);
  const int* istrm(0);
  p2->GetField(SFRpack::ISTRM, &istrm);
  int nistrmd(0);
  g->GetIntVar(SFRpack::NISTRMD, nistrmd);

  MfPackage* p6 = g->GetPackage(SFRLine6);
  const int* iotsg(0),* idivar(0),* iseg(0);
  const Real* seg(0);
  p6->GetField(SFRpack::IOTSG, &iotsg);
  p6->GetField(SFRpack::IDIVAR, &idivar);
  p6->GetField(SFRpack::SEG, &seg);
  p6->GetField(SFRpack::ISEG, &iseg);
  ASSERT(iotsg && idivar && seg && iseg);
  int segSize;
  m_pack->GetGlobal()->GetIntVar("SFR_SEG_SIZE", segSize);

  int startReachId(1);

  // allocate segments and reaches
  data().m_segments.assign(*nss, segment());
  data().m_reaches.assign(data().m_nstrm, reach());

  // first get segment connectivity
  for (int i=0; i<*nss; ++i)
  {
    int outseg = iotsg[i];
    int iupseg = ForElement(idivar, 1, i+1, 2);
    int iprior = ForElement(idivar, 2, i+1, 2);
    Real roughch = ForElement(seg, 16, i+1, segSize);
    int icalc = ForElement(iseg, 1, i+1, 4);
    if (icalc != 0 && icalc != 1)
    {
      CStr msg = "ERROR: SFR6 only supports simulation options equivalent to "
        "ICALC = 0 and ICALC = 1. Convert all segments to ICALC = 0 or 1 and "
        "run converter again.";
      printf("%s\n", msg.c_str());
    }

    segment& s(data().m_segments[i]);
    s.m_icalc = icalc;
    s.m_roughch = roughch;
    if (outseg > 0 && outseg <= *nss)
    {
      s.m_downStreamSegIds.push_back(outseg);
      s.m_cprior.push_back(""); // this is not a diversion
      data().m_segments[outseg-1].m_upStreamSegIds.push_back(i+1);
    }
    if (iupseg > 0 && iupseg <= *nss)
    {
      s.m_upStreamSegIds.push_back(iupseg);
      data().m_segments[iupseg-1].m_downStreamSegIds.push_back(i+1);
      CStr cprior = ConvertSfrIpriorToMf6Cprior(iprior);
      data().m_segments[iupseg-1].m_cprior.push_back(cprior);
    }

    s.m_startReachId = startReachId;
    // count number of reaches
    bool done(false);
    int cnt(0);
    while (!done)
    {
      int idx = ( (startReachId-1) * (nistrmd)) + 3;
      int iseg = istrm[idx];
      if (iseg == i+1)
      {
        startReachId++;
        cnt++;
      }
      else
      {
        done = true;
      }
    }
    data().m_segments[i].m_nReaches = cnt;
  }

  // create diversions
  for (int i=0; i<*nss; ++i)
  {
    segment& s(data().m_segments[i]);
    if (s.m_downStreamSegIds.size() > 1)
    {
      Real ustrf(1);
      if (!s.m_downStreamSegIds.empty())
        ustrf = 1 / (Real)(s.m_downStreamSegIds.size());
      // this segment has diversions. The diversion segments are in indexes
      // 1..size of m_downStreamSegIds
      for (size_t j=0; j<s.m_downStreamSegIds.size(); ++j)
      {
        // modify ustrf for the reaches connected at this diversion. This is
        // applied to all reaches (even non-diversions)
        segment& downSeg(data().m_segments[s.m_downStreamSegIds[j]-1]);
        data().m_reaches[downSeg.m_startReachId-1].m_ustrf = ustrf;
        // skip this because it is not a diversion
        if (s.m_cprior[j] == "")
          continue;

        int diversionSegId = s.m_downStreamSegIds[j];
        diversion div;
        div.m_segId = diversionSegId;
        // last reach number from this segment
        div.m_rno = s.m_startReachId + s.m_nReaches - 1;
        // diversion type
        div.m_cprior = s.m_cprior[j];
        // first reach of diversion segment
        div.m_iconr = data().m_segments[diversionSegId-1].m_startReachId;
        data().m_diversions.push_back(div);
        data().m_reaches[div.m_rno-1].m_ndv.push_back((int)data().m_diversions.size());
      }
    }
  }
} // NativeExpMf6Sfr::impl::SegmentsFromPackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::ReachConnectionsFromSegments ()
{
  // fill in reach connections
  for (size_t i=0; i<data().m_segments.size(); ++i)
  {
    segment& seg(data().m_segments[i]);
    // find the upstream segment. The first segment will not have one
    std::vector<segment*> upSegs;
    for (size_t j=0; j<seg.m_upStreamSegIds.size(); ++j)
      upSegs.push_back(&data().m_segments[seg.m_upStreamSegIds[j]-1]);
    // get pointers to downstream segments
    std::vector<segment*> downSegs;
    for (size_t j=0; j<seg.m_downStreamSegIds.size(); ++j)
    {
      downSegs.push_back(&data().m_segments[seg.m_downStreamSegIds[j]-1]);
    }
    // number of reaches in this segment
    int nReaches = seg.m_nReaches;
    // id of first reach in this segment
    int start = seg.m_startReachId;

    for (int r=0; r<nReaches; ++r)
    {
      reach& rch(data().m_reaches[start+r-1]);
      rch.m_segId = (int)i+1;
      // first reach in this segment
      if (r == 0)
      { // loop through upstream segments if they exist
        for (size_t q=0; q<upSegs.size(); ++q)
        {
          int upSegLastReachId = upSegs[q]->m_startReachId + upSegs[q]->m_nReaches - 1;
          rch.m_connections.push_back(upSegLastReachId);
        }
      }
      else
      { // we are not at the first reach so our previous reach is the prior
        // reach in this segment
        int prevReachId = start + r - 1; 
        rch.m_connections.push_back(prevReachId);
      }

      // last reach of this segment
      if (r+1 == nReaches)
      { // loop through downstream reaches is they exist
        for (size_t q=0; q<downSegs.size(); ++q)
        {
          segment& dSeg(*downSegs[q]);
          // negative because down stream connection
          rch.m_connections.push_back(-dSeg.m_startReachId);
        }
      }
      else
      { // as long as we are not at the last reach we want to connect to the
        // next reach in this segment
        int nextReachId = start + r + 1;
        rch.m_connections.push_back(-nextReachId);
      }
    }
  }
} // NativeExpMf6Sfr::impl::ReachConnectionsFromSegments
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Sfr::impl::FillReachesFromPackageData ()
{
  using namespace MfData::Packages;
  int nstrmd(0), nistrmd(0);
  m_pack->GetGlobal()->GetIntVar(SFRpack::NSTRMD, nstrmd);
  m_pack->GetGlobal()->GetIntVar(SFRpack::NISTRMD, nistrmd);

  MfPackage* pSFRLine2 = m_pack->GetGlobal()->GetPackage(Packages::SFRLine2);
  const Real* strm(0);
  const int* istrm(0);
  pSFRLine2->GetField(SFRpack::ISTRM, &istrm);
  pSFRLine2->GetField(SFRpack::STRM, &strm);
  ASSERT(strm && istrm);
  if (!strm || !istrm) return;

  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();

  for (size_t i=0; i<data().m_reaches.size(); ++i)
  {
    int idx = (int)i * nstrmd;
    reach& rch(data().m_reaches[i]);
    rch.m_rlen = strm[idx+0];
    rch.m_rgrd = strm[idx+1];
    rch.m_rtp  = strm[idx+2];
    rch.m_rwid = strm[idx+4];
    rch.m_rhk  = strm[idx+5];
    rch.m_rbth = strm[idx+7];

    if (data().m_unstructured)
    {
      int nrch = istrm[i*nistrmd+5];
      rch.m_cellid = nrch;
    }
    else
    {
      int ck = istrm[i*nistrmd+0];
      int ci = istrm[i*nistrmd+1];
      int cj = istrm[i*nistrmd+2];
      rch.m_cellid = cn->IdFromIjk(ci, cj, ck);
    }
  }
} // NativeExpMf6Sfr::impl::FillReachesFromPackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Sfr::impl::ConvertSfrIpriorToMf6Cprior(int a_iprior)
{
  CStr rval("UPTO");
  if (-1 == a_iprior)      rval = "THRESHOLD";
  else if (-2 == a_iprior) rval = "FRACTION";
  else if (-3 == a_iprior) rval = "EXCESS";
  else if (0 == a_iprior)  rval = "UPTO"; // these 2 lines are here for documentation. 
  else                     rval = "UPTO"; // the default is "UPTO" and if the value is
                                          // something unexpected then you get the default.
  return rval;
} // NativeExpMf6Sfr::impl::DefineCPRIOR

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif