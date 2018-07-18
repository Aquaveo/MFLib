//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Maw.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Maw.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/CellNumbering.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>



using namespace MfData::Export;

class NativeExpMf6Maw::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_) {}

  void WriteOptions();
  void WriteDimensions();
  void WritePackageData ();
  void WriteConnectionData ();
  void WriteStressPeriod();

  NativePackExp* m_pack;
  std::vector<CStr> m_lines;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Maw::NativeExpMf6Maw (NativePackExp* a_) :
m_p(new impl(a_))

{
} // MfNativeExpMf6Maw::MfNativeExpMf6Maw
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Maw::~NativeExpMf6Maw ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // MfNativeExpMf6Maw::~MfNativeExpMf6Maw
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Maw::Export ()
{
  if (!m_p || !m_p->m_pack) return false;

  MfGlobal* g = m_p->m_pack->GetGlobal();
  MfPackage* p = m_p->m_pack->GetPackage();
  if (!p) return false;

  CStr ln = p->GetLineNumber();
  if ("1" == ln)
  {
    m_p->WriteOptions();
    m_p->WriteDimensions();
  }
  else if ("34" == ln)
  {
    if (g->GetCurrentPeriod() == 1)
    {
      m_p->WritePackageData();
      m_p->WriteConnectionData();
    }
    m_p->WriteStressPeriod();
  }

  std::vector<CStr> desc(m_p->m_lines.size(), "");
  TmpPackageNameChanger tmp(p, "MAW");
  m_p->m_pack->AddToStoredLinesDesc(m_p->m_lines, desc);
  m_p->m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Maw::ExportMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Maw::impl::WriteOptions ()
{
  using namespace MfData::Packages;
  MfGlobal *g = m_pack->GetGlobal();

  const int *MNWMAX(0),*IWL2CB(0),*MNWPRNT(0),*NAUX(0);
  const char *MNWAUX(0);

  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !p->GetField(MNW2pack::IWL2CB, &IWL2CB) || !IWL2CB ||
      !p->GetField(MNW2pack::MNWPRNT, &MNWPRNT) || !MNWPRNT ||
      !p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX ||
      !p->GetField(MNW2pack::MNWAUX, &MNWAUX) || !MNWAUX)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr> auxNames;
  char              tmpAux[17];
  int               i, j, cnt(0);
  tmpAux[16] = '\0';
  for (i=0; i<*NAUX; i++)
  {
    for (j=0; j<16; j++)
    {
      tmpAux[j] = MNWAUX[cnt++];
    }
    auxNames.push_back(tmpAux);
    auxNames.back().Trim();
  }

  m_lines.push_back("BEGIN OPTIONS");
  if (!auxNames.empty())
  {
    std::stringstream ss;
    ss << "  AUXILIARY ";
    for (size_t i=0; i<auxNames.size(); ++i)
    {
      ss << auxNames[i] << " ";
    }
    m_lines.push_back(ss.str());
  }
  if (*MNWPRNT > -1) m_lines.push_back("  PRINT_INPUT");
  if (*MNWPRNT > 1)  m_lines.push_back("  PRINT_FLOWS");
  if (*IWL2CB > -1)
  {
    g->SetIntVar("MF6_SAVE_FLOWS", 1);
    m_lines.push_back("  SAVE_FLOWS");
  }

  m_lines.push_back("END OPTIONS");
  m_lines.push_back("");
} // NativeExpMf6Maw::impl::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Maw::impl::WriteDimensions ()
{
  using namespace MfData::Packages; 
  m_lines.push_back("BEGIN DIMENSIONS");

  const int *MNWMAX(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX)
  {
    ASSERT(0);
    return;
  }

  int nWells(*MNWMAX);
  std::stringstream ss; 
  ss << "  NMAWWELLS " << nWells;
  m_lines.push_back(ss.str());
  m_lines.push_back("END DIMENSIONS");
  m_lines.push_back("");

} // NativeExpMf6Maw::impl::WriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Maw::impl::WritePackageData ()
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  ASSERT(cn);

  Mf2kNative* nat = m_pack->GetNative();

  using namespace MfData::Packages;

  const int* NMNWVL(0),* MNWMAX(0),* NAUX(0),* MNWNODSZ(0);
  const double *MNW2d(0), *MNWINT(0), *MNWNOD(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
      !p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
      !p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !p->GetField(MNW2pack::MNWINT, &MNWINT) || !MNWINT ||
      !p->GetField(MNW2pack::MNWNOD, &MNWNOD) || !MNWNOD ||
      !p->GetField(MNW2pack::MNWNODSZ, &MNWNODSZ) || !MNWNODSZ ||
      !p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  {
    ASSERT(0);
    return;
  }

  int w = util::RealWidth();

  m_lines.push_back("BEGIN PACKAGEDATA");

  if (w < 13)
    m_lines.push_back("#   wellno       radius       bottom         strt     condeqn  ngwfnodes");
  else
    m_lines.push_back("#   wellno               radius               bottom                 strt     condeqn  ngwfnodes");


  int wellno(0), ngwfnodes(0);
  CStr condeqn("");
  double radius(0), bottom(0), strt(0); 
  for (int i=0; i<*MNWMAX; ++i)
  {
    // logic copied from mf5to6 MawPackageWriter.f90, WriteWellsBlock
    ngwfnodes =     (int)util::ForElement(MNW2d,  2, i+1, *NMNWVL);
    int firstNode = (int)util::ForElement(MNW2d,  4, i+1, *NMNWVL);
    int lastNode = firstNode + abs(ngwfnodes) - 1;
    if (ngwfnodes > 0)
    {
      std::map<CStr, std::vector< std::vector<Real> > >& mymap(nat->SavedRealArrays());
      std::map<CStr, std::vector<Real> >& mymapMult(nat->SavedRealArraysMult());
      std::vector<std::vector<Real>>& top(mymap[ARR_DIS_TOP]), &bot(mymap[ARR_DIS_BOT]);
      std::vector<Real>& topMult(mymapMult[ARR_DIS_TOP]), &botMult(mymapMult[ARR_DIS_BOT]);

      radius = util::ForElement(MNWNOD, 5, firstNode, *MNWNODSZ);
      // loop through cells associated with this MNW and set the bottom to the
      // bottom most elevation in the cells and set start to the top most elevation
      double maxTop(-DBL_MAX), minBot(DBL_MAX), cellTop, cellBot;
      int celli, cellj, cellk, idInLay;
      for (int c = firstNode; c <= lastNode; ++c)
      {
        cellk = (int)util::ForElement(MNWNOD, 1, c, *MNWNODSZ);
        celli = (int)util::ForElement(MNWNOD, 2, c, *MNWNODSZ);
        cellj = (int)util::ForElement(MNWNOD, 3, c, *MNWNODSZ);
        idInLay = cn->IdInLayerFromIjk(celli, cellj, cellk);
        if (1 == cellk)
          cellTop = top[0][idInLay-1] * topMult[0];
        else
          cellTop = bot[cellk-2][idInLay-1] * botMult[cellk-2];
        cellBot = bot[cellk-1][idInLay-1] * botMult[cellk-1];
        if (cellTop > maxTop) maxTop = cellTop;
        if (cellBot < minBot) minBot = cellBot;
      }
      strt = maxTop;
      bottom = minBot;
    }
    else
    {
      int firstInt = (int)util::ForElement(MNWNOD,12, firstNode, *MNWNODSZ);
      int lastInt =  (int)util::ForElement(MNWNOD,13, lastNode, *MNWNODSZ);
      radius = util::ForElement(MNWINT, 5, firstInt, 11);
      strt =   util::ForElement(MNWINT, 1, firstInt, 11);
      bottom = util::ForElement(MNWINT, 2, lastInt, 11);
    }

    int iloss = (int)util::ForElement(MNW2d,3, i+1, *NMNWVL);
    //if (0 == iloss)      condeqn = "INVALID";
    if (1 == iloss)      condeqn = "      THEIM";
    else if (2 == iloss) condeqn = "       SKIN";
    //else if (3 == iloss) condeqn = "INVALID";
    else if (4 == iloss) condeqn = "  SPECIFIED";

    if (0 == iloss || 3 == iloss)
    {
      printf("ERROR: Invalid LOSSTYPE specified in MNW2 file. LOSSTYPE must be THEIM, "
        "SKIN, or SPECIFYcwc. Well will not be written to the MAW file.\n");
      continue;
    }

    wellno++;

    CStr wellnoStr, radiusStr, bottomStr, strtStr, ngwfnodesStr;
    wellnoStr.Format("%10d", wellno);
    ngwfnodesStr.Format("%10d", abs(ngwfnodes));
    radiusStr = STR(radius, -1, w, STR_FULLWIDTH);
    bottomStr = STR(bottom, -1, w, STR_FULLWIDTH);
    strtStr =   STR(strt, -1, w, STR_FULLWIDTH);

    std::stringstream ss;
    ss << wellnoStr << " " << radiusStr << " " << bottomStr << " " << strtStr << " "
       << condeqn << " " << ngwfnodesStr << " ";
    for (int n=0; n<*NAUX; ++n)
    {
      double auxVal = util::ForElement(MNW2d, 31+n, i+1, *NMNWVL);
      CStr auxStr = STR(auxVal,-1,w,STR_FULLWIDTH);
      ss << auxStr << " ";
    }
    m_lines.push_back(ss.str());
  }

  m_lines.push_back("END PACKAGEDATA");
  m_lines.push_back("");
}//NativeExpMf6Maw::impl::WritePackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Maw::impl::WriteConnectionData ()
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  ASSERT(cn);
  Mf2kNative* nat = m_pack->GetNative();

  using namespace MfData::Packages;
  const int* NMNWVL(0),* MNWMAX(0),* NAUX(0),* MNWNODSZ(0);
  const double *MNW2d(0), *MNWINT(0), *MNWNOD(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
      !p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
      !p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !p->GetField(MNW2pack::MNWINT, &MNWINT) || !MNWINT ||
      !p->GetField(MNW2pack::MNWNOD, &MNWNOD) || !MNWNOD ||
      !p->GetField(MNW2pack::MNWNODSZ, &MNWNODSZ) || !MNWNODSZ ||
      !p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  {
    ASSERT(0);
    return;
  }

  int w = util::RealWidth();

  m_lines.push_back("BEGIN CONNECTIONDATA");

  if (w < 13)
    m_lines.push_back("#   wellno  icon     K     I     J      scrn_top     scrn_bot      hk_skin  radius_skin");
  else
    m_lines.push_back("#   wellno  icon     K     I     J              scrn_top             scrn_bot              hk_skin          radius_skin");

  int wellno(0);
  for (int i=0; i<*MNWMAX; ++i)
  {
    // logic copied from mf5to6 MawPackageWriter.f90, WriteWellConnections
    int iloss = (int)util::ForElement(MNW2d,3, i+1, *NMNWVL);
    if (0 == iloss || 3 == iloss) continue;

    wellno++;
    CStr wellnoStr;
    wellnoStr.Format("%10d", wellno);

    double scrn_top(0), scrn_bot(0), hk_skin(0), radius_skin(0);
    CStr scrn_topStr, scrn_botStr, hk_skinStr, radius_skinStr;

    int ngwfnodes = (int)util::ForElement(MNW2d,  2, i+1, *NMNWVL);
    int firstNode = (int)util::ForElement(MNW2d,  4, i+1, *NMNWVL);
    int lastNode  = firstNode + abs(ngwfnodes) - 1;
    if (ngwfnodes > 0)
    {
      std::map<CStr, std::vector< std::vector<Real> > >& mymap(nat->SavedRealArrays());
      std::map<CStr, std::vector<Real> >& mymapMult(nat->SavedRealArraysMult());
      std::vector<std::vector<Real>>& top(mymap[ARR_DIS_TOP]), &bot(mymap[ARR_DIS_BOT]);
      std::vector<Real>& topMult(mymapMult[ARR_DIS_TOP]), &botMult(mymapMult[ARR_DIS_BOT]);
      int icon(0);
      for (int c = firstNode; c <= lastNode; ++c)
      {
        icon++;
        int cellk, celli, cellj, cellId, idInLay;
        cellk = (int)util::ForElement(MNWNOD, 1, c, *MNWNODSZ);
        celli = (int)util::ForElement(MNWNOD, 2, c, *MNWNODSZ);
        cellj = (int)util::ForElement(MNWNOD, 3, c, *MNWNODSZ);
        cellId = cn->IdFromIjk(celli, cellj, cellk);
        idInLay = cn->IdInLayerFromIjk(celli, cellj, cellk);
        // set screen top and bottom to the top and bottom of the cell
        if (1 == cellk)
          scrn_top = top[0][idInLay-1] * topMult[0];
        else
          scrn_top = bot[cellk-2][idInLay-1] * botMult[cellk-2];
        scrn_bot = bot[cellk-1][idInLay-1] * botMult[cellk-1];
        hk_skin = 0;
        if (2 == iloss)
          hk_skin = util::ForElement(MNWNOD, 7, c, *MNWNODSZ);
        else if (4 == iloss)
          hk_skin = util::ForElement(MNWNOD, 11, c, *MNWNODSZ);
        radius_skin = util::ForElement(MNWNOD, 6, c, *MNWNODSZ);

        CStr iconStr, cellIdStr;
        iconStr.Format("%5d", icon);
        cellIdStr = cn->CellIdStringFromId(cellId);
        scrn_topStr = STR(scrn_top,-1,w,STR_FULLWIDTH);
        scrn_botStr = STR(scrn_bot,-1,w,STR_FULLWIDTH);
        hk_skinStr  = STR(hk_skin,-1,w,STR_FULLWIDTH);
        radius_skinStr = STR(radius_skin,-1,w,STR_FULLWIDTH);
        std::stringstream ss;
        ss << wellnoStr << " " << iconStr << " " << cellIdStr << " "
           << scrn_topStr << " " << scrn_botStr << " " << hk_skinStr << " "
           << radius_skinStr;
        m_lines.push_back(ss.str());
      }
    }
    else
    {
      int firstInt =  (int)util::ForElement(MNWNOD,12, firstNode, *MNWNODSZ);
      int lastInt =   (int)util::ForElement(MNWNOD,13, lastNode, *MNWNODSZ);
      for (int c = firstInt; c <= lastInt; ++c)
      {
        int celli, cellj;
        celli = (int)util::ForElement(MNWINT,3,c,11);
        cellj = (int)util::ForElement(MNWINT,4,c,11);
        scrn_top = util::ForElement(MNWINT,1,c,11);
        scrn_bot = util::ForElement(MNWINT,2,c,11);
        hk_skin = 0;
        if (2 == iloss)
          hk_skin = util::ForElement(MNWINT,7,c,11);
        else if (4 == iloss)
          hk_skin = util::ForElement(MNWINT,11,c,11);
        radius_skin = util::ForElement(MNWINT,6,c,11);
        scrn_topStr = STR(scrn_top,-1,w,STR_FULLWIDTH);
        scrn_botStr = STR(scrn_bot,-1,w,STR_FULLWIDTH);
        hk_skinStr  = STR(hk_skin,-1,w,STR_FULLWIDTH);
        radius_skinStr = STR(radius_skin,-1,w,STR_FULLWIDTH);
        int icon(0);
        for (int n = firstNode; n <= lastNode; ++n)
        {
          icon++;
          int cellk = (int)util::ForElement(MNWNOD,1,n,*MNWNODSZ);
          int cellId = cn->IdFromIjk(celli, cellj, cellk);
          CStr cellIdStr = cn->CellIdStringFromId(cellId);
          CStr iconStr;
          iconStr.Format("%5d", icon);
          std::stringstream ss;
          ss << wellnoStr << " " << iconStr << " " << cellIdStr << " "
             << scrn_topStr << " " << scrn_botStr << " " << hk_skinStr << " "
             << radius_skinStr;
          m_lines.push_back(ss.str());
        }
      }
    }
  }

  m_lines.push_back("END CONNECTIONDATA");
  m_lines.push_back("");
}//NativeExpMf6Maw::impl::WriteConnectionData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Maw::impl::WriteStressPeriod ()
{
  MfGlobal* g = m_pack->GetGlobal();
#if 0
  Mf2kNative* nat = m_pack->GetNative();
  int nCells = g->NumRow() * g->NumCol();

  using namespace MfData::Packages;
  MfPackage* p = m_pack->GetPackage();
#endif

  using namespace MfData::Packages;
  const int* ITMP(0),* NMNWVL(0),* MNWMAX(0),* NAUX(0),* MNWNODSZ(0);
  const double *MNW2d(0), *MNWINT(0), *MNWNOD(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(MNW2pack::ITMP, &ITMP) || !ITMP ||
      !p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
      !p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
      !p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !p->GetField(MNW2pack::MNWINT, &MNWINT) || !MNWINT ||
      !p->GetField(MNW2pack::MNWNOD, &MNWNOD) || !MNWNOD ||
      !p->GetField(MNW2pack::MNWNODSZ, &MNWNODSZ) || !MNWNODSZ ||
      !p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  {
    ASSERT(0);
    return;
  }

  if (*ITMP < 0) return;
  int w = util::RealWidth();

  {
    std::stringstream ss;
    ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
    m_lines.push_back(ss.str());
  }

  std::vector<CStr> mawsetting;
  mawsetting.push_back("     STATUS ");
  mawsetting.push_back("       RATE ");
  mawsetting.push_back(" HEAD_LIMIT ");
  mawsetting.push_back("   SHUT_OFF ");
  mawsetting.push_back("  AUXILIARY ");
  int wellno(0);
  for (int i=0; i<*MNWMAX; ++i)
  {
    // logic copied from mf5to6 MawPackageWriter.f90, WriteWellConnections
    int iloss = (int)util::ForElement(MNW2d,3, i+1, *NMNWVL);
    if (0 == iloss || 3 == iloss) continue;

    wellno++;
    CStr wellnoStr;
    wellnoStr.Format("%10d", wellno);

    {
      // STATUS
      double active = util::ForElement(MNW2d,  1, i+1, *NMNWVL); // active
      bool isActive = static_cast<int>(active) == 1;
      CStr statusStr, pad("    ");
      if (w > 13) pad = "            ";
      statusStr.Format("%s%s%s%s", wellnoStr, mawsetting[0], pad,
        isActive ? "  ACTIVE": "INACTIVE");
      m_lines.push_back(statusStr);
      if (isActive)
      {
        // RATE
        double rate = util::ForElement(MNW2d,  5, i+1, *NMNWVL); // Qdes
        CStr rateStr;
        rateStr.Format("%s%s%s", wellnoStr, mawsetting[1], STR(rate,-1,w,STR_FULLWIDTH));
        m_lines.push_back(rateStr);

        double Qlimit = util::ForElement(MNW2d,  6, i+1, *NMNWVL); // Qlimit
        if (Qlimit < 0)
        {
          // HEAD_LIMIT
          double headLimit = util::ForElement(MNW2d,  7, i+1, *NMNWVL); // Hlim
          CStr headLimitStr;
          headLimitStr.Format("%s%s%s", wellnoStr, mawsetting[2],
            STR(headLimit,-1,w,STR_FULLWIDTH));
          m_lines.push_back(headLimitStr);

          // SHUT_OFF
          double QCUT    = util::ForElement(MNW2d,  8, i+1, *NMNWVL); // QCUT
          if (0.0 != QCUT)
          {
            double minRate = util::ForElement(MNW2d,  9, i+1, *NMNWVL); // Qfrcmn
            double maxRate = util::ForElement(MNW2d, 10, i+1, *NMNWVL); // Qfrcmx
            if (QCUT < 0)
            {
              minRate = rate * minRate;
              maxRate = rate * maxRate;
            }
            CStr shutOffStr;
            shutOffStr.Format("%s%s%s %s", wellnoStr, mawsetting[3],
              STR(minRate,-1,w,STR_FULLWIDTH), STR(maxRate,-1,w,STR_FULLWIDTH));
            m_lines.push_back(shutOffStr);
          }
        }

        // AUXILIARY
        for (int n=0; n<*NAUX; ++n)
        {
          double auxVal = util::ForElement(MNW2d, 31+n, i+1, *NMNWVL);
          CStr auxStr;
          auxStr.Format("%s%s%s", wellnoStr, mawsetting[4],
            STR(auxVal,-1,w,STR_FULLWIDTH));
          m_lines.push_back(auxStr);
        }

      }
    }
  }
  m_lines.push_back("END PERIOD");
  m_lines.push_back("");
}//NativeExpMf6Maw::impl::WriteStressPeriod () 


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif