//------------------------------------------------------------------------------
// FILE      H5LstPack.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\H5\H5LstPack.h>

// 3. Standard library headers
#include <hash_map>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>
#include <private/ListReader/CellIdToIJK.h>
#include <private/MfData/MfExport/private/H5/H5BcList.h>
#include <private/MfData/MfExport/private/H5/H5Strings.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>
#include <private/util/EReadAsciiFile.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
class H5SfrPack
{
public:
  H5SfrPack (H5LstPack::impl& a_) :
     m_p(a_)
   , m_nstrm(0), m_nss(0), m_isfropt(0), m_istrm(0), m_nistrmd(0), m_nstrmd(0)
   , m_strm(0), m_iseg(0), m_iotsg(0), m_idivar(0), m_seg(0), m_xsec(0)
   , m_qstage(0)
      {}

  bool Init ();
  CStr Ln2 ();
  CStr Ln6 (int& a_itmp);
  void WriteReach ();
  void WriteUseLast (int a_itmp);
  void WriteSegData ();
  void WriteSegFlow ();

  H5LstPack::impl& m_p;
  const int *m_nstrm, *m_nss, *m_isfropt, *m_istrm, *m_nistrmd, *m_nstrmd;
  const Real *m_strm;
  const int *m_iseg, *m_iotsg, *m_idivar;
  const Real *m_seg, *m_xsec, *m_qstage;
  int m_numReaches;
};

//------------------------------------------------------------------------------
class H5StrPack
{
public:
  H5StrPack (H5LstPack::impl& a_) :
      m_p(a_),
        m_istrpb(0), m_nss(0), m_ntrib(0), m_ndiv(0), m_icalc(0),
        m_istcb1(0), m_istcb2(0), m_itmp(0), m_irdflg(0), m_iptflg(0),
        m_istrm(0), m_nstrem(0), m_mxstrm(0), m_itrbar(0), m_idivar(0),
        m_constv(0), m_strm(0), m_streamFields(11), m_npstr(0)
      {
      }

      CStr Write (int& a_itmp);
      bool Init ();
      void FillBcData ();
      void WriteSegData ();
      void SegTribToh5 ();

      H5LstPack::impl& m_p;
      const int  *m_istrpb, *m_nss, *m_ntrib, *m_ndiv, *m_icalc, *m_istcb1,
                 *m_istcb2, *m_itmp, *m_irdflg, *m_iptflg, *m_istrm,
                 *m_nstrem, *m_mxstrm, *m_itrbar, *m_idivar, *m_npstr;
      const Real *m_constv, *m_strm;
      int         m_streamFields;

};

//------------------------------------------------------------------------------
class H5LstPack::impl
{
  friend H5StrPack;
  friend H5SfrPack;

public:
  impl (  MfData::MfGlobal* a_g
        , MfData::MfPackage* a_p
        , Mf2kNative* a_n
        , H5BcList* a_lbc
        )
    :   m_glob(a_g), m_p(a_p), m_native(a_n), m_lbc(a_lbc), m_itmp(0)
      , m_maxBc(0), m_np(0), m_vCellids(0), m_vIface(0), m_vCellgrp(0)
      , m_nBcs(0), m_nAux(0), m_nDataFields(0), m_istrm(0), m_data(0)
      , m_PHIRAMP(0), m_ifaceIdx(-1), m_cellgrpIdx(-1), m_seawatIdx0(-1)
      , m_seawatIdx1(-1), m_condfactIdx(-1), m_qfactIdx(-1), m_maxBcSoFar(0)
      , m_prevSpNumBc(0), m_grid(a_g->NumRow(),a_g->NumCol()), m_istrmSize(5)
      , m_usg(0), m_unstructured(0)
      , m_istrmCellIdIdx(0), m_dataStartIdx(0), m_cellIdOffset(0)
  {
    m_usg = a_g->ModelType() == MfData::USG;
    m_unstructured = a_g->Unstructured() ? 1 : 0;
  } // impl
  ~impl () {}

  CStr Write (int& a_maxBc);
  bool Init ();
  bool SetupForWrite ();
  void SetupForWriteEnd ();
  void SetType ();
  void SetParamType ();
  void SetAuxFieldIdx ();
  void FillBcData ();
  void Set_dSize ();
  void ExistingBcData (int a_start);
  int  GetBcIndex (int a_cellid);
  void GetArrayOfUsedBcIndices (std::vector<char>& a_vAlreadyUsed);
  void WriteUseLast ();
  void GetBcDataFromPar ();
  void HandleUseLast ();
  void FillData ();
  void GetSeawatBcIdx (int& a_seawatBcIdx0, int& a_seawatBcIdx1);
  CStr WriteBcData ();
  void LstPar ();
  void WriteMapIds ();
  bool ReadListBcMapIdComments (
      const CStr& a_fname
    , const CStr& a_ftype
    , std::vector<CStr>& a_mapids);
  void FillInListBcMapIds (
      const std::vector<CStr>& a_mapids
    , const std::vector<int>& a_cellGrp
    , std::vector<CStr>& a_mapids2);
  void WriteListBcMapIdsToH5 (
      TxtExporter *a_exp
    , const std::vector<CStr>& a_mapids
    , const CStr& a_type);
  CStr StrPack (int& a_itmp);
  CStr SfrLn2 ();
  CStr SfrLn6 (int& a_itmp);
  CStr ClnWel (int& a_itmp);
  void AddToSkippedParameters ();

  MfData::MfPackage* m_p;
  MfData::MfGlobal*  m_glob;
  Mf2kNative*        m_native;
  H5BcList*          m_lbc;

  CStr                m_type, m_ptype, m_file, m_packName;
  const int          *m_itmp, *m_maxBc, *m_np;
  std::vector<int>   *m_vCellids, *m_vIface, *m_vCellgrp, m_idxs;
  const int          *m_nBcs, *m_nAux, *m_nDataFields, *m_istrm;
  int                 m_nFields, m_modIdx, m_sp, m_maxIdx, m_minIdx, m_dSize,
                      m_istrmSize, m_istrmCellIdIdx;
  const Real         *m_data, *m_PHIRAMP;
  std::vector<CStr>   m_fieldStrings;
  int                 m_ifaceIdx, m_cellgrpIdx, m_seawatIdx0, m_seawatIdx1,
                      m_condfactIdx, m_qfactIdx;
  int                *m_maxBcSoFar, m_prevSpNumBc, m_dataStartIdx,
                      m_cellIdOffset;
  std::vector<Param>  m_pList;
  CellIdToIJK         m_grid;
  CAR_DBL2D           m_bcData;
  bool                m_usg, m_unstructured;
};


typedef std::vector< std::pair<int, int> > VEC_INT_PAIR;
typedef stdext::hash_map<int, VEC_INT_PAIR> HMAP;
//------------------------------------------------------------------------------
namespace
{
  enum sfr2SegFds_enum {
    SFR2S_ICALC=0, SFR2S_OUTSEG,SFR2S_IUPSEG,
    SFR2S_IPRIOR, SFR2S_FLOW,
    SFR2S_RUNOFF, SFR2S_ETSW, SFR2S_PPTSW, SFR2S_ROUGHCH,
    SFR2S_ROUGHBK, SFR2S_CDPTH, SFR2S_FDPTH, SFR2S_AWDPTH,
    SFR2S_BWDTH, SFR2S_HCOND1, SFR2S_THICKM1, SFR2S_ELEVUP,
    SFR2S_WIDTH1, SFR2S_DEPTH1, SFR2S_HCOND2,SFR2S_THICKM2,
    SFR2S_ELEVDN, SFR2S_WIDTH2, SFR2S_DEPTH2,
    SFR2S_XSECT,
    SFR2S_XS02,SFR2S_XS03,SFR2S_XS04,SFR25_XS05,SFR2S_XS06,
    SFR2S_XS07,SFR2S_XS08,SFR2S_XS09,SFR25_XS10,SFR2S_XS11,
    SFR2S_XS12,SFR2S_XS13,SFR2S_XS14,SFR25_XS15,
    SFR2S_XSECT_END,
    SFR2S_COND1FACT, SFR2S_COND2FACT,
    SFR2S_NPROP };

//------------------------------------------------------------------------------
/// \brief writes an int attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeInt (hid_t a_Loc,
                          const char *a_Name,
                          int a_Number,
                          int *a_val)
{
  hid_t   AttId, SpaceId;
  hsize_t Dims;
  herr_t  status;

  /* Create the dataspace; */
  Dims = a_Number;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Aopen_name(a_Loc, a_Name);
  if (AttId < 0) {
    AttId = H5Acreate(a_Loc, a_Name, H5T_NATIVE_INT, SpaceId, H5P_DEFAULT);
    if (AttId < 0) {
      H5Sclose(SpaceId);
      return AttId;
    }
  }

  status = H5Awrite(AttId, H5T_NATIVE_INT, a_val);

  /* close resources */
  H5Sclose(SpaceId);
  H5Aclose(AttId);

  return status;
} // xfpWriteAttributeInt
//------------------------------------------------------------------------------
void iSizeBcDataArray (CStr& a_type, int a_maxIdx, CAR_DBL2D& a_bcData)
{
  a_bcData.Clear();
  int start(0), num(1);
  if (a_type == "River")
  { // stage, cond, elev, factor, RBDTHK, RIVDEN
    a_bcData.SetSize(6, a_maxIdx+1, 0);
    start = 3;
  }
  else if (a_type == "Specified Head")
  { // startHead, endHead, factor1, factor2, CHDDENSOPT, CHDDEN
    a_bcData.SetSize(6, a_maxIdx+1, 0);
    start = 2;
    num = 2;
  }
  else if (a_type == "Drain")
  { // elev, cond, factor, DRNBELEV
    a_bcData.SetSize(4, a_maxIdx+1, 0);
    start = 2;
  }
  else if (a_type == "General Head")
  { // head, cond, factor, GHBELEV, GHBDENS
    a_bcData.SetSize(5, a_maxIdx+1, 0);
    start = 2;
  }
  else if (a_type == "Drain Return")
  { // elev, cond, layR, rowR, colR, Rfprop, factor
    a_bcData.SetSize(7, a_maxIdx+1, 0);
    start = 6;
  }
  else if (a_type == "Well" ||
           a_type == "WEL (CLN)")
  { // Q, factor, WELDENS
    a_bcData.SetSize(3, a_maxIdx+1, 0);
    start = 1;
  }
  else if (a_type == "Stream")
  { // stage, cond, bot. elev., top elev., width, slope, rough, factor
    a_bcData.SetSize(8, a_maxIdx+1, 0);
    start = 7;
  }
  else if (a_type == "Stream (SFR2)")
  { // RCHLEN
    a_bcData.SetSize(1, a_maxIdx+1, 0);
  }

  for (int i=start; (i-start)<num; i++)
  { // this initializes the condfact to 1.0 or for CHD the headfact
    for (int j=0; j<a_bcData.GetSize2(); j++)
      a_bcData.at(i, j) = 1.0;
  }
} // iSizeBcDataArray
//------------------------------------------------------------------------------
static std::vector<int>& iBcCellIds (int a_, CStr a_type)
{
  static std::map<int, std::map<CStr, std::vector<int> > > m_;
  return m_[a_][a_type];
} // iBcCellIds
//------------------------------------------------------------------------------
static std::vector<int>& iBcIface (int a_, CStr a_type)
{
  static std::map<int, std::map<CStr, std::vector<int> > > m_;
  return m_[a_][a_type];
} // iBcIface
//------------------------------------------------------------------------------
static std::vector<int>& iBcCellGrp (int a_, CStr a_type)
{
  static std::map<int, std::map<CStr, std::vector<int> > > m_;
  return m_[a_][a_type];
} // iBcCellGrp
//------------------------------------------------------------------------------
static int& iMaxBcSoFar (int a_, CStr a_type)
{
  static std::map<int, std::map<CStr, int> > m_;
  return m_[a_][a_type];
} // iMaxBcSoFar
//------------------------------------------------------------------------------
#pragma warning(disable:4503)
static HMAP& iBcIdxMap (int a_, CStr a_type)
{
  static std::map<int, std::map<CStr, HMAP>> m_;
  return m_[a_][a_type];
} // iBcIdxMap
} // unnamed namespace

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5LstPack::H5LstPack (
  MfData::MfGlobal* a_g
, MfData::MfPackage* a_p
, Mf2kNative* a_n
, H5BcList* a_lbc) : m_p(new H5LstPack::impl(a_g, a_p, a_n, a_lbc))
{
} // H5LstPack::H5LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5LstPack::~H5LstPack ()
{
  if (m_p) delete(m_p);
} // H5LstPack::~H5LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::Write (int& a_maxBc)
{
  return m_p->Write(a_maxBc);
} // H5LstPack::Write
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::StrPack (int& a_itmp)
{
  return m_p->StrPack(a_itmp);
} // H5LstPack::StrPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::SfrLn2 ()
{
  return m_p->SfrLn2();
} // H5LstPack::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::SfrLn6 (int& a_itmp)
{
  return m_p->SfrLn6(a_itmp);
} // H5LstPack::SfrLn6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::ClnWel (int& a_itmp)
{
  return m_p->ClnWel(a_itmp);
} // H5LstPack::ClnWel
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::LstPar ()
{
 m_p->LstPar();
} // H5LstPack::LstPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::WriteMapIds ()
{
  m_p->WriteMapIds();
} // H5LstPack::WriteMapIds

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::Write (int& a_maxBc)
{
  CStr rval;
  if (SetupForWrite())
  {
    FillBcData();
    rval = WriteBcData();
    if (*m_itmp > 0 || !m_pList.empty()) a_maxBc = (int)m_vCellids->size();
  }
  return rval;
} // H5LstPack::impl::Write
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5LstPack::impl::Init ()
{
  using namespace MfData::Packages;
  m_packName = m_p->PackageName();
  if (LPRM == m_packName) GetPackNameFromParameter(m_p, m_packName);
  if (SFRLine1 == m_packName || SFRLine2 == m_packName ||
      SFRLine5 == m_packName || SFRLine6 == m_packName) m_packName = SFR;
  SetType();
  SetParamType();
  m_sp = m_glob->GetCurrentPeriod();
  m_modIdx = (int)m_glob->CurModIdx();
  m_vCellids = &iBcCellIds(m_modIdx, m_type);
  m_vIface   = &iBcIface(m_modIdx, m_type);
  m_vCellgrp = &iBcCellGrp(m_modIdx, m_type);
  if (!GetBcData(m_p, m_packName, &m_nBcs, &m_nFields, &m_nAux,
                 &m_data, &m_nDataFields, m_fieldStrings)) return false;
  SetAuxFieldIdx();
  m_file.Format("%s.h5", m_native->GetExp()->GetBaseFileName());
  return true;
} // H5LstPack::impl::Init
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5LstPack::impl::SetupForWrite ()
{
  using namespace MfData::Packages;
  if (!Init()) return false;
  if (!m_p->GetField(ListPack::ITMP, &m_itmp) || !m_itmp ||
      !m_p->GetField(ListPack::MAXBC, &m_maxBc) || !m_maxBc ||
      !m_p->GetField(ListPack::NP, &m_np) || !m_np) return false;
  SetupForWriteEnd();
  return true;
} // H5LstPack::impl::SetupForWrite
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::SetupForWriteEnd ()
{
  m_maxBcSoFar = &iMaxBcSoFar(m_modIdx, m_type);
  if (*m_maxBcSoFar < *m_nBcs) *m_maxBcSoFar = *m_nBcs;
  m_prevSpNumBc = (int)m_vCellids->size();
} // H5LstPack::impl::SetupForWriteEnd
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::SetType ()
{
  using namespace MfData::Packages;
  if (DRN == m_packName)        m_type = "Drain";
  else if (DRT == m_packName)   m_type = "Drain Return";
  else if (RIV == m_packName)   m_type = "River";
  else if (WEL == m_packName)   m_type = "Well";
  else if (GHB == m_packName)   m_type = "General Head";
  else if (CHD == m_packName)   m_type = "Specified Head";
  else if (STRSP == m_packName) m_type = "Stream";
  else if (SFR == m_packName)   m_type = "Stream (SFR2)";
  else if (CLNWEL == m_packName) m_type = "WEL (CLN)";
  else m_type = "";
} // H5LstPack::impl::SetType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::SetParamType ()
{
  using namespace MfData::Packages;
  if (DRN == m_packName)        m_ptype = "DRN";
  else if (DRT == m_packName)   m_ptype = "DRT";
  else if (RIV == m_packName)   m_ptype = "RIV";
  else if (WEL == m_packName)   m_ptype = "Q";
  else if (GHB == m_packName)   m_ptype = "GHB";
  else if (CHD == m_packName)   m_ptype = "CHD";
  else if (STRSP == m_packName) m_ptype = "STR";
  else if (SFR == m_packName)   m_ptype = "SFR";
  else                          m_ptype = "";
  m_pList = MfExportUtil::GetParamsOfType(m_ptype);
} // H5LstPack::impl::SetParamType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::SetAuxFieldIdx ()
{
  for (size_t q=0; q<m_fieldStrings.size(); q++)
  {
    // iface can come from AUX fields
    if (m_fieldStrings[q].CompareNoCase("IFACE") == 0)
      m_ifaceIdx = static_cast<int>(q);
    if (m_fieldStrings[q].CompareNoCase("CELLGRP") == 0)
      m_cellgrpIdx = static_cast<int>(q);
    if (m_fieldStrings[q].CompareNoCase("CONDFACT") == 0)
      m_condfactIdx = static_cast<int>(q);
    if (m_fieldStrings[q].CompareNoCase("QFACT") == 0)
      m_qfactIdx = static_cast<int>(q);

    // seawat stuff
    if (   m_fieldStrings[q].CompareNoCase("RBDTHK") == 0
        || m_fieldStrings[q].CompareNoCase("DRNBELEV") == 0
        || m_fieldStrings[q].CompareNoCase("WELDENS") == 0
        || m_fieldStrings[q].CompareNoCase("GHBELEV") == 0
        || m_fieldStrings[q].CompareNoCase("CHDDENSOPT") == 0
       )
      m_seawatIdx0 = static_cast<int>(q);
    if (   m_fieldStrings[q].CompareNoCase("RIVDEN") == 0
        || m_fieldStrings[q].CompareNoCase("GHBDENS") == 0
        || m_fieldStrings[q].CompareNoCase("CHDDEN") == 0
       )
      m_seawatIdx1 = static_cast<int>(q);
  }
} // H5LstPack::impl::SetAuxFieldIdx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::FillBcData ()
{
  // Go through the data that was passed in. It is possible that there are
  // already BCs active in this stress period that have come from parameters.
  ExistingBcData(m_dataStartIdx);
  // Write the use last flag. If there are any parameters then we always set
  // the use last flag to false because we don't know what parameters may
  // have changed since the last stress period.
  WriteUseLast();
  // size the data array for this stress period
  Set_dSize();
  iSizeBcDataArray(m_type, m_maxIdx, m_bcData);
  // get the bc data for the current stress period if there are parameters
  GetBcDataFromPar();
  HandleUseLast();
  // fill in the bcData. Use the indices that we calculated above to figure
  // out where to put the data associated with a particular BC.
  FillData();
} // H5LstPack::impl::FillBcData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void H5LstPack::impl::Set_dSize ()
{
  m_dSize = m_maxIdx;
  if (*m_nBcs < 1)
  {
    m_dSize = static_cast<int>(m_vCellids->size());
    --m_dSize;
  }
} // H5LstPack::impl::Set_dSize
//------------------------------------------------------------------------------
/// \brief Go through the data that was passed in. It is possible that there are
/// already BCs active in this stress period that have come from parameters.
//------------------------------------------------------------------------------
void H5LstPack::impl::ExistingBcData (int a_start)
{
  m_maxIdx = (int)m_vCellids->size()-1;
  m_minIdx = (int)m_vCellids->size()+1;
  m_idxs.reserve(*m_nBcs);
  int ck, ci, cj, cellId;
  for (int i=a_start; i<(*m_nBcs+a_start) && *m_itmp > -1; i++)
  {
    cellId = -1;
    if (m_istrm)
    {
      ck = m_istrm[i*(m_istrmSize)+0];
      ci = m_istrm[i*(m_istrmSize)+1];
      cj = m_istrm[i*(m_istrmSize)+2];
      if (m_usg) cellId = m_istrm[i*(m_istrmSize)+m_istrmCellIdIdx];
    }
    else
    {
      ck = static_cast<int>(m_data[i*(*m_nDataFields)+0]);
      ci = static_cast<int>(m_data[i*(*m_nDataFields)+1]);
      cj = static_cast<int>(m_data[i*(*m_nDataFields)+2]);
      if (m_usg) cellId = (int)m_data[i*(*m_nDataFields)+0] - m_cellIdOffset;
    }
    if (-1 == cellId) cellId = m_grid.IdFromIJK(ci, cj, ck);
    m_idxs.push_back(GetBcIndex(cellId));
    if (m_idxs.back() > m_maxIdx)
      m_maxIdx = m_idxs.back();
    if (m_idxs.back() < m_minIdx)
      m_minIdx = m_idxs.back();
    if (m_ifaceIdx != -1)
    { // get the iface values if they exist
      m_vIface->at(m_idxs.back()) =
        static_cast<int>(m_data[i*(*m_nDataFields)+m_ifaceIdx]);
    }
    if (m_cellgrpIdx != -1)
    { // get the cellgrp values if they exist
      m_vCellgrp->at(m_idxs.back()) = 
        static_cast<int>(m_data[i*(*m_nDataFields)+m_cellgrpIdx]);
    }
  }
} // H5LstPack::impl::ExistingBcData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int H5LstPack::impl::GetBcIndex (int a_cellid)
{
  HMAP& hMap(iBcIdxMap(m_modIdx, m_type));
  HMAP::iterator it;

  size_t i;
  int    idx(-1);
  bool   found(false);
  // see if this cellid is in the hash_map
  it = hMap.find(a_cellid);
  if (it != hMap.end())
  {
    // inside the hash_map we have a vector of int pairs
    // the map has cellid as the key and associated with cellid is a vector
    // of pairs. In the pair the 'first' is the array index (the spot in the
    // data array [this is where we store stage, cond, elev...] associated with
    // the cellid). The 'second' in the pair is the stress period number of the
    // last time this index was used.
    int     arrayIndex, lastUsedInSp;
    std::pair<int, int> *p;
    // loop through the vector and find the first index that hasn't been used
    // in this stress period
    for (i=0; i<it->second.size() && !found; i++)
    {
      p = &it->second.at(i);
      arrayIndex = p->first;
      lastUsedInSp = p->second;

      // see if the BC has been used yet
      if (lastUsedInSp > m_sp)
        continue;

      idx = p->first;
      // update the lastUsedInSp field
      p->second = m_sp + 1;
      found = true;
    }
  }

  // no available spot for this BC so make a new one
  if (!found)
  {
    // this means that this is the first time a BC is in this cell OR
    // in this stress period we now have more BCs in this cell than in any
    // previous stress period
    if (it == hMap.end())
    {
      hMap.insert(std::make_pair(a_cellid, VEC_INT_PAIR()));
      it = hMap.find(a_cellid);
    }

    idx = static_cast<int>(m_vCellids->size());
    m_vCellids->push_back(a_cellid);
    std::pair<int, int> p1(idx, m_sp+1);
    it->second.push_back(p1);

    m_vIface->push_back(6);
    if (m_type.CompareNoCase("well") == 0)
      m_vIface->back() = 0;
    m_vCellgrp->push_back(-1);
  }

  return idx;
} // H5LstPack::impl::GetBcIndex
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::GetArrayOfUsedBcIndices (std::vector<char>& a_vAlreadyUsed)
{
  HMAP& hMap(iBcIdxMap(m_modIdx, m_type));
  HMAP::iterator it(hMap.begin());
  std::pair<int, int> *p;
  int arrayIndex, lastUsedInSp;

  for ( ; it != hMap.end(); it++)
  {
    for (size_t i=0; i<it->second.size(); i++)
    {
      p = &it->second.at(i);
      arrayIndex = p->first;
      lastUsedInSp = p->second;

      // see if the BC has been used yet
      if (lastUsedInSp > m_sp)
      {
        a_vAlreadyUsed.at(arrayIndex) = true;
      }
    }
  }
} // H5LstPack::impl::GetArrayOfUsedBcIndices
//------------------------------------------------------------------------------
/// \brief Write the use last flag. If there are any parameters then we always
/// set the use last flag to false because we don't know what parameters may
/// have changed since the last stress period.
//------------------------------------------------------------------------------
void H5LstPack::impl::WriteUseLast ()
{
  CStr path;
  path.Format("%s/%s", m_type, MFBC_USELAST);
  H5DataSetWriterSetup s(m_file, path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, m_sp-1), n2write(1,1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  int tmpItmp(*m_itmp < 0 ? 1 : 0);
  if (!m_pList.empty()) // see comment above
    tmpItmp = 0;
  w.WriteData(&tmpItmp, 1);
} // H5LstPack::impl::WriteUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::GetBcDataFromPar ()
{
  if (*m_np > 0 && m_prevSpNumBc > 0)
  {
    CAR_DBL2D tmpData;
    iSizeBcDataArray(m_type, m_prevSpNumBc-1, tmpData);
    CStr path;
    path.Format("%s/%s", m_type, MFBC_DATA);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[0].second = tmpData.GetSize1();
    indices[1].second = m_prevSpNumBc;
    indices[2].first = m_sp-1;
    H5DataSetReader r(m_file, path, indices);
    r.GetData(&tmpData.at(0,0),
      static_cast<size_t>(tmpData.GetSize1()*m_prevSpNumBc));
    for (int i=0; i<tmpData.GetSize1(); i++)
    {
      for (int j=0; j<tmpData.GetSize2(); j++)
      {
        m_bcData.at(i,j) = tmpData.at(i,j);
      }
    }
  }
} // H5LstPack::impl::GetBcDataFromPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::HandleUseLast ()
{
  if (*m_itmp < 0)
  { // get the previous stress period
    CAR_DBL2D prevData;
    iSizeBcDataArray(m_type, m_dSize, prevData);
    CStr path;
    path.Format("%s/%s", m_type, MFBC_DATA);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[0].second = prevData.GetSize1();
    indices[1].second = prevData.GetSize2();
    indices[2].first = m_sp-2;
    H5DataSetReader r(m_file, path, indices);
    r.GetData(&prevData.at(0,0),
              static_cast<size_t>(prevData.GetSize1()*m_prevSpNumBc));
    // If there are parameters then we can't overwrite the current sp with
    // the previous. So we have to check to see which bcs have been used by
    // the parameters and skip those ones.
    std::vector<char> vAlreadyUsed(m_prevSpNumBc, 0);
    GetArrayOfUsedBcIndices(vAlreadyUsed);
    for (int i=0; i<m_bcData.GetSize1(); i++)
    {
      for (int j=0; j<m_bcData.GetSize2(); j++)
      {
        if (!vAlreadyUsed.at(j))
          m_bcData.at(i,j) = prevData.at(i,j);
      }
    }
    // If there are no parameters then get any key values for this type of
    // parameter and set those to zero (0.0) in the bcData array so that they
    // don't affect the model.
    if (*m_np < 1)
    {
      std::set<Real> keys;
      for (int i=0; i<(int)m_pList.size(); i++)
      {
        keys.insert(static_cast<Real>(m_pList.at(i).m_key));
      }
      for (int i=0; !keys.empty() && i<m_bcData.GetSize1(); i++)
      {
        for (int j=0; j<m_bcData.GetSize2(); j++)
        {
          if (keys.find(static_cast<Real>(m_bcData.at(i,j))) != keys.end())
          {
            m_bcData.at(i,j) = 0.0;
          }
        }
      }
    }
  }
} // H5LstPack::impl::HandleUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::FillData ()
{
  // fill in the bcData. Use the indices that we calculated above to figure
  // out where to put the data associated with a particular BC.
  int nBcFields = m_nFields - *m_nAux - 3; // k,i,j
  int seawatBcIdx0(-1), seawatBcIdx1(-1);
  GetSeawatBcIdx(seawatBcIdx0, seawatBcIdx1);
  for (int i=0; i<*m_nBcs && *m_itmp > -1; i++)
  {
    for (int j=0; j<nBcFields; j++)
    {
      m_bcData.at(j, m_idxs.at(i)) =
        static_cast<double>(m_data[i*(*m_nDataFields)+3+j]);
    }
    if (m_condfactIdx != -1)
    {
      m_bcData.at(nBcFields, m_idxs.at(i)) =
        static_cast<double>(m_data[i*(*m_nDataFields)+m_condfactIdx]);
    }
    if (m_qfactIdx != -1)
    {
      m_bcData.at(nBcFields, m_idxs.at(i)) =
        static_cast<double>(m_data[i*(*m_nDataFields)+m_qfactIdx]);
    }
    if (m_seawatIdx0 != -1 && seawatBcIdx0 != -1)
    {
      m_bcData.at(seawatBcIdx0, m_idxs.at(i)) =
        static_cast<double>(m_data[i*(*m_nDataFields)+m_seawatIdx0]);
    }
    if (m_seawatIdx1 != -1 && seawatBcIdx1 != -1)
    {
      m_bcData.at(seawatBcIdx1, m_idxs.at(i)) =
        static_cast<double>(m_data[i*(*m_nDataFields)+m_seawatIdx1]);
    }
  }
} // H5LstPack::impl::FillData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::GetSeawatBcIdx (
  int& a_seawatBcIdx0
, int& a_seawatBcIdx1)
{
  a_seawatBcIdx0 = a_seawatBcIdx1 = -1;
  if (m_type == "River")
  { // stage, cond, elev, factor, RBDTHK, RIVDEN
    a_seawatBcIdx0 = 4;
    a_seawatBcIdx1 = 5;
  }
  else if (m_type == "Specified Head")
  { // startHead, endHead, factor1, factor2, CHDDENSOPT, CHDDEN
    a_seawatBcIdx0 = 4;
    a_seawatBcIdx1 = 5;
  }
  else if (m_type == "Drain")
  { // elev, cond, factor, DRNBELEV
    a_seawatBcIdx0 = 3;
  }
  else if (m_type == "General Head")
  { // head, cond, factor, GHBELEV, GHBDENS
    a_seawatBcIdx0 = 3;
    a_seawatBcIdx1 = 4;
  }
  else if (m_type == "Drain Return")
  { // elev, cond, layR, rowR, colR, Rfprop, factor
  }
  else if (m_type == "Well")
  { // Q, factor, WELDENS
    a_seawatBcIdx0 = 2;
  }
  else if (m_type == "Stream")
  { // stage, cond, bot. elev., top elev., width, slope, rough, factor
  }
  else if (m_type == "Stream (SFR2)")
  { // RCHLEN
  }
} // H5LstPack::impl::GetSeawatBcIdx
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::WriteBcData ()
{
  if (m_bcData.GetSize1() * m_bcData.GetSize2() > 0)
    m_lbc->WriteList(m_sp, 0, m_type, m_file, *m_vCellids, m_bcData, *m_vIface);

  CStr file1, line;
  util::StripPathFromFilename(m_file, file1);
  line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, m_type, m_sp);
  if ((!m_pList.empty() && m_maxIdx+1 > 0) ||
      *m_itmp > 0)
  {
    return line;
  }
  return CStr();
} // H5LstPack::impl::WriteBcData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::LstPar ()
{
  if (!Init()) return;
  AddToSkippedParameters();
  SetupForWriteEnd();

  // get some info about the parameter
  int                 start;
  Real                key;
  if (!MfData::Packages::GetParamKeyAndDataStart(m_p, key, start)) return;

  int tmpItmp=1;
  m_itmp = &tmpItmp;
  ExistingBcData(start);
  iSizeBcDataArray(m_type, m_maxIdx, m_bcData);
  int np(1);
  if (!m_np) m_np = &np;
  GetBcDataFromPar();

  int bcIdx, fieldIdx, dataIdx;
  std::map<int, int> srcDestIdxs;
  MfData::Packages::GetParamSrcDestFields(m_packName, m_fieldStrings, srcDestIdxs);
  // fill in the bcData
  int nBcFields = m_nFields - *m_nAux - 3; // k,i,j
  for (int i=start; i<(*m_nBcs+start); i++)
  {
    for (int j=0; j<nBcFields; j++)
    {
      bcIdx = m_idxs.at(i-start);
      dataIdx = i*(*m_nDataFields)+3+j;
      fieldIdx = j;
      if (srcDestIdxs.find(j+3) != srcDestIdxs.end())
        fieldIdx = srcDestIdxs[j+3];
      m_bcData.at(fieldIdx, bcIdx) = static_cast<double>(m_data[dataIdx]);
      if (j != fieldIdx)
        m_bcData.at(j, bcIdx) = key;
    }
  }

  m_lbc->WriteList(m_sp, 0, m_type, m_file, *m_vCellids,
                   m_bcData, *m_vIface);
} // H5LstPack::impl::LstPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::WriteMapIds ()
{
  using namespace MfData::Packages;
  if (!m_glob) return;

  const char *name(0);
  MfPackage *pack(m_glob->GetPackage("NAM1"));
  if (!pack || !pack->GetField(NameFile::FNAME, &name) || !name)
    return;

  CStr fname(name);
  if (fname.IsEmpty()) return;

  util::StripExtensionFromFilename(fname, fname);

  m_modIdx = (int)m_glob->CurModIdx();
  // loop through the list packages and see if mapids are in there
  // chd, riv, drn, ghb
  int i;
  std::vector<CStr> mapids, mapids2;
  CStr packages[4] = {DRN, RIV, GHB, CHD};
  for (i=0; i<4; i++)
  {
    // see if cellgrp exists for this type
    m_packName = packages[i];
    SetType();
    std::vector<int> &vCellgrp(iBcCellGrp(m_modIdx, m_type));
    if (vCellgrp.empty()) continue;
    // if all values are -1 then don't do anything
    bool flag = false;
    for (size_t j=0; !flag && j<vCellgrp.size(); j++)
    {
      if (vCellgrp.at(j) != -1)
        flag = true;
    }
    if (!flag) continue;

    // read the mapids from the file
    if (m_native->GetExp()->FileTypeExists(packages[i]))
    {
      // read the comments at the top of the file
      if (ReadListBcMapIdComments(fname, packages[i], mapids))
      {
        // fill an array of CStrings with the map ids
        FillInListBcMapIds(mapids, vCellgrp, mapids2);
        if (!mapids2.empty())
        {
          // write the map ids to the h5 file
          WriteListBcMapIdsToH5(m_native->GetExp(), mapids2, m_type);
        }
      }
    }
  }
} // H5LstPack::impl::WriteMapIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5LstPack::impl::ReadListBcMapIdComments (
  const CStr& a_fname
, const CStr& a_ftype
, std::vector<CStr>& a_mapids
)
{
  bool found = false;
  a_mapids.resize(0);
  CStr fname;
  fname.Format("%s.%s", a_fname, a_ftype);

  EReadAsciiFile r(fname.c_str());

  if (!r.OpenFile())
  {
    CStr msg("Error opening file: " + fname + ".");
    ErrorStack::Get().PutError(msg);
    return found;
  }

  r.UseExceptions();
  try
  {
    CStr str, line;
    while (r.GetLine(&line))
    {
      r.ReadData(str);
      if (str == "#GMSCOMMENT")
      {
        line.Replace("#GMSCOMMENT ", "");
        a_mapids.push_back(line);
        found = true;
      }
      else if (found)
      {
        r.CloseFile();
      }
    }
  }
  catch (ioexception &e)
  {
    CStr msg(e.what());
    if (!msg.IsEmpty())
    {
      ErrorStack::Get().PutError(msg);
    }
  }
  return found;
} // H5LstPack::impl::ReadListBcMapIdComments
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::FillInListBcMapIds (
  const std::vector<CStr>& a_mapids
, const std::vector<int>& a_cellGrp
, std::vector<CStr>& a_mapids2
)
{
  a_mapids2.resize(0);
  try
  {
    a_mapids2.reserve(a_cellGrp.size());
    for (size_t j=0; j<a_cellGrp.size(); j++)
    {
      a_mapids2.push_back(a_mapids.at(a_cellGrp.at(j)-1));
    }
  }
  catch (std::out_of_range&)
  {
    ASSERT(0);
    a_mapids2.clear();
  }
} // H5LstPack::impl::FillInListBcMapIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::WriteListBcMapIdsToH5 (
  TxtExporter *a_exp
, const std::vector<CStr>& a_mapids
, const CStr& a_type
)
{
  CStr f(a_exp->GetBaseFileName()), path;
  f += ".h5";

  // figure out the max string length
  size_t i;
  int maxStrLen(-1);
  for (i=0; i<a_mapids.size(); i++)
  {
    if (a_mapids.at(i).GetLength() > maxStrLen)
      maxStrLen = a_mapids.at(i).GetLength();
  }
  maxStrLen++;
  if (maxStrLen < 2)
    return;

  size_t nToWrite(a_mapids.size() * maxStrLen);
  CAR_CHR2D chars;
  chars.SetSize((int)a_mapids.size(), maxStrLen, '\0');
  for (i=0; i<a_mapids.size(); i++)
  {
    strcpy(&chars.at((int)i, 0), a_mapids.at(i).c_str());
  }

  path.Format("%s/%s", a_type, MFBC_MAPIDSTR);
  H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, nToWrite);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&chars.at(0,0), nToWrite);

  // write the string length attribute
  hid_t fid(H5DataReader::GetFileId(f));
  hid_t dataId(H5Dopen(fid, path));
  if (dataId > -1)
  {
    xfpWriteAttributeInt(dataId, MFBC_MAX_STR_LEN, 1, &maxStrLen);
    H5Dclose(dataId);
  }
} // H5LstPack::impl::WriteListBcMapIdsToH5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::StrPack (int& a_itmp)
{
  CStr rval;
  H5StrPack s(*this); 
  rval = s.Write(a_itmp);
  return rval;
} // H5LstPack::impl::StrPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::SfrLn2 ()
{
  CStr rval;
  H5SfrPack s(*this); 
  rval = s.Ln2();
  return rval;
} // H5LstPack::impl::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::SfrLn6 (int& a_itmp)
{
  CStr rval;
  H5SfrPack s(*this); 
  rval = s.Ln6(a_itmp);
  return rval;
} // H5LstPack::impl::SfrLn6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5LstPack::impl::ClnWel (int& a_itmp)
{
  CStr rval;
  // do some setup to change things for CLN Wells
  MfPackage p(*m_p), *tmpP(m_p);
  TmpPackageNameChanger chg(&p, "CLNWEL");
  m_p = &p;

  const int *nnpwcln(0), *nodes(0), *itmpcln(0), *numbc(0);
  m_p->GetField(Packages::ListPack::ITMPCLN, &itmpcln);
  m_p->GetField("NNPWCLN", &nnpwcln);
  m_p->GetField("NODES", &nodes);
  m_p->GetField(Packages::ListPack::NUMBC, &numbc);
  if (!itmpcln || !nnpwcln || !nodes || !numbc)
  {
    ASSERT(0);
    return rval;
  }
  m_p->SetField(Packages::ListPack::ITMP, itmpcln);
  m_p->SetField(Packages::ListPack::NUMBC, nnpwcln);
  m_dataStartIdx = *numbc;
  m_cellIdOffset = *nodes;
  rval = Write(a_itmp);
  m_p = tmpP;
  return rval;
} // H5LstPack::impl::ClnWel
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5LstPack::impl::AddToSkippedParameters ()
{
  if (m_glob->ModelType() != MfData::MF2K &&
      m_glob->ModelType() != MfData::SEAWAT) return;

  using namespace MfData::Packages;
  const char *nm(0), *typ(0);
  if (!m_p->GetField(ListParameter::PNAME, &nm) || !nm ||
      !m_p->GetField(ListParameter::PTYPE, &typ) || !typ) return;

  CStr name(nm), type(typ);
  CStr parToSkip;
  m_glob->GetStrVar("Pars2Skip", parToSkip);
  parToSkip += " ";
  parToSkip += name;
  m_glob->SetStrVar("Pars2Skip", parToSkip);
} // H5LstPack::impl::AddToSkippedParameters

////////////////////////////////////////////////////////////////////////////////
/// \class H5StrPack
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5StrPack::Write (int& a_itmp)
{
  CStr rval; 
  if (Init())
  {
    m_p.ExistingBcData(0);
    iSizeBcDataArray(m_p.m_type, m_p.m_maxIdx, m_p.m_bcData);
    // size the data array for this stress period
    m_p.Set_dSize();
    m_p.HandleUseLast();
    FillBcData();
    rval = m_p.WriteBcData();
    WriteSegData();
    if (*m_itmp > 0 || !m_p.m_pList.empty())
    {
      a_itmp = (int)m_p.m_vCellids->size();
    }
    m_p.m_itmp = &a_itmp;
    m_p.WriteUseLast();
  }
  return rval;
} // H5StrPack::Write
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5StrPack::Init ()
{
  using namespace MfData;
  using namespace MfData::Packages;
  MfPackage* a_p = m_p.m_p;
  if (a_p->GetField(STRpack::MXACTS, &m_istrpb) && m_istrpb &&
      a_p->GetField(STRpack::NSS, &m_nss) && m_nss &&
      a_p->GetField(STRpack::NTRIB, &m_ntrib) && m_ntrib &&
      a_p->GetField(STRpack::NDIV, &m_ndiv) && m_ndiv &&
      a_p->GetField(STRpack::ICALC, &m_icalc) && m_icalc &&
      a_p->GetField(STRpack::CONSTV, &m_constv) && m_constv &&
      a_p->GetField(STRpack::ISTCB1, &m_istcb1) && m_istcb1 &&
      a_p->GetField(STRpack::ISTCB2, &m_istcb2) && m_istcb2 &&
      a_p->GetField(STRpack::ITMP, &m_itmp) && m_itmp &&
      a_p->GetField(STRpack::IRDFLG, &m_irdflg) && m_irdflg &&
      a_p->GetField(STRpack::IPTFLG, &m_iptflg) && m_iptflg &&
      a_p->GetField(STRpack::STRM, &m_strm) && m_strm &&
      a_p->GetField(STRpack::ISTRM, &m_istrm) && m_istrm &&
      a_p->GetField(STRpack::NSTREM, &m_nstrem) && m_nstrem &&
      a_p->GetField(STRpack::MXSTRM, &m_mxstrm) && m_mxstrm &&
      a_p->GetField(STRpack::ITRBAR, &m_itrbar) && m_itrbar &&
      a_p->GetField(STRpack::IDIVAR, &m_idivar) && m_idivar &&
      a_p->GetField(STRpack::NPSTR, &m_npstr) && m_npstr &&
      a_p->GetField(ListPack::ITMP, &m_p.m_itmp) && m_p.m_itmp)
  {
    m_p.Init();
    m_p.m_maxBc = m_mxstrm;
    m_p.m_data = m_strm;
    m_p.m_nBcs = m_nstrem;
    m_p.m_nFields = m_streamFields;
    m_p.m_istrm = m_istrm;
    m_p.m_np = m_npstr;

    m_p.SetupForWriteEnd();
    return true;
  }
  return false;
} // H5StrPack::Init
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5StrPack::FillBcData ()
{
  if (*m_p.m_itmp < 1) return;
  int nBcFields = 7;
  for (int i=0; i<*m_p.m_nBcs; i++)
  {
    for (int j=0; j<nBcFields; j++)
    {
      m_p.m_bcData.at(j, m_p.m_idxs.at(i)) =
        static_cast<double>(m_p.m_data[i*(m_p.m_nFields)+1+j]);
    }
    // CONDFACT
    m_p.m_bcData.at(7, i) = 1.0;
  }
  // handle any stream parameters
  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    Param p;
    for (size_t ii=0; ii<list->Size(); ii++)
    {
      list->At(ii, &p);
      if (p.m_type == "STR")
      { // move the current conductance value to the condfact
        // and set conductance to the key value
        int stop = p.m_str_start + p.m_str_nbc;
        for (int i=p.m_str_start; i<stop; i++)
        {
          m_p.m_bcData.at(7, i) = m_p.m_bcData.at(1, i);
          m_p.m_bcData.at(1, i) = p.m_key;
        }
        p.m_str_start = -1;
        p.m_str_nbc = -1;
        list->UpdateParameter(&p);
      }
    }
  }
} // H5StrPack::FillBcData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5StrPack::WriteSegData ()
{
  SegTribToh5(); // only on 1st stress period

  { // write the segment ids and flow
    std::vector<int> segmentIds;
    std::vector<Real> flow;
    int lastSegmentId = m_istrm[3] - 1;
    segmentIds.assign(*m_nss, 0);
    flow.assign(*m_nss, 0);
    for (int i=0; i<*m_nss; ++i) segmentIds[i] = i+1;
    for (int i = 0; i<*m_p.m_nBcs; ++i)
    {
      int segmentId = m_istrm[i*5+3];
      if (segmentId != lastSegmentId)
      {
        flow[segmentId-1] = m_strm[i*(m_p.m_nFields)+0];
        lastSegmentId = segmentId;
      }
    }

    CStr path;
    path.Format("%s/%s", m_p.m_type, MFBC_SEGFLW);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start(2, 0), n2write(2, flow.size());
    start[1] = m_p.m_sp - 1;
    n2write[0] = flow.size();
    n2write[1] = 1;
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.AllowTypeConversions(true);
    w.WriteData(&flow.at(0), flow.size());
  }

} // H5StrPack::WriteSegData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5StrPack::SegTribToh5 ()
{
  if (m_p.m_sp != 1) return;
  CStr path;
  {
    path.Format("%s/%s", m_p.m_type, MFBC_NSEG);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1,1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(m_nss, 1);
  }
  { // write the str reach segment ids
    std::vector<int> reachIds;
    for (int i=0; i<*m_p.m_nBcs; i++) reachIds.push_back(m_istrm[i*5+3]);
    path.Format("%s/%s", m_p.m_type, MFBC_STRSEGID);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, reachIds.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&reachIds.at(0), reachIds.size());
  }
  { // write the segment ids
    std::vector<int> segmentIds;
    segmentIds.assign(*m_nss, 0);
    for (int i=0; i<*m_nss; ++i) segmentIds[i] = i+1;
    path.Format("%s/%s", m_p.m_type, MFBC_SEGID);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, segmentIds.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&segmentIds.at(0), segmentIds.size());
  }
  { // write the tributaries
    CAR_INT2D itrib;
    itrib.SetSize(*m_nss, 10, 0);
    for (int segment = 0; segment < *m_nss; ++segment)
    {
      for (int trib = 0; trib < *m_ntrib; ++trib)
      {
        itrib.at(segment, trib) = m_itrbar[trib*(*m_nss) + segment];
      }
    }
    path.Format("%s/%s", m_p.m_type, MFBC_ITRIB);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 2);
    std::vector<hsize_t> start(2, 0), n2write(2, 0);
    n2write[0] = *m_nss;
    n2write[1] = 10;
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&itrib.at(0,0), *m_nss*10);
  }
  { // write the upstream segment ids
    path.Format("%s/%s", m_p.m_type, MFBC_UPID);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, *m_nss);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(m_idivar, *m_nss);
  }
} // H5StrPack::SegTribToh5

////////////////////////////////////////////////////////////////////////////////
/// \class H5SfrPack
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool H5SfrPack::Init ()
{
  using namespace MfData::Packages;
  if (m_p.Init())
  {
    MfData::MfPackage* a_pSFRLine1 = m_p.m_glob->GetPackage(SFRLine1);
    MfData::MfPackage* a_pSFRLine2 = m_p.m_glob->GetPackage(SFRLine2);
    MfData::MfPackage* a_pSFRLine5 = m_p.m_glob->GetPackage(SFRLine5);
    MfData::MfPackage* a_pSFRLine6 = m_p.m_glob->GetPackage(SFRLine6);
    if (a_pSFRLine5 &&
        a_pSFRLine5->GetField(SFRpack::ITMP, &m_p.m_itmp) && m_p.m_itmp &&
        a_pSFRLine6 &&
        a_pSFRLine6->GetField(SFRpack::ISEG, &m_iseg) && m_iseg &&
        a_pSFRLine6->GetField(SFRpack::IOTSG, &m_iotsg) && m_iotsg &&
        a_pSFRLine6->GetField(SFRpack::IDIVAR, &m_idivar) && m_idivar &&
        a_pSFRLine6->GetField(SFRpack::SEG, &m_seg) && m_seg &&
        a_pSFRLine6->GetField(SFRpack::XSEC, &m_xsec) && m_xsec &&
        a_pSFRLine6->GetField(SFRpack::QSTAGE, &m_qstage) && m_qstage)
    {
    }

    if (a_pSFRLine1->GetField(SFRpack::NSTRM, &m_nstrm) && m_nstrm &&
        a_pSFRLine1->GetField(SFRpack::NSS, &m_nss) && m_nss &&
        a_pSFRLine1->GetField(SFRpack::ISFROPT, &m_isfropt) && m_isfropt &&

        a_pSFRLine2->GetField(SFRpack::ISTRM, &m_istrm) && m_istrm &&
        a_pSFRLine2->GetField(SFRpack::NISTRMD, &m_nistrmd) && m_nistrmd &&
        a_pSFRLine2->GetField(SFRpack::STRM, &m_strm) && m_strm &&
        a_pSFRLine2->GetField(SFRpack::NSTRMD, &m_nstrmd) && m_nstrmd)
    {
      m_numReaches = *m_nstrm;
      if (m_numReaches < 0) m_numReaches *= -1;
      m_p.m_istrm = m_istrm;
      m_p.m_nBcs = &m_numReaches;
      m_p.m_istrmSize = *m_nistrmd;
      m_p.m_istrmCellIdIdx = 5;
      return true;
    }
  }
  return false;
} // H5SfrPack::Init
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5SfrPack::Ln2 ()
{
  CStr rval;
  if (Init())
  {
    int itmp(1);
    m_p.m_itmp = &itmp;
    CStr file1;
    util::StripPathFromFilename(m_p.m_file, file1);
    rval.Format("GMS_HDF5_SFR2_REACH \"%s\" \"SFR2\"", file1);
    if (!m_p.m_glob->Unstructured()) m_p.m_usg = false;
    m_p.ExistingBcData(0);
    if (!m_p.m_glob->Unstructured()) m_p.m_usg = true;
    iSizeBcDataArray(m_p.m_type, m_p.m_maxIdx, m_p.m_bcData);
    for (int i = 0; i < m_numReaches; ++i)
    { // fill in the bcData
      m_p.m_bcData.at(0, m_p.m_idxs.at(i)) =
        static_cast<double>(m_strm[i*(*m_nstrmd)]);
    }
    //m_p.WriteBcData();
    int nSp = m_p.m_glob->NumPeriods();
    for (int i=0; i<nSp; ++i)
    {
      m_p.m_sp = i+1;
      m_p.WriteBcData();
    }
    WriteReach();
  }
  return rval;
} // H5SfrPack::Ln2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5SfrPack::WriteReach ()
{
  CStr path;
  { // 08. Str reach segment ID (size: NSTRM)
    std::vector<int> reachIds;
    for (int i = 0; i < m_numReaches; ++i)
    {
      reachIds.push_back(m_istrm[i*(*m_nistrmd)+3]);
    }
    path.Format("%s/%s", m_p.m_type, MFBC_STRSEGID);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, reachIds.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&reachIds.at(0), reachIds.size());
  }
  { // 13. Number of Segments (NSS)
    path.Format("%s/%s", m_p.m_type, MFBC_NSEG);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1,1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(m_nss, 1);
  }
} // H5SfrPack::WriteReach
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5SfrPack::Ln6 (int& a_itmp)
{
  CStr rval;
  if (Init())
  {
    WriteSegData();
    WriteSegFlow();
    CStr file1;
    util::StripPathFromFilename(m_p.m_file, file1);
    if (*m_p.m_itmp > 0 || !m_p.m_pList.empty())
    {
      if (*m_p.m_itmp < 1) a_itmp = *m_nss;
      rval.Format("GMS_HDF5_01 \"%s\" \"SFR2\" %d", file1, m_p.m_sp);
    }
    WriteUseLast(a_itmp);
  }
  return rval;
} // H5SfrPack::Ln6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5SfrPack::WriteUseLast (int a_itmp)
{
  CStr path;
  path.Format("%s/%s", m_p.m_type, MFBC_USELAST);
  H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, m_p.m_sp-1), n2write(1,1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  int itmpToWrite = a_itmp > 0 ? 0 : 1;
  w.WriteData(&itmpToWrite, 1);
} // H5SfrPack::WriteUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5SfrPack::WriteSegData ()
{
  using util::ForElement;
  int sz=26;
  m_p.m_glob->GetIntVar("SFR_SEG_SIZE", sz);
  // 14. Segment Property
  CAR_DBL2D v;
  v.SetSize(SFR2S_NPROP, *m_nss, 0.0);

  for (int i = 0; i < *m_nss; ++i)
  {
    int segnum = i + 1;
    v.at(SFR2S_ICALC, i)   = ForElement(m_iseg, 1, segnum, 4);
    v.at(SFR2S_OUTSEG, i)  = m_iotsg[i];
    v.at(SFR2S_IUPSEG, i)  = ForElement(m_idivar, 1, segnum, 2);
    v.at(SFR2S_IPRIOR, i)  = ForElement(m_idivar, 2, segnum, 2);
    v.at(SFR2S_FLOW, i)    = ForElement(m_seg, 2, segnum, sz);
    v.at(SFR2S_RUNOFF, i)  = ForElement(m_seg, 3, segnum, sz);
    v.at(SFR2S_ETSW, i)    = ForElement(m_seg, 4, segnum, sz);
    v.at(SFR2S_PPTSW, i)   = ForElement(m_seg, 5, segnum, sz);
    v.at(SFR2S_ROUGHCH, i) = ForElement(m_seg, 16, segnum, sz);
    v.at(SFR2S_ROUGHBK, i) = ForElement(m_seg, 17, segnum, sz);
    v.at(SFR2S_CDPTH, i)   = ForElement(m_seg, 9, segnum, sz);
    v.at(SFR2S_FDPTH, i)   = ForElement(m_seg, 10, segnum, sz);
    v.at(SFR2S_AWDPTH, i)  = ForElement(m_seg, 14, segnum, sz);
    v.at(SFR2S_BWDTH, i)   = ForElement(m_seg, 15, segnum, sz);
    v.at(SFR2S_HCOND1, i)  = ForElement(m_seg, 6, segnum, sz);
    v.at(SFR2S_THICKM1, i) = ForElement(m_seg, 7, segnum, sz);
    v.at(SFR2S_ELEVUP, i)  = ForElement(m_seg, 8, segnum, sz);
    v.at(SFR2S_WIDTH1, i)  = ForElement(m_seg, 9, segnum, sz);
    v.at(SFR2S_DEPTH1, i)  = ForElement(m_seg, 10, segnum, sz);
    v.at(SFR2S_HCOND2, i)  = ForElement(m_seg, 11, segnum, sz);
    v.at(SFR2S_THICKM2, i) = ForElement(m_seg, 12, segnum, sz);
    v.at(SFR2S_ELEVDN, i)  = ForElement(m_seg, 13, segnum, sz);
    v.at(SFR2S_WIDTH2, i)  = ForElement(m_seg, 14, segnum, sz);
    v.at(SFR2S_DEPTH2, i)  = ForElement(m_seg, 15, segnum, sz);
      
    for (int j = 0; j < 16; ++j)
    {
      int jj = j + 1;
      v.at(SFR2S_XSECT+j, i) = ForElement(m_xsec, jj, segnum, 16);
    }
      
    v.at(SFR2S_COND1FACT, i) = 1.0;
    v.at(SFR2S_COND2FACT, i) = 1.0;
  }

  // handle any sfr parameters
  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    Param p;
    for (size_t ii=0; ii<list->Size(); ii++)
    {
      list->At(ii, &p);
      if (p.m_type == "SFR")
      { // Move the current conductance value to the condfact
        // and set conductance to the key value.
        // Must be done for both Hc1fact and Hc2fact.
        int stop = p.m_str_start + p.m_str_nbc;
        for (int i = p.m_str_start; i < stop; i++)
        {
          int nseg = ForElement(m_iseg, 3, i, 4);
          v.at(SFR2S_COND1FACT, nseg-1) = ForElement(m_seg, 6, i, sz);
          v.at(SFR2S_HCOND1, nseg-1) = p.m_key;

          v.at(SFR2S_COND2FACT, nseg-1) = ForElement(m_seg, 11, i, sz);
          v.at(SFR2S_HCOND2, nseg-1) = p.m_key;
        }
        p.m_str_start = -1;
        p.m_str_nbc = -1;
        list->UpdateParameter(&p);
      }
    }
  }

  { // write data for 14. Segment Property
    CStr path;
    path.Format("%s/%s", m_p.m_type, MFBC_SEGP);
    H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start(3, 0), n2write(3,1);
    n2write[0] = v.GetSize1();
    n2write[1] = v.GetSize2();
    start[2] = m_p.m_sp - 1;
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&v.at(0,0), static_cast<size_t>(v.GetSize1()*v.GetSize2()));
  }
} // H5SfrPack::WriteSegData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5SfrPack::WriteSegFlow ()
{
  using util::ForElement;
  // get the number of QSTAGE items for this stress period to size array
  int numQstage(0);
  for (int i = 0; i < *m_nss; ++i)
  {
    int segnum = i + 1;
    int icalc = ForElement(m_iseg, 1, segnum, 4);

    // when ICALC == 4 add NSTRPTS items
    if (icalc == 4)
      numQstage += ForElement(m_iseg, 2, segnum, 4);
  }
  if (numQstage > 0)
  {
    CAR_DBL2D v;
    v.SetSize(5, numQstage, 0.0);

    int entryNum = 0;
    for (int i = 0; i < *m_nss; ++i)
    {
      int segnum = i + 1;
      if (ForElement(m_iseg, 1, segnum, 4) == 4)
      {
        int nstrpts = ForElement(m_iseg, 2, segnum, 4);
        for (int j = 0; j < nstrpts; ++j)
        {
          int jj = j + 1;
          v.at(0, entryNum+j) = segnum;
          v.at(1, entryNum+j) = m_p.m_sp;
          v.at(2, entryNum+j) = ForElement(m_qstage, jj, segnum, 150);

          jj += nstrpts;
          v.at(3, entryNum+j) = ForElement(m_qstage, jj, segnum, 150);

          jj += nstrpts;
          v.at(4, entryNum+j) = ForElement(m_qstage, jj, segnum, 150);
        }
        entryNum++;
      }
    }
    { // 15. Segment Flow Table
      CStr path;
      path.Format("%s/%s", m_p.m_type, MFBC_SEGFLWT);

      // get size of existing data
      H5DataSetReader r(m_p.m_file, path);
      int tmpNrow(0);
      r.GetAtt("NumRows", tmpNrow);

      // write to end of existing data
      H5DataSetWriterSetup s(m_p.m_file, path, H5T_NATIVE_DOUBLE, 2);
      std::vector<hsize_t> start(2, 0), n2write(2, 1);
      n2write[0] = v.GetSize1();
      n2write[1] = v.GetSize2();
      start[1] = tmpNrow;
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&v.at(0,0), static_cast<size_t>(v.GetSize1()*v.GetSize2()));
      int numRows = tmpNrow + numQstage;
      w.WriteAtt("NumRows", numRows);
    }
  }
} // H5SfrPack::WriteSegFlow



