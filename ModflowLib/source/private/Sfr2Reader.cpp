//------------------------------------------------------------------------------
// FILE      Sfr2Reader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Sfr2Reader.h>

#include <Export.h>
#include <private/ListReader/CellIdToIJK.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/MfData/MfGlobal.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>

#define NUMBC     "00. Number of BCs"
#define CELLIDS   "02. Cell IDs"
#define MAPIDSTR  "04. Map ID"
#define FACTOR    "05. Factor"
#define IFACE     "06. IFACE"
#define PROPERTY  "07. Property"
#define MFBC_STRSEGID  "08. Str reach segment ID"
#define MFBC_SFR2_SEG  "14. Segment Property"
#define MFBC_SFR2_FLW  "15. Segment Flow Table"

enum sfr2SegFds_enum { SFR2S_ICALC=0, SFR2S_OUTSEG,SFR2S_IUPSEG,
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
                       SFR2S_HCOND1FACT, SFR2S_HCOND2FACT,
                       SFR2S_NPROP };

//------------------------------------------------------------------------------
/// \brief Reads file name out of the line from the MODFLOW file
//------------------------------------------------------------------------------
static CStr iGetFileFromLine (CStr& a_line)
{
  // read the line
  CStr str, delim("\"");
  if (a_line.Find('"') == -1)
    delim = " ";
  CToken t1(a_line, delim);;
  str = t1.GetNextToken();
  return(t1.GetNextToken());
} // iGetFileFromLine
//------------------------------------------------------------------------------
/// \brief Reads the reach data from the h5 file and fills in the various
/// vectors.
//------------------------------------------------------------------------------
static void iGetReachData (int a_nrow,
                           int a_ncol,
                           CStr& a_line,
                           std::vector<int>& a_krch,
                           std::vector<int>& a_irch,
                           std::vector<int>& a_jrch,
                           std::vector<int>& a_jseg,
                           std::vector<int>& a_ireach,
                           std::vector<Real>& a_reachLen)
{
  CellIdToIJK g(a_nrow, a_ncol);
  CStr file(iGetFileFromLine(a_line)), pathBase("Stream (SFR2)/"), path;
  int nBcs(0);

  std::pair<int, int> myPair(0,1);
  VEC_INT_PAIR indices(1, myPair);
  // read the number of bcs
  {
    path = pathBase + NUMBC;
    H5DataSetReader r(file, path, indices);
    r.GetData(&nBcs, 1);
  }
  indices.back().second = nBcs;

  // get the cell ids
  {
    std::vector<int> cellids;
    path = pathBase + CELLIDS;
    H5DataSetReader r(file, path, indices);
    r.GetData(cellids);
    a_krch.reserve(cellids.size());
    a_irch.reserve(cellids.size());
    a_jrch.reserve(cellids.size());
    for (size_t j=0; j<cellids.size(); j++)
    {
      a_krch.push_back(g.KFromId(cellids[j]));
      a_irch.push_back(g.IFromId(cellids[j]));
      a_jrch.push_back(g.JFromId(cellids[j]));
    }
    if (MfData::MfGlobal::Get().Unstructured()) a_krch = cellids;
  }

  // get the segment ids
  {
    path = pathBase + MFBC_STRSEGID;
    H5DataSetReader r(file, path, indices);
    r.GetData(a_jseg);
    // create the reach ids
    a_ireach.reserve(a_jseg.size());
    int segId(1), rch(1);
    for (size_t j=0; j<a_jseg.size(); j++)
    {
      if (a_jseg[j] != segId)
      {
        segId = a_jseg[j];
        rch = 1;
      }
      a_ireach.push_back(rch++);
    }
  }

  // get the reach length
  {
    indices.push_back(myPair);
    indices.push_back(myPair);
    indices[0].second = 1;
    indices[1].second = nBcs;
    path = pathBase + PROPERTY;
    H5DataSetReader r(file, path, indices);
    r.AllowTypeConversions(true);
    r.GetData(a_reachLen);
  }

} // iGetReachData
//------------------------------------------------------------------------------
/// \brief Fills in the reach data. We get all of the reach data from the H5
/// file the first time we come into this function and then we clear out that
/// data on the last time we come into the function.
//------------------------------------------------------------------------------
void sfr2::GetReachData (int ii,
                         int nrow,
                         int ncol,
                         int& krch,
                         int& irch,
                         int& jrch,
                         int& jseg,
                         int& ireach,
                         Real* Strm,
                         int NStrmD,
                         CStr& line)
{
  // ok to leave static, they are cleared at the bottom of the method
  static std::vector<int>  m_krch, m_irch, m_jrch; // ok to leave static
  static std::vector<int>  m_ireach, m_jseg; // ok to leave static
  static std::vector<Real> m_rchLen; // ok to leave static
  if (m_krch.empty())
  {
    iGetReachData(nrow, ncol, line,
                  m_krch, m_irch, m_jrch, m_jseg, m_ireach, m_rchLen);
  }

  // set the values for the reach
  try
  {
    krch = m_krch.at(ii-1);
    irch = m_irch.at(ii-1);
    jrch = m_jrch.at(ii-1);
    ireach = m_ireach.at(ii-1);
    jseg = m_jseg.at(ii-1);
    Strm[NStrmD*(ii-1)] = m_rchLen.at(ii-1);
  }
  catch (std::out_of_range&)
  {
    ASSERT(0);
  }
 

  // if this is the last reach then clear the data after setting the values
  if (ii == static_cast<int>(m_krch.size()))
  {
    m_krch.clear();
    m_irch.clear();
    m_jrch.clear();
    m_jseg.clear();
    m_ireach.clear();
    m_rchLen.clear();
  }
} // sfr2::GetReachData
//------------------------------------------------------------------------------
/// \brief Reads an attribute on the Stream Flow Table
//------------------------------------------------------------------------------
static int xfpReadAttributeInt (hid_t a_Loc,
                                const char *a_Name,
                                int a_Num,
                                int *a_val)
{
  hid_t   AttId, DataspaceId, DataTypeId;
  herr_t  status;
  htri_t  IsSimple;
  int     Rank;
  std::vector<hsize_t> Dims, MaxDims;
  H5T_class_t DatasetClass;

  AttId = H5Aopen_name(a_Loc, a_Name);
  if (AttId < 0) {
    return AttId;
  }

  // open the datatype and make sure it is an integer type
  DataTypeId = H5Aget_type(AttId);
  if (DataTypeId < 0) {
    H5Aclose(AttId);
    return DataTypeId;
  }

  // Make sure it is an integer type
  DatasetClass = H5Tget_class(DataTypeId);
  if (DatasetClass != H5T_INTEGER) {
    H5Tclose(DataTypeId);
    return -1;
  }

  // Get the dataspace
  DataspaceId = H5Aget_space(AttId);

  // Dataspace must be simply defined
  IsSimple = H5Sis_simple(DataspaceId);
  if (!IsSimple) {
    H5Tclose(DataTypeId);
    H5Sclose(DataspaceId);
    H5Aclose(AttId);
    return -1;
  }

  // Get the rank and dimensions for simple dataspace
  Rank = H5Sget_simple_extent_ndims(DataspaceId);
  // allocate the arrays
  if (Rank > 0)
  {
    Dims.assign(Rank, 0);
    MaxDims.assign(Rank, 0);
  }
  // Get the dimensions
  status = H5Sget_simple_extent_dims(DataspaceId, &Dims[0], &MaxDims[0]);
  if (status < 0 || Rank != 1) {
    H5Tclose(DataTypeId);
    H5Sclose(DataspaceId);
    H5Aclose(AttId);
    return -1;
  }

  // Make sure we have a single integer
  if (Dims[0] != a_Num) {
    H5Sclose(DataspaceId);
    H5Aclose(AttId);
    H5Tclose(DataTypeId);
    return -1;
  }
 
  status = H5Aread(AttId, H5T_NATIVE_INT, a_val);

  // close resources
  H5Tclose(DataTypeId);
  H5Sclose(DataspaceId);
  H5Aclose(AttId);

  return status;
} // xfpReadAttributeInt
//------------------------------------------------------------------------------
/// \brief Reads an attribute on the Stream Flow Table
//------------------------------------------------------------------------------
static void iGetNumToReadFlowTable (const std::string& a_file,
                                    const std::string& a_path,
                                    int& nToRead)
{
  hid_t fId(H5Reader::OpenFile(a_file.c_str()));
  if (fId < 0)
  {
    ASSERT(0);
    return;
  }
  hid_t dId(H5Dopen(fId, a_path.c_str()));
  if (dId < 0)
  {
    ASSERT(0);
    return;
  }
  xfpReadAttributeInt(dId, "NumRows", 1, &nToRead);
} // iGetNumToReadFlowTable
//------------------------------------------------------------------------------
/// \brief Fills in the flow table data
//------------------------------------------------------------------------------
static void iGetFlowTabData (int a_segid,
                             int a_per,
                             CAR_DBL2D& a_flowtab,
                             int& a_nstrpts,
                             Real* Qstage)
{
  using namespace util;
  // count up the number of points for this segment and this period
  int i;
  a_nstrpts = 0;
  for (i=0; i<a_flowtab.GetSize2(); i++)
  {
    if (static_cast<int>(a_flowtab.at(0, i)) == a_segid &&
        static_cast<int>(a_flowtab.at(1, i)) == a_per)
      ++a_nstrpts;
  }

  // fill in Qstage with the data
  int pos(0);
  for (i=0; i<a_flowtab.GetSize2(); i++)
  {
    if (static_cast<int>(a_flowtab.at(0, i)) == a_segid &&
        static_cast<int>(a_flowtab.at(1, i)) == a_per)
    {
      ++pos;
      ForElement(Qstage, pos, a_segid, 150) = (Real)a_flowtab.at(2,i);
      ForElement(Qstage, pos+a_nstrpts, a_segid, 150) = (Real)a_flowtab.at(3,i);
      ForElement(Qstage, pos+(a_nstrpts*2), a_segid, 150) = (Real)a_flowtab.at(4,i);
    }
  }
} // iGetFlowTabData
//------------------------------------------------------------------------------
/// \brief Fills in the segment data.
//------------------------------------------------------------------------------
void sfr2::GetSegData (int Nlst,
                       int Kper,
                       int* Iseg,
                       int* Iotsg,
                       int* Idivar,
                       Real* Seg,
                       Real* Xsec,
                       Real* Qstage,
                       CStr& line)
{
  using namespace util;

  int sz = 26;
  MfData::Get().GetIntVar("SFR_SEG_SIZE", sz);
  CStr file(iGetFileFromLine(line)), pathBase("Stream (SFR2)/"), path;

  CAR_DBL2D dat, flowTab;
  dat.SetSize(SFR2S_NPROP, Nlst, 0.0);
  // read the segment data for the stress period
  {
    size_t num(static_cast<size_t>(SFR2S_NPROP*Nlst));
    std::pair<int, int> myPair(0,1);
    VEC_INT_PAIR indices(3, myPair);
    indices[0].second = SFR2S_NPROP;
    indices[1].second = Nlst;
    indices[2].first = Kper-1;

    path = pathBase + MFBC_SFR2_SEG;
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(&dat.at(0,0), num))
    {
      ErrorStack::Get().PutError("Unable to read SFR2 segment data set."
                                 "Aborting.");
      throw EException(); // this gets caught up one level
    }
  }
  {
    int nToRead(0);
    // get the number of entries in the flow table
    path = pathBase + MFBC_SFR2_FLW;
    iGetNumToReadFlowTable(file, path, nToRead);

    if (nToRead > 0)
    {
      std::pair<int, int> myPair(0, 0);
      VEC_INT_PAIR indices(2, myPair);
      indices.front().second = 5;
      indices.back().second = nToRead;
      size_t num(static_cast<size_t>(5*nToRead));
      H5DataSetReader r(file, path, indices);
      flowTab.SetSize(5, nToRead, 0);
      if (!r.GetData(&flowTab.at(0,0), num))
      {
        ErrorStack::Get().PutError("Unable to read SFR2 Flow Table data set."
                                   "Aborting.");
        throw EException(); // this gets caught up one level
      }
    }
  }

  // publish condfact values to exporter
  if (mfLibExp_Exporting())
  {
    std::vector<Real> condfact1(Nlst, 0), condfact2(Nlst, 0);
    int n1(Nlst), n2(Nlst);
    for (int i=0; i<Nlst; ++i)
    {
      condfact1[i] = (Real)dat.at(SFR2S_HCOND1FACT, i);
      condfact2[i] = (Real)dat.at(SFR2S_HCOND2FACT, i);
    }
    mfLibExp_SfrCondFact(&condfact1[0], n1, &condfact2[0], n2);
  }
  // substitute SFR key value parameters
  if (Parameters::CheckListSubstituteOk())
  {
    Parameters::SubstituteProperty(dat, "SFR", SFR2S_HCOND1, SFR2S_HCOND1FACT);
    Parameters::SubstituteProperty(dat, "SFR", SFR2S_HCOND2, SFR2S_HCOND2FACT);
  }

  // put the data in the various arrays
  int i, j, k, f;
  for (i=0; i<Nlst; i++)
  {
    f = i+1;
    int icalc = static_cast<int>(dat.at(SFR2S_ICALC, i));
    ForElement(Iseg, 1, f, 4) = icalc;
    Iotsg[i] = static_cast<int>(dat.at(SFR2S_OUTSEG, i));
    ForElement(Idivar, 1, f, 2) = static_cast<int>(dat.at(SFR2S_IUPSEG, i));
    ForElement(Idivar, 2, f, 2) = static_cast<int>(dat.at(SFR2S_IPRIOR, i));

    ForElement(Seg, 2, f, sz) = static_cast<Real>(dat.at(SFR2S_FLOW, i));
    ForElement(Seg, 3, f, sz) = static_cast<Real>(dat.at(SFR2S_RUNOFF, i));
    ForElement(Seg, 4, f, sz) = static_cast<Real>(dat.at(SFR2S_ETSW, i));
    ForElement(Seg, 5, f, sz) = static_cast<Real>(dat.at(SFR2S_PPTSW, i));
    ForElement(Seg, 6, f, sz) = static_cast<Real>(dat.at(SFR2S_HCOND1, i));
    ForElement(Seg, 7, f, sz) = static_cast<Real>(dat.at(SFR2S_THICKM1, i));
    ForElement(Seg, 8, f, sz) = static_cast<Real>(dat.at(SFR2S_ELEVUP, i));
    ForElement(Seg, 11, f, sz) = static_cast<Real>(dat.at(SFR2S_HCOND2, i));
    ForElement(Seg, 12, f, sz) = static_cast<Real>(dat.at(SFR2S_THICKM2, i));
    ForElement(Seg, 13, f, sz) = static_cast<Real>(dat.at(SFR2S_ELEVDN, i));

    if (dat.at(SFR2S_ICALC, i) != 3)
    {
      ForElement(Seg, 9, f, sz) = static_cast<Real>(dat.at(SFR2S_WIDTH1, i));
      ForElement(Seg, 10, f, sz) = static_cast<Real>(dat.at(SFR2S_DEPTH1, i));
      ForElement(Seg, 14, f, sz) = static_cast<Real>(dat.at(SFR2S_WIDTH2, i));
      ForElement(Seg, 15, f, sz) = static_cast<Real>(dat.at(SFR2S_DEPTH2, i));
    }
    else
    {
      ForElement(Seg, 9, f, sz) = static_cast<Real>(dat.at(SFR2S_CDPTH, i));
      ForElement(Seg, 10, f, sz) = static_cast<Real>(dat.at(SFR2S_FDPTH, i));
      ForElement(Seg, 14, f, sz) = static_cast<Real>(dat.at(SFR2S_AWDPTH, i));
      ForElement(Seg, 15, f, sz) = static_cast<Real>(dat.at(SFR2S_BWDTH, i));
    }

    ForElement(Seg, 16, f, sz) = static_cast<Real>(dat.at(SFR2S_ROUGHCH, i));
    ForElement(Seg, 17, f, sz) = static_cast<Real>(dat.at(SFR2S_ROUGHBK, i));

    for (j=SFR2S_XSECT, k=0; j<SFR2S_XSECT_END+1; j++, k++)
    {
      ForElement(Xsec, k+1, f, 16) = static_cast<Real>(dat.at(j, i));
    }

    if (icalc == 4) // only do this for ICALC = 4
      iGetFlowTabData(f, Kper, flowTab, ForElement(Iseg, 2, f, 4), Qstage);
  }

} // sfr2::GetSegData

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Sfr2Reader.t.h>

//------------------------------------------------------------------------------
void Sfr2ReaderT::testiGetReachData ()
{
  H5Initialize::Init();
  CStr file;
  file = util::GetTestFilesDirectory() +
         "\\Gms2Mf2k\\sfr2\\stream1\\stream1.h5";

  CStr line;
  line.Format("GMS_HDF5_SFR2_REACH \"%s\" \"SFR2\"", file);
  int nrow(10), ncol(10);

  std::vector<int>  m_krch, m_irch, m_jrch;
  std::vector<int>  m_ireach, m_jseg;
  std::vector<Real> m_rchLen;

  MfData::Get().SetUnstructured(0);
  iGetReachData(nrow, ncol, line, m_krch, m_irch, m_jrch,
                m_jseg, m_ireach, m_rchLen);

  TS_ASSERT(m_krch.size() == 5);
  TS_ASSERT(m_irch.size() == 5);
  TS_ASSERT(m_jrch.size() == 5);
  TS_ASSERT(m_jseg.size() == 5);
  TS_ASSERT(m_ireach.size() == 5);
  TS_ASSERT(m_rchLen.size() == 5);

  int k[5]={1,1,1,1,1},
      i[5]={1,2,3,4,5},
      j[5]={5,5,5,5,5},
      seg[5]={1,1,1,1,1},
      reach[5]={1,2,3,4,5};
  Real len[5]={REAL(5.5142620372051354), REAL(10.109480401542783),
               REAL(10.109480401542781), REAL(10.109480401542777),
               REAL(3.8273081017494661)};

  int q;
  for (q=0; q<5; q++)
  {
    TS_ASSERT_EQUALS(k[q], m_krch[q]);
    TS_ASSERT_EQUALS(i[q], m_irch[q]);
    TS_ASSERT_EQUALS(j[q], m_jrch[q]);
    TS_ASSERT_EQUALS(seg[q], m_jseg[q]);
    TS_ASSERT_EQUALS(reach[q], m_ireach[q]);
    TS_ASSERT_EQUALS(len[q], m_rchLen[q]);
  }
}
//------------------------------------------------------------------------------
void Sfr2ReaderT::testiGetFileFromLine ()
{
  H5Initialize::Init();
  CStr file;

  file = util::GetTestFilesDirectory() +
         "\\Gms2Mf2k\\sfr2\\stream1\\stream1.h5";

  CStr line, file1;
  line.Format("GMS_HDF5_SFR2_REACH \"%s\" \"SFR2\"", file);

  file1 = iGetFileFromLine(line);
  TS_ASSERT(file1 == file);

  line.Format("GMS_HDF5_SFR2_REACH %s SFR2", file);
  file1 = iGetFileFromLine(line);
  TS_ASSERT(file1 == file);

}
//------------------------------------------------------------------------------
void Sfr2ReaderT::testGetSegData ()
{
  H5Initialize::Init();
  CStr file;
  file = util::GetTestFilesDirectory() +
         "\\Gms2Mf2k\\sfr2\\stream1\\stream1.h5";

  CStr line, file1;
  line.Format("GMS_HDF5_01 \"%s\" \"SFR2\" 1", file);

  int sz=26;
  std::vector<int> Iseg(1, 0), Iotsg(1, 0), Idivar(2, 0);;
  std::vector<Real> Seg(sz, 0), Xsec(16, 0), Qstage(150*3, 0);
  sfr2::GetSegData(1, 1, &Iseg[0], &Iotsg[0], &Idivar[0], &Seg[0],
                   &Xsec[0], &Qstage[0], line);

  TS_ASSERT_EQUALS(Iseg[0] , 0);
  TS_ASSERT_EQUALS(Iotsg[0], 0);
  TS_ASSERT_EQUALS(Idivar[0], 0);
  TS_ASSERT_EQUALS(Idivar[1], 0);

  Real seg[26] = {0,0.5,1,2,3,-30,31,32,33,34,
                  -20,21,22,23,24,4,5,0,0,0,
                  0,0,0,0,0,0},
       xsec[16]= {0,1,2,3,4,5,6,7,720,1430,
                  478,1221,219,944,605,496};
  int i;
  for (i=0; i<sz; i++)
    TS_ASSERT_EQUALS(seg[i], Seg[i]);
  for (i=0; i<16; i++)
    TS_ASSERT_EQUALS(xsec[i], Xsec[i]);
  
  ParamList *list;
  Parameters::GetParameterList(&list);
  list->Clear();
  Param p1("SFR1", -30, "SFR", 6, 1, 20);
  list->PushBack(&p1);
  Param p2("SFR2", -20, "SFR", 6, 1, 20);
  list->PushBack(&p2);

  sfr2::GetSegData(1, 1, &Iseg[0], &Iotsg[0], &Idivar[0], &Seg[0],
                   &Xsec[0], &Qstage[0], line);
  seg[5] = 6;
  seg[10] = 6;
  for (i=0; i<sz; i++)
    TS_ASSERT_EQUALS(seg[i], Seg[i]);
  for (i=0; i<16; i++)
    TS_ASSERT_EQUALS(xsec[i], Xsec[i]);
  list->Clear();
}

#endif

