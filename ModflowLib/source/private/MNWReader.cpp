//------------------------------------------------------------------------------
// FILE      MNWReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MNWReader.h>

#include <math.h>
#include <sstream>

#include <private/H5DataReader/H5DataSetReader.h>
#include <private/H5DataReader/H5VecCStrReader.h>
#include <private/util/util.h>


#define NUMBC     "00. Number of BCs"
#define CELLIDS   "02. Cell IDs"
#define MFBC_NAME "03. Name"
#define MAPIDSTR  "04. Map ID"
#define FACTOR    "05. Factor"
#define IFACE     "06. IFACE"
#define PROPERTY  "07. Property"

int MNW2_NVL ()
{
  return 13;
} // MNW2_NVL
//------------------------------------------------------------------------------
/// \brief Reads file name out of the line from the MODFLOW file
//------------------------------------------------------------------------------
static CStr iGetFileFromLine (const CStr& a_line)
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
/// \brief
//------------------------------------------------------------------------------
bool mnw::ReadH5Data(std::vector<int>& a_cellids,
                     CAR_DBL2D& a_well2,
                     std::vector<CStr>& a_names,
                     const CStr& a_line)
{
  CStr file(iGetFileFromLine(a_line)), pathBase("Multi-Node Well/"), path;
  int numberH5Wells(0);

  std::stringstream ss;
  ss << a_line;
  CStr gmsCard, h5file, h5path;
  int sp(1);
  ss >> gmsCard >> h5file >> h5path >> h5path >> sp;
  // get stress period
  //size_t stressPeriodStart = a_line.find_last_of(" ");
  //if (stressPeriodStart != std::string::npos)
  //  sscanf(a_line.c_str() + stressPeriodStart, "%d", &sp);
  //else
  //  return false;

  std::pair<int, int> myPair(0,1);
  VEC_INT_PAIR indices(1, myPair);
  // read the number of bcs
  {
    path = pathBase + NUMBC;
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(&numberH5Wells, 1))
      return false;
  }
  indices.back().second = numberH5Wells;

  // get the cell ids
  a_cellids.clear();
  {
    path = pathBase + CELLIDS;
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(a_cellids) || a_cellids.size() != (size_t)numberH5Wells)
      return false;
  }

  // get the bc data
  a_well2.SetSize(H5_SIZE, (int)a_cellids.size(), 0.0);
  {
    path = pathBase + PROPERTY;
    indices.clear();
    indices.push_back(std::make_pair(0, H5_SIZE));
    indices.push_back(std::make_pair(0, (int)a_cellids.size()));
    indices.push_back(std::make_pair(sp - 1, 1));
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(&a_well2.at(0, 0), H5_SIZE*a_cellids.size()))
      return false;
  }

  // get the well names
  a_names.clear();
  {
    path = pathBase + MFBC_NAME;
    H5VecCStrReader r(file, path);
    r.FillInStrings(a_names);
  }
  return true;
} // mnw::ReadH5Data
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool mnw::ReadMfH5Data(int a_itmp,
                       double* a_well2,
                       char* a_mnwsite,
                       double* a_mnwflgs,
                       const char* a_line)
{
  bool success = true;
  try
  {
    using util::ForElement;
    CAR_DBL2D p;
    std::vector<int> cellids;
    std::vector<CStr> names;

    if (!ReadH5Data(cellids, p, names, a_line))
      return false;

    int nwell2 = (int)cellids.size();
    int j = 1;
    CStr currWell(names.at(0));
    for (int i = 0; i < nwell2; ++i)
    {
      if (util::lrint(p.at(H5_ACTIVE, i)) == ACTIVE)
      {
        int cellid = cellids.at(i);
        ForElement(a_well2, W2_NODE, j, W2_SIZE) = cellid;
        ForElement(a_well2, W2_QDES, j, W2_SIZE) = p.at(H5_QDES, i);
        double qwval = p.at(H5_QWVAL, i);
        ForElement(a_well2, W2_QWVAL, j, W2_SIZE) = qwval;
        double rw = p.at(H5_RW, i);
        ForElement(a_well2, W2_RW, j, W2_SIZE) = rw;
        double skin = p.at(H5_SKIN, i);
        ForElement(a_well2, W2_SKIN, j, W2_SIZE) = skin;
        ForElement(a_well2, W2_HLIM, j, W2_SIZE) = p.at(H5_HLIM, i);
        ForElement(a_well2, W2_HREF, j, W2_SIZE) = p.at(H5_HREF, i);
        ForElement(a_mnwflgs, MNWFLGS_HREF, j, MNWFLGS_SIZE) = p.at(H5_HREF, i);
        int iwgrp = util::lrint(p.at(H5_IWGRP, i));
        ForElement(a_well2, W2_IWGRP, j, W2_SIZE) = iwgrp;
        ForElement(a_well2, W2_C, j, W2_SIZE) = p.at(H5_C, i);
        ForElement(a_well2, W2_QFRCMN, j, W2_SIZE) = p.at(H5_QFRCMN, i);
        ForElement(a_well2, W2_QFRCMX, j, W2_SIZE) = p.at(H5_QFRCMX, i);
        ForElement(a_well2, W2_ID, j, W2_SIZE) = p.at(H5_WELLID, i);

        int dd = util::lrint(p.at(H5_DD, i));
        ForElement(a_mnwflgs, MNWFLGS_DD, j, MNWFLGS_SIZE) = dd;
        int qcut = util::lrint(p.at(H5_QCUT, i));
        ForElement(a_mnwflgs, MNWFLGS_QCUT, j, MNWFLGS_SIZE) = qcut;

        // Retrieve the number of numeric items that would have been missing if
        // this was a text line.  For most items zero means it wasn't specified.
        // For iwgrp -1 means it wasn't specified.
        int ierr(6);
        if (qwval >= 0.0)
          ierr = 5;
        if (rw != 0.0)
          ierr = 4;
        if (skin != 0.0)
          ierr = 3;
        if (dd != 0)
          ierr = 1;
        if (iwgrp >= 0)
          ierr = 0;
        ForElement(a_mnwflgs, MNWFLGS_IERR, j, MNWFLGS_SIZE) = ierr;
        if (ierr == 0 && dd == 0)
        {
          printf("WARNING: MNW package DD is set to none and the Iqw grp is\n"
                 "         greater than -1 for cell id %d.  Using elevation\n"
                 "         option for DD.\n", cellid);
        }

        const char* siteName;
        if (util::lrint(p.at(H5_SITE, i)) == SITE_PRINT)
          siteName = names.at(i).c_str();
        else
          siteName = "NO-PRINT";
        memset(a_mnwsite + 32*(j-1), ' ', 32);
        memcpy(a_mnwsite + 32*(j-1), siteName, strlen(siteName));

        j++;
      }
    }

    if (a_itmp >= 0)
      success = nwell2 == a_itmp;
  }
  catch (std::out_of_range&)
  {
    ASSERT(0);
    success = false;
  }

  return success;
} // mnw::GetWellData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool mnw::ReadMnw2H5Data(double* MNW2,
                         const int* MNWMAX,
                         const int* NMNWVL,
                         const int* NAUX,
                         const CStr& a_line)
{
  using util::ForElement;
  CStr file(iGetFileFromLine(a_line)), pathBase("MNW2/"), path;
  int numberH5Wells(0);

  std::stringstream ss;
  ss << a_line;
  CStr gmsCard, h5file, h5path;
  int sp(1);
  ss >> gmsCard >> h5file >> h5path >> sp;
  // get stress period
  //size_t stressPeriodStart = a_line.find_last_of(" ");
  //if (stressPeriodStart != std::string::npos)
  //  sscanf(a_line.c_str() + stressPeriodStart, "%d", &sp);
  //else
  //  return false;

  std::pair<int, int> myPair(0,1);
  VEC_INT_PAIR indices(1, myPair);
  // read the number of bcs
  {
    path = pathBase + NUMBC;
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(&numberH5Wells, 1))
      return false;
  }
  indices.back().second = numberH5Wells;

  // get the bc data
  CAR_DBL2D well2;
  well2.SetSize(MNW2_NVL(), numberH5Wells, 0.0);
  {
    path = pathBase + PROPERTY;
    indices.clear();
    indices.push_back(std::make_pair(0, MNW2_NVL()));
    indices.push_back(std::make_pair(0, numberH5Wells));
    indices.push_back(std::make_pair(sp - 1, 1));
    H5DataSetReader r(file, path, indices);
    if (!r.GetData(&well2.at(0, 0), numberH5Wells*MNW2_NVL()))
      return false;
  }

  for (int i=0; i<*MNWMAX; i++)
  {
    ForElement(MNW2,  1, i+1, *NMNWVL) = well2.at(0, i); // active
    ForElement(MNW2,  5, i+1, *NMNWVL) = well2.at(1, i); // Qdes
    ForElement(MNW2, 24, i+1, *NMNWVL) = well2.at(2, i); // CapMult
    ForElement(MNW2, 12, i+1, *NMNWVL) = well2.at(3, i); // Cprime
    ForElement(MNW2,  7, i+1, *NMNWVL) = well2.at(4, i); // Hlim
    ForElement(MNW2,  8, i+1, *NMNWVL) = well2.at(5, i); // QCUT
    ForElement(MNW2,  9, i+1, *NMNWVL) = well2.at(6, i); // Qfrcmn
    ForElement(MNW2, 10, i+1, *NMNWVL) = well2.at(7, i); // Qfrcmn
    for (int j=0; j<*NAUX; j++)
    {
      ForElement(MNW2, 31+j, i+1, *NMNWVL) = well2.at(8+j, i); // AUX
    }
  }
  return true;
} // mnw::ReadH5Data

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MNWReader.t.h>

#include <private/MfLibAsserts.h>

#include <sstream>

//------------------------------------------------------------------------------
void MNWReaderT::testGetWellData ()
{
  using util::ForElement;
  H5Initialize::Init();

  CStr file;
  file = util::GetTestFilesDirectory() + "\\Gms2Mf2k\\mnw\\mnw1\\mnw1.h5";

  CStr line, file1;
  line.Format("GMS_HDF5_MNW \"%s\" \"Multi-Node Well\" 3", file);

  int itmp(16);
  std::vector<double> well2(18*17, 0);
  const int flgsize = mnw::MNWFLGS_SIZE;
  std::vector<double> mnwflgs(flgsize*17, 0);
  std::vector<char> mnwsite(32*17+1, ' ');
  mnwsite[32*17] = 0;
  TS_ASSERT(!mnw::ReadMfH5Data(itmp, &well2[0], &mnwsite[0],
                               &mnwflgs[0], line));
  itmp = 17;
  TS_ASSERT(mnw::ReadMfH5Data(itmp, &well2[0], &mnwsite[0],
                              &mnwflgs[0], line));
  TS_ASSERT_DELTA(31.0,  ForElement(well2, 1, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,   ForElement(well2, 2, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(395.0, ForElement(well2, 4, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.5,   ForElement(well2, 5, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(1.0,   ForElement(well2, 6, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(2.0,   ForElement(well2, 7, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(1.0,   ForElement(well2, 9, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(45.0,   ForElement(well2, 13, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(65.0,   ForElement(well2, 14, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,   ForElement(well2, 16, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(1.0,   ForElement(well2, 18, 1, 18), CXXDELTA);
  TS_ASSERT_DELTA(2.0, ForElement(mnwflgs, 4, 1, flgsize), CXXDELTA); // DD
  TS_ASSERT_DELTA(2.0, ForElement(mnwflgs, 5, 1, flgsize), CXXDELTA); // QCUT
  TS_ASSERT_DELTA(0.0, ForElement(mnwflgs, 6, 1, flgsize), CXXDELTA); // IERR

  TS_ASSERT_DELTA(242.0,     ForElement(well2, 1, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(-100300.0, ForElement(well2, 2, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(-1.0,      ForElement(well2, 4, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,       ForElement(well2, 5, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,       ForElement(well2, 6, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(-1.0E26,   ForElement(well2, 7, 17, 18), 1.0E20);
  TS_ASSERT_DELTA(100.0,     ForElement(well2, 8, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(-1.0,      ForElement(well2, 9, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(45.0,      ForElement(well2, 13, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(65.0,      ForElement(well2, 14, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,       ForElement(well2, 16, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0,       ForElement(well2, 18, 17, 18), CXXDELTA);
  TS_ASSERT_DELTA(0.0, ForElement(mnwflgs, 4, 17, flgsize), CXXDELTA); // DD
  TS_ASSERT_DELTA(2.0, ForElement(mnwflgs, 5, 17, flgsize), CXXDELTA); // QCUT
  TS_ASSERT_DELTA(6.0, ForElement(mnwflgs, 6, 17, flgsize), CXXDELTA); // IERR

  CStr expectedNames = "Well-A                          "
                       "Well-A                          "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "Well-B                          "
                       "Well-B                          "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "NO-PRINT                        "
                       "Simple-C                        "
                       "NO-PRINT                        ";
  CStr actualNames(&mnwsite[0]);
  TS_ASSERT_EQUALS2(expectedNames, actualNames);
}

#endif
