//------------------------------------------------------------------------------
// FILE      H5BcList.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Native\H5BcList.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/H5Strings.h>
#include <private/MfData/MfExport/private/Native/H5LstPack.h>
#include <private/MfData/MfExport/private/Native/Mnw1PropList.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MNWReader.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------
//------------------------------------------------------------------------------
namespace
{

//------------------------------------------------------------------------------
static Mnw1PropList& iGet_Mnw1PropList (int a_)
{
  static std::map<int, Mnw1PropList> m_;
  return m_[a_];
} // iGet_Mnw1PropList
//------------------------------------------------------------------------------
static std::vector<double>& iGet_Mnw1Flags (int a_)
{
  static std::map<int, std::vector<double> > m_;
  return m_[a_];
} // iGet_Mnw1Flags

} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5BcList::H5BcList (NativePackExp* a_) :
  m_pack(a_)
{
} // H5BcList::H5BcList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5BcList::~H5BcList ()
{
} // H5BcList::~H5BcList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::WriteList (
    const int a_sp
  , const int a_start
  , CStr& a_type
  , CStr& a_f
  , std::vector<int>& a_cellids
  , CAR_DBL2D& a_bcData
  , std::vector<int>& a_vIface)
{
  CStr path;
  // writing the bc "Property"
  {
    path.Format("%s/%s", a_type, MFBC_DATA);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start(3, 0), n2write(3,1);
    n2write[0] = a_bcData.GetSize1();
    n2write[1] = a_bcData.GetSize2();
    start[1] = a_start;
    start[2] = a_sp - 1;
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&a_bcData.at(0,0),
      static_cast<size_t>(a_bcData.GetSize1()*a_bcData.GetSize2()));
  }
  // writing the "numbcs"
  {
    path.Format("%s/%s", a_type, MFBC_NUMBC);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, 1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    int nCellids(static_cast<int>(a_cellids.size()));
    w.WriteData(&nCellids, 1);
  }
  // writing the bc "cellids"
  if (!a_cellids.empty())
  {
    path.Format("%s/%s", a_type, MFBC_CELLIDS);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, a_cellids.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&a_cellids.at(0), a_cellids.size());
  }
  // writing the bc "iface"
  if (!a_vIface.empty())
  {
    path.Format("%s/%s", a_type, MFBC_IFACE);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, a_vIface.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&a_vIface.at(0), a_vIface.size());
  }
  // write name and map id
  std::vector<char> vC(a_cellids.size(), 0);
  if (!a_cellids.empty())
  {
    path.Format("%s/%s", a_type, MFBC_NAME);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_CHAR, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vC.at(0), vC.size());
  }
  if (!a_cellids.empty())
  {
    path.Format("%s/%s", a_type, MFBC_MAPIDSTR);
    H5DataSetWriterSetup s(a_f, path, H5T_NATIVE_CHAR, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vC.at(0), vC.size());
  }
} // H5BcList::WriteList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::WriteSingleH5IntValue (
  const char *filePath
, const char *h5Path
, int value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // H5BcList::WriteSingleH5IntValue
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::WriteSingleH5DoubleValue (
  const char *filePath
, const char *h5Path
, double value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_DOUBLE, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // H5BcList::WriteSingleH5IntValue
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::Write1DIntArray (
  const char *a_filePath
, const char *a_h5Path
, const int *a_array
, size_t a_length
, size_t a_start)
{
  H5DataSetWriterSetup s(a_filePath, a_h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, a_start), n2write(1, a_length);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(a_array, a_length);
} // H5BcList::Write1DIntArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::Write1DH5Value (
  const char *filePath
, const char *h5Path
, hsize_t position
, int value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, position), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // H5BcList::Write1DH5Value
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::WriteH5StringArray (
  const char *a_filePath
, const char *a_h5Path
, const std::vector<CStr>& a_array)
{
  int maxLen(0);

  for (std::vector<CStr>::const_iterator s = a_array.begin();
       s != a_array.end(); ++s)
  {
    int len = (int)s->length()+1;
    if (len > maxLen)
      maxLen = len;
  }

  std::vector<char> strings(a_array.size()*maxLen, ' ');
  for (size_t i = 0; i < a_array.size(); ++i)
  {
    strcpy(&strings.at(i*maxLen), a_array.at(i).c_str());
  }

  H5DataSetWriterSetup ws(a_filePath, a_h5Path, H5T_NATIVE_CHAR, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, strings.size());
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&ws);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&strings[0], strings.size());
  w.WriteAtt(MFBC_MAX_STR_LEN, maxLen);
} // H5BcList::WriteH5StringArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::WriteMapIdsForListBcs ()
{
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  h.WriteMapIds();
} // H5BcList::WriteMapIdsForListBcs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::LstPack (int& a_maxBc)
{
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  return h.Write(a_maxBc);
} // H5BcList::LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5BcList::LstPar ()
{
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  h.LstPar();
} // H5BcList::LstPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::Str (int& a_itmp)
{
  CStr rval;
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  rval = h.StrPack(a_itmp);
  return rval;
} // H5BcList::Str
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::SfrLn2 ()
{
  CStr rval;
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  rval = h.SfrLn2();
  return rval;
} // H5BcList::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::SfrLn6 (int& a_itmp)
{
  CStr rval;
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  rval = h.SfrLn6(a_itmp);
  return rval;
} // H5BcList::SfrLn2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::Mnw2 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  CStr rval;
  int a_sp = m_pack->GetGlobal()->GetCurrentPeriod();
  const int* ITMP(0),* NMNWVL(0),* MNWMAX(0),* NAUX(0);
  const double* MNW2d(0);
  MfPackage* a_p=m_pack->GetPackage();
  if (!a_p->GetField(MNW2pack::ITMP, &ITMP) || !ITMP ||
      !a_p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
      !a_p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
      !a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
  {
    ASSERT(0);
    return rval;
  }

  CStr f(m_pack->GetNative()->GetExp()->GetBaseFileName()), path;
  f += ".h5";
  {// write the use last flag
    path.Format("%s/%s", "MNW2", MFBC_USELAST);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    int tmpItmp(*ITMP < 0 ? 1 : 0);
    w.WriteData(&tmpItmp, 1);
  }
  CAR_DBL2D bcData;
  bcData.SetSize(13, *MNWMAX, 0);
  if (*ITMP > 0)
  { // get the data from the MNW2 array
    for (int i=0; i<*MNWMAX; i++)
    {
      bcData.at(0,i) = ForElement(MNW2d,  1, i+1, *NMNWVL); // active
      bcData.at(1,i) = ForElement(MNW2d,  5, i+1, *NMNWVL); // Qdes
      bcData.at(2,i) = ForElement(MNW2d, 24, i+1, *NMNWVL); // CapMult
      bcData.at(3,i) = ForElement(MNW2d, 12, i+1, *NMNWVL); // Cprime
      bcData.at(4,i) = ForElement(MNW2d,  7, i+1, *NMNWVL); // Hlim
      bcData.at(5,i) = ForElement(MNW2d,  8, i+1, *NMNWVL); // QCUT
      bcData.at(6,i) = ForElement(MNW2d,  9, i+1, *NMNWVL); // Qfrcmn
      bcData.at(7,i) = ForElement(MNW2d, 10, i+1, *NMNWVL); // Qfrcmx
      for (int j=0; j<*NAUX; j++) // AUX
      {
        bcData.at(8+j,i) = ForElement(MNW2d, 31+j, i+1, *NMNWVL);
      }
    }
  }
  else if (*ITMP < 0 && a_sp > 1)
  { // get the previous stress period
    path.Format("%s/%s", "MNW2", MFBC_DATA);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[0].second = bcData.GetSize1();
    indices[1].second = bcData.GetSize2();
    indices[2].first = a_sp-2;
    H5DataSetReader r(f, path, indices);
    r.GetData(&bcData.at(0,0),
      static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
  }

  std::vector<int> cellids(*MNWMAX, 0), iface(*MNWMAX, 0);
  for (size_t q=0; q<cellids.size(); q++)
  {
    cellids[q] = static_cast<int>(q+1);
  }
  // write the data for the stress period
  int sp = m_pack->GetGlobal()->GetCurrentPeriod();
  CStr file1, line, mStr = "MNW2";
  WriteList(sp, 0, mStr, f, cellids, bcData, iface);
  util::StripPathFromFilename(f, file1);
  line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, "MNW2", sp);
  if (*ITMP > 0)
  {
    rval = line;
  }
  return rval;
} // H5BcList::Mnw2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::Mnw1 ()
{
  CStr rval;

  using namespace MfData::Packages;
  using util::ForElement;

  int modIdx = (int)m_pack->GetGlobal()->CurModIdx();
  const int *itmp(0), *nwell2(0);
  const double *well2(0), *mnwflgs(0);
  const char *mnwsite(0);
  const int WELL2_SIZE = 18;
  MfPackage* a_p = m_pack->GetPackage();
  int sp = m_pack->GetGlobal()->GetCurrentPeriod();
  if (a_p->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_p->GetField(MNWpack::NWELL2, &nwell2) && nwell2 &&
      a_p->GetField(MNWpack::WELL2, &well2) && well2 &&
      a_p->GetField(MNWpack::MNWSITE, &mnwsite) && mnwsite &&
      a_p->GetField(MNWpack::MNWFLGS, &mnwflgs) && mnwflgs)
  {
    CStr line, f(m_pack->GetNative()->GetExp()->GetBaseFileName()), fileName;
    const char *type = "Multi-Node Well";
    CStr path;
    f += ".h5";
    util::StripPathFromFilename(f, fileName);

    // line 4. ITMP (ADD)
    // not handling ADD so use nwell2 if itmp > 0
    int useLast(*itmp);
    if (useLast > 0)
      useLast = *nwell2;

    // line 5. as HDF5
    if (*itmp > 0)
    {
      rval.Format("GMS_HDF5_MNW \"%s\" \"Multi-Node Well\" %d", fileName, sp);
    }

    // update use last
    path.Format("%s/%s", type, MFBC_USELAST);
    Write1DH5Value(f, path, sp-1, useLast < 0 ? 1 : 0);

    Mnw1PropList& wpl = iGet_Mnw1PropList(modIdx);
    wpl.NewStressPeriod();
    std::vector<int> properties;

    if (*nwell2 != 0)
    { // get a list of indicees into the properties array for each line 5
      // row from well2 and append to properties array
      std::vector<int> wellCellIds;
      std::vector<int> wellProperties;
      int currWellNum = static_cast<int>(ForElement(well2, 18, 1, 18));
      int firstWell2Index = 1;
      for (int i = 1; i <= *nwell2; ++i)
      {
        int cell = static_cast<int>(ForElement(well2, 1, i, 18));
        int wellNum = static_cast<int>(ForElement(well2, 18, i, 18));
        if (wellNum != currWellNum)
        { // write properties for well
          CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
          siteName.Trim();
          if (siteName == "NO-PRINT")
            siteName = "";
          wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
          properties.insert(properties.end(), wellProperties.begin(),
                            wellProperties.end());

          wellCellIds.clear();
          wellProperties.clear();
          currWellNum = wellNum;
          firstWell2Index = i;
        }
        wellCellIds.push_back(cell);
      }

      // get properties for last well
      CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
      siteName.Trim();
      if (siteName == "NO-PRINT")
        siteName = "";
      wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
      properties.insert(properties.end(), wellProperties.begin(),
                        wellProperties.end());
    }

    if (wpl.GetCellIds().size() != 0)
    {
      // save mnwflgs in case using last next stress period
      std::vector<double>& mnwFlgsCopy = iGet_Mnw1Flags(modIdx);
      if (sp == 1 || *itmp > 0)
      {
        mnwFlgsCopy.clear();
        mnwFlgsCopy.insert(mnwFlgsCopy.begin(), mnwflgs, 
                           mnwflgs+mnw::MNWFLGS_SIZE*(*nwell2));
      }

      // set property values
      CAR_DBL2D v;
      v.SetSize(mnw::H5_SIZE, (int)wpl.GetCellIds().size(), 0.0);
      for (int i = 1; i <= *nwell2; ++i)
      {
        int j = properties.at(i-1);
        double *flgs = &mnwFlgsCopy.at(0);
        v.at(mnw::H5_ACTIVE, j) = mnw::ACTIVE;
        double q = ForElement(flgs, mnw::MNWFLGS_QDES, i, mnw::MNWFLGS_SIZE);
        v.at(mnw::H5_QDES, j)   = q;
        v.at(mnw::H5_WELLID, j) = wpl.GetWellIds().at(j);
        v.at(mnw::H5_QWVAL, j)  = ForElement(well2, 4, i, WELL2_SIZE);
        v.at(mnw::H5_RW, j)     = ForElement(well2, 5, i, WELL2_SIZE);
        v.at(mnw::H5_SKIN, j)   = ForElement(well2, 6, i, WELL2_SIZE);
        if (ForElement(flgs, mnw::MNWFLGS_DD, i, mnw::MNWFLGS_SIZE) ==
            mnw::DD_NONE)
        {
          v.at(mnw::H5_HLIM, j) = 0.0;
          v.at(mnw::H5_HREF, j) = 0.0;
        }
        else
        {
          v.at(mnw::H5_HLIM, j)   = ForElement(flgs, mnw::MNWFLGS_HLIM, i, 
                                               mnw::MNWFLGS_SIZE);
          v.at(mnw::H5_HREF, j)   = ForElement(flgs, mnw::MNWFLGS_HREF, i,
                                               mnw::MNWFLGS_SIZE);
        }
        v.at(mnw::H5_DD, j)     = ForElement(flgs, mnw::MNWFLGS_DD, i,
                                             mnw::MNWFLGS_SIZE);
        if (util::lrint(ForElement(flgs, mnw::MNWFLGS_IERR, i,
                                   mnw::MNWFLGS_SIZE)) >= 1)
          v.at(mnw::H5_IWGRP, j) = -1;
        else
          v.at(mnw::H5_IWGRP, j) = ForElement(well2, 9, i, WELL2_SIZE);
        v.at(mnw::H5_C, j)      = ForElement(well2, 16, i, WELL2_SIZE);
        int qcut = util::lrint(ForElement(flgs, mnw::MNWFLGS_QCUT, i,
                                          mnw::MNWFLGS_SIZE));
        v.at(mnw::H5_QCUT, j)   = qcut;
        //if (v.at(mnw::H5_QDES, j) == 0)
        //{ // if the Q is zero then the MNW packages doesn't read these values
        //  // the values may have been set if the user specifies "DEFAULT" but
        //  // the package code ignores the values
        //  v.at(mnw::H5_QFRCMN, j) = 0;
        //  v.at(mnw::H5_QFRCMX, j) = 0;
        //}
        //else if (qcut == 1)
        if (qcut == 1)
        {
          v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*q;
          v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*q;
        }
        else if (qcut == 2)
        {
          v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*100;
          v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*100;
        }
        v.at(mnw::H5_SITE, j)   = wpl.GetIsSiteName().at(j) ? mnw::SITE_PRINT : 
                                                          mnw::SITE_DONT_PRINT;
      }

      // write properties
      {
        path.Format("%s/%s", type, MFBC_DATA);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
        std::vector<hsize_t> start(3, 0), n2write(3,1);
        n2write[0] = v.GetSize1();
        n2write[1] = v.GetSize2();
        start[2] = sp - 1;
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.WriteData(&v.at(0,0),
                    static_cast<size_t>(v.GetSize1()*v.GetSize2()));
      }

      // update number of boundary conditions
      path.Format("%s/%s", type, MFBC_NUMBC);
      WriteSingleH5IntValue(f, path, (int)wpl.GetCellIds().size());

      // update cellids
      path.Format("%s/%s", type, MFBC_CELLIDS);
      Write1DIntArray(f, path, &wpl.GetCellIds()[0], wpl.GetCellIds().size());

      // update names
      path.Format("%s/%s", type, MFBC_NAME);
      WriteH5StringArray(f, path, wpl.GetWellNames());

      // update iface
      std::vector<int> iface(wpl.GetCellIds().size(), 0);
      path.Format("%s/%s", type, MFBC_IFACE);
      Write1DIntArray(f, path, &iface[0], iface.size());

      // update mapids
      std::vector<CStr> mapids(wpl.GetCellIds().size(), CStr());
      path.Format("%s/%s", type, MFBC_MAPIDSTR);
      WriteH5StringArray(f, path, mapids);
    }
  }
  return rval;
} // H5BcList::Mnw1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5BcList::ClnWel (int& a_itmp)
{
  CStr rval;
  H5LstPack h(m_pack->GetGlobal(), m_pack->GetPackage(),
              m_pack->GetNative(), this);
  rval = h.ClnWel(a_itmp);
  return rval;
} // H5BcList::ClnWel


