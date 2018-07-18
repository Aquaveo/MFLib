//------------------------------------------------------------------------------
// FILE      H5UseLastWriter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private/MfData/MfExport/private/H5/H5UseLastWriter.h>

// 3. Standard library headers

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/H5DataReader/H5DataSetWriter.h>
#include <private/H5DataReader/H5DataSetWriterSetup.h>
#include <private/MfData/MfExport/private/H5/H5ArrayWriter.h>
#include <private/MfData/MfExport/private/H5/H5Strings.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

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
static void iGetAllArealUseLast (CAR_INT2D& a_flags,
                                 const char *a_path,
                                 const char *a_baseName)
{
  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_USELAST;
  std::vector<hsize_t> dims;

  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    a_flags.SetSize(static_cast<int>(dims[0]), static_cast<int>(dims[1]), 0);
  }
  {
    std::pair<int, int> p(0,0);
    VEC_INT_PAIR indices(2, p);
    indices[0].second = a_flags.GetSize1();
    indices[1].second = a_flags.GetSize2();
    H5DataSetReader r(f, path, indices);
    r.GetData(&a_flags.at(0,0), a_flags.GetSize1()*a_flags.GetSize2());
  }
} // iGetAllArealUseLast
//------------------------------------------------------------------------------
static void iArealPropFromUseLast (CAR_INT2D& a_flags,
                                   int a_nCells,
                                   const char *a_path,
                                   const char *a_baseName)
{
  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_DATA;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 3)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[3] = {0,0,0};
  hsize_t dim[3] = {a_flags.GetSize1() - 1, a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1 ||
      dims[2] != 1)
  {
    if (CStr(a_path) == "ETS/")
      dim[0] = 3;
    if (CStr(a_path) == "UZF/")
      dim[0] = 4;
    dim[2] = a_flags.GetSize2() - dims[2];
    start[2] = dims[2];
  }

  if (dim[0] < 1 ||
      dim[1] < 1 ||
      dim[2] < 1)
    return;

  // write the data set
  {
    H5ArrayWriter::WriteDataSetWithZeros(f, path, dim, start);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(static_cast<int>(dim[0]),
                 static_cast<int>(dim[2]), 1);
    path = a_path;
    path += MFBC_DATAMULT;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
                         n2write(2, 0);
    start1[1] = start[2];
    n2write[0] = dim[0];
    n2write[1] = dim[2];
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), static_cast<size_t>(n2write[0]*n2write[1]));
  }
} // iArealPropFromUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iArealLayFromUseLast (CAR_INT2D& a_flags,
                                  int a_nCells,
                                  const char *a_path,
                                  const char *a_baseName)
{
  if (a_flags.GetSize1() <= 1)
    return;

  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_LAY;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 2)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[2] = {0,0};
  hsize_t dim[2] = {a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1)
  {
    dim[1] = a_flags.GetSize2() - dims[1];
    start[1] = dims[1];
  }

  CAR_INT2D dat;
  dat.SetSize(static_cast<int>(dim[0]),
              static_cast<int>(dim[1]), 1);
  // write the data set
  if (dim[0] > 0 && dim[1] > 0)
  {
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 2);
    std::vector<hsize_t> start1(&start[0], &start[2]),
                         n2write(&dim[0], &dim[2]);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    size_t num(static_cast<size_t>(dim[0] * dim[1]));
    w.WriteData(&dat.at(0,0), num);
  }
  if (dim[0] > 0 && dim[1] > 0)
  {
    std::vector<int> mult(static_cast<size_t>(dim[1]), 1);
    path = a_path;
    path += MFBC_LAYMULT;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start1(1, start[1]),
                         n2write(1, dim[1]);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0), static_cast<size_t>(dim[1]));
  }
} // iArealLayFromUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iEtSegFromUseLast (CAR_INT2D& a_flags,
                               int a_nCells,
                               const char *a_baseName,
                               const char *a_dataPath,
                               const char *a_multPath)
{
  if (a_flags.GetSize1() <= 1)
    return;

  CStr f(a_baseName), path("ETS/");
  f += ".h5";
  path += a_dataPath;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 3)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[3] = {0,0,0};
  hsize_t dim[3] = {dims[0], a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1 ||
      dims[2] != 1)
  {
    dim[2] = a_flags.GetSize2() - dims[2];
    start[2] = dims[2];
  }

  if (dim[0] < 1 ||
      dim[1] < 1 ||
      dim[2] < 1)
    return;

  // write the data set
  {
    H5ArrayWriter::WriteDataSetWithZeros(f, path, dim, start);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(static_cast<int>(dim[0]),
                 static_cast<int>(dim[2]), 1);
    path = "ETS/";
    path += a_multPath;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
                         n2write(2, 0);
    start1[1] = start[2];
    n2write[0] = dim[0];
    n2write[1] = dim[2];
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), static_cast<size_t>(n2write[0]*n2write[1]));
  }
} // expEtSegFromUseLast

} // unnamed namespace


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5UseLastWriter::H5UseLastWriter (NativePackExp* a_) :
   m_pack(a_)
{
} // H5UseLastWriter::H5UseLastWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
H5UseLastWriter::~H5UseLastWriter ()
{
} // H5UseLastWriter::~H5UseLastWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5UseLastWriter::WriteData (std::vector<int>& a_data)
{
  CStr packName(m_pack->GetPackage()->PackageName());
  int  spIdx(m_pack->GetGlobal()->GetCurrentPeriod()-1);
  CStr fname(m_pack->GetNative()->GetExp()->GetBaseFileName());
  fname += ".h5";
  CStr path(GetH5DataPath());
  H5DataSetWriterSetup setup(fname, path, H5T_NATIVE_INT, 2);
  if (packName.Find(Packages::VDFStressPeriod) != -1 ||
      packName.Find(Packages::VSCStressPeriod) != -1) setup.m_nDim = 1;
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);

  std::vector<hsize_t> start(2,0), n2write(2,1);
  start[1] = spIdx;
  n2write[0] = a_data.size();
  if (packName.Find(Packages::VDFStressPeriod) != -1 ||
      packName.Find(Packages::VSCStressPeriod) != -1)
  {
    start[0] = spIdx;
    start.pop_back();
    n2write.pop_back();
  }
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  h.WriteData(&a_data[0], a_data.size());
} // H5ArrayWriter::WriteData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr H5UseLastWriter::GetH5DataPath ()
{
  CStr rval;
  CStr packName(m_pack->GetPackage()->PackageName());
  if (packName.Find(Packages::RCH) != -1)      rval = "Recharge/";
  else if (packName.Find(Packages::EVT) != -1) rval = "ET/";
  else if (packName.Find(Packages::ETS) != -1) rval = "ETS/";
  else if (packName.Find(Packages::UZF) != -1) rval = "UZF/";
  else if (packName.Find(Packages::VDFStressPeriod) != -1) rval = "VDF/";
  else if (packName.Find(Packages::VSCStressPeriod) != -1) rval = "VSC/";
  rval += MFBC_USELAST;
  return rval;
} // H5UseLastWriter::GetH5DataPath
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5UseLastWriter::WriteEtsNetSeg (int a_)
{
  CStr fname(m_pack->GetNative()->GetExp()->GetBaseFileName());
  fname += ".h5";
  CStr path = "ETS/";
  path += MFBC_NETSEG;
  H5DataSetWriterSetup setup(fname, path, H5T_NATIVE_INT, 1);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  h.WriteData(&a_, 1);
} // H5UseLastWriter::WriteEtsNetSeg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void H5UseLastWriter::CheckArealFromUseLast ()
{
  int nCells = m_pack->GetGlobal()->NumRow() * m_pack->GetGlobal()->NumCol();
  if (m_pack->GetGlobal()->Unstructured())
  {
    nCells = m_pack->GetGlobal()->NumNodesUnstructured();
  }
  CStr baseName(m_pack->GetNative()->GetExp()->GetBaseFileName());
  // read all of the use last flags on the data
  CAR_INT2D flags;
  iGetAllArealUseLast(flags, "Recharge/", baseName);
  // update the areal data
  iArealPropFromUseLast(flags, nCells, "Recharge/", baseName);
  // update the areal layer
  iArealLayFromUseLast(flags, nCells, "Recharge/", baseName);

  iGetAllArealUseLast(flags, "ET/", baseName);
  // update the areal data
  iArealPropFromUseLast(flags, nCells, "ET/", baseName);
  // update the areal layer
  iArealLayFromUseLast(flags, nCells, "ET/", baseName);

  iGetAllArealUseLast(flags, "ETS/", baseName);
  // update the areal data
  iArealPropFromUseLast(flags, nCells, "ETS/", baseName);
  // update the areal layer
  iArealLayFromUseLast(flags, nCells, "ETS/", baseName);
  // update the extinction depth proportion
  iEtSegFromUseLast(flags, nCells, baseName, MFBC_PXDP, MFBC_PXDPMULT);
  // update the extinction rate proportion
  iEtSegFromUseLast(flags, nCells, baseName, MFBC_PETM, MFBC_PETMMULT);

  iGetAllArealUseLast(flags, "UZF/", baseName);
  // update the areal data
  iArealPropFromUseLast(flags, nCells, "UZF/", baseName);

} // H5UseLastWriter::CheckArealFromUseLast


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#if CXX_TEST
#include <private/MfData/MfExport/private/H5/H5UseLastWriter.t.h>
#include <private/MfData/MfExport/private/H5/H5Util.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
/// \brief Exports the use last information for the areal packages
//------------------------------------------------------------------------------
static void expUseLastAreal (const char * a_file,
                             const char * a_path,
                             int a_sp,
                             std::vector<int> &a_data)
{
  CStr f(a_file), path(a_path);
  f += ".h5";
  path += MFBC_USELAST;
  H5DataSetWriterSetup setup(f, path, H5T_NATIVE_INT, 2);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  // do multidim stuff if needed
  std::vector<hsize_t> start(2,0), n2write(2,1);
  start[1] = a_sp - 1;
  n2write[0] = a_data.size();
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  h.WriteData(&a_data[0], a_data.size());
} // expUseLastAreal


//------------------------------------------------------------------------------
void H5UseLastWriterT::testCreateClass ()
{
  H5UseLastWriter* p = new H5UseLastWriter(NULL);
  TS_ASSERT(p);
  if (p) delete(p);
} // H5UseLastWriterT::testCreateClass
//------------------------------------------------------------------------------
void H5UseLastWriterT::test_iGetAllArealUseLast ()
{
  CStr f, f1, path, path1;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_USELAST;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  iGetAllArealUseLast(flags, "Recharge/", f);
  TS_ASSERT_EQUALS(flags.GetSize1(), 2);
  TS_ASSERT_EQUALS(flags.GetSize2(), 2);
  TS_ASSERT_EQUALS(flags.at(0,0), 0);
  TS_ASSERT_EQUALS(flags.at(0,1), 1);
  TS_ASSERT_EQUALS(flags.at(1,0), 0);
  TS_ASSERT_EQUALS(flags.at(1,1), 1);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  TS_ASSERT(!remove(f1));
} // H5UseLastWriterT::testexpGetAllArealUseLast
//------------------------------------------------------------------------------
void H5UseLastWriterT::test_iArealPropFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_DATA;
  path2 = path;
  path2 += MFBC_DATAMULT;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  iGetAllArealUseLast(flags, "Recharge/", f);
  iArealPropFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));

  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_DATA;
  dat[0] = dat[1] = 0;
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);
  iGetAllArealUseLast(flags, "Recharge/", f);
  // write some data and then call the function
  {
    std::vector<double> vd(10, 2);
    H5DataSetWriterSetup s(f1, path1, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start1(3, 0), n2write(3, 1);
    n2write[1] = 10;
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&vd[0], 10);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(1,1,2);
    H5DataSetWriterSetup s(f1, path2, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
      n2write(2, 1);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), 1);
  }

  iArealPropFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,9,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
} // H5UseLastWriterT::test_iArealPropFromUseLast
//------------------------------------------------------------------------------
void H5UseLastWriterT::test_iEtSegFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "ETS/";
  path1 = path;
  path1 += MFBC_PXDP;
  path2 = path;
  path2 += MFBC_PXDPMULT;

  // shouldn't add items from Use Last when ETS package not enabled
  CAR_INT2D flags;
  iGetAllArealUseLast(flags, "ETS/", f);
  // update the areal data
  iArealPropFromUseLast(flags, 10, "ETS/", f);
  // update the areal layer
  iArealLayFromUseLast(flags, 10, "ETS/", f);
  // update the extinction depth proportion
  iEtSegFromUseLast(flags, 10, f, MFBC_PXDP, MFBC_PXDPMULT);
  {
    std::vector<hsize_t> size;

    // property
    H5DataSetReader propertyReader(f1, path + MFBC_DATA);
    propertyReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 3);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));
    TS_ASSERT_EQUALS2(1, size.at(2));

    // layer
    H5DataSetReader layerReader(f1, path + MFBC_LAY);
    layerReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 2);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));

    // extinction depth
    H5DataSetReader extDepthReader(f1, path + MFBC_PXDP);
    extDepthReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 3);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));
    TS_ASSERT_EQUALS2(1, size.at(2));
  }

  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  iGetAllArealUseLast(flags, "ETS/", f);
  iEtSegFromUseLast(flags, 10, f, MFBC_PXDP, MFBC_PXDPMULT);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
} // H5UseLastWriterT::testexpEtSegFromUseLast
//------------------------------------------------------------------------------
void H5UseLastWriterT::test_iArealLayFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_LAY;
  path2 = path;
  path2 += MFBC_LAYMULT;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  iGetAllArealUseLast(flags, "Recharge/", f);
  iArealLayFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_INT2D dat;
    dat.SetSize(10, 2, 2);
    std::pair<int, int> p(0,10);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(9,1), 1);
  }
  {
    std::vector<int> dat(2, 0);
    std::pair<int, int> p(0,2);
    VEC_INT_PAIR indices(1,p);
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(0), 1);
    TS_ASSERT_EQUALS(dat.at(1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));


  H5Util_CreateDefaultMfH5File(f,1,0);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_LAY;
  dat[0] = dat[1] = 0;
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);
  iGetAllArealUseLast(flags, "Recharge/", f);
  // write some data and then call the function
  {
    std::vector<int> vd(10, 2);
    H5DataSetWriterSetup s(f1, path1, H5T_NATIVE_INT, 2);
    std::vector<hsize_t> start1(2, 0), n2write(2, 1);
    n2write[0] = 10;
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&vd[0], 10);
  }
  {
    int mult(2);
    H5DataSetWriterSetup s(f1, path2, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start1(1, 0),
      n2write(1, 1);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult, 1);
  }

  iArealLayFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_INT2D dat;
    dat.SetSize(10, 2, 2);
    std::pair<int, int> p(0,10);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(9,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
    TS_ASSERT_EQUALS(dat.at(9,1), 1);
  }
  {
    std::vector<int> dat(2, 0);
    std::pair<int, int> p(0,2);
    VEC_INT_PAIR indices(1,p);
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
} // H5UseLastWriterT::test_iArealLayFromUseLast

#endif

