//------------------------------------------------------------------------------
// FILE      ListReaderH5.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private/ListReader/ListReaderH5.t.h>

#include <private/ListReader.h>
#include <private/ListReader/ListReaderH5.h>
#include <private/ListReader/ListReaderParser.h>

//------------------------------------------------------------------------------
void ListReaderH5T::setUp ()
{
  H5Initialize::Init();
  using util::GetTestFilesDirectory;
  m_file = GetTestFilesDirectory() + "\\HDF5_InputFiles\\smallGrid_Trans.h5";
  m_file1 = GetTestFilesDirectory() + "\\HDF5_InputFiles\\modfmap.h5";
  m_strParse.Format("GMS_HDF5_01 \"%s\" \"Drain\" 1", m_file.c_str());
  m_strParse1.Format("GMS_HDF5_01 \"%s\" \"General Head\" 1", m_file.c_str());
  m_strParse2.Format("GMS_HDF5_01 \"%s\" \"Drain\" 1", m_file1.c_str());
  m_strParse3.Format("GMS_HDF5_01 \"%s\" \"Specified Head\" 1", m_file1.c_str());
}
//------------------------------------------------------------------------------
void ListReaderH5T::testCreateClass ()
{
  CStr str("stuff");
  ListReaderParser parser(str);
  ListReaderSetUp setup;
  ListReaderH5 *p = new ListReaderH5(parser, setup);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testFillInKIJ ()
{
  ListReaderParser parser(m_strParse);
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(24, 0.0);
  r.GetCellKIJ(&f[0]);
  TS_ASSERT_EQUALS(f[0], 1);
  TS_ASSERT_EQUALS(f[1], 3);
  TS_ASSERT_EQUALS(f[2], 2);
  TS_ASSERT_EQUALS(f[8], 1);
  TS_ASSERT_EQUALS(f[9], 4);
  TS_ASSERT_EQUALS(f[10], 3);
  TS_ASSERT_EQUALS(f[16], 1);
  TS_ASSERT_EQUALS(f[17], 5);
  TS_ASSERT_EQUALS(f[18], 4);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetIface ()
{
  ListReaderParser parser(m_strParse);
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(24, 0.0);
  r.GetIface(&f[0]);

  TS_ASSERT_EQUALS(f[5], 6);
  TS_ASSERT_EQUALS(f[13], 4);
  TS_ASSERT_EQUALS(f[21], 5);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetCellGroup ()
{
  ListReaderParser parser(m_strParse);
  std::vector<CStr> vS;
  vS.push_back("IFACE");
  vS.push_back("CONDFACT");
  vS.push_back("CELLGRP");
  CStr line;
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6, line, vS);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(24, 0.0);
  r.GetCellGroup(&f[0]);

  TS_ASSERT_EQUALS(f[7], -1);
  TS_ASSERT_EQUALS(f[15], -1);
  TS_ASSERT_EQUALS(f[23], -1);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetFactor ()
{
  ListReaderParser parser(m_strParse);
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(24, 0.0);
  r.GetFactor(&f[0], 1, 0);

  TS_ASSERT_EQUALS(f[6], 1);
  TS_ASSERT_EQUALS(f[14], 1);
  TS_ASSERT_EQUALS(f[22], 1);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetCellGroup1 ()
{
  ListReaderParser parser(m_strParse1);
  std::vector<CStr> vS;
  vS.push_back("IFACE");
  vS.push_back("CONDFACT");
  vS.push_back("CELLGRP");
  CStr line;

  ListReaderSetUp setup(12, 8, 0, 3, 5, 6, line, vS);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(96, 0.0);
  r.GetCellGroup(&f[0]);

  TS_ASSERT_EQUALS(f[7], 1);
  TS_ASSERT_EQUALS(f[15], 1);
  TS_ASSERT_EQUALS(f[23], 1);
  TS_ASSERT_EQUALS(f[31], 1);
  TS_ASSERT_EQUALS(f[39], 1);
  TS_ASSERT_EQUALS(f[47], 1);
  TS_ASSERT_EQUALS(f[55], 1);
  TS_ASSERT_EQUALS(f[63], 1);
  TS_ASSERT_EQUALS(f[71], 1);
  TS_ASSERT_EQUALS(f[79], 1);
  TS_ASSERT_EQUALS(f[87], 1);
  TS_ASSERT_EQUALS(f[95], 1);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetFactor1 ()
{
  ListReaderParser parser(m_strParse1);
  ListReaderSetUp setup(12, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(96, 0.0);
  r.GetFactor(&f[0], 1, 0);

  Real f1(f[6]);
  CStr str;
  str.Format("%f", f1);
  TS_ASSERT(str == "10.000010");
  TS_ASSERT_DELTA(f[6], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[14], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[22], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[30], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[38], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[46], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[54], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[62], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[70], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[78], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[86], f1, CXXDELTA);
  TS_ASSERT_DELTA(f[94], f1, CXXDELTA);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetFactor2 ()
{
  {
    ListReaderParser parser(m_strParse2);
    ListReaderSetUp setup(92, 8, 0, 3, 72, 42);
    ListReaderH5 r(parser, setup);

    TS_ASSERT_EQUALS(r.GetVersion(), true);
    std::vector<Real> vf;
    TS_ASSERT(r.GetFactor(vf, 0));
    TS_ASSERT_DELTA(vf[0], (Real)29.304847864807307, CXXDELTA);
    TS_ASSERT_DELTA(vf[13], (Real)54.902980742801340, CXXDELTA);
    TS_ASSERT_DELTA(vf[29], (Real)29.942714301335318, CXXDELTA);
    TS_ASSERT_DELTA(vf[41], (Real)24.096709305823968, CXXDELTA);
    TS_ASSERT_DELTA(vf[63], (Real)27.557791721893999, CXXDELTA);
    TS_ASSERT_DELTA(vf[91], (Real)41.768747489126810, CXXDELTA);
  }
  {
    ListReaderParser parser(m_strParse3);
    ListReaderSetUp setup(344, 8, 0, 3, 72, 42);
    ListReaderH5 r(parser, setup);

    TS_ASSERT_EQUALS(r.GetVersion(), true);
    std::vector<Real> vf;
    TS_ASSERT(r.GetFactor(vf, 0));
    TS_ASSERT(vf.size() == 688);
    TS_ASSERT_DELTA(vf[344+0], (Real)14.691913184449145, CXXDELTA);
    TS_ASSERT_DELTA(vf[344+51], (Real)75.652831593881288, CXXDELTA);
    TS_ASSERT_DELTA(vf[344+101], (Real)11.779859755891993, CXXDELTA);
    TS_ASSERT_DELTA(vf[344+201], (Real)57.781433107171146, CXXDELTA);
    TS_ASSERT_DELTA(vf[344+301], (Real)53.713705311949965, CXXDELTA);
    TS_ASSERT_DELTA(vf[344+343], (Real)92.913769617033097, CXXDELTA);
  }
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetStressData ()
{
  ListReaderParser parser(m_strParse);
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);

  std::vector<Real> f(24, 0.0);
  r.GetStressData(&f[0]);

  TS_ASSERT_EQUALS(f[3], 10);
  TS_ASSERT_EQUALS(f[11], 10);
  TS_ASSERT_EQUALS(f[19], 10);
  TS_ASSERT_EQUALS(f[4], 19);
  TS_ASSERT_EQUALS(f[12], 19);
  TS_ASSERT_EQUALS(f[20], 19);
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetVersion ()
{
  {
    ListReaderParser parser(m_strParse);
    ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
    ListReaderH5 r(parser, setup);
    TS_ASSERT_EQUALS(r.GetVersion(), false);
    TS_ASSERT_EQUALS(r.m_fileVersion, 1.0);
  }
  {
    ListReaderParser parser(m_strParse2);
    ListReaderSetUp setup(92, 8, 0, 3, 72, 42);
    ListReaderH5 r(parser, setup);
    TS_ASSERT_EQUALS(r.GetVersion(), true);
    TS_ASSERT_EQUALS(r.m_fileVersion, 2.0);
  }
}
//------------------------------------------------------------------------------
void ListReaderH5T::testGetSeawatAuxH5Idx ()
{
  ListReaderParser parser(m_strParse);
  ListReaderSetUp setup(3, 8, 0, 3, 5, 6);
  ListReaderH5 r(parser, setup);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("RBDTHK"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("RbDtHk"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("RIVDEN"), 5);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("RiVdEn"), 5);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("DRNBELEV"), 3);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("DrNbElEv"), 3);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("GHBELEV"), 3);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("GhBeLeV"), 3);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("GHBDENS"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("GhBdEnS"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("CHDDENSOPT"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("ChDdEnSoPt"), 4);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("CHDDEN"), 5);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("ChDdEn"), 5);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("WELDENS"), 2);
  TS_ASSERT_EQUALS(r.GetSeawatAuxH5Idx("WeLdEnS"), 2);
}
#endif
