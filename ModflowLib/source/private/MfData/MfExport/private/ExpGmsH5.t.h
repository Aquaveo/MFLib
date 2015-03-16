//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPGMSH5_T_H
#define EXPGMSH5_T_H

#include <cxxtest/TestSuite.h>

class ExpGmsH5T : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testSupportedPackage();
  void testexpNameFile();
  void testGetArrayMap();
  void testIsDataArray();
  void testexpDataArray();
  void testCreateDefaultMfH5File();
  void testexpRecharge();
  void testexpEVT();
  void testexpETS();
  void testexpOc();
  void testexpOct();
  void testGetMultiDimArrayIndex();
  void testexpSEN();
  void testexpPVAL();
  void testexpRiv();
  void testexpDRT();
  void testexpPES();
  void testiGetBcIndex();
  void testiSizeBcDataArray();
  void testexpHFB();
  void testexpLAK();
  void testexpLAKSP();
  void testexpSTR();
  void testexpSFRLine1();
  void testexpSFRLine2();
  void testexpSFRLine5();
  void testexpSFRLine6();
  void testWellPropertyList();
  void testexpMNWSetup();
  void testexpMNWStressPeriod();
  void testiGetListPackTypeFromPtr();
  void testexpUseLastAreal();
  void testexpGetAllArealUseLast();
  void testexpArealPropFromUseLast();
  void testexpArealLayFromUseLast();
  void testexpEtSegFromUseLast();
  void testexpMultFile();
  void testexpZoneFile();
  void testexpListParameterData();
  void testexpSuperFile();
  void testexpParamFile();
  void testiGetLPFParamTypes();
  void testiCountLPFParams();
  void testiWriteHufLine1to5();
  void testiWriteHGUs();
  void testiGetHUFParamTypes();
  void testiWriteHGUPar();
  void testexpSUB();
  void testexpSUBLine15();
  void testexpSUBLine16();
  void testexpUZFLine1();
  void testexpUZFLine8();
  void testexpUZFStressPeriod();
  void testexpVDFLine5();
  void testexpVDFStressPeriod();
  void testexpVSCLine3();
  void testexpVSCStressPeriod();
  void testexpGAG();
  void testexpLGR_1();
  void testexpLGR_2();
};
#endif
