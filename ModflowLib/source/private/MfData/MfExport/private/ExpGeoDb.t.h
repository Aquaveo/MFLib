//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPGEODB_T_H
#define EXPGEODB_T_H

#include <cxxtest/TestSuite.h>

class ExpGeoDbT : public CxxTest::TestSuite
{
public:
  void testExportPackage();
  void testExportWEL();
  void testExportRIV();
  void testGetArrayTableAndField();
  void testTableUsesStressPeriod();
  void testGetQueryString();
  void testGetSolverFieldsFromPackage();
  void testGetDataFromPackage();
  void testGetSENFields();
  void testGetPESFields();
  void testGetOBFields();
  void testGetOBFields_2005();
  void testGetOCFields();
  void testExportOC();
  void testExportFloObs();
  void testExportFLOB();
  void testExportFLOBFactors();
  void testExportHOBPack();
  void testExportHOB();
  void testExportHOBLayers();
  void testExportHOBTimes();
  void testExportOBSVars();
  void testExportOBSVars_2005();
  void testExportOB1();
  void testExportPES();
  void testExportSEN();
  void testExportHFB();
  void testGetArealFields();
  void testExportArealCBF();
  void testExportArealOption();
  void testExportArealArrayFlags();
  void testExportAreal();
  void testExportBAS();
  void testExportBCFCbf();
  void testExportBCFVars();
  void testExportBCFLayers();
  void testExportBCF();
  void testExportLPF99();
  void testExportLPFCbf();
  void testExportLPFVars();
  void testExportLPFLayers();
  void testExportLPF();
  void testExportSolver();
  void testExportDE4SolverLine1();
  void testExportDE4SolverLine2();
  void testExportListPackData();
  void testExportListPackITMP();
  void testExportListPackCBF();
  void testExportDISVars();
  void testExportStressPeriods();
  void testExportDelRC();
  void testExportLayCBD();
  void testExportMultZoneName();
  void testExportMultZoneVals();
  void testExportMultArrayFunc();
  void testCalculateNumDbFields();
  void testCalcListBcCellId();
  void testExportListParameterData();
  void testExportHFBListParameterData();
  void testExportParamTable();
  void testExportParInstances();
  void testExportParInstSp();
  void test1GetInstances();
  void testGetClusterData();
  void testExportClstTab();
  void testExportIZTab();
  void testExportPilotPointTab();
  void testPPToMultNameTab();
  void testPPToMultipliersTab();
  void testNameFile();
  void testNameFileFilename();
  void testCheckFieldIsParameterFactorFieldAndUpdateName();
  void testExportUZFVars();
  void testExportUZFCbf();
  void testExportUZFLine8();
  void testExportUZFStressPeriod();
  void testExportSFRLine1();
  void testExportSFRLine2();
  void testExportSFRLine5();
  void testExportSFRLine6();
  void testExportLAK();
  void testExportLAKSP();
  void testExportSTRCbf();
  void testExportSTR();
  void testExportSTRSP();
  void testExportHUFCbf();
  void testExportHUFVars();
  void testExportHUFLayers();
  void testiWriteHGUs();
  void testExportGAG();
};


#endif
