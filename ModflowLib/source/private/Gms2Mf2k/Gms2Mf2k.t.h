//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef GMS2MF2K_T_H
#define GMS2MF2K_T_H
#include <cxxtest/TestSuite.h>

class Gms2Mf2kT : public CxxTest::TestSuite
{
public:
  void testConvert_SmallGrid_Trans();
  void testConvert_sg_t_pest();
  void testConvert_sg_ss_pest();
  void testConvertNameWithSpace();
  void testConvertNoSeawatAux();
  void testConvertWithSeawatAux();
  void testConvertWrongOrderedAux();
  void testConvertMNW2();
  void testUsgArrayAndList();
  void testUsgSfr();
  void testUsgStr();
};


#endif
