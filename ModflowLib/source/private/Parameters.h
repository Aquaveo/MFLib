//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <private/util/util.h>

class ParamList;
class ParPub;

namespace Parameters
{
  bool SetFileName(const char * const a_fName);
  bool SetSenFileName(const char * const a_fName);
  bool FillInParType(const int* a_NPVAL,
                     const char* a_PARNAM,
                     char* a_PARTYP);
  bool SubstituteArray(double *a_,
                       size_t a_size,
                       int a_layer,
                       const CStr& a_name);
  bool SubstituteArray(float *a_,
                       size_t a_size,
                       int a_layer,
                       const CStr& a_name);
  void SubstituteArray(int *a_,
                       size_t a_size,
                       int a_layer,
                       const CStr& a_name);
  bool SubstituteValue(double *a_);
  bool SubstituteList(std::vector<Real> &a_,
                      const std::vector<Real> &a_fact,
                      const char * const a_parType);
  bool SubstituteProperty(CAR_DBL2D &a_,
                          const CStr &a_parType,
                          const int a_valueIndex,
                          const int a_factorIndex);
  bool ParTypeFromH5Path(const char * const a_path,
                         CStr &a_type);
  bool CheckListSubstituteOk();
  bool CheckArraySubstituteOk(const char * const a_h5Path);
  void test_ClearParData();
  void GetParameterList(ParamList **a_);
  void FillArrayParamData();
  void ExportParameterArrayToDataSet(CStr& a_NAME,
                                     const Real* a_ARR,
                                     const Real* a_MULT,
                                     const int* a_K,
                                     const int* a_JJ,
                                     const int* a_II);
  void ExportParameterArrayToDataSet8(CStr& a_NAME,
                                      const double* a_ARR,
                                      const Real* a_MULT,
                                      const int* a_K,
                                      const int* a_JJ,
                                      const int* a_II);
  void ExportParameterArrayToDataSet(CStr& a_NAME,
                                     const int* a_ARR,
                                     const int* a_MULT,
                                     const int* a_K,
                                     const int* a_JJ,
                                     const int* a_II);
}

ParPub* New_ParPub();
void Delete_ParPub(ParPub* a_);

#endif
