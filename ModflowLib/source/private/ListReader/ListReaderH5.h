//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADERH5_H
#define LISTREADERH5_H

#include <private\util\util.h>

class ListReaderParser;
class ListReaderSetUp;
class ListReaderH5T;

class ListReaderH5
{
  friend ListReaderH5T;
public:
  ListReaderH5(const ListReaderParser &a_parser,
               const ListReaderSetUp &a_setup);

  bool FillInData(double *a_);
  bool FillInData(float *a_);

private:
  ListReaderH5(const ListReaderH5 &rhs);
  const ListReaderH5& operator=(const ListReaderH5 &rhs);

  //void CreatePath(int a_);

  template <class T>
  bool FillInDataT(T *a_);
  template <class T>
  bool GetCellKIJ(T *a_) const;
  template <class T>
  bool GetIface(T *a_) const;
  template <class T>
  bool GetStressData(T *a_) const;
  template <class T>
  bool GetFactor(T *a_,
                 const int a_auxIdx,
                 const int a_sheadFact) const;
  template <class T>
  bool GetCellGroup(T *a_) const;
  template <class T>
  bool GetSeawatAux(T *a_) const;

  int  GetNumFactors() const;
  bool GetFactor(std::vector<Real> &a_,
                 const int a_sheadFact) const;
  bool GetFactorVer1(std::vector<Real> &a_) const;
  bool GetVersion();
  int  GetAuxIdx(const char * a_name) const;
  int  GetSeawatAuxH5Idx(const char * a_name) const;
  void GetH5IndicesForSeawatAux(const std::vector<CStr> a_auxNames,
                                std::vector<std::pair<int, int> >& a_indices) const;

  int    m_stress, m_nRows, m_nFields, m_ial, m_nAuxFields,
         m_nGridRows, m_nGridCols, m_nCbcFields;
  double m_fileVersion;
  CStr m_file, m_path;
  std::vector<CStr> m_auxNames;
};

std::map<CStr, Real>& listReader_GetMapId();
#endif

