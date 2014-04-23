//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADER_H
#define LISTREADER_H

#include <private\util\util.h>
class ListReaderParser;
////////////////////////////////////////////////////////////////////////////////
/// \class ListReaderSetUP
/// \brief Used to set up the list reader class.
////////////////////////////////////////////////////////////////////////////////
class ListReaderSetUp
{
public:
  ListReaderSetUp(int a_nRows=0, int a_nFields=0, int a_ial=0, int a_nAuxFields=0,
                  int a_nGridCols=0, int a_nGridRows=0, CStr a_line="",
                  std::vector<CStr> a_auxNames=std::vector<CStr>()) :
    m_nRows(a_nRows),
    m_nFields(a_nFields),
    m_ial(a_ial),
    m_nAuxFields(a_nAuxFields),
    m_nGridCols(a_nGridCols),
    m_nGridRows(a_nGridRows),
    m_line(a_line),
    m_auxNames(a_auxNames) {}
  ListReaderSetUp(const ListReaderSetUp &rhs) :
    m_nRows(rhs.m_nRows),
    m_nFields(rhs.m_nFields),
    m_ial(rhs.m_ial),
    m_nAuxFields(rhs.m_nAuxFields),
    m_nGridCols(rhs.m_nGridCols),
    m_nGridRows(rhs.m_nGridRows),
    m_line(rhs.m_line),
    m_auxNames(rhs.m_auxNames) {}

  int m_nRows, m_nFields, m_ial, m_nAuxFields, m_nGridRows, m_nGridCols;
  CStr m_line;
  std::vector<CStr> m_auxNames;
private:
  const ListReaderSetUp& operator=(const ListReaderSetUp &rhs);
};

////////////////////////////////////////////////////////////////////////////////
/// \class ListReader
/// \brief Used to read data from an HDF5 file and give it to the MODFLOW
///  ULSTRD subroutine.
////////////////////////////////////////////////////////////////////////////////
class ListReader
{
public:
  ListReader(ListReaderSetUp &a_);

  bool  ValidSetUp() const;

  bool  GetData(std::vector<double> &a_) const;
  bool  GetData(double *a_) const;
  bool  GetData(std::vector<float> &a_) const;
  bool  GetData(float *a_) const;

protected:
  const ListReaderSetUp& SetUp() const;
  const ListReaderParser& Parser() const;

private:
  ListReader(const ListReader &rhs);
  const ListReader& operator=(const ListReader &rhs);

  class impl;
  impl *m_impl;
};

#endif

