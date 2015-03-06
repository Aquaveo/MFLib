//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef ARRAYREADER_H
#define ARRAYREADER_H

#include <private\util\util.h>

////////////////////////////////////////////////////////////////////////////////
/// \class ArrayReader
/// \brief Used to read data from an HDF5 file and give it to the MODFLOW
///  U2DREL subroutine.
////////////////////////////////////////////////////////////////////////////////
class ArrayReader
{
public:
  ArrayReader(const CStr &a_="");
  ~ArrayReader();

  bool  ValidInputString() const;
  void  SetKvar(int a_);

  int    GetIPRN() const;
  double GetMultiplier() const;
  void   GetData(double *a_arr, size_t a_size, const CStr& a_name) const;
  void   GetData(float *a_arr, size_t a_size, const CStr& a_name) const;
  void   GetData(int *a_arr, size_t a_size, const CStr& a_name) const;

private:
  ArrayReader(const ArrayReader &rhs);
  const ArrayReader &operator= (const ArrayReader &rhs);

  template <class T>
  void GetDataT(T *a_arr, size_t a_size, const CStr& a_name) const;

  class impl;
  impl  *m_impl;
};

#endif
