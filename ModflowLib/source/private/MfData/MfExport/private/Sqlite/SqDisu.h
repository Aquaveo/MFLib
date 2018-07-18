//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SqDisu_H
#define SqDisu_H

#include <private/util/util.h>
#include <private/MfData/MfExport/private/Sqlite/SqExporter.h>

namespace MfData {

//----- Forward declarations----------------------------------------------------

namespace Export {

//----- Forward declarations----------------------------------------------------
class NativePackExp;
class NativeExpDisu;

////////////////////////////////////////////////////////////////////////////////
class SqDisu : public SqExporter
{
public:
  explicit SqDisu(NativeExpDisu* a_);
  SqDisu();
  ~SqDisu();

  void Export();
  virtual void ExportArray(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const float* a_array, Real a_mult,
                int a_layer);
  virtual void ExportArray(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const double* a_array, Real a_mult,
                int a_layer);
  virtual void ExportArray(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer);

private:
  class impl;
  impl *m_p;
}; // class SqDisu

} // namespace Export
} // namespace MfData
#endif
