//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SqExporter_H
#define SqExporter_H
#include <private\util\util.h>

namespace MfData {

//----- Forward declarations----------------------------------------------------

namespace Export {

//----- Forward declarations----------------------------------------------------

class NativePackExp;

////////////////////////////////////////////////////////////////////////////////
class SqExporter
{
public:
  SqExporter() {}
  virtual ~SqExporter();

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

}; // class SqExporter

} // namespace Export
} // namespace MfData
#endif
