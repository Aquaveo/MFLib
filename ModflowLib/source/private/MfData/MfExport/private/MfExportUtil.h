//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFEXPORTUTIL_H
#define MFEXPORTUTIL_H

#include <private\util\util.h>
class ParamList;
class Param;
namespace MfData
{
  namespace Export
  {
    class MfExporterImpl;
    class Mf2kNative;

    class MfExportUtil
    {
    public:
      static MfExporterImpl *CreateExporter(const char *a_type);

      static bool IsDataArray(const CStr &a_name,
                              std::map<CStr, CStr>& a_map);
      static bool Is1dArray(const CStr &a_name);
      static bool IsSolver(const CStr& a_name);
      static std::set<CStr> LpfParamTypes();
      static void InsertSingleQuotesInName(CStr& a_name);
      static std::set<CStr> HufParamTypes();
      static std::vector<Param> GetParamsOfType(const char* const a_type);
      static bool ArrayWriteNextLineInternal(Mf2kNative* a_native,
                                             const CStr& a_line);

    private:
    };
  }
}

#endif

