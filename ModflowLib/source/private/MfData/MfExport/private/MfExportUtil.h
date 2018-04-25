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
  class MfGlobal;
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
      static CStr GetMf6ArrayString(MfGlobal* a_g, Mf2kNative* a_native,
                                    CStr a_packName);
      static void Mf6StringToArray(const CStr& a_str, std::vector<Real>& a_array,
        int a_nVals);
      static void Mf6StringToArray(const CStr& a_str, std::vector<int>& a_array,
        int a_nVals);
      static void Mf6MultiLayerStringToArray(const CStr& a_str,
        std::vector<int>& a_array, int a_nLay, int a_nValsPerLayer);
      static void Mf6MultiLayerStringToArray(const CStr& a_str,
        std::vector<Real>& a_array, int a_nLay, int a_nValsPerLayer);
      static CStr GetMf6CommentHeader();
      static void Mf6IboundToIdomain(MfGlobal* a_g, Mf2kNative* a_native);
      static CStr Mf6IboundToChd(MfGlobal* a_g, int& MAXBOUND,
        const std::vector<CStr>& a_fieldStrings);

    private:
    };
  }
}

#endif

