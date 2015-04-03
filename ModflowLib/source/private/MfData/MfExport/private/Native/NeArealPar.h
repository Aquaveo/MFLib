//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NEAREALPAR_H
#define NEAREALPAR_H

#include <private/util/util.h>

class Param;
class ParamList;
namespace MfData
{
  namespace Export
  {
    class NativePackExp;

    class NeArealPar
    {
    public:
      NeArealPar();
      ~NeArealPar();

      void SetPackage(NativePackExp* a_p);
      int  NumPar();
      bool WriteStressPar(int a_sp);
      bool ClustersDefinedFromStart();

      void Line1();
      void Lines3to4(bool a_beginExport);
      CStr GetParInstanceFromSpIdx(Param& a_p, int a_sp);
      bool ParNamesFromInstanceData(Param& a_, int a_sp,
                                    ParamList* a_pList,
                                    std::vector<CStr>& a_pnames);
      void LineWithPar(int a_sp, CStr& a_line5);
      void RewriteFileWithParameters();
      void RemoveArrayFile(CStr a_line);

    private:
      CStr           m_type;
      bool           m_laySpecified;
      int            m_NETSEG;
      NativePackExp* m_pack;
    };

  }
}

#endif

