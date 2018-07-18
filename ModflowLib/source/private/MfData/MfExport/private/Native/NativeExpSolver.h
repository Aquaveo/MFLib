//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSOLVER_H
#define NATIVEEXPSOLVER_H

class NativeExpSolverT;
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpSolver : public NativePackExp
    {
      friend NativeExpSolverT;
    public:
      NativeExpSolver();
      ~NativeExpSolver();
      virtual bool Export();

    private:
      NativeExpSolver(const NativeExpSolver& rhs);
      const NativeExpSolver& operator=(const NativeExpSolver& rhs);

      CStr Desc(CStr a_name, int a_line);
      void Export_SIP();
      CStr Line1_SIP();
      CStr Line2_SIP();

      void Export_DE4();
      CStr Line1_DE4();
      CStr Line2_DE4();

      void Export_SOR();
      CStr Line1_SOR();
      CStr Line2_SOR();

      void Export_PCG();
      CStr Line1_PCG();
      CStr Line2_PCG();

      void Export_PCGN();
      CStr Line1_PCGN();
      CStr Line2_PCGN();
      CStr Line3_PCGN();
      CStr Line4_PCGN();

      void Export_LMG();
      CStr Line1_LMG();
      CStr Line2_LMG();
      bool WriteLine3_LMG();
      CStr Line3_LMG();
      CStr Line4_LMG();

      void Export_GMG();
      CStr Line1_GMG();
      CStr Line2_GMG();
      CStr Line3_GMG();
      CStr Line4_GMG();

      void Export_NWT();
      CStr Line1_NWT();
      int  WhichLine2_NWT();
      CStr Line2a_NWT();
      CStr Line2b_NWT();

      void Export_SMS();
      bool WriteLine1a_SMS();
      bool WriteLine2_SMS();
      bool WriteLine3_SMS();
      bool WriteLine4_SMS();
      CStr Line1a_SMS();
      CStr Line1b_SMS();
      CStr Line2_SMS();
      CStr Line3_SMS();
      CStr Line4_SMS();

      void Export_IMS();
      CStr GetPrintOption();
      CStr GetImsNonlinearLine();
      CStr GetImsLinearLine();
    };

  }
}

#endif

