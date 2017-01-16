//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPARR1D_H
#define NATIVEEXPARR1D_H

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpArr1d : public NativePackExp
    {
    public:
      NativeExpArr1d();
      ~NativeExpArr1d();
      virtual bool Export();

    private:
      NativeExpArr1d(const NativeExpArr1d& rhs);
      const NativeExpArr1d& operator=(const NativeExpArr1d& rhs);

      CStr GetFname(const int* K);
      void AddLine(const int* IPRN, const Real* MULT, CStr fname);

      //------------------------------------------------------------------------
      template<typename T>
      bool ExportT (const int* JJ, const int* IPRN, const T* ARR,
                    const Real* MULT, const int* K)
      {
        if (1.0 == *MULT)
        {
          if (IsConstant(JJ, ARR))
          {
            CStr str;
            str.Format("CONSTANT %s", STR(ARR[0]));
            AddToStoredLinesDesc(str, "");
            return true;
          }
        }

        if (!GetNative()->GetArraysInternal())
          WriteExternal(JJ, IPRN, ARR, MULT, K);
        else
          WriteInternal(JJ, IPRN, ARR, MULT);

        // if we are doing SQLite we need to get a copy of the array so we can
        // be sure that MODFLOW has not modified the values
        if (GetNative()->GetUseSQLite())
        {
          std::vector<T> vec;
          MfPackage* p = GetPackage();
          CStr packageName(p->PackageName());
          CStr layerString;
          layerString.Format("_Layer_%d", *K);
          CStr vname = packageName + "_ARR" + layerString;
          MfGlobal* g = GetGlobal();
          g->GetVector(vname, vec);
          vec.resize(*JJ);
          for (int i = 0; i < *JJ; ++i) vec[i] = ARR[i];
          g->SetVector(vname, vec);

          vname = packageName + "_MULT" + layerString;
          g->SetRealVar(vname, *MULT);
          vname = packageName + "_IPRN" + layerString;
          g->SetIntVar(vname, *IPRN);
        }

        return true;
      } // ExportT
      //------------------------------------------------------------------------
      template<typename T>
      bool IsConstant (const int* JJ, T* ARR) const
      {
        bool constant(true);
        for (int i=1; i<*JJ && constant; ++i) {
          if (ARR[0] != ARR[i]) constant = false;
        }
        return constant;
      } // IsConstant
      //------------------------------------------------------------------------
      template<typename T>
      void WriteExternal (const int* JJ, const int* IPRN, const T* ARR,
                          const Real* MULT, const int* K)
      {
        CStr fname = GetFname(K);
        std::fstream os;
        os.open((LPCTSTR)fname, std::ios_base::out);
        if (os.bad()) return;
        WriteToStream(os, JJ, ARR);
        os.close();
        AddLine(IPRN, MULT, fname, ARR);
      } // WriteExternal
      //------------------------------------------------------------------------
      CStr MultString (const float*, const Real* MULT)
      {
        return STR(*MULT);
      }
      CStr MultString (const double*, const Real* MULT)
      {
        return STR(*MULT);
      }
      CStr MultString (const int*, const Real* MULT)
      {
        CStr s;
        s.Format("%d", (int)*MULT);
        return s;
      }
      //------------------------------------------------------------------------
      template<typename T>
      void WriteInternal  (const int* JJ, const int* IPRN, const T* ARR,
                           const Real* MULT)
      {
        CStr str, strIprn, strMult;
        strIprn.Format("%5d", *IPRN);
        strMult = MultString(ARR,MULT);
        str.Format("INTERNAL %s (FREE) %s", strMult, strIprn);
        AddToStoredLinesDesc(str, "");
        std::stringstream os;
        WriteToStream(os, JJ, ARR);
        str = os.str();
        while (str.at(str.GetLength()-1) == '\n')
        {
          str.pop_back();
        }
        AddToStoredLinesDesc(str, "");
      } // WriteInternal
      //------------------------------------------------------------------------
      template<typename T>
      void WriteToStream (std::ostream& a_os, const int* JJ, const T* ARR) const
      {
        for (int i=0; i<*JJ; ++i)
        {
          a_os << STR(ARR[i]) << " ";
          if (i > 0 && i%10 == 0) a_os << "\n";
        }
      } // WriteToStream
      //------------------------------------------------------------------------
      template<typename T>
      void AddLine (const int* IPRN, const Real* MULT, CStr fname, const T* ARR)
      {
        CStr strIprn, strMult;
        strIprn.Format("%5d", *IPRN);
        strMult = MultString(ARR, MULT);
        util::StripPathFromFilename(fname, fname);
        if (GetNative()->GetArraysInFolder())
        {
          fname = ".\\arrays\\" + fname;
        }
        CStr str;
        str.Format("OPEN/CLOSE %s %s (FREE) %s", fname, strMult, strIprn);
        AddToStoredLinesDesc(str, "");
      } // NativeExpArr1d::AddLine

    }; // class NativeExpArr1d
  } // namespace Export
} // namespace MfData

#endif

