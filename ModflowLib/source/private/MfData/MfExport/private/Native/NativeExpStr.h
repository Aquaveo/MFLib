//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSTR_H
#define NATIVEEXPSTR_H

class NativeExpStrT;
class EReadAsciiFile;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpStr : public NativePackExp
    {
      friend NativeExpStrT;
    public:
      NativeExpStr();
      ~NativeExpStr();
      virtual bool Export();

    private:
      NativeExpStr(const NativeExpStr& rhs);
      const NativeExpStr& operator=(const NativeExpStr& rhs);

      virtual void LastChanceBeforeWriting();

      void Line2();
      void Lines5to6();
      void Lines8to10();
      CStr Line6FromData(int a_i, const int* a_istrm, const Real* a_str);
      void Lines8to10FromData(const int* itmp, const int* icalc,
                              const int* nss, const int* itrib,
                              const int* ndiv, const Real* strm,
                              const int *itrbar, const int* idivar);

      void SaveCondFact();
      void GetParSegForAllSp();
      void CreateParSegVector(std::vector<int>& a_);
      void CheckParameters();
      CStr StrTmpFileName();
      std::map<int, double>& GetParamMap();
      void WriteParDefAndStressToTmp(std::vector<int>& a_parSeg);
      CStr Lines6ForPar(int& cnt, int nextSeg, int itmp,
                        std::vector<int>& istrm,
                        std::vector<Real>& strm);
      void StoreParDef(CStr& line, CStr& pName, int a_sp, int a_seg);
      void ReadStrDataFromTmp (FILE *fp,
                               int& itmp,
                               int& icalc,
                               int& nss,
                               int& ntrib,
                               int& ndiv,
                               std::vector<int>& istrm,
                               std::vector<Real>& strm,
                               std::vector<int>& itrbar,
                               std::vector<int>& idivar);
      CStr  ParTmpFileName(CStr a_pName);
      FILE* ParTmpFilePtr(CStr& a_pName, int a_sp, bool a_readonly);
      void  Store8to10ForPar(int sp, int itmp, int icalc,
                             int nss, int ntrib, int ndiv,
                             std::vector<int>& istrm,
                             std::vector<Real>& strm,
                             std::vector<int>& itrbar,
                             std::vector<int>& idivar);
      void AdjustStrDataUsingParams(int itmp,
                                    std::vector<int>& istrm,
                                    std::vector<Real>& strm);

      void WriteStrFile();
      void Par_Lines1to2();
      void Par_Lines3to4(std::vector< std::vector<CStr> >& a_spLines);
      void Par_Line5Data(std::vector< std::vector<int> >& a_iData);
      void Par_Line5(int a_spIdx, std::vector< std::vector<CStr> >& a_spLines,
                     std::vector< std::vector<int> >& a_iData);
      void Par_Line7(int a_spIdx, std::vector< std::vector<CStr> >& a_spLines);
      void Par_Lines8to10(EReadAsciiFile& a_File,
                          std::vector< std::vector<CStr> >& a_prev);
      void Par_SetToSkipExistingStrParams();

      std::map<int, double> m_mapParKeyVal;
      std::map<int, int>    m_mapParSegKey;
      std::map<int, int>    m_mapParSegInstances;
      std::map<int, int>    m_mapParSegNlst;

    };

  }
}

#endif

