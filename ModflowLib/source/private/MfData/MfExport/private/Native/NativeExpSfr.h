//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSFR_H
#define NATIVEEXPSFR_H

class NativeExpSfrT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpSfr : public NativePackExp
    {
      friend NativeExpSfrT;
    public:
      NativeExpSfr(bool a_h5);
      ~NativeExpSfr();
      virtual bool Export();

    private:
      NativeExpSfr(const NativeExpSfr& rhs);
      const NativeExpSfr& operator=(const NativeExpSfr& rhs);

      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

      void Line1 ();
      void Line2 ();
      void Line5 ();
      void Line6 ();
      void Line6a (int nseg);
      void Line6b (int nseg, int isfropt, int kper);
      void Line6c (int nseg, int isfropt, int kper);
      void Line6d (int nseg, int isfropt, int kper);
      void Line6e (int nseg, int isfropt, int kper);

      CStr StrVal (int a_);
      CStr StrVal (Real a_);
      CStr Iseg (int a_i, int a_j);
      CStr Iotsg (int a_i);
      CStr Idivar (int a_i, int a_j);
      CStr Idivar (int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
      CStr Seg (int a_i, int a_j);
      CStr Seg (int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
      CStr Xsec (int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
      CStr Qstage (int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);

      bool        m_h5;
      const int  *m_iseg, *m_iotsg, *m_idivar;
      const Real *m_segOrig, *m_xsec, *m_qstage;
      bool        m_writingPar;
      Real       *m_seg;
      std::vector<Real> m_segVec;
      int         m_sz;

      virtual void   LastChanceBeforeWriting();
      CStr           CopySfrFile();
      void           RewriteLines1and2(CStr& a_tmpF, int a_npar);
      void           WriteLines3and4(std::vector< std::vector<CStr> >& a_vec);
      void           WriteLines5to7(std::vector< std::vector<CStr> >& a_vec);
      CStr           Line4a(int a_sp);
      void           Line4bto4g(std::vector<CStr>& a_lines);
      CStr           Line3(CStr& a_name, int a_nSeg, int a_nInst, int a_cnt);
      void           GetParameterStessPeriods(CStr& a_pname,
                                              std::vector<int>& a_vStress,
                                              std::vector<int>& a_vNseg);
      void           GetParameterNSegNInst(std::vector<int>& a_vStress,
                                           std::vector<int>& a_vNseg,
                                           std::map<int, int>& a_mapNseg_NInst);
      int            GetITMP();
      void           WriteCommentsSfr();
      void           WriteStoredLinesSfr();
      std::map<int, CStr>& GetParMap();
      std::map<int, Real>& GetParMapVal();
      void           CheckSegForPar(std::map<CStr, std::vector<int> >& a_map,
                                    int a_nss);
      void           ParToTmp(std::map<CStr, std::vector<int> >& a_map,
                              int isfropt, int curSp);
      bool           WriteStoredLinesToTmp(const CStr& a_pname, int a_curSp);
      CStr           TmpFileName(const CStr& a_name);
      bool           NoParamExists();
      void           AddToPval(const CStr& a_name, double a_val);
      int            NparToWrite();
      void           SaveCondFact();


      std::map<int, CStr> m_mapKeyName;
      std::map<int, Real> m_mapKeyVal;
      bool                m_usg, m_unstructured;
    };

  }
}

#endif

