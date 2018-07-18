//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLSTPACK_H
#define NATIVEEXPLSTPACK_H

class NativeExpLstPackT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NeLstPar;
    class H5BcList;
    class SqBcList;
    class NativeExpMf6LstPack;

    class NativeExpLstPack : public NativePackExp
    {
      friend NativeExpMf6LstPack;
      friend NativeExpLstPackT;
      friend NeLstPar;
      friend SqBcList;
    public:
      NativeExpLstPack();
      ~NativeExpLstPack();
      virtual bool Export();

    private:
      NativeExpLstPack(const NativeExpLstPack& rhs);
      const NativeExpLstPack& operator=(const NativeExpLstPack& rhs);

      virtual void OnSetData();
      virtual void LastChanceBeforeWriting();

      CStr CbFieldName();
      CStr Desc2();
      CStr Desc6();

      void Line1();
      void Line2();

      void Line5();
      void Line6();

      CStr IjkToStr(int a_i);
      CStr DataToStr(int a_i, int a_j);
      CStr GetParType();
      int  GetParamFieldIndex();
      int  GetParamFactorFieldIndex();
      void GetParameterFieldIndices(int& a_pFieldIdx,
                                    int& a_pFieldIdx2,
                                    int& a_pFactIdx,
                                    int& a_pFactIdx2);
      CStr GetParNameForBc(int a_bcIdx,
                           int a_pFieldIdx,
                           int a_pFieldIdx2,
                           std::map<int, CStr>& a_mapKeyName);
      std::map<int, CStr> GetParMap();
      bool ParametersUsed(int itmp, std::vector<CStr>& a_lns);
      void WriteParDataToTmp(int itmp,
                            std::map<CStr, std::vector<CStr> >& a_mapNameLines);
      void RewriteFileWithParameters();
      void ReadParSpInfo();
      CStr ParInfoFileName();
      CStr ParSpFileName(const CStr& a_pName, int a_sp);
      void WriteParDefToFile();
      void AddToParSpVec(CStr a_pName, int a_sp, int a_nInst,
                         std::vector<int>& a_vecSp);
      void WriteLstWithPar();
      void CheckNumBcsForPar(CStr& a_pName, int a_nBc);
      void ModifyParNameAndCreatePar(CStr& a_pName, int a_idx);
      CStr Line1to2WithDesc(CStr a_l2);
      CStr Line1a();
      CStr Desc1();
      CStr Line3(const CStr& a_pName, int a_nBc, int a_nInst);
      CStr Desc3();
      CStr Line4a(int a_sp);
      CStr Desc4a();
      CStr Desc4b();
      void GetCommentsAndLine2(const CStr& a_fname,
                               std::vector<CStr>& a_lines);
      int  CommentCharPos();
      void CommentCharPos(int a_);
      void WriteParToPackage(FILE* a_fp);
      void WriteSpWithPar(FILE* a_fp);
      CStr Line5WithPar(std::vector<int>& a_vecNoPar,
                        int a_sp, int& a_ITMP, int a_nPar);
      void Line6WithPar(FILE* a_fp, int a_sp, int a_ITMP);
      void Line7(FILE* a_fp, int a_sp);
      void BufferTheLineForComments(CStr& a_line);

      bool                m_usg, m_unstructured, m_mf6, m_disv;
      const int          *m_nBcs, *m_nAux, *m_nDataFields;
      int                 m_nFields, m_nI, m_nJ, m_nK, m_nodeOffset;
      const Real         *m_data;
      std::vector<CStr>   m_fieldStrings;
      bool                m_returnFlow;
      int                 m_NP, m_MXL;
      std::map<CStr, std::vector<int> > m_mapParSp;
      std::vector< std::vector<CStr> > m_parSp;
      std::vector<int>    m_NODLAY;

      NeLstPar *m_par;
      H5BcList *m_h5Bc;
      SqBcList *m_sqList;
      CStr      m_h5BcStr, m_h5BcStrClnWell;
    };

  }
}

#endif

