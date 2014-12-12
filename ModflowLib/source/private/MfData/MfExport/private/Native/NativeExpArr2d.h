//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPARR2D_H
#define NATIVEEXPARR2D_H

class ParamList;
class Param;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpArr2d : public NativePackExp
    {
    public:
      NativeExpArr2d();
      ~NativeExpArr2d();
      virtual bool Export();

    protected:
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);

    private:
      NativeExpArr2d(const NativeExpArr2d& rhs);
      const NativeExpArr2d& operator=(const NativeExpArr2d& rhs);

      CStr ArrayName();
      bool GetData();
      void SaveIbound();
      CStr GetArrayFileName(const CStr& a_name);
      bool CanDoConstant();
      bool WriteInternalArray();
      void WriteToFile();
      void ArrayDataToFile(const CStr& a_fname,
                           const Real* a_data,
                           const int* a_idata);
      void ArrayDataToStream(std::ostream& a_os,
                             const Real* a_data,
                             const int* a_idata);
      CStr StrIprn();
      CStr StrMult();
      CStr ArrayFileName(const CStr& a_fname);
      bool CheckParameters();
      void WriteZoneMultArrays(std::set<double>& a_keys, ParamList* a_list);
      void WritePilotPointMultArrays(ParamList* a_list, Param& a_p);
      CStr ParArrayName();
      void EnsureParCluster(Real a_key, ParamList* a_list);
      void SubstituteMultArray();
      void EtsSegnumber(CStr& a_layStr, CStr& a_name);
      bool StressPeriodPar(const CStr& a_type);
      void CreateFolderIfNeeded();

      CStr        m_name;
      const int*  m_lay, *m_iData, *m_iMult, *m_iPRN;
      const Real* m_dataConst, *m_mult;
      Real*       m_data;
      int         m_nrow, m_ncol, m_curSp;
      bool        m_firstTime;
      std::vector<double> m_pilotKeys;
    };

  }
}

#endif

