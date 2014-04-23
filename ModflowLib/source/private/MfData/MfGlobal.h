//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFGLOBAL_H
#define MFGLOBAL_H

#include <private/util/util.h>

class MfGlobalT;
class ParamList;

namespace MfData
{
  class MfPackage;
  
  enum enumModelType { MF2K = 0, MF2K5, MFNWT, SEAWAT, LGR };

  class MfGlobal
  {
  friend MfGlobalT;
  public:
    MfGlobal(int a_NROW,
             int a_NCOL,
             int a_NLAY,
             int a_NPER,
             int a_ITMUNI,
             int a_LENUNI,
             const int *a_LAYCBD);
    ~MfGlobal();
    MfGlobal(const MfGlobal &rhs);
    const MfGlobal &operator=(const MfGlobal &rhs);

    int ModelType();
    int NumRow();
    int NumCol();
    int NumLay();
    int NumPeriods();
    int LengthUnit();
    int TimeUnit();
    int *LayCbd();
    ParamList& GetParamList();
    size_t CurModIdx();


    void SetIntVar(const char* a_NAME,
                   int a_var);
    bool GetIntVar(const char* a_NAME,
                   int& a_var);
    void SetRealVar(const char* a_NAME,
                    Real a_var);
    bool GetRealVar(const char* a_NAME,
                    Real& a_var);
    void SetStrVar(const char* a_NAME,
                   const CStr& a_var);
    bool GetStrVar(const char* a_NAME,
                   CStr& a_var);
    void PutCurrentPeriod(int a_);
    int  GetCurrentPeriod();

    bool AddPackage(MfPackage *a_);
    //bool ExportArray(MfPackage *a_);
    MfPackage *GetPackage(const char *a_);
    bool Export(const char *a_);
    void IGRID(int a_IGRID);
    void LgrName(const char *a_);
    const char* LgrName();
    void ModelType(int a_modelType);

    static MfGlobal& Get();
    static void Init(int a_modelType,
                     int a_IGRID,
                     const char *a_exp,
                     const char *a_fileName,
                     const char *a_tables);
    static void Set(int a_NROW,
                    int a_NCOL,
                    int a_NLAY,
                    int a_NPER);
    static void Set(int a_NROW,
                    int a_NCOL,
                    int a_NLAY,
                    int a_NPER,
                    int a_ITMUNI,
                    int a_LENUNI,
                    const int *a_LAYCBD);

  private:

    class impl;
    impl *m_p;
  };

  MfGlobal& Get();
  void Init(int a_modelType,
            int a_IGRID,
            const char *a_exp,
            const char *a_fileName,
            const char *a_packages);
  void Set(int a_NROW,
           int a_NCOL,
           int a_NLAY,
           int a_NPER);
  void Set(int a_NROW,
           int a_NCOL,
           int a_NLAY,
           int a_NPER,
           int a_ITMUNI,
           int a_LENUNI,
           const int *a_LAYCBD);
}

#endif
