//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPGMSH5_H
#define EXPGMSH5_H

#include <private\MfData\MfExport\private\MfExporterImpl.h>

class ExpGmsH5T;
class ExpGmsH5Public;

namespace MfData
{
  class MfGlobal;

  namespace Export
  {
    class ExpGmsH5 : public MfExporterImpl
    {
    friend ExpGmsH5T;
    public:
      explicit ExpGmsH5(bool a_compressed=true);
      explicit ExpGmsH5(const char *a_fileName);
      virtual ~ExpGmsH5();

      virtual void SetFileName(const char *a_);

      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package);

    private:
      ExpGmsH5(const ExpGmsH5 &rhs);
      const ExpGmsH5& operator=(const ExpGmsH5 &rhs);

      class impl;
      impl *m_p;
    };
  }
}
ExpGmsH5Public* New_ExpGmsH5Public();
void Delete_ExpGmsH5Public(ExpGmsH5Public* a_);
int xfpWriteDatasetString(hid_t a_Loc,
                          const char *a_Name,
                          const char *a_Str);
int xfpWriteAttributeInt(hid_t a_Loc,
                         const char *a_Name,
                         int a_Number,
                         int *a_val);
int xfpWriteAttributeString(hid_t a_Loc,
                            const char * a_Name,
                            const char * a_Str);
int xfpWriteAttributeDouble(hid_t a_Loc,
                            const char *a_Name,
                            int a_Number,
                            double *a_val);
void expGmsH5_CreateDefaultH5File (const char *a_,
                                   int a_modelType,
                                   bool a_compress);
void expGmsH5_CreateWelClnGroup (const char *a_,
                                 bool a_compress);

#endif
