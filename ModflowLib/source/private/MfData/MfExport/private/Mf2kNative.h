//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MF2KNATIVE_H
#define MF2KNATIVE_H

#include <private\MfData\MfExport\private\MfExporterImpl.h>

namespace MfData
{
  class MfGlobal;

  namespace Export
  {
    class Mf2kNative : public MfExporterImpl
    {
    public:
      Mf2kNative();

      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package);
      void SetArraysInFolder(bool a_);
      bool GetArraysInFolder() const {return m_arraysInFolder;}
      void SetArraysInternal(bool a_);
      bool GetArraysInternal() const {return m_arraysInternal;}
#pragma warning(disable : 4100)
      //virtual bool ExportArray(MfGlobal *a_global,
      //                         MfPackage *a_package) {return false;}

    private:
      Mf2kNative(const Mf2kNative &rhs);
      const Mf2kNative& operator=(const Mf2kNative &rhs);

      bool m_arraysInFolder;
      bool m_arraysInternal;
    };
  }
}

#endif
