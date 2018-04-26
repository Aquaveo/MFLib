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
    class CellNumbering;
    class Mf2kNative : public MfExporterImpl
    {
    public:
      Mf2kNative();
      ~Mf2kNative();

      virtual void SetFileName(const char *a_);
      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package);
      void SetArraysInFolder(bool a_);
      bool GetArraysInFolder() const {return m_arraysInFolder;}
      void SetArraysInternal(bool a_);
      bool GetArraysInternal() const {return m_arraysInternal;}
      void SetUseH5(bool a_, bool a_compress);
      bool GetUseH5() const { return m_h5; }
      void SetUseSQLite(bool a_) { m_sqlite = a_; }
      bool GetUseSQLite() const { return m_sqlite; }
      void SetExportMf6(bool a_) { m_exportMf6 = a_; }
      bool GetExportMf6() const { return m_exportMf6; }

      CellNumbering* GetCellNumbering() { return m_cn; }
      void SetCellNumbering(CellNumbering* a_) { m_cn = a_; }

    private:
      Mf2kNative(const Mf2kNative &rhs);
      const Mf2kNative& operator=(const Mf2kNative &rhs);

      bool m_arraysInFolder;
      bool m_arraysInternal;
      bool m_h5;
      bool m_sqlite;
      bool m_exportMf6;
      CellNumbering* m_cn;
    };
  }
}

#endif
