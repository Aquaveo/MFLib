//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFEXPORTER_H
#define MFEXPORTER_H

namespace MfData
{
  class MfGlobal;
  class MfPackage;

  namespace Export
  {
    class MfExporterImpl;

    class MfExporter
    {
    public:
      MfExporter(const char *a_);
      ~MfExporter();

      const char *GetTypeName();
      void SetFileName(const char *a_);
      void SetTablesStr(const char* a_);
      const char* GetTablesStr();
      int GetModelType();
      void SetModelType(int a_modelType);

      bool ExportPackage(MfGlobal *a_global,
                         MfPackage *a_package);

      bool CanExportTable(const char *a_);
      bool Compress() const;

    private:
      MfExporter(const MfExporter &rhs);
      const MfExporter& operator=(const MfExporter &rhs);

      MfExporterImpl *m_p;
    };
  }
}

#endif

