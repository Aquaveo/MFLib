//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef TXTEXPORTER_H
#define TXTEXPORTER_H

#include <private\util\util.h>

class TxtExporterT;
class ExpGmsH5T;
class ExpGmsH5Public;

class TxtExporter
{
friend TxtExporterT;
friend ExpGmsH5T;
public:
  TxtExporter(const char *a_base);
  virtual ~TxtExporter();

  void SetBaseFileName(const char *a_base);
  const char * const GetBaseFileName();
  bool WriteLineToFile(const char *a_type,
                       const char *a_line);
  bool WriteLineToFile(const char *a_type,
                       const std::string& a_line);
  bool WriteStringToFile(const char *a_type,
                         const char *a_string);
  bool WriteLinesAndDescriptionsToFile(const char* a_type,
                                       std::vector<CStr>& a_lines,
                                       std::vector<CStr>& a_desc);
  CStr GetExtension(const char *a_type);
  void SetTypesToExtensions(const std::map<CStr, CStr> &a_);
  bool IsTypeSupported(const char *a_type);
 
  bool FileTypeExists(const char *a_type);

  bool& AtLeastOneTransientSPExists();
  std::set<int>& SetOfSteadyStateStressPeriods();
  int GetMaxLineLengthFromType(const char * const a_);
  bool FirstStressIsSteadyState();
  bool ClearFile(const char* const a_);

  std::map<CStr, std::vector<Real> >& HfbParData() { return m_hfbPar; }

  ExpGmsH5Public* m_public;


private:
  TxtExporter(const TxtExporter &rhs);
  const TxtExporter& operator=(const TxtExporter &rhs);

  void GetFileContents(const char *a_type,
                       CStr &a_stream);

  class impl;
  impl *m_p;

  std::map<CStr, std::vector<Real> > m_hfbPar;
};
#endif
