//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMLIST_H
#define PARAMLIST_H

#include <private\util\util.h>

class Param;
class ParamListT;
class ParPub;

class ParamList
{
  friend ParamListT;
public:
  ParamList();
  ~ParamList();

  size_t Size() const;
  void   Clear();
  bool   PushBack(Param *a_);
  bool   At(size_t a_i,
            Param *a_) const;
  bool   FindByName(const char *a_name,
                    Param *a_) const;
  bool   FindByKey(double a_key,
                   Param *a_) const;
  bool   FindFromPilotName(const char *a_name,
                           const char *a_type,
                           Param *a_) const;
  bool   GetPilotPtValues(int a_ptSetIndex,
                          std::vector<double> &a_vals) const;
  bool   GetPilotPtIsens(int a_ptSetIndex,
                         std::vector<int> &a_vals) const;
  bool   GetParMultArray(Param *a_,
                         std::vector<Real> &a_vals) const;

  void   SetSourceFile(const char *a_fName);
  const char * GetSourceFile() const;
  bool   UpdateParameter(Param *a_);
  bool   SetPilotPtVal(const char *a_name,
                       double a_val,
                       int a_isens);
  bool   SetPPValsIsens(int a_scatIdx,
                        const std::vector<double> a_vals,
                        const std::vector<int> a_isens);
  double UnusedParamKey();
  double MinParamKey();
  bool   ParamOfTypeExists(const char *a_type);

  static bool IsPilotParName(const char *PNAM,
                             const char *PTYP,
                             int *a_scatIndex=NULL);

  ParPub* m_public;
private:
  ParamList(const ParamList &rhs);
  const ParamList& operator=(const ParamList &rhs);

  class impl;
  impl *m_p;
};

#endif
