//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAM_H
#define PARAM_H

#include <private\util\util.h>

class PClust
{
public:
  PClust(CStr a_mlt="",
         CStr a_zon="",
         int  a_lay=1) :
    m_mlt(a_mlt),
    m_zon(a_zon),
    m_hgu(),
    m_lay(a_lay),
    m_iz()
    {}
  PClust(const PClust &rhs) :
    m_mlt(rhs.m_mlt),
    m_zon(rhs.m_zon),
    m_hgu(rhs.m_hgu),
    m_lay(rhs.m_lay),
    m_iz(rhs.m_iz)
    {}

  const PClust &operator=(const PClust &rhs)
  {
    if (this != &rhs)
    {
      m_mlt = rhs.m_mlt;
      m_zon = rhs.m_zon;
      m_hgu = rhs.m_hgu;
      m_lay = rhs.m_lay;
      m_iz  = rhs.m_iz;
    }
    return(*this);
  }
  
  CStr m_mlt, m_zon, m_hgu;
  int  m_lay;
  std::vector<int> m_iz;
};

class Param
{
public:
  Param(CStr a_name="",
        double a_key=0.0,
        CStr a_type="",
        double a_value=0.0,
        double a_min=0.0,
        double a_max=0.0,
        double a_logMinVal=0.0,
        bool a_pilotPoints=false,
        int a_scatIndex=0,
        bool a_logInterp=false,
        bool a_multArray=false,
        bool a_logTrans=false,
        double a_bscal=1.0,
        int a_isens=0) :
    m_name(a_name),
    m_key(a_key),
    m_type(a_type),
    m_value(a_value),
    m_start(a_value),
    m_b(0),
    m_min(a_min),
    m_max(a_max),
    m_logMinVal(a_logMinVal),
    m_pilotPoints(a_pilotPoints),
    m_scatIndex(a_scatIndex),
    m_logInterp(a_logInterp),
    m_multArray(a_multArray),
    m_logTrans(a_logTrans),
    m_bscal(a_bscal),
    m_isens(a_isens),
    m_clust(),
    m_instNames(),
    m_instStress(),
    m_tied(),
    m_clustInParamFile(0),
    m_str_start(-1),
    m_str_nbc(-1) {}

  Param(const Param &rhs) :
    m_name(rhs.m_name),
    m_key(rhs.m_key),
    m_type(rhs.m_type),
    m_value(rhs.m_value),
    m_start(rhs.m_start),
    m_b(rhs.m_b),
    m_min(rhs.m_min),
    m_max(rhs.m_max),
    m_logMinVal(rhs.m_logMinVal),
    m_pilotPoints(rhs.m_pilotPoints),
    m_scatIndex(rhs.m_scatIndex),
    m_logInterp(rhs.m_logInterp),
    m_multArray(rhs.m_multArray),
    m_logTrans(rhs.m_logTrans),
    m_bscal(rhs.m_bscal),
    m_isens(rhs.m_isens),
    m_clust(rhs.m_clust),
    m_instNames(rhs.m_instNames),
    m_instStress(rhs.m_instStress),
    m_tied(rhs.m_tied),
    m_clustInParamFile(rhs.m_clustInParamFile),
    m_str_start(rhs.m_str_start),
    m_str_nbc(rhs.m_str_nbc) {}

  const Param& operator=(const Param &rhs)
  {
    if (this != &rhs)
    {
      m_name = rhs.m_name;
      m_key = rhs.m_key;
      m_type = rhs.m_type;
      m_value = rhs.m_value;
      m_start = rhs.m_start;
      m_b = rhs.m_b;
      m_min = rhs.m_min;
      m_max = rhs.m_max;
      m_logMinVal = rhs.m_logMinVal;
      m_pilotPoints = rhs.m_pilotPoints;
      m_scatIndex = rhs.m_scatIndex;
      m_logInterp = rhs.m_logInterp;
      m_multArray = rhs.m_multArray;
      m_logTrans = rhs.m_logTrans;
      m_bscal = rhs.m_bscal;
      m_isens = rhs.m_isens;
      m_clust = rhs.m_clust;
      m_instNames = rhs.m_instNames;
      m_instStress = rhs.m_instStress;
      m_tied = rhs.m_tied;
      m_clustInParamFile = rhs.m_clustInParamFile;
      m_str_start = rhs.m_str_start;
      m_str_nbc = rhs.m_str_nbc;
    }
    return(*this);
  }

  CStr    m_name, m_type;
  double  m_key, m_value, m_start, m_b, m_min, m_max, m_logMinVal, m_bscal;
  int     m_scatIndex, m_isens;
  bool    m_pilotPoints, m_logInterp, m_multArray, m_logTrans,
          m_clustInParamFile;
  std::vector<PClust> m_clust;
  std::vector<CStr> m_instNames;
  std::map<CStr, std::vector<int> > m_instStress;
  CStr    m_tied;
  int     m_str_start, m_str_nbc;
};

#endif

