//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef OBSHD_H
#define OBSHD_H
#include <private/util/util.h>

class ObLay
{
public:
  ObLay() : m_lay(0), m_factor(0) {}
  int   m_lay;
  Real m_factor;
};
class ObTime
{
public:
  ObTime() : m_tOff(0), m_hob(0), m_statH(0), m_statdd(0), m_iRefSp(0),
             m_statFlag(0), m_plot(0) {}
  CStr  m_name;
  Real m_tOff, m_hob, m_statH, m_statdd;
  int   m_iRefSp, m_statFlag, m_plot;
};
class HdObs
{
public:
  HdObs() : m_row(0), m_col(0), m_ITT(0), m_rOff(0), m_cOff(0) {}
  CStr                m_name;
  int                 m_row, m_col, m_ITT;
  Real               m_rOff, m_cOff;
  std::vector<ObLay>  m_vLay;
  std::vector<ObTime> m_vTimes;
private:
};

class Flob
{
public:
  Flob() : m_factorId(0), m_IREFSP(0), m_TOFFSET(0), m_HOB(0), m_STAT(0),
           m_STATFLG(0), m_PLOT(0) {}
  CStr        m_type;
  CStr        m_name;
  int         m_factorId;
  int         m_IREFSP;
  Real       m_TOFFSET;
  Real       m_HOB;
  Real       m_STAT;
  int         m_STATFLG;
  int         m_PLOT;
};
class FlobFact
{
public:
  FlobFact() : m_factorId(0), m_i(0), m_j(0), m_k(0), m_factor(0) {}
  int         m_factorId;
  int         m_i, m_j, m_k;
  Real       m_factor;
};

class FlowObs
{
public:
  FlowObs() : m_currFactorId(1) {}

  int                    m_currFactorId;
  std::vector<Flob>      m_flob;
  std::vector<FlobFact>  m_fact;
};

std::vector<HdObs>& GetHOB ();
FlowObs& GetFLOB();

#endif
