//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PILOTPOINTS_H
#define PILOTPOINTS_H

#include <private\util\util.h>

class PilotPointsT;
class Param;
namespace MfData { class MfPackage; }

class PilotPoints
{
  friend PilotPointsT;
public:
  PilotPoints(const char *a_fName,
              const Param &a_param);

  // Don't need to change this float to Real
  void SetPPStartVals(const std::vector<double> &a_vals);
  void GetPPStartVals(std::vector<double> &a_vals) const;
  void GetPPStartValsReadFromH5IfNeeded(std::vector<double> &a_vals);
  bool DoInterpolation(std::vector<Real> &a_arrayVals);
  void GetWeightsForPoint(int a_idx, std::vector<Real>& a_w);

  void SetUnstructured(std::vector<int>& a_nodes);
  void SetLayer(int a_);

private:
  PilotPoints(const PilotPoints &rhs);
  const PilotPoints& operator=(const PilotPoints &rhs);

  class impl;
  impl *m_p;
};

#endif

