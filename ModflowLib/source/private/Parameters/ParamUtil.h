#ifndef PARAMUTIL_H
#define PARAMUTIL_H

#include <private/util/util.h>

namespace Param
{
  void SetFileName(const CStr &a_fName);
  void SubstituteArray(float *a_, size_t a_size);
  void SubstituteArray(int *a_, size_t a_size);
  void SubstituteValue(double *a_);
}
#endif

