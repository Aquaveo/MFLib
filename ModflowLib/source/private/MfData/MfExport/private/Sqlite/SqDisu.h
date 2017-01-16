//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SqDisu_H
#define SqDisu_H
#include <private\util\util.h>

namespace MfData {

//----- Forward declarations----------------------------------------------------
class MfGlobal;
class MfPackage;

namespace Export {

//----- Forward declarations----------------------------------------------------
class NativeExpDisu;

////////////////////////////////////////////////////////////////////////////////
class SqDisu
{
public:
  SqDisu(NativeExpDisu* a_);
  ~SqDisu();

  void Export();

private:
  class impl;
  impl *m_p;
}; // class SqDisu

} // namespace Export
} // namespace MfData
#endif
