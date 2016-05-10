//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQBCLIST_H
#define SQBCLIST_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativeExpLstPack;
    class SqBcList
    {
    public:
      SqBcList(NativeExpLstPack* a_);
      ~SqBcList();

      void AddVariable (const char* a_var,
                        const char* a_val);
      void AddItmp (int a_sp, int a_itmp);
      void AddStressPeriodData ();
      void EndWriteFile();
      void AddSqComment();

      void WriteMapIdsForListBcs ();
      void LstPar ();
      CStr Str (int& a_itmp);
      CStr SfrLn2 ();
      CStr SfrLn6 (int& a_itmp);
      CStr Mnw2 ();
      CStr Mnw1 ();
      CStr ClnWel (int& a_itmp);

    private:
      class impl;
      impl *m_p;
      NativeExpLstPack* m_pack;
    };
  } // namespace Export
} // namespace MfData
#endif
