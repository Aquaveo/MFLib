//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5LSTPACK_H
#define H5LSTPACK_H
#include <private\util\util.h>

class H5StrPack;
class H5SfrPack;
namespace MfData
{
  class MfGlobal;
  class MfPackage;
  namespace Export
  {
    class Mf2kNative;
    class H5BcList;
    class H5LstPack
    {
    friend H5StrPack;
    friend H5SfrPack;
    public:
      H5LstPack (
          MfData::MfGlobal* a_g
        , MfData::MfPackage* a_p
        , Mf2kNative* a_n
        , H5BcList* a_lbc);
      ~H5LstPack();

      CStr Write (int& a_maxBc);
      CStr StrPack (int& a_itmp);
      CStr SfrLn2 ();
      CStr SfrLn6 (int& a_itmp);
      CStr ClnWel (int& a_itmp);
      void LstPar ();
      void WriteMapIds ();

    private:
      class impl;
      impl *m_p;
    };
  } // namespace Export
} // namespace MfData
#endif
