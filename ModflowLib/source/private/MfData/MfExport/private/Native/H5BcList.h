//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5BCLIST_H
#define H5BCLIST_H
#include <private\util\util.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class H5BcList
    {
    public:
      H5BcList(NativePackExp* a_);
      ~H5BcList();

      void WriteList (const int a_sp,
                      const int a_start,
                      CStr& a_type,
                      CStr& a_f,
                      std::vector<int>& a_cellids,
                      CAR_DBL2D& a_bcData,
                      std::vector<int>& a_vIface);
      void WriteSingleH5IntValue (const char *filePath,
                                  const char *h5Path,
                                  int value);
      void WriteSingleH5DoubleValue (const char *filePath,
                                     const char *h5Path,
                                     double value);
      void Write1DIntArray (const char *a_filePath,
                            const char *a_h5Path,
                            const int *a_array,
                            size_t a_length,
                            size_t a_start = 0);
      void Write1DH5Value (const char *filePath,
                           const char *h5Path,
                           hsize_t position,
                           int value);
      void WriteH5StringArray (const char *a_filePath,
                               const char *a_h5Path,
                               const std::vector<CStr>& a_array);

      void WriteMapIdsForListBcs ();
      CStr LstPack (int& a_maxBc);
      void LstPar ();
      CStr Str (int& a_itmp);
      CStr SfrLn2 ();
      CStr SfrLn6 (int& a_itmp);
      CStr Mnw2 ();
      CStr Mnw1 ();
      CStr ClnWel (int& a_itmp);

    private:
      NativePackExp* m_pack;
    };
  } // namespace Export
} // namespace MfData
#endif
