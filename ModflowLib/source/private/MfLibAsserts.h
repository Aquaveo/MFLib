#pragma once

#include <vector>

//template <typename T>
//inline void TS_ASSERT_EQUALS2(const T &a, const T &b)
//{
//  if (!(a == b)) {
//    stringstream  s;
//    s << "Expected: \"" << a << "\" Found: \"" << b << "\"";
//    TS_FAIL(s.str().c_str());
//  }
//}

template<class _T, class _U, class _V>
bool  TS_EQ_TOL(_T A, _U B, _V tolerance) {return (fabs((A) - (B)) <= (tolerance));}
  // RDJ - This was changed to a MACRO so that the failure printout would point
  // to where this was called from instead of this file.
#define _TS_ASSERT_EQUALS2(f, l, a, b) {                                       \
  if (!(a == b)) {                                                             \
    std::stringstream  sTestString;                                            \
    sTestString << "Expected: \"" << a << "\" Found: \"" << b << "\"";         \
    _TS_FAIL(f, l, sTestString.str().c_str());                                 \
  }                                                                            \
}
#define TS_ASSERT_EQUALS2(expected, found) _TS_ASSERT_EQUALS2(__FILE__, __LINE__, expected, found)

#define _TS_ASSERT_STRINGS(file,line,expect,found) {    \
  if (0 != expect.compare(found)) {                     \
    _TS_FAIL(file, line,(expect+"=expected").c_str());  \
    _TS_FAIL(file,line,(found +"=found").c_str());      \
  }                                                     \
}
#define TS_ASSERT_STRINGS(expect,found) _TS_ASSERT_STRINGS(__FILE__,__LINE__,expect,found)

  // This is used to compare to vectors and give useful output information
#define _TS_ASSERT_EQUALS_VEC(f, l, a, b) {                                    \
  if (a.size() != b.size()) {                                                  \
    std::stringstream msg;                                                     \
    msg << "Incorrect size Expecting size: " << a.size()                       \
        << " Found size: " << b.size();                                        \
    _TS_FAIL(f, l, msg.str().c_str());                                         \
  }                                                                            \
  else {                                                                       \
    for (size_t i = 0; i < a.size(); ++i) {                                    \
      if (a.at(i) != b.at(i)) {                                                \
        std::stringstream msg;                                                 \
        msg << "Incorrect value at position : " << i                           \
            << " Expecting: " << a.at(i)                                       \
            << " Found: " << b.at(i);                                          \
        _TS_FAIL(f, l, msg.str().c_str());                                     \
      }                                                                        \
    }                                                                          \
  }                                                                            \
}
#define TS_ASSERT_EQUALS_VEC(a, b)                                             \
  _TS_ASSERT_EQUALS_VEC(__FILE__, __LINE__, a, b)

  // Used to compare array and a vector and give useful output info
#define _TS_ASSERT_EQUALS_AVEC(f, l, expected, length, actual)                 \
  MfLibAsserts::AssertEqualsAVec(f, l, expected, length, actual)
#define TS_ASSERT_EQUALS_AVEC(expected, length, actual)                        \
  _TS_ASSERT_EQUALS_AVEC(__FILE__, __LINE__, expected, length, actual)

#define _TS_ASSERT_DELTA_VEC(f, l, a, b, delta)                                \
  MfLibAsserts::AssertDeltaVec( (f), (l), (a), (b), (delta) )

#define TS_ASSERT_DELTA_VEC(a, b, delta)                                       \
  _TS_ASSERT_DELTA_VEC(__FILE__, __LINE__, a, b, delta)

#define _TS_ASSERT_DELTA_VEC2D(f, l, a, b, delta)                              \
  MfLibAsserts::AssertDeltaVec2D( (f), (l), (a), (b), (delta) )

#define TS_ASSERT_DELTA_VEC2D(a, b, delta)                                     \
  _TS_ASSERT_DELTA_VEC2D(__FILE__, __LINE__, a, b, delta)

#define _TS_ASSERT_EQUALS_VEC2D(f, l, a, b, delta)                             \
  MfLibAsserts::AssertEqualsVec2D( (f), (l), (a), (b))

#define TS_ASSERT_EQUALS_VEC2D(a, b, delta)                                    \
  _TS_ASSERT_EQUALS_VEC2D(__FILE__, __LINE__, a, b)

  // This is used to compare to vectors and give useful output information
#define _TS_ASSERT_DELTA_VEC_MP3(f, l, a, b, delta) {                          \
  if (a.size() != b.size()) {                                                  \
    std::stringstream msg;                                                     \
    msg << "Incorrect size Expecting size: " << a.size()                       \
        << " Found size: " << b.size();                                        \
    _TS_FAIL(f, l, msg.str().c_str());                                         \
  }                                                                            \
  else {                                                                       \
    for (size_t i = 0; i < a.size(); ++i) {                                    \
      if (!gmEqualPointsXYZ(a.at(i), b.at(i), delta)) {                 \
        std::stringstream msg;                                                 \
        msg << "Incorrect value at position : " << i                           \
            << " Expecting: " << a.at(i)                                       \
            << " Found: " << b.at(i);                                          \
        _TS_FAIL(f, l, msg.str().c_str());                                     \
        return;                                                                \
      }                                                                        \
    }                                                                          \
  }                                                                            \
}
#define TS_ASSERT_DELTA_VEC_MP3(a, b, delta)                                   \
  _TS_ASSERT_DELTA_VEC_MP3(__FILE__, __LINE__, a, b, delta)

///-----------------------------------------------------------------------------
/// \brief Used to compare two Pt3ds and give useful output information.
///
///  You'll need to include the following wherever you use this:
///  #include <shared/geometry/geoms.h>     // for gmEqualPointsXYZ
///  #include <shared/guido/mpts_streams.h> // for << on Pt3d
//------------------------------------------------------------------------------
#define _TS_ASSERT_DELTA_PT3D(f, l, a, b, delta) {                             \
  if (!gmEqualPointsXYZ(a, b, delta))                                   \
  {                                                                            \
    std::stringstream msg;                                                     \
    msg << "(" << a << "), != (" << b << ") within delta (" << delta << ")";   \
    _TS_FAIL(f, l, msg.str().c_str());                                         \
  }                                                                            \
}
//----- OVERLOAD (kind of) -----------------------------------------------------
#define _TS_ASSERT_DELTA_PT2D(f, l, a, b, delta) {                             \
  if (!gmEqualPointsXY(a, b, delta))                                    \
  {                                                                            \
    std::stringstream msg;                                                     \
    msg << "(" << a << "), != (" << b << ") within delta (" << delta << ")";   \
    _TS_FAIL(f, l, msg.str().c_str());                                         \
  }                                                                            \
}
#define TS_ASSERT_DELTA_PT3D(a, b, delta)                                      \
  _TS_ASSERT_DELTA_PT3D(__FILE__, __LINE__, a, b, delta)
//----- OVERLOAD (kind of) -----------------------------------------------------
#define TS_ASSERT_DELTA_PT2D(a, b, delta)                                      \
  _TS_ASSERT_DELTA_PT2D(__FILE__, __LINE__, a, b, delta)

#define _TS_ASSERT_TXT_FILES_EQUAL(f,l,a,b)                                    \
  MfLibAsserts::TextFilesEqual( (f), (l), a, b )
#define TS_ASSERT_TXT_FILES_EQUAL(a,b)                                         \
  _TS_ASSERT_TXT_FILES_EQUAL(__FILE__,__LINE__,a,b)

#define _TS_ASSERT_STREAMS_EQUAL(f,l,a,b)                                      \
  MfLibAsserts::StreamsEqual((f), (l), a, b)
#define TS_ASSERT_STREAMS_EQUAL(a,b)                                           \
  _TS_ASSERT_STREAMS_EQUAL(__FILE__,__LINE__,a,b)

//----- Template functions -----------------------------------------------------

namespace MfLibAsserts
{
  void TextFilesEqual(const std::string& a_srcFile,
                      unsigned line,
                      const std::string& a_file1,
                      const std::string& a_file2);

  bool TextFilesEqual(const std::string& a_file1, const std::string& a_file2,
                      std::string& a_message);

  void StreamsEqual (const std::string& a_src, unsigned int a_line,
         std::istream& a_strm1, std::istream& a_strm2);
  //----------------------------------------------------------------------------
  /// \brief Template function returning true or false to help debug tests.
  //----------------------------------------------------------------------------
  template<typename T,typename U>
  bool AssertDeltaVec (const char *f, unsigned l,
                              const std::vector<T>& a, const std::vector<T>& b,
                              U delta)
  {
    bool ok = true;
    if (a.size() != b.size())
    {
      std::stringstream msg;
      msg << "Incorrect size (" << a.size() << " != " << b.size() << ")";
      _TS_FAIL(f, l, msg.str().c_str());
      ok = false;
    }
    else
    {
      for (size_t i = 0; i < a.size(); ++i)
      {
        if (!TS_EQ_TOL(a.at(i), b.at(i), delta))
        {
          std::stringstream msg;
          msg << "Incorrect value at position (" << i << "). "
              << "(" << a.at(i) << " != " << b.at(i) << ")"
              << " within delta (" << delta << ")";
          _TS_FAIL(f, l, msg.str().c_str());
          ok = false;
        }
      }
    }
    return ok;
  } // AssertDeltaVec
  //----------------------------------------------------------------------------
  /// \brief Template function returning true or false to help debug tests.
  //----------------------------------------------------------------------------
  template<typename T,typename U>
  bool AssertDeltaVec2D (const char *f, unsigned l,
         const std::vector<std::vector<T> >& a,
         const std::vector<std::vector<T> >& b,
                              U delta)
  {
    bool ok = true;
    if (a.size() != b.size())
    {
      std::stringstream msg;
      msg << "Incorrect size (" << a.size() << " != " << b.size() << ")";
      _TS_FAIL(f, l, msg.str().c_str());
      ok = false;
    }
    else
    {
      for (size_t i = 0; i < a.size(); ++i)
      {
        if (a.at(i).size() != b.at(i).size())
        {
          std::stringstream msg;
          msg << "Incorrect size (" << a.at(i).size() << " != " << b.at(i).size() << ")";
          _TS_FAIL(f, l, msg.str().c_str());
          ok = false;
        }
        else
        {
          for (size_t j = 0; j < a.at(i).size(); ++j)
          {
            if (!TS_EQ_TOL(a.at(i).at(j), b.at(i).at(j), delta))
            {
              std::stringstream msg;
              msg << "Incorrect value at position (" << i << ")(" << j <<"). "
                  << "(" << a.at(i).at(j) << " != " << b.at(i).at(j) << ")"
                  << " within delta (" << delta << ")";
              _TS_FAIL(f, l, msg.str().c_str());
              ok = false;
            }
          }
        }
      }
    }
    return ok;
  } // AssertDeltaVec2D
  //----------------------------------------------------------------------------
  /// \brief Function for TS_ASSERT_EQUALS_AVEC
  //----------------------------------------------------------------------------
  template <class T>
  void AssertEqualsAVec (const char *a_file,
                         int a_line,
                         const T* a_expected,
                         size_t a_expectedLength,
                         const std::vector<T>& a_actual)
  {
    std::vector<T> expected(a_expected, a_expected+a_expectedLength);
    _TS_ASSERT_EQUALS_VEC(a_file, a_line, expected, a_actual);
  }
  //----------------------------------------------------------------------------
  /// \brief Template function returning true or false to help debug tests.
  //----------------------------------------------------------------------------
  template<typename T>
  bool AssertEqualsVec2D (const char *f, unsigned l,
         const std::vector<std::vector<T> >& a,
         const std::vector<std::vector<T> >& b)
  {
    bool ok = true;
    if (a.size() != b.size())
    {
      std::stringstream msg;
      msg << "Incorrect size (" << a.size() << " != " << b.size() << ")";
      _TS_FAIL(f, l, msg.str().c_str());
      ok = false;
    }
    else
    {
      for (size_t i = 0; i < a.size(); ++i)
      {
        if (a.at(i).size() != b.at(i).size())
        {
          std::stringstream msg;
          msg << "Incorrect size (" << a.at(i).size() << " != " << b.at(i).size() << ")";
          _TS_FAIL(f, l, msg.str().c_str());
          ok = false;
        }
        else
        {
          for (size_t j = 0; j < a.at(i).size(); ++j)
          {
            if (a.at(i).at(j) != b.at(i).at(j))
            {
              std::stringstream msg;
              msg << "Incorrect value at position (" << i << ")(" << j <<"). "
                  << "(" << a.at(i).at(j) << " != " << b.at(i).at(j) << ")";
              _TS_FAIL(f, l, msg.str().c_str());
              ok = false;
            }
          }
        }
      }
    }
    return ok;
  } // AssertEqualsVec2D

} // namespace MfLibAsserts
