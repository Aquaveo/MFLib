//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CONTINUOUSARRAYSdotH
#define CONTINUOUSARRAYSdotH

#define NONE -1
//#define TRUE 1
//#define FALSE 0

typedef unsigned char boolean;

template <class T> class EContinuousArray3D;

// template class (eventually) to create and use 2D arrays that are continuous
// and take only one allocation.  These arrays can be accessed using brackets
// like [Index1][Index2]

template <class T>
class EContinuousArray2D {
friend  class EContinuousArray3D<T>;
public:
  EContinuousArray2D();
  virtual ~EContinuousArray2D();

  EContinuousArray2D(const EContinuousArray2D &a_ArrayB);

  const EContinuousArray2D & operator = (const EContinuousArray2D &a_rhs);
  operator T *() const; // Get pointer to 1D array (whole array)
  T * operator [] (int nIndex) const; // use bracket to access array
  T &at(int a_i, int a_j);

  // Array is reinitialized with value
  void    SetSizeNoInit(int a_Size1, int a_Size2); 
  void    SetSize(int a_Size1, int a_Size2, T a_Value); 

  int     GetSize1() const { return m_Size[0]; };
  int     GetSize2() const { return m_Size[1]; };

  void    Clear(); 

  void    SetAll(T a_Value); // Set all array values to a specific value

  void GetPtr(T** a_) { if (a_) *a_=m_Array; }
private:
  // This function is used by the ContinuousArray3D class to set the address
  // to a 2D array without reallocating the memory.  This is used so that the
  // ContinuousArray3D class can return a 2D array from the bracket operator
  // without having to make a copy of the data or anything
  void  Set2DArrayNoDelete(int a_Size1, int a_Size2, T *a_Array);

  T   *m_Array;
  boolean   m_bNoDelete;
  int       m_Size[2];
};

template<class T>
class EContinuousArray3D {
public:
  EContinuousArray3D();
  virtual ~EContinuousArray3D();

  EContinuousArray3D(const EContinuousArray3D &a_ArrayB);

  const EContinuousArray3D & operator = (const EContinuousArray3D &a_rhs);
  operator T *() const; // Get pointer to 1D array (whole array)
  // use bracket to access array
  const EContinuousArray2D<T> &operator [] (int nIndex);

  T &at(int a_i, int a_j, int a_k);

  // Array is reinitialized with value
  void    SetSize(int a_Size1, int a_Size2, int a_Size3, T a_Value); 

  int     GetSize1() const { return m_Size[0]; };
  int     GetSize2() const { return m_Size[1]; };
  int     GetSize3() const { return m_Size[2]; };

  void    Clear(); 
private:
  T        *m_Array;
  int       m_Size[3];
  EContinuousArray2D<T>  m_Array2D; // This is returned with [] operator
};

// The functions are in this file because it will be a template function

template <class T>
EContinuousArray2D<T>::EContinuousArray2D()
{
  m_Array = NULL;
  m_bNoDelete = FALSE;
  m_Size[0] = m_Size[1] = NONE;
}

template<class T>
EContinuousArray2D<T>::~EContinuousArray2D()
{
  if (m_Array && m_bNoDelete == FALSE) {
    delete [] m_Array;
  }
}

template<class T>
EContinuousArray2D<T>::EContinuousArray2D(const EContinuousArray2D &a_ArrayB)
{
  *this = a_ArrayB;
}

template<class T>
const EContinuousArray2D<T> & EContinuousArray2D<T>::operator = (
                                             const EContinuousArray2D<T> &a_rhs)
{
  int Totalsize, i;

  m_Size[0] = a_rhs.GetSize1();
  m_Size[1] = a_rhs.GetSize2();

  // delete any old memory
  if (m_bNoDelete == FALSE) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_bNoDelete = FALSE;

  if (m_Size[0] > 0 && m_Size[1] > 0) {
    Totalsize = m_Size[0] * m_Size[1];
  }
  else {
    Totalsize = 0;
  }

  if (Totalsize > 0) {
    m_Array = new T [Totalsize];
    for (i = 0; i < Totalsize; i++) {
      m_Array[i] = a_rhs.m_Array[i];
    }
  }
  return *this;
}


template<class T>
EContinuousArray2D<T>::operator T *() const
{
  return (m_Array);
}

template<class T>
T * EContinuousArray2D<T>::operator [] (int nIndex) const
{
  return (&m_Array[m_Size[1]*nIndex]);
}

template<class T>
void EContinuousArray2D<T>::SetSizeNoInit(int a_Size1, int a_Size2)
{
  int  Totalsize;

  if (m_Array != NULL && m_bNoDelete == FALSE) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_Size[0] = a_Size1;
  m_Size[1] = a_Size2;
  Totalsize = m_Size[0] * m_Size[1];
  if (Totalsize > 0) {
    m_Array = new T[Totalsize];
  }
}

template<class T>
void EContinuousArray2D<T>::SetSize(int a_Size1, int a_Size2, T a_Value)
{
  int  i, Totalsize;

  if (m_Array != NULL && m_bNoDelete == FALSE) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_Size[0] = a_Size1;
  m_Size[1] = a_Size2;
  Totalsize = m_Size[0] * m_Size[1];
  if (Totalsize > 0) {
    m_Array = new T[Totalsize];
    for (i = 0; i < Totalsize; i++) {
      m_Array[i] = a_Value;
    }
  }
}

template<class T>
void EContinuousArray2D<T>::Clear()
{
  if (m_Array) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_Size[0] = 0;
  m_Size[1] = 0;
}

template<class T>
void EContinuousArray2D<T>::Set2DArrayNoDelete(int a_Size1, int a_Size2, 
                                            T *a_Array)
{
  m_Size[0] = a_Size1;
  m_Size[1] = a_Size2;
  m_Array = a_Array;
  m_bNoDelete = TRUE;
}

template<class T>
T &EContinuousArray2D<T>::at(int a_i, int a_j)
{
  // first check the bounds and throw an exception if necessary
  if (a_i < 0 || a_i >= m_Size[0] ||
      a_j < 0 || a_j >= m_Size[1]) {
    std::string msg("Index is out of range.");
    throw std::out_of_range(msg);
  }

  //get the data
  return (m_Array[(m_Size[1]*a_i) + a_j]);
}

template<class T>
void EContinuousArray2D<T>::SetAll(T a_Value)
{
  int TotalSize = m_Size[0] * m_Size[1];
  for (int i = 0; i < TotalSize; i++) {
    m_Array[i] = a_Value;
  }
}

template<class T>
EContinuousArray3D<T>::EContinuousArray3D()
{
  m_Array = NULL;
  m_Size[0] = m_Size[1] = m_Size[2] = NONE;
}

template<class T>
EContinuousArray3D<T>::~EContinuousArray3D()
{
  if (m_Array) {
    delete [] m_Array;
  }
}

template<class T>
EContinuousArray3D<T>::EContinuousArray3D(const EContinuousArray3D &a_ArrayB)
{
  *this = a_ArrayB;
}

template<class T>
const EContinuousArray3D<T> & EContinuousArray3D<T>::operator = (
                                               const EContinuousArray3D &a_rhs)
{
  int Totalsize, i;

  m_Size[0] = a_rhs.GetSize1();
  m_Size[1] = a_rhs.GetSize2();
  m_Size[2] = a_rhs.GetSize3();

  // delete any old memory
  delete [] m_Array;
  m_Array = NULL;

  if (m_Size[0] > 0 && m_Size[1] > 0 && m_Size[2]) {
    Totalsize = m_Size[0] * m_Size[1] * m_Size[2];
  }
  else {
    Totalsize = 0;
  }

  if (Totalsize > 0) {
    m_Array = new T [Totalsize];
    for (i = 0; i < Totalsize; i++) {
      m_Array[i] = a_rhs.m_Array[i];
    }
  }
  return *this;
}

template<class T>
EContinuousArray3D<T>::operator T *() const
{
  return (m_Array);
}

template<class T>
const EContinuousArray2D<T> & EContinuousArray3D<T>::operator [] (int nIndex) 
{
  // We need to return a continuous 2D array
  m_Array2D.Set2DArrayNoDelete(m_Size[1], m_Size[2], 
                               &m_Array[m_Size[1]*m_Size[2]*nIndex]);
  return (m_Array2D);
}

template<class T>
T &EContinuousArray3D<T>::at(int a_i, int a_j, int a_k)
{
  // first check the bounds and throw an exception if necessary
  if (a_i < 0 || a_i >= m_Size[0] ||
      a_j < 0 || a_j >= m_Size[1] ||
      a_k < 0 || a_k >= m_Size[2]) {
    std::string msg("Index is out of range.");
    throw std::out_of_range(msg);
  }

  //get the data
  return (m_Array[(m_Size[2]*m_Size[1]*a_i)+ (m_Size[2]*a_j) + a_k]);
}

template<class T>
void EContinuousArray3D<T>::SetSize(int a_Size1, int a_Size2, int a_Size3,
                                 T a_Value)
{
  int  i, Totalsize;

  if (m_Array != NULL) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_Size[0] = a_Size1;
  m_Size[1] = a_Size2;
  m_Size[2] = a_Size3;

  if (m_Size[0] > 0 && m_Size[1] > 0 && m_Size[2] > 0) {
    Totalsize = m_Size[0] * m_Size[1] * m_Size[2];
  }
  else {
    Totalsize = 0;
  }

  if (Totalsize > 0) {
    m_Array = new T[Totalsize];
    for (i = 0; i < Totalsize; i++) {
      m_Array[i] = a_Value;
    }
  }
}

template<class T>
void EContinuousArray3D<T>::Clear()
{
  if (m_Array) {
    delete [] m_Array;
    m_Array = NULL;
  }
  m_Size[0] = 0;
  m_Size[1] = 0;
  m_Size[2] = 0;
}

typedef EContinuousArray2D<Real> CAR_REL2D;
typedef EContinuousArray3D<Real> CAR_REL3D;
typedef EContinuousArray2D<double> CAR_DBL2D;
typedef EContinuousArray3D<double> CAR_DBL3D;
typedef EContinuousArray2D<float> CAR_FLT2D;
typedef EContinuousArray3D<float> CAR_FLT3D;
typedef EContinuousArray2D<int>    CAR_INT2D;
typedef EContinuousArray2D<char>   CAR_CHR2D;

#endif
