//------------------------------------------------------------------------------
// FILE      PilotPoints.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\Parameters\PilotPoints.h>

#include <math.h>
#include <private\util\util.h>

#include <private\H5DataReader\H5DataSetReader.h>
#include <private\MfData\MfPackageUtil.h>
#include <private\Parameters\Param.h>

class PilotPointsT;

class PilotPoints::impl
{
  friend PilotPointsT;
public:
  impl(const char *a_,
       const Param &a_param);

  bool DoInterpolation(std::vector<Real> &a_arrayVals);

  void SetStartVals(const std::vector<double> &a_);
  void GetStartVals(std::vector<double> &a_) const { a_ = m_vals; }
  void GetWeightsForPoint(int a_idx, std::vector<Real>& a_w);
  bool ReadStartVals();

  bool ReadWeights();
  bool ReadIndices();

  void PostDataForExport();
  int  GetStartIdx();
  int  GetEndIdx(int a_start);

  template<class T>
  bool ReadData(const CStr &a_path,
                T &a_);

  Param m_param;
  CStr m_file;
  int  m_setIndex;
  std::vector<double> m_vals;
  std::vector<int> m_isens;
  CAR_REL2D m_weights;
  CAR_INT2D m_indices;
  int m_layer, m_mxnodlay;
  std::vector<int> m_nodes;
};

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
PilotPoints::impl::impl (const char *a_,
                         const Param &a_param) :
m_file(a_),
m_param(a_param),
m_layer(-1),
m_mxnodlay(-1)
{
  m_setIndex = m_param.m_scatIndex;
} // PilotPoints::PilotPoints::impl
//------------------------------------------------------------------------------
/// \brief Reads the array of weights associated with the pilot points.
//------------------------------------------------------------------------------
bool PilotPoints::impl::ReadWeights ()
{
  CStr path;
  path.Format("Pilot Points/Index%d/weights", m_setIndex);
  return (ReadData(path, m_weights));
} // PilotPoints::impl::ReadWeights
//------------------------------------------------------------------------------
/// \brief Reads the array of pilot point indices.
//------------------------------------------------------------------------------
bool PilotPoints::impl::ReadIndices ()
{
  CStr path;
  path.Format("Pilot Points/Index%d/point ids", m_setIndex);
  return (ReadData(path, m_indices));
} // PilotPoints::impl::ReadWeights
//------------------------------------------------------------------------------
/// \brief Reads the data in the path variable associated with the pilot points.
//------------------------------------------------------------------------------
template<class T>
bool PilotPoints::impl::ReadData (const CStr &a_path,
                                  T &a_)
{
  std::vector<hsize_t> dims;

  {
    H5DataSetReader r(m_file, a_path);
    if (!r.GetDataSetDimensions(dims))
      return false;
    if (dims.size() != 2)
      return false;
  }

  std::pair<int,int> p(0, (int)dims.at(0));
  VEC_INT_PAIR indices(2, p);
  indices.at(1).second = (int)dims.at(1);
  H5DataSetReader r(m_file, a_path, indices);
  size_t totSize;
  totSize = size_t(dims.at(0) * dims.at(1));
  a_.SetSizeNoInit((int)dims.at(0), (int)dims.at(1));

  r.AllowTypeConversions(true);
  if (!r.GetData(&a_.at(0, 0), totSize))
    return false;

  return true;
} // PilotPoints::impl::ReadData
//------------------------------------------------------------------------------
/// \brief Reads the starting values for the pilot points.
//------------------------------------------------------------------------------
bool PilotPoints::impl::ReadStartVals ()
{
  CStr path;
  path.Format("Pilot Points/Index%d/start vals", m_setIndex);
  H5DataSetReader r(m_file, path);
  r.AllowTypeConversions(true);
  return(r.GetAllData(m_vals));
} // PilotPoints::impl::ReadStartVals
//------------------------------------------------------------------------------
/// \brief Does the pilot point interpolation.
//------------------------------------------------------------------------------
bool PilotPoints::impl::DoInterpolation (std::vector<Real> &a_arrayVals)
{
  if (!ReadWeights() || !ReadIndices())
    return false;
  if (m_weights.GetSize1() != m_indices.GetSize1() ||
      m_weights.GetSize2() != m_indices.GetSize2())
    return false;
  if (m_vals.empty())
  {
    if (!ReadStartVals())
      return false;
    if (m_vals.empty())
      return false;
  }
  PostDataForExport();

  double val, val1, aVal;
  int *iPtr;
  Real *fPtr;
  bool retval(true);
  a_arrayVals.assign(m_weights.GetSize1(), 0);
  int cnt(0), i, j;

  m_indices.GetPtr(&iPtr);
  m_weights.GetPtr(&fPtr);
  try 
  {
    for (i=0; i<m_weights.GetSize1(); i++)
    {
      aVal = 0;
      for (j=0; j<m_weights.GetSize2(); j++)
      {
#ifdef _DEBUG
        //Real wt(m_weights.at(i,j));
        //int id(m_indices.at(i,j));
        //double d(m_vals.at(id-1));
#endif
        //val1 = m_vals[m_indices[i][j]-1];
        if (iPtr[cnt] < 1)
        {
          cnt++;
          continue;
        }

        val1 = m_vals[iPtr[cnt]-1];
        if (!m_param.m_logInterp)
          val = val1;
        else
        {
          if (val1 <= 0.0)
            val = log10(m_param.m_logMinVal);
          else
            val = log10(val1);
        }
        // add these up using a double so that we minimize round off error
        aVal += fPtr[cnt]*val;
        //a_arrayVals[i] += fPtr[cnt]*(Real)val;
        //a_arrayVals[i] += m_weights[i][j]*(Real)val;
        cnt++;
      }
      a_arrayVals[i] = static_cast<Real>(aVal);
      if (m_param.m_logInterp)
        a_arrayVals[i] = pow(10, a_arrayVals[i]);
    }
  }
  catch (std::out_of_range)
  {
    a_arrayVals.clear();
    retval = false;
  }
  // go through the array vals and trim round off error from going between
  // floats and doubles
  for (size_t k=0; k<a_arrayVals.size(); k++)
    a_arrayVals[k] = (Real)((float)a_arrayVals[k]);
  return retval;
} // PilotPoints::impl::DoInterpolation
//------------------------------------------------------------------------------
/// \brief Writes pilot point to multiplier arrays
//------------------------------------------------------------------------------
void PilotPoints::impl::GetWeightsForPoint (int a_idx, std::vector<Real>& a_w)
{
  for (size_t i=0; i<a_w.size(); ++i) a_w[i] = 0;
  if (1 > m_weights.GetSize1())
  {
    if (!ReadWeights() || !ReadIndices()) return;
  }
  if (m_weights.GetSize1() != m_indices.GetSize1() ||
      m_weights.GetSize2() != m_indices.GetSize2())
  {
    if (m_nodes.empty() && (int)a_w.size() != m_weights.GetSize1()) return;
  }

  int *iPtr, cnt(0), start(GetStartIdx()), end(GetEndIdx(start));
  Real *fPtr;
  m_indices.GetPtr(&iPtr);
  m_weights.GetPtr(&fPtr);
  cnt = start;
  for (int i=start; i<end; i++)
  {
    for(int j=0; j<m_weights.GetSize2(); ++j, ++cnt)
    {
      if (a_idx == iPtr[cnt]) a_w[i-start] = fPtr[cnt];
    }
  }
} // PilotPoints::impl::WriteMultiplierArrays
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
void PilotPoints::impl::SetStartVals (const std::vector<double> &a_)
{
  m_vals = a_;
} // PilotPoints::impl::SetStartVals
//------------------------------------------------------------------------------
/// \brief send pp data to the exporter
//------------------------------------------------------------------------------
void PilotPoints::impl::PostDataForExport ()
{
  MfData::Packages::PilotPointData(m_setIndex,
                                   static_cast<int>(m_vals.size()),
                                   m_weights.GetSize2(),
                                   &m_weights.at(0,0),
                                   &m_indices.at(0,0));
} // PilotPoints::impl::PostDataForExport
//------------------------------------------------------------------------------
/// \brief Get the starting index. This is usually 0 but except when usg is
/// using an unstructured grid.
//------------------------------------------------------------------------------
int PilotPoints::impl::GetStartIdx ()
{
  int rval(0);
  if (m_nodes.empty() || m_layer < 2) return rval;
  if (m_layer > 0 && m_layer <= (int)m_nodes.size())
  {
    rval = m_weights.GetSize2() * m_nodes[m_layer-1];
  }
  return rval;
} // PilotPoints::impl::GetStartIdx
//------------------------------------------------------------------------------
/// \brief This is usually the size of the m_weights variable unless we are
/// doing usg with an unstructured grid
//------------------------------------------------------------------------------
int PilotPoints::impl::GetEndIdx (int a_start)
{
  int rval(m_weights.GetSize1());
  if (m_nodes.empty()) return rval;
  rval = a_start + m_mxnodlay;
  return rval;
} // PilotPoints::impl::GetEndIdx

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
PilotPoints::PilotPoints (const char *a_fName,
                          const Param &a_param) :
m_p(new PilotPoints::impl(a_fName, a_param))
{
} // PilotPoints::PilotPoints
//------------------------------------------------------------------------------
/// \brief Sets the starting values for the pilot points
//------------------------------------------------------------------------------
void PilotPoints::SetPPStartVals (const std::vector<double> &a_vals)
{
  m_p->SetStartVals(a_vals);
} // PilotPoints::SetPPStartVals
//------------------------------------------------------------------------------
/// \brief Sets the starting values for the pilot points
//------------------------------------------------------------------------------
void PilotPoints::GetPPStartVals (std::vector<double> &a_vals) const
{
  m_p->GetStartVals(a_vals);
} // PilotPoints::GetPPStartVals
//------------------------------------------------------------------------------
/// \brief Sets the starting values for the pilot points
//------------------------------------------------------------------------------
void PilotPoints::GetPPStartValsReadFromH5IfNeeded (std::vector<double> &a_vals)
{
  m_p->GetStartVals(a_vals);
  if (a_vals.empty()) m_p->ReadStartVals();
  m_p->GetStartVals(a_vals);
} // PilotPoints::GetPPStartValsReadFromH5IfNeeded
//------------------------------------------------------------------------------
/// \brief This function recieves the current values of the pilot points
/// and uses the information in the h5 file to do the interpolation to the
/// grid array. The second argument to this function is the array that gets
/// filled up.
//------------------------------------------------------------------------------
bool PilotPoints::DoInterpolation (std::vector<Real> &a_arrayVals)
{
  return(m_p->DoInterpolation(a_arrayVals));
} // PilotPoints::DoInterpolation
//------------------------------------------------------------------------------
/// \brief Writes pilot point to multiplier arrays
//------------------------------------------------------------------------------
void PilotPoints::GetWeightsForPoint (int a_idx, std::vector<Real>& a_w)
{
  m_p->GetWeightsForPoint(a_idx, a_w);
} // PilotPoints::WriteMultiplierArrays
//------------------------------------------------------------------------------
/// \brief Sets the layer if we are doing usg with an unstructured grid
//------------------------------------------------------------------------------
void PilotPoints::SetLayer (int a_)
{
  m_p->m_layer = a_;
} // PilotPoints::SetLayer
//------------------------------------------------------------------------------
/// \brief Sets the number of nodes in each layer if doing usg with an
/// unstructured grid
//------------------------------------------------------------------------------
void PilotPoints::SetUnstructured (std::vector<int>& a_nodes)
{
  m_p->m_nodes = a_nodes;
  for (size_t i=0; i<a_nodes.size(); ++i)
  {
    if (a_nodes[i] > m_p->m_mxnodlay) m_p->m_mxnodlay = a_nodes[i];
  }
} // PilotPoints::SetUnstructured
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Parameters\PilotPoints.t.h>

//------------------------------------------------------------------------------
void PilotPointsT::setUp ()
{
  //char c[5000];
  m_file = util::GetTestFilesDirectory() + "\\HDF5_InputFiles\\pest.h5";
  m_f2 = util::GetTestFilesDirectory() + "\\HDF5_InputFiles\\input.h5";
  m_par = new Param("p1", -1, "HK", 5, .001, 100, 1e-6, true, 1, false);
}
//------------------------------------------------------------------------------
void PilotPointsT::tearDown ()
{
  if (m_par) delete(m_par);
}
//------------------------------------------------------------------------------
void PilotPointsT::testCreateClass ()
{
  PilotPoints *p = new PilotPoints("myfile", *m_par);
  TS_ASSERT(p);
  if (p) delete(p);
}
//------------------------------------------------------------------------------
void PilotPointsT::testReadWeights ()
{
  // in this case the file doesn't exist
  {
    PilotPoints p("myfile", *m_par);
    TS_ASSERT(!p.m_p->ReadWeights());
  }
  // file exists but the pilot point data set doesn't
  {
    PilotPoints p(m_f2, *m_par);
    TS_ASSERT(!p.m_p->ReadWeights());
  }
  // file and data set exist but we will ask for the wrong index
  {
    m_par->m_scatIndex = 2;
    PilotPoints p(m_file, *m_par);
    TS_ASSERT(!p.m_p->ReadWeights());
  }
  // this one should read the data set
  {
    m_par->m_scatIndex = 1;
    PilotPoints p(m_file, *m_par);
    TS_ASSERT(p.m_p->ReadWeights());
    TS_ASSERT(p.m_p->m_weights.GetSize1() == 2940);
    TS_ASSERT(p.m_p->m_weights.GetSize2() == 15);
    TS_ASSERT_DELTA(p.m_p->m_weights.at(0,0), (Real)0.034509297, CXXDELTA);
    TS_ASSERT_DELTA(p.m_p->m_weights.at(23,5), (Real)0.00029361554, CXXDELTA);
    TS_ASSERT_DELTA(p.m_p->m_weights.at(2507,0), (Real)0.038629621, CXXDELTA);
    TS_ASSERT_DELTA(p.m_p->m_weights.at(2939,14), 0.0, CXXDELTA);
    TS_ASSERT_DELTA(p.m_p->m_weights.at(2939,13), (Real)0.325517920, CXXDELTA);
  }
}
//------------------------------------------------------------------------------
void PilotPointsT::testReadIndices ()
{
  // in this case the file doesn't exist
  {
    PilotPoints p("myfile", *m_par);
    TS_ASSERT(!p.m_p->ReadIndices());
  }
  // file exists but the pilot point data set doesn't
  {
    PilotPoints p(m_f2, *m_par);
    TS_ASSERT(!p.m_p->ReadIndices());
  }
  // file and data set exist but we will ask for the wrong index
  {
    m_par->m_scatIndex = 2;
    PilotPoints p(m_file, *m_par);
    TS_ASSERT(!p.m_p->ReadIndices());
  }
  // this one should read the data set
  {
    m_par->m_scatIndex = 1;
    PilotPoints p(m_file, *m_par);
    TS_ASSERT(p.m_p->ReadIndices());
    TS_ASSERT(p.m_p->m_indices.GetSize1() == 2940);
    TS_ASSERT(p.m_p->m_indices.GetSize2() == 15);
    TS_ASSERT_EQUALS(p.m_p->m_indices.at(0,0), 14);
    TS_ASSERT_EQUALS(p.m_p->m_indices.at(23,5), 10);
    TS_ASSERT_EQUALS(p.m_p->m_indices.at(2507,0), 10);
    TS_ASSERT_EQUALS(p.m_p->m_indices.at(2939,14), 1);
    TS_ASSERT_EQUALS(p.m_p->m_indices.at(2939,13), 10);
  }
}
//------------------------------------------------------------------------------
void PilotPointsT::testReadStartVals ()
{
  PilotPoints p(m_file,*m_par);
  TS_ASSERT(p.m_p->ReadStartVals());
  TS_ASSERT(p.m_p->m_vals.size() == 15);
  TS_ASSERT_DELTA(p.m_p->m_vals.at(0), (Real)1.1543750, CXXDELTA);
  TS_ASSERT_DELTA(p.m_p->m_vals.at(1), (Real)0.95622528, CXXDELTA);
  TS_ASSERT_DELTA(p.m_p->m_vals.at(8), (Real)0.94399530, CXXDELTA);
  TS_ASSERT_DELTA(p.m_p->m_vals.at(13), (Real)0.24132849, CXXDELTA);
  TS_ASSERT_DELTA(p.m_p->m_vals.at(14), (Real)0.89240378, CXXDELTA);
}
//------------------------------------------------------------------------------
void PilotPointsT::testDoInterpolation ()
{
  PilotPoints p(m_file, *m_par);
  Real d[15] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  std::vector<double> vF(&d[0], &d[14]);
  std::vector<Real> arrGrid;
  // can't do this anymore because it will crash
  //p.SetPPStartVals(vF);
  //TS_ASSERT(!p.DoInterpolation(arrGrid));
  //TS_ASSERT(arrGrid.empty());

  vF.push_back(d[14]);
  p.SetPPStartVals(vF);
  TS_ASSERT(p.DoInterpolation(arrGrid));
  TS_ASSERT(arrGrid.size() == 2940);
  TS_ASSERT_DELTA(arrGrid.at(0), (Real)4.2312121, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1), (Real)4.2429090, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1000), (Real)13.343507, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2000), (Real)8.1206160, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2938), (Real)9.3863764, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2939), (Real)9.4297485, CXXDELTA);
}
//------------------------------------------------------------------------------
void PilotPointsT::testDoInterpolation2 ()
{
  PilotPoints p(m_file, *m_par);
  std::vector<Real> arrGrid;
  TS_ASSERT(p.DoInterpolation(arrGrid));
  TS_ASSERT(arrGrid.size() == 2940);
  TS_ASSERT_DELTA(arrGrid.at(0), (Real)0.95093203, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1), (Real)0.95366085, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1000), (Real)0.37517014, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2000), (Real)0.48567227, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2938), (Real)0.94030368, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2939), (Real)0.94190466, CXXDELTA);
}
//------------------------------------------------------------------------------
void PilotPointsT::testDoInterpolationLog ()
{
  m_par->m_logInterp = true;
  PilotPoints p(m_file, *m_par);
  std::vector<Real> arrGrid;
  TS_ASSERT(p.DoInterpolation(arrGrid));
  TS_ASSERT(arrGrid.size() == 2940);
  TS_ASSERT_DELTA(arrGrid.at(0), (Real)0.90495789, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1), (Real)0.90758395, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(1000), (Real)0.31325194, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2000), (Real)0.46946296, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2938), (Real)0.90911561, CXXDELTA);
  TS_ASSERT_DELTA(arrGrid.at(2939), (Real)0.90953046, CXXDELTA);
}
#endif

