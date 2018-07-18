//------------------------------------------------------------------------------
// FILE      MfGlobal.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/MfData/MfGlobal.h>

#include <memory> // for shared_ptr

#include <private/util/util.h>
#include <private/MfData/MfExport/MfExporter.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/Parameters/ParamList.h>
#include <private/MfData/MfExport/private/Sqlite/SqArrayWriter.h>

using namespace MfData;

class MfGlobal::impl
{
public:
  impl(int a_NROW,
       int a_NCOL,
       int a_NLAY,
       int a_NPER,
       int a_ITMUNI,
       int a_LENUNI,
       const int *a_LAYCBD);
  impl(const impl &rhs);
  const impl& operator=(const impl &rhs);


  void SetNameFile(const char* const a_) { m_nameFile.push_back(a_); }
  bool AttachExporter(const char *a_,
                      const char *a_fileName,
                      const char *a_tables);

  void IGRID(int a_) { if (a_ < 1) return; m_curModIdx = a_-1; }
  int& Row() { VectorSizes(); return m_nRow[m_curModIdx]; }
  int& Col() { VectorSizes(); return m_nCol[m_curModIdx]; }
  int& Lay() { VectorSizes(); return m_nLay[m_curModIdx]; }
  int& nPer() { VectorSizes(); return m_nPer[m_curModIdx]; }
  int& TimeU() { VectorSizes(); return m_timeUnit[m_curModIdx]; }
  int& LengthU() { VectorSizes(); return m_lengthUnit[m_curModIdx]; }
  int& ModelType() { VectorSizes(); return m_modelType[m_curModIdx]; }
  std::vector<int>& LAYCBD() { VectorSizes(); return m_laycbd[m_curModIdx]; }
  std::vector< std::shared_ptr<MfPackage> >& Packages() { VectorSizes(); return m_packages[m_curModIdx]; }
  std::map<CStr, int>& iVars() { VectorSizes(); return m_intVars[m_curModIdx]; }
  std::map<CStr, Real>& rVars() { VectorSizes(); return m_realVars[m_curModIdx]; }
  std::map<CStr, CStr>& strVars() { VectorSizes(); return m_strVars[m_curModIdx]; }
  std::map<CStr, std::vector<int>>& iVecs() { VectorSizes(); return m_intVecs[m_curModIdx]; }
  std::map<CStr, std::vector<Real>>& rVecs() { VectorSizes(); return m_realVecs[m_curModIdx]; }
  std::map<CStr, std::vector<double>>& dVecs() { VectorSizes(); return m_doubleVecs[m_curModIdx]; }
  MfData::Export::MfExporter* Exporter() { VectorSizes(); return m_export[m_curModIdx]; }
  CStr& NameFile() { VectorSizes(); return m_nameFile[m_curModIdx]; }
  ParamList& GetParamList() { VectorSizes(); return *m_params[m_curModIdx]; }
  MfData::Export::SqArrayWriter* GetSqArrayWriter();
  size_t CurModIdx() { return m_curModIdx; }
  CStr& LgrName() { return m_LgrName; }
  int& Unstructured () { return m_unstructured; }
  bool& StackedGrid() { return m_stackedGrid; }
  int NumNodesUnstructured ()
  {
    int rval(-1);
    if (iVars().find("DISU_NODES") != iVars().end())
      rval = iVars()["DISU_NODES"];
    return rval;
  }

  std::vector<MfData::Export::MfExporter*>& vExporter() { return m_export; }

  int m_curPer;

private:
  std::vector<int> m_nRow, m_nCol, m_nLay, m_nPer, m_timeUnit, m_lengthUnit;
  std::vector<int> m_modelType;
  std::vector< std::vector<int> > m_laycbd;
  std::vector< std::vector< std::shared_ptr<MfPackage> > > m_packages;
  std::vector< std::map<CStr, int> > m_intVars;
  std::vector< std::map<CStr, Real> > m_realVars;
  std::vector< std::map<CStr, CStr> > m_strVars;
  std::vector< std::map<CStr, std::vector<int>> > m_intVecs;
  std::vector< std::map<CStr, std::vector<Real>> > m_realVecs;
  std::vector< std::map<CStr, std::vector<double>> > m_doubleVecs;
  std::vector<MfData::Export::MfExporter*> m_export;
  std::vector<CStr> m_nameFile;
  std::vector<ParamList*> m_params;
  size_t m_curModIdx;
  CStr m_LgrName;
  int m_unstructured;
  bool m_stackedGrid;
  MfData::Export::SqArrayWriter* m_sqArrayWriter;
  void VectorSizes()
  {
    size_t idx = m_curModIdx + 1;
    if (idx > m_nRow.size()) m_nRow.resize(idx, 0);
    if (idx > m_nCol.size()) m_nCol.resize(idx, 0);
    if (idx > m_nLay.size()) m_nLay.resize(idx, 0);
    if (idx > m_nPer.size()) m_nPer.resize(idx, 0);
    if (idx > m_timeUnit.size()) m_timeUnit.resize(idx, 0);
    if (idx > m_lengthUnit.size()) m_lengthUnit.resize(idx, 0);
    if (idx > m_modelType.size()) m_modelType.resize(idx, 0);
    if (idx > m_laycbd.size()) m_laycbd.resize(idx);
    if (idx > m_packages.size()) m_packages.resize(idx);
    if (idx > m_intVars.size()) m_intVars.resize(idx);
    if (idx > m_realVars.size()) m_realVars.resize(idx);
    if (idx > m_strVars.size()) m_strVars.resize(idx);
    if (idx > m_intVecs.size()) m_intVecs.resize(idx);
    if (idx > m_realVecs.size()) m_realVecs.resize(idx);
    if (idx > m_doubleVecs.size()) m_doubleVecs.resize(idx);
    if (idx > m_export.size()) m_export.resize(idx, 0);
    if (idx > m_nameFile.size()) m_nameFile.resize(idx, "");
    while (idx > m_params.size()) m_params.push_back(new ParamList());
  }
};

//------------------------------------------------------------------------------
/// \brief Gets a reference to the Global Modflow data.
//------------------------------------------------------------------------------
MfGlobal& MfData::Get ()
{
  return MfData::MfGlobal::Get();
} // MfData::Get
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::Init (int a_modelType,
                   int a_IGRID,
                   const char *a_exp,
                   const char *a_fileName,
                   const char *a_packages)
{
  MfData::MfGlobal::Init(a_modelType, a_IGRID, a_exp, a_fileName, a_packages);
} // MfData::Init
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::Set (int a_NROW,
                  int a_NCOL,
                  int a_NLAY,
                  int a_NPER)
{
  MfData::MfGlobal::Set(a_NROW, a_NCOL, a_NLAY, a_NPER);
} // MfData::Set
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::Set (int a_NROW,
                  int a_NCOL,
                  int a_NLAY,
                  int a_NPER,
                  int a_ITMUNI,
                  int a_LENUNI,
                  const int *a_LAYCBD,
                  int a_IUNSTR)
{
  MfData::MfGlobal::Set(a_NROW, a_NCOL, a_NLAY, a_NPER, a_ITMUNI, a_LENUNI,
                        a_LAYCBD, a_IUNSTR);
} // MfData::Set
//------------------------------------------------------------------------------
/// \brief Gets a reference to the Global Modflow data.
//------------------------------------------------------------------------------
MfGlobal& MfData::MfGlobal::Get ()
{
  static MfGlobal m_(0,0,0,0,1,1,0);
  return (m_);
} // MfData::MfGlobal::Get
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::MfGlobal::Init (int a_modelType,
                             int a_IGRID,
                             const char *a_exp,
                             const char *a_fileName,
                             const char *a_tables)
{
  // attach the exporter to this class
  MfGlobal& mfGlobal = Get();
  mfGlobal.IGRID(a_IGRID);
  mfGlobal.ModelType(a_modelType);
  mfGlobal.m_p->SetNameFile(a_fileName);
  mfGlobal.m_p->AttachExporter(a_exp, a_fileName, a_tables);
} // MfData::MfGlobal::Init
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::MfGlobal::Set (int a_NROW,
                            int a_NCOL,
                            int a_NLAY,
                            int a_NPER)
{
  MfGlobal& global = MfGlobal::Get();
  global.m_p->Row() = a_NROW;
  global.m_p->Col() = a_NCOL;
  global.m_p->Lay() = a_NLAY;
  global.m_p->nPer() = a_NPER;
} // MfData::MfGlobal::Set
//------------------------------------------------------------------------------
/// \brief Initializes the Global Modflow data.
//------------------------------------------------------------------------------
void MfData::MfGlobal::Set (int a_NROW,
                            int a_NCOL,
                            int a_NLAY,
                            int a_NPER,
                            int a_ITMUNI,
                            int a_LENUNI,
                            const int *a_LAYCBD,
                            int a_IUNSTR)
{
  MfGlobal& global = MfData::Get();
  global.m_p->Row() = a_NROW;
  global.m_p->Col() = a_NCOL;
  global.m_p->Lay() = a_NLAY;
  global.m_p->nPer() = a_NPER;
  global.m_p->TimeU() = a_ITMUNI;
  global.m_p->LengthU() = a_LENUNI;
  global.m_p->Unstructured() = a_IUNSTR;

  if (a_NLAY > 0)
    global.m_p->LAYCBD().assign(a_NLAY, 0);
  for (int i=0; a_LAYCBD && i<a_NLAY; i++)
    global.m_p->LAYCBD()[i] = a_LAYCBD[i];
} // MfData::MfGlobal::Set
//------------------------------------------------------------------------------
/// \brief Sets the "stackedGrid" variable
//------------------------------------------------------------------------------
void MfData::MfGlobal::SetUnstructured (bool a_unstructured)
{
  MfGlobal& global = MfData::Get();
  global.m_p->Unstructured() = a_unstructured ? 1 : 0;
} // MfData::MfGlobal::SetUnstructured
//------------------------------------------------------------------------------
/// \brief Sets the "stackedGrid" variable
//------------------------------------------------------------------------------
void MfData::MfGlobal::SetStackedGrid (bool a_stackedGrid)
{
  MfGlobal& global = MfData::Get();
  global.m_p->StackedGrid() = a_stackedGrid;
} // MfData::MfGlobal::SetStackedGrid
//------------------------------------------------------------------------------
/// \brief Sets the "stackedGrid" variablew
//------------------------------------------------------------------------------
bool MfData::MfGlobal::GetStackedGrid ()
{
  MfGlobal& global = MfData::Get();
  return global.m_p->StackedGrid();
} // MfData::MfGlobal::GetStackedGrid
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
MfGlobal::MfGlobal (int a_NROW,
                    int a_NCOL,
                    int a_NLAY,
                    int a_NPER,
                    int a_ITMUNI,
                    int a_LENUNI,
                    const int *a_LAYCBD) :
m_p(new MfGlobal::impl(a_NROW,a_NCOL,a_NLAY,a_NPER,a_ITMUNI,a_LENUNI, a_LAYCBD))
{
} // MfGlobal::MfGlobal
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
MfGlobal::~MfGlobal ()
{
  if (m_p)
    delete(m_p);
  m_p = 0;
} // MfGlobal::~MfGlobal
//------------------------------------------------------------------------------
/// \brief Copy constructor.
//------------------------------------------------------------------------------
MfGlobal::MfGlobal (const MfGlobal &rhs) :
m_p(new MfGlobal::impl(*rhs.m_p))
{
} // MfGlobal::MfGlobal
//------------------------------------------------------------------------------
/// \brief Copy constructor.
//------------------------------------------------------------------------------
const MfGlobal& MfGlobal::operator= (const MfGlobal &rhs)
{
  if (this != &rhs)
  {
    *m_p = *rhs.m_p;
  }
  return(*this);
} // MfGlobal::operator=
//------------------------------------------------------------------------------
/// \brief Gets the number of rows in the MODFLOW grid.
//------------------------------------------------------------------------------
int MfGlobal::NumRow ()
{
  return(m_p->Row());
} // MfGlobal::NumRow
//------------------------------------------------------------------------------
/// \brief Gets the number of columns in the MODFLOW grid.
//------------------------------------------------------------------------------
int MfGlobal::NumCol ()
{
  return(m_p->Col());
} // MfGlobal::NumCol
//------------------------------------------------------------------------------
/// \brief Gets the number of layers in the MODFLOW grid.
//------------------------------------------------------------------------------
int MfGlobal::NumLay ()
{
  return(m_p->Lay());
} // MfGlobal::NumLay
//------------------------------------------------------------------------------
/// \brief Gets the number of stress periods in the MODFLOW simulation.
//------------------------------------------------------------------------------
int MfGlobal::NumPeriods ()
{
  return(m_p->nPer());
} // MfGlobal::NumPeriods
//------------------------------------------------------------------------------
/// \brief Gets the length unit for the MODFLOW simulation.
//------------------------------------------------------------------------------
int MfGlobal::LengthUnit ()
{
  return(m_p->LengthU());
} // MfGlobal::LengthUnit
//------------------------------------------------------------------------------
/// \brief Gets the time unit for the MODFLOW simulation.
//------------------------------------------------------------------------------
int MfGlobal::TimeUnit ()
{
  return(m_p->TimeU());
} // MfGlobal::TimeUnit
//------------------------------------------------------------------------------
/// \brief Gets the time unit for the MODFLOW simulation.
//------------------------------------------------------------------------------
int* MfGlobal::LayCbd ()
{
  return(&m_p->LAYCBD()[0]);
} // MfGlobal::LayCbd
//------------------------------------------------------------------------------
/// \brief Gets the time unit for the MODFLOW simulation.
//------------------------------------------------------------------------------
ParamList& MfGlobal::GetParamList ()
{
  return(m_p->GetParamList());
} // MfGlobal::GetParamList
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
MfData::Export::SqArrayWriter* MfGlobal::GetSqArrayWriter()
{
  return m_p->GetSqArrayWriter();
} // MfGlobal::GetSqArrayWriter
//------------------------------------------------------------------------------
/// \brief Gets the time unit for the MODFLOW simulation.
//------------------------------------------------------------------------------
size_t MfGlobal::CurModIdx ()
{
  return(m_p->CurModIdx());
} // MfGlobal::CurModIdx
//------------------------------------------------------------------------------
/// \brief Sets the current stress period
//------------------------------------------------------------------------------
void MfGlobal::PutCurrentPeriod (int a_)
{
  m_p->m_curPer = a_;
} // MfGlobal::PutCurrentPeriod
//------------------------------------------------------------------------------
/// \brief Gets the current stress period
//------------------------------------------------------------------------------
int MfGlobal::GetCurrentPeriod ()
{
  return (m_p->m_curPer);
} // MfGlobal::GetCurrentPeriod
//------------------------------------------------------------------------------
/// \brief Adds a package to the MODFLOW simulation. If the package already
/// exists then false is returned.
//------------------------------------------------------------------------------
bool MfGlobal::AddPackage (MfPackage *a_)
{
  if (!a_)
    return false;
  CStr p(a_->PackageName()), p1;
  bool alreadyExists(false);
  for (size_t i=0; !alreadyExists && i<m_p->Packages().size(); i++)
  {
    p1 = m_p->Packages().at(i)->PackageName();
    if (p == p1)
      alreadyExists = true;
  }
  if (!alreadyExists)
  {
    std::shared_ptr<MfPackage> ptr(new MfPackage(a_->PackageName()));
    *ptr = *a_;
    m_p->Packages().push_back(ptr);
  }
  return(!alreadyExists);
} // MfGlobal::AddPackage
//------------------------------------------------------------------------------
/// \brief Gets a package based on the name. This may return NULL.
//------------------------------------------------------------------------------
MfPackage *MfGlobal::GetPackage (const char *a_)
{
  MfPackage *pack(NULL);
  CStr p(a_), p1;
  for (size_t i=0; !pack && i<m_p->Packages().size(); i++)
  {
    p1 = m_p->Packages().at(i)->PackageName();
    if (p == p1)
      pack = m_p->Packages().at(i).get();
  }
  return pack;
} // MfGlobal::GetPackage
//------------------------------------------------------------------------------
/// \brief Exports the data contained in the package
//------------------------------------------------------------------------------
bool MfGlobal::Export (const char *a_)
{
  if (!m_p->Exporter())
    return false;

  CStr type(a_);
  MfPackage *p(0);
  size_t size = m_p->Packages().size();
  for (size_t i=0; !p && i<size; i++)
  {
    if (type == m_p->Packages().at(i)->PackageName())
      p = m_p->Packages().at(i).get();
  }

  if (!p)
    return false;

  if (p->PackageName() == "S99")
  {
    const int* val;
    if (p->GetField("SEG_SIZE", &val) && val)
    {
      SetIntVar("SFR_SEG_SIZE", *val);
    }
  }

  return(m_p->Exporter()->ExportPackage(this, p));
} // MfGlobal::Export
//------------------------------------------------------------------------------
/// \brief Set the type of current model index for LGR
//------------------------------------------------------------------------------
void MfGlobal::IGRID (int a_IGRID)
{
  m_p->IGRID(a_IGRID);
} // MfGlobal::IGRID
//------------------------------------------------------------------------------
/// \brief Set the type of model being exported (MF2K, MF2K5, SEAWAT).
/// \param a_modelType => 0 = MODFLOW-2000; 1 = MODFLOW-2005; 2 = MODFLOW-NWT;
///                       3 = SEAWAT; 4 = MODFLOW-LGR
//------------------------------------------------------------------------------
void MfGlobal::ModelType (int a_modelType)
{
  m_p->ModelType() = a_modelType;
} // MfGlobal::ModelType
//------------------------------------------------------------------------------
/// \brief Get the type of model being exported (MF2K, MF2K5, SEAWAT).
//------------------------------------------------------------------------------
int MfGlobal::ModelType ()
{
  return m_p->ModelType();
} // MfGlobal::ModelType
//------------------------------------------------------------------------------
/// \brief Set the LGR name
//------------------------------------------------------------------------------
void MfGlobal::LgrName (const char* a_)
{
  m_p->LgrName() = a_;
} // MfGlobal::LgrName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
const char* MfGlobal::LgrName ()
{
  return (LPCTSTR)m_p->LgrName();
}
//------------------------------------------------------------------------------
int MfGlobal::Unstructured () const
{
  return m_p->Unstructured();
}
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int MfGlobal::NumNodesUnstructured () const
{
  return m_p->NumNodesUnstructured();
} // MfGlobal::NumNodesUnstructured
//------------------------------------------------------------------------------
/// \brief Sets an integer variable by name
//------------------------------------------------------------------------------
void MfGlobal::SetIntVar (const char* a_NAME,
                          int a_var)
{
  CStr nm(a_NAME);
  m_p->iVars()[nm] = a_var;
} // MfGlobal::SetIntVar
//------------------------------------------------------------------------------
/// \brief Gets an integer variable by name
//------------------------------------------------------------------------------
bool MfGlobal::GetIntVar (const char* a_NAME,
                          int& a_var) const
{
  CStr nm(a_NAME);
  std::map<CStr, int>::const_iterator it = m_p->iVars().find(nm);
  if (it != m_p->iVars().end())
  {
    a_var = it->second;
    return true;
  }
  return false;
} // MfGlobal::GetIntVar
//------------------------------------------------------------------------------
/// \brief Sets an integer variable by name
//------------------------------------------------------------------------------
void MfGlobal::SetRealVar (const char* a_NAME,
                           Real a_var)
{
  CStr nm(a_NAME);
  m_p->rVars()[nm] = a_var;
} // MfGlobal::SetRealVar
//------------------------------------------------------------------------------
/// \brief Gets an integer variable by name
//------------------------------------------------------------------------------
bool MfGlobal::GetRealVar (const char* a_NAME,
                           Real& a_var) const
{
  CStr nm(a_NAME);
  std::map<CStr, Real>::const_iterator it = m_p->rVars().find(nm);
  if (it != m_p->rVars().end())
  {
    a_var = it->second;
    return true;
  }
  return false;
} // MfGlobal::GetRealVar
//------------------------------------------------------------------------------
/// \brief Sets an integer variable by name
//------------------------------------------------------------------------------
void MfGlobal::SetStrVar (const char* a_NAME,
                          const CStr& a_var)
{
  CStr nm(a_NAME);
  m_p->strVars()[nm] = a_var;
} // MfGlobal::SetStrVar
//------------------------------------------------------------------------------
/// \brief Gets an integer variable by name
//------------------------------------------------------------------------------
bool MfGlobal::GetStrVar (const char* a_NAME,
                          CStr& a_var) const
{
  CStr nm(a_NAME);
  std::map<CStr, CStr>::const_iterator it = m_p->strVars().find(nm);
  if (it != m_p->strVars().end())
  {
    a_var = it->second;
    return true;
  }
  return false;
} // MfGlobal::GetStrVar

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
MfGlobal::impl::impl (int a_NROW,
                      int a_NCOL,
                      int a_NLAY,
                      int a_NPER,
                      int a_ITMUNI,
                      int a_LENUNI,
                      const int *a_LAYCBD) :
m_nRow(1, a_NROW),
m_nCol(1,a_NCOL),
m_nLay(1,a_NLAY),
m_nPer(1,a_NPER),
m_timeUnit(1,a_ITMUNI),
m_lengthUnit(1,a_LENUNI),
m_curPer(1),
m_modelType(0),
m_laycbd(),
m_packages(),
m_intVars(),
m_realVars(),
m_strVars(),
m_intVecs(),
m_realVecs(),
m_doubleVecs(),
m_export(),
m_nameFile(),
m_curModIdx(),
m_LgrName(),
m_unstructured(0),
m_stackedGrid(0),
m_sqArrayWriter(nullptr)
{
  m_laycbd.push_back(std::vector<int>());
  if (Lay() > 0)
    m_laycbd[0].assign(Lay(), 0);
  for (int i=0; a_LAYCBD && i<Lay(); i++)
    m_laycbd[0][i] = a_LAYCBD[i];
} // MfGlobal::impl::impl
//------------------------------------------------------------------------------
/// \brief Copy constructor.
//------------------------------------------------------------------------------
MfGlobal::impl::impl (const MfGlobal::impl &rhs) :
m_nRow(rhs.m_nRow),
m_nCol(rhs.m_nCol),
m_nLay(rhs.m_nLay),
m_nPer(rhs.m_nPer),
m_timeUnit(rhs.m_timeUnit),
m_lengthUnit(rhs.m_lengthUnit),
m_curPer(rhs.m_curPer),
m_modelType(rhs.m_modelType),
m_laycbd(rhs.m_laycbd),
m_packages(rhs.m_packages),
m_intVars(rhs.m_intVars),
m_realVars(rhs.m_realVars),
m_strVars(rhs.m_strVars),
m_intVecs(rhs.m_intVecs),
m_realVecs(rhs.m_realVecs),
m_doubleVecs(rhs.m_doubleVecs),
m_export(),
m_nameFile(rhs.m_nameFile),
m_curModIdx(rhs.m_curModIdx),
m_LgrName(rhs.m_LgrName),
m_unstructured(rhs.m_unstructured),
m_stackedGrid(rhs.m_stackedGrid)
{
  for (size_t i=0; i<rhs.m_export.size(); ++i)
  {
    m_export.push_back(
      new MfData::Export::MfExporter(rhs.m_export[i]->GetTypeName()) );
  }
} // MfGlobal::impl::impl
//------------------------------------------------------------------------------
/// \brief Operator=
//------------------------------------------------------------------------------
const MfGlobal::impl& MfGlobal::impl::operator= (const MfGlobal::impl &rhs)
{
  if (this != &rhs)
  {
    m_nRow = rhs.m_nRow;
    m_nCol = rhs.m_nCol;
    m_nLay = rhs.m_nLay;
    m_nPer = rhs.m_nPer;
    m_timeUnit = rhs.m_timeUnit;
    m_lengthUnit = rhs.m_lengthUnit;
    m_curPer = rhs.m_curPer;
    m_modelType = rhs.m_modelType;
    m_laycbd = rhs.m_laycbd;
    m_packages = rhs.m_packages;
    m_intVars = rhs.m_intVars;
    m_realVars = rhs.m_realVars;
    m_strVars = rhs.m_strVars;
    m_intVecs = rhs.m_intVecs;
    m_realVecs = rhs.m_realVecs;
    m_doubleVecs = rhs.m_doubleVecs;
    for (size_t i=0; i<rhs.m_export.size(); ++i)
    {
      m_export.push_back(
        new MfData::Export::MfExporter(rhs.m_export[i]->GetTypeName()) );
    }
    m_nameFile = rhs.m_nameFile;
    m_curModIdx = rhs.m_curModIdx;
    m_LgrName = rhs.m_LgrName;
    m_unstructured = rhs.m_unstructured;
    m_stackedGrid = rhs.m_stackedGrid;
  }
  return(*this);
} // MfGlobal::impl::impl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
MfData::Export::SqArrayWriter* MfGlobal::impl::GetSqArrayWriter()
{
  if (!m_sqArrayWriter)
    m_sqArrayWriter = new MfData::Export::SqArrayWriter();
  return m_sqArrayWriter;
} // MfGlobal::impl::GetSqArrayWriter
//------------------------------------------------------------------------------
/// \brief Creates an exporter that is attached to the MODFLOW data. When a 
/// package is added to the MODFLOW data then the exporter will be notified.
//------------------------------------------------------------------------------
bool MfGlobal::impl::AttachExporter (const char *a_,
                                     const char *a_fileName,
                                     const char *a_tables)
{
  try
  {
    if (Exporter())
    {
      return false;
    }

    m_export[m_curModIdx] = new MfData::Export::MfExporter(a_);
    if (m_export.at(m_curModIdx))
    {
      while (m_modelType.size() < m_curModIdx+1)
      {
        m_modelType.push_back(0);
      }
      // this must happen before SetFileName
      m_export.at(m_curModIdx)->SetTablesStr(a_tables);
      m_export.at(m_curModIdx)->SetModelType(m_modelType.at(m_curModIdx));
      m_export.at(m_curModIdx)->SetFileName(a_fileName);
    }
    return (m_export.at(m_curModIdx) ? 1 : 0);
  }
  catch (std::out_of_range&)
  {
    return false;
  }
} // MfGlobal::impl::AttachExporter


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfGlobal.t.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
void MfGlobalT::setUp ()
{
  std::vector<int> laycbd(30, 0);
  m_p = new MfGlobal(25,27,30,10,2,3,&laycbd[0]);
}
//------------------------------------------------------------------------------
void MfGlobalT::tearDown ()
{
  if (m_p)
    delete(m_p);
}
//------------------------------------------------------------------------------
void MfGlobalT::testCreateClass ()
{
  MfGlobal *p = new MfGlobal(1,1,1,1,1,1,0);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void MfGlobalT::testNumRows ()
{
  TS_ASSERT_EQUALS(m_p->NumRow(), 25);
}
//------------------------------------------------------------------------------
void MfGlobalT::testNumCols ()
{
  TS_ASSERT_EQUALS(m_p->NumCol(), 27);
}
//------------------------------------------------------------------------------
void MfGlobalT::testNumLay ()
{
  TS_ASSERT_EQUALS(m_p->NumLay(), 30);
}
//------------------------------------------------------------------------------
void MfGlobalT::testNumPeriods ()
{
  TS_ASSERT_EQUALS(m_p->NumPeriods(), 10);
}
//------------------------------------------------------------------------------
void MfGlobalT::testLengthUnit ()
{
  TS_ASSERT_EQUALS(m_p->LengthUnit(), 3);
}
//------------------------------------------------------------------------------
void MfGlobalT::testTimeUnit ()
{
  TS_ASSERT_EQUALS(m_p->TimeUnit(), 2);
}
//------------------------------------------------------------------------------
void MfGlobalT::testAddPackage()
{
  MfPackage *p1(NULL);
  MfPackage p("DIS");

  TS_ASSERT(!m_p->AddPackage(p1));
  TS_ASSERT(m_p->AddPackage(&p));
  TS_ASSERT(!m_p->AddPackage(&p));
}
//------------------------------------------------------------------------------
void MfGlobalT::testGetPackage()
{
  MfPackage p("DIS");

  TS_ASSERT(!m_p->GetPackage("DIS"));
  TS_ASSERT(m_p->AddPackage(&p));
  TS_ASSERT(m_p->GetPackage("DIS"));
}
//------------------------------------------------------------------------------
void MfGlobalT::testInit_Get ()
{
  MfData::Init(1, -1, "", "", "");
  MfData::Set(1,2,3,4,5,6,0,1);
  TS_ASSERT_EQUALS(MfData::Get().ModelType(), 1);
  TS_ASSERT_EQUALS(MfData::Get().NumRow(), 1);
  TS_ASSERT_EQUALS(MfData::Get().NumCol(), 2);
  TS_ASSERT_EQUALS(MfData::Get().NumLay(), 3);
  TS_ASSERT_EQUALS(MfData::Get().NumPeriods(), 4);
  TS_ASSERT_EQUALS(MfData::Get().TimeUnit(), 5);
  TS_ASSERT_EQUALS(MfData::Get().LengthUnit(), 6);
  TS_ASSERT_EQUALS(MfData::Get().Unstructured(), 1);
  MfData::Init(MfData::MF2K, -1, "", "", "");
}
//------------------------------------------------------------------------------
void MfGlobalT::testAttachExporter ()
{
  CStr path;
  util::GetTempDirectory(path);
  path += "exportFile.txt";
  TS_ASSERT(m_p->m_p->AttachExporter("", path, ""));
  TS_ASSERT(!m_p->m_p->AttachExporter("", path, ""));
}
//------------------------------------------------------------------------------
void MfGlobalT::testExport ()
{
  MfData::Init(1, -1, "", "", "");
  MfData::Set(1,2,3,4,5,6,0,1);
  TS_ASSERT(!MfData::Get().Export("dis"));
  MfData::MfPackage pack("stuff");
  MfData::Get().AddPackage(&pack);
  TS_ASSERT(MfData::Get().Export("stuff"));
  MfData::Init(MfData::MF2K, -1, "", "", "");
}
//------------------------------------------------------------------------------
void MfGlobalT::testVariables ()
{
  //TS_FAIL("MfGlobalT::testVariables");
  {
    m_p->SetIntVar("MyInt", 2);
    int var;
    bool rv = m_p->GetIntVar("MyInt", var);
    TS_ASSERT(rv);
    TS_ASSERT_EQUALS(var, 2);
  }

  {
    m_p->SetRealVar("MyReal", 3.0);
    Real var;
    bool rv = m_p->GetRealVar("MyReal", var);
    TS_ASSERT(rv);
    TS_ASSERT_EQUALS(var, 3.0);
  }

  {
    m_p->SetStrVar("MyStr", "Bob");
    CStr var;
    bool rv = m_p->GetStrVar("MyStr", var);
    TS_ASSERT(rv);
    TS_ASSERT_EQUALS(var, "Bob");
  }
} // MfGlobalT::testVariables

#endif
