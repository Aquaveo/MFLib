//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Uzf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Uzf.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>



using namespace MfData::Export;

class NativeExpMf6Uzf::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_) {}

  void WriteOptions();
  void WriteDimensions();

  NativePackExp* m_pack;
  std::vector<CStr> m_lines;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Uzf::NativeExpMf6Uzf (NativePackExp* a_) :
m_p(new impl(a_))

{
} // MfNativeExpMf6Uzf::MfNativeExpMf6Uzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Uzf::~NativeExpMf6Uzf ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // MfNativeExpMf6Uzf::~MfNativeExpMf6Uzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Uzf::Export ()
{
  if (!m_p) return false;

  m_p->WriteOptions();
  m_p->WriteDimensions();

  return true;
} // NativeExpMf6Uzf::ExportMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WriteOptions ()
{
  using namespace MfData::Packages;
  const int *nuztop, *iuzfopt, *irunflg, *ietflg, *iuzfcb1, *iuzfcb2, *ntrail2,
    *nsets2, *nuzgag, *nosurfleak;
  const Real *surfdep;
  MfPackage* p= m_pack->GetPackage();
  if (p->GetField(UZFpack::NUZTOP, &nuztop) && nuztop &&
    p->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
    p->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
    p->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
    p->GetField(UZFpack::IUZFCB1, &iuzfcb1) && iuzfcb1 &&
    p->GetField(UZFpack::IUZFCB2, &iuzfcb2) && iuzfcb2 &&
    p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
    p->GetField(UZFpack::NSETS2, &nsets2) && nsets2 &&
    p->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
    p->GetField(UZFpack::SURFDEP, &surfdep) && surfdep &&
    p->GetField(UZFpack::NOSURFLEAK, &nosurfleak) && nosurfleak)
  {
  }

} // NativeExpMf6Uzf::impl::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WriteDimensions ()
{
  using namespace MfData::Packages;
  MfGlobal *g = m_pack->GetGlobal();
  m_lines.push_back("BEGIN DIMENSIONS");
  int nCells = g->NumRow() * g->NumCol();

  const int* ntrail2(0),* nsets2(0);
  MfPackage* p= m_pack->GetPackage();
  if (p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
      p->GetField(UZFpack::NSETS2, &nsets2) && nsets2)
  {

  }

  m_lines.push_back("END DIMENSIONS");
} // NativeExpMf6Uzf::impl::WriteDimensions





///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif