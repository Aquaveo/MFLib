//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Npf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Npf.h>

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

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Npf::NativeExpMf6Npf (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Npf::MfNativeExpMf6Npf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Npf::~NativeExpMf6Npf ()
{
} // MfNativeExpMf6Npf::~MfNativeExpMf6Npf
//------------------------------------------------------------------------------
/// \brief

/// BEGIN OPTIONS
///  [SAVE_FLOWS]
///  [ALTERNATIVE_CELL_AVERAGING <alternative_cell_averaging>]
///  [THICKSTRT]
///  [VARIABLECV [DEWATERED]]
///  [PERCHED]
///  [REWET WETFCT <wetfct> IWETIT <iwetit> IHDWET <ihdwet>]
///  [XT3D [RHS]]
/// END OPTIONS
///
/// BEGIN GRIDDATA
///  ICELLTYPE [LAYERED]
///   <icelltype(nodes)> -- READARRAY
///  K [LAYERED]
///   <k(nodes)> -- READARRAY
///  [K22 [LAYERED]
///   <k22(nodes)> -- READARRAY]
///  [K33 [LAYERED]
///   <k33(nodes)> -- READARRAY]
///  [ANGLE1 [LAYERED]
///   <angle1(nodes)> -- READARRAY]
///  [ANGLE2 [LAYERED]
///   <angle2(nodes)> -- READARRAY]
///  [ANGLE3 [LAYERED]
///   <angle3(nodes)> -- READARRAY]
///  [WETDRY [LAYERED]
///   <wetdry(nodes)> -- READARRAY]
/// END GRIDDATA
//------------------------------------------------------------------------------
bool NativeExpMf6Npf::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;

  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  lines.push_back("BEGIN OPTIONS");
  if (SaveFlows()) lines.push_back("  SAVE_FLOWS");
  lines.push_back(GetAlternativeCellAveragingLine());
  if (!GetThickStrtLine().empty()) lines.push_back(GetThickStrtLine());
  if (!GetVariableCvLine().empty()) lines.push_back(GetVariableCvLine());
  if (!GetPerchedLine().empty()) lines.push_back(GetPerchedLine());
  if (WettingActive(g->NumLay())) lines.push_back(GetWetOptionsLine());
  
  lines.push_back("END OPTIONS");
  lines.push_back("");


  lines.push_back("BEGIN GRIDDATA"); 
  lines.push_back("  ICELLTYPE LAYERED");
  lines.push_back("    CONSTANT -1");
  lines.push_back("  K LAYERED");
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, ARR_LPF_HK));

  GenerateK22K33();

  if (MfData::Get().GetPackage("K22"))
  {
    lines.push_back("  K22 LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, "K22"));
  }
  lines.push_back("  K33 LAYERED");
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, "K33"));

  if (WettingActive(g->NumLay()))
  {
    lines.push_back("  WETDRY LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, ARR_LPF_WET));
  }
  lines.push_back("END GRIDDATA"); 

  comments.assign(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "NPF");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Npf::ExportMf6Npf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Npf::SaveFlows ()
{
  const int* ILPFCB(0);
  if (Packages::UPW == m_pack->GetPackage()->PackageName())
  {
    m_pack->GetPackage()->GetField(Packages::UPWpack::IUPWCB, &ILPFCB);
  }
  else
  {
    m_pack->GetPackage()->GetField(Packages::LPFpack::ILPFCB, &ILPFCB);
  }
  if (ILPFCB && *ILPFCB != 0)
    return true;
  return false;
} // NativeExpMf6Npf::SaveFlows
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Npf::WettingActive (int a_nLay)
{
  const int* vals(0);
  if (m_pack->GetPackage()->GetField(Packages::LPFpack::LAYWET, &vals) && vals)
  {
    for (int i=0; i<a_nLay; ++i)
    {
      if (0 != vals[i]) return true;
    }
  }
  return false;
} // NativeExpMf6Npf::WettingActive
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Npf::GetWetOptionsLine ()
{
  CStr rval;
  const int *iwetit(0), *ihdwet(0);
  const Real *wetfct(0);
  MfPackage* p = m_pack->GetGlobal()->GetPackage("L99");
  if (p->GetField("WETFCT", &wetfct) && wetfct &&
      p->GetField("IWETIT", &iwetit) && iwetit &&
      p->GetField("IHDWET", &ihdwet) && ihdwet)
  {
    rval.Format("%s %d %d", STR(*wetfct), *iwetit, *ihdwet);
  }
  return rval;
} // NativeExpMf6Npf::WettingActive
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Npf::GetAlternativeCellAveragingLine ()
{
  CStr rval("  ALTERNATIVE_CELL_AVERAGING ");
  const int*  LAYAVG(0);
  m_pack->GetPackage()->GetField(Packages::LPFpack::LAYAVG, &LAYAVG);
  if (LAYAVG)
  {
    if (1 == *LAYAVG)      rval += "LOGARITHMIC";
    else if (2 == *LAYAVG) rval += "AMT-LMK";
    else                   rval += "AMT-HMK";
  }

  return rval;
} // NativeExpMf6Npf::GetAlternativeCellAveragingLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Npf::GetThickStrtLine ()
{
  CStr rval("");
  const int* ITHFLG(0);
  m_pack->GetPackage()->GetField(Packages::LPFpack::ITHFLG, &ITHFLG);
  if (ITHFLG && *ITHFLG)
  {
    rval += "  THICKSTRT";
  }

  return rval;
} // NativeExpMf6Npf::GetThickStrtLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Npf::GetVariableCvLine ()
{
  CStr rval("");
  const int  *ICONCV(0), *NOCVCO(0);

  m_pack->GetPackage()->GetField(Packages::LPFpack::ICONCV, &ICONCV);
  m_pack->GetPackage()->GetField(Packages::LPFpack::NOCVCO, &NOCVCO);
  if (ICONCV && *ICONCV)
  {
  }
  else
  {
    rval = "  VARIABLECV ";
    if (NOCVCO && *NOCVCO)
    {
    }
    else
    {
      rval += "DEWATERED";
    }
  }

  return rval;
} // NativeExpMf6Npf::GetVariableCvLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMf6Npf::GetPerchedLine ()
{
  CStr rval("");
  const int* NOVFC(0);
  m_pack->GetPackage()->GetField(Packages::LPFpack::NOVFC, &NOVFC);
  if (NOVFC && *NOVFC)
  {
  }
  else
  {
    rval = "  PERCHED";
  }

  return rval;
} // NativeExpMf6Npf::GetPerchedLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Npf::GenerateK22K33 ()
{
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return;
  MfGlobal* g = m_pack->GetGlobal();
  if (!g) return;
  int nLay = g->NumLay();

  std::map<CStr, std::vector< std::vector<Real> > >& mymap(nat->SavedRealArrays());
  std::map<CStr, std::vector<Real> >& mymapMult(nat->SavedRealArraysMult());

  std::vector< std::vector<Real> >& hk(mymap[ARR_LPF_HK]);
  std::vector<Real>& hkMult(mymapMult[ARR_LPF_HK]);

  int IPRN(-1), LAYER;
  Real rArrayMult(1);
  std::vector<Real> tmpRealArray(hk.front().size(), 0);

  // get HANI values
  std::vector< std::vector<Real> >& hani(mymap[ARR_LPF_HANI]);
  std::vector<Real>& haniMult(mymapMult[ARR_LPF_HANI]);
  const Real* chani;
  m_pack->GetPackage()->GetField(Packages::LPFpack::CHANI, &chani);
  // first check to see if every layer is equal to 1
  bool allEqualToOne(true);
  for (int i=0; i<nLay; ++i)
  {
    if (1 != fabs(chani[i])) allEqualToOne = false;
  }
  if (!allEqualToOne)
  {
    MfPackage tmpPack("K22");
    MfData::Get().AddPackage(&tmpPack);
    MfPackage* p = MfData::Get().GetPackage("K22");
    p->SetField(MfData::Packages::Array::LAYER, &LAYER);
    p->SetField(MfData::Packages::Array::IPRN, &IPRN);
    p->SetField(MfData::Packages::Array::MULT, &rArrayMult);
    p->SetField(MfData::Packages::Array::ARRAY, &tmpRealArray[0]);
    for (int i=0; i<nLay; ++i)
    {
      LAYER = i + 1;
      if (chani[i] <= 0)
      {
        rArrayMult = 1.0;
        Real r = fabs(chani[i]);
        for (size_t j=0; j<tmpRealArray.size(); ++j)
          tmpRealArray[j] = r;
      }
      else
      {
        rArrayMult = hkMult[i] * haniMult[i];
        for (size_t j=0; j<tmpRealArray.size(); ++j)
        {
          tmpRealArray[j] = hk[i][j] * hani[i][j];
        }
      }
      MfData::Get().Export("K22");
    }
  }

  // get VK values
  std::vector< std::vector<Real> >& vk(mymap[ARR_LPF_VK]);
  std::vector<Real>& vkMult(mymapMult[ARR_LPF_VK]);
  const int* layvka;
  if (Packages::UPW == m_pack->GetPackage()->PackageName())
    m_pack->GetPackage()->GetField(Packages::UPWpack::LAYVKAUPW, &layvka);
  else
    m_pack->GetPackage()->GetField(Packages::LPFpack::LAYVKA, &layvka);
  {
    MfPackage tmpPack("K33");
    MfData::Get().AddPackage(&tmpPack);
    MfPackage* p = MfData::Get().GetPackage("K33");
    p->SetField(MfData::Packages::Array::LAYER, &LAYER);
    p->SetField(MfData::Packages::Array::IPRN, &IPRN);
    p->SetField(MfData::Packages::Array::MULT, &rArrayMult);
    p->SetField(MfData::Packages::Array::ARRAY, &tmpRealArray[0]);
    for (int i=0; i<nLay; ++i)
    {
      LAYER = i + 1;
      if (0 == layvka[i]) // vertical K
      {
        rArrayMult = vkMult[i];
        for (size_t j=0; j<tmpRealArray.size(); ++j)
        {
          tmpRealArray[j] = vk[i][j];
        }
      }
      else
      { // vertical anisotropy
        rArrayMult = hkMult[i] * vkMult[i];
        for (size_t j=0; j<tmpRealArray.size(); ++j)
        {
          tmpRealArray[j] = hk[i][j] / vk[i][j];
        }
      }
      MfData::Get().Export("K33");
    }
  }
} // NativeExpMf6Npf::GenerateK22K33

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif