//------------------------------------------------------------------------------
// FILE      MfPackageUtil.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/MfData/MfPackageUtil.h>

#include <set>
#include <sstream>

#include <private/MfData/MfExport/private/Native/NativeExpCln.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfData/Packages/ObsHd.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>

//------------------------------------------------------------------------------
static std::set<int>& GetSuspendedCommentsSet ()
{
  static std::vector< std::set<int> > m_set; // ok to leave static
  size_t idx = MfData::Get().CurModIdx();
  while (m_set.size() < idx+1) m_set.push_back(std::set<int>());
  return m_set[idx];
} // GetSuspendedCommentsSet
//------------------------------------------------------------------------------
static std::map<CStr, CStr>& GetCommentsMap ()
{
  static std::vector< std::map<CStr, CStr> > m_map; // ok to leave static
  size_t idx = MfData::Get().CurModIdx();
  while (m_map.size() < idx+1) m_map.push_back(std::map<CStr, CStr>());
  return m_map[idx];
  //static std::map<CStr, CStr> m_map;
  //return m_map;
} // GetCommentMap
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLines0And1 (const int *a_ICLNTIB,
                                      const int *a_NCLN,
                                      const int *a_ICLNNDS,
                                      const int *a_ICLNCB,
                                      const int *a_ICLNHD,
                                      const int *a_ICLNDD,
                                      const int *a_ICLNIB,
                                      const int *a_NCLNGWC,
                                      const int *a_NCONDUITYP,
                                      const int *a_ICLNPCB,
                                      const int *a_ICLNGWCB)
{
  MfPackage pack(MfData::Packages::CLNLines0And1);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::CLNLines0And1));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::Cln::ICLNTIB, a_ICLNTIB);
  p->SetField(MfData::Packages::Cln::NCLN, a_NCLN);
  p->SetField(MfData::Packages::Cln::ICLNNDS, a_ICLNNDS);
  p->SetField(MfData::Packages::Cln::ICLNCB, a_ICLNCB);
  p->SetField(MfData::Packages::Cln::ICLNHD, a_ICLNHD);
  p->SetField(MfData::Packages::Cln::ICLNDD, a_ICLNDD);
  p->SetField(MfData::Packages::Cln::ICLNIB, a_ICLNIB);
  p->SetField(MfData::Packages::Cln::NCLNGWC, a_NCLNGWC);
  p->SetField(MfData::Packages::Cln::NCONDUITYP, a_NCONDUITYP);
  p->SetField(MfData::Packages::Cln::ICLNPCB, a_ICLNPCB);
  p->SetField(MfData::Packages::Cln::ICLNGWCB, a_ICLNGWCB);
  if (!exists)
    MfData::Get().AddPackage(&pack);
  MfData::Get().Export(MfData::Packages::CLNLines0And1);
} // MfData::Packages::ClnLines0And1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLine7 (const Real *a_ACLNNDSAQ,
                                 const int *a_NCLNNDS)
{
  MfPackage pack(MfData::Packages::CLN);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::CLN));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  // Make a copy of the array because it is freed right away
  int size = (*a_NCLNNDS) * MfData::Export::NativeExpCln::ASE_LN7;
  Real* aclnndsaq = util::NewRealArray(size);
  for (int i=0; i<size; i++) {
    aclnndsaq[i] = a_ACLNNDSAQ[i];
  }

  p->SetField(MfData::Packages::Cln::ACLNNDSAQ, aclnndsaq);
  p->SetField(MfData::Packages::Cln::NCLNNDS, a_NCLNNDS);
  if (!exists)
    MfData::Get().AddPackage(&pack);
} // MfData::Packages::ClnLine7
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLine8Or9 (const Real *a_ACLNGWCAQ, int a_size2)
{
  MfPackage pack(MfData::Packages::CLN);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::CLN));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  // Get size of the array
  MfPackage *pLine1 = MfData::Get().GetPackage(MfData::Packages::CLNLines0And1);
  const int* nclngwc(0);
  if (!pLine1->GetField(Cln::NCLNGWC, &nclngwc) || !nclngwc)
    return;

  // Make a copy of the array because it is freed right away
  int size = (*nclngwc) * a_size2;
  Real* aclnndsaq = util::NewRealArray(size);
  for (int i=0; i<size; i++) {
    aclnndsaq[i] = a_ACLNGWCAQ[i];
  }

  p->SetField(MfData::Packages::Cln::ACLNGWCAQ, aclnndsaq);
  if (!exists)
    MfData::Get().AddPackage(&pack);
} // MfData::Packages::ClnLine8Or9
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLine8 (const Real *a_ACLNGWCAQ)
{
  ClnLine8Or9(a_ACLNGWCAQ, MfData::Export::NativeExpCln::ASE_LN8);
} // MfData::Packages::ClnLine8
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLine9 (const Real *a_ACLNGWCAQ)
{
  ClnLine8Or9(a_ACLNGWCAQ, MfData::Export::NativeExpCln::ASE_LN9);
} // MfData::Packages::ClnLine9
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLine10 (const Real *a_ACLNCOND)
{
  MfPackage pack(MfData::Packages::CLN);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::CLN));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(MfData::Packages::Cln::ACLNCOND, a_ACLNCOND);
  if (!exists)
    MfData::Get().AddPackage(&pack);
} // MfData::Packages::ClnLine10
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ClnLines11And12 ()
{
  MfData::Get().Export(MfData::Packages::CLN);
} // MfData::Packages::ClnLines11And12
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::Disu1 (const int *a_NODES,
                              const int *a_NJAG,
                              const int *a_IVSD,
                              const int *a_IDSYMRD)
{
  MfData::Get().SetIntVar("DISU_NODES", *a_NODES);
  MfPackage pack(MfData::Packages::DISU);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::DISU));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  // we have to copy the LAYCBD because MODFLOW changes the values before
  // we write the file
  p->SetField(MfData::Packages::Disu::LAYCBD, MfGlobal::Get().LayCbd());
  p->SetField(MfData::Packages::Disu::NODES, a_NODES);
  p->SetField(MfData::Packages::Disu::NJAG, a_NJAG);
  p->SetField(MfData::Packages::Disu::IVSD, a_IVSD);
  p->SetField(MfData::Packages::Disu::IDSYMRD, a_IDSYMRD);
  if (!exists)
    MfData::Get().AddPackage(&pack);

} // MfData::Packages::Disu1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::Disu2 (const int *a_NODLAY)
{
  MfPackage pack(MfData::Packages::DISU);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::DISU));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  // we need to copy the data because modflow changes the values
  // after they are read
  int nLay = MfGlobal::Get().NumLay();
  int* nodlay = util::NewIntArray(nLay);
  for (int i=0; i<nLay; i++)
  {
    nodlay[i] = a_NODLAY[i];
  }

  p->SetField(MfData::Packages::Disu::NODLAY, nodlay);
  if (!exists)
    MfData::Get().AddPackage(&pack);
} // MfData::Packages::Disu2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::Disu3 (const Real* a_PERLEN,
                              const int* a_NSTP,
                              const Real* a_TSMULT,
                              const int* a_ISSFLG)
{
  MfPackage pack(MfData::Packages::DISU);
  MfPackage *p(MfData::Get().GetPackage(MfData::Packages::DISU));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::Disu::PERLEN, a_PERLEN);
  p->SetField(MfData::Packages::Disu::NSTP, a_NSTP);
  p->SetField(MfData::Packages::Disu::TSMULT, a_TSMULT);
  p->SetField(MfData::Packages::Disu::ISSFLG, a_ISSFLG);
  if (!exists)
    MfData::Get().AddPackage(&pack);
  MfData::Get().Export(MfData::Packages::DISU);
} // MfData::Packages::Disu3
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DIS (discretization)
/// package.
//------------------------------------------------------------------------------
void MfData::Packages::DisPackage (const Real *a_DELR,
                                   const Real *a_DELC,
                                   const int *a_NBTOM,
                                   const Real *a_BOTM,
                                   const Real *a_PERLEN,
                                   const int *a_NSTP,
                                   const Real *a_TSMULT,
                                   const int *a_ISSFLG)
{
  MfPackage pack(MfData::Packages::DIS);
  // we have to copy the LAYCBD because MODFLOW changes the values before
  // we write the file
  pack.SetField(MfData::Packages::DisPack::LAYCBD, MfGlobal::Get().LayCbd());
  pack.SetField(MfData::Packages::DisPack::DELR, a_DELR);
  pack.SetField(MfData::Packages::DisPack::DELC, a_DELC);
  pack.SetField(MfData::Packages::DisPack::NBTOM, a_NBTOM);
  pack.SetField(MfData::Packages::DisPack::BOTM, a_BOTM);
  pack.SetField(MfData::Packages::DisPack::PERLEN, a_PERLEN);
  pack.SetField(MfData::Packages::DisPack::NSTP, a_NSTP);
  pack.SetField(MfData::Packages::DisPack::TSMULT, a_TSMULT);
  pack.SetField(MfData::Packages::DisPack::ISSFLG, a_ISSFLG);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(MfData::Packages::DIS);
} // MfData::Packages::DisPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the list based packages
//------------------------------------------------------------------------------
void MfData::Packages::ListPackage (const char * const a_type,
                                    const int *a_ITMP,
                                    const int *a_MAXBC,
                                    const int *a_NUMBC,
                                    const int *a_NUMFIELDS,
                                    const int *a_NAUX,
                                    const Real *a_DATA,
                                    const int *a_NP,
                                    const char *a_AUX)
{
  MfPackage pack(a_type);
  MfPackage *p(MfData::Get().GetPackage(a_type));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::ListPack::ITMP, a_ITMP);
  p->SetField(MfData::Packages::ListPack::MAXBC, a_MAXBC);
  p->SetField(MfData::Packages::ListPack::NUMBC, a_NUMBC);
  p->SetField(MfData::Packages::ListPack::NUMFIELDS, a_NUMFIELDS);
  p->SetField(MfData::Packages::ListPack::NAUX, a_NAUX);
  p->SetField(MfData::Packages::ListPack::DATA, a_DATA);
  p->SetField(MfData::Packages::ListPack::NP, a_NP);
  p->SetField(MfData::Packages::ListPack::AUX, a_AUX);
  if (!exists)
    MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::ListPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SIP packages
//------------------------------------------------------------------------------
void MfData::Packages::SipPackage (const char*  const a_type,
                                   const int* a_MXITER,
                                   const int* a_NPARM,
                                   const Real* a_ACCL,
                                   const Real* a_HCLOSE,
                                   const int* a_IPCALC,
                                   const Real* a_WSEED,
                                   const int* a_IPRSIP)
{
  MfPackage pack(a_type);
  pack.SetField(MfData::Packages::SipPack::MXITER, a_MXITER);
  pack.SetField(MfData::Packages::SipPack::NPARM, a_NPARM);
  pack.SetField(MfData::Packages::SipPack::ACCL, a_ACCL);
  pack.SetField(MfData::Packages::SipPack::HCLOSE, a_HCLOSE);
  pack.SetField(MfData::Packages::SipPack::IPCALC, a_IPCALC);
  pack.SetField(MfData::Packages::SipPack::WSEED, a_WSEED);
  pack.SetField(MfData::Packages::SipPack::IPRSIP, a_IPRSIP);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::SipPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DE4 packages
//------------------------------------------------------------------------------
void MfData::Packages::De4Line1 (const int* a_ITMX,
                                 const int* a_MXUP,
                                 const int* a_MXLOW,
                                 const int* a_MXBW)
{
  MfPackage pack(MfData::Packages::DE4Line1);
  pack.SetField(MfData::Packages::De4Pack::ITMX, a_ITMX);
  pack.SetField(MfData::Packages::De4Pack::MXUP, a_MXUP);
  pack.SetField(MfData::Packages::De4Pack::MXLOW, a_MXLOW);
  pack.SetField(MfData::Packages::De4Pack::MXBW, a_MXBW);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(MfData::Packages::DE4Line1);
} // MfData::Packages::De4Line1
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the DE4 packages
//------------------------------------------------------------------------------
void MfData::Packages::De4Line2 (const int* a_IFREQ,
                                 const int* a_MUTD4,
                                 const Real* a_ACCL,
                                 const Real* a_HCLOSE,
                                 const int* a_IPRD4)
{
  MfPackage pack(MfData::Packages::DE4Line2);
  pack.SetField(MfData::Packages::De4Pack::IFREQ, a_IFREQ);
  pack.SetField(MfData::Packages::De4Pack::MUTD4, a_MUTD4);
  pack.SetField(MfData::Packages::De4Pack::ACCL, a_ACCL);
  pack.SetField(MfData::Packages::De4Pack::HCLOSE, a_HCLOSE);
  pack.SetField(MfData::Packages::De4Pack::IPRD4, a_IPRD4);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(MfData::Packages::DE4Line2);
} // MfData::Packages::De4Line2
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SOR package
//------------------------------------------------------------------------------
void MfData::Packages::SorPackage (const char*  const a_type,
                                   const int* a_MXITER,
                                   const Real* a_ACCL,
                                   const Real* a_HCLOSE,
                                   const int* a_IPRSOR)
{
  MfPackage pack(a_type);
  pack.SetField(MfData::Packages::SorPack::MXITER, a_MXITER);
  pack.SetField(MfData::Packages::SorPack::ACCL, a_ACCL);
  pack.SetField(MfData::Packages::SorPack::HCLOSE, a_HCLOSE);
  pack.SetField(MfData::Packages::SorPack::IPRSOR, a_IPRSOR);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::SorPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the PCG package
//------------------------------------------------------------------------------
void MfData::Packages::PcgPackage (const char*  const a_type,
                                   const int* a_MXITER,
                                   const int* a_ITER1,
                                   const int* a_NPCOND,
                                   const Real* a_HCLOSE,
                                   const Real* a_RCLOSE,
                                   const Real* a_RELAX,
                                   const int* a_NBPOL,
                                   const int* a_IPRPCG,
                                   const int* a_MUTPCG,
                                   const Real* a_DAMP)
{
  MfPackage pack(a_type);
  MfPackage *p(MfData::Get().GetPackage(a_type));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::PcgPack::MXITER, a_MXITER);
  p->SetField(MfData::Packages::PcgPack::ITER1, a_ITER1);
  p->SetField(MfData::Packages::PcgPack::NPCOND, a_NPCOND);
  p->SetField(MfData::Packages::PcgPack::HCLOSE, a_HCLOSE);
  p->SetField(MfData::Packages::PcgPack::RCLOSE, a_RCLOSE);
  p->SetField(MfData::Packages::PcgPack::RELAX, a_RELAX);
  p->SetField(MfData::Packages::PcgPack::NBPOL, a_NBPOL);
  p->SetField(MfData::Packages::PcgPack::IPRPCG, a_IPRPCG);
  p->SetField(MfData::Packages::PcgPack::MUTPCG, a_MUTPCG);
  p->SetField(MfData::Packages::PcgPack::DAMP, a_DAMP);
  if (!exists)
    MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::PcgPackage
//------------------------------------------------------------------------------
/// \brief SMS Solver stuff common to both xMD and PCGU.
//------------------------------------------------------------------------------
void MfData::Packages::SmsPackage (const int* IFDPARAM,
                                   const double* HCLOSE,
                                   const double* HICLOSE,
                                   const int* MXITER,
                                   const int* ITER1,
                                   const int* IPRSMS,
                                   const int* NONMETH,
                                   const int* LINMETH,
                                   const double* THETA,
                                   const double* AKAPPA,
                                   const double* GAMMA,
                                   const double* AMOMENTUM,
                                   const int* NUMTRACK,
                                   const double* BTOL,
                                   const double* BREDUC,
                                   const double* RESLIM)
{
  const char* const type = MfData::Packages::SMS;
  MfPackage pack(type);
  MfPackage *p(MfData::Get().GetPackage(type));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::SmsPack::IFDPARAM, IFDPARAM);
  p->SetField(MfData::Packages::SmsPack::HCLOSE, HCLOSE);
  p->SetField(MfData::Packages::SmsPack::HICLOSE, HICLOSE);
  p->SetField(MfData::Packages::SmsPack::MXITER, MXITER);
  p->SetField(MfData::Packages::SmsPack::ITER1, ITER1);
  p->SetField(MfData::Packages::SmsPack::IPRSMS, IPRSMS);
  p->SetField(MfData::Packages::SmsPack::NONLINMETH, NONMETH);
  p->SetField(MfData::Packages::SmsPack::LINMETH, LINMETH);
  p->SetField(MfData::Packages::SmsPack::THETA, THETA);
  p->SetField(MfData::Packages::SmsPack::AKAPPA, AKAPPA);
  p->SetField(MfData::Packages::SmsPack::GAMMA, GAMMA);
  p->SetField(MfData::Packages::SmsPack::AMOMENTUM, AMOMENTUM);
  p->SetField(MfData::Packages::SmsPack::NUMTRACK, NUMTRACK);
  p->SetField(MfData::Packages::SmsPack::BTOL, BTOL);
  p->SetField(MfData::Packages::SmsPack::BREDUC, BREDUC);
  p->SetField(MfData::Packages::SmsPack::RESLIM, RESLIM);
  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::SmsPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SmsXmdPackage (const int* IACL,
                                      const int* NORDER,
                                      const int* LEVEL,
                                      const int* NORTH,
                                      const int* IREDSYS,
                                      const double* RRCTOL,
                                      const int* IDROPTOL,
                                      const double* EPSRN)
{
  const char* const type = MfData::Packages::SMS;
  MfPackage pack(type);
  MfPackage *p(MfData::Get().GetPackage(type));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::SmsPack::IACL, IACL);
  p->SetField(MfData::Packages::SmsPack::NORDER, NORDER);
  p->SetField(MfData::Packages::SmsPack::LEVEL, LEVEL);
  p->SetField(MfData::Packages::SmsPack::NORTH, NORTH);
  p->SetField(MfData::Packages::SmsPack::IREDSYS, IREDSYS);
  p->SetField(MfData::Packages::SmsPack::RRCTOL, RRCTOL);
  p->SetField(MfData::Packages::SmsPack::IDROPTOL, IDROPTOL);
  p->SetField(MfData::Packages::SmsPack::EPSRN, EPSRN);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(type);
} // MfData::Packages::SmsXmdPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SmsPcguPackage (const int* IPC,
                                       const int* ISCL,
                                       const int* IORD,
                                       const Real* RCLOSEPCGU,
                                       const int* IFLAG)
{
  const char* const type = MfData::Packages::SMS;
  MfPackage pack(type);
  MfPackage *p(MfData::Get().GetPackage(type));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::SmsPack::IPC, IPC);
  p->SetField(MfData::Packages::SmsPack::ISCL, ISCL);
  p->SetField(MfData::Packages::SmsPack::IORD, IORD);
  p->SetField(MfData::Packages::SmsPack::RCLOSEPCGU, RCLOSEPCGU);
  p->SetField(MfData::Packages::SmsPack::IFLAG, IFLAG);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(type);
} // MfData::Packages::SmsPcguPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the LMG package
//------------------------------------------------------------------------------
void MfData::Packages::LmgPackage (const char* const a_type,
                                   const Real* a_STOR1,
                                   const Real* a_STOR2,
                                   const Real* a_STOR3,
                                   const int* a_ICG,
                                   const int* a_MXITER,
                                   const int* a_MXCYC,
                                   const Real* a_BCLOSE,
                                   const Real* a_DAMP,
                                   const int* a_IOUTAMG,
                                   const Real* a_DUP,
                                   const Real* a_DLOW,
                                   const Real* a_HCLOSE,
                                   const int*  a_CONTROL)
{
  MfPackage pack(a_type);
  pack.SetField(MfData::Packages::LmgPack::STOR1, a_STOR1);
  pack.SetField(MfData::Packages::LmgPack::STOR2, a_STOR2);
  pack.SetField(MfData::Packages::LmgPack::STOR3, a_STOR3);
  pack.SetField(MfData::Packages::LmgPack::ICG, a_ICG);
  pack.SetField(MfData::Packages::LmgPack::MXITER, a_MXITER);
  pack.SetField(MfData::Packages::LmgPack::MXCYC, a_MXCYC);
  pack.SetField(MfData::Packages::LmgPack::BCLOSE, a_BCLOSE);
  pack.SetField(MfData::Packages::LmgPack::DAMP, a_DAMP);
  pack.SetField(MfData::Packages::LmgPack::IOUTAMG, a_IOUTAMG);
  pack.SetField(MfData::Packages::LmgPack::DUP, a_DUP);
  pack.SetField(MfData::Packages::LmgPack::DLOW, a_DLOW);
  pack.SetField(MfData::Packages::LmgPack::HCLOSE, a_HCLOSE);
  pack.SetField(MfData::Packages::LmgPack::CONTROL, a_CONTROL);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::LmgPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the GMG package
//------------------------------------------------------------------------------
void MfData::Packages::GmgPackage (const char* const a_type,
                                   const Real* a_RCLOSE,
                                   const int* a_IITER,
                                   const Real* a_HCLOSE,
                                   const int* a_MXITER,
                                   const Real* a_DAMP,
                                   const int* a_IADAMP,
                                   const int* a_IOUTGMG,
                                   const int* a_ISM,
                                   const int* a_ISC,
                                   const double* a_RELAX)
{
  MfPackage pack(a_type);
  pack.SetField(MfData::Packages::GmgPack::RCLOSE, a_RCLOSE);
  pack.SetField(MfData::Packages::GmgPack::IITER, a_IITER);
  pack.SetField(MfData::Packages::GmgPack::HCLOSE, a_HCLOSE);
  pack.SetField(MfData::Packages::GmgPack::MXITER, a_MXITER);
  pack.SetField(MfData::Packages::GmgPack::DAMP, a_DAMP);
  pack.SetField(MfData::Packages::GmgPack::IADAMP, a_IADAMP);
  pack.SetField(MfData::Packages::GmgPack::IOUTGMG, a_IOUTGMG);
  pack.SetField(MfData::Packages::GmgPack::ISM, a_ISM);
  pack.SetField(MfData::Packages::GmgPack::ISC, a_ISC);
  pack.SetField(MfData::Packages::GmgPack::RELAX, a_RELAX);
  MfData::Get().AddPackage(&pack);
  MfData::Get().Export(a_type);
} // MfData::Packages::GmgPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to a 2D array
//------------------------------------------------------------------------------
namespace MfData
{
  namespace Packages
  {
    template <class T, class U>
static void iArray2D (const char * const a_name,
                          const T *a_data,
                          const U *a_multiplier,
                          const int *a_LAY,
                          const int *a_IPRN)
    {
      MfPackage *p(MfData::Get().GetPackage(a_name));
      MfPackage pack(a_name);
      bool exists(p ? 1 : 0);

      if (!exists)
        p = &pack;

      p->SetField(MfData::Packages::Array::ARRAY, a_data);
      p->SetField(MfData::Packages::Array::MULT, a_multiplier);
      p->SetField(MfData::Packages::Array::LAYER, a_LAY);
      p->SetField(MfData::Packages::Array::IPRN, a_IPRN);

      if (!exists)
        MfData::Get().AddPackage(p);
      MfData::Get().Export(p->PackageName());
    } // iArray2D
  }
}
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to a 2D array
//------------------------------------------------------------------------------
void MfData::Packages::Array2D (const char * const a_name,
                                const Real *a_data,
                                const Real *a_multiplier,
                                const int *a_LAY,
                                const int *a_IPRN)
{
  iArray2D(a_name, a_data, a_multiplier, a_LAY, a_IPRN);
} // MfData::Packages::Array2D
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to a 2D array
//------------------------------------------------------------------------------
void MfData::Packages::Array2D8 (const char * const a_name,
                                 const double *a_data,
                                 const Real *a_multiplier,
                                 const int *a_LAY,
                                 const int *a_IPRN)
{
  iArray2D(a_name, a_data, a_multiplier, a_LAY, a_IPRN);
} // MfData::Packages::Array2D8
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to a 2D array
//------------------------------------------------------------------------------
void MfData::Packages::Array2D (const char * const a_name,
                                const int *a_data,
                                const int *a_multiplier,
                                const int *a_LAY,
                                const int *a_IPRN)
{
  iArray2D(a_name, a_data, a_multiplier, a_LAY, a_IPRN);
} // MfData::Packages::Array2D
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the EVT and ETS packages
//------------------------------------------------------------------------------
void MfData::Packages::ETPackage (const char *a_PACK,
                                   const int *a_NEVTOP,
                                   const int *a_INSURF,
                                   const int *a_INEVTR,
                                   const int *a_INEXDP,
                                   const int *a_INIEVT,
                                   const int *a_NETSEG,
                                   const int *a_INSGDF)
{
  MfPackage pack(a_PACK);
  MfPackage *p(MfData::Get().GetPackage(a_PACK));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(MfData::Packages::EVTpack::NEVTOP, a_NEVTOP);
  p->SetField(MfData::Packages::EVTpack::INSURF, a_INSURF);
  p->SetField(MfData::Packages::EVTpack::INEVTR, a_INEVTR);
  p->SetField(MfData::Packages::EVTpack::INEXDP, a_INEXDP);
  p->SetField(MfData::Packages::EVTpack::INIEVT, a_INIEVT);
  p->SetField(MfData::Packages::EVTpack::NETSEG, a_NETSEG);
  p->SetField(MfData::Packages::EVTpack::INSGDF, a_INSGDF);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::ETPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the RCH package
//------------------------------------------------------------------------------
void MfData::Packages::RCHPackage (const int *a_NRCHOP,
                                   const int *a_INRECH,
                                   const int *a_INIRCH)
{
  MfPackage pack(RCH);
  MfPackage *p(MfData::Get().GetPackage(RCH));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(MfData::Packages::RCHpack::NRCHOP, a_NRCHOP);
  p->SetField(MfData::Packages::RCHpack::INRECH, a_INRECH);
  p->SetField(MfData::Packages::RCHpack::INIRCH, a_INIRCH);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::RCHPackage
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the GNC package
/// \param a_N1: The first dimension of the array.
/// \param a_N2: The second dimension of the array.
//------------------------------------------------------------------------------
void MfData::Packages::GNCPackage1 (const int *a_NPGNCn,
                                   const int *a_MXGNn,
                                   const int *a_NGNCNPn,
                                   const int *a_MXADJn,
                                   const int *a_I2Kn,
                                   const int *a_ISYMGNCn,
                                   const int *a_IFLALPHAn,
                                   const int *a_IPRGNCn,
                                   const int *a_N1,
                                   const int *a_N2,
                                   const Real *a_GNCn)
{
  MfPackage pack(GNC);
  MfPackage *p(MfData::Get().GetPackage(GNC));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(MfData::Packages::GNCpack::NPGNCn, a_NPGNCn);
  p->SetField(MfData::Packages::GNCpack::MXGNn, a_MXGNn);
  p->SetField(MfData::Packages::GNCpack::NGNCNPn, a_NGNCNPn);
  p->SetField(MfData::Packages::GNCpack::MXADJn, a_MXADJn);
  p->SetField(MfData::Packages::GNCpack::I2Kn, a_I2Kn);
  p->SetField(MfData::Packages::GNCpack::ISYMGNCn, a_ISYMGNCn);
  p->SetField(MfData::Packages::GNCpack::IFLALPHAn, a_IFLALPHAn);
  p->SetField(MfData::Packages::GNCpack::IPRGNCn, a_IPRGNCn);
  p->SetField(MfData::Packages::GNCpack::N1, a_N1);
  p->SetField(MfData::Packages::GNCpack::N2, a_N2);
  p->SetField(MfData::Packages::GNCpack::GNCn, a_GNCn);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::GNCPackage1
//------------------------------------------------------------------------------
/// \brief This receives the data that belongs to the SWI package
/// \param a_N1: The first dimension of the array.
/// \param a_N2: The second dimension of the array.
//------------------------------------------------------------------------------
void MfData::Packages::SwiPack (const int *a_NSRF,
                                const int *a_ISTRAT,
                                const int *a_NOBS,
                                const int *a_ISWIZT,
                                const int *a_ISWIBD,
                                const int *a_ISWIOBS,
                                const int *a_iadptflg,
                                const int *a_NSOLVER,
                                const int *a_IPRSOL,
                                const int *a_MUTSOL,
                                const int *a_MXITER,
                                const int *a_ITER1,
                                const int *a_NPCOND,
                                const Real *a_ZCLOSE,
                                const Real *a_RCLOSE,
                                const Real *a_RELAX,
                                const int *a_NBPOL,
                                const Real *a_DAMP,
                                const Real *a_DAMPT,
                                const Real *a_TOESLOPE,
                                const Real *a_TIPSLOPE,
                                const Real *a_ALPHA,
                                const Real *a_BETA,
                                const int *a_NADPTMX,
                                const int *a_NADPTMN,
                                const Real *a_ADPTFCT,
                                const char* a_obsname,
                                const int *a_obsk,
                                const int *a_obsi,
                                const int *a_obsj)
{
  MfPackage pack(SWI);
  MfPackage *p(MfData::Get().GetPackage(SWI));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(MfData::Packages::Swi::NSRF, a_NSRF);
  p->SetField(MfData::Packages::Swi::ISTRAT, a_ISTRAT);
  p->SetField(MfData::Packages::Swi::NOBS, a_NOBS);
  p->SetField(MfData::Packages::Swi::ISWIZT, a_ISWIZT);
  p->SetField(MfData::Packages::Swi::ISWIBD, a_ISWIBD);
  p->SetField(MfData::Packages::Swi::ISWIOBS, a_ISWIOBS);
  p->SetField(MfData::Packages::Swi::iadptflg, a_iadptflg);
  p->SetField(MfData::Packages::Swi::NSOLVER, a_NSOLVER);
  p->SetField(MfData::Packages::Swi::IPRSOL, a_IPRSOL);
  p->SetField(MfData::Packages::Swi::MUTSOL, a_MUTSOL);
  p->SetField(MfData::Packages::Swi::MXITER, a_MXITER);
  p->SetField(MfData::Packages::Swi::ITER1, a_ITER1);
  p->SetField(MfData::Packages::Swi::NPCOND, a_NPCOND);
  p->SetField(MfData::Packages::Swi::ZCLOSE, a_ZCLOSE);
  p->SetField(MfData::Packages::Swi::RCLOSE, a_RCLOSE);
  p->SetField(MfData::Packages::Swi::RELAX, a_RELAX);
  p->SetField(MfData::Packages::Swi::NBPOL, a_NBPOL);
  p->SetField(MfData::Packages::Swi::DAMP, a_DAMP);
  p->SetField(MfData::Packages::Swi::DAMPT, a_DAMPT);
  p->SetField(MfData::Packages::Swi::TOESLOPE, a_TOESLOPE);
  p->SetField(MfData::Packages::Swi::TIPSLOPE, a_TIPSLOPE);
  p->SetField(MfData::Packages::Swi::ALPHA, a_ALPHA);
  p->SetField(MfData::Packages::Swi::BETA, a_BETA);
  p->SetField(MfData::Packages::Swi::NADPTMX, a_NADPTMX);
  p->SetField(MfData::Packages::Swi::NADPTMN, a_NADPTMN);
  p->SetField(MfData::Packages::Swi::ADPTFCT, a_ADPTFCT);
  p->SetField(MfData::Packages::Swi::OBSNAME, a_obsname);
  p->SetField(MfData::Packages::Swi::OBSK, a_obsk);
  p->SetField(MfData::Packages::Swi::OBSI, a_obsi);
  p->SetField(MfData::Packages::Swi::OBSJ, a_obsj);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::SwiPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::Head (const int* a_iper,
                             const int* a_ncol,
                             const int* a_nrow,
                             const int* a_nlay,
                             const Real* a_head)
{
  MfPackage pack(HED);
  MfPackage *p(MfData::Get().GetPackage(HED));
  bool exists(p ? 1 : 0);
  if (!exists)
  {
    p = &pack;
  }

  p->SetField(MfData::Packages::HeadPack::IPER, a_iper);
  p->SetField(MfData::Packages::HeadPack::NCOL, a_ncol);
  p->SetField(MfData::Packages::HeadPack::NROW, a_nrow);
  p->SetField(MfData::Packages::HeadPack::NLAY, a_nlay);
  p->SetField(MfData::Packages::HeadPack::HEAD, a_head);

  if (!exists)
  {
    MfData::Get().AddPackage(p);
  }
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::Head
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SingleValIntToPack (const char *a_pckg,
                                           const char *a_name,
                                           const int *a_flag)
{
  MfPackage pack(a_pckg);
  MfPackage *p(MfData::Get().GetPackage(a_pckg));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(a_name, a_flag);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::SingleValIntToPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SingleValFltToPack (const char *a_pckg,
                                           const char *a_name,
                                           const Real *a_flag)
{
  MfPackage pack(a_pckg);
  MfPackage *p(MfData::Get().GetPackage(a_pckg));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(a_name, a_flag);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::SingleValFltToPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SingleValDblToPack (const char *a_pckg,
                                           const char *a_name,
                                           const double *a_flag)
{
  MfPackage pack(a_pckg);
  MfPackage *p(MfData::Get().GetPackage(a_pckg));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(a_name, a_flag);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::SingleValDblToPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SingleValStrToPack (const char *a_pckg,
                                           const char *a_name,
                                           const char *a_flag)
{
  MfPackage pack(a_pckg);
  MfPackage *p(MfData::Get().GetPackage(a_pckg));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(a_name, a_flag);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::SingleValStrToPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::LPF1Package (const int *NLAY,
                                    const int *ILPFCB,
                                    const Real *HDRY,
                                    const int *LAYTYP,
                                    const int *LAYAVG,
                                    const Real *CHANI,
                                    const int *LAYVKA,
                                    const int *LAYWET,
                                    const int *VERTLEAKFLAG,
                                    const int *MF2K5)
{
  MfPackage pack(LPF);
  MfPackage *p(MfData::Get().GetPackage(LPF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  // we need to copy the data in the LAYTYP, CHANI, & LAYWET because
  // modflow changes the values after they are read
  int nLay(*NLAY);
  int* laytyp = util::NewIntArray(nLay);
  int* laywet = util::NewIntArray(nLay);
  Real* chani = util::NewRealArray(nLay);
  for (int i=0; i<nLay; i++)
  {
    laytyp[i] = LAYTYP[i];
    chani[i] = CHANI[i];
    laywet[i] = LAYWET[i];
  }

  p->SetField(MfData::Packages::LPFpack::CHANI, chani);
  p->SetField(MfData::Packages::LPFpack::HDRY, HDRY);
  p->SetField(MfData::Packages::LPFpack::ILPFCB, ILPFCB);
  p->SetField(MfData::Packages::LPFpack::LAYAVG, LAYAVG);
  p->SetField(MfData::Packages::LPFpack::LAYTYP, laytyp);
  p->SetField(MfData::Packages::LPFpack::LAYVKA, LAYVKA);
  p->SetField(MfData::Packages::LPFpack::LAYWET, laywet);
  p->SetField(MfData::Packages::LPFpack::VERTLEAKFLAG, VERTLEAKFLAG);
  p->SetField(MfData::Packages::LPFpack::MF2K5, MF2K5);
  p->SetField(MfData::Packages::LPFpack::NLAY, NLAY);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::LPF1Package
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::LPF_OPT (const char* PCK,
                                const int* ISFAC,
                                const int* ICONCV,
                                const int* ITHFLG,
                                const int* NOCVCO,
                                const int* NOVFC)
{
  MfPackage pack(PCK);
  MfPackage *p(MfData::Get().GetPackage(PCK));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::LPFpack::ISFAC, ISFAC);
  p->SetField(MfData::Packages::LPFpack::ICONCV, ICONCV);
  p->SetField(MfData::Packages::LPFpack::ITHFLG, ITHFLG);
  p->SetField(MfData::Packages::LPFpack::NOCVCO, NOCVCO);
  p->SetField(MfData::Packages::LPFpack::NOVFC, NOVFC);
  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::LPF_OPT
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::BCF1Package (const int *NLAY,
                                    const int *IBCFCB,
                                    const Real *HDRY,
                                    const int *IWDFLG,
                                    const Real *WETFCT,
                                    const int *IWETIT,
                                    const int *IHDWET,
                                    const int *LAYCON,
                                    const int *LAYAVG)
{
  MfPackage pack(BCF);
  MfPackage *p(MfData::Get().GetPackage(BCF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::BCFpack::NLAY, NLAY);
  p->SetField(MfData::Packages::BCFpack::IBCFCB, IBCFCB);
  p->SetField(MfData::Packages::BCFpack::HDRY, HDRY);
  p->SetField(MfData::Packages::BCFpack::IWDFLG, IWDFLG);
  p->SetField(MfData::Packages::BCFpack::WETFCT, WETFCT);
  p->SetField(MfData::Packages::BCFpack::IWETIT, IWETIT);
  p->SetField(MfData::Packages::BCFpack::IHDWET, IHDWET);
  p->SetField(MfData::Packages::BCFpack::LAYCON, LAYCON);
  p->SetField(MfData::Packages::BCFpack::LAYAVG, LAYAVG);
  if (!exists)
    MfData::Get().AddPackage(p);
  //MfData::Get().Export(p->PackageName());
} // MfData::Packages::BCF1Package
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::HFBPackage (const int *NHFBNP,
                                   const Real *HFBf)
{
  MfPackage pack(HFB);
  MfPackage *p(MfData::Get().GetPackage(HFB));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::HFBpack::NHFBNP, NHFBNP);
  p->SetField(MfData::Packages::HFBpack::HFB, HFBf);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::HFBPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SENPackage (const int *ISENALL,
                                   const int *IUHEAD,
                                   const int *IPRINTS,
                                   const int *ISENSU,
                                   const int *ISENPU,
                                   const int *ISENFM)
{
  MfPackage pack(SEN);
  MfPackage *p(MfData::Get().GetPackage(SEN));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::SENpack::ISENALL, ISENALL);
  p->SetField(MfData::Packages::SENpack::IUHEAD, IUHEAD);
  p->SetField(MfData::Packages::SENpack::IPRINTS, IPRINTS);
  p->SetField(MfData::Packages::SENpack::ISENSU, ISENSU);
  p->SetField(MfData::Packages::SENpack::ISENPU, ISENPU);
  p->SetField(MfData::Packages::SENpack::ISENFM, ISENFM);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::SENPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::PVALPackage (const char *PARNAM,
                                    const Real *B,
                                    const int *NPVAL)
{
  MfPackage pack(PVAL);
  MfPackage *p(MfData::Get().GetPackage(PVAL));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::PVALpack::PARNAM, PARNAM);
  p->SetField(MfData::Packages::PVALpack::B, B);
  p->SetField(MfData::Packages::PVALpack::NPVAL, NPVAL);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::PVALPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::PESPackage (const int *ITMXP,
                                   const Real *DMAX,
                                   const Real *RTOL,
                                   const Real *SOSC,
                                   const int *IBEFLG,
                                   const int *IYCFLG,
                                   const int *IOSTAR,
                                   const int *NOPT,
                                   const int *NFIT,
                                   const Real *SOSR,
                                   const Real *RMAR,
                                   const Real *RMARM,
                                   const int *IAP,
                                   const int *IPRC,
                                   const int *IPRINT,
                                   const int *LPRINT,
                                   const Real *CSA,
                                   const Real *FCONV,
                                   const int *LASTX)
{
  MfPackage pack(PES);
  MfPackage *p(MfData::Get().GetPackage(PES));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  p->SetField(MfData::Packages::PESpack::ITMXP, ITMXP);
  p->SetField(MfData::Packages::PESpack::DMAX, DMAX);
  p->SetField(MfData::Packages::PESpack::RTOL, RTOL);
  p->SetField(MfData::Packages::PESpack::SOSC, SOSC);
  p->SetField(MfData::Packages::PESpack::IBEFLG, IBEFLG);
  p->SetField(MfData::Packages::PESpack::IYCFLG, IYCFLG);
  p->SetField(MfData::Packages::PESpack::IOSTAR, IOSTAR);
  p->SetField(MfData::Packages::PESpack::NOPT, NOPT);
  p->SetField(MfData::Packages::PESpack::NFIT, NFIT);
  p->SetField(MfData::Packages::PESpack::SOSR, SOSR);
  p->SetField(MfData::Packages::PESpack::RMAR, RMAR);
  p->SetField(MfData::Packages::PESpack::RMARM, RMARM);
  p->SetField(MfData::Packages::PESpack::IAP, IAP);
  p->SetField(MfData::Packages::PESpack::IPRC, IPRC);
  p->SetField(MfData::Packages::PESpack::IPRINT, IPRINT);
  p->SetField(MfData::Packages::PESpack::LPRINT, LPRINT);
  p->SetField(MfData::Packages::PESpack::CSA, CSA);
  p->SetField(MfData::Packages::PESpack::FCONV, FCONV);
  p->SetField(MfData::Packages::PESpack::LASTX, LASTX);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::PESPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ObsHdPackage (const char *OBSNAME,
                                     const int *LAYER,
                                     const int *ROW,
                                     const int *COL,
                                     const int *IREFSP,
                                     const Real *TOFFSET,
                                     const Real *ROFF,
                                     const Real *COFF,
                                     const Real *HOBS,
                                     const Real *STAT,
                                     const int *STATFLG,
                                     const int *PLOT)
{
  MfPackage pack(HOB);
  MfPackage *p(MfData::Get().GetPackage(HOB));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  HdObs ob;
  ob.m_ITT = 1;
  ob.m_name = OBSNAME;
  ob.m_cOff = *COFF;
  ob.m_rOff = *ROFF;
  ob.m_col = *COL;
  ob.m_row = *ROW;
  if (*LAYER > 0)
  {
    ObLay lay;
    lay.m_lay = *LAYER;
    lay.m_factor = 1.0;
    ob.m_vLay.push_back(lay);
  }
  if (*IREFSP > 0)
  {
    ObTime time;
    time.m_hob = *HOBS;
    time.m_plot = *PLOT;
    time.m_statdd = 1.0;
    time.m_statH = *STAT;
    time.m_statFlag = *STATFLG;
    time.m_tOff = *TOFFSET;
    time.m_name = OBSNAME;
    time.m_iRefSp = *IREFSP;
    ob.m_vTimes.push_back(time);
  }

  GetHOB().push_back(ob);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::ObsHdPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ObsHd4 (const int *MLAY,
                               const Real *PR,
                               const int *ML,
                               const int *NL,
                               const int *MAXM)
{
  if (GetHOB().empty())
    return;

  ObLay lay;
  int start = (*ML-1) * (*MAXM);
  for (int i=start; i<*NL+start; i++)
  {
    lay.m_factor = PR[i];
    lay.m_lay = MLAY[i];
    GetHOB().back().m_vLay.push_back(lay);
  }
} // MfData::Packages::ObsHd4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ObsHd5 (const int *ITT)
{
  if (GetHOB().empty())
    return;
  GetHOB().back().m_ITT = *ITT;
} // MfData::Packages::ObsHd5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ObsHd6 (const char *OBSNAME,
                               const int *IREFSP,
                               const Real *TOFFSET,
                               const Real *HOBS,
                               const Real *STATH,
                               const Real *STATDD,
                               const int *STATFLG,
                               const int *PLOT)
{
  if (GetHOB().empty())
    return;
  ObTime t;
  t.m_hob = *HOBS;
  t.m_iRefSp = *IREFSP;
  t.m_tOff = *TOFFSET;
  t.m_statH = *STATH;
  t.m_statFlag = *STATFLG;
  t.m_statdd = *STATDD;
  t.m_plot = *PLOT;
  t.m_name = OBSNAME;

  GetHOB().back().m_vTimes.push_back(t);
} // MfData::Packages::ObsHd5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::FloObs4 (const char *OBTYPE,
                                const char *OBSNAME,
                                const int *IREFSP,
                                const Real *TOFFSET,
                                const Real *HOB,
                                const Real *STAT,
                                const int *STATFLG,
                                const int *PLOT)
{
  MfPackage pack(FOB);
  MfPackage *p(MfData::Get().GetPackage(FOB));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  Flob ob;
  ob.m_factorId = GetFLOB().m_currFactorId;
  ob.m_HOB = *HOB;
  ob.m_IREFSP = *IREFSP;
  ob.m_name = OBSNAME;
  ob.m_PLOT = *PLOT;
  ob.m_STAT = *STAT;
  ob.m_STATFLG = *STATFLG;
  ob.m_TOFFSET = *TOFFSET;
  ob.m_type = OBTYPE;
  GetFLOB().m_flob.push_back(ob);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::FloObs4
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::FloObs4_5 (const char *OBTYPE,
                                  const char *OBSNAME,
                                  const int *IREFSP,
                                  const Real *TOFFSET,
                                  const Real *FLWOBS)
{
  MfPackage pack(FOB);
  MfPackage *p(MfData::Get().GetPackage(FOB));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  Flob ob;
  ob.m_factorId = GetFLOB().m_currFactorId;
  ob.m_HOB = *FLWOBS;
  ob.m_IREFSP = *IREFSP;
  ob.m_name = OBSNAME;
  ob.m_PLOT = -1;
  ob.m_STAT = 0.0;
  ob.m_STATFLG = -1;
  ob.m_TOFFSET = *TOFFSET;
  ob.m_type = OBTYPE;
  GetFLOB().m_flob.push_back(ob);

  if (!exists)
    MfData::Get().AddPackage(p);
} // MfData::Packages::FloObs4_5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::FloObs5 (const int *NUM,
                                const int *START,
                                const Real *QCLS)
{
  FlobFact fact;
  fact.m_factorId = GetFLOB().m_currFactorId;
  int num = 5;
  if (MfData::Get().ModelType() != MF2K) num = 4;
  int start(*START-1);
  int end(*NUM+start);
  for (int i=start; i<end; i++)
  {
    fact.m_k = static_cast<int>(QCLS[(i*num)+0]);
    fact.m_i = static_cast<int>(QCLS[(i*num)+1]);
    fact.m_j = static_cast<int>(QCLS[(i*num)+2]);
    fact.m_factor = QCLS[(i*num)+3];
    GetFLOB().m_fact.push_back(fact);
  }
  GetFLOB().m_currFactorId++;
} // MfData::Packages::FloObs5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SENParList (const int *NPLIST,
                                   const char *PARNAM,
                                   const int *ISENS,
                                   const int *LN,
                                   const Real *B,
                                   const Real *BL,
                                   const Real *BU,
                                   const Real *BSCAL)
{
  MfPackage pack(SEN1);
  MfPackage *p(MfData::Get().GetPackage(SEN1));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::SEN1pack::NPLIST, NPLIST);
  p->SetField(Packages::SEN1pack::PARNAM, PARNAM);
  p->SetField(Packages::SEN1pack::ISENS, ISENS);
  p->SetField(Packages::SEN1pack::LN, LN);
  p->SetField(Packages::SEN1pack::B, B);
  p->SetField(Packages::SEN1pack::BL, BL);
  p->SetField(Packages::SEN1pack::BU, BU);
  p->SetField(Packages::SEN1pack::BSCAL, BSCAL);

  int nPar(*NPLIST);
  if (nPar < 1)
    return;
  // add any parameters that don't exist in the parameter list
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return;

  Param par;
  CStr name;
  int  i, j, cnt(0);
  char c[11];
  c[10] = '\0';
  for (i=0; i<nPar; i++)
  {
    for (j=0; j<10; j++)
      c[j] = PARNAM[cnt++];
    name = c;
    name.Trim();
    list->FindByName(name, &par);
    if (!list->FindByName(name, &par))
    {
      par = Param();
      par.m_name = name;

      // find a key value
      bool done(false);
      Param p1;
      for (j=10000; j<100000 && !done; j++)
      {
        if (!list->FindByKey(-j,&p1))
        {
          par.m_key = -j;
          done = true;
        }
      }

      list->PushBack(&par);
    }
    par.m_isens = ISENS[i];
    par.m_logTrans = LN[i] > 0 ? 1 : 0;
    par.m_value = B[i];
    par.m_b = B[i];
    par.m_b_set = true;
    par.m_min = BL[i];
    par.m_max = BU[i];
    par.m_bscal = BSCAL[i];
    list->UpdateParameter(&par);
  }
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::SENParList
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::LAKSPPackage(const int *NSOL,
                                    const Real *STAGES,
                                    const Real *SSMN,
                                    const Real *SSMX,
                                    const Real *CLAKE,
                                    const int *ITMP,
                                    const int *ITMP1,
                                    const int *LWRT,
                                    const int *LKARR,
                                    const Real *BDLKNC,
                                    const int *NSLMS,
                                    const int *IC,
                                    const int *ISUB,
                                    const Real *SILLVT,
                                    const double *PRCPLK,
                                    const double *EVAPLK,
                                    const Real *RNF,
                                    const Real *WTHDRW,
                                    const Real *CPPT,
                                    const Real *CRNF,
                                    const Real *CAUG)
{
  MfPackage pack(LAKSP);
  MfPackage *p(MfData::Get().GetPackage(LAKSP));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::LAKSPpack::NSOL, NSOL);
  p->SetField(Packages::LAKSPpack::STAGES, STAGES);
  p->SetField(Packages::LAKSPpack::SSMN, SSMN);
  p->SetField(Packages::LAKSPpack::SSMX, SSMX);
  p->SetField(Packages::LAKSPpack::CLAKE, CLAKE);
  p->SetField(Packages::LAKSPpack::ITMP, ITMP);
  p->SetField(Packages::LAKSPpack::ITMP1, ITMP1);
  p->SetField(Packages::LAKSPpack::LWRT, LWRT);
  p->SetField(Packages::LAKSPpack::LKARR, LKARR);
  p->SetField(Packages::LAKSPpack::BDLKNC, BDLKNC);
  p->SetField(Packages::LAKSPpack::NSLMS, NSLMS);
  p->SetField(Packages::LAKSPpack::IC, IC);
  p->SetField(Packages::LAKSPpack::ISUB, ISUB);
  p->SetField(Packages::LAKSPpack::SILLVT, SILLVT);
  p->SetField(Packages::LAKSPpack::PRCPLK, PRCPLK);
  p->SetField(Packages::LAKSPpack::EVAPLK, EVAPLK);
  p->SetField(Packages::LAKSPpack::RNF, RNF);
  p->SetField(Packages::LAKSPpack::WTHDRW, WTHDRW);
  p->SetField(Packages::LAKSPpack::CPPT, CPPT);
  p->SetField(Packages::LAKSPpack::CRNF, CRNF);
  p->SetField(Packages::LAKSPpack::CAUG, CAUG);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::LAKSPPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::STRPackage(const int *ITMP,
                                    const int *IRDFLG,
                                    const int *IPTFLG,
                                    const Real *STRM,
                                    const int *ISTRM,
                                    const int *NSTREM,
                                    const int *MXSTRM,
                                    const int *ITRBAR,
                                    const int *IDIVAR)
{
  MfPackage pack(STRSP);
  MfPackage *p(MfData::Get().GetPackage(STRSP));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::STRpack::ITMP, ITMP);
  p->SetField(Packages::STRpack::IRDFLG, IRDFLG);
  p->SetField(Packages::STRpack::IPTFLG, IPTFLG);
  p->SetField(Packages::STRpack::STRM, STRM);
  p->SetField(Packages::STRpack::ISTRM, ISTRM);
  p->SetField(Packages::STRpack::NSTREM, NSTREM);
  p->SetField(Packages::STRpack::MXSTRM, MXSTRM);
  p->SetField(Packages::STRpack::ITRBAR, ITRBAR);
  p->SetField(Packages::STRpack::IDIVAR, IDIVAR);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::STRPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SFRPackageLine2(const int *ISTRM,
                                       int NISTRMD,
                                       const Real *STRM,
                                       int NSTRMD)
{
  MfPackage pack(SFRLine2);
  MfPackage *p(MfData::Get().GetPackage(SFRLine2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::SFRpack::ISTRM, ISTRM);
  p->SetField(Packages::SFRpack::NISTRMD, &NISTRMD);
  p->SetField(Packages::SFRpack::STRM, STRM);
  p->SetField(Packages::SFRpack::NSTRMD, &NSTRMD);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::SFRPackageLine2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SFRPackageLine6(const int *ISEG,
                                       const int *IOTSG,
                                       const int *IDIVAR,
                                       const Real *SEG,
                                       const Real *XSEC,
                                       const Real *QSTAGE)
{
  MfPackage pack(SFRLine6);
  MfPackage *p(MfData::Get().GetPackage(SFRLine6));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::SFRpack::ISEG, ISEG);
  p->SetField(Packages::SFRpack::IOTSG, IOTSG);
  p->SetField(Packages::SFRpack::IDIVAR, IDIVAR);
  p->SetField(Packages::SFRpack::SEG, SEG);
  p->SetField(Packages::SFRpack::XSEC, XSEC);
  p->SetField(Packages::SFRpack::QSTAGE, QSTAGE);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::SFRPackageLine6
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::MnwPackageSetup(const int *MXWEL2,
                                       const int *IWL2CB,
                                       const int *IWELPT,
                                       const int *KSPREF,
                                       const double *PLoss, // double precision in MODFLOW
                                       const int *IOWELL2,
                                       const int* NOMOITER,
                                       const char* FTAG,
                                       const char* PREFIX,
                                       const char* NAMES)
{
  MfPackage pack(MNWSetup);
  MfPackage *p(MfData::Get().GetPackage(MNWSetup));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::MNWpack::MXWEL2, MXWEL2);
  p->SetField(Packages::MNWpack::IWL2CB, IWL2CB);
  p->SetField(Packages::MNWpack::IWELPT, IWELPT);
  p->SetField(Packages::MNWpack::KSPREF, KSPREF);
  p->SetField(Packages::MNWpack::PLoss, PLoss);
  p->SetField(Packages::MNWpack::IOWELL2, IOWELL2);
  p->SetField(Packages::MNWpack::NOMOITER, NOMOITER);
  p->SetField(Packages::MNWpack::FTAG, FTAG);
  p->SetField(Packages::MNWpack::PREFIX, PREFIX);
  p->SetField(Packages::MNWpack::NAMES, NAMES);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::MnwPackageSetup
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::MnwPackageStressPeriod(const int *ITMP,
                                              const int *NWELL2,
                                              const double *WELL2, // double precision in MODFLOW
                                              const char *MNWSITE,
                                              const double *MNWFLGS)
{
  MfPackage pack(MNWStressPeriod);
  MfPackage *p(MfData::Get().GetPackage(MNWStressPeriod));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(Packages::MNWpack::ITMP, ITMP);
  p->SetField(Packages::MNWpack::NWELL2, NWELL2);
  p->SetField(Packages::MNWpack::WELL2, WELL2);
  p->SetField(Packages::MNWpack::MNWSITE, MNWSITE);
  p->SetField(Packages::MNWpack::MNWFLGS, MNWFLGS);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::MnwPackageStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool GetPar (const char *PNAME,
                    const char *PTYPE,
                    Param &par)
{
  // see if the parameter exists
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return false;

  if (ParamList::IsPilotParName(PNAME, PTYPE))
    return false;

  bool found = list->FindByName(PNAME, &par);
  if (found)
  {
    par.m_type = PTYPE;
    list->UpdateParameter(&par);
  }
  return found;
} // GetPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool GetPilotPar (const char *PNAME,
                         const char *PTYPE,
                         Param &par)
{
  // see if the parameter exists
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return false;

  if (!ParamList::IsPilotParName(PNAME, PTYPE))
    return false;

  bool found = list->FindFromPilotName(PNAME, PTYPE, &par);
  if (found)
  {
    par.m_type = PTYPE;
    list->UpdateParameter(&par);
  }
  return found;
} // GetPilotPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool ParamWithPackageValue (const char *PNAME,
                                  const char *PTYPE,
                                  const Real *PVAL,
                                  Param &par)
{
  // see if the parameter exists
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return false;

  if (GetPar(PNAME, PTYPE, par))
  {
    par.m_parVal = *PVAL;
    list->UpdateParameter(&par);
  }
  else if (!ParamList::IsPilotParName(PNAME, PTYPE))
  {
    par.m_name = PNAME;
    par.m_type = PTYPE;
    par.m_parVal = *PVAL;
    if (!par.m_b_set)
      par.m_value = *PVAL;

    bool done(false);
    Param p1;
    double minParKey = -list->MinParamKey();
    int start = 10000;
    if ((int)minParKey >= start) start = (int)(minParKey + 1);
    for (int j=start; j<100000 && !done; j++)
    {
      if (!list->FindByKey(-j, &p1))
      {
        par.m_key = -j;
        done = true;
      }
    }
    list->PushBack(&par);
  }
  else
  {
    return false;
  }
  return true;
}
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void UpdatePar (Param& par)
{
  // see if the parameter exists
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (list != NULL)
    list->UpdateParameter(&par);
}
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ListPar (const char *PNAME,
                                const char *PTYPE,
                                Real *PVAL)
{
  Param par;
  ParamWithPackageValue(PNAME,PTYPE,PVAL,par);

  // change the PVAL to the key value
  *PVAL = static_cast<Real>(par.m_key);
} // MfData::Packages::ListPar
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ListParData (const char *PNAME,
                                    const char *PTYPE,
                                    const int *START,
                                    const int *LSTDIM,
                                    const int *NBC,
                                    const int *NVALS,
                                    const int *NAUX,
                                    const Real *BCDATA,
                                    const char *AUXNMS)
{
  using namespace MfData::Packages;
  MfPackage pack(LPRM);
  MfPackage *p(MfData::Get().GetPackage(LPRM));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(ListParameter::PNAME, PNAME);
  p->SetField(ListParameter::PTYPE, PTYPE);
  p->SetField(ListParameter::START, START);
  p->SetField(ListParameter::LSTDIM, LSTDIM);
  p->SetField(ListParameter::NUMBC, NBC);
  p->SetField(ListParameter::NUMFIELDS, NVALS);
  p->SetField(ListParameter::NAUX, NAUX);
  p->SetField(ListParameter::DATA, BCDATA);
  p->SetField(ListParameter::AUX, AUXNMS);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::ListParData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::StreamParInfo (const Real *PVAL,
                                      const int *START,
                                      const int *NUMBC)
{
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return;

  double key(static_cast<double>(*PVAL));
  Param par;
  if (!list->FindByKey(key, &par))
    return;
  par.m_str_start = *START;
  par.m_str_nbc = *NUMBC;
  list->UpdateParameter(&par);
} // MfData::Packages::StreamParInfo
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ArrayPar (bool a_hufPar,
                                 const char *PNAME,
                                 const char *PTYPE,
                                 Real *PVAL,
                                 const int *NP,
                                 const int *IPLOC,
                                 const int *IPCLST,
                                 const char *MLTNAM,
                                 const char *ZONNAM,
                                 const char *INAME)
{
  bool pilot(false);
  // see if the parameter exists
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list)
    return;
  list->SetPilotPtVal(PNAME,static_cast<double>(*PVAL),0);
  Param par;

  if (ParamList::IsPilotParName(PNAME, PTYPE))
  {
    pilot = true;
    if (!list->FindFromPilotName(PNAME, PTYPE, &par))
      return;
    // Check if the parameter has any clusters. If so we are done.
    if (!par.m_clust.empty())
      return;
  }
  else if (!ParamWithPackageValue(PNAME,PTYPE,PVAL,par))
    return;
  // we now need to store the cluster information
  int i, j, k, idx, multArrIdx, zoneArrIdx, lastIzIdx, inst;
  int start, stop, nClst, nInst, inIdx;
  char c[11] = {0};

  start = IPLOC[((*NP-1)*4)+0]-1;
  stop  = IPLOC[((*NP-1)*4)+1];
  nInst = IPLOC[((*NP-1)*4)+2];
  inIdx = IPLOC[((*NP-1)*4)+3]-1;
  nClst = stop - start;
  if (nInst > 0)
    nClst = static_cast<int>(nClst/nInst);

  idx = start * 14;

  inst = nInst>0 ? nInst : 1;
  for (k=0; k<inst; k++)
  {
    for (i=0; i<nClst; i++)
    {
      PClust clust;

      if (!a_hufPar)
        clust.m_lay = IPCLST[idx+0]; // or idx to HGUNAME
      else
      {
        int hIdx = IPCLST[idx+0];
        for (j=0; j<10; j++)
        {
          c[j] = INAME[((hIdx-1)*10)+j];
        }
        clust.m_hgu = c;
        clust.m_hgu.Trim();
      }
      multArrIdx = IPCLST[idx+1];
      zoneArrIdx = IPCLST[idx+2];
      lastIzIdx = IPCLST[idx+3];
      for (j=4; j<lastIzIdx; j++)
      {
        clust.m_iz.push_back(IPCLST[idx+j]);
      }

      if (multArrIdx > 0)
      {
        for (j=0; j<10; j++)
        {
          c[j] = MLTNAM[(multArrIdx-1)*10+j];
        }
      }
      else
        strcpy(c, "NONE");
      clust.m_mlt = c;
      clust.m_mlt.Trim();

      if (zoneArrIdx > 0)
      {
        for (j=0; j<10; j++)
        {
          c[j] = ZONNAM[(zoneArrIdx-1)*10+j];
        }
      }
      else
        strcpy(c, "ALL");
      clust.m_zon = c;
      clust.m_zon.Trim();

      if (pilot) // fix for pilot points
        clust.m_mlt = clust.m_zon;

      par.m_clust.push_back(clust);
      idx += 14;
    }

    if (nInst > 0)
    {
      for (j=0; j<10; j++)
      {
        c[j] = INAME[(inIdx+k)*10+j];
      }
      par.m_instNames.push_back(c);
      par.m_instNames.back().Trim();
    }
  }
  list->UpdateParameter(&par);
} // MfData::Packages::ArrayPar
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::ArrayParUsed (const char *PNAME,
                                     const char *PTYPE,
                                     const char *INAME)
{
  Param par;
  bool pilotParameter = false;
  bool regularParameter = false;
  if (GetPar(PNAME,PTYPE,par))
    regularParameter = true;
  else if (GetPilotPar(PNAME,PTYPE,par))
    pilotParameter = true;

  if (pilotParameter || regularParameter)
  {
    // get the current stress period
    int sp(MfData::Get().GetCurrentPeriod());

    // get instance name
    CStr iname;
    if (regularParameter)
      iname = INAME;
    else
      iname = par.m_name;
    if (iname.IsEmpty())
      iname = PNAME;
    iname.ToLower();

    // add instance name to stress period
    if (par.m_instStress.find(iname) == par.m_instStress.end())
      par.m_instStress.insert(std::make_pair(iname, std::vector<int>()));
    par.m_instStress[iname].push_back(sp);
    UpdatePar(par);
  }
} // MfData::Packages::ArrayParUsed
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::PilotPointData (int a_scatIdx,
                                       int a_nPts,
                                       int a_nWts,
                                       const Real *a_weights,
                                       const int *a_indices)
{
  using namespace MfData::Packages::PilotPoints;
  MfPackage pack(PPT);
  MfPackage *p(MfData::Get().GetPackage(PPT));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(SCIDX, &a_scatIdx);
  p->SetField(NPTS, &a_nPts);
  p->SetField(NWTS, &a_nWts);
  p->SetField(IDX, a_indices);
  p->SetField(WTS, a_weights);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::PilotPointData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::NameFileItem (const char *FTYPE,
                                     const char *FNAME,
                                     const int *NIU)
{
  using namespace MfData::Packages;
  MfPackage pack(NAM);
  MfPackage *p(MfData::Get().GetPackage(NAM));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(NameFile::FTYPE, FTYPE);
  p->SetField(NameFile::FNAME, FNAME);
  p->SetField(NameFile::NIU, NIU);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::NameFileItem
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::Gage (const int* IGGLST, const int *NUMGAGE)
{
  using namespace MfData::Packages;
  MfPackage pack(GAGE);
  MfPackage *p(MfData::Get().GetPackage(GAGE));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(GAGpack::IGGLST, IGGLST);
  p->SetField(GAGpack::NUMGAGE, NUMGAGE);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::Gage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::NameFileFilename (const char *FNAME)
{
  using namespace MfData::Packages;
  MfPackage pack("NAM1");
  MfPackage *p(MfData::Get().GetPackage("NAM1"));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(NameFile::FNAME, FNAME);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
} // MfData::Packages::NameFileFilename
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::GetParamKeyAndDataStart (MfData::MfPackage* a_package,
                                                Real &a_key,
                                                int &a_start)
{
  using namespace MfData::Packages;
  a_start = -1;
  a_key = 0;

  const int *s(0);
  const char *nm(0), *typ(0);
  if (!a_package->GetField(ListParameter::START, &s) || !s ||
      !a_package->GetField(ListParameter::PNAME, &nm) || !nm ||
      !a_package->GetField(ListParameter::PTYPE, &typ) || !typ)
    return false;

  CStr name(nm), type(typ);
  Param p;
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list || !list->FindByName(name, &p))
    return false;
  // make sure it is the right type
  if (type.CompareNoCase(p.m_type) != 0)
    return false;

  a_key = static_cast<Real>(p.m_key);
  a_start = *s - 1;
  return true;
} // MfData::Packages::GetParamKeyAndDataStart
//------------------------------------------------------------------------------
/// \brief Gets the well data from the package. I did it this way so
/// I could test this function.
//------------------------------------------------------------------------------
bool MfData::Packages::GetBcData (MfData::MfPackage* a_package,
                                  const char *a_packName,
                                  const int **a_nBcs,
                                  int *a_nFields,
                                  const int **a_nAux,
                                  const Real **a_data,
                                  const int **a_dataFields,
                                  std::vector<CStr> &a_names)
{
  const char          *auxNames(0);
  int                  nAux(0);
  char                 tmpAux[17];
  std::vector<CStr>    bcFieldNames;

  a_package->GetField(MfData::Packages::ListPack::NUMBC, a_nBcs);
  a_package->GetField(MfData::Packages::ListPack::NUMFIELDS, a_dataFields);
  a_package->GetField(MfData::Packages::ListPack::DATA, a_data);
  a_package->GetField(MfData::Packages::ListPack::NAUX, a_nAux);
  a_package->GetField(MfData::Packages::ListPack::AUX, &auxNames);
  if (*a_nAux) nAux = **a_nAux;
  if (!a_nBcs || !a_dataFields || !a_data/* || !a_nAux || !auxNames*/)
    return false;

  MfData::Packages::GetBcFieldNames(a_packName, bcFieldNames);
  if (a_packName == CStr("DRT") && **a_dataFields - nAux < 9)
  {
    // if using DRT without return flow then remove last four fields
    bcFieldNames.resize(2);
  }
  *a_nFields = (int)bcFieldNames.size() + 3 + nAux;

  a_names.assign(*a_nFields, CStr());
  a_names.at(0) = "k";
  a_names.at(1) = "i";
  a_names.at(2) = "j";
  for (size_t q=0; q<bcFieldNames.size(); q++)
  {
    a_names.at(3+q) = bcFieldNames.at(q);
  }

  // copy the aux variable names
  int start, i, j, cnt(0);
  tmpAux[16] = '\0';
  start = static_cast<int>(3 + bcFieldNames.size());
  for (i=0; i<nAux; i++)
  {
    for (j=0; j<16; j++)
    {
      tmpAux[j] = auxNames[cnt++];
    }
    a_names.at(start+i) = tmpAux;
    a_names.at(start+i).Trim();
  }
  return true;
} // MfData::Packages::GetBcData
//------------------------------------------------------------------------------
/// \brief Gets the names of the fields for each type of bc
//------------------------------------------------------------------------------
bool MfData::Packages::GetBcFieldNames (const char * const a_type,
                                        std::vector<CStr> &bcFieldNames)
{
  bcFieldNames.resize(0);
  CStr type(a_type);
  if (type == MfData::Packages::WEL ||
      type == MfData::Packages::CLNWEL)
  {
    bcFieldNames.push_back("Q");
  }
  else if (type == MfData::Packages::RIV)
  {
    bcFieldNames.push_back("Stage");
    bcFieldNames.push_back("Cond");
    bcFieldNames.push_back("Rbot");
  }
  else if (type == MfData::Packages::DRN)
  {
    bcFieldNames.push_back("Elevation");
    bcFieldNames.push_back("Cond");
  }
  else if (type == MfData::Packages::DRT)
  {
    bcFieldNames.push_back("Elevation");
    bcFieldNames.push_back("Cond");
    bcFieldNames.push_back("LayR");
    bcFieldNames.push_back("RowR");
    bcFieldNames.push_back("ColR");
    bcFieldNames.push_back("Rfprop");
  }
  else if (type == MfData::Packages::GHB)
  {
    bcFieldNames.push_back("BHead");
    bcFieldNames.push_back("Cond");
  }
  else if (type == MfData::Packages::CHD)
  {
    bcFieldNames.push_back("SHead");
    bcFieldNames.push_back("EHead");
  }
  else if (type == MfData::Packages::HFB)
  {
    bcFieldNames.push_back("i2");
    bcFieldNames.push_back("j2");
    bcFieldNames.push_back("Hydchr");
  }
  else if (type == MfData::Packages::PCB)
  {
    bcFieldNames.push_back("Species_No");
    bcFieldNames.push_back("Conc");
  }
  else
    return false;
  return true;
} // MfData::Packages::GetBcFieldNames
//------------------------------------------------------------------------------
/// \brief Gets the package name from the type of parameter
//------------------------------------------------------------------------------
bool MfData::Packages::GetPackNameFromParameter (MfData::MfPackage* a_package,
                                                 CStr &a_name)
{
  using namespace MfData::Packages;
  a_name = "";
  const char *ptype(0);
  if (!a_package->GetField(ListParameter::PTYPE, &ptype) || !ptype)
    return false;
  CStr p(ptype);
  if (p.CompareNoCase("DRN") == 0)
    a_name = DRN;
  else if (p.CompareNoCase("DRT") == 0)
    a_name = DRT;
  else if (p.CompareNoCase("Q") == 0)
    a_name = WEL;
  else if (p.CompareNoCase("RIV") == 0)
    a_name = RIV;
  else if (p.CompareNoCase("GHB") == 0)
    a_name = GHB;
  else if (p.CompareNoCase("CHD") == 0)
    a_name = CHD;
  else if (p.CompareNoCase("HFB") == 0)
    a_name = HFB;
  else if (p.CompareNoCase("GNC") == 0)
    a_name = GNC;
  else
    return false;
  return true;
} // MfData::Packages::GetPackNameFromParameter
//------------------------------------------------------------------------------
static bool iGetParSrcDestFields (const CStr& a_packName,
                                  std::vector<CStr>& srcFields,
                                  std::vector<CStr>& destFields,
                                  std::vector<int>& destIdxs)
{
  using namespace MfData::Packages;
  if (a_packName == DRN)
  {
    srcFields.push_back("Cond");
    destFields.push_back("Condfact");
    destIdxs.push_back(2);
  }
  else if (a_packName == DRT)
  {
    srcFields.push_back("Cond");
    destFields.push_back("Condfact");
    destIdxs.push_back(6);
  }
  else if (a_packName == WEL)
  {
    srcFields.push_back("Q");
    destFields.push_back("Qfact");
    destIdxs.push_back(1);
  }
  else if (a_packName == RIV)
  {
    srcFields.push_back("Cond");
    destFields.push_back("Condfact");
    destIdxs.push_back(3);
  }
  else if (a_packName == GHB)
  {
    srcFields.push_back("Cond");
    destFields.push_back("Condfact");
    destIdxs.push_back(2);
  }
  else if (a_packName == CHD)
  {
    srcFields.push_back("SHead");
    srcFields.push_back("EHead");
    destFields.push_back("SHdfact");
    destFields.push_back("EHdfact");
    destIdxs.push_back(2);
    destIdxs.push_back(3);
  }
  else if (a_packName == HFB)
  {
    srcFields.push_back("Hydchr");
    destFields.push_back("Factor");
    destIdxs.push_back(0); // not applicable
  }
  else
    return false;
  return true;
} // iGetParSrcDestFields
//------------------------------------------------------------------------------
/// \brief Figures out mapping between bc data fields when using parameters
//------------------------------------------------------------------------------
bool MfData::Packages::GetParamSrcDestFields (const CStr &a_packName,
                                              const std::vector<CStr> &a_fields,
                                              std::map<int, CStr> &a_srcIdx_destField)
{
  a_srcIdx_destField.clear();
  std::vector<CStr> srcFields, destFields;
  std::vector<int> destIdxs;
  if (!iGetParSrcDestFields(a_packName, srcFields, destFields, destIdxs))
    return false;

  size_t pos(0);
  for (size_t i=0; i<a_fields.size() && pos<srcFields.size(); i++)
  {
    if (a_fields[i] == srcFields[pos])
    {
      a_srcIdx_destField[static_cast<int>(i)] = destFields[pos];
      pos++;
    }
  }
  return (destFields.size() == a_srcIdx_destField.size());
} // MfData::Packages::GetParamSrcDestFields
//------------------------------------------------------------------------------
/// \brief overload 
//------------------------------------------------------------------------------
bool MfData::Packages::GetParamSrcDestFields (const CStr &a_packName,
                                              const std::vector<CStr> &a_fields,
                                              std::map<int, int> &a_srcIdx_destField)
{
  a_srcIdx_destField.clear();
  std::vector<CStr> srcFields, destFields;
  std::vector<int> destIdxs;
  if (!iGetParSrcDestFields(a_packName, srcFields, destFields, destIdxs))
    return false;

  size_t pos(0);
  for (size_t i=0; i<a_fields.size() && pos<srcFields.size(); i++)
  {
    if (a_fields[i] == srcFields[pos])
    {
      a_srcIdx_destField[static_cast<int>(i)] = destIdxs[pos];
      pos++;
    }
  }
  return (destIdxs.size() == a_srcIdx_destField.size());
} // MfData::Packages::GetParamSrcDestFields
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SaveHfbParameterData (MfPackage *a_p,
                                   std::map<CStr, std::vector<Real> >& a_hfbPar)
{
  const int*   nBcs;
  const int*   nAux;
  const int*   nDataFields;
  int          nFields;
  const Real*  data;
  Real         key;
  const char*  name;
  std::vector<CStr>   fieldStrings;
  int          start;
  if (GetBcData(a_p, "HFB", &nBcs, &nFields, &nAux, &data, &nDataFields,
                fieldStrings) &&
      GetParamKeyAndDataStart(a_p, key, start) &&
      a_p->GetField(ListParameter::PNAME, &name) && name)
  {
    std::vector<Real> dataCopy;
    for (int i = 0; i < *nBcs; ++i)
    {
      int idx = (i + start) * 7;
      for (int j = 0; j < 6; ++j)
      {
        dataCopy.push_back(data[idx+j]);
      }
    }
    a_hfbPar[name] = dataCopy;
  }
} // MfData::Packages::SaveHfbParameterData
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::HUF1 (const int *IHUFCB,
                             const Real *HDRY,
                             const int *NHUF,
                             const int *NPHUF,
                             const int *IOHUFHEADS,
                             const int *IOHUFFLOWS,
                             const int *LTHUF,
                             const int *LAYWT)
{
  using namespace MfData::Packages;
  MfPackage pack(HUF);
  MfPackage *p(MfData::Get().GetPackage(HUF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  int nlay(MfGlobal::Get().NumLay());
  int* lthuf = util::NewIntArray(nlay);
  int* laywt = util::NewIntArray(nlay);
  for (int i=0; i<nlay; i++)
  {
    lthuf[i] = LTHUF[i];
    laywt[i] = LAYWT[i];
  }


  p->SetField(HUFPack::IHUFCB, IHUFCB);
  p->SetField(HUFPack::HDRY, HDRY);
  p->SetField(HUFPack::NHUF, NHUF);
  p->SetField(HUFPack::NPHUF, NPHUF);
  p->SetField(HUFPack::IOHUFHEADS, IOHUFHEADS);
  p->SetField(HUFPack::IOHUFFLOWS, IOHUFFLOWS);
  p->SetField(HUFPack::LTHUF, lthuf);
  p->SetField(HUFPack::LAYWT, laywt);
  if (!exists)
    MfData::Get().AddPackage(p);
  return true;
} // MfData::Packages::HUF1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::HufFlag (const int *IHGUFLG)
{
  using namespace MfData::Packages;
  MfPackage pack(HUF);
  MfPackage *p(MfData::Get().GetPackage(HUF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(HUFPack::IHGUFLG, IHGUFLG);

  if (!exists)
    MfData::Get().AddPackage(p);
  return true;
} // MfData::Packages::HufFlag
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::HufWet (const Real *WETFCT,
                               const int *IWETIT,
                               const int *IHDWET)
{
  using namespace MfData::Packages;
  MfPackage pack(HUF);
  MfPackage *p(MfData::Get().GetPackage(HUF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(HUFPack::WETFCT, WETFCT);
  p->SetField(HUFPack::IWETIT, IWETIT);
  p->SetField(HUFPack::IHDWET, IHDWET);
  if (!exists)
    MfData::Get().AddPackage(p);
  return true;
} // MfData::Packages::HufWet
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::HufHani (const char *HGUNAM,
                                const Real *HGUHANI,
                                const Real *HGUVANI)
{
  using namespace MfData::Packages;
  MfPackage pack(HUF);
  MfPackage *p(MfData::Get().GetPackage(HUF));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(HUFPack::HGUNAM, HGUNAM);
  p->SetField(HUFPack::HGUHANI, HGUHANI);
  p->SetField(HUFPack::HGUVANI, HGUVANI);
  if (!exists)
    MfData::Get().AddPackage(p);
  return true;
} // MfData::Packages::HufHani
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::UZFPackageLine1 (const int *NUZTOP,
                                        const int *IUZFOPT,
                                        const int *IRUNFLG,
                                        const int *IETFLG,
                                        const int *IUZFCB1,
                                        const int *IUZFCB2,
                                        const int *NTRAIL2,
                                        const int *NSETS2,
                                        const int *NUZGAG,
                                        const Real *SURFDEP,
                                        const int *NOSURFLEAK)
{
  using namespace MfData::Packages;
  MfPackage pack(UZFLine1);
  MfPackage *p(MfData::Get().GetPackage(UZFLine1));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(UZFpack::NUZTOP, NUZTOP);
  p->SetField(UZFpack::IUZFOPT, IUZFOPT);
  p->SetField(UZFpack::IRUNFLG, IRUNFLG);
  p->SetField(UZFpack::IETFLG, IETFLG);
  p->SetField(UZFpack::IUZFCB1, IUZFCB1);
  p->SetField(UZFpack::IUZFCB2, IUZFCB2);
  p->SetField(UZFpack::NTRAIL2, NTRAIL2);
  p->SetField(UZFpack::NSETS2, NSETS2);
  p->SetField(UZFpack::NUZGAG, NUZGAG);
  p->SetField(UZFpack::SURFDEP, SURFDEP);
  p->SetField(UZFpack::NOSURFLEAK, NOSURFLEAK);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::UZFPackageLine1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::UZFPackageLine8 (const int *IUZLIST)
{
  using namespace MfData::Packages;
  MfPackage pack(UZFLine8);
  MfPackage *p(MfData::Get().GetPackage(UZFLine8));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(UZFpack::IUZLIST, IUZLIST);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::UZFPackageLine8
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::UZFPackageSP (const int *NUZF1,
                                     const int *NUZF2,
                                     const int *NUZF3,
                                     const int *NUZF4)
{
  using namespace MfData::Packages;
  MfPackage pack(UZFStressPeriod);
  MfPackage *p(MfData::Get().GetPackage(UZFStressPeriod));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(UZFpack::NUZF1, NUZF1);
  p->SetField(UZFpack::NUZF2, NUZF2);
  p->SetField(UZFpack::NUZF3, NUZF3);
  p->SetField(UZFpack::NUZF4, NUZF4);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::UZFPackageSP
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::VDFPackageLine5 (const int *MT3DRHOFLG,
                                        const int *MFNADVFD,
                                        const int *NSWTCPL,
                                        const int *IWTABLE,
                                        const Real *DENSEMIN,
                                        const Real *DENSEMAX,
                                        const Real *DNSCRIT,
                                        const Real *DENSEREF,
                                        const Real *DRHODC,
                                        const Real *DRHODPRHD,
                                        const Real *PRHDREF,
                                        const int *NSRHOEOS,
                                        const int *MTRHOSPEC,
                                        const Real *CRHOREF,
                                        const Real *FIRSTDT)
{
  using namespace MfData::Packages;
  MfPackage pack(VDFLine5);
  MfPackage *p(MfData::Get().GetPackage(VDFLine5));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(VDFpack::MT3DRHOFLG, MT3DRHOFLG);
  p->SetField(VDFpack::MFNADVFD, MFNADVFD);
  p->SetField(VDFpack::NSWTCPL, NSWTCPL);
  p->SetField(VDFpack::IWTABLE, IWTABLE);
  p->SetField(VDFpack::DENSEMIN, DENSEMIN);
  p->SetField(VDFpack::DENSEMAX, DENSEMAX);
  p->SetField(VDFpack::DNSCRIT, DNSCRIT);
  p->SetField(VDFpack::DENSEREF, DENSEREF);
  p->SetField(VDFpack::DRHODC, DRHODC);
  p->SetField(VDFpack::DRHODPRHD, DRHODPRHD);
  p->SetField(VDFpack::PRHDREF, PRHDREF);
  p->SetField(VDFpack::NSRHOEOS, NSRHOEOS);
  p->SetField(VDFpack::MTRHOSPEC, MTRHOSPEC);
  p->SetField(VDFpack::CRHOREF, CRHOREF);
  p->SetField(VDFpack::FIRSTDT, FIRSTDT);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::VDFLine5
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::VDFPackageStressPeriod (const int *INDENSE)
{
  using namespace MfData::Packages;
  MfPackage pack(VDFStressPeriod);
  MfPackage *p(MfData::Get().GetPackage(VDFStressPeriod));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(VDFpack::INDENSE, INDENSE);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::VDFStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::VSCPackageLine3 (const int *MT3DMUFLG,
                                        const Real *VISCMIN,
                                        const Real *VISCMAX,
                                        const Real *VISCREF,
                                        const Real *DMUDC,
                                        const Real *CMUREF,
                                        const int *NSMUEOS,
                                        const int *MUTEMPOPT,
                                        const int *MTMUSPEC,
                                        const int *MTMUTEMPSPEC,
                                        const Real *AMUCOEFF)
{
  using namespace MfData::Packages;
  MfPackage pack(VSCLine3);
  MfPackage *p(MfData::Get().GetPackage(VSCLine3));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(VSCpack::MT3DMUFLG, MT3DMUFLG);
  p->SetField(VSCpack::VISCMIN, VISCMIN);
  p->SetField(VSCpack::VISCMAX, VISCMAX);
  p->SetField(VSCpack::VISCREF, VISCREF);
  p->SetField(VSCpack::DMUDC, DMUDC);
  p->SetField(VSCpack::CMUREF, CMUREF);
  p->SetField(VSCpack::NSMUEOS, NSMUEOS);
  p->SetField(VSCpack::MUTEMPOPT, MUTEMPOPT);
  p->SetField(VSCpack::MTMUSPEC, MTMUSPEC);
  p->SetField(VSCpack::MTMUTEMPSPEC, MTMUTEMPSPEC);
  p->SetField(VSCpack::AMUCOEFF, AMUCOEFF);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::VSCLine3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::VSCPackageStressPeriod (const int *INVISC)
{
  using namespace MfData::Packages;
  MfPackage pack(VSCStressPeriod);
  MfPackage *p(MfData::Get().GetPackage(VSCStressPeriod));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(VSCpack::INVISC, INVISC);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::VSCStressPeriod
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::NwtLn1 (const Real* toldum,
                               const Real* ftoldum,
                               const int* Mxiter,
                               const Real* Thickdum,
                               const int* Linmeth,
                               const int* IPRNWT,
                               const int* IBOTAV,
                               const int* IFDPARAM,
                               const Real* thetadum,
                               const Real* akappadum,
                               const Real* gammadum,
                               const Real* amomentdum,
                               const int* Btrack,
                               const int* Numtrack,
                               const Real* Btoldum,
                               const Real* Breducdum,
                               const int* ICNVGFLG)
{
  using namespace MfData::Packages;
  MfPackage pack(NWT);
  MfPackage *p(MfData::Get().GetPackage(NWT));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(NWTpack::toldum, toldum);
  p->SetField(NWTpack::ftoldum, ftoldum);
  p->SetField(NWTpack::Mxiter, Mxiter);
  p->SetField(NWTpack::Thickdum, Thickdum);
  p->SetField(NWTpack::Linmeth, Linmeth);
  p->SetField(NWTpack::IPRNWT, IPRNWT);
  p->SetField(NWTpack::IBOTAV, IBOTAV);
  p->SetField(NWTpack::IFDPARAM, IFDPARAM);
  p->SetField(NWTpack::thetadum, thetadum);
  p->SetField(NWTpack::akappadum, akappadum);
  p->SetField(NWTpack::gammadum, gammadum);
  p->SetField(NWTpack::amomentdum, amomentdum);
  p->SetField(NWTpack::Btrack, Btrack);
  p->SetField(NWTpack::Numtrack, Numtrack);
  p->SetField(NWTpack::Btoldum, Btoldum);
  p->SetField(NWTpack::Breducdum, Breducdum);
  p->SetField(NWTpack::ICNVGFLG, ICNVGFLG);

  if (!exists)
    MfData::Get().AddPackage(p);
  //MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::NwtLn1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::NwtLn2 (const int* IACL,
                               const int* NORDER,
                               const int* LEVEL,
                               const int* NORTH,
                               const int* IREDSYS,
                               const Real* RRCTOLS,
                               const int* IDROPTOL,
                               const Real* EPSRNS,
                               const Real* HCLOSEXMDDUM,
                               const int* MXITERXMD)
{
  using namespace MfData::Packages;
  MfPackage pack(NWT);
  MfPackage *p(MfData::Get().GetPackage(NWT));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(NWTpack::IACL, IACL);
  p->SetField(NWTpack::NORDER, NORDER);
  p->SetField(NWTpack::LEVEL, LEVEL);
  p->SetField(NWTpack::NORTH, NORTH);
  p->SetField(NWTpack::IREDSYS, IREDSYS);
  p->SetField(NWTpack::RRCTOLS, RRCTOLS);
  p->SetField(NWTpack::IDROPTOL, IDROPTOL);
  p->SetField(NWTpack::EPSRNS, EPSRNS);
  p->SetField(NWTpack::HCLOSEXMDDUM, HCLOSEXMDDUM);
  p->SetField(NWTpack::MXITERXMD, MXITERXMD);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::NwtLn1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::NwtLn2a (const int* Maxitr_gmres,
                                const int* Ilu_method,
                                const int* Lev_fill,
                                const Real* Stop_toldum,
                                const int* Msdr)
{
  using namespace MfData::Packages;
  MfPackage pack(NWT);
  MfPackage *p(MfData::Get().GetPackage(NWT));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetField(NWTpack::Maxitr_gmres, Maxitr_gmres);
  p->SetField(NWTpack::Ilu_method, Ilu_method);
  p->SetField(NWTpack::Lev_fill, Lev_fill);
  p->SetField(NWTpack::Stop_toldum, Stop_toldum);
  p->SetField(NWTpack::Msdr, Msdr);
  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::NwtLn2a
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::UPW1 (const int* NLAY,
                             const int* IUPWCB,
                             const Real* HDRY,
                             const int* IPHDRY,
                             const int* LAYTYPUPW,
                             const int* LAYAVG,
                             const Real* CHANI,
                             const int* LAYVKAUPW,
                             const int* LAYWET)
{
  using namespace MfData::Packages;
  MfPackage pack(UPW);
  MfPackage *p(MfData::Get().GetPackage(UPW));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;
  int nLay(*NLAY);
  int* laytypupw = util::NewIntArray(nLay);
  int* laywet = util::NewIntArray(nLay);
  Real* chani = util::NewRealArray(nLay);
  for (int i=0; i<nLay; i++)
  {
    laytypupw[i] = LAYTYPUPW[i];
    chani[i] = CHANI[i];
    laywet[i] = LAYWET[i];
  }

  p->SetField(UPWpack::IUPWCB, IUPWCB);
  p->SetField(UPWpack::HDRY, HDRY);
  p->SetField(UPWpack::IPHDRY, IPHDRY);
  p->SetField(UPWpack::LAYTYPUPW, laytypupw);
  p->SetField(UPWpack::LAYAVG, LAYAVG);
  p->SetField(UPWpack::CHANI, chani);
  p->SetField(UPWpack::LAYVKAUPW, LAYVKAUPW);
  p->SetField(UPWpack::LAYWET, laywet);
  if (!exists)
    MfData::Get().AddPackage(p);
  return true;
} // MfData::Packages::UPW1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::GetSaveComments (int UNIT)
{
  std::set<int>& aSet(GetSuspendedCommentsSet());

  std::set<int>::iterator it(aSet.find(UNIT));
  return (it == aSet.end());
} // MfData::Packages::GetSaveComments
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::SetSaveComments (int UNIT,
                                        int ISAVE)
{
  std::set<int>& aSet(GetSuspendedCommentsSet());

  std::set<int>::iterator it(aSet.find(UNIT));
  if (ISAVE != 0) { // yes, save
    // Remove item from set if it exists
    if (it != aSet.end())
      aSet.erase(it);
  }
  else { // don't save
    // Add item to set if it exists
    if (it == aSet.end())
      aSet.insert(UNIT);
  }
} // MfData::Packages::SetSaveComments
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::Comment (const CStr& a_pack,
                                const CStr& a_line)
{
  std::map<CStr, CStr>& aMap(GetCommentsMap());

  std::map<CStr, CStr>::iterator it(aMap.find(a_pack));
  if (it == aMap.end())
  {
    aMap.insert(std::make_pair(a_pack, a_line));
  }
  else
  {
    it->second += "\n";
    it->second += a_line;
  }

  return true;
} // MfData::Packages::Comment
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::CommentPushFront (const CStr& a_pack,
                                         const CStr& a_line)
{
  std::map<CStr, CStr>& aMap(GetCommentsMap());

  std::map<CStr, CStr>::iterator it(aMap.find(a_pack));
  if (it == aMap.end())
  {
    aMap.insert(std::make_pair(a_pack, a_line));
  }
  else
  {
    CStr c = a_line;
    c += "\n";
    c += it->second;
    it->second = c;
  }

  return true;
} // MfData::Packages::Comment
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfData::Packages::GetComments (CStr a_pack,
                                    CStr& a_comment)
{
  a_comment = "";

  std::map<CStr, CStr>& aMap(GetCommentsMap());
  std::map<CStr, CStr>::iterator it(aMap.find(a_pack));
  if (it != aMap.end())
  {
    a_comment = it->second;
    if (MfData::MF2K >= MfData::Get().ModelType() &&
        (MfData::Packages::SEN == a_pack ||
         MfData::Packages::PES == a_pack)
       )
    { // get rid of half of the string
      a_comment = a_comment.Left(a_comment.GetLength() / 2);      
    }
  }
} // MfData::Packages::GetComments
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln1 (const int* MNWMAX,
                                 const int* IWL2CB,
                                 const int* MNWPRNT,
                                 const int* NAUX,
                                 const char* MNWAUX)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("1");
  p->SetField(MNW2pack::MNWMAX, MNWMAX);
  p->SetField(MNW2pack::IWL2CB, IWL2CB);
  p->SetField(MNW2pack::MNWPRNT, MNWPRNT);
  p->SetField(MNW2pack::NAUX, NAUX);
  p->SetField(MNW2pack::MNWAUX, MNWAUX);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2ab (const char* WELLID,
                                   const int* NNODES,
                                   const char* LOSSTYPE,
                                   const int* PUMPLOC,
                                   const int* Qlimit,
                                   const int* PPFLAG,
                                   const int* PUMPCAP)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2ab");
  p->SetField(MNW2pack::WELLID, WELLID);
  p->SetField(MNW2pack::NNODES, NNODES);
  p->SetField(MNW2pack::LOSSTYPE, LOSSTYPE);
  p->SetField(MNW2pack::PUMPLOC, PUMPLOC);
  p->SetField(MNW2pack::Qlimit, Qlimit);
  p->SetField(MNW2pack::PPFLAG, PPFLAG);
  p->SetField(MNW2pack::PUMPCAP, PUMPCAP);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2ab
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2c (const double* Rw,
                                  const double* Rskin,
                                  const double* Kskin,
                                  const double* B,
                                  const double* C,
                                  const double* P,
                                  const double* CWC,
                                  const char* LnDesc)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2c");
  p->SetField(MNW2pack::Rw, Rw);
  p->SetField(MNW2pack::Rskin, Rskin);
  p->SetField(MNW2pack::Kskin, Kskin);
  p->SetField(MNW2pack::B, B);
  p->SetField(MNW2pack::C, C);
  p->SetField(MNW2pack::P, P);
  p->SetField(MNW2pack::CWC, CWC);
  p->SetField(MNW2pack::LnDesc, LnDesc);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2c
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2d (const int* IL,
                                  const int* IR,
                                  const int* IC,
                                  const double* RwNode,
                                  const double* RskinNode,
                                  const double* KskinNode,
                                  const double* BNode,
                                  const double* CNode,
                                  const double* PNode,
                                  const double* CWCNode,
                                  const double* PP,
                                  const char* LnDesc,
                                  const double* Ztop,
                                  const double* Zbotm)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2d");
  if (IL) p->SetField(MNW2pack::IL, IL);
  else p->RemoveField(MNW2pack::IL);
  p->SetField(MNW2pack::IR, IR);
  p->SetField(MNW2pack::IC, IC);
  p->SetField(MNW2pack::RwNode, RwNode);
  p->SetField(MNW2pack::RskinNode, RskinNode);
  p->SetField(MNW2pack::KskinNode, KskinNode);
  p->SetField(MNW2pack::BNode, BNode);
  p->SetField(MNW2pack::CNode, CNode);
  p->SetField(MNW2pack::PNode, PNode);
  p->SetField(MNW2pack::CWCNode, CWCNode);
  p->SetField(MNW2pack::PP, PP);
  p->SetField(MNW2pack::LnDesc, LnDesc);
  if (Ztop) p->SetField(MNW2pack::Ztop, Ztop);
  else p->RemoveField(MNW2pack::Ztop);
  if (Zbotm) p->SetField(MNW2pack::Zbotm, Zbotm);
  else p->RemoveField(MNW2pack::Zbotm);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2d1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2e (const int* PUMPLAY,
                                  const int* PUMPROW,
                                  const int* PUMPCOL,
                                  const double* Zpump,
                                  const char* LnDesc)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2e");
  p->SetField(MNW2pack::PUMPLAY, PUMPLAY);
  p->SetField(MNW2pack::PUMPROW, PUMPROW);
  p->SetField(MNW2pack::PUMPCOL, PUMPCOL);
  p->SetField(MNW2pack::Zpump, Zpump);
  p->SetField(MNW2pack::LnDesc, LnDesc);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2e
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2f (const double* Hlim,
                                  const int* QCUT,
                                  const double* Qfrcmn,
                                  const double* Qfrcmx,
                                  const char* LnDesc)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2f");
  p->SetField(MNW2pack::Hlim, Hlim);
  p->SetField(MNW2pack::QCUT, QCUT);
  p->SetField(MNW2pack::Qfrcmn, Qfrcmn);
  p->SetField(MNW2pack::Qfrcmx, Qfrcmx);
  p->SetField(MNW2pack::LnDesc, LnDesc);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2g
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2g (const double* Hlift,
                                  const double* LIFTq0,
                                  const double* LIFTqdes,
                                  const double* HWtol,
                                  const char* LnDesc)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2g");
  p->SetField(MNW2pack::Hlift, Hlift);
  p->SetField(MNW2pack::LIFTq0, LIFTq0);
  p->SetField(MNW2pack::LIFTqdes, LIFTqdes);
  p->SetField(MNW2pack::HWtol, HWtol);
  p->SetField(MNW2pack::LnDesc, LnDesc);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2g
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln2h (const double* Liftn,
                                  const double* Qn,
                                  const char* LnDesc)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2h");
  p->SetField(MNW2pack::Liftn, Liftn);
  p->SetField(MNW2pack::Qn, Qn);
  p->SetField(MNW2pack::LnDesc, LnDesc);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2g
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNW2_Ln34 (const int* ITMP,
                                  const double* MNW2d,
                                  const int* NMNWVL,
                                  const int* MNWMAX,
                                  const int* NAUX)
{
  using namespace MfData::Packages;
  MfPackage pack(MNW2);
  MfPackage *p(MfData::Get().GetPackage(MNW2));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("34");
  p->SetField(MNW2pack::ITMP, ITMP);
  p->SetField(MNW2pack::MNW2, MNW2d);
  p->SetField(MNW2pack::NMNWVL, NMNWVL);
  p->SetField(MNW2pack::MNWMAX, MNWMAX);
  p->SetField(MNW2pack::NAUX, NAUX);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNW2_Ln2g
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNWI_Ln1 (const int* Wel1flag,
                                 const int* QSUMflag,
                                 const int* BYNDflag)
{
  using namespace MfData::Packages;
  MfPackage pack(MNWI);
  MfPackage *p(MfData::Get().GetPackage(MNWI));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("1");
  p->SetField(MNWIpack::Wel1flag, Wel1flag);
  p->SetField(MNWIpack::QSUMflag, QSUMflag);
  p->SetField(MNWIpack::BYNDflag, BYNDflag);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNWI_Ln1
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNWI_Ln2 (const int* MNWOBS)
{
  using namespace MfData::Packages;
  MfPackage pack(MNWI);
  MfPackage *p(MfData::Get().GetPackage(MNWI));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("2");
  p->SetField(MNWIpack::MNWOBS, MNWOBS);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNWI_Ln2
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNWI_Ln3 (const char* WELLID,
                                 const int* UNIT,
                                 const int* QNDflag,
                                 const int* QBHflag,
                                 const int* CONCflag)
{
  using namespace MfData::Packages;
  MfPackage pack(MNWI);
  MfPackage *p(MfData::Get().GetPackage(MNWI));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("3");
  p->SetField(MNWIpack::WELLID, WELLID);
  p->SetField(MNWIpack::UNIT, UNIT);
  p->SetField(MNWIpack::QNDflag, QNDflag);
  p->SetField(MNWIpack::QBHflag, QBHflag);
  p->SetField(MNWIpack::CONCflag, CONCflag);

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNWI_Ln3
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfData::Packages::MNWI_End ()
{
  using namespace MfData::Packages;
  MfPackage pack(MNWI);
  MfPackage *p(MfData::Get().GetPackage(MNWI));
  bool exists(p ? 1 : 0);
  if (!exists)
    p = &pack;

  p->SetLineNumber("end");

  if (!exists)
    MfData::Get().AddPackage(p);
  MfData::Get().Export(p->PackageName());
  return true;
} // MfData::Packages::MNWI_End


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <sstream>

#include <private/MfData/MfPackageUtil.t.h>

#include <private/MfLibAsserts.h>


//------------------------------------------------------------------------------
void MfPackageUtilT::testGetParamKeyAndDataStart ()
{
  using namespace MfData::Packages;
  const char *nm="mypar";
  const char *typ="DRN";
  int start, s(7);
  Real key;
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  list->Clear();
  Param par(nm,-300.0,typ);
  list->PushBack(&par);
  MfData::MfPackage p(MfData::Packages::LPRM);
  TS_ASSERT(!GetParamKeyAndDataStart(&p, key, start));
  p.SetField(ListParameter::PNAME, nm);
  p.SetField(ListParameter::PTYPE, typ);
  p.SetField(ListParameter::START, &s);
  TS_ASSERT(GetParamKeyAndDataStart(&p, key, start));
  TS_ASSERT_EQUALS(key, -300.0);
  TS_ASSERT_EQUALS(start, 6);
  list->Clear();
}
//------------------------------------------------------------------------------
void MfPackageUtilT::testGetBcFieldNames ()
{
  using namespace MfData::Packages;
  std::vector<CStr> names;
  TS_ASSERT(!GetBcFieldNames("crap", names));
  TS_ASSERT(GetBcFieldNames(MfData::Packages::WEL, names));
  TS_ASSERT(names.size() == 1);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::RIV, names));
  TS_ASSERT(names.size() == 3);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::DRN, names));
  TS_ASSERT(names.size() == 2);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::DRT, names));
  TS_ASSERT(names.size() == 6);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::GHB, names));
  TS_ASSERT(names.size() == 2);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::CHD, names));
  TS_ASSERT(names.size() == 2);
  TS_ASSERT(GetBcFieldNames(MfData::Packages::HFB, names));
  TS_ASSERT(names.size() == 3);
}
//------------------------------------------------------------------------------
void MfPackageUtilT::testGetPackNameFromParameter ()
{
  using namespace MfData::Packages;
  CStr name;
  {
    const char *c="stuff";
    MfData::MfPackage p(MfData::Packages::LPRM);
    TS_ASSERT(!GetPackNameFromParameter(&p, name));
    p.SetField(MfData::Packages::ListParameter::PTYPE, c);
    TS_ASSERT(!GetPackNameFromParameter(&p, name));
  }
  {
    const char *c[7] = {"q","drn","drt","riv","chd","ghb","hfb"};
    const char *out[7] = {WEL,DRN,DRT,RIV,CHD,GHB,HFB};
    for (int i=0; i<7; i++)
    {
      MfData::MfPackage p(MfData::Packages::LPRM);
      p.SetField(MfData::Packages::ListParameter::PTYPE, c[i]);
      TS_ASSERT(GetPackNameFromParameter(&p, name));
      TS_ASSERT_EQUALS2(name, out[i]);
    }
  }
}
//------------------------------------------------------------------------------
void MfPackageUtilT::testGetParamSrcDestFields ()
{
  using namespace MfData::Packages;
  CStr packName;
  std::vector<CStr> fields;
  std::map<int, CStr> srcIdx_destField;
  TS_ASSERT(!GetParamSrcDestFields(packName, fields, srcIdx_destField));

  packName = DRN;
  TS_ASSERT(!GetParamSrcDestFields(packName, fields, srcIdx_destField));
  fields.push_back("Cond");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS2(srcIdx_destField[0], "Condfact");

  packName = RIV;
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS2(srcIdx_destField[0], "Condfact");

  packName = GHB;
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS2(srcIdx_destField[0], "Condfact");

  packName = WEL;
  fields.push_back("Q");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS2(srcIdx_destField[1], "Qfact");

  packName = CHD;
  fields.push_back("SHead");
  fields.push_back("EHead");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 2);
  TS_ASSERT_EQUALS2(srcIdx_destField[2], "SHdfact");
  TS_ASSERT_EQUALS2(srcIdx_destField[3], "EHdfact");

  packName = HFB;
  fields.push_back("Hydchr");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS2(srcIdx_destField[4], "Factor");

  packName = DRT;
  fields.push_back("Rfprop");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
}
//------------------------------------------------------------------------------
void MfPackageUtilT::testGetParamSrcDestFields2 ()
{
  using namespace MfData::Packages;
  CStr packName;
  std::vector<CStr> fields;
  std::map<int, int> srcIdx_destField;
  TS_ASSERT(!GetParamSrcDestFields(packName, fields, srcIdx_destField));

  packName = DRN;
  TS_ASSERT(!GetParamSrcDestFields(packName, fields, srcIdx_destField));
  fields.push_back("Cond");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS(srcIdx_destField[0], 2);

  packName = RIV;
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS(srcIdx_destField[0], 3);

  packName = GHB;
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS(srcIdx_destField[0], 2);

  packName = WEL;
  fields.push_back("Q");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS(srcIdx_destField[1], 1);

  packName = CHD;
  fields.push_back("SHead");
  fields.push_back("EHead");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 2);
  TS_ASSERT_EQUALS(srcIdx_destField[2], 2);
  TS_ASSERT_EQUALS(srcIdx_destField[3], 3);

  packName = HFB;
  fields.push_back("Hydchr");
  TS_ASSERT(GetParamSrcDestFields(packName, fields, srcIdx_destField));
  TS_ASSERT(srcIdx_destField.size() == 1);
  TS_ASSERT_EQUALS(srcIdx_destField[4], 0);
}

#endif
