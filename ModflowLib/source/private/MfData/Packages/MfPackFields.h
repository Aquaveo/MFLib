//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFPACKFIELDS_H
#define MFPACKFIELDS_H

namespace MfData
{
  namespace Packages
  {
    namespace Cln
    {
      const char * const NCLN = "NCLN";
      const char * const ICLNNDS = "ICLNNDS";
      const char * const ICLNCB = "ICLNCB";
      const char * const ICLNHD = "ICLNHD";
      const char * const ICLNDD = "ICLNDD";
      const char * const ICLNIB = "ICLNIB";
      const char * const NCLNGWC = "NCLNGWC";
      const char * const NCONDUITYP = "NCONDUITYP";
      const char * const NNDCLN = "NNDCLN";
      const char * const ACLNNDSAQ = "ACLNNDSAQ";
      const char * const NCLNNDS = "NCLNNDS";
      const char * const ACLNGWCAQ = "ACLNGWCAQ";
      const char * const ACLNCOND = "ACLNCOND";
      const char * const IBOUND = "IBOUND";
      const char * const STRT = "STRT";
    }
    namespace DisPack
    {
      const char * const LAYCBD = "LAYCBD";
      const char * const DELR = "DELR";
      const char * const DELC = "DELC";
      const char * const NBTOM = "NBTOM";
      const char * const BOTM = "BOTM";
      const char * const PERLEN = "PERLEN";
      const char * const NSTP = "NSTP";
      const char * const TSMULT = "TSMULT";
      const char * const ISSFLG = "SSorTr";
    }
    namespace Disu
    {
      const char * const LAYCBD = "LAYCBD";
      const char * const NODES = "NODES";
      const char * const NJAG = "NJAG";
      const char * const IVSD = "IVSD";
      const char * const IDSYMRD = "IDSYMRD";
      const char * const NODLAY = "NODLAY";
      const char * const TOP = "TOP";
      const char * const BOT = "BOT";
      const char * const AREA = "AREA";
      const char * const IA = "IA";
      const char * const JA = "JA";
      const char * const IVC = "IVC";
      const char * const CL1 = "CL1";
      const char * const CL2 = "CL2";
      const char * const CL12 = "CL12";
      const char * const FAHL = "FAHL";
      const char * const PERLEN = "PERLEN";
      const char * const NSTP = "NSTP";
      const char * const TSMULT = "TSMULT";
      const char * const ISSFLG = "ISSFLG";
    }
    namespace ListPack
    {
      const char * const ITMP = "ITMP";
      const char * const MAXBC = "MAXBC";
      const char * const NUMBC = "NUMBC";
      const char * const NUMFIELDS = "NUMFIELDS";
      const char * const NAUX = "NAUX";
      const char * const DATA = "DATA";
      const char * const NP = "NP";
      const char * const AUX = "AUX";
    }
    namespace ListParameter
    {
      const char * const PNAME = "PNAME";
      const char * const PTYPE = "PTYPE";
      const char * const START = "START";
      const char * const LSTDIM = "LSTDIM";
      const char * const NUMBC = "NUMBC";
      const char * const NUMFIELDS = "NUMFIELDS";
      const char * const NAUX = "NAUX";
      const char * const DATA = "DATA";
      const char * const AUX = "AUX";
    }
    namespace SipPack
    {
      const char * const MXITER = "MXITER";
      const char * const NPARM = "NPARM";
      const char * const ACCL = "ACCL";
      const char * const HCLOSE = "HCLOSE";
      const char * const IPCALC = "IPCALC";
      const char * const WSEED = "WSEED";
      const char * const IPRSIP = "IPRSIP";
    }
    namespace De4Pack
    {
      const char * const ITMX = "ITMX";
      const char * const MXUP = "MXUP";
      const char * const MXLOW = "MXLOW";
      const char * const MXBW = "MXBW";
      const char * const IFREQ = "IFREQ";
      const char * const MUTD4 = "MUTD4";
      const char * const ACCL = "ACCL";
      const char * const HCLOSE = "HCLOSE";
      const char * const IPRD4 = "IPRD4";
    }
    namespace SorPack
    {
      const char * const MXITER = "MXITER";
      const char * const ACCL = "ACCL";
      const char * const HCLOSE = "HCLOSE";
      const char * const IPRSOR = "IPRSOR";
    }
    namespace PcgPack
    {
      const char * const MXITER = "MXITER";
      const char * const ITER1 = "ITER1";
      const char * const NPCOND = "NPCOND";
      const char * const HCLOSE = "HCLOSE";
      const char * const RCLOSE = "RCLOSE";
      const char * const RELAX = "RELAX";
      const char * const NBPOL = "NBPOL";
      const char * const IPRPCG = "IPRPCG";
      const char * const MUTPCG = "MUTPCG";
      const char * const DAMP = "DAMP";
    }
    namespace PcgnPack
    {
      const char * const ITER_MO = "ITER_MO";
      const char * const ITER_MI = "ITER_MI";
      const char * const CLOSE_R = "CLOSE_R";
      const char * const CLOSE_H = "CLOSE_H";
      const char * const RELAX = "RELAX";
      const char * const IFILL = "IFILL";
      const char * const UNIT_PC = "UNIT_PC";
      const char * const UNIT_TS = "UNIT_TS";
      const char * const ADAMP = "ADAMP";
      const char * const DAMP = "DAMP";
      const char * const DAMP_LB = "DAMP_LB";
      const char * const RATE_D = "RATE_D";
      const char * const CHGLIMIT = "CHGLIMIT";
      const char * const ACNVG = "ACNVG";
      const char * const CNVG_LB = "CNVG_LB";
      const char * const MCNVG = "MCNVG";
      const char * const RATE_C = "RATE_C";
      const char * const IPUNIT = "IPUNIT";
    }
    namespace LmgPack
    {
      const char * const STOR1 = "STOR1";
      const char * const STOR2 = "STOR2";
      const char * const STOR3 = "STOR3";
      const char * const ICG = "ICG";
      const char * const MXITER = "MXITER";
      const char * const MXCYC = "MXCYC";
      const char * const BCLOSE = "BCLOSE";
      const char * const DAMP = "DAMP";
      const char * const IOUTAMG = "IOUTAMG";
      const char * const DUP = "DUP";
      const char * const DLOW = "DLOW";
      const char * const HCLOSE = "HCLOSE";
      const char * const CONTROL = "CONTROL";
    }
    namespace GmgPack
    {
      const char * const RCLOSE = "RCLOSE";
      const char * const IITER = "IITER";
      const char * const HCLOSE = "HCLOSE";
      const char * const MXITER = "MXITER";
      const char * const DAMP = "DAMP";
      const char * const IADAMP = "IADAMP";
      const char * const IOUTGMG = "IOUTGMG";
      const char * const ISM = "ISM";
      const char * const ISC = "ISC";
      const char * const RELAX = "RELAX";
    }
    namespace SmsPack
    {
      const char * const IFDPARAM = "IFDPARAM";
      const char * const HCLOSE = "HCLOSE";
      const char * const HICLOSE = "HICLOSE";
      const char * const MXITER = "MXITER";
      const char * const ITER1 = "ITER1";
      const char * const IPRSMS = "IPRSMS";
      const char * const NONLINMETH = "NONLINMETH";
      const char * const LINMETH = "LINMETH";

      const char * const THETA = "THETA";
      const char * const AKAPPA = "AKAPPA";
      const char * const GAMMA = "GAMMA";
      const char * const AMOMENTUM = "AMOMENTUM";
      const char * const NUMTRACK = "NUMTRACK";
      const char * const BTOL = "BTOL";
      const char * const BREDUC = "BREDUC";
      const char * const RESLIM = "RSLIM";

      // XMD
      const char * const IACL = "IACL";
      const char * const NORDER = "NORDER";
      const char * const LEVEL = "LEVEL";
      const char * const NORTH = "NORTH";
      const char * const IREDSYS = "IREDSYS";
      const char * const RRCTOL = "RRCTOL";
      const char * const IDROPTOL = "IDROPTOL";
      const char * const EPSRN = "EPSRN";
      // PCGU
      const char * const IPC = "IPC";
      const char * const ISCL = "ISCL";
      const char * const IORD = "IORD";
      const char * const RCLOSEPCGU = "RCLOSEPCGU";
    }
    namespace Array
    {
      const char * const ARRAY = "ARRAY";
      const char * const MULT = "MULT";
      const char * const LAYER = "LAYER";
      const char * const IPRN = "IPRN";
    }
    namespace EVTpack
    {
      const char * const NEVTOP = "NEVTOP";
      const char * const INSURF = "EVT_INSURF";
      const char * const INEVTR = "EVT_INEVTR";
      const char * const INEXDP = "EVT_INEXDP";
      const char * const INIEVT = "EVT_INIEVT";
      const char * const NETSEG = "EVT_NETSEG";
      const char * const INSGDF = "EVT_INSGDF";
    }
    namespace RCHpack
    {
      const char * const NRCHOP = "NRCHOP";
      const char * const INRECH = "RCH_INRECH";
      const char * const INIRCH = "RCH_INIRCH";
    }
    namespace GNCpack
    {
      const char * const NPGNCn = "NPGNCn";
      const char * const MXGNn = "MXGNn";
      const char * const NGNCNPn = "NGNCNPn";
      const char * const MXADJn = "MXADJn";
      const char * const I2Kn = "I2Kn";
      const char * const ISYMGNCn = "ISYMGNCn";
      const char * const IFLALPHAn = "IFALPHAn";
      const char * const IPRGNCn = "IPRGNCn";
      const char * const N1 = "N1";
      const char * const N2 = "N2";
      const char * const GNCn = "GNCn";
    }
    namespace LPFpack
    {
      const char * const NLAY = "NLAY";
      const char * const ILPFCB = "ILPFCB";
      const char * const HDRY = "HDRY";
      const char * const LAYTYP = "LAYTYP";
      const char * const LAYAVG = "LAYAVG";
      const char * const CHANI = "CHANI";
      const char * const LAYVKA = "LAYVKA";
      const char * const LAYWET = "LAYWET";
      const char * const VERTLEAKFLAG = "VERTLEAKFLAG";
      const char * const MF2K5 = "MF2K5";
      const char * const ISFAC = "ISFAC";
      const char * const ICONCV = "ICONCV";
      const char * const ITHFLG = "ITHFLG";
      const char * const NOCVCO = "NOCVCO";
      const char * const NOVFC = "NOVFC";
    }
    namespace BCFpack
    {
      const char * const NLAY = "NLAY";
      const char * const IBCFCB = "IBCFCB";
      const char * const HDRY = "HDRY";
      const char * const IWDFLG = "IWDFLG";
      const char * const WETFCT = "WETFCT";
      const char * const IWETIT = "IWETIT";
      const char * const IHDWET = "IHDWET";
      const char * const LAYCON = "LAYCON";
      const char * const LAYAVG = "LAYAVG";
      const char * const TRPY = "TRPY";
    }
    namespace HeadPack
    {
      const char * const IPER = "IPER";
      const char * const NCOL = "NCOL";
      const char * const NROW = "NROW";
      const char * const NLAY = "NLAY";
      const char * const HEAD = "HEAD";
    }
    namespace BASpack
    {
      const char * const OPT = "Options";
      const char * const HNOFLO = "HNOFLO";
      const char * const HEADNG1 = "HEADNG1";
      const char * const HEADNG2 = "HEADNG2";
    }
    namespace HFBpack
    {
      const char * const NHFBNP = "NHFBNP";
      const char * const HFB = "HFB";
    }
    namespace SENpack
    {
      const char * const ISENALL = "ISENALL";
      const char * const IUHEAD = "IUHEAD";
      const char * const IPRINTS = "IPRINTS";
      const char * const ISENSU = "ISENSU";
      const char * const ISENPU = "ISENPU";
      const char * const ISENFM = "ISENFM";
      const char * const MXSEN = "MXSEN";
    }
    namespace PVALpack
    {
      const char * const PARNAM = "PARNAM";
      const char * const B = "B";
      const char * const NPVAL = "NPVAL";
    }
    namespace SEN1pack
    {
      const char * const NPLIST = "NPLIST";
      const char * const PARNAM = "PARNAM";
      const char * const ISENS = "ISENS";
      const char * const LN = "LN";
      const char * const B = "B";
      const char * const BL = "BL";
      const char * const BU = "BU";
      const char * const BSCAL = "BSCAL";
    }
    namespace PESpack
    {
      const char * const ITMXP = "MAXITER";
      const char * const DMAX = "MAXCHANGE";
      const char * const RTOL = "TOL";
      const char * const SOSC = "SOSC";
      const char * const IBEFLG = "IBEFLG";
      const char * const IYCFLG = "IYCFLG";
      const char * const IOSTAR = "IOSTAR";
      const char * const NOPT = "NOPT";
      const char * const NFIT = "NFIT";
      const char * const SOSR = "SOSR";
      const char * const RMAR = "RMAR";
      const char * const RMARM = "RMARM";
      const char * const IAP = "IAP";
      const char * const IPRC = "IPRCOV";
      const char * const IPRINT = "IPRINT";
      const char * const LPRINT = "LPRINT";
      const char * const CSA = "CSA";
      const char * const FCONV = "FCONV";
      const char * const LASTX = "LASTX";
    }
    namespace OCpack
    {
      const char * const IHEDFM = "IHEDFM";
      const char * const IDDNFM = "IDDNFM";
      const char * const IHEDUN = "IHEDUN";
      const char * const IDDNUN = "IDDNUN";
      const char * const IBDOPT = "IBDOPT";
      const char * const IAUXSV = "IAUXSV";
      const char * const CHEDFM = "CHEDFM";
      const char * const CDDNFM = "CDDNFM";
      const char * const CBOUFM = "CBOUFM";
      const char * const IBOUUN = "IBOUUN";
      const char * const LBBOSV = "LBBOSV";
    }
    namespace OCTpack
    {
      const char * const SPID = "SPID";
      const char * const TSNum = "TSNum";
      const char * const IHDDFL = "IHDDFL";
      const char * const IBUDFL = "IBUDFL";
      const char * const ICBCFL = "ICBCFL";
      const char * const Hdpr = "Hdpr";
      const char * const Ddpr = "Ddpr";
      const char * const Hdsv = "Hdsv";
      const char * const Ddsv = "Ddsv";
      const char * const Ibsv = "Ibsv";
    }
    namespace LAKpack
    {
      const char * const NLAKES = "NLAKES";
      const char * const ILKCB = "ILKCB";
      const char * const THETA = "THETA";
      const char * const NSSITR = "NSSITR";
      const char * const SSCNCR = "SSCNCR";
      const char * const ITRSS = "ITRSS";
    }
    namespace LAKSPpack
    {
      const char * const NSOL = "NSOL";
      const char * const STAGES = "STAGES";
      const char * const SSMN = "SSMN";
      const char * const SSMX = "SSMX";
      const char * const CLAKE = "CLAKE";
      const char * const ITMP = "ITMP";
      const char * const ITMP1 = "ITMP1";
      const char * const LWRT = "LWRT";
      const char * const LKARR = "LKARR";
      const char * const BDLKNC = "BDLKNC";
      const char * const NSLMS = "NSLMS";
      const char * const IC = "IC";
      const char * const ISUB = "ISUB";
      const char * const SILLVT = "SILLVT";
      const char * const PRCPLK = "PRCPLK";
      const char * const EVAPLK = "EVAPLK";
      const char * const RNF = "RNF";
      const char * const WTHDRW = "WTHDRW";
      const char * const CPPT = "CPPT";
      const char * const CRNF = "CRNF";
      const char * const CAUG = "CAUG";
    }
    namespace STRpack
    {
      const char * const MXACTS = "MXACTS";
      const char * const NSS = "NSS";
      const char * const NTRIB = "NTRIB";
      const char * const NDIV = "NDIV";
      const char * const ICALC = "ICALC";
      const char * const CONSTV = "CONST";
      const char * const ISTCB1 = "ISTCB1";
      const char * const ISTCB2 = "ISTCB2";
      const char * const ITMP = "ITMP";
      const char * const IRDFLG = "IRDFLG";
      const char * const IPTFLG = "IPTFLG";
      const char * const STRM = "STRM";
      const char * const ISTRM = "ISTRM";
      const char * const NSTREM = "NSTREM";
      const char * const MXSTRM = "MXSTRM";
      const char * const ITRBAR = "ITRBAR";
      const char * const IDIVAR = "IDIVAR";
    }
    namespace SFRpack
    {
      const char * const NSTRM = "NSTRM";
      const char * const NSS = "NSS";
      const char * const CONSTV = "CONST";
      const char * const DLEAK = "DLEAK";
      const char * const ISTCB1 = "ISTCB1";
      const char * const ISTCB2 = "ISTCB2";
      const char * const ISFROPT = "ISFROPT";
      const char * const NSTRAIL = "NSTRAIL";
      const char * const ISUZN = "ISUZN";
      const char * const NSFRSETS = "NSFRSETS";

      const char * const ISTRM = "ISTRM";
      const char * const NISTRMD = "NISTRMD";
      const char * const STRM = "STRM";
      const char * const NSTRMD = "NSTRMD";

      const char * const THTS = "THTS";
      const char * const THTI = "THTI";
      const char * const EPS  = "EPS";
      const char * const UHC  = "UHC";

      const char * const ITMP = "ITMP";
      const char * const IRDFLG = "IRDFLG";
      const char * const IPTFLG = "IPTFLG";

      const char * const NSEG = "NSEG";
      const char * const ISEG = "ISEG";
      const char * const IOTSG = "IOTSG";
      const char * const IDIVAR = "IDIVAR";
      const char * const SEG = "SEG";
      const char * const XSEC = "XSEC";
      const char * const QSTAGE = "QSTAGE";
    }
    namespace MNWpack
    {
      const char * const MXWEL2 = "MXMNW";
      const char * const IWL2CB = "IWL2CB";
      const char * const IWELPT = "IWELPT";
      const char * const KSPREF = "KSPREF";
      const char * const PLoss = "PLossMNW";
      const char * const IOWELL2 = "IOWELL2";
      const char * const NOMOITER = "NOMOITER";
      const char * const FTAG = "FTAG";
      const char * const PREFIX = "PREFIX";
      const char * const NAMES = "NAMES";
      const char * const ITMP = "ITMP";
      const char * const NWELL2 = "NWELL2";
      const char * const WELL2 = "WELL2";
      const char * const MNWSITE = "MNWsite";
      const char * const MNWFLGS = "MNWFLGS";
    }
    namespace PilotPoints
    {
      const char * const SCIDX = "SCIDX";
      const char * const NPTS = "NPTS";
      const char * const NWTS = "NWTS";
      const char * const IDX = "IDX";
      const char * const WTS = "WTS";
    }
    namespace NameFile
    {
      const char * const FTYPE = "FTYPE";
      const char * const FNAME = "FNAME";
      const char * const NIU = "NIU";
    }
    namespace GAGpack
    {
      const char * const IGGLST = "IGGLST";
      const char * const NUMGAGE = "NUMGAGE";
    }
    namespace HUFPack
    {
      const char * const IHUFCB = "IHUFCB";
      const char * const HDRY = "HDRY";
      const char * const NHUF = "NHUF";
      const char * const NPHUF = "NPHUF";
      const char * const IOHUFHEADS = "IOHUFHEADS";
      const char * const IOHUFFLOWS = "IOHUFFLOWS";
      const char * const LTHUF = "LTHUF";
      const char * const LAYWT = "LAYWT";
      const char * const IHGUFLG = "IHGUFLG";
      const char * const WETFCT = "WETFCT";
      const char * const IWETIT = "IWETIT";
      const char * const IHDWET = "IHDWET";
      const char * const HGUNAM = "HGUNAM";
      const char * const HGUHANI = "HGUHANI";
      const char * const HGUVANI = "HGUVANI";
    }
    namespace UZFpack
    {
      const char * const NUZTOP = "NUZTOP";
      const char * const IUZFOPT = "IUZFOPT";
      const char * const IRUNFLG = "IRUNFLG";
      const char * const IETFLG = "IETFLG";
      const char * const IUZFCB1 = "IUZFCB1";
      const char * const IUZFCB2 = "IUZFCB2";
      const char * const NTRAIL2 = "NTRAIL2";
      const char * const NSETS2 = "NSETS2";
      const char * const NUZGAG = "NUZGAG";
      const char * const SURFDEP = "SURFDEP";
      const char * const IUZLIST = "IUZLIST";
      const char * const NUZF1 = "NUZF1";
      const char * const NUZF2 = "NUZF2";
      const char * const NUZF3 = "NUZF3";
      const char * const NUZF4 = "NUZF4";
    }
    namespace VDFpack
    {
      const char * const MT3DRHOFLG = "MT3DRHOFLG";
      const char * const MFNADVFD = "MFNADVFD";
      const char * const NSWTCPL = "NSWTCPL";
      const char * const IWTABLE = "IWTABLE";
      const char * const DENSEMIN = "DENSEMIN";
      const char * const DENSEMAX = "DENSEMAX";
      const char * const DNSCRIT = "DNSCRIT";
      const char * const DENSEREF = "DENSEREF";
      const char * const DRHODC = "DRHODC";
      const char * const DRHODPRHD = "DRHODPRHD";
      const char * const PRHDREF = "PRHDREF";
      const char * const NSRHOEOS = "NSRHOEOS";
      const char * const MTRHOSPEC = "MTRHOSPEC";
      const char * const CRHOREF = "CRHOREF";
      const char * const FIRSTDT = "FIRSTDT";
      const char * const INDENSE = "INDENSE";
    }
    namespace VSCpack
    {
      const char * const MT3DMUFLG = "MT3DMUFLG";
      const char * const VISCMIN = "VISCMIN";
      const char * const VISCMAX = "VISCMAX";
      const char * const VISCREF = "VISCREF";
      const char * const DMUDC = "DMUDC";
      const char * const CMUREF = "CMUREF";
      const char * const NSMUEOS = "NSMUEOS";
      const char * const MUTEMPOPT = "MUTEMPOPT";
      const char * const MTMUSPEC = "MTMUSPEC";
      const char * const MTMUTEMPSPEC = "MTMUTEMPSPEC";
      const char * const AMUCOEFF = "AMUCOEFF";
      const char * const INVISC = "INVISC";
    }
    namespace NWTpack
    {
      const char * const toldum = "toldum";
      const char * const ftoldum = "ftoldum";
      const char * const Mxiter = "Mxiter";
      const char * const Thickdum = "Thickdum";
      const char * const Linmeth = "Linmeth";
      const char * const IPRNWT = "IPRNWT";
      const char * const IBOTAV = "IBOTAV";
      const char * const IFDPARAM = "IFDPARAM";
      const char * const thetadum = "thetadum";
      const char * const akappadum = "akappadum";
      const char * const gammadum = "gammadum";
      const char * const amomentdum = "amomentdum";
      const char * const Btrack = "Btrack";
      const char * const Numtrack = "Numtrack";
      const char * const Btoldum = "Btoldum";
      const char * const Breducdum = "Breducdum";
      const char * const IACL = "IACL";
      const char * const NORDER = "NORDER";
      const char * const LEVEL = "LEVEL";
      const char * const NORTH = "NORTH";
      const char * const IREDSYS = "IREDSYS";
      const char * const RRCTOLS = "RRCTOLS";
      const char * const IDROPTOL = "IDROPTOL";
      const char * const EPSRNS = "EPSRNS";
      const char * const HCLOSEXMDDUM = "HCLOSEXMDDUM";
      const char * const MXITERXMD = "MXITERXMD";
      const char * const Maxitr_gmres = "Maxitr_gmres";
      const char * const Ilu_method = "Ilu_method";
      const char * const Lev_fill = "Lev_fill";
      const char * const Stop_toldum = "Stop_toldum";
      const char * const Msdr = "Msdr";
    }
    namespace UPWpack
    {
      const char* const IUPWCB = "IUPWCD";
      const char* const HDRY = "HDRY";
      const char* const IPHDRY = "IPHDRY";
      const char* const LAYTYPUPW = "LAYTYPUPW";
      const char* const LAYAVG = "LAYAVG";
      const char* const CHANI = "CHANI";
      const char* const LAYVKAUPW = "LAYVKAUPW";
      const char* const LAYWET = "LAYWET";
    }
    namespace MNW2pack
    {
      const char* const MNWMAX = "MNWMAX";
      const char* const IWL2CB = "IWL2CB";
      const char* const MNWPRNT = "MNWPRNT";
      const char* const NAUX = "NAUX";
      const char* const MNWAUX = "MNWAUX";
      const char* const WELLID = "WELLID";
      const char* const NNODES = "NNODES";
      const char* const LOSSTYPE = "LOSSTYPE";
      const char* const PUMPLOC = "PUMPLOC";
      const char* const Qlimit = "Qlimit";
      const char* const PPFLAG = "PPFLAG";
      const char* const PUMPCAP = "PUMPCAP";
      const char* const Rw = "Rw";
      const char* const Rskin = "Rskin";
      const char* const Kskin = "Kskin";
      const char* const B = "B";
      const char* const C = "C";
      const char* const P = "P";
      const char* const CWC = "CWC";
      const char* const LnDesc = "LnDesc";
      const char* const IL = "IL";
      const char* const IR = "IR";
      const char* const IC = "IC";
      const char* const RwNode = "RwNode";
      const char* const RskinNode = "RskinNode";
      const char* const KskinNode = "KskinNode";
      const char* const BNode = "BNode";
      const char* const CNode = "CNode";
      const char* const PNode = "PNode";
      const char* const CWCNode = "CWCNode";
      const char* const PP = "PP";
      const char* const Ztop = "Ztop";
      const char* const Zbotm = "Zbotm";
      const char* const PUMPLAY = "PUMPLAY";
      const char* const PUMPROW = "PUMPROW";
      const char* const PUMPCOL = "PUMPCOL";
      const char* const Zpump = "Zpump";
      const char* const Hlim = "Hlim";
      const char* const QCUT = "QCUT";
      const char* const Qfrcmn = "Qfrcmn";
      const char* const Qfrcmx = "Qfrcmx";
      const char* const Hlift = "Hlift";
      const char* const LIFTq0 = "LIFTq0";
      const char* const LIFTqdes = "LIFTqdes";
      const char* const HWtol = "HWtol";
      const char* const Liftn = "Liftn";
      const char* const Qn = "Qn";
      const char* const ITMP = "ITMP";
      const char* const MNW2 = "MNW2";
      const char* const NMNWVL = "NMNWVL";
    }
    namespace MNWIpack
    {
      const char* const Wel1flag = "Wel1flag";
      const char* const QSUMflag = "QSUMflag";
      const char* const BYNDflag = "BYNDflag";
      const char* const MNWOBS = "MNWOBS";
      const char* const WELLID = "WELLID";
      const char* const UNIT = "UNIT";
      const char* const QNDflag = "QNDflag";
      const char* const QBHflag = "QBHflag";
      const char* const CONCflag = "CONCflag";
    }
    namespace SUBpack
    {
      const char* const ISUBCB = "ISUBCB";
      const char* const ISUBOC = "ISUBOC";
      const char* const NNDB = "NNDB";
      const char* const NDB = "NDB";
      const char* const NMZ = "NMZ";
      const char* const NN = "NN";
      const char* const AC1 = "AC1";
      const char* const AC2 = "AC2";
      const char* const ITMIN = "ITMIN";
      const char* const IDSAVE = "IDSAVE";
      const char* const IDREST = "IDREST";
      const char* const LN = "LN";
      const char* const LDN = "LDN";
      const char* const DP = "DP";
      const char* const ISBOCF = "ISBOCF";
      const char* const ISBOCU = "ISBOCU";
      const char* const ISP1 = "ISP1";
      const char* const ISP2 = "ISP2";
      const char* const JTS1 = "JTS1";
      const char* const JTS2 = "JTS2";
      const char* const IFL = "IFL";
    }
  }
}

#endif
