//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFEXPORTERIMPL_H
#define MFEXPORTERIMPL_H

#include <private\util\util.h>

class TxtExporter;
const CStr ARR_DIS_TOP   = "TOP ELEVATION OF LAYER 1";
const CStr ARR_DIS_BOT   = "MODEL LAYER BOTTOM EL.";
const CStr ARR_DIS_VCB   = "BOT. EL. OF QUASI-3D BED";
const CStr ARR_BAS_IBND  = "BOUNDARY ARRAY";
const CStr ARR_BAS_SHEAD = "INITIAL HEAD";
const CStr ARR_BCF_HY    = "BCF HYD. COND. ALONG ROWS";
const CStr ARR_BCF_TRAN  = "TRANSMIS. ALONG ROWS";
const CStr ARR_BCF_TRAN_U= "TRANSMIS. OR HYD COND";
const CStr ARR_BCF_HK_U  = "TRANSMIS. OR HYD COND";
const CStr ARR_BCF_VCONT = "VERT HYD COND /THICKNESS";
const CStr ARR_BCF_SF1   = "PRIMARY STORAGE COEF";
const CStr ARR_BCF_SF2   = "SECONDARY STORAGE COEF";
const CStr ARR_BCF_WET   = "BCF WETDRY PARAMETER";
const CStr ARR_BCF_TRPY  = "COLUMN TO ROW ANISOTROPY";

const CStr ARR_BCT_ICBUND= "ICBUND";
const CStr ARR_BCT_PRSITY= "POROSITY";
const CStr ARR_BCT_BULKD = "BULK DENSITY";
const CStr ARR_BCT_ANGLEX= "FACE ANGLE";
const CStr ARR_BCT_ADSORB= "ADSORPTION COEFFICIENT";
const CStr ARR_BCT_CONC  = "CONCENTRATION";
const CStr ARR_BCT_DL    = "LONG DISPERSIVITY";
const CStr ARR_BCT_DT    = "TRNS DISPERSIVITY";
const CStr ARR_BCT_DLX   = "X-DIR LONG DISPERSIVITY";
const CStr ARR_BCT_DLY   = "Y-DIR LONG DISPERSIVITY";
const CStr ARR_BCT_DLZ   = "Z-DIR LONG DISPERSIVITY";
const CStr ARR_BCT_DTXY  = "XY-DIR TRNS DISPERSIVITY";
const CStr ARR_BCT_DTYZ  = "YZ-DIR TRNS DISPERSIVITY";
const CStr ARR_BCT_DTXZ  = "XZ-DIR TRNS DISPERSIVITY";
const CStr ARR_BCT_FLICH = "FREUNDLICH EXPONENT";
const CStr ARR_BCT_ZODRW = "ZERO-ORDER DECAY WATER";
const CStr ARR_BCT_ZODRS = "ZERO-ORDER DECAY SOIL";
const CStr ARR_BCT_FODRW = "FIRST-ORDER DECAY WATER";
const CStr ARR_BCT_FODRS = "FIRST-ORDER DECAY SOIL";

const CStr ARR_HUF_TOP   = "TOP ELEVATN: ";
const CStr ARR_HUF_THCK  = "THICKNESS: ";
const CStr ARR_HUF_WET   = "HUF WETDRY PARAMETER";
const CStr ARR_LPF_HK    = "HYD. COND. ALONG ROWS";
const CStr ARR_LPF_HANI  = "HORIZ. ANI. (COL./ROW)";
const CStr ARR_LPF_VK    = "VERTICAL HYD. COND.";
const CStr ARR_LPF_VANI  = "HORIZ. TO VERTICAL ANI.";
const CStr ARR_LPF_SS    = "SPECIFIC STORAGE";
const CStr ARR_LPF_SY    = "SPECIFIC YIELD";
const CStr ARR_LPF_SC    = "STORAGE COEFFICIENT";
const CStr ARR_LPF_WET   = "WETDRY PARAMETER";
const CStr ARR_LPF_VKCBD = "QUASI3D VERT. HYD. COND.";
const CStr ARR_LPF_ANGX  = "FACE ANGLE";
const CStr ARR_RCH_RCH   = "RECHARGE";
const CStr ARR_RCH_LAY   = "RECHARGE LAYER INDEX";
const CStr ARR_EVT_SURF  = "ET SURFACE";
const CStr ARR_EVT_RATE  = "EVAPOTRANSPIRATION RATE";
const CStr ARR_EVT_EXT   = "EXTINCTION DEPTH";
const CStr ARR_EVT_LAY   = "ET LAYER INDEX";
const CStr ARR_ETS_SURF  = "ET SURFACE (ETSS)";
const CStr ARR_ETS_RATE  = "EVAPOTRANS. RATE (ETSR)";
const CStr ARR_ETS_EXT   = "EXTINCTION DEPTH (ETSX)";
const CStr ARR_ETS_LAY   = "ET LAYER INDEX (IETS)";
const CStr ARR_ETS_PXDP  = "EXTINCT. DEP. PROPORTION";
const CStr ARR_ETS_PETM  = "ET RATE PROPORTION";
const CStr ARR_UZF_UBND  = "AREAL EXTENT OF UZ FLOW";
const CStr ARR_UZF_RBND  = "ROUTING OVERLAND RUNOFF";
const CStr ARR_UZF_VKS   = "SATURATED VERTICAL K";
const CStr ARR_UZF_EPS   = "BROOKS-COREY EPSILON";
const CStr ARR_UZF_THTS  = "SATURATED WATER CONTENT";
const CStr ARR_UZF_THTI  = "INITIAL WATER CONTENT";
const CStr ARR_UZF_RCH   = "AREAL INFILTRATION RATE";
const CStr ARR_UZF_ET    = "ET RATE";
const CStr ARR_UZF_EXT   = "ET EXTINCTION DEPTH";
const CStr ARR_UZF_EXTWC = "EXTINCTION WATER CONTENT";
const CStr ARR_SUB_RNB   = "NUMBER OF BEDS IN SYSTEM";
const CStr ARR_SUB_HC    = "PRECONSOLIDATION HEAD";
const CStr ARR_SUB_SFE   = "ELASTIC INTERBED STORAGE";
const CStr ARR_SUB_SFV   = "VIRGIN INTERBED STORAGE";
const CStr ARR_SUB_COM   = "STARTING COMPACTION";
const CStr ARR_SUB_DSTRT = "DELAY STARTING HEAD";
const CStr ARR_SUB_DHC   = "DELAY PRECOLSOL. HEAD";
const CStr ARR_SUB_DCOM  = "DELAY INITIAL COMPACTION";
const CStr ARR_SUB_DZ    = "DELAY INTERBED THICKNESS";
const CStr ARR_SUB_NZ    = "MATERIAL ZONE INDICES";
const CStr ARR_SWI_ZETA  = "ZETA SURFACE";
const CStr ARR_SWI_SSZ  = "SSZ";
const CStr ARR_SWI_ISOURCE  = "IZONENR";
const CStr ARR_ZON       = "ZONE ARRAY:";
const CStr ARR_MLT       = "MULT. ARRAY:";
const CStr ARR_LAK_ID    = "LAKE ID ARRAY";
const CStr ARR_LAK_LEAK  = "LAKEBED LEAKANCE ARRAY";

// SEAWAT
const CStr ARR_VDF_DENS  = "FLUID DENSITY";
const CStr ARR_VDF_CONC  = "VDF CONCENTRATION";
const CStr ARR_VSC_VSC   = "FLUID VISCOSITY";
const CStr ARR_VSC_CONC  = "VSC CONCENTRATION";

namespace MfData
{
  class MfGlobal;
  class MfPackage;

  namespace Export
  {

    class MfExporterImpl
    {
    public:
      MfExporterImpl(const char *a_, bool m_compressH5=true);
      ~MfExporterImpl();

      const char* GetTypeName();

      virtual void SetFileName(const char *a_);
              void SetTablesStr(const char *a_);
      const char*  GetTablesStr();
      int GetModelType();
      void SetModelType(int a_modelType);

      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package)=0;

              bool CanExportTable(const char *a_);

      TxtExporter *GetExp() { return m_exp; }
      bool CompressH5() const { return m_compressH5; }
      void SetCompressH5(bool a_) { m_compressH5 = a_; }

      std::map<CStr, CStr> &GetMapArrays() { return m_map; }
      CStr PackageFromArrayName(CStr a_name);
      CStr ParamTypeFromArrayName(CStr a_name);
      CStr VarNameFromArrayName(CStr a_name);
      const CStr &FileName() { return m_fileName; }
      std::vector< std::vector<int> >& Ibound() { return m_ibound; }
      std::map<CStr, std::vector< std::vector<Real> > >& SavedRealArrays() { return m_rArrays; }
      std::map<CStr, std::vector<Real> >& SavedRealArraysMult() { return m_rArraysMult; }
      std::map<CStr, std::vector<int> >& SavedRealArraysJj() { return m_rArraysJj; }
      void BuildUniqueName(const CStr& a_baseName,
                           const CStr& a_extension,
                           int a_unitNumber,
                           std::set<CStr>& a_uniqueNames,
                           CStr& a_fileName);
      const std::map<CStr, CStr> GetTypes() const { return m_types; }

    protected:
      std::map<CStr, CStr> m_types;

    private:
      MfExporterImpl();
      MfExporterImpl(const MfExporterImpl &rhs);
      const MfExporterImpl& operator=(const MfExporterImpl &rhs);
      void InitExtensions();

      CStr m_typeName;
      CStr m_fileName;
      CStr m_tables;
      int  m_modelType;
      TxtExporter* m_exp;
      bool m_compressH5;
      std::vector< std::vector<int> > m_ibound;
      std::map<CStr, std::vector< std::vector<Real> > > m_rArrays;
      std::map<CStr, std::vector<Real> > m_rArraysMult;
      std::map<CStr, std::vector<int> > m_rArraysJj;
      std::map<CStr, CStr> m_map;

    };
  }
}

#endif
