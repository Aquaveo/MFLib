//------------------------------------------------------------------------------
// FILE      NativeExpSTP.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpSTP.h>

#include <Export.h>
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\MfData\MfExport\private\H5\H5BcList.h>
#include <private\MfData\MfExport\private\H5\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\Sqlite\SqFile.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

using namespace MfData::Export;
namespace {
const char* PARAM_FILE_TYPES = "DRN DRT GHB RIV CHD Q   STR SFR";
const char* PAR_FILE_EXPORTED = "PAR_FILE_EXPORTED";
}; // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSTP::NativeExpSTP ()
{
} // MfNativeExpSTP::MfNativeExpSTP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSTP::~NativeExpSTP ()
{
} // MfNativeExpSTP::~MfNativeExpSTP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSTP::Export ()
{
  // write zone file
  MfPackage* zn = GetGlobal()->GetPackage(MfData::Packages::ZON);
  NativePackExp* exZn = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), zn);
  if (exZn) exZn->Export(); delete(exZn);
  // write mult file
  MfPackage* mlt = GetGlobal()->GetPackage(MfData::Packages::MLT);
  NativePackExp* exMlt = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), mlt);
  if (exMlt) exMlt->Export(); delete(exMlt);
  // write the pval file
  GetGlobal()->SetIntVar("Write PVAL", 1);
  MfPackage* pvl = GetGlobal()->GetPackage(MfData::Packages::PVAL);
  NativePackExp* exPvl = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), pvl);
  if (exPvl) exPvl->Export(); delete(exPvl);
  // write MNW2
  MfPackage* mnw2 = GetGlobal()->GetPackage(MfData::Packages::MNW2);
  if (mnw2)
  {
    mnw2->SetLineNumber("Export Final");
    NativePackExp* exMnw2 = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), mnw2);
    if (exMnw2) exMnw2->Export(); delete(exMnw2);
  }

  ForcePackageWrite(MfData::Packages::SEN1); // must be before the name file

  {
    // write the name file
    MfPackage* nm = GetGlobal()->GetPackage(MfData::Packages::NAM);
    NativePackExp* ex = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), nm);
    NativeExpNam* exNm = dynamic_cast<NativeExpNam*>(ex);
    if (exNm) exNm->WriteFileStp(); delete(exNm);
  }

  ForcePackageWrite(MfData::Packages::RCH);
  ForcePackageWrite(MfData::Packages::EVT);
  ForcePackageWrite(MfData::Packages::ETS);
  ForcePackageWrite(MfData::Packages::LAK);
  ForcePackageWrite(MfData::Packages::MNW);
  ForcePackageWrite(MfData::Packages::LGR);
  // write the oc file
  MfPackage* oc = GetGlobal()->GetPackage(MfData::Packages::OC);
  if (oc)
  {
    NativePackExp* exOc = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), oc);
    if (exOc) exOc->WriteComments(); exOc->WriteStoredLines(); delete(exOc);
  }

  if (GetH5Flag())
  {
    H5BcList h(this);
    h.WriteMapIdsForListBcs();
    H5UseLastWriter ul(this);
    ul.CheckArealFromUseLast();
    H5DataReader::CloseAllH5FilesOpenForWriting();
    ExpParamFile();
    ExpSuperFile();
  }

  if (GetNative()->GetUseSQLite())
  {
    sqLiteCloseAllDb();
  }

  ShowWarnings();
  // copy world file and prj file if they exist
  CopyAdditionalFiles();
  ASSERT(_CrtCheckMemory());
  return true;
} // MfNativeExpSTP::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::ForcePackageWrite (const char* const a_)
{
  MfPackage* p = GetGlobal()->GetPackage(a_);
  if (p)
  {
    NativePackExp* exP = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), p);
    if (exP)
    {
      exP->LastChanceBeforeWriting();
      exP->WriteStoredLines();
      delete(exP);
    }
  }
} // NativeExpSTP::ForcePackageWrite
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::CopyAdditionalFiles ()
{
  if (!mfLibExp_Exporting()) return;

  CStr nf, base, outBase, mwf, prj, outMwf, outPrj, vtu, gsf, outVtu, outGsf;
  outBase = GetNative()->GetExp()->GetBaseFileName();
  GetGlobal()->GetStrVar("NAME_FILE_STR", nf);
  util::StripExtensionFromFilename(nf.c_str(), base);
  mwf = base + ".mfw";
  prj = base + ".prj";
  vtu = base + ".vtu";
  gsf = base + ".gsf";
  outMwf = outBase + ".mfw";
  outPrj = outBase + ".prj";
  outVtu = outBase + ".vtu";
  outGsf = outBase + ".gsf";
  FILE* fp = fopen(mwf.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(mwf, outMwf);
  }
  fp = fopen(prj.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(prj, outPrj);
  }
  fp = fopen(vtu.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(vtu, outVtu);
  }
  fp = fopen(gsf.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(gsf, outGsf);
  }
} // NativeExpSTP::CopyAdditionalFiles
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::ShowWarnings ()
{
  int binaryExport(0);
  GetGlobal()->GetIntVar("BINARY_EXPORT", binaryExport);
  CStr msg;
  if (binaryExport && GetH5Flag())
  {
    msg = "\nGMS Binary Array Warning:: This model conatins binary arrays. "
      "The binary format could possibly differ from GMS MODFLOW. The array "
      "values need to be manually verified in GMS.\n\n";
  }
  else if (binaryExport)
  {
    msg = "\nBinary Array Warning: This model contains binary arrays. The "
      "binary format could differ from this version of MODFLOW. The array "
      "values need to be manually verified.\n\n";
  }
  printf(msg.c_str());
} // NativeExpSTP::ShowWarnings
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::ExpParamFile ()
{
  using namespace MfData::Packages;
  MfPackage* p(GetGlobal()->GetPackage("NAM1"));
  if (!p) return;
  const char *fname(NULL);
  TxtExporter* exp(GetNative()->GetExp());

  int parFileExported(0);
  GetGlobal()->SetIntVar(PAR_FILE_EXPORTED, parFileExported);
  if (p->GetField(NameFile::FNAME, &fname) && fname)
  {
    CStr sourceFile;
    util::StripExtensionFromFilename(fname, sourceFile);
    sourceFile += ".param";
    FILE* sourceParamFile = fopen(sourceFile.c_str(), "r");
    if (sourceParamFile != NULL)
    {
      const size_t maxLineSize = 256;
      char line[maxLineSize];
      while (fgets(line, maxLineSize, sourceParamFile))
      {
        exp->WriteStringToFile("param", line);
      }
      parFileExported = 1;
    }
  }
  
  if (!parFileExported)
  {

    Param p;
    ParamList *list(0);
    Parameters::GetParameterList(&list);

    // write out parameters for the DRN, DRT, GHB, RIV, CHD, WEL (Q)
    CStr parTypes(PARAM_FILE_TYPES);

    // We decided that we didn't want to duplicate code in mfLib for reading
    // and interpreting array based parameters. So we decided that we would
    // not write them to the param file and we would just have the code in
    // GMS to read the array based parameters and figure out if they are
    // using clusters or if they can be represented with key values. Once you
    // write out the files from GMS then all of the parameters will be written
    // to the param file.

    for (size_t i=0; i<list->Size(); i++)
    {
      CStr line, type;
      list->At(i, &p);
      type = p.m_type;
      line.ToUpper();
      if (!type.empty() && parTypes.find(type) != std::string::npos)
      {
        exp->WriteLineToFile("param", "BEGPAR");

        line.Format("NAME \"%s\"", p.m_name);
        exp->WriteLineToFile("param", line);

        if (type == "Q")
          type = "WELL";
        line.Format("TYPE %s", type);
        exp->WriteLineToFile("param", line);

        line.Format("KEY %s", STR(p.m_key));
        exp->WriteLineToFile("param", line);

        line.Format("VALUE %s %s %s",
                    STR(p.m_value),
                    STR(p.m_min == 0 ? 1e-10 : p.m_min),
                    STR(p.m_max == 0 ? p.m_parVal*100 : p.m_max));
        exp->WriteLineToFile("param", line);

        line.Format("SOLVE %d", p.m_isens ? 1 : 0);
        exp->WriteLineToFile("param", line);

        if (p.m_logTrans)
        {
          line = "LOGXFORM";
          exp->WriteLineToFile("param", line);
        }

        line.Format("BSCAL %s", STR(p.m_bscal));
        exp->WriteLineToFile("param", line);

        exp->WriteLineToFile("param", "ENDPAR");
        parFileExported = 1;
      }
    }
  }
  GetGlobal()->SetIntVar(PAR_FILE_EXPORTED, parFileExported);
} // NativeExpSTP::ExpParamFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::ExpSuperFile ()
{
  using namespace MfData::Packages;
  MfPackage* p(GetGlobal()->GetPackage("NAM1"));
  if (!p) return;
  TxtExporter* exp(GetNative()->GetExp());

  const char *fname(NULL);
  if (p->GetField(NameFile::FNAME, &fname) && fname)
  {
    CStr sourceFile;
    util::StripExtensionFromFilename(fname, sourceFile);
    sourceFile += ".mfs";
    FILE *fp(fopen(sourceFile, "r"));
    if (fp)
    {
      fclose(fp);
      CStr destFile(exp->GetBaseFileName());
      destFile += ".mfs";
      util::FileCopy(sourceFile, destFile);
      return;
    }
  }


  CStr line, base(exp->GetBaseFileName());
  //util::StripExtensionFromFilename(base, base);
  util::StripPathFromFilename(base, base);

  switch (GetGlobal()->ModelType())
  {
  case 0: // MODFLOW-2000
    exp->WriteLineToFile("mfs", "MF2KSUP");
    break;
  case 1: // MODFLOW-2005
    exp->WriteLineToFile("mfs", "MF2K5SUP");
    break;
  case 2: // MODFLOW-NWT
    exp->WriteLineToFile("mfs", "MFNWTSUP");
    break;
  case 3: // SEAWAT
    exp->WriteLineToFile("mfs", "MF2KSUP");
    break;
  case 4: // MODFLOW-LGR
    exp->WriteLineToFile("mfs", "MFLGRSUP");
    break;
  case 5: // MODFLOW-USG
    exp->WriteLineToFile("mfs", "MFUSGSUP");
    break;
  default:
    ASSERT(0);
    break;
  }

  int parFileExported(0);
  GetGlobal()->GetIntVar(PAR_FILE_EXPORTED, parFileExported);
  if (parFileExported)
  {
    line.Format("MPARAM \"%s.param\"", base);
    exp->WriteLineToFile("mfs", line);
  }
  line.Format("NAME 99 \"%s.mfn\"", base);
  exp->WriteLineToFile("mfs", line);
} // NativeExpSTP::ExpSuperFile

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpSTP.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSTPT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage("STP");
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSTP*>(p);
} // NativeExpSTPT::setUp
//------------------------------------------------------------------------------
void NativeExpSTPT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSTPT::tearDown
//------------------------------------------------------------------------------
void NativeExpSTPT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSTPT::testCreateClass

#endif