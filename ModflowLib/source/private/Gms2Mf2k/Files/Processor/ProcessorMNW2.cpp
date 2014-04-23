//------------------------------------------------------------------------------
// FILE:      ProcessorMNW2.cpp
// PURPOSE:   Describe the purpose of the functions in the file.
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private/Gms2Mf2k/Files/Processor/ProcessorMNW2.h>

// 3. Standard library headers
#include <fstream>
//#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/util/EReadAsciiFile.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------

//----- Constants / Enumerations -----------------------------------------------
namespace MNW2
{
    enum { ACTIVE=0, QDES, CAPMULT, CPRIME, HLIM, QCUT, QFRCMN, QFRCMX,
           AUX_0, AUX_1, AUX_2, AUX_3, AUX_4,
           NPROP };
};
//----- Classes / Structs ------------------------------------------------------
class Mnw2Well
{
public:
  Mnw2Well() :
      m_WELLID()
    , m_LOSSTYPE()
    , m_NNODES(0)
    , m_PUMPLOC(0)
    , m_Qlimit(0)
    , m_PUMPCAP(0)
      {}

  CStr m_WELLID, m_LOSSTYPE;
  int  m_NNODES, m_PUMPLOC, m_Qlimit, m_PPFLAG, m_PUMPCAP;
};
class ProcessorMNW2::impl
{
public:
  impl();

  bool DoConvertFile(const char * const a_inputFile,
                     const char * const a_outputFile,
                     const int &a_nRow,
                     const int &a_nCol);

  void ReadComments(std::fstream& a_is,
                    std::fstream& a_os);
  void ReadLn1(std::fstream& a_is,
               std::fstream& a_os);
  void ReadWells(std::fstream& a_is,
                 std::fstream& a_os);
  void ReadLn2a(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2b(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2c(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2d(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2e(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2f(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2g(std::fstream& a_is,
                std::fstream& a_os);
  void ReadLn2h(std::fstream& a_is,
                std::fstream& a_os);
  void ReadStressPeriods(std::fstream& a_is,
                         std::fstream& a_os);

  int m_MNWMAX,
      m_nAUX;

  std::vector<Mnw2Well> m_wells;
private:
};
//----- Internal functions -----------------------------------------------------
namespace
{
  int numMnw2Prop () { return MNW2::NPROP; }
};
//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ProcessorMNW2::ProcessorMNW2 (const char * const a_inputFile,
                              const char * const a_outputFile,
                              const int& a_nRow,
                              const int& a_nCol) :
Processor(a_inputFile, a_outputFile, a_nRow, a_nCol, false),
m_p(new ProcessorMNW2::impl())
{
} // ProcessorMNW2::ProcessorMNW2
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
ProcessorMNW2::~ProcessorMNW2 ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...)
  {
  }
} // ProcessorMNW2::~ProcessorMNW2
//------------------------------------------------------------------------------
/// \brief Private virtual function that actually does the conversion
//------------------------------------------------------------------------------
bool ProcessorMNW2::DoConvertFile ()
{
  return(m_p->DoConvertFile(InputFile(),
                            OutputFile(),
                            NumRow(),
                            NumCol()));
} // ProcessorMNW2::DoConvertFile

//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ProcessorMNW2::impl::impl ()
{
} // ProcessorMNW2::impl::impl
//------------------------------------------------------------------------------
/// \brief Does file conversion
//------------------------------------------------------------------------------
bool ProcessorMNW2::impl::DoConvertFile (const char * const a_inputFile,
                                         const char * const a_outputFile,
                                         const int& /*a_nRow*/,
                                         const int& /*a_nCol*/)
{
  bool rval(1);
  CStr inputFile(a_inputFile), outputFile(a_outputFile);

  try
  {
    std::fstream is;
    is.open((LPCTSTR)a_inputFile, std::ios_base::in);
    if (is.bad())
      throw ioError();

    std::fstream os;
    os.open((LPCTSTR)a_outputFile, std::ios_base::out);
    if (os.bad())
      throw ioError();

    ReadComments(is, os);
    ReadLn1(is, os);
    ReadWells(is, os);
    ReadStressPeriods(is, os);

  }
  catch (std::exception&)
  {
    CStr msg("Error processing file: ");
    msg += inputFile;
    msg += ".";
    ErrorStack::Get().PutError(msg);
    rval = false;
  }

  return rval;
} // ProcessorMNW2::impl::DoConvertFile
//------------------------------------------------------------------------------
/// \brief reads comments
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadComments (std::fstream& a_is,
                                        std::fstream& a_os)
{
  char c = (char)a_is.peek();
  while (c == '#')
  {
    std::string line;
    std::getline(a_is, line);
    // write all the comments back to the file except for #GMS_HDF5_01
    if ("#GMS_HDF5_01" != line)
    {
      a_os << line << "\n";
    }
    c = (char)a_is.peek();
  }
} // ProcessorMNW2::impl::ReadComments
//------------------------------------------------------------------------------
/// \brief reads line 1
/// # 1.    MNWMAX,IWL2CB,MNWPRNT,{OPTION}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn1 (std::fstream& a_is,
                                   std::fstream& a_os)
{
  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";

  // Read number of wells and number of AUX variables
  EReadAsciiFile e;
  e.UseExceptions();
  e.SetLine(line.c_str());
  e.ReadData(m_MNWMAX);
  int myInt;
  e.ReadData(myInt);
  e.ReadData(myInt);
  // read the AUX
  m_nAUX = 0;
  CStr str;
  e.UseExceptions(false);
  while (e.ReadData(str))
  {
    if ("AUX" == str ||
        "AUXILIARY" == str)
    {
      e.ReadData(str);
      m_nAUX++;
    }
  }
} // ProcessorMNW2::impl::ReadLn1
//------------------------------------------------------------------------------
/// \brief reads lines 2a-2h
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadWells (std::fstream& a_is,
                                     std::fstream& a_os)
{
  for (int i=0; i<m_MNWMAX; i++)
  {
    ReadLn2a(a_is, a_os);
    ReadLn2b(a_is, a_os);
    ReadLn2c(a_is, a_os);
    ReadLn2d(a_is, a_os);
    ReadLn2e(a_is, a_os);
    ReadLn2f(a_is, a_os);
    ReadLn2g(a_is, a_os);
    ReadLn2h(a_is, a_os);
  }
} // ProcessorMNW2::impl::ReadWells
//------------------------------------------------------------------------------
/// \brief Reads Line 2a.   WELLID,NNODES
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2a (std::fstream& a_is,
                                    std::fstream& a_os)
{
  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";

  CStr str = line.c_str();
  str.Replace('\'', '\"');
  EReadAsciiFile e;
  e.UseExceptions();
  e.SetLine((LPCTSTR)str);
  Mnw2Well w;
  e.ReadData(w.m_WELLID);
  e.ReadData(w.m_NNODES);
  m_wells.push_back(w);
} // ProcessorMNW2::impl::ReadLn2a
//------------------------------------------------------------------------------
/// \brief Reads Line 2b.   LOSSTYPE,PUMPLOC,Qlimit,PPFFLAG,PUMPCAP
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2b (std::fstream& a_is,
                                    std::fstream& a_os)
{
  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";

  EReadAsciiFile e;
  e.UseExceptions();
  e.SetLine(line.c_str());
  CStr str;
  e.ReadData(m_wells.back().m_LOSSTYPE);
  e.ReadData(m_wells.back().m_PUMPLOC);
  e.ReadData(m_wells.back().m_Qlimit);
  e.ReadData(m_wells.back().m_PPFLAG);
  e.ReadData(m_wells.back().m_PUMPCAP);

} // ProcessorMNW2::impl::ReadLn2b
//------------------------------------------------------------------------------
/// \brief Reads 2c. Data: {Rw Rskin Kskin B C P CWC}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2c (std::fstream& a_is,
                                    std::fstream& a_os)
{
  if (m_wells.back().m_LOSSTYPE.CompareNoCase("NONE") == 0)
    return;

  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";
} // ProcessorMNW2::impl::ReadLn2c
//------------------------------------------------------------------------------
/// \brief Reads 2d-1. Data: LAY ROW COL {Rw Rskin Kskin B C P CWC PP}
///              2d-2. Data: Ztop Zbotm ROW COL {Rw Rskin Kskin B C P CWC PP}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2d (std::fstream& a_is,
                                    std::fstream& a_os)
{
  std::string line;
  for (int i=0; i<abs(m_wells.back().m_NNODES); i++)
  {
    std::getline(a_is, line);
    a_os << line << "\n";
  }
} // ProcessorMNW2::impl::ReadLn2d
//------------------------------------------------------------------------------
/// \brief Reads 2e. Data: {PUMPLAY PUMPROW PUMPCOL} {Zpump}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2e (std::fstream& a_is,
                                    std::fstream& a_os)
{
  if (m_wells.back().m_PUMPLOC == 0)
    return;

  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";
} // ProcessorMNW2::impl::ReadLn2e
//------------------------------------------------------------------------------
/// \brief Reads 2f. Data: Hlim QCUT {Qfrcmn Qfrcmx}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2f (std::fstream& a_is,
                                    std::fstream& a_os)
{
  if (m_wells.back().m_Qlimit < 1)
    return;

  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";

} // ProcessorMNW2::impl::ReadLn2f
//------------------------------------------------------------------------------
/// \brief Reads 2g. Data: Hlift LIFTq0 LIFTqmax HWtol
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2g (std::fstream& a_is,
                                    std::fstream& a_os)
{
  if (m_wells.back().m_PUMPCAP < 1)
    return;

  std::string line;
  std::getline(a_is, line);
  a_os << line << "\n";
} // ProcessorMNW2::impl::ReadLn2g
//------------------------------------------------------------------------------
/// \brief Reads 2h. Data: LIFTn Qn
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadLn2h (std::fstream& a_is,
                                    std::fstream& a_os)
{
  std::string line;
  for (int i=0; i<m_wells.back().m_PUMPCAP; i++)
  {
    std::getline(a_is, line);
    a_os << line << "\n";
  }
} // ProcessorMNW2::impl::ReadLn2h
//------------------------------------------------------------------------------
/// \brief Reads 3. Data: ITMP
/// \brief Reads 4. Data: WELLID Qdes {CapMult} {Cprime} {xyz}
//------------------------------------------------------------------------------
void ProcessorMNW2::impl::ReadStressPeriods (std::fstream& a_is,
                                             std::fstream& a_os)
{
  CStr str;
  std::string line;
  EReadAsciiFile e;
  e.UseExceptions();
  while (!a_is.bad())
  {
    std::getline(a_is, line);
    if (line.empty())
    {
      return;
    }

    int ITMP;
    a_os << line << "\n";
    e.SetLine(line.c_str());
    e.ReadData(ITMP);
    if (ITMP < 1)
      continue;

    std::getline(a_is, line);
    e.SetLine(line.c_str());
    e.ReadData(str);
    if ("GMS_HDF5_01" == str)
    {
      CStr fname, path;
      int sp;
      e.ReadData(fname);
      e.ReadData(path);
      path += "/07. Property";
      e.ReadData(sp);
      std::pair<int, int> myPair(0,1);
      VEC_INT_PAIR indices(3, myPair);
      indices[0].second = numMnw2Prop();
      indices[1].second = m_MNWMAX;
      indices[2].first = sp - 1;
      H5DataSetReader r(fname, path, indices);
      CAR_DBL2D vals;
      vals.SetSize(MNW2::NPROP, m_MNWMAX, 0);
      if (!r.GetData(&vals[0][0], MNW2::NPROP*m_MNWMAX))
      {
        ErrorStack::Get().PutError("Unable to read MNW2 hdf5 data.");
        throw ioError("");
      }

      for (int i = 0; i<m_MNWMAX; i++)
      {
        if (vals.at(MNW2::ACTIVE, i) != 0)
        { // this well is active
          // 4a. Data: WELLID Qdes {CapMult} {Cprime} {xyz}
          CStr ln;
          ln.Format("'%s' %s", m_wells.at(i).m_WELLID,
                               STR(vals.at(MNW2::QDES, i)));
          if (m_wells.at(i).m_PUMPCAP > 0)
          {
            ln += " ";
            ln += STR(vals.at(MNW2::CAPMULT, i));
          }
          // don't write Cprime

          // get AUX
          for (int j = 0; j<m_nAUX; j++)
          {
            ln += " ";
            ln += STR(vals.at(MNW2::AUX_0+j, i));
          }
          a_os << ln << "\n";

          if (m_wells.at(i).m_Qlimit < 0)
          {
            double QCUT = vals.at(MNW2::QCUT, i);
            ln.Format("%s %s", STR(vals.at(MNW2::HLIM, i)), STR(QCUT));
            if (QCUT != 0)
            {
              CStr ln2;
              ln2.Format(" %s %s", STR(vals.at(MNW2::QFRCMN, i)),
                                   STR(vals.at(MNW2::QFRCMX, i)));
              ln += ln2;
            }
            a_os << ln << "\n";
          }
        }
      }
    }
  }
} // ProcessorMNW2::impl::ReadStressPeriods
