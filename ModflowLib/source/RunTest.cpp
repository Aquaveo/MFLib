
#ifdef CXX_TEST
#include <RunTest.h>
#include <cxxtest/TeeListener.h>
#include <cxxtest/TestListener.h>
#include <cxxtest/TestTracker.h>
#include <cxxtest/TestRunner.h>
#include <cxxtest/RealDescriptions.h>
#include <cxxtest/ParenPrinter.h>
#include <cxxtest/TestMain.h>
#include <cxxtest/XmlPrinter.h>
#include <cxxtest/XUnitPrinter.h>

#include <fstream>

namespace {

class ParenXmlPrinter : public CxxTest::TeeListener {
public:

    CxxTest::XmlPrinter xml_printer;
    CxxTest::ParenPrinter paren_printer;

    ParenXmlPrinter(CXXTEST_STD(ostream) &o = CXXTEST_STD(cout))
        : xml_printer(o) {
        setFirst(paren_printer);
        setSecond(xml_printer);
    }

    int run() {
        CxxTest::TestRunner::runAllTests(*this);
        return CxxTest::tracker().failedTests();
    }
};

}

namespace testCxx
{

int RunUnitTests(double /*a_maxTestTime*/ /*= 10000*/)
{
  char* worldName;
#ifdef DBLPREC
  worldName = "MfLibDouble";
#else
  worldName = "MfLibSingle";
#endif
  int argc = 1;
  char* argv[] = {worldName};
  int status;
  CxxTest::RealWorldDescription::_worldName = worldName;
  TestsRunning() = true;
  std::ofstream o(std::string(worldName) + ".xml");
  ParenXmlPrinter tmp(o);
  status = CxxTest::Main< ParenXmlPrinter >( tmp, argc, argv );
  if (status == 0)
    std::cout << "OK!" << std::endl;
  TestsRunning() = false;
  return status;
}

bool& TestsRunning ()
{
  static bool m_testsRunning(false);
  return m_testsRunning;
}

bool& RunAllTests ()
{
  static bool m_runAllTests(false);
  return(m_runAllTests);
}

} // namespace testCxx

#endif
