
#ifdef CXX_TEST
#include <RunTest.h>
#include <cxxtest/TestListener.h>
#include <cxxtest/TestTracker.h>
#include <cxxtest/TestRunner.h>
#include <cxxtest/RealDescriptions.h>
#include <cxxtest/ParenPrinter.h>
#include <cxxtest/TestMain.h>
#include <cxxtest/XmlPrinter.h>
#include <cxxtest/XUnitPrinter.h>

#include <RunTest.h>

namespace testCxx
{

int RunUnitTests(double /*a_maxTestTime*/ /*= 10000*/)
{
  int argc = 1;
  char* argv[] = {"cxxtest"};
  int status;
  CxxTest::RealWorldDescription::_worldName = "cxxtest";
  TestsRunning() = true;
  if (PrintXML()) {
    CxxTest::XUnitPrinter tmp;
    status = CxxTest::Main< CxxTest::XUnitPrinter >( tmp, argc, argv );
  }
  else {
    CxxTest::ParenPrinter tmp;
    status = CxxTest::Main< CxxTest::ParenPrinter >( tmp, argc, argv );
  }
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

bool& PrintXML ()
{
  static bool m_printXML(false);
  return(m_printXML);
}

} // namespace testCxx

#endif
