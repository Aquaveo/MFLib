#ifndef RUNTEST_H
#define RUNTEST_H

#ifdef CXX_TEST

namespace testCxx
{
int RunUnitTests(double a_maxTestTime = 10000.0);
bool& TestsRunning();
bool& RunAllTests();
bool& PrintXML();
}

#endif

#endif
