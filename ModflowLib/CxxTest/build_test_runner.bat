@echo on
python.exe CxxTest\list_test_headers.py ModflowLib.vcxproj > test_headers.txt
IF NOT %errorlevel% == 0 GOTO :END
python.exe ..\..\cxxtest-4.2.1\bin\cxxtestgen --have-std --have-eh --main=run_cxxtests_main --headers=test_headers.txt --output=CxxTest\1.0\new_runner.cpp
IF NOT %errorlevel% == 0 GOTO :END
python.exe CxxTest\update_on_changed.py CxxTest\1.0\new_runner.cpp CxxTest\1.0\runner.cpp
:END
