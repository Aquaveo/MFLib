"""
Tests a model by reading an xml file and using the info in the file to
run the tests.

"""
# function that takes in 2 arrays and an index
# progs - array of programs being tested - same size as testInputs
# testInputs - array of inputs to the programs
# idx - index to tell which process this is - test results written to temp file based on the idx
def runListOfTests (progs, testInputs, idx):
    cnt = 0
    for t in testInputs:
        runOneTest(progs[cnt], t, idx)
        cnt = cnt + 1

# function to run a single test
def runOneTest (mod, t, idx):
    import subprocess
    import os
    import string
    import copy

    inFile = t.m_fname
    print ".",
    curDir = os.getcwd()
    # change the directory to where the input file is
    aDir = os.path.dirname(inFile)
    tstr = "TestRun\\" + str(idx)
    aDir = string.replace(aDir, "TestRun", tstr)
    os.chdir(aDir)
    infile = os.path.basename(inFile)
    infilePrefix = os.path.splitext(infile)[0]
    oFile = aDir + "\\" + infilePrefix + ".lst"
    arrOutputs = copy.copy(t.m_outputs)
    cnt = 0
    for b in arrOutputs:
        arrOutputs[cnt] = aDir + "\\" + b
        cnt = cnt + 1
    if not arrOutputs:
        arrOutputs.append(oFile)


    for cmd in mod.m_cmdLines:
        cmdStr = cmd.m_commandStr
        modStr = curDir + mod.m_path + mod.m_name
        cmdStr = cmdStr.replace('%program%', modStr)
        cmdStr = cmdStr.replace('%program-name%', mod.m_name)
        cmdStr = cmdStr.replace("%test-name%", infile)
        cmdStr = cmdStr.replace("%test-name-prefix%", infilePrefix)
        outFile = infile + "-" + mod.m_name + cmd.m_pipeOut + ".txt"
        cmdStr = cmdStr + " > " + outFile + " 2>&1"
        directory = cmd.m_directory
        if directory:
          directory = directory.replace('%program%', modStr)
          directory = directory.replace('%program-name%', mod.m_name)
          directory = directory.replace("%test-name%", infile)
          directory = directory.replace("%test-name-prefix%", infilePrefix)
          output = subprocess.call('echo ' + "cd " + directory + ' >> ' + str(idx) + '-commands.txt', shell=True)
          output = subprocess.call('echo ' + cmdStr + ' >> ' + str(idx) + '-commands.txt', shell=True)
          output = subprocess.call(cmdStr, shell=True, cwd=directory)
        else:
          output = subprocess.call('echo ' + cmdStr + ' >> ' + str(idx) + '-commands.txt', shell=True)
          output = subprocess.call(cmdStr, shell=True)
        #print cmdStr
    os.chdir(curDir) # change the directory back to what it was

    passed = True
    for o in arrOutputs:
        if not checkModelOutput(o, mod.m_outputDirBase + mod.m_outputDirName):
            passed = False

    #write results to file
    fname = mod.m_outputDirBase + "testrun" + str(idx) + ".txt"
    #print fname
    strsuccess = "0"
    if passed:
        strsuccess = "1"
    mystr = mod.m_suiteName + " " + os.path.basename(oFile) + " " + strsuccess +  " " + oFile + "\n"
    #print mystr
    myfile = open(fname, 'a')
    myfile.write(mystr)
    myfile.close()

def checkModelOutput (a_oFile, a_oDir):
    import filecmp
    import os
    import shutil

    oDir = a_oDir
    if not os.path.exists(oDir):
        try:
            os.mkdir(oDir)
        except:
            # other process must have created it
            # if not it will fail later on
            pass
    baseDir = oDir.replace("out", "base");

    oFile = oDir + "\\" +os.path.basename(a_oFile)
    baseFile = baseDir + "\\" + os.path.basename(a_oFile)
    # copy the output file
    shutil.copyfile(a_oFile, oFile)

    # make sure the files exist
    info = ""
    if not os.path.exists(baseFile):
        info = "Model did not execute properly. File not found: "
        info += baseFile
    if not os.path.exists(oFile):
        info = "Model did not execute properly. File not found: "
        info += oFile

    status = True
    if info != "":
        status = False
        return status

    outF = open(oFile, "r")
    lines = outF.readlines()
    outF.close()
    # remove the line about how long it took to complete the model
    s0 = "Run end date and time"
    s1 = "Elapsed run time:"
    outF = open(oFile, "w")
    line = ""
    for line in lines:
        if line.find(s0) == -1 and line.find(s1) == -1:
            outF.write(line)
    outF.close()

    status = filecmp.cmp(baseFile, oFile)
    #if not status:
    #    print "failed: " + os.path.basename(baseFile)
    return status

class programCommand:
    def __init__ (self, a_index, a_commandStr, a_directory, a_pipeOut):
        self.m_index = a_index                        # index for when you have more than one command
        astr = a_commandStr.encode('ascii','ignore')
        self.m_commandStr = astr                      # the command line string
        self.m_directory = a_directory                # the directory to run from
        self.m_pipeOut = a_pipeOut                    # string for where to redirect std out

class inputToTest:
    def __init__ (self, a_name, a_groupIdx, a_outputs):
        self.m_fname = a_name                         # the input to test
        self.m_groupIdx = a_groupIdx                  # an index used to segregate tests and programs into groups
        self.m_outputs = a_outputs                    # an array of the outputs that we want to compare to see if the test passed

class programToTest:
    def __init__ (self, a_name, a_suiteName, a_path, a_description, a_outputDirName, a_cmdLines, a_outBase, a_groupIdx):
        self.m_name = a_name                          # test name
        self.m_suiteName = a_suiteName                # suite name
        self.m_path = a_path                          # path location of the executable
        self.m_description = a_description            # description of the program
        self.m_outputDirName = a_outputDirName        # the directory where the output should go
        self.m_cmdLines = a_cmdLines                  # array of programCommand classes
        self.m_outputDirBase = a_outBase              # location where the baseline files are stored for comparing output
        self.m_groupIdx = a_groupIdx                  # an index used to segregate tests and programs into groups

class theTester:
    def __init__ (self, a_xmlFile):
        self.m_xmlInputFile = a_xmlFile               # the XML input file that gives testing directions
        self.m_description = ""                       # description of the tests in the xml file
        self.m_outputDir = ""                         # directory where output is copied and compared to baseline
        self.m_testInputs = []                        # read from XML file to identify the tests
        self.m_programsToTest = []                    # read from the XML file to identify the programs we are testing
        self.m_arrayProgs = []                        # array of programs for use with multiprocessing - the array is nProcess big
        self.m_arrayTestInputs = []                   # array of test inputs for use with multiprocessing
        self.m_jobs = []                              # array that keeps track of multiple processes
        self.m_nProcess = 4                           # number of processes to spawn
        self.m_doSerial = 0                           # used for debugging - change value to 1 to avoid parallel code
        self.m_failed = 0

    def copyTestInputs (self, dom):
        from xml.dom.minidom import parse
        import os
        import shutil

        for node in dom.getElementsByTagName('CopyTestInputsToRunDir'):
            dirName = os.getcwd() + node.getAttribute("dirName")
            # delete the test run directory
            if os.path.exists(dirName):
                shutil.rmtree(dirName, ignore_errors=1)
            os.mkdir(dirName)
            dirNameBase = dirName
            dirName = dirNameBase + "0\\"
            dirName0 = dirName

            for child in node.childNodes:
                if child.nodeName == "Dir":
                    ipath = child.getAttribute("input")
                    dstDir = dirName + ipath
                    shutil.copytree(os.getcwd()+ipath, dstDir)

            if not self.m_doSerial:
                cnt = 1
                while cnt < self.m_nProcess:
                    dirName = dirNameBase + str(cnt) + "\\"
                    shutil.copytree(dirName0, dirName)
                    cnt = cnt + 1

    def copyExesFromXml (self, dom):
        from xml.dom.minidom import parse
        import os
        import shutil

        for node in dom.getElementsByTagName('CopyExesToBin'):
            for child in node.childNodes:
                if child.nodeName == "Dir":
                    fnames = []
                    dirName = os.getcwd() + child.getAttribute("dirName")
                    for c in child.childNodes:
                        if c.nodeName == "file":
                            fnames.append(os.getcwd() + "\\" + c.getAttribute("input"))

                    if not os.path.exists(dirName):
                        os.mkdir(dirName)

                    for f in fnames:
                        dst = dirName + os.path.basename(f)
                        shutil.copy(f, dst)

    def readTestInputs (self, dom):
        from xml.dom.minidom import parse
        import os

        for node in dom.getElementsByTagName('testinputs'):
            for child in node.childNodes:
                if child.nodeName == "Group":
                    gpath = child.getAttribute("path")
                    groupIdx = 0
                    if child.hasAttribute("groupIndex"):
                        groupIdx = int(child.getAttribute("groupIndex"))
                    for test in child.childNodes:
                        if test.nodeName == "test":
                            tpath = test.getAttribute("path")
                            fname = test.getAttribute("name")
                            outputs = []
                            for c in test.childNodes:
                                if c.nodeName == "output":
                                    outputs.append(c.getAttribute("name"))
                            if not fname == "":
                                fileToTest = os.getcwd() + gpath + tpath + fname
                                fileToTest = fileToTest.encode('ascii','ignore')
                                self.m_testInputs.append(inputToTest(fileToTest, groupIdx, outputs))

    def readProgramsToTest (self, dom):
        from xml.dom.minidom import parse
        import os

        for node in dom.getElementsByTagName('programsToTest'):
            self.m_outputDir = os.getcwd() + node.getAttribute("outputDirectory")
            for child in node.childNodes:
                if child.nodeName == "Programs":
                    cmdLines = self.readCmdLines(child)
                    groupIdx = 0
                    if child.hasAttribute("groupIndex"):
                        groupIdx = int(child.getAttribute("groupIndex"))
                    for c in child.childNodes:
                        if c.nodeName == "program":
                            pName = c.getAttribute("name")
                            sName = c.getAttribute("suiteName")
                            pPath = c.getAttribute("path")
                            pDesc = c.getAttribute("description")
                            pOutput = c.getAttribute("output")
                            pOutputDirName = c.getAttribute("outputDirName")
                            self.m_programsToTest.append(programToTest(pName,sName,pPath,pDesc,pOutputDirName,cmdLines,self.m_outputDir,groupIdx))

    def readCmdLines (self, a_pNode):
        from xml.dom.minidom import parse

        cmdLines = []
        for child in a_pNode.childNodes:
            if child.nodeName == "command":
                idx = int(child.getAttribute("index"))
                cmdLine = child.getAttribute("cmdLine")
                pipeOut = child.getAttribute("pipeOut")
                directory = None
                if child.hasAttribute("directory"):
                    directory = child.getAttribute("directory")
                cLine = programCommand(idx, cmdLine, directory, pipeOut)
                cmdLines.append(cLine)
        cmdLines.sort(key=lambda programCommand: programCommand.m_index)
        return cmdLines


    def readXmlInput (self):
        import os
        from xml.dom.minidom import parse

        if self.m_xmlInputFile == "":
            self.m_xmlInputFile = "runTests.xml"

        xFile = os.getcwd() + "\\" + self.m_xmlInputFile
        dom = parse(xFile)
        print "Copying test inputs"
        for node in dom.getElementsByTagName('root'):
            self.m_description = node.getAttribute('description')
            self.m_nProcess = int(node.getAttribute('nProcess'))
        #print self.m_nProcess + "\n"
        if (self.m_nProcess > 8) :
            self.m_nProcess = 8
        self.copyTestInputs(dom)
        self.copyExesFromXml(dom)
        self.readTestInputs(dom)
        self.readProgramsToTest(dom)


    def printTestInfo (self):
        import os

        print "These files will be tested:"
        str = ""
        for t in self.m_testInputs:
            if not str == "":
                str += ", "
            str += os.path.basename(t.m_fname)
        print str

        print "These models will be tested"
        for mod in self.m_programsToTest:
            print "Name: " + mod.m_name + " - Description: " + mod.m_description

    def writeTestResults (self, elaspedTime):
        import os
        import string
        import xml.etree.ElementTree as xml
        import xml.dom.minidom as minidom

        top = xml.Element("world")

        total = 0
        self.m_failed = 0
        for cnt in xrange(self.m_nProcess):
            fname = self.m_outputDir + "testrun" + str(cnt) + ".txt"
            f = open(fname, "r")
            for line in f:
                items = string.split(line, ' ')
                if len(items) == 4:
                    self.addTestCaseToXml(top, items[0], items[1], items[2], items[3])
                    total = total + 1
                    if items[2] != "1":
                        self.m_failed = self.m_failed + 1
            f.close

        self.addTestResultsToXml(top,total,self.m_failed,elaspedTime)

        consiseXmlStr = xml.tostring(top, 'utf-8')
        reparsed = minidom.parseString(consiseXmlStr)
        xmlStr = reparsed.toprettyxml(indent="  ")
        f = open(os.getcwd() + "\\test.xml", "w")
        f.write(xmlStr)
        f.close()


    def addTestCaseToXml(self,top,suiteName,fname,success,path):
        import os
        from xml.etree.ElementTree import Element, SubElement

        testSuite = Element("testsuite")
        testSuite.attrib['name'] = suiteName
        suiteFound = 0

        # see if the suite exists
        for node in top.getiterator("testsuite"):
            aName = node.attrib['name']
            if aName == suiteName:
                testSuite = node
                suiteFound = 1

        if suiteFound == 0:
            top.append(testSuite)

        child1 = Element("testcase")
        child1.attrib['name'] = suiteName + "::" + fname
        testSuite.append(child1)
        if success != "1":
            atts = {}
            child2 = SubElement(child1, "failure", atts)
            child2.attrib['path'] = path.rstrip('\r\n')


    def addTestResultsToXml (self,xmlTop,total,failures,elapsedTime):
        import xml.etree.ElementTree as xml
        import datetime

        child = xml.Element("test-results")
        xmlTop.append(child)
        child.attrib['name'] = self.m_description
        child.attrib['total'] = str(total)
        child.attrib['errors'] = str(failures)
        child.attrib['failures'] = str(failures)
        child.attrib['not-run'] = "0"
        child.attrib['inconclusive'] = "0"
        child.attrib['ignored'] = "0"
        child.attrib['skipped'] = "0"
        child.attrib['invalid'] = "0"
        now = datetime.datetime.now()
        child.attrib['date'] = now.strftime("%Y-%m-%d")
        child.attrib['time'] = now.strftime("%H:%M:%S")
        child.attrib['elapsedTime'] = str(elapsedTime)
        #atts = {not-run":0, "inconclusive":0, "ignored":0, "skipped":0, "invalid":0, "date":Date(), "time":Time()}
        #child = SubElement(self.m_xmlTop, 'test-results', atts)

    def runParallel (self):
        # create the multiple processes
        self.startJobs()
        # wait for processes to finish
        self.waitForJobsToFinish()

    def runSerial (self):
        cnt = 0
        for arr in self.m_arrayProgs:
            runListOfTests(self.m_arrayProgs[cnt],self.m_arrayTestInputs[cnt], 0)
            cnt = cnt + 1

    def cleanOutTestResultFiles(self):
        import os
        for cnt in xrange(self.m_nProcess):
            fname = self.m_outputDir + "testrun" + str(cnt) + ".txt"
            file = open(fname, "w")


    def doTests (self, elapsedTime):
        import time

        self.printTestInfo()
        # create arrays of the programs and files that can be passed to
        # multiple processors
        self.fillInLists()
        # remove out files
        self.cleanOutTestResultFiles()
        # run tests serial or parallel
        if self.m_doSerial:
            self.runSerial() # just for testing
        else:
            self.runParallel()

        elapsedTime = time.time() - elapsedTime

        # write the results
        self.writeTestResults(elapsedTime)



    def waitForJobsToFinish (self):
        import multiprocessing
        import time

        done = 0
        while done != 1:
            done = 1
            for j in self.m_jobs:
                if j.is_alive():
                    done = 0
            time.sleep(.1)


    def startJobs (self):
        import multiprocessing
        for cnt in xrange(self.m_nProcess):
            p = multiprocessing.Process(target=runListOfTests,args=(self.m_arrayProgs[cnt],self.m_arrayTestInputs[cnt], cnt))
            p.start()
            self.m_jobs.append(p)


    def fillInLists (self):
        for i in xrange(self.m_nProcess):
            self.m_arrayProgs.append([])
            self.m_arrayTestInputs.append([])
        total = 0
        cnt = 0
        for mod in self.m_programsToTest:
            for t in self.m_testInputs:
                if mod.m_groupIdx == t.m_groupIdx:
                    self.m_arrayProgs[cnt].append(mod)
                    self.m_arrayTestInputs[cnt].append(t)
                    cnt = cnt + 1
                    total = total + 1
                    if cnt == self.m_nProcess:
                        cnt = 0

        print "Running " + str(total) + " tests."

    def runTests (self):
        import time
        elapsedTime = time.time()
        self.readXmlInput()
        self.doTests(elapsedTime)



def runMain(argv=None):
    import sys
    import getopt

    if argv is None:
        argv = sys.argv[1:]
    try:
        opts, args = getopt.getopt(argv, "t:", ["testXmlFile="])
    except getopt.GetoptError:
        usage()
        return 1

    testXmlFile = ""
    for opt, arg in opts:
        if opt == "-t":
            testXmlFile = arg

    tester = theTester(testXmlFile)
    tester.runTests()
    return tester.m_failed

if __name__ == '__main__':
    import sys
    sys.exit(runMain())
