import os
import re
import subprocess
import sys

def slurp_file(file_name):
    f = open(file_name)
    contents = f.read()
    f.close()
    return contents

def shell(command):
    #print command
    result = subprocess.call(command, shell=True)
    if result != 0:
        print "Error running shell command: " + command
        print "result: " + str(result)
        raise StandardError("Error running shell command. Error: " + str(result))

if len(sys.argv) == 3:
    changed = True
    if os.path.isfile(sys.argv[2]):
        new = slurp_file(sys.argv[1])
        old = slurp_file(sys.argv[2])
        changed = new != old
    if changed:
        print 'Updating runner.cpp'
        shell('copy /y ' + '"' + sys.argv[1] + '" "' + sys.argv[2] + '" > NUL')

exit(0)
