import re
import sys

if len(sys.argv) == 2:
	test_header_regex = re.compile(r'Include="(.+\.t\.h)"')
	f = open(sys.argv[1])
	lines = f.readlines()
	for line in lines:
		test_header_match = test_header_regex.search(line)
		if test_header_match:
			include = test_header_match.group(1)
			print include
exit(0)
