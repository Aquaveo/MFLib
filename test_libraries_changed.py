

def get_diff_files(left_dir, right_dir, ignore):
    from filecmp import dircmp
    dcmp = dircmp(left_dir, right_dir, ignore)
    results = ""

    # left only
    if dcmp.left_only:
        results += "Found only in " + dcmp.left + "\n"
        for name in dcmp.left_only:
            results += name + "\n"

    # right only
    if dcmp.right_only:
        results += "Found only in " + dcmp.right + "\n"
        for name in dcmp.right_only:
            results += name + "\n"

    # different
    if dcmp.diff_files:
        results += "Different files:\n"
        for name in dcmp.diff_files:
            results += name + "\n"

    # unable to compare
    if dcmp.common_funny:
        results += "Unable to compare files:\n"
        for name in dcmp.common_funny:
            results += name + "\n"

    for sub_dcmp in dcmp.subdirs.values():
        results += get_diff_files(sub_dcmp.left, sub_dcmp.right, ignore)

    return results

    
def print_xml_head(errors):
    print '<?xml version="1.0" encoding="UTF-8" ?>'
    print '<testsuite name="CheckLibraryChanges"  tests="2" errors="0" failures="' + str(errors) + '" time="0" >'
    

def print_test_xml(output, test_name):
    if output:
        print '  <testcase classname="check_for_library_change.py" name="' + test_name + '" line="1">'
        print '    <failure file="python.py" line="1" type="failure" >Test failed: ' + test_name + '</failure>'
        print '    <system-out >'
        print 'Directory comparison failed. May need to update libraries in files_gms/MODELS/external_libs.'
        print output
        print '    </system-out>'
        print '  </testcase>'
    else:
        print '  <testcase classname="check_for_library_change.py" name="' + test_name + '" line="1" />'

        
def print_xml_tail():
    print '</testsuite>'

errors = 0
xmdf_diff = get_diff_files(r"..\..\..\external_libs\xmdf", r"..\external_libs\xmdf", ignore=["lib100md"])
if xmdf_diff:
    errors += 1
hdf5_diff = get_diff_files(r"..\..\..\external_libs\hdf5_1.8.1", r"..\external_libs\hdf5_1.8.1", ignore=["lib"])
if hdf5_diff:
    errors += 1

print_xml_head(errors)
print_test_xml(xmdf_diff, "testXmdfLibraryChanged")
print_test_xml(hdf5_diff, "testHdf5LibraryChanged")
print_xml_tail()
