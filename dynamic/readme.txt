This is slightly modified version of dynamic-memory-allocation package by Ulrich Hoffmann from the Forth repository:
http://theforth.net/package/dynamic-memory-allocation
The following modifications have been made: line ending changed to be compliant, case converted to uppercase
(this Forth implementation does not support lowercase due to system limitations), and a couple of wordds got
a check for negative parameter added - the standard does not require that, but the test suite would fail.
Only the main source file and the license from the package are included here. Check the original location for
documentation and updated versions. 