This a modified subset of Gerry Jackson's Forth test suite that can be found at:
https://github.com/gerryjackson/forth2012-test-suite
Only the relevant files have been included (tests for unsupported component have been omitted for now). Files are
modified for compatible line endings and brought to uppercase. There are a few local changes disabling tests
that are valid only for lowercase supporting systems. Only the relevant tests are enabled in RUNTESTS.FTH. The
dynamic memory tests is present but disabled - it can be reenabled by uncommening the corresponding line, but
make sure the dynamic-memory-allocation package is loaded and initialized before starting the test suite.
At this point all supported tests should pass with Forth X16 with no errors.