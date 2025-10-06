\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

\ Modifications for Forth TX16:
\ - Disabled unsupported component tests
\ - Changed all filenames to conform the filesystem requirements

\ Forcing lower case on Commander X16/C64
14 emit

CR .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) CR

S" FORTH/PRELIM.FTH" INCLUDED
S" FORTH/TESTER.FR" INCLUDED
\ S" ttester.fs" INCLUDED

S" FORTH/CORE.FR" INCLUDED
S" FORTH/COREPLUS.FTH" INCLUDED
S" FORTH/UTIL.FTH" INCLUDED
S" FORTH/ERRORREP.FTH" INCLUDED
S" FORTH/COREEXT.FTH" INCLUDED
\ S" blocktest.fth" INCLUDED
S" FORTH/DOUBLE.FTH" INCLUDED
\ S" exceptiontest.fth" INCLUDED
S" FORTH/FACILITY.FTH" INCLUDED
\ S" filetest.fth" INCLUDED
\ S" localstest.fth" INCLUDED
\ S" FORTH/MEMORY.FTH" INCLUDED
\ S" toolstest.fth" INCLUDED
S" FORTH/SEARCH.FTH" INCLUDED
\ S" stringtest.fth" INCLUDED
REPORT-ERRORS

CR .( Forth tests completed ) CR CR


