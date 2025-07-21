\ ANS Forth tests - run all tests

\ Adjust the file paths as appropriate to your system
\ Select the appropriate test harness, either the simple tester.fr
\ or the more complex ttester.fs 

\ Changes: added include of dynamic.fs; disabled unsupported module tests;
\          changed all filenames to originate from "forth" subfolder

include forth/dynamic.fs
here 10000 dup allot empty-memory

CR .( Running ANS Forth and Forth 2012 test programs, version 0.13.4) CR

S" forth/prelimtest.fth" INCLUDED
S" forth/tester.fr" INCLUDED
\ S" forth/ttester.fs" INCLUDED

S" forth/core.fr" INCLUDED
S" forth/coreplustest.fth" INCLUDED
S" forth/utilities.fth" INCLUDED
S" forth/errorreport.fth" INCLUDED
S" forth/coreexttest.fth" INCLUDED
\ S" forth/blocktest.fth" INCLUDED
S" forth/doubletest.fth" INCLUDED
\ S" forth/exceptiontest.fth" INCLUDED
S" forth/facilitytest.fth" INCLUDED
\ S" forth/filetest.fth" INCLUDED
\ S" forth/localstest.fth" INCLUDED
S" forth/memorytest.fth" INCLUDED
\ S" forth/toolstest.fth" INCLUDED
S" forth/searchordertest.fth" INCLUDED
\ S" forth/stringtest.fth" INCLUDED
REPORT-ERRORS

CR .( Forth tests completed ) CR CR


