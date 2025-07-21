del fthtx16.prg
del tests.d64
.\asm\acme --cpu 6502 --outfile fthtx16.prg --format cbm buildprg.asm

.\vice\bin\c1541 -format "forth,01" d64 tests.d64 -attach tests.d64 -write fthtx16.prg
.\vice\bin\c1541 -attach tests.d64 -write tests\runtests.fth "runtests.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\prelim.fth "prelim.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\tester.fr "tester.fr,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\core.fr "core.fr,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\coreplus.fth "coreplus.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\util.fth "util.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\errorrep.fth "errorrep.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\coreext.fth "coreext.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\double.fth "double.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\facility.fth "facility.fth,s"
.\vice\bin\c1541 -attach tests.d64 -write tests\search.fth "search.fth,s"
