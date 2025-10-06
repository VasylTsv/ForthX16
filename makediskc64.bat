del fortht.prg
del fortht.d64
.\asm\acme --cpu 6502 --outfile fortht.prg --format cbm buildprg.asm

.\vice\bin\c1541 -format "forth,01" d64 fortht.d64 -attach fortht.d64 -write fortht.prg
.\vice\bin\c1541 -attach fortht.d64 -write tests\runtests.fth "runtests.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\prelim.fth "prelim.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\tester.fr "tester.fr,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\core.fr "core.fr,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\coreplus.fth "coreplus.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\util.fth "util.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\errorrep.fth "errorrep.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\coreext.fth "coreext.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\double.fth "double.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\facility.fth "facility.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write tests\search.fth "search.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write toolkit\assembler.fth "assembler.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write dynamic\dynamic.fs "dynamic.fs,s"
.\vice\bin\c1541 -attach fortht.d64 -write other\bench.fth "bench.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write other\erasto.fth "erasto.fth,s"
.\vice\bin\c1541 -attach fortht.d64 -write other\rc4test.fth "rc4test.fth,s"
