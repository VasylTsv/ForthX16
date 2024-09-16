# ForthX16
Forth TX16 or ForthX16 is an enhanced port of my older project [Forth Model T](https://github.com/VasylTsv/ForthModelT) for [Commander X16](https://www.commanderx16.com/). It is a completely functional Forth 2012 core for Commander X16 with most features from Forth Model T included. The only two limitations are related to the limited capabilities of the file system on X16 and to the lack of lowercase in C64/X16 charset (it is more nuanced, but it may not be worth effort supporting it). There are also some minor additions and improvements.

One of the goals of this project is to make the core functional on C64 and small enough to fit into an 8K cartridge. The code is heavily optimized for size, but it may not be the fastest as a result.

The root contains just one assembly file `fthtx16.asm`. It is a proper 6502 assembly in [ACME assembler](https://sourceforge.net/projects/acme-crossass/files/win32/) format. Use the following command line to build the binary:
```
acme --cpu 6502 --outfile fthtx16.prg --format cbm fthtx16.asm
```
The resulting PRG file can be copied to Commander X16 file system (or C64) and loaded as usual. It is thoroughly tested with the emulator, but I don't have access to the actual hardware yet, so let me know if it breaks on metal.

A prebuilt binary can be found in `binary`folder.

A modified copy of dynamic memory support package can be found in `dynamic`.

A modified copy of Forth test suite is in `tests` - copy files from there to the file system of Commander X16 and start it with `INCLUDE RUNTESTS.FTH`. The current version should run all tests without errors. The runtime on the emulator is about 4 minutes on Commander X16 (and a LOT more on C64).

A few examples and benchmarks are in `other` - `BENCH.FTH` and `ERASTO.FTH` are old benchmarking programs calculating primes, practically unchanged (`BENCH` had a few `ENDIF`s replaced by `THEN`s). `RC4TEST.FTH` is a sample from the [Wikipedia](https://en.wikipedia.org/wiki/Forth_(programming_language)) page, unmodified.

There are a few known issues with the current release.
* Opening a file for write does not overwrite an existing file. This is actually the default system behavior, but it feels quite wrong now.
* A few file system words cannot be implemented on C64/X16 using the existing Kernal facilities - mainly those related to the file position and advanced access modes.
* C64 file handling has a logical bug making it very hard to handle 0-byte files properly. INCLUDE will fail on such files.
* Unlike X16, C64 does not have a large user area on the zero page. This results in Basic being broken by Forth, so BYE does not work properly

This is work in progress. I am planning to do a few rounds of updates. First of all, the Model T was done in a few evenings, there are a number of cut corners and rather questionable implementations there, some cleanups needed. There are known issues that need to be addressed (above). Need to write a better documentation as well instead of these quick notes.

The other plans include porting this to Atari computers - that would only require a limited number of changes around the file system handling and other I/O.
