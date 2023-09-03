# ForthX16
Forth TX16 or ForthX16 is an enhanced port of my older project [Forth Model T](https://github.com/VasylTsv/ForthModelT) for [Commander X16](https://www.commanderx16.com/). It is a completely functional Forth 2012 core for Commander X16 with most features from Forth Model T included. The only two limitations are related to the limited capabilities of the file system on X16 and to the lack of lowercase in C64/X16 charset (it is more nuanced, but it may not be worth effort supporting it). There are also some minor additions and improvements.

The root contains just one assembly file `fthtx16.asm`. It is proper 6502 assebly in [ACME assembler](https://sourceforge.net/projects/acme-crossass/files/win32/) format. Use the following command line to build the binary:
```
acme --cpu 6502 --outfile fthtx16.prg --format cbm fthtx16.asm
```
The resulting PRG file can be copied to Commander X16 file system and loaded as usual. It is thoroughly tested with the emulator, but I don't have access to the actual hardware yet, so let me know if it breaks on metal.

Note that the format is compatible with Commodore 64, but it has not been tested on that system. It might actually work, but YMMV.

A prebuilt binary can be found in `binary`folder.

A modified copy of dynamic memory support package can be found in `dynamic`.

A modified copy of Forth test suite is in `tests` - copy files from there to the file system of Commander X16 and start it with `INCLUDE RUNTESTS.FTH`. The current version should run all tests without errors. The runtime on the emulator is about 4 minutes.

A few examples and benchmarks are in `other` - `BENCH.FTH` and `ERASTO.FTH` are old benchmarking programs calculating primes, practically unchanged (`BENCH` had a few `ENDIF`s replaced by `THEN`s). `RC4TEST.FTH` is a sample from the [Wikipedia](https://en.wikipedia.org/wiki/Forth_(programming_language)) page, unmodified.

There are a few known issues with the current release.
* For some reason, open file does not seem to properly return an error if file is missing (at least, in the emulator). This causes a lot of small issues around file handling. To be fixed in the future.
* Implementation of KEY is incorrect, it is supposed to asynchronously get key without echo. It does it synchronously and with echo in this version. This was also the case in Model T, but there the was an excuse that the desired functionality cannot be implemented with standard C features only. It can certainly be implemented on the hardware, but there are some oddities of X16 I still need to trace - the keyboard buffer is not where it was on C64, and the new KERNAL calls are not sufficient to implement this functionality.

This is work in progress. I am planning to do a few rounds of updates. First of all, the Model T was done in a few evenings, there are a number of cut corners and rather questionable implementations there, some cleanups needed. There are known issues that need to be addressed (above). I also want to get the binary smaller - it is at 11K now, but my goal is to get it to 8K and make it relocatable if possible, that would open an opportunity to easily move it to the ROM area. Finally, I should test it with C64, it might just work there.
The other plans include porting this to Atari computers - that would only require a limited number of changes around the file system handling and other I/O.
