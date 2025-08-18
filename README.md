# ForthX16
## Intro
Forth TX16 or ForthX16 is an enhanced port of my older project [Forth Model T](https://github.com/VasylTsv/ForthModelT) for [Commander X16](https://www.commanderx16.com/) and other 6502-based platforms. It is a completely functional Forth 2012 standard implementation. Unlike Forth Model T which used direct threaded model, Forth TX16 is using token threaded model. This was mostly done to minimize the size - one of the goals of the project was to fit the entire interpreter on 8K C64 cartridge.

The other, or rather the main goal of the project was to create an interpreter as compliant to Forth 2012 standard as possible.
## Supported Platforms
The original target platform was Commander X16. However, in the middle of developmente I've realized that the same code would run on Commodore 64 with reasonable effort. Thus C64 became a second platform. The third platform is quite different and it was added separately - [Foenix F256](https://c256foenix.com). It is also 6502-based (sort of), but not derived from C64, so console and file I/O are very different. More platforms may be coming.
## Prerequisites and Building
The instructions below are for Windows-based systems. It should not be difficult to modify them to other platforms as long as ACME assembler is available there.
[ACME assembler](https://sourceforge.net/projects/acme-crossass/files/win32/) is used to build the interpreter from the source. It is expected to be places in a subfolder ASM in the project folder.
Optionally, platform emulators can be used for testing. Scripts assume [VICE](https://vice-emu.sourceforge.io/) for C64 and the [official X16 emulator](https://cx16forum.com/forum/viewtopic.php?t=8443) for Commander X16. These should be placed in subfolders VICE and X16 correspondingly. [Foenix F256 IDE](https://github.com/Trinity-11/FoenixIDE) can be used to test that platform, but there is no special support for it in the scripts. Just install the IDE and point the "SD card" to the location of the binaries.

* `makeprg.bat` will build PRG compatible with both C64 and X16
* `makecart.bat` will build cartridge image for C64
* `makediskc64.bat` builds a disk image for C64. It can be used on X16, but there is not much point
* `makef256.bat` build PGZ executable for F256

* `testcart.bat` will build and load the cartridge in VICE
* `testdiskc64.bat` tests the disk image in VICE as well
* `testprg.bat` will build and start PRG file in X16 emulator

There are pre-build binaries in the `binary` folder.
## The Language Support

This implementation closely follows the Forth 2012 Standard. The following describes the list
of supported words grouped per the said standard.

### Core words
```
!			#			#>			#S			'			(			*			*/
*/MOD		+			+!			+LOOP		,			-			.			."
/			/MOD		0<			0=			1+			1-			2!			2*
2/			2@			2DROP		2DUP		2OVER		2SWAP		:			;
<			<#			=			>			>BODY		>IN			>NUMBER		>R
?DUP		@			ABORT		ABORT"		ABS			ACCEPT		ALIGN		ALIGNED
ALLOT		AND			BASE		BEGIN		BL			C!			C,			C@
CELL+		CELLS		CHAR		CHAR+		CHARS		CONSTANT	COUNT		CR
CREATE		DECIMAL		DEPTH		DO			DOES>		DROP		DUP			ELSE
EMIT		ENVIRONMENT? EVALUATE	EXECUTE		EXIT		FILL		FIND		FM/MOD
HERE		HOLD		I			IF			IMMEDIATE	INVERT		J			KEY
LEAVE		LITERAL		LOOP		LSHIFT		M*			MAX			MIN			MOD
MOVE		NEGATE		OR			OVER		POSTPONE	QUIT		R>			R@
RECURSE		REPEAT		ROT			RSHIFT		S"			S>D			SIGN		SM/REM
SOURCE		SPACE		SPACES		STATE		SWAP		THEN		TYPE		U.
U<			UM*			UM/MOD		UNLOOP		UNTIL		VARIABLE	WHILE		WORD
XOR			[			[']			[CHAR]		]
```
### Core Extension words
```
.(			.R			0<>			0>			2>R			2R>			2R@			:NONAME
<>			?DO			ACTION-OF	AGAIN		BUFFER:		C"			CASE		COMPILE,
DEFER		DEFER!		DEFER@		ENDCASE		ENDOF		ERASE		FALSE		HEX
HOLDS		IS			MARKER		NIP			OF			PAD			PARSE		PARSE-NAME
PICK		REFILL		RESTORE-INPUT ROLL		S\"			SAVE-INPUT	SOURCE-ID	TO
TRUE		TUCK		U.R			U>			UNUSED		VALUE		WITHIN		[COMPILE]
\
```
All of Core and Core Extension words are supported and perform very close to the Standard.
Note that the cell size for CELL+ and CELLS is 2 bytes as the system is inherently 16-bit.
However, compilation tokens may be just one byte in size. This does not contradict the standard.
The word ENVIRONMENT? is recognized but does nothing. It is rarely used and defined in a very
strange way largely inconsistent with the rest of the language.

There are a few non-standard words supported by this implementation:
* `0` `-1` `1` `2` - self-explanatory
* `PLACE` and `+PLACE` - these are two very useful string manipulation words from [a Standard proposal](https://forth-standard.org/proposals/place-place)
* `?COMP` - check if current mode is compilation, abort otherwise
* `?STACK` - check the data stack for overflow/underflow
* `UD/MOD ( ud1 u1 -- u2 ud2 )` - commonly used word to divide unsigned double `ud1` by unsigned `u1`. 'u2' is remainder and 'ud2' is quotient. Notice that unlike `UM/MOD`, the quotient is a double.

### Block words
Not supported and not planned. This set makes more sense for embedded systems without existing filesystems, so not very useful
for this implementation.

### Double-Number words
```
2CONSTANT	2LITERAL	2VARIABLE	D+			D-			D.			D.R			D0<
D0=			D2*			D2/			D<			D=			D>S			DABS		DMAX
DMIN		DNEGATE		M*/			M+
```
### Double-Number extension words
```
2ROT		2VALUE		DU<
```
All Double-Number and Extension words are completely supported and compliant.

### Exception words
Not supported and not planned. This set is optional, but the implementation may be too heavy for 16-bit core.

### Facility words
No words from the main Facility set are currently supported.

### Facility extension words
Partial support.
```
+FIELD   BEGIN-STRUCTURE   CFIELD:   END-STRUCTURE   FIELD:
```
Only a small group of Facility words is supported, but those are compliant with the standard.
The rest of the group is considered for possible extension in the future. Most of those words
are quite simple but they would take too much RAM just for names.

### File-Access words
All File-Access words are supported, but not all of them work completely or at all on some platforms. Check platform-specific notes for details
```
( (extended) BIN		CLOSE-FILE	CREATE-FILE	DELETE-FILE	FILE-POSITION FILE-SIZE	INCLUDE-FILE
INCLUDED	OPEN-FILE	R/O			R/W			READ-FILE	READ-LINE	REPOSITION-FILE RESIZE-FILE
S" (extended) SOURCE-ID	W/O			WRITE-FILE	WRITE-LINE
```
### File-Access extension words
```
FILE-STATUS	FLUSH-FILE	INCLUDE		REFILL (extended) RENAME-FILE	REQUIRE		REQUIRED S\" (extended)
```
### Floating-Point words
Not supported and not planned.

### Local words
Not supported and not planned. I find this set a very questionable addition to the standard.

### Memory-Allocation
Supported as an extension. Include DYNAMIC.FS and initialize that library for complete support.

### Programming-Tools
```
.S   ?   WORDS
```
### Programming-Tools extension
Small subset is supported.
```
BYE   FORGET
```
The following are not supported:
```
DUMP		SEE

;CODE		AHEAD		ASSEMBLER	CODE		CS-PICK		CS-ROLL		EDITOR		N>R
NAME>COMPILE NAME>INTERPRET	NAME>STRING NR>		STATE (extended)		SYNONYM		TRAVERSE-WORDLIST
[DEFINED]	[ELSE]		[IF]		[THEN]		[UNDEFINED]
```
Some of these are planned for extensions.

### Search-Order
All Search-Order and Extension words are completely supported.
```
DEFINITIONS FIND (extended) FORTH-WORDLIST GET-CURRENT GET-ORDER SEARCH-WORDLIST SET-CURRENT
SET-ORDER WORDLIST
```
### Search-Order extension
```
ALSO   FORTH   ONLY   ORDER   PREVIOUS
```
### String
```
/STRING		BLANK		CMOVE		CMOVE>		COMPARE		SLITERAL
```
Most words with exception for `-TRAILING` and `SEARCH` are supported. None of the three extension words
(`REPLACES` `SUBSTITUTE` `UNESCAPE`) are supported at this time. Support through extension is considered.
### Extended-Character words
Not supported and not planned.

## Platform-Specific Notes

### Commodore 64
* KEY echoes the input character
* BYE reboots the interpreter in cartridge mode and does nothing in PRG.
* R/W mode is not supported, the word will do the same as W/O.
* FILE-POSITION FILE-SIZE	REPOSITION-FILE RESIZE-FILE are not functional and will just fail.
* Opening file for write does not overwrite an existing file. This is a default system behavior but it feels quite wrong.
* C64 file handling makes it very hard to distinguish 0-byte file from a non-existing one. INCLUDE will just fail on empty files.

### Commander X16
All Commodore 64 notes apply except for BYE which properly returns to Basic.

### Foenix F256
* BYE hangs the interpreter.
* R/W mode is not supported, the word will do the same as W/O.
* FILE-POSITION FILE-SIZE REPOSITION-FILE RESIZE-FILE are not functional and will just fail.

## Other Notes

A modified copy of dynamic memory support package can be found in `dynamic`. This brings in standard Memory-Allocation set. The modification was needed to fix a non-compliant issue - allocations of negative size were not properly rejected.

A modified copy of Forth test suite is in `tests` - copy files from there to the file system of Commander X16 and start it with `INCLUDE RUNTESTS.FTH`. The current version should run all tests without errors. The runtime on the emulator is about 4 minutes on Commander X16 (and a LOT more on C64).

A practically stock copy of Forth test suite is in `tests-F256` as that platform uses ASCII and does not need character hacks.

A few examples and benchmarks are in `other` - `BENCH.FTH` and `ERASTO.FTH` are old benchmarking programs calculating primes, practically unchanged (`BENCH` had a few `ENDIF`s replaced by `THEN`s). `RC4TEST.FTH` is a sample from the [Wikipedia](https://en.wikipedia.org/wiki/Forth_(programming_language)) page, unmodified.

## Roadmap

I don't have any particular time estimates, but I do have some plans. The following items can be
considered my roadmap for the project.

### Cleanups
The project was originally based on the source code written in one weekend so it was already quite messy.
Multiple optimization passes may have improved the size, but did not make it cleaner. The latest addition
of F256 target was a bit rushed as well. As a result, the code is not in the best shape, needs some
cleaning and documenting. Mind you, it is practically impossbile to make a largish 6502 assembly project
to look clean, there are always some tricks there that would look questionable. So, the best effort here.

### Optimization in F256 specific section
F256 has a large block of platform specific code for console and file I/O. The file I/O may be more or
less straightforward, but console I/O may still shed some bytes.

### Atari 8-bit target
My first computer was Atari 65XE, so it is only right to support that. Now that I have support for two
very different platforms the actual platform dependencies are easy to identify and port.

### Toolkits
These are required for any usable Forth system. I am already looking into inline assembler. Editor is
a reasonable thing to have. Some parts of the Standard may be added through a toolkit expansion.
Finally, there are also a lot of platform-specific things that would make the system a lot more usable.
