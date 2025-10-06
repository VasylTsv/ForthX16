del fortht.rom
del fortht.crt
.\asm\acme --cpu 6502 --outfile fortht.rom --format plain buildcrt.asm
.\vice\bin\cartconv -t normal -n "Forth T for C64" -i fortht.rom -o fortht.crt
 
