del fthtx16.rom
del fthtx16.crt
.\asm\acme --cpu 6502 --outfile fthtx16.rom --format plain buildcrt.asm
.\vice\bin\cartconv -t normal -n "Forth TX16" -i fthtx16.rom -o fthtx16.crt

.\vice\bin\x64sc.exe -cartcrt fthtx16.crt
 
