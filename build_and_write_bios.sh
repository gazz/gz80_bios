zasm -uwy bios.z80 || { echo 'assembly failed' ; exit 1; }
minipro -p CAT28C16A -w bios.rom