zasm -uwy test.z80 || { echo 'assembly failed' ; exit 1; }
minipro -p CAT28C16A -w test.rom