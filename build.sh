zasm -uwy bios.asm || { echo 'assembly failed' ; exit 1; }
xxd bios.bin