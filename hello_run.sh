zasm -uwy hello.s || { echo 'assembly failed' ; exit 1; }
xxd hello.bin
python /darbi/pyWorkspace/z80_upload_ram/flash.py /dev/cu.usbserial-AG0JNMSW hello.bin --baud 76800