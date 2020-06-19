zasm -uwy move_around.z80 || { echo 'assembly failed' ; exit 1; }
xxd move_around.bin
python /darbi/pyWorkspace/z80_upload_ram/flash.py /dev/cu.usbserial-AG0JNMSW move_around.bin --baud 76800