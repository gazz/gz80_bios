./build.sh || { echo 'build failed' ; exit 1; }
minipro -p CAT28C16A -w bios.rom
