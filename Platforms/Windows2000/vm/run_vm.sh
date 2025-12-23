#!/bin/bash
# Run script for Windows 2000 VM

VM_DIR="$(dirname "$0")"
DISK_IMG="$VM_DIR/win2k.img"

if [ ! -f "$DISK_IMG" ]; then
    echo "Error: Disk image not found. Run setup_vm.sh first."
    exit 1
fi

qemu-system-i386 \
    -m 256 \
    -hda "$DISK_IMG" \
    -net nic,model=rtl8139 -net user \
    -vga cirrus
