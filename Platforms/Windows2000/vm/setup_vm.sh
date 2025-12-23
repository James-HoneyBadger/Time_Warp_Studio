#!/bin/bash
# Setup script for Windows 2000 VM

VM_DIR="$(dirname "$0")"
DISK_IMG="$VM_DIR/win2k.img"
DISK_SIZE="2G"

if ! command -v qemu-system-i386 &> /dev/null; then
    echo "Error: qemu-system-i386 is not installed."
    exit 1
fi

if [ ! -f "$DISK_IMG" ]; then
    echo "Creating disk image..."
    qemu-img create -f qcow2 "$DISK_IMG" "$DISK_SIZE"
else
    echo "Disk image already exists."
fi

echo "Please provide the path to your Windows 2000 ISO file:"
read -r ISO_PATH

if [ ! -f "$ISO_PATH" ]; then
    echo "Error: ISO file not found at $ISO_PATH"
    exit 1
fi

echo "Launching QEMU to install Windows 2000..."
qemu-system-i386 \
    -m 256 \
    -hda "$DISK_IMG" \
    -cdrom "$ISO_PATH" \
    -boot d \
    -net nic,model=rtl8139 -net user \
    -vga cirrus
