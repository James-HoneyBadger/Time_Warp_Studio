// SPDX-License-Identifier: MIT
//
// The HB-2's map, as data. One list, included by whoever needs it: the bus routes
// with it and the monitor prints it. A second copy of "where is video memory" is how
// a manual and a machine end up disagreeing inside a release, so there is one.
//
// HB2_REGION(name, base, size, kind, device offset, compat only)
//
//   base     the address a program uses, in the 24-bit space
//   size     bytes the region answers for
//   kind     which device owns it (see hb2/device.hpp)
//   offset   where in that device's own address space the region starts; the video
//            chip answers on three windows, and 1982's screen and attributes are two
//            of them
//   compat   true when the region only exists in MODE 0, the 1982 map. Those rows
//            overlay RAM, which is why the bus consults them first.

// The 1982 map, kept whole. A program written for the HB-1 runs on this machine
// without being told it is on a different one -- and the `retro_*` suites are what
// tests that promise rather than the comment above it.
HB2_REGION("screen", 0x008000, 0x0007D0, Video, 0x0040000, true)
HB2_REGION("attributes", 0x009000, 0x0007D0, Video, 0x0041000, true)
HB2_REGION("io1982", 0x00A000, 0x000100, Video, 0x0042000, true)
HB2_REGION("rom1982", 0x00F000, 0x001000, Rom, 0x0000000, true)

// The 1992 map.
HB2_REGION("ram", 0x000000, 0x100000, Ram, 0x0000000, false)
HB2_REGION("vram", 0x100000, 0x025800, Video, 0x0000000, false)
HB2_REGION("registers", 0x200000, 0x001000, Video, 0x0042000, false)
HB2_REGION("audio", 0x201000, 0x001000, Audio, 0x0000000, false)
HB2_REGION("input", 0x202000, 0x000100, Input, 0x0000000, false)
HB2_REGION("rtc", 0x202100, 0x000020, Rtc, 0x0000000, false)
HB2_REGION("uart", 0x202200, 0x000010, Uart, 0x0000000, false)
HB2_REGION("rom", 0x210000, 0x010000, Rom, 0x0000000, false)
