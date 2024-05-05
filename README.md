# DivTIESUS utilities (dot commands for ESXDOS)
Source code for some dot commands I've written to support the DivTIESUS interface. To generate the binary file, use the [PASMO assembler](https://pasmo.speccy.org/) this way: `pasmo --bin sourcecode.asm BINARYFILE`
- GO3E . This dot command has no parameters. It loads a +3E ROM image from the BIN directory of the SD card and installs it as the new system ROM, at the same time as the automapping feature of DivMMC is disabled. So, a +2A/B/3 computer becomes a full featured +2E/+3E machine without the needing of opening and physically changing the ROM chips. GO3E needs any of dvmen3eE.rom or dvmes3eE.rom (just one of them) to be present in the /BIN directory. Such files can be get from the [The ZX Spectrum +3E Homepage](https://worldofspectrum.org/zxplus3e/). To return to ESXDOS mode, you must press NMI+RESET.
- LOADROM . This dot command has one mandatory parameter: the file name of the ROM image to use. LOADROM loads a 16K ROM image and installs it such it becomes the new system ROM for all effects and purposes. To return to ESXDOS, press NMI+RESET. Visit [this section from the Paul Farrow website](http://www.fruitcake.plus.com/Sinclair/Interface2/Cartridges/Interface2_RC_Cartridges.htm) to download and test some of them.
- MOUSE . This dot command is used to query/modify the configuration of the Kempston mouse emulation mode. Invoked with no arguments, it will show the current state of Kempston mouse emulation and the syntax for parameters.
- WCONF . This dot command has no parameters. It enters an interactive mode to configure the wifi module of DivTIESUS, prompt the user for an access point to connect to, and the password for it. After succesfully configured, AP data is stored within non volatile memory in the wifi module and doesn't need to be entered again.
- NTPDATE . This dot command is a STNP (simple Time Netowork Protocol) client. With no parameters, it queries time.google.com for the current UTC date and time and updates PCF8563 RTC chip registers accordingly. PCF8563 access is done via bit banged I2C protocol. It can accept up to two parameters: a number precedded by + or - changes timezone (e.g. +2 means GMT+2) . An IP address or domain name for a NTP server quries that server instead of time.google.com
