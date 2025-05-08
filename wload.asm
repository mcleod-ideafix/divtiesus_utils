; Machine code load and execute utility for ZXUNO / ESP8266 module.
; Copyright (C)2020 Miguel Angel Rodriguez Jodar (mcleod_ideafix). ZX Projects.
;
; This program is free software: you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free Software
; Foundation, either version 3 of the License, or (at your option) any later
; version.
;
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along with
; this program.  If not, see <http://www.gnu.org/licenses/>.

ZXUNOADDR                    equ 0fc3bh                   ;
ZXUNODATA                    equ 0fd3bh                   ;Register definition
UARTDATA                     equ 0c6h                     ;for ZXUNO UART
UARTSTAT                     equ 0c7h                     ;

TIMEOUT                      equ 10000

CR                           equ 13
LF                           equ 10
NULL                         equ 0

include "esxdos.inc"
include "errors.inc"

ATSend                       macro                        ;Just a quicker way to write
                             call WriteATCommand          ;a call to this routine
                             endm                         ;

Print                        macro                        ;
                             call PrintString             ;Ditto
                             endm                         ;

WaitResponse                 macro                        ;
                             call ReadAndParseResponse    ;Ditto
                             endm                         ;

                             org 2000h                    ;ESXDOS DOT command

;------------------------------------------------------------------------------

Main                         call CleanUART               ;clear any spurious data

                             ATSend                       ;mute echo of commands
                             db "ATE0",CR,LF,NULL
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             call GetLocalIP              ;try to get the local IP
                             jp c,ResetUART

CIFSRok                      ld hl,IPAddrBuffer           ;
                             ld de,IPAllZeros             ;if we can get an IP and it is not 0.0.0.0, then
                             call CmpStrings              ;
                             jp nz,OKWifiConnect          ;assume we are connected and skip all the init process

ResetUART                    Print
                             db "Initializing wifi module...",CR,NULL

                             ATSend
                             db "AT+RST",CR,LF,NULL            ;reset the wifi module
                             WaitResponse
                             db "ready",NULL
                             db NULL

                             ATSend
                             db "ATE0",CR,LF,NULL              ;mute command echo (was unmuted after reset)
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             call GetLocalIP              ;try to get the local IP
                             jp c,ErrorCWJAPcommand

                             ld hl,IPAddrBuffer           ;
                             ld de,IPAllZeros             ;if we can get an IP and it is not 0.0.0.0, then
                             call CmpStrings              ;
                             jp nz,OKWifiConnect          ;assume we are connected and skip all the init process

ErrorCWJAPcommand            Print
                             db "Couldn't connect to AP. Exiting...",CR,NULL
                             jp ExitByError

OKWifiConnect                Print
                             db "AP connection established.",CR,"Creating TCP server...",CR,NULL

                             ATSend
                             db "AT+CIPMUX=1",CR,LF,NULL      ;allow multiple connections (needed for creating a TCP server)
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             ATSend
                             db "AT+CIPSERVER=1,6912",CR,LF,NULL  ;create TCP server at port 6912 ;)
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             cp 0
                             jr z,CIPSERVERok
                             Print
                             db "Couldn't open server",CR,NULL
                             jp ExitByError

CIPSERVERok                  Print
                             db "TCP server created.",CR,"Getting local IP...",CR,NULL

                             ATSend
                             db "AT+CIPSTO=2",CR,LF,NULL      ;set a small timeout (2 s.)
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             call GetLocalIP          ;get the local IP again. It should not be 0.0.0.0 this time

                             Print
                             db CR,"Send your data to:",CR,"IP address: ",NULL
                             ld hl,IPAddrBuffer
                             call PrintStringHL
                             call CleanUART
                             Print
                             db CR,"Port: 6912/TCP",CR,NULL

                             ld a,1
                             ld (FirstBlock),a        ;signal that the next data block is the first data block, to get origin and entry point

LoopReceiveProgram           WaitResponse
                             db "+IPD,",NULL              ;new data received
                             db ",CLOSED",NULL            ;end of
                             db ",CONNECT FAIL",NULL      ;connection
                             db NULL

                             cp 0
                             jr z,ManageDataBlock
                             jp ManageCloseConnection
                             jr LoopReceiveProgram

ManageDataBlock              call ReadByte                ;skip read data
                             cp ","                       ;until we found
                             jr nz,ManageDataBlock        ;a comma

                             ld hl,0                      ;HL=length of block. Parsing numeric string now
ParseDigitLength             call ReadByte                ;read digit
                             cp ":"                       ;end of number?
                             jr z,EndDigit
                             sub "0"                      ;convert ASCII digit to value
                             ld d,h                       ;
                             ld e,l                       ;
                             add hl,hl                    ;HL = HL*10
                             add hl,hl                    ;
                             add hl,hl                    ;
                             ex de,hl                     ;
                             add hl,hl                    ;
                             add hl,de                    ;
                             ld e,a
                             ld d,0
                             add hl,de                    ;HL = HL+value
                             jr ParseDigitLength          ;

EndDigit                     ld a,(FirstBlock)
                             or a
                             jr z,NotFirstBlock           ;skip origin and
                             xor a                        ;entry point retrieving if not first block
                             ld (FirstBlock),a

                             push hl                      ;save block length
                             call ReadByte                ;
                             ld l,a                       ;
                             call ReadByte                ;get program origin
                             ld h,a                       ;and save it
                             ld (ProgramPosition),hl      ;
                             call ReadByte                ;get program entry
                             ld l,a                       ;point and save it
                             call ReadByte                ;
                             ld h,a                       ;
                             ld (ProgramEntryPoint),hl    ;
                             pop hl                       ;restore block length

                             dec hl                       ;
                             dec hl                       ;count 4 bytes already
                             dec hl                       ;read this time
                             dec hl                       ;

NotFirstBlock                ld ix,(ProgramPosition)      ;
LoopReceiveProgramBytes      ld a,h                       ;
                             or l                         ;loop to read HL bytes
                             jr z,EndBlock                ;from UART to the memory
                             call ReadByte                ;block pointed by IX
                             ld (ix),a                    ;
                             inc ix                       ;
                             dec hl                       ;
                             jr LoopReceiveProgramBytes   ;
EndBlock                     ld (ProgramPosition),ix
                             ld a,"."                     ;
                             rst 10h                      ;a dot on screen to show progress
                             jp LoopReceiveProgram

ManageCloseConnection        Print
                             db CR,"Program received!",CR,NULL

                             call CleanUART
                             ATSend
                             db "AT+CIPSERVER=0",CR,LF,NULL     ;close TCP server
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             ld hl,(ProgramEntryPoint)          ;load entry point in HL
                             ld a,h
                             or l
                             jp nz,1ffbh                        ;exit ESXDOS and jump to code at HL if not zero
                             or a
                             ret

ExitByError                  ld hl,Log                          ;if exiting due to network failure
                             call PrintStringHL                 ;print reception log to diagnose the cause
                             or a
                             ret

                             ld hl,WifiErrorMsg                 ;if exiting due to IW.CFG not found
                             xor a                              ;just notify it through ESXDOS
                             scf
                             ret

;------------------------------------------------------------------------------

WaitByteRcv                  proc                               ;Busy wait for byte reception
                             local LoopWait                     ;with timeout
                             ld bc,ZXUNOADDR
                             ld a,UARTSTAT
                             ld de,TIMEOUT
                             out (c),a
                             inc b
LoopWait                     in a,(c)
                             or a
                             ret m                              ;Byte available. CF=0
                             dec de
                             ld a,d
                             or e
                             jr nz,LoopWait
                             scf                                ;Timeout. CF=1
                             ret
                             endp

;------------------------------------------------------------------------------

ReadByte                     proc                               ;Read byte from UART, if available
                             call WaitByteRcv                   ;Is there byte available
                             ret c                              ;return if not (CF=1)
                             dec b
                             ld a,UARTDATA
                             out (c),a
                             inc b
                             in a,(c)
                             or a
                             ret                                ;Read it and return it. CF=0
                             endp

;------------------------------------------------------------------------------

WriteByte                    proc                               ;Write byte to UART, waiting for empty write buffer first
                             local LoopWait,ReadyToWrite
                             ex af,af'
                             ld bc,ZXUNOADDR
                             ld a,UARTSTAT
                             out (c),a
                             ld de,TIMEOUT
                             inc b
LoopWait                     in a,(c)
                             bit 6,a                            ;bit 6 is write buffer status
                             jr z,ReadyToWrite
                             dec de
                             ld a,d
                             or e
                             jr nz,LoopWait
                             ex af,af'
                             ret
ReadyToWrite                 dec b
                             ld a,UARTDATA
                             out (c),a
                             inc b
                             ex af,af'
                             out (c),a
                             ret
                             endp

;------------------------------------------------------------------------------

CleanUART                    proc                               ;read and discard bytes until there is no left bytes in the wifi module
                             local LoopReadAndThrow
LoopReadAndThrow             call ReadByte
                             ret c
                             jr LoopReadAndThrow
                             endp

;------------------------------------------------------------------------------

WriteATCommand               proc                               ;send a string of NULL terminated bytes (normally an AT command) to the UART
                             ex (sp),hl                         ;such string follows the CALL instruction (as an inmediate operand)
                             call WriteATCommandHL
                             ex (sp),hl
                             ret
                             endp

WriteATCommandHL             proc                               ;alternate call method: HL=pointer to string
                             local LoopWrite
LoopWrite                    ld a,(hl)
                             or a
                             inc hl
                             ret z
                             call WriteByte
                             jr LoopWrite
                             endp

;------------------------------------------------------------------------------

PrintString                  proc                               ;print string to screen (ESXDOS opens channel 2)
                             ex (sp),hl                         ;such string follows the CALL instruction (as an inmediate operand)
                             call PrintStringHL
                             ex (sp),hl
                             ret
                             endp

PrintStringHL                proc                               ;alternate call method: HL=pointer to string
                             local LoopPrint
LoopPrint                    ld a,(hl)
                             or a
                             inc hl
                             ret z
                             rst 10h
                             jr LoopPrint
                             endp

;------------------------------------------------------------------------------

ReadAndParseResponse         proc                                ;read bytes until any of the programmed responses is found
                             ex (sp),hl                          ;responses are NULL terminated strings, one after the other. Mark the end of responses with a NULL string.
                             ld de,ResponseTable                 ;HL points to the first of the strings. DE points to a table which will store the address of each string as they are identified
ReadNextResponse             ld a,(hl)
                             or a
                             jr z,EndInitTable                   ;if this is the NULL string, we have finished parsing the responses
                             ld a,l                              ;
                             ld (de),a                           ;if not, take the current value of HL and store in (DE,DE+1)
                             inc de                              ;
                             ld a,h                              ;
                             ld (de),a                           ;
                             inc de
                             inc hl
ReadOverResponse             ld a,(hl)                           ;
                             or a                                ;read over the entire string until we find its NULL
                             inc hl                              ;
                             jr nz,ReadOverResponse              ;
                             jr ReadNextResponse                 ;Go for another string
EndInitTable                 inc hl                              ;
                             ex (sp),hl                          ;Store the updated return address to the stack
                             ld (de),a                           ;
                             inc de                              ;Mark the end of the response table with a 0000 address
                             ld (de),a                           ;

                             ld hl,IndexTable                    ;
                             ld de,IndexTable+1                  ;Clear tthe index table (offset to each response string as
                             ld bc,7                             ;each one matches succesive characters read from the network
                             ld (hl),0                           ;
                             ldir                                ;

                             ld hl,Log                           ;HL points to a memory area where bytes received are stored

LoopRecSingleByte            call ReadByte                       ;Receive one byte (try to)
                             jr nc,ByteReceived
                             ld a,7fh
                             in a,(0feh)
                             rra
                             jp nc,ExitBreak                     ;Exit if SPACE pressed
                             jr LoopRecSingleByte

ByteReceived                 ld (hl),a                           ;Store received byte in the log
                             inc hl
                             push hl

                             ld b,a                              ;Save received byte in B
                             ld c,0                              ;C counts which response string we are using for comparison
                             ld hl,ResponseTable
                             ld de,IndexTable
BeginParseResponse           push hl                             ;
                             ld a,(hl)                           ;HL = address of next response string
                             inc hl                              ;
                             ld h,(hl)                           ;
                             ld l,a
                             or h
                             jr z,EndStrings                     ;if we reached the end of the response table
                             ld a,(de)                           ;
                             push de                             ;
                             ld e,a                              ;Add the current value of the index table for this string
                             ld d,0                              ;to the string address to point to the current character in that string
                             add hl,de                           ;
                             ld a,(hl)
                             cp b                                ;Compare current character with byte from network
                             jr z,MatchFound                     ;character match found

                             pop de                              ;
                             xor a                               ;character match not found. Reset index table entry
                             ld (de),a                           ;for this string
                             jr NextResponse

MatchFound                   pop de                              ;
                             ld a,(de)                           ;Increase index table entry for this string
                             inc a                               ;
                             ld (de),a                           ;
                             inc hl
                             ld a,(hl)                           ;is it the end of the string? then we have found
                             or a                                ;a string match
                             jr z,StringMatchFound               ;

NextResponse                 inc c                               ;
                             inc de                              ;
                             pop hl                              ;go for another string
                             inc hl                              ;
                             inc hl                              ;
                             jr BeginParseResponse               ;

EndStrings                   pop hl                              ;end of strings. Go for another byte
                             pop hl                              ;from the network
                             jr LoopRecSingleByte                ;

StringMatchFound             pop hl                              ;
                             pop hl                              ;string match found. Return with A=number of string that matched
                             ld (hl),0                           ;also NULL terminates log
                             ld a,c                              ;
                             ret

ExitBreak                    ld (hl),0                           ;NULL terminate log and return with A=255 (SPACE pressed)
                             ld a,255                            ;
                             ret
                             endp

;------------------------------------------------------------------------------

GetLocalIP                   proc                                ;Gets current local IP address (ASCII string) and store it at IPAddrBuffer
                             local CIFSRok,LoopGetIP
                             local EndLoopGetIP
                             ATSend
                             db "AT+CIFSR",CR,LF,NULL            ;Use AT+CIFSR to do it
                             WaitResponse
                             db '+CIFSR:STAIP,"',NULL            ;and look for response +CIFSR:STAIP,"
                             db "ERROR",CR,LF,NULL
                             db NULL

                             cp 0
                             jr z,CIFSRok
                             scf                                 ;CF=1 if error
                             ret

CIFSRok                      ld hl,IPAddrBuffer                  ;read and parse the rest of the response: <ip address>"
LoopGetIP                    call ReadByte
                             cp 34                               ;close quote
                             jr z,EndLoopGetIP
                             ld (hl),a                           ;store and go for the next
                             inc hl                              ;character
                             jr LoopGetIP
EndLoopGetIP                 ld (hl),0                           ;NULL terminate the IP address string
                             or a                                ;CF=0 success
                             ret
                             endp

;------------------------------------------------------------------------------

CmpStrings                   proc                                ;Compares string at HL with string at DE.
                             local CmpAnotherByte,Different      ;Return in flags. ZF=1 if identical. CF=0 if str(HL)>str(DE), CF=1 if str(HL)<str(DE)
CmpAnotherByte               ld a,(de)
                             cp (hl)
                             jr nz,Different
                             or a                                ;Both strings must be NULL terminated
                             ret z
                             inc hl
                             inc de
                             jr CmpAnotherByte
Different                    ccf
                             ret
                             endp

;------------------------------------------------------------------------------

IPAddrBuffer                 ds 20
IPAllZeros                   db "0.0.0.0",NULL
WifiErrorMsg                 db "Network erro","r"+80h
FirstBlock                   db 0
ProgramPosition              dw 0
ProgramEntryPoint            dw 0
ResponseTable                dw 0,0,0,0,0,0,0,0
IndexTable                   db 0,0,0,0,0,0,0,0
Log                          equ $

                             end
