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

DOTCOMMAND                   equ 1

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

IF DEFINED DOTCOMMAND
                             org 2000h                    ;ESXDOS DOT command
ELSE
                             org 32768-4
                             dw Main
                             dw 0
ENDIF

;------------------------------------------------------------------------------

Main
IF NOT DEFINED DOTCOMMAND
                             ld a,2
                             call 1601h
ENDIF
                             Print
                             db "Detecting module...",CR,NULL
                             call DetectModule
                             jr c,NotFound
                             jr z,UARTisOK

NotFound                     Print
                             db "Wifi module not found. Aborting.",CR,NULL
                             scf
                             ld a,EIO
                             ret

UARTisOK                     Print
                             db "Found! Setting to 115.2 kbps...",CR,NULL

                             ATSend
                             db "AT+UART_DEF=115200,8,1,0,2",CR,LF,NULL
                             call CleanUART

                             ATSend
                             db "ATE0",CR,LF,NULL
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             Print
                             db "Done! Setting station mode...",CR,NULL

                             ATSend
                             db "AT+CWMODE_DEF=1",CR,LF,NULL
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             Print
                             ;   01234567890123456789012345678901
                             db "Done! Searching for APs...",CR,CR,NULL

                             ATSend
                             db "AT+CWLAPOPT=1,7",CR,LF,NULL
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             ld a,1
                             ld (23692),a    ;Enable scroll to browse APs

ScanAPs                      ATSend
                             db "AT+CWLAP",CR,LF,NULL

                             ld hl,APList
                             xor a
                             ld (LAPList),a
AnotherStation               push hl
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "+CWLAP:",NULL
                             db NULL
                             pop hl
                             cp 0
                             jr z,EndGetAPs

                             push hl
SearchFirstQuote             call ReadByte
                             cp 34
                             jr nz,SearchFirstQuote
NextStationCharacter         call ReadByte
                             cp 34
                             jr z,EndNameStation
                             ld (hl),a
                             inc hl
                             ld a,l
                             and 1Fh
                             cp 1Fh
                             jr nz,NextStationCharacter
EndNameStation               ld (hl),0

                             pop hl
                             ld de,32
                             add hl,de
                             ld a,(LAPList)
                             inc a
                             ld (LAPList),a
                             cp 36
                             jr nz,AnotherStation

EndGetAPs                    ld hl,APList

                             ld b,0
ListAPs                      ld a,(LAPList)
                             cp b
                             jr z,EndListAPs
                             push bc
                             push hl

                             ld a,b
                             cp 10
                             jr c,NoAdd7
                             add a,7
NoAdd7                       add a,"0"
                             rst 10h
                             ld a,"."
                             rst 10h
                             ld a," "
                             rst 10h
                             call PrintStringHL
                             ld a,CR
                             rst 10h

                             pop hl
                             ld de,32
                             add hl,de
                             pop bc
                             inc b
                             jr ListAPs

EndListAPs                   ld a,22
                             ld (23692),a
                             Print
                             ;      01234567890123456789012345678901
                             db CR,"Press key for desired AP."
                             db CR,"SPACE to exit. ENTER to relist.",CR,NULL

                             ld a,8
                             ld (23658),a    ;CAPS LOCK on

WaitKey                      xor a
                             ld (23560),a
WaitKey2                     halt
                             rst 18h
                             dw 02BFh   ;lectura de teclado. Parece que no se hace durante un dot command
                             ld a,(23560)
                             or a
                             jr z,WaitKey2
                             call Tick

                             cp 32
                             jp z,ExitConf

                             cp 13
                             jp z,ScanAPs

                             sub "0"
                             cp 10
                             jr c,NoSub7
                             sub 7
NoSub7                       cp 36
                             jr nc,WaitKey

                             ld hl,APList
                             ld de,32
SearchAPName                 or a
                             jr z,APNameFound
                             add hl,de
                             dec a
                             jr SearchAPName

APNameFound                  ld (SSIDNamePointer),hl
                             Print
                             db CR,"Connecting to ",NULL
                             ld hl,(SSIDNamePointer)
                             call PrintStringHL

                             ATSend
                             db "AT+CWQAP",CR,LF,NULL
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             call CleanUART

LoopGetPasswd                ld a,22
                             ld (23692),a
                             
                             Print
                             ;      01234567890123456789012345678901
                             db CR,"Enter password for this AP.",CR
                             db "Empty password to exit",CR," ",NULL

                             call GetPasswd
                             ld hl,Log
                             xor a
                             cp (hl)
                             jp z,ExitConf

                             Print
                             db "Please, wait...",CR,NULL

                             ATSend                              ;Beginning of a AT+CWJAP command
                             db 'AT+CWJAP_DEF="',NULL                ;

                             ld hl,(SSIDNamePointer)             ;Add SSID
                             call WriteATCommandHL               ;

                             ATSend                              ;Continuation of command
                             db '","',NULL                       ;

                             ld hl,Log                           ;Add password
                             call WriteATCommandHL               ;

                             ATSend                              ;End of command
                             db '"',CR,LF,NULL                   ;

                             WaitResponse                        ;Wait for expected responses
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db "FAIL",CR,LF,NULL
                             db NULL

                             cp 0
                             jr z,ConnectSuccess

                             Print
                             ;   01234567890123456789012345678901
                             db "Error connecting AP. Try again.",CR,NULL
                             jp LoopGetPasswd

ConnectSuccess               call CleanUART

                             ATSend
                             db "AT+CWAUTOCONN=1",CR,LF,NULL

                             call CleanUART
                             call GetLocalIP
                             Print
                             ;   01234567890123456789012345678901
                             db "Success. Wifi module configured."
                             db "IP address: ",NULL
                             ld hl,IPAddrBuffer
                             call PrintStringHL

ExitConf                     Print
                             db CR,"Exiting...",NULL

                             or a
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

DetectModule                 proc
                             local ReadAgain
                             ATSend
                             db "AT+UART_CUR=115200,8,1,0,2",CR,LF,NULL
                             call CleanUART

                             ATSend
                             db "ATE0",CR,LF,NULL
ReadAgain                    call ReadByte
                             ret c
                             cp "O"
                             jr nz,ReadAgain
                             call ReadByte
                             ret c
                             cp "K"
                             jr nz,ReadAgain
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

Tick                         proc
                             push af
                             ld a,(23609)
                             ld e,a
                             ld d,0
                             ld hl,00C8h
IF DEFINED DOTCOMMAND
                             rst 18h
                             dw 03B5h
ELSE
                             call 03B5h
ENDIF
                             pop af
                             ret
                             endp

;------------------------------------------------------------------------------

GetPasswd                    proc
                             xor a
                             ld (23658),a   ;turn CAPS LOCK off

                             ld a,(23689)
                             ld b,a
                             ld a,24
                             sub b
                             cp 22
                             jr c,LineOK
                             dec a
LineOK                       ld (ILine),a
                             xor a
                             ld (IColumn),a
                             ld hl,Log

NextKey                      ld a,22
                             rst 10h
                             ld a,(ILine)
                             rst 10h
                             ld a,(IColumn)
                             rst 10h
                             ld a,143
                             rst 10h

WaitInputKey                 xor a
                             ld (23560),a
                             push hl
WaitInputKey2                halt
                             rst 18h
                             dw 02BFh
                             ld a,(23560)
                             or a
                             jr z,WaitInputKey2
                             call Tick
                             pop hl
                             cp 13
                             jr z,Enter
                             cp 12
                             jr z,Delete
                             cp 32
                             jr c,WaitInputKey
                             cp 127
                             jr nc,WaitInputKey
                             ld (hl),a
                             inc hl
                             ld a,22
                             rst 10h
                             ld a,(ILine)
                             rst 10h
                             ld a,(IColumn)
                             rst 10h
                             ld a,"*"
                             rst 10h
                             ld a,(IColumn)
                             inc a
                             ld (IColumn),a
                             jr NextKey

Delete                       ld a,(IColumn)
                             or a
                             ret z
                             ld a,22
                             rst 10h
                             ld a,(ILine)
                             rst 10h
                             ld a,(IColumn)
                             rst 10h
                             ld a," "
                             rst 10h
                             ld a,(IColumn)
                             dec a
                             ld (IColumn),a
                             dec hl
                             jr NextKey

Enter                        ld (hl),0
                             ld a,22
                             rst 10h
                             ld a,(ILine)
                             rst 10h
                             ld a,0
                             rst 10h
                             Print
                             db "                                ",NULL
                             ret
                             endp


ILine                        db 0
IColumn                      db 0
SSIDNamePointer              dw 0
IPAddrBuffer                 ds 20
ResponseTable                dw 0,0,0,0,0,0,0,0
IndexTable                   db 0,0,0,0,0,0,0,0
LAPList                      db 0
                             org $+32-($ MOD 32)
APList                       ds 36*32
Log                          equ $
                             end
