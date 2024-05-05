; SNTP client for time update over internet, for ZXUNO / ESP8266 module.
; Copyright (C)2022 Miguel Angel Rodriguez Jodar (mcleod_ideafix). ZX Projects.
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

TIMEOUT                      equ 40000

CR                           equ 13
LF                           equ 10
NULL                         equ 0

I2CREG             equ 0F8h

SCL0SDA0           equ 00b
SCL0SDA1           equ 01b
SCL1SDA0           equ 10b
SCL1SDA1           equ 11b

I2CADDR_W          equ 0A2h

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

Main                         ld a,h
                             or l
                             jp z,LoadDefaults

ProcessNextCharCLine         ld a,(hl)
                             cp ":"
                             jp z,LoadDefaults
                             cp CR
                             jp z,LoadDefaults
                             cp NULL
                             jp z,LoadDefaults
                             cp "+"
                             jp z,GMTCopy
                             cp "-"
                             jp z,GMTCopy
                             cp " "
                             jp nz,NTPHNCopy
                             inc hl
                             jr ProcessNextCharCLine

NTPHNCopy                    ld de,NTPHostname
NTPHNNextChar                ld a,(hl)
                             cp " "
                             jr z,EndNTPHNCopy
                             cp ":"
                             jr z,EndNTPHNCopy
                             cp CR
                             jr z,EndNTPHNCopy
                             cp NULL
                             jr z,EndNTPHNCopy
                             ld (de),a
                             inc hl
                             inc de
                             jr NTPHNNextChar
EndNTPHNCopy                 xor a
                             ld (de),a
                             jr ProcessNextCharCLine

GMTCopy                      ld de,GMTOffset
                             ld (de),a
                             inc hl
                             inc de
                             ld a,(hl)
                             cp "h"
                             jp z,ShowHelpAndExit
                             cp "0"
                             jr c,EndGMTCopy
                             cp "9"+1
                             jr nc,EndGMTCopy
                             ld (de),a
                             inc hl
                             inc de
                             ld a,(hl)
                             cp "0"
                             jr c,EndGMTCopy
                             cp "9"+1
                             jr nc,EndGMTCopy
                             ld (de),a
                             inc hl
                             inc de
EndGMTCopy                   xor a
                             ld (de),a
                             jp ProcessNextCharCLine

ShowHelpAndExit              Print
                             db "NTPDATE [+/-nn] [NTP hostname]",CR
                             db "nn is GMT offset",CR,CR,NULL
                             or a
                             ret

LoadDefaults                 ld hl,NTPHostname
                             ld de,NTPServer
CopyNTPServer                ld a,(hl)
                             or a
                             jr z,IsNull
                             ld (de),a
                             inc hl
                             inc de
                             jr CopyNTPServer
IsNull                       ex de,hl
                             ld (hl),34
                             inc hl
                             ld (hl),CR
                             inc hl
                             ld (hl),LF
                             inc hl
FillServerZero               ld a,(hl)
                             or a
                             jr z,FinCopyServer
                             ld (hl),0
                             inc hl
                             jr FillServerZero

FinCopyServer                call CleanUART               ;clear any spurious data

                             ;ATSend
                             ;db "AT+RST",CR,LF,NULL
                             ;WaitResponse
                             ;db "WIFI GOT IP",CR,LF,NULL
                             ;db NULL
                             ;cp 0
                             ;jr nz,ErrorCWJAPcommand

                             ATSend
                             db "ATE0",CR,LF,NULL         ;mute command echo (was unmuted after reset)
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             call GetLocalIP              ;try to get the local IP
                             jp c,ErrorCWJAPcommand

;------------------------------------------------------------------------------

                             ld hl,IPAddrBuffer           ;
                             ld de,IPAllZeros             ;if we can get an IP and it is not 0.0.0.0, then
                             call CmpStrings              ;
                             jp nz,OKWifiConnect          ;assume we are connected and skip all the init process

ErrorCWJAPcommand            Print
                             db "Couldn't connect to AP. Exiting...",CR,NULL
                             jp ExitByError

;------------------------------------------------------------------------------

OKWifiConnect                ;Print
                             ;db "AP connection established.",CR,NULL

                             ATSend
                             db "AT+CIPMUX=0",CR,LF,NULL      ;single connection
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db "ERROR",CR,LF,NULL
                             db NULL

                             ;Print
                             ;db "Single connection mode set.",CR,NULL

;------------------------------------------------------------------------------

                             ATSend
                             db 'AT+CIPDOMAIN="'
NTPServer                    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             WaitResponse
                             db "+CIPDOMAIN:",NULL
                             db "ERROR",CR,LF,NULL
                             db NULL
                             cp 0
                             jp nz,ExitByError

                             ld hl,IPAddrBuffer
GetIPFromDomain              call ReadByte
                             cp " "
                             jr c,EndGetIPFromDomain
                             ld (hl),a
                             inc hl
                             jr GetIPFromDomain

EndGetIPFromDomain           ld (hl),0
                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             ;Print
                             ;db "DNS query success.",CR,"IP: ",NULL
                             Print 
                             db "Querying ",NULL
                             ld hl,IPAddrBuffer
                             call PrintStringHL
                             ld a,13
                             rst 10h

;------------------------------------------------------------------------------

                             ATSend
                             db 'AT+CIPSTART="UDP","',NULL
                             ld hl,IPAddrBuffer
                             call WriteATCommandHL
                             ATSend
                             db '",123',CR,LF,NULL

                             WaitResponse
                             db "OK",CR,LF,NULL
                             db NULL

                             cp 0
                             jp nz,ExitByError

                             ;Print
                             ;db "UDP connection created",CR,NULL

;------------------------------------------------------------------------------

                             ATSend
                             db "AT+CIPSEND=60",CR,LF,NULL  ;send SNTP request
                             WaitResponse
                             db ">",NULL
                             db NULL

                             cp 0
                             jp nz,ExitByError

                             ld hl,BufferNTP
                             ld b,60
SendByteUART                 push bc
                             ld a,(hl)
                             call WriteByte   ;send actual NTP request (60 bytes)
                             inc hl
                             pop bc
                             djnz SendByteUART

                             WaitResponse
                             db "SEND OK",NULL
                             db NULL

                             cp 0
                             jp nz,ExitByError

                             ;Print
                             ;db "NTP request sent.",CR,NULL

;------------------------------------------------------------------------------

                             ;wait for NTP response
                             WaitResponse
                             db "+IPD,",NULL              ;new data received
                             db NULL

                             cp 255
                             jr nz,NotTOutIPD
                             Print
                             db "No response from NTP server",CR,NULL
                             jp ExitByError

NotTOutIPD                   cp 0
                             jp nz,ExitByError

                             ld hl,0                      ;HL=length of block. Parsing numeric string now
ParseDigitLength             call ReadByte                ;read digit
                             cp ":"                       ;end of number?
                             jr z,EndDigit
                             sub "0"                      ;convert ASCII digit to value
                             call MultHL10PlusA
                             jr ParseDigitLength          ;

EndDigit                     ld ix,BufferNTP              ;
LoopReceiveNTPData           ld a,h                       ;
                             or l                         ;loop to read HL bytes
                             jr z,EndBlock                ;from UART to the memory
                             call ReadByte                ;block pointed by IX
                             ld (ix),a                    ;
                             inc ix                       ;
                             dec hl                       ;
                             jr LoopReceiveNTPData        ;

EndBlock                     ;Print
                             ;db "NTP response received!",CR,NULL

;------------------------------------------------------------------------------

                             call CleanUART
                             ATSend
                             db "AT+CIPCLOSE",CR,LF,NULL     ;close TCP server

                             ; Compute date and time from response at BufferNTP+40 (big endian)
                             ld de,BufferNTP+43
                             ld hl,AdjustJan2018+3
                             call Sub32Bits

                             ;add GMT+n here (add/substract n*3600)
                             ld bc,GMTOffset+1
                             ld hl,0
CalculaGMTValue              ld a,(bc)
                             cp "0"
                             jr c,EndGMTValue
                             sub "0"
                             call MultHL10PlusA
                             inc bc
                             jr CalculaGMTValue

EndGMTValue                  ld c,l  ;store GMT value into c
                             ld a,(GMTOffset)
                             cp "-"
                             jp z,SubGMT
AddGMT                       ld a,c
                             or a
                             jr z,EndCalcGMT
                             ld de,BufferNTP+43
                             ld hl,SecondsInHour+3
                             call Add32Bits
                             dec c
                             jr AddGMT

SubGMT                       ld a,c
                             or a
                             jr z,EndCalcGMT
                             ld de,BufferNTP+43
                             ld hl,SecondsInHour+3
                             call Sub32Bits
                             dec c
                             jr SubGMT

EndCalcGMT                   ld c,2    ;lap year when C is 0
CalculateYear                ld hl,SecondsInYear+3
                             ld a,c
                             and 3
                             jr nz,NotLapYear
                             ld hl,SecondsInLapYear+3

NotLapYear                   push hl
                             ld de,BufferNTP+43
                             call Cmp32Bits
                             pop hl
                             jp m,YearFound
                             ld de,BufferNTP+43
                             call Sub32Bits
                             inc c
                             ld hl,(Year)
                             inc hl
                             ld (Year),hl
                             jr CalculateYear

YearFound                    ld a,c
                             and 3
                             ld hl,NormalYear
                             jp nz,NotLapYear2
                             ld hl,LapYear
NotLapYear2

CalculateMonth               ld e,(hl)
                             inc hl
                             ld d,(hl)
                             inc hl
                             push hl
                             ex de,hl
                             push hl
                             ld de,BufferNTP+43
                             call Cmp32Bits
                             pop hl
                             jp m,MonthFound
                             ld de,BufferNTP+43
                             call Sub32Bits
                             pop hl
                             ld a,(Month)
                             inc a
                             ld (Month),a
                             jr CalculateMonth

MonthFound                   pop hl
CalculateDay                 ld hl,SecondsInDay+3
                             ld de,BufferNTP+43
                             call Cmp32Bits
                             jp m,DayFound
                             ld hl,SecondsInDay+3
                             ld de,BufferNTP+43
                             call Sub32Bits
                             ld a,(Day)
                             inc a
                             ld (Day),a
                             jr CalculateDay

DayFound
CalculateHour                ld hl,SecondsInHour+3
                             ld de,BufferNTP+43
                             call Cmp32Bits
                             jp m,HourFound
                             ld hl,SecondsInHour+3
                             ld de,BufferNTP+43
                             call Sub32Bits
                             ld a,(Hour)
                             inc a
                             ld (Hour),a
                             jr CalculateHour

HourFound
CalculateMinute              ld hl,SecondsInMinute+3
                             ld de,BufferNTP+43
                             call Cmp32Bits
                             jp m,MinuteFound
                             ld hl,SecondsInMinute+3
                             ld de,BufferNTP+43
                             call Sub32Bits
                             ld a,(Minute)
                             inc a
                             ld (Minute),a
                             jr CalculateMinute

MinuteFound                  ld a,(BufferNTP+43)
                             ld (Second),a

;------------------------------------------------------------------------------

                             ld a,(Day)
                             call Print2DigitDec
                             ld a,"/"
                             rst 10h
                             ld a,(Month)
                             call Print2DigitDec
                             ld a,"/"
                             rst 10h
                             ld a,20
                             call Print2DigitDec
                             ld hl,(Year)
                             ld de,2000
                             or a
                             sbc hl,de
                             ld a,l
                             call Print2DigitDec
                             ld a," "
                             rst 10h
                             ld a,(Hour)
                             call Print2DigitDec
                             ld a,":"
                             rst 10h
                             ld a,(Minute)
                             call Print2DigitDec
                             ld a,":"
                             rst 10h
                             ld a,(Second)
                             call Print2DigitDec
                             ld a,13
                             rst 10h

                             call UpdateRTC

                             or a
                             ret

Print2DigitDec               ld l,a
                             call DivideLby10
                             ld a,l
                             add a,"0"
                             rst 10h
                             ld a,h
                             add a,"0"
                             rst 10h
                             ret

ExitByError                  ;ld hl,Log                          ;if exiting due to network failure
                             ;call PrintStringHL                 ;print reception log to diagnose the cause
                             ATSend
                             db "AT+CIPCLOSE",CR,LF,NULL     ;close TCP server
                             or a
                             ret

;------------------------------------------------------------------------------

Cmp32Bits                    proc   ;COMP Mem32(DE) < Mem32(HL) ? S set if so.
                             ld b,4
                             or a
BucCompara32b                ld a,(de)
                             sbc a,(hl)
                             dec hl
                             dec de
                             djnz BucCompara32b
                             ret
                             endp

;------------------------------------------------------------------------------

Add32Bits                    proc  ;ADD Mem32(DE) , Mem32(HL)
                             ld b,4
                             or a
BucAdd32b                    ld a,(de)
                             adc a,(hl)
                             ld (de),a
                             dec hl
                             dec de
                             djnz BucAdd32b
                             ret
                             endp

;------------------------------------------------------------------------------

Sub32Bits                    proc  ;SUB Mem32(DE) , Mem32(HL)
                             ld b,4
                             or a
BucSub32b                    ld a,(de)
                             sbc a,(hl)
                             ld (de),a
                             dec hl
                             dec de
                             djnz BucSub32b
                             ret
                             endp

;------------------------------------------------------------------------------

MultHL10PlusA                proc
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
                             ret
                             endp

;------------------------------------------------------------------------------

DivideLby10                  proc    ;HL/10 : L=cociente  H=resto
                             xor a
                             ld h,a
                             ld b,8
BucDivide10                  add hl,hl
                             ld a,h
                             cp 10
                             jr c,SigueDiv
                             sub 10
                             ld h,a
                             inc hl
SigueDiv                     djnz BucDivide10
                             ret
                             endp

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
                             jp c,ExitBreak                      ;Exit if SPACE pressed

                             ld (hl),a                           ;Store received byte in the log
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

ToBCD                        proc
                             ld l,a
                             call DivideLby10
                             ld a,l
                             add a,a
                             add a,a
                             add a,a
                             add a,a
                             or h
                             ret
                             endp

;------------------------------------------------------------------------------

;Los cambios en SDA deben hacerse cuando SCL es 0. Cuando SCL es 1
;el valor en SDA debe estar estable. No se puede cambiar a la vez SCL y SDA o puede haber
;violacion del tiempo de setup. Primero se pone SCL a 0, y ahi ya se cambia SDA a lo que sea

;Se transmiten datos de 8 bits desde el MSb al LSb.
;Quien recibe debe enviar un 0 (ACK) en el siguiente pulso de SCL tras recibir el dato correctamente

;Para enviar un 1 se pone durante un semiperíodo el reloj a 0 y el dato a 1, y en
;el siguiente semiperíodo, el reloj a 1 y el dato a 1

;Para enviar un 0 se pone durante un semiperíodo el reloj a 0 y el dato a 0, y en
;el siguiente semiperíodo, el reloj a 1 y el dato a 0

UpdateRTC          proc
                   ld de,Params+1
                   ld a,(Second)
                   call ToBCD
                   ld (de),a
                   inc de
                   ld a,(Minute)
                   call ToBCD
                   ld (de),a
                   inc de
                   ld a,(Hour)
                   call ToBCD
                   ld (de),a
                   inc de
                   ld a,(Day)
                   call ToBCD
                   ld (de),a
                   inc de
                   inc de   ;skip DoW
                   ld a,(Month)
                   call ToBCD
                   or 80h   ; para indicar centuria 2000
                   ld (de),a
                   inc de
                   ld hl,(Year)
                   ld bc,2000
                   or a
                   sbc hl,bc
                   ld a,l
                   call ToBCD
                   ld (de),a

                   ld bc,ZXUNOADDR
                   ld a,I2CREG
                   out (c),a    ; selecciono el registro I2C.
                   inc b

                   ld hl,Params
                   ld e,8
                   call SendW    ;Envío en modo escritura el número de registro desde el que quiero empezar a escribir, y a continuación, 7 bytes con la fecha actual
                   ret

Reset              ld a,SCL1SDA1 ;Bus en modo idle.
                   out (c),a
                   ret

SendByte        ;Enviar un byte por I2C. Byte en A. BC=puerto de datos del ZXUNO ya apuntando a I2CREG. Usa y modifica: A, D, flags
                scf
TransBit        adc     a, a   ;A otro bit
                jr      z, Endbyte
                call    c, Send1
                call    nc, Send0
                and     a
                jr      TransBit
Endbyte         ; Wait for ACK
Send1           ld      d, SCL0SDA1
                out     (c), d
                ld      d, SCL1SDA1
                out     (c), d
                ld      d, SCL0SDA1
                out     (c), d          ;Transmito un bit 1 para dejar SDA en modo de alta impedancia y que el receptor puedo ponerlo a 0
                ret

Send0           ld      d, SCL0SDA0
                out     (c), d
                ld      d, SCL1SDA0
                out     (c), d
                ld      d, SCL0SDA0
                out     (c), d
                ret

SendW              ;Transmite E bytes apuntados por HL al bus I2C (escritura)
                di
                call Reset

                ld      a, SCL1SDA1     ;Bus en modo idle.
                out     (c), a          ;EnvÃ­o condiciÃ³n de START al bus (SDA a 0 mientras mantengo SCL a 1)
                ld      a, SCL1SDA0
                out     (c), a
                ld      a, I2CADDR_W    ;EnvÃ­o la direcciÃ³n I2C de escritura del RTC
                call    SendByte
LoopSendMess    ld      a, (hl)         ;y en un bucle, me pongo a enviar uno tras otro, los E bytes a los que apunta HL
                inc     hl
                call    SendByte
                dec     e
                jr      nz, LoopSendMess
                call    Send0                   ;despuÃ©s de enviar el ultimo byte, envÃ­o la condiciÃ³n de STOP al bus (envÃ­o un 0, con lo que se queda SDA a 0 y SCL a 1, y a continuaciÃ³n pongo SDA a 1)
                ld      d, SCL1SDA1
                out     (c), d

                ei
                ret
                endp

Params             db 02h  ; VL_seconds register . Indico que quiero empezar a leer desde aquí (que es lo típico para leer toda la fecha y hora)
                   db 00h,40h,16h,24h,01h,10h,22h ;La hora a la que quieres poner el reloj. En lectura, estos datos se machacan con la hora leída del RTC
                   ;  S   M   H   D   W   Mo  Y   (en BCD) OJO cuidao porque el dígito de las decenas no usa todos los bits (excepto en el año)
                   ;                               así que hay que enmascararlo antes de hacer algo con él (yo lo hago justo antes de imprirmilo)


;------------------------------------------------------------------------------

AdjustJan2018                db 0DDh,0F3h,0F8h, 80h ;3723753600   1/1/2018 0:0:0 GMT+0
SecondsInYear                db  01h,0E1h, 33h, 80h
SecondsInLapYear             db  01h,0E2h, 85h, 00h
SecondsIn28dMonth            db  00h, 24h,0EAh, 00h
SecondsIn29dMonth            db  00h, 26h, 3Bh, 80h
SecondsIn30dMonth            db  00h, 27h, 8Dh, 00h
SecondsIn31dMonth            db  00h, 28h,0DEh, 80h
SecondsInDay                 db  00h, 01h, 51h, 80h
SecondsInHour                db  00h, 00h, 0Eh, 10h
SecondsInMinute              db  00h, 00h, 00h, 3Ch

GMTOffset                    db "+00",NULL
NTPHostname                  db "time.google.com"
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                             db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
Year                         dw 2018
Month                        db 1
Day                          db 1
Hour                         db 0
Minute                       db 0
Second                       db 0

NormalYear                   dw SecondsIn31dMonth+3, SecondsIn28dMonth+3, SecondsIn31dMonth+3, SecondsIn30dMonth+3
                             dw SecondsIn31dMonth+3, SecondsIn30dMonth+3, SecondsIn31dMonth+3, SecondsIn31dMonth+3
                             dw SecondsIn30dMonth+3, SecondsIn31dMonth+3, SecondsIn30dMonth+3, SecondsIn31dMonth+3

LapYear                      dw SecondsIn31dMonth+3, SecondsIn29dMonth+3, SecondsIn31dMonth+3, SecondsIn30dMonth+3
                             dw SecondsIn31dMonth+3, SecondsIn30dMonth+3, SecondsIn31dMonth+3, SecondsIn31dMonth+3
                             dw SecondsIn30dMonth+3, SecondsIn31dMonth+3, SecondsIn30dMonth+3, SecondsIn31dMonth+3

IPAddrBuffer                 ds 20
IPAllZeros                   db "0.0.0.0",NULL
BufferNTP                    db 0bh   ;initial request here
                             ds 59
ResponseTable                dw 0,0,0,0,0,0,0,0
IndexTable                   db 0,0,0,0,0,0,0,0
Log                          equ $
                             end
