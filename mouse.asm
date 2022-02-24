; Amiga/Atari mouse configurator for DivTIESUS
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
MOUSECONF                    equ 0dfh                     ;
include "esxdos.inc"
include "errors.inc"

Print                        macro                        ;
                             call PrintString             ;Ditto
                             endm                         ;
                             org 2000h                    ;ESXDOS DOT command

;------------------------------------------------------------------------------

Main                         ld a,h
                             or l
                             jp nz,SetMouse

                             Print
                             ;   01234567890123456789012345678901
                             db 13
                             db "Syntax: .MOUSE [m|t][s][d]",13
                             db "Configures Amiga/Atari mouse",13
                             db "for Kempston mouse emulation.",13
                             db " m. Enable Amiga->Kempston mouse"
                             db " t. Enable Atari->Kempston mouse"
                             db " s. Swaps left/right buttons",13
                             db " d. Disable Kempston mouse",13,13,0

                             ld de,00FFh
                             jp DoConfig

SetMouse                     ld de,00FFh   ;new config and new mask
NewParam                     ld a,(hl)
                             or a
                             jp z,DoConfig
                             cp 13
                             jp z,DoConfig
                             cp ":"
                             jp z,DoConfig

                             cp "m"
                             jr nz,NotAmigaMouse
                             res 0,d
                             res 2,d
                             res 0,e
                             res 2,e
                             res 1,d
                             res 1,e
                             inc hl
                             jr NewParam

NotAmigaMouse                cp "t"
                             jr nz,NotAtariMouse
                             set 0,d
                             res 0,e
                             res 2,d
                             res 2,e
                             res 1,d
                             res 1,e
                             inc hl
                             jr NewParam

NotAtariMouse                cp "s"
                             jr nz,NotSwapButton
                             set 1,d
                             res 1,e
                             inc hl
                             jr NewParam

NotSwapButton                inc hl
                             cp "d"
                             jr nz,NewParam
                             ld d,100b
                             ld e,0
                             jr NewParam

DoConfig                     ld bc,ZXUNOADDR
                             ld a,MOUSECONF
                             out (c),a
                             inc b
                             in a,(c)
                             and e
                             or d
                             out (c),a

                             ;prints current configuration

                             Print
                             db "Kempston mouse current config:",13,0

                             bit 0,a
                             jr nz,AtariThen
                             Print
                             db " AMIGA device",13,0
                             jr TestSwap
AtariThen                    Print
                             db " ATARI device",13,0

TestSwap                     bit 1,a
                             jr z,TestDisabled
                             Print
                             db " L/R buttons swapped",13,0
                             
TestDisabled                 bit 2,a
                             jr z,EndPrintConfig
                             Print
                             db " Disabled",13,0

EndPrintConfig               or a
                             ret

;------------------------------------------------------------------------------

PrintString                  proc                               ;print string to screen (ESXDOS opens channel 2)
                             ex (sp),hl                         ;such string follows the CALL instruction (as an inmediate operand)
                             push af
                             call PrintStringHL
                             pop af
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

                             end
