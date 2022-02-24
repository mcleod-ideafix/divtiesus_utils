; API de ESXDOS.
include "esxdos.inc"
include "errors.inc"

; LOADROM: un comando para cargar un fichero ROM de 16K de forma que aparezca como la ROM
; del sistema, permitiendo usar juegos y aplicaciones que se publicaron en este formato.

; La ROM es cargada en el banco 8 y 9 de la memoria DivMMC. Para activar este modo, hay
; que poner a 1 el bit 7 del registro $DF y justo después saltar a la dirección 0000h. Esto debe hacerse
; desde la RAM principal, no desde la RAM del dot command, y, muy recomendable, con las interrupciones
; deshabilitadas.

;Para ensamblar con PASMO como archivo binario (no TAP)

PILA                equ 3deah   ;valor sugerido por Miguel Ângelo para poner la pila en el área de comando
DIVCTRL             equ 0E3h
ZXUNOADDR           equ 0FC3Bh
MODE                equ 0DFh

                    org 2000h  ;comienzo de la ejecución de los comandos ESXDOS.

Main                proc
                    ld a,h
                    or l
                    jr z,PrintUsage
                    call GetCmdLine

                    xor a
                    rst 08h
                    db M_GETSETDRV  ;A = unidad actual
                    ld hl,BufferParam
                    ld b,FA_READ    ;B = modo de apertura
                    rst 08h
                    db F_OPEN
                    ret c   ;Volver si hay error
                    ld (FHandle),a

                    ; leo temporalmente la ROM a partir de la posición 32768
                    ld bc,16384
                    ld hl,32768
                    rst 08h
                    db F_READ
                    ret c
                    ld a,(FHandle)
                    rst 08h
                    db F_CLOSE

                    ld hl,CodigoEnRAM
                    ld de,23296
                    ld bc,LCodigoEnRAM
                    ldir
                    jp 23296
                    
PrintUsage          ld hl,MsgUsage
                    call Print
                    or a
                    ret
                    endp

MsgUsage            db "LOADROM file_to_load",13
                    db "  file_to_load must have 16K",0

Print               proc
                    ld a,(hl)
                    or a
                    ret z
                    rst 10h
                    inc hl
                    jr Print
                    endp

CodigoEnRAM         proc     ;codigo en RAM, a partir de 8000h
                    di
                    ld sp,65535
                    ld a,128+8   ;valor inicial para DIVCTRL (COMEN activado)
                    call FuncionCopia16K  ;copia ROM a bancos 8 y 9
                    ld a,128+0   ;banco 0 de DivMMC, para borrarlo
                    out (DIVCTRL),a
                    ld hl,8192
                    ld de,8193
                    ld (hl),l
                    ld bc,8191
                    ldir

                    ;Establecemos ROM 0 para arrancar y bloqueamos paginación de 128K
                    xor a
                    ld bc,1ffdh
                    out (c),a
                    ld b,7fh
                    ld a,32
                    out (c),a

                    ;Activamos el "modo 3E" del DivTIESUS
                    ld bc,ZXUNOADDR
                    ld a,MODE
                    out (c),a
                    inc b
                    in a,(c)
                    set 7,a   ;modo 3E, respetando la conf. de ratón
                    out (c),a

                    jp 0

                    ;////////////////////////////

Copia16K            out (DIVCTRL),a
                    ld hl,32768
                    ld de,8192
                    ld bc,8192
                    ldir
                    inc a
                    out (DIVCTRL),a
                    ld de,8192
                    ld bc,8192
                    ldir
                    inc a
                    ret
                    endp

GetCmdLine          proc   ;HL apunta a los argumentos
                    ld de,BufferParam
CheckCaracter       ld a,(hl)
                    or a
                    jr z,FinRecoger
                    cp ":"
                    jr z,FinRecoger
                    cp 13
                    jr z,FinRecoger
                    ldi
                    jr CheckCaracter
FinRecoger          xor a
                    ld (de),a
                    ret
                    endp


LCodigoEnRAM        equ $-CodigoEnRAM
FuncionCopia16K     equ Copia16K-CodigoEnRAM+23296

FHandle             db 0
BufferParam         equ $
