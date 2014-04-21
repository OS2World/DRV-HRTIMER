;**********************************************************************
;* MODULE NAME :  hrtimer.asm            AUTHOR:  Rick Fishman        *
;* DATE WRITTEN:  11-23-91                                            *
;*                                                                    *
;* DESCRIPTION:                                                       *
;*                                                                    *
;*  This device driver provides a high-resolution timer for OS/2      *
;*                                                                    *
;*  The source code was obtained from the Fall 1991 issue of IBM      *
;*  Personal Systems Developer magazine.                              *
;*                                                                    *
;**********************************************************************

        .286

;*********************************************************************
;*----------------------------- EQUATES -----------------------------*
;*********************************************************************

RP_StatusError          equ     80h     ; RP_Status error bit
RP_StatusDone           equ     01h     ; RP_Status done bit

                                        ; DEVICE HELPER FUNCTIONS
DevHlp_PhysToVirt       equ     15h     ; Convert a physical address to virtual
DevHlp_SetTimer         equ     1Dh     ; Hook into the Motorola timer int
DevHlp_UnPhysToVirt     equ     32h     ; Release virtual memory

i8253CountRegister      equ     40h     ; 8253 Counter Register
i8253CtrlByteRegister   equ     43h     ; 8253 Control Byte Register
i8253CmdReadCtrZero     equ     0       ; Latch Command
i8253CmdInitCtrZero     equ     34h     ; LSB first, MSB second, Rate generator

NanosInATick            equ     840     ; Number of nanoseconds in 1 8253 tick
MillionDividedBy64      equ     15625   ; 1,000,000 divided by 64

cr                      equ     0dh     ; ASCII code for carraige return
lf                      equ     0ah     ; ASCII code for line feed

stdout                  equ     1       ; File handle for standard output

;**********************************************************************
;*------------------------------ MACROS ------------------------------*
;**********************************************************************

Read8253IntoCx  MACRO                      ; Put 8253 counter 0 value in cx

        mov     al, i8253CmdReadCtrZero    ; Request Counter Latch
        out     i8253CtrlByteRegister, al
        in      al, i8253CountRegister     ; Get LSB and save it
        mov     cl, al
        in      al, i8253CountRegister     ; Get MSB and save it
        mov     ch, al

                ENDM

;**********************************************************************
;*---------------------------- STRUCTURES ----------------------------*
;**********************************************************************

ReadData                struc           ; Data passed to caller of DosRead

    RD_Millisecs        dd      ?       ; Current millisecond count
    RD_Nanosecs         dd      ?       ; Current nanosecond count
    RD_Version          dw      ?       ; HRTIMER.SYS version - hi
    RD_Revision         dw      ?       ; HRTIMER.SYS version - lo

ReadData                ends


RequestPacket           struc           ; Request Packet header

    RP_Length           db      ?       ; Request Packet length
                        db      ?       ; Block devices only
    RP_CommandCode      db      ?       ; Command
    RP_ErrorCode        db      ?       ; Command Error Code
    RP_Status           db      ?       ; Command Status Code
                        dd      ?       ; Reserved
                        dd      ?       ; Queue Linkage - not used here

RequestPacket           ends


RequestPktInit          struc           ; Initialization Request Packet

                        db 13 dup(?)    ; Request Packet Header
    RPI_NumberUnits     db      ?       ; Block devices only - used to cancel
                                        ; DevHlp pointer in, on return from init
    RPI_CodeSegLen      dw      ?       ;     Code segment length
    RPI_DataSegLen      dw      ?       ;     Data segment length
    RPI_CommandLine     dd      ?       ; Pointer to command line
                        db      ?       ; Block devices only

RequestPktInit          ends


RequestPktRead          struc           ; Read Request Packet (from DosRead)

                        db 13 dup(?)    ; Request Packet header
                        db      ?       ; Block devices only
    RPR_TransferAddr    dd      ?       ; Physical address of read buffer
    RPR_BytesRequested  dw      ?       ; Number of bytes to read

RequestPktRead          ends

;**********************************************************************
;*----------------------------- EXTERNS ------------------------------*
;**********************************************************************

        extrn  DosWrite:far

;**********************************************************************
;*-------------------------- DATA SEGMENT ----------------------------*
;**********************************************************************

DGROUP          group   _DATA

_DATA           SEGMENT word public  'DATA'

;**********************************************************************
;*---------------------- Device Driver Header ------------------------*
;**********************************************************************

TimerHeader             label   byte            ; Device Driver header

    NextDeviceDriver    dd      -1              ; Last driver in chain
    DeviceAttribute     dw      1000100010000000B  ; Char,Open/Close,OS/2 1.1
    StrategyOffset      dw      Strategy        ; Offset of Strategy Routine
                        dw      -1              ; IDC - not used here
    DeviceName          db      'TIMER$  '      ; Driver Device-Name
                        db 8 dup(0)             ; Reserved

;**********************************************************************
;*------------ Data areas used by Strategy and Interrupt -------------*
;**********************************************************************

DevHlpPtr               dd      ?               ; Pointer to Device Helper
                                                ;   routine - Set at init rqst
ReadDataBuf             ReadData < 0, 0, 1, 0 > ; Buffer to return to caller

ReadDataLen             equ     $ - ReadDataBuf ; Length of ReadDataBuf

UserCount               dw      0               ; Number of active users

Last8253                dw      ?               ; 8253 ticks at last interrupt

BytesWritten            dw      ?               ; Used for DosWrite calls

;**********************************************************************
;*--------------------- Command (Request) List -----------------------*
;**********************************************************************

CmdList         label   word

                dw      Initialize      ;  0 = Initialize driver
                dw      Error           ;  1 = Media Check
                dw      Error           ;  2 = Build BPB
                dw      Error           ;  3 = Not used
                dw      Read            ;  4 = Read from device
                dw      DummyRet        ;  5 = Non-destructive read
                dw      DummyRet        ;  6 = Return input status
                dw      DummyRet        ;  7 = Flush input buffers
                dw      DummyRet        ;  8 = Write to device
                dw      DummyRet        ;  9 = Write with verify
                dw      DummyRet        ; 10 = Return output status
                dw      DummyRet        ; 11 = Flush output buffers
                dw      Error           ; 12 = Not used
                dw      Open            ; 13 = Device open
                dw      Close           ; 14 = Device close

MaxCmd          equ     ( $ - CmdList ) / TYPE CmdList

CopyRightMsg    db      cr, lf
                db      'High Resolution Timer - Version 1.0', cr, lf
                db      'Courtesy of Code Blazers, Inc. 1991', cr, lf
                db      'Source code from IBM Personal Systems Developer '
                db      '(Fall 1991)', cr, lf, lf
CopyRightMsgLen equ     $ - CopyRightMsg

InitNotOkMsg    db      'HRTIMER.SYS Initialization Failed', cr, lf
InitNotOkMsgLen equ     $ - InitNotOkMsg

LastData        equ     $ - CopyRightMsg

_DATA           ENDS

;**********************************************************************
;*-------------------------- CODE SEGMENT ----------------------------*
;**********************************************************************

_TEXT           SEGMENT word public  'CODE'

                assume cs:_TEXT, ds:DGROUP

;**********************************************************************
;*---------------------------- Strategy ------------------------------*
;*                                                                    *
;*  STRATEGY ENTRY POINT.                                             *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: nothing                                                   *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Strategy        PROC    far

        cmp     es:[bx].RP_CommandCode, MaxCmd  ; Command within jumb table?
        jbe     JumpCmd                         ;   YES: execute command routine

        call    Error                           ;   NO: send back error
        jmp     short exit                      ;       and exit

JumpCmd:

        mov     al, es:[bx].RP_CommandCode      ; Isolate command,
        cbw                                     ;   convert to word,
        mov     si, ax                          ;   put into index register,
        shl     si, 1                           ;   multiply by 2 so it is a
                                                ;   word rather than byte offset
        call    CmdList [si]                    ; Call command routine

exit:
        ret                                     ; Return to operating system

Strategy        ENDP

;**********************************************************************
;*------------------------------ Read --------------------------------*
;*                                                                    *
;*  HANDLE A READ REQUEST.                                            *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1.                                                                *
;*                                                                    *
;*  OUTPUT: status byte set                                           *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Read            PROC    near

        cli                             ; Disable interrupts

        Read8253IntoCx                  ; Get current tick count

        call    UpdateTimeStamp         ; Update running time stamp

        cmp     es:[bx].RPR_BytesRequested, ReadDataLen
        jl      SetError                ; Caller's buffer is too small

        push    es                      ; Save packet pointer
        push    bx

        mov     ax, word ptr es:[bx].RPR_TransferAddr + 2   ; Store ReadPacket
        mov     cx, es:[bx].RPR_BytesRequested              ;   variables in
        mov     bx, word ptr es:[bx].RPR_TransferAddr       ;   registers

        mov     dh, 1                   ; 1 = Store result in ES:DI
        mov     dl, DevHlp_PhysToVirt   ; Use the PhysToVirt function
        call    DevHlpPtr               ; Call the Device Helper routine
        jc      DevHlpError

        mov     si, offset ReadDataBuf  ; Address of timestamp data
        mov     cx, (ReadDataLen / 2)   ; Number of words in timestamp data
        cld                             ; Move forward
        rep     movsw                   ; Move that data

        sti                             ; Enable interrupts

        mov     dl, DevHlp_UnPhysToVirt ; Free Virtual Memory function
        call    DevHlpPtr               ; Call the Device Helper routine

        pop     bx                      ; Restore packet pointer
        pop     es

        mov     es:[bx].RPR_BytesRequested, ReadDataLen

        jmp     short GetOut

DevHlpError:

        pop     bx                      ; Restore packet pointer
        pop     es

SetError:

        mov     es:[bx].RPR_BytesRequested, 0

        sti                             ; Enable Interrupts

GetOut:
        or      byte ptr es:[bx].RP_Status, RP_StatusDone

        ret

Read            ENDP

;**********************************************************************
;*------------------------------ Open --------------------------------*
;*                                                                    *
;*  HANDLE AN OPEN REQUEST.                                           *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: status byte set                                           *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Open            PROC    near

        cli                                     ; Disable interrupts

        cmp     UserCount, 0                    ; If not first user,
        jnz     AddlUser                        ;    bypass initialization

        mov     word ptr ReadDataBuf,     0     ; Set buffer to 0. This is
        mov     word ptr ReadDataBuf + 2, 0     ;    faster than saving and
        mov     word ptr ReadDataBuf + 4, 0     ;    restoring ES in order to
        mov     word ptr ReadDataBuf + 6, 0     ;    use STOSW

        Read8253IntoCx                          ; Get current tick count

        mov     Last8253, cx                    ; Save it for next interrupt

AddlUser:

        inc     UserCount                       ; Add another user

        sti                                     ; Enable interrupts

        or      byte ptr es:[bx].RP_Status, RP_StatusDone  ; Indicate DONE

        ret

Open            ENDP

;**********************************************************************
;*------------------------------ Close -------------------------------*
;*                                                                    *
;*  HANDLE A CLOSE REQUEST.                                           *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: status byte set                                           *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Close           PROC    near

        cli                             ; Disable interrupts

        cmp     UserCount, 0            ; If no users, don't do anything
        jz      NoUsers

        dec     UserCount               ; Decrement number of users

NoUsers:
        sti                             ; Enable interrupts

        or      byte ptr es:[bx].RP_Status, RP_StatusDone  ; Indicate DONE

        ret

Close           ENDP

;**********************************************************************
;*------------------------------ Error -------------------------------*
;*                                                                    *
;*  HANDLE AN UNSUPPORTED REQUEST.                                    *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: status byte set                                           *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Error           PROC    near

        mov     byte ptr es:[bx].RP_ErrorCode, 3  ; OS/2 Unknown Command RC
                                                  ; Indicate DONE and ERROR
        or      byte ptr es:[bx].RP_Status, RP_StatusError + RP_StatusDone

        ret

Error           ENDP

;**********************************************************************
;*---------------------------- DummyRet ------------------------------*
;*                                                                    *
;*  HANDLE A REQUIRED BUT UNUSED REQUEST                              *
;*                                                                    *
;*  INPUT: ES:BX = address of request packet                          *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: status byte set                                           *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

DummyRet        PROC    near

        or      byte ptr es:[bx].RP_Status, RP_StatusDone  ; Indicate DONE

        ret

DummyRet        ENDP

;**********************************************************************
;*---------------------------- Interrupt -----------------------------*
;*                                                                    *
;*  DEVICE DRIVER TIME-INTERRUPT ROUTINE. CALLED ON EACH OS/2 CLOCK   *
;*  TICK (MC146818 CHIP) VIA THE SetTimer DevHlp.                     *
;*                                                                    *
;*  INPUT: nothing                                                    *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: Updated time stamp                                        *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Interrupt       PROC    far

        pushf                           ; Save flags

        cmp     UserCount, 0            ; If no users, no need to do anything
        jz      NoUser

        push    ax                      ; Save registers
        push    bx
        push    cx
        push    dx

        cli                             ; Disable interrupts

        Read8253IntoCx                  ; Get current tick count

        sti                             ; Enable interrupts

        call    UpdateTimeStamp         ; Update the running time stamp

        pop     dx                      ; Restore registers
        pop     cx
        pop     bx
        pop     ax

NoUser:
        popf                            ; Restore flags

        ret

Interrupt       ENDP

;**********************************************************************
;*------------------------- UpdateTimeStamp --------------------------*
;*                                                                    *
;*  UPDATE THE RUNNING TIMESTAMP.                                     *
;*                                                                    *
;*  INPUT: CX contains current 8253 tick count                        *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: ReadDataBuf contains current time stamp,                  *
;*          Last8253 contains prior 8253 tick count,                  *
;*          AX, DX modified                                           *
;*                                                                    *
;*  NOTE: The 8253 counter counts from 65536 to zero                  *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

UpdateTimeStamp PROC    near

        push    bx                      ; Save bx for caller

        ;*******************************************************************
        ;* Determine the number of nanoseconds passed since the last       *
        ;*  timestamp update (NanosecsDelta):                              *
        ;*                                                                 *
        ;*     if( Last8253 >= cx )                                        *
        ;*         NanosecsDelta = (Last8253 - cx) * NanosIn1Tick          *
        ;*     else                                                        *
        ;*         NanosecsDelta = (0xFFFF - cx + Last8253) * NanosInATick *
        ;*                                                                 *
        ;* where cx is the current 8253 tick count                         *
        ;*   and NanosInATick is the number of nanoseconds in an 8253 tick *
        ;*                                                                 *
        ;*******************************************************************

        mov     ax, Last8253            ; Get prior tick count

        cmp     ax, cx                  ; If it has wrapped (i.e. gone to zero
        jae     NoWrap                  ;   and started again from 65536):

                                        ; Same as: mov  dx, 0FFFFh
        sub     ax, cx                  ;          sub  dx, cx
        dec     ax                      ;          add  dx, ax
                                        ;          xchg ax, dx
                                        ; Only faster....
        jmp     short CvtNsecs

NoWrap:
        sub     ax, cx                  ; Difference between current tick count
                                        ;   and last tick count
CvtNsecs:

        mov     dx, NanosInATick        ; Get total nanos from total ticks
        mul     dx

        mov     Last8253, cx            ; Save tick count for next interrupt

        ;*******************************************************************
        ;* Update the running timestamp and normalize the millisecond and  *
        ;*  nanosecond portions:                                           *
        ;*                                                                 *
        ;*     Nanoseconds += NanosecsDelta;                               *
        ;*                                                                 *
        ;*     if( Nanoseconds >= 1000000 )                                *
        ;*     {                                                           *
        ;*         Milliseconds = Milliseconds + (Nanoseconds / 1000000);  *
        ;*                                                                 *
        ;*         Nanoseconds = Nanoseconds % 1000000;                    *
        ;*     }                                                           *
        ;*                                                                 *
        ;* where Nanoseconds is ReadDataBuf.RD_Nanosecs                    *
        ;*   and Milliseconds is ReadDataBuf.RD_Millisecs                  *
        ;*                                                                 *
        ;* NOTE: 1000000 = 0xF4240                                         *
        ;*                                                                 *
        ;*******************************************************************

        add     word ptr ReadDataBuf.RD_Nanosecs, ax       ; Add lo-order word
        adc     word ptr ReadDataBuf.RD_Nanosecs + 2, dx   ; Add hi-order word
        cmp     word ptr ReadDataBuf.RD_Nanosecs + 2, 0Fh  ; > million?
        jb      UpdateExit                                 ;   NO: exit
        ja      Normalize                                  ;   YES: normalize

        cmp     word ptr ReadDataBuf.RD_Nanosecs, 4240h    ; < million?
        jb      UpdateExit                                 ;   YES: exit
                                                           ;   NO: normalize
Normalize:

        ;*******************************************************************
        ;* We need the result and remainder from dividing current nanos    *
        ;*  by 1,000,000. The 80286 doesn't support double-word divisors   *
        ;*  so we must stage the division. So we first divide by a number  *
        ;*  that is the quotient of 1,000,000 and 64. Then we divide by 64.*
        ;*  To do this:                                                    *
        ;*                                                                 *
        ;*  1. Divide Nanoseconds by (1000000 / 64). Save the remainder.   *
        ;*                                                                 *
        ;*  2. Divide the result of that by 64. Save the remainder.        *
        ;*                                                                 *
        ;*  3. Multiply the current remainder by (1000000 /64).            *
        ;*                                                                 *
        ;*  4. Add in the first remainder.                                 *
        ;*                                                                 *
        ;* We're left with the result from step 2 and the remainder from   *
        ;*  step 4.                                                        *
        ;*                                                                 *
        ;*******************************************************************

        mov     dx, word ptr ReadDataBuf.RD_Nanosecs + 2 ; hi-order word
        mov     ax, word ptr ReadDataBuf.RD_Nanosecs     ; lo-order word
        mov     bx, MillionDividedBy64                   ; divisor
        div     bx                                       ; do the divide

        mov     bx, dx                  ; Remainder
        mov     dx, ax                  ; Quotient
        and     dx, 00111111b           ; Low 6 bits = remainder from the next
                                        ;   divide (lower 6 bits are lost
                                        ;   during the 'shr ax, 6')
        mov     cx, dx
        shr     ax, 6                   ; Divide by 64

        xchg    ax, cx

        mov     dx, MillionDividedBy64  ; Multiply remainder from step 4
                                        ;    by (1000000 / 64)
        mul     dx

        add     ax, bx                  ; Add remainder from step 2

        mov     bx, 0                   ; Compensate for doubleword addition
        adc     dx, bx

        add     word ptr ReadDataBuf.RD_Millisecs, cx      ; Update millisecs
        adc     word ptr ReadDataBuf.RD_Millisecs + 2, bx  ; bx is still 0
        mov     word ptr ReadDataBuf.RD_Nanosecs, ax       ; Update nanosecs
        mov     word ptr ReadDataBuf.RD_Nanosecs + 2, dx

UpdateExit:

        pop     bx                      ; Restore BX

        ret

UpdateTimeStamp ENDP

;**********************************************************************
;*---------------------------- Initialize ----------------------------*
;*                                                                    *
;*  DEVICE DRIVER INTIALIZATION ROUTINE (DISCARDED BY OS2 AFTER USE)  *
;*                                                                    *
;*  INPUT: ES:BX = address of init packet                             *
;*                                                                    *
;*  1 -                                                               *
;*                                                                    *
;*  OUTPUT: nothing                                                   *
;*                                                                    *
;*--------------------------------------------------------------------*
;**********************************************************************

Initialize      PROC    near

        push    stdout                          ; Write copyright info
        push    ds
        push    offset CopyRightMsg
        push    CopyRightMsgLen
        push    ds
        push    offset BytesWritten
        call    DosWrite

        mov     ax, word ptr es:[bx].RPI_CodeSegLen   ; Save pointer to
        mov     word ptr DevHlpPtr , ax               ;   Device Helper routine
        mov     ax, word ptr es:[bx].RPI_DataSegLen
        mov     word ptr DevHlpPtr  + 2, ax

        cli                                     ; Disable interrupts

        mov     al, i8253CmdInitCtrZero         ; Set 8253 counter 0 to mode 2
        out     i8253CtrlByteRegister, al       ;     ( Rate generator )

        xor     ax, ax                          ; Init Count Register to zero
        out     i8253CountRegister, al          ;   by writing 0 to LSB
        out     i8253CountRegister, al          ;   and MSB

        sti                                     ; Enable interrupts

        mov     ax, offset Interrupt            ; Our timer hook address
        mov     dl, DevHlp_SetTimer             ; SetTimer function
        call    DevHlpPtr                       ; Call Device Helper routine
        jnc     NoError
                                                ; ****** ERROR ******
        push    stdout                          ; Write error message
        push    ds
        push    offset InitNotOkMsg
        push    InitNotOkMsgLen
        push    ds
        push    offset BytesWritten
        call    DosWrite

        mov     es:[bx].RPI_NumberUnits, 0      ; Zero these fields so OS/2
        mov     es:[bx].RPI_CodeSegLen, 0       ;    knows to cancel this
        mov     es:[bx].RPI_DataSegLen, 0       ;    device driver

        mov     byte ptr es:[bx].RP_ErrorCode, 0ch      ; General Failure error
        or      byte ptr es:[bx].RP_Status, RP_StatusError ; Error condition

        jmp     InitExit                        ; **** END ERROR ****

NoError:
        mov     es:[bx].RPI_CodeSegLen, offset Initialize   ; End of code seg
        mov     es:[bx].RPI_DataSegLen, offset LastData     ; End of data seg

InitExit:
        or      byte ptr es:[bx].RP_Status, RP_StatusDone   ; Indicate DONE

        ret

Initialize      ENDP

_TEXT           ENDS

END

;**********************************************************************
;*                       END OF SOURCE CODE                           *
;**********************************************************************
