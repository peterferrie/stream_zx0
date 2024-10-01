;license:MIT 
;(c) 2024 by qkumba 

;streaming ZX0 decompressor for Apple II ProDOS

;ZX0 decompressor adapted from DZX0 by Einar Saukas
;(https://github.com/einar-saukas/ZX0)

;assemble with ACME

!cpu 6502
!to "dzx0#062000",plain

*=$2000

;------------------------------------------------------------------------------
; current memory map (4K in, 4K out)
; 0800-0BFF - ProDOS input file buffer
; 0C00-0FFF - ProDOS output file buffer
; 1000-1FFF - input buffer
; 2000-2EFF - decompression code (not entirely)
; 2F00-3EFF - output buffer
; 3F00-BEFF - ZX0 history buffer
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; potential alternative memory map (2K in, 8K out)
; 0800-0BFF - ProDOS input file buffer
; 0C00-0FFF - ProDOS output file buffer
; 1000-17FF - input buffer
; 1800-1EFF - decompression code
; 1F00-3EFF - output buffer
; 3F00-BEFF - ZX0 history buffer
;------------------------------------------------------------------------------

verbose_info = 0     ; set to 1 to enable display of memory usage
MAX_OFFSET = 32640   ; constant, comes from zx0.c

BUFFER_SIZE = 32768  ; must be > MAX_OFFSET, and power of 2

INITIAL_OFFSET = 1   ; constant, comes from zx0.c

MAXPACKED = 4096     ; size of input buffer, larger is faster
MAXFILE   = 4096     ; size of output buffer, larger is faster, must be power of 2

infile  = $0800
outfile = $0C00
inbuff  = $1000
outbuff = $2F00
history = $3F00

; state machine
COPY_LIT         = 00 + <switch
COPY_NEW         = 02 + <switch
READ_BYTE2       = 04 + <switch
READ_BYTE1       = 06 + <switch
WRITE_BYTE1      = 08 + <switch
WRITE_BYTE2      = 10 + <switch
READ_GAMMA1      = 12 + <switch
READ_GAMMA2      = 14 + <switch
READ_GAMMA3      = 16 + <switch
READ_GAMMA4      = 18 + <switch
READ_BYTE1_SUB   = 20 + <switch
WRITE_BYTE1_SUB  = 22 + <switch
WRITE_BYTE2_SUB  = 24 + <switch
READ_GAMMA1_SUB1 = 26 + <switch
READ_GAMMA2_SUB1 = 28 + <switch
READ_GAMMA3_SUB1 = 30 + <switch
READ_GAMMA4_SUB1 = 32 + <switch
READ_GAMMA1_SUB2 = 34 + <switch
READ_GAMMA2_SUB2 = 36 + <switch
READ_GAMMA3_SUB2 = 38 + <switch
READ_GAMMA4_SUB2 = 40 + <switch

; zpage
last_offset     = $50 ; 2 bytes
output_index    = $52 ; 2 bytes
bit_mask        = $54 ; 1 byte
bit_value       = $55 ; 1 byte
backtrack       = $56 ; 1 byte
last_byte       = $57 ; 1 byte
total_records   = $58 ; 2 bytes
scratch         = $5A ; 2 bytes
i               = $5C ; 2 bytes
length          = $5E ; 2 bytes
c               = $60 ; 1 byte

         jmp   init
         !byte $ee,$ee,64
         !fill 64

switch
         !word copy_lit
         !word copy_new
         !word read_byte2
         !word read_byte1
         !word write_byte1
         !word write_byte2
         !word read_gamma1
         !word read_gamma2
         !word read_gamma3
         !word read_gamma4
         !word read_byte1_sub
         !word write_byte1_sub
         !word write_byte2_sub
         !word read_gamma1_sub1
         !word read_gamma2_sub1
         !word read_gamma3_sub1
         !word read_gamma4_sub1
         !word read_gamma1_sub2
         !word read_gamma2_sub2
         !word read_gamma3_sub2
         !word read_gamma4_sub2

         ;get prefix, if any
init
         lda   $bf30
         sta   c5_parms+1
-        jsr   $bf00
op_c7
         !byte $c7
         !word c7_parms
         ldx   $280
         bne   +

         ;if not, get volume name

         jsr   $bf00
         !byte $c5
         !word c5_parms
         lda   $281
         and   #$0f
         tax
         inx
         stx   $280
         lda   #$2f
         sta   $281

         ;set that as prefix

         dec   op_c7
         bne   -
+        lda   #$2f
         cmp   $280,x
         beq   +
         inx
         stx   $280
         sta   $280,x

         ;form absolute path

+        ldy   $2006
-        dey
         beq   +
         lda   $2006,y
         cmp   #$2f
         bne   -
         tya
         pha
         clc
         adc   $280
         sta   $280
         tax
-        lda   $2006,y
         sta   $280,x
         dex
         dey
         bne   -
         pla
         tay
         ldx   #0
-        iny
         lda   $2006,y
         sta   $2007,x
         inx
         cpy   $2006
         bne   -
         stx   $2006

         ;set that as prefix

         jsr   $bf00
         !byte $c6
         !word c7_parms

         ;open input file

+        jsr   $bf00
         !byte $c8
         !word c8_parms
         bcc   +
--       jmp   quit

         ;let's gooo

+        lda   #INITIAL_OFFSET
         sta   last_offset
         lsr
         sta   last_offset + 1
         ldx   #COPY_LIT
         stx   dispatch + 1
         sta   output_index
         sta   bit_mask
         sta   backtrack

         jsr   fetch_byte
;... your code here ...

done
quit     jsr   $bf00
         !byte $cc
         !word cc_parms
         jsr   $bf00
         !byte $65
         !word quit_parms

fetch_byte
         txa
         pha
fetch_byte1
         lda   avail_in
         ora   avail_in + 1
         bne   +
         jsr   $bf00
         !byte $ca
         !word ca_parms
         ldx   #0
         stx   input_index + 1
         ldx   #>inbuff
         stx   input_index + 2
         bcc   +
         cmp   #$46
         bne   quit
+        jsr   decompress
         beq   fetch_byte1
         pla
         tax
         lda   c
         rts

setstate
         sta   dispatch + 1

decompress
dispatch
         jmp   ($20d1)               ; low-byte SMC

copy_lit
COPY_LITERALS
         jsr   length1
         lda   #READ_GAMMA1
         bne   setstate              ; always

read_gamma1
read_gamma1_sub1
read_gamma1_sub2
         lda   #0
         sta   i
         sta   i + 1
         jsr   read_interlaced_elias_gamma
         beq   ++
         lda   #READ_BYTE1
         bne   setstate              ; always

read_byte1
         lda   i
         cmp   length
         lda   i + 1
         sbc   length + 1
         beq   read_byte1_sub
         jsr   read_byte
         beq   ++
         lda   last_byte
         jsr   write_byte
         inc   i
         bne   +
         inc   i + 1
+        lda   #1
++
-        rts

read_byte1_sub
         jsr   read_bit
         beq   -
         lda   #COPY_NEW
         dex
         beq   setstate              ; if (bit) goto COPY_FROM_NEW_OFFSET;

COPY_FROM_LAST_OFFSET
         jsr   length1
         lda   #READ_GAMMA2
         bne   setstate              ; always

read_gamma2
read_gamma2_sub1
read_gamma2_sub2
         jsr   read_interlaced_elias_gamma0
         beq   -
         lda   #WRITE_BYTE1
--       bne   setstate              ; always

write_byte1
         lda   length
         ora   length + 1
         beq   write_byte1_sub
         sec
         lda   output_index
         sbc   last_offset
         sta   scratch
         lda   output_index + 1
         sbc   last_offset + 1
         and   #>(BUFFER_SIZE - 1)
         clc
         adc   #>history
         sta   scratch + 1
         ldy   #0
         lda   (scratch), y
         jsr   write_byte
         lda   length
         bne   +
         dec   length + 1
+        dec   length
         lda   #1                    ; clear Z
-        rts

write_byte1_sub
         jsr   read_bit
         beq   -
         lda   #COPY_LIT
         dex
         bne   --                    ; goto COPY_LITERALS

copy_new
         jsr   length1
         lda   #READ_GAMMA3
         bne   --                    ; always

read_gamma3
read_gamma3_sub1
read_gamma3_sub2
         lda   #1
         jsr   read_interlaced_elias_gamma
         beq   -
         lda   #READ_BYTE2
         ldx   length
         bne   --
         ldx   length + 1
         dex
         bne   --
         asl                         ; clear Z
-        rts

read_byte2
         jsr   read_byte
         beq   -
         lda   last_byte
         lsr
         sta   last_offset
         ldx   #7
         lda   length
-        asl
         rol   length + 1
         dex
         bne   -
         sec
         sbc   last_offset
         sta   last_offset
         lda   length + 1
         sbc   #0
         sta   last_offset + 1
         inc   backtrack
         jsr   length1
         lda   #READ_GAMMA4
         bne   --                    ; always

read_gamma4
read_gamma4_sub1
read_gamma4_sub2
         jsr   read_interlaced_elias_gamma0
         beq   ++
         inc   length
         bne   +
         inc   length + 1
+        lda   #WRITE_BYTE2
         bne   +++                   ; always

write_byte2
         lda   length
         ora   length + 1
         beq   write_byte2_sub
         sec
         lda   output_index
         sbc   last_offset
         sta   scratch
         lda   output_index + 1
         sbc   last_offset + 1
         and   #>(BUFFER_SIZE - 1)
         clc
         adc   #>history
         sta   scratch + 1
         ldy   #0
         lda   (scratch), y
         jsr   write_byte
         lda   length
         bne   +
         dec   length + 1
+        dec   length
         lda   #1
++
-        rts

write_byte2_sub
         jsr   read_bit
         beq   -
         lda   #COPY_NEW
         dex
         beq   +++
         lda   #COPY_LIT
+++      jmp   setstate

length1
         lda   #1
         sta   length
         lsr
         sta   length + 1
         rts

read_byte
         lda   avail_in
         ora   avail_in + 1
         beq   ++

input_index
         lda   $d1d1                 ; SMC
         sta   last_byte
         inc   input_index + 1
         bne   +
         inc   input_index + 2
+        lda   avail_in
         bne   +
         dec   avail_in + 1
+        dec   avail_in
         lda   #1                    ; clear Z
++       rts

read_bit
         lda   backtrack
         beq   +
         dec   backtrack
         lda   last_byte
         and   #1
         tax
         lda   #1                    ; clear Z
         rts

+        lda   dispatch + 1
         cmp   #READ_BYTE1_SUB
         bcs   +
         lsr   bit_mask
         bne   ++
+        lda   #$80
         sta   bit_mask
         jsr   read_byte
         bne   +
         clc
         lda   dispatch + 1
         adc   #READ_BYTE1_SUB - READ_BYTE1
         sta   dispatch + 1
         lda   #0                    ; set Z
         rts

+        lda   last_byte
         sta   bit_value
++       ldx   #0
         lda   bit_value
         and   bit_mask
         beq   +
         inx
+        lda   #1                    ; clear Z
--       rts

read_interlaced_elias_gamma0
         lda   #0

read_interlaced_elias_gamma
         sta   c
-        lda   dispatch + 1
         cmp   #READ_GAMMA1_SUB2
         bcs   ++
         jsr   read_bit
         beq   --
         lda   dispatch + 1
         cmp   #READ_GAMMA1_SUB1
         bcc   +
         lda   #0
         sta   dispatch + 1
+        dex
         beq   +++
++       jsr   read_bit
         bne   +
         clc
         lda   dispatch + 1
         adc   #READ_GAMMA1_SUB2 - READ_GAMMA1_SUB1
         sta   dispatch + 1
         lda   #0                    ; set Z
         rts

+        lda   dispatch + 1
         cmp   #READ_GAMMA1_SUB2
         bcc   +
         lda   #0
         sta   dispatch + 1
+        txa
         eor   c
         lsr
         rol   length
         rol   length + 1
         bcc   -                     ; always

+++      lda   #1                    ; clear Z
         rts

write_byte
         sta   c
         ldx   output_index
         stx   scratch
         clc
         lda   output_index + 1
         adc   #>history
         sta   scratch + 1
         lda   c
         ldy   #0
         sta   (scratch), y
         ldy   output_index + 1
         inx
         bne   +
         iny
+        stx   output_index
         tya
         and   #>(BUFFER_SIZE - 1)
         sta   output_index + 1
         rts

c7_parms
         !byte 1
         !word $280

ccout_parms
         !byte 1
c5_parms
         !byte 2
         !byte 0
         !word $281
         !byte $d1

c6_parms
         !byte 1
         !word $380

c8_parms
         !byte 3
         !word $2006
c8_buff
         !word infile
         !byte 0

quit_parms
ca_parms
         !byte 4
cc_parms
         !byte 1
         !word inbuff
         !word MAXPACKED
avail_in
         !word 0

code_end
!if verbose_info = 1 {
  !warn "code ends at ", *
}
!if code_end > $2EFF {
  !error "code is too large, ends at ", *
}
