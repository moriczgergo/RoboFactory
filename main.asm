;****************************************************************************************************************************************************
;*	Blank Simple Source File
;*
;****************************************************************************************************************************************************
;*
;*
;****************************************************************************************************************************************************

;****************************************************************************************************************************************************
;*	Includes
;****************************************************************************************************************************************************
	; system includes
	INCLUDE	"inc/Hardware.inc"
	
;****************************************************************************************************************************************************
;*	user data (constants)
;****************************************************************************************************************************************************


;****************************************************************************************************************************************************
;*	equates
;****************************************************************************************************************************************************

	SECTION "Variables",WRAM0[$C000]
; variables for joypad read algorithms
prevjoyread ds 1 ; used by JoypadRead, has nothing usable for other stuff.
joyread ds 1 ; set by JoypadRead, $FF if it's the same as the previously processed joypad data
prevjoydread ds 1 ; used by JoypadDRead, has nothing usable for other stuff.
joydread ds 1 ; set by JoypadDRead, $FF if it's the same as the previously processed joydpad data
; GUI variables
oamupdated ds 1 ; if oamupdated's bit 0 is set, the OAM data will be updated on the latest VBlank, and oamupdated will be set to 0.

	SECTION "OAM Sprite Data",WRAM0[$C100]
OAM_SPRITES_DATA ds $100

;****************************************************************************************************************************************************
;*	cartridge header
;****************************************************************************************************************************************************

	SECTION	"Org $00",ROM0[$00]
RST_00:	
	jp	$100

	SECTION	"Org $08",ROM0[$08]
RST_08:	
	jp	$100

	SECTION	"Org $10",ROM0[$10]
RST_10:
	jp	$100

	SECTION	"Org $18",ROM0[$18]
RST_18:
	jp	$100

	SECTION	"Org $20",ROM0[$20]
RST_20:
	jp	$100

	SECTION	"Org $28",ROM0[$28]
RST_28:
	jp	$100

	SECTION	"Org $30",ROM0[$30]
RST_30:
	jp	$100

	SECTION	"Org $38",ROM0[$38]
RST_38:
	jp	$100

	SECTION	"V-Blank IRQ Vector",ROM0[$40]
VBL_VECT:
	call VBScript
	reti
	
	SECTION	"LCD IRQ Vector",ROM0[$48]
LCD_VECT:
	reti

	SECTION	"Timer IRQ Vector",ROM0[$50]
TIMER_VECT:
	reti

	SECTION	"Serial IRQ Vector",ROM0[$58]
SERIAL_VECT:
	reti

	SECTION	"Joypad IRQ Vector",ROM0[$60]
JOYPAD_VECT:
	reti
	
	SECTION	"Start",ROM0[$100]
	nop
	jp	Start

	; $0104-$0133 (Nintendo logo - do _not_ modify the logo data here or the GB will not run the program)
	DB	$CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
	DB	$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
	DB	$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E

	; $0134-$013E (Game title - up to 11 upper case ASCII characters; pad with $00)
	DB	"ROBOFACTORY"
		;0123456789A

	; $013F-$0142 (Product code - 4 ASCII characters, assigned by Nintendo, just leave blank)
	DB	"    "
		;0123

	; $0143 (Color GameBoy compatibility code)
	DB	$00	; $00 - DMG 
			; $80 - DMG/GBC
			; $C0 - GBC Only cartridge

	; $0144 (High-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0145 (Low-nibble of license code - normally $00 if $014B != $33)
	DB	$00

	; $0146 (GameBoy/Super GameBoy indicator)
	DB	$00	; $00 - GameBoy

	; $0147 (Cartridge type - all Color GameBoy cartridges are at least $19)
	DB	$19	; $19 - ROM + MBC5

	; $0148 (ROM size)
	DB	$01	; $01 - 512Kbit = 64Kbyte = 4 banks

	; $0149 (RAM size)
	DB	$00	; $00 - None

	; $014A (Destination code)
	DB	$01	; $01 - All others
			; $00 - Japan

	; $014B (Licensee code - this _must_ be $33)
	DB	$33	; $33 - Check $0144/$0145 for Licensee code.

	; $014C (Mask ROM version - handled by RGBFIX)
	DB	$00

	; $014D (Complement check - handled by RGBFIX)
	DB	$00

	; $014E-$014F (Cartridge checksum - handled by RGBFIX)
	DW	$00

;****************************************************************************************************************************************************
;*	Program Start
;****************************************************************************************************************************************************

	SECTION "Program Start",ROM0[$0150]

;****************************************************************************************************************************************************
;* Tile Data
;****************************************************************************************************************************************************
TILE_COUNT EQU 288
TILE_DATA:
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $00,$FF,$00,$FF,$00,$FF,$00,$FF
DB $FF,$00,$FF,$00,$FF,$00,$FF,$00
DB $FF,$00,$FF,$00,$FF,$00,$FF,$00
DB $FF,$EE,$FF,$AA,$FF,$AA,$FF,$AA
DB $FF,$AA,$FF,$AA,$FF,$AB,$FF,$BA
DB $00,$00,$5A,$5A,$24,$24,$42,$42
DB $42,$42,$24,$24,$5A,$5A,$00,$00
DB $BD,$BD,$42,$42,$81,$81,$81,$81
DB $81,$81,$81,$81,$42,$42,$BD,$BD
DB $00,$00,$00,$00,$24,$24,$18,$18
DB $18,$18,$24,$24,$00,$00,$00,$00
DB $18,$18,$3C,$3C,$5A,$5A,$81,$81
DB $18,$18,$3C,$3C,$3C,$3C,$18,$18
DB $08,$08,$04,$04,$62,$62,$F7,$F7
DB $F7,$F7,$62,$62,$04,$04,$08,$08
DB $07,$07,$04,$04,$04,$04,$04,$04
DB $04,$04,$FC,$FC,$80,$80,$80,$80
DB $80,$80,$80,$80,$FC,$FC,$04,$04
DB $04,$04,$04,$04,$04,$04,$07,$07
DB $1F,$1F,$3F,$3F,$70,$70,$E0,$E0
DB $C7,$C7,$C4,$C4,$C4,$C4,$C4,$C4
DB $C7,$C7,$C4,$C4,$C4,$C4,$C4,$C4
DB $E0,$E0,$70,$70,$3F,$3F,$1F,$1F
DB $1F,$1F,$3F,$3F,$70,$70,$E0,$E0
DB $C7,$C7,$C4,$C4,$C4,$C4,$C4,$C4
DB $C4,$C4,$C4,$C4,$C4,$C4,$E7,$E7
DB $F0,$F0,$78,$78,$3F,$3F,$1F,$1F
DB $F8,$F8,$FC,$FC,$0E,$0E,$07,$07
DB $C3,$C3,$23,$23,$23,$23,$C3,$C3
DB $23,$23,$23,$23,$23,$23,$C7,$C7
DB $0F,$0F,$1E,$1E,$FC,$FC,$F8,$F8

;****************************************************************************************************************************************************
;* Map Data
;****************************************************************************************************************************************************
MAP_SIZE EQU 1024 
MAP_DATA:
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$00,$01,$02,$01,$02,$01,$02,$01,$02,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$00,$00,$02,$02,$00,$00,$02,$02,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$01,$01,$02,$00,$00,$00,$00,$02,$01,$01,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$01,$00,$02,$02,$00,$00,$02,$02,$00,$01,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

OAM_SPRITES_SIZE EQU 16
OAM_SPRITES:
DB 128,64,12,%00000000
DB 136,64,13,%00000000
DB 128,72,12,%00100000
DB 136,72,13,%00100000
DB 128,96,10,%00000000
DB 136,96,11,%00000000
DB 128,104,10,%00100000
DB 136,104,11,%00100000
DB 130,100,8,%00000000

Start::
	DI ;disable interrupts while init
.WAIT
	LDH A,[$FF44] ;get current scanline
	CP $91 ;check if scanline is in VBlank
	JR NZ,.WAIT ;wait if we aren't in VBlank yet

	;disable screen
	LD A,0
	LDH [rLCDC],A

	;initialize screen
	LD A,%11100100
	LDH [$FF47],A

	call ClearScreen
	call CopyGraphics

	; reset joypad reads to $FF
	LD A, $FF
	LD [joyread], A
	LD [prevjoyread], A

	LD [oamupdated], A ; set OAMUpdated to $FF for initial print

	; prepare OAM_DMA copy to HRAM
	LD HL,OAM_DMA ; load OAM_DMA's address to HL
	LD BC,$1080 ; B=$10, a.k.a. $10 bytes to copy, C=$80 a.k.a copy to $(FF)80
.oamcopy ; this is the routine to copy OAM_DMA to HRAM
	LD A,[HL+] ; load current value at HL and increment HL
	LD [C],A ; save loaded HL value to C's place

	INC C ; increment copy addr
	DEC B ; decrement bytes-to-copy counter
	JR NZ,.oamcopy ; if B wasn't zero after decrementing, repeat copy
.videoinit
	;enable the VBlank interrupt
	LD A,%00000001
	LDH [$FFFF],A

	;enable and configure screen
	LD A,LCDCF_ON|LCDCF_OBJON|LCDCF_BGON|LCDCF_BG8000 ; OR is fun!
	LD [rLCDC],A ; rLCDC: LCD config register defined in HARDWARE.INC
.loopjump
	EI ;enable interrupts
	LD B,0 ; setup B for loop
	jr loop

OAM_DMA: ; note: i can't comment this properly, due to lack of knowledge. if you do, open an issue on GitHub explaning this.
    ld    a,high(OAM_SPRITES)
    ld    [rDMA],a
    ld    a,$28
.wait ; wait for $28(*2?) cycles
    dec    a ; decrement A
    jr    nz,.wait ; if A isn't zero yet, jump back to .wait
    ret ; OAM_DMA will be `call`-ed. let's use `ret` to return to the rest of the VBlank code!

loop::
	call JoypadRead
	call JoypadDRead
	call joypad_test
	jr loop

JoypadRead:: ; reads joypad ABSelectStart data
	ld hl, $FF00 ; load the joypad data address into HL

	; set up joypad, select to read ABSelectStart
	SET 4, [HL]
	RES 5, [HL] ; reset D-Pad read bit, in case it's set

	LD A, [HL] ;load joypad data into A
	ld hl, prevjoyread ;load prevjoyread's address into HL
	CP [hl] ;compare previous joypad data with currentjoypad data
	jr z,.EQ ; if they're the same, jump to .EQ
	jr .DIFF ; if they aren't the same, jump to .DIFF
.EQ ; this will be jumped to if the joypad data hasn't changed yet
	LD HL,joyread ; load joyread's address into HL
	LD [HL],%11111111 ; set joyread to $FF, representing that the value hasn't changed from the previous one
	jr .RETURN
.DIFF
	LD [joyread],A ; load new data into joydread
	jr .RETURN
.RETURN
	LD [prevjoyread],A ; load new data into prevjoyread
	ret

JoypadDRead:: ; reads joypad D-Pad data
	ld hl, $FF00 ; load the joypad data address into HL

	; set up joypad, select to read D-Pad
	SET 5, [HL]
	RES 4, [HL] ; reset ABSelectStart read bit, in case it's set

	LD A, [HL] ;load joypad data into A
	ld hl, prevjoydread ;load prevjoydread's address into HL
	CP [hl] ;compare previous joypad data with current joypad data
	jr z,.EQ ; if they're the same, jump to .EQ
	jr .DIFF ; if they aren't the same, jump to .DIFF
.EQ
	LD HL,joydread ; load joydread's address into HL
	LD [HL],%11111111 ; set joydread to $FF, representing that the value hasn't changed from the previous one
	jr .RETURN
.DIFF
	LD [joydread],A ; load new data into joydread
	jr .RETURN
.RETURN
	LD [prevjoydread],A ; load new data into prevjoydread
	ret

joypad_test::
	LD HL, joyread
	BIT 0, [HL] ; test A button
	jr nz,.RETURN
	;inc b
	LD A,1
	LD [oamupdated],A
	jr .RETURN
.RETURN
	ret

copy_tiles:: ; copies BG tiles
	; setting up base addresses
	ld de, TILE_DATA ; load local tile data address
	ld hl, $8000 ; load destination address
	ld bc, TILE_COUNT ; load count of bytes in the tile data

	; increase b and c, since we're jumping to .skip by default
	inc b
	inc c

	jr .skip
.loop
	ld a, [de] ; load current tile data into A
	ld [hli], a ; load current tile data from A into HL, and increment HL
	inc de ; increment DE
.skip
	dec c ; decrement c
	jr nz,.loop ; if it's not zero, copy again
	dec b ; decrement b
	jr nz,.loop ; if it's not zero, copy again
	ret

copy_map:: ; copies BG tile map
	; setting up base addresses
	ld de, MAP_DATA ; load local map data address
	ld hl, $9800 ; load destination address
	ld bc, MAP_SIZE ; load count of bytes in the map

	; increase b and c, since we're jumping to .skip by default
	inc b
	inc c

	jr .skip
.loop
	ld a, [de] ; load current tile data into A
	ld [hli], a ; load current tile data from A into HL, and increment HL
	inc de ; increment DE
.skip
	dec c ; decrement c
	jr nz,.loop ; if it's not zero, copy again
	dec b ; decrement b
	jr nz,.loop ; if it's not zero, copy again
	ret

copy_oam:: ; copies OAM sprite data
	; setting up base addresses
	ld de, OAM_SPRITES ; load local OAM data address
	ld hl, OAM_SPRITES_DATA ; load destination address
	ld bc, OAM_SPRITES_SIZE ; load count of bytes in the OAM sprites

	; increase b and c, since we're jumping to .skip by default
	inc b
	inc c

	jr .skip
.loop
	ld a, [de] ; load current tile data into A
	ld [hli], a ; load current tile data from A into HL, and increment HL
	inc de ; increment DE
.skip
	dec c ; decrement c
	jr nz,.loop ; if it's not zero, copy again
	dec b ; decrement b
	jr nz,.loop ; if it's not zero, copy again
	ret

CopyGraphics::
	call copy_tiles
	call copy_map
	call copy_oam
	ret

wipe_map:: ; wipes BG tile map
	ld  hl,$9800 ; load map data address
	ld  bc,$07FF ; bytes to clear count
.loop
	xor a ; quickhand ld a,0
	ld  [hl+],a ; load hl with 0, and increment hl
	dec bc ; decrease counter
	ld  a,b ; load upper 8bits of counter into a
	or  c ; a | c, check if the whole counter is 0
	jr  nz,.loop ; if it's not zero, loop again
	ret

wipe_oam::
	ld  hl,$fe00 ; load OAM data address
	ld  b,$9F ; bytes to clear count
	xor a ; quickhand ld a,0
.loop
	ld  [hl+],a ; load hl contents with 0, and increment hl
	dec b ; decrease b
	jr  nz,.loop ; if it's not zero, loop again
	ret

ClearScreen::
	call wipe_map
	call wipe_oam
	ret

VBScript:: ; vblank procedure
	; push all registers to stack
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL

	LD HL, oamupdated ; load oamupdated's address to HL
	BIT 0,[HL] ; check but 0 of oamupdated
	jr z, .RETURN ; if it's zero, we don't need to draw the OAM, so just jump to return.
	call $FF80 ; let's call OAM_DMA, copied to $FF80 by Start.oamcopy
	LD [HL], $00 ; resetting oamupdated
.RETURN
	; restore the registers from stack
	POP HL
	POP DE
	POP BC
	POP AF
	ret
;*** End Of File ***