/*
    Wa-Tor for commodore 64

Rules for fish
1) At every quantum of time, the fish move randomly in one of the adjacent squares, 
    provided that there is one free, that is, without sharks and fish inside it.
    If there are no free spaces, no movement takes place.
2) The fish have an associated breeding time, after which the fish can reproduce.
    This happens if the fish can move to a new position (and leave the new born in the position previously occupied).
    If this happens its playing time returns to zero.

Rules for sharks
1) At each chronon, the sharks randomly move to one of the adjacent squares occupied by the fish.
    If there are none, they move to a random square among the adjacent ones as long as there are no sharks inside. If no square meets the requirements, no movement takes place.
2) Each turn the sharks are deprived of one unit of energy.
3) If it moves into a square occupied by a fish, the shark eats it and gains a certain amount of energy.
4) If the energy exceeds a certain reproductive threshold, the shark gives birth to another shark in an adjacent free cell, as long as there is one.
    If so, his energy is split in half with the offspring.
5) If a shark's energy level is below zero, the shark dies.


The Wa-Tor use Von-Neumann neighbour

Field $0400-$07E7
no-Border left right and the upper border is not linked with the lower one

Cell positions:
     U
    LCR
     D

(C)urrent cell
(U)pper neighbour
(L)eft neighbour
(R)ight neighbour
(D)own neighbour


- Clear Screen
- const: max breeding time
- positioning sharks and fish randomly
- set randomly energy to the sharks and a random breeding time to the fish


*/

.const SCREEN = $0400

.const ZPCNTL= $02              // Counter
.const ZPCNTH= $03              // CounterH
.const ZPBUFL= $04              // Buffer Pointer Low Byte
.const ZPBUFH= $05

.const ZPFCNTL= $06              // Fishes Counter of the allocated records low-byte
.const ZPFCNTH= $07              // Fishes Counter of the allocated records high-byte
.const ZPSCNTL= $08              // Sharks Counter of the allocated records low-byte
.const ZPSCNTH= $09              // Sharks Counter of the allocated records high-byte

/*Free Bytes $0B-$0D*/
.const ZPTABADR= $0A            // Flag from Buffer

.const ZPTABL  = $0E             // Tab address low
.const ZPTABH  = $0F             // Tab address high

.const ZPRCNTL = $10             // record counter low byte
.const ZPRCNTH = $11             // high byte

.const ZPFECNTL= $12                // Counter of the empty records tables for the fishes
.const ZPFECNTH= $13                // Counter of the empty records tables for the fishes
.const ZPSECNTL= $14                // Counter of the empty records tables for the sharks
.const ZPSECNTH= $15                // Counter of the empty records tables for the sharks

.const ZPFEADL = $16                // stack pointer for fishes it will point on last free record
.const ZPFEADH = $17                // stack pointer for fishes it will point on last free record

.const ZPCNTBYTE = $18              // Byte counter for temporary purpose

.const ZPTABLCOPY= $19              // Copy of current table pointer
.const ZPTABHCOPY= $1A              // used on for save/restore ZTABL/H

.const ZPTABADLCOPY= $1B             // Copy of the Buffer for table low byte address
.const ZPTABADHCOPY= $1C             // Copy of the Buffer for table high byte address
.const ZPTABVALCOPY= $1D             // Copy of the Buffer for table value (breeding/energy)
.const ZPTABADRCOPY= $1E             // Copy of the Buffer for table flag 

.const ZPSEADL = $1F                // stack pointer for sharks it will point on last free record
.const ZPSEADH = $20                // stack pointer for sharks it will point on last free record

.const ZPTABADL= $21             // Buffer for table low byte address
.const ZPTABADH= $22             // Buffer for table high byte address
.const ZPTABVAL= $23             // Buffer for table value (breeding/energy)
.const ZPTABNXL= $25             // Buffer for table NextRecord Low Byte
.const ZPTABNXH= $26             // Buffer for table NextRecord High Byte ($00 = Last Record)

.const ZPTABLASTL= $27           // Pointer Low byte to first empty record
.const ZPTABLASTH= $28           // Pointer High byte to first empty record
.const ZPTABFREEL= $29           // Pointer Low byte of free record
.const ZPTABFREEH= $2A           // Pointer high byte of free record ($00) if no free records

.const ZPFREECNTL=$2B            // deleted record counter
.const ZPFREECNTH=$2C

.const ZPPREVL=$2D               // Prevois record Low byte
.const ZPPREVH=$2E               // Previous record Hi Byte ($00 does not exitsts)

.const ZPTABINIL=$2F            // Pointer for record manager to the first logical record on the table
.const ZPTABINIH=$30            // low and high value

.const ZPTABINIFL=$31           // Pointer to the first logical record on the table for Fishes
.const ZPTABINIFH=$32           // low and high value

.const ZPTABINISL=$33           // Pointer to the first logical record on the table for Sharks
.const ZPTABINISH=$34           // low and high value

.const ZPTABLASTFL=$35           // Pointer to the last logical record on the table for Fishes
.const ZPTABLASTFH=$36           // low and high value

.const ZPTABLASTSL=$37           // Pointer to the last logical record on the table for Sharks
.const ZPTABLASTSH=$38           // low and high value




.const ZPFB = $FB
.const ZPFC = $FC
.const ZPFD = $FD
.const ZPFE = $FE



.const OP_BPL = $10             // Opcode for dynamic programming
.const OP_BNE = $D0


.const FREECELL = ' '+128       // Free cell's char
.const FISH     = '.'           // Fish's char
.const SHARK    = '>'           // Shark's char
.const BORDER_CHAR = '-'        // border char

                                // NUMFISH funziona con numeri piccoli ma non con numeri come 99 100 -....
.const NUMFISH  = 1            // number of the fishes population at start
.const NUMSHARK = 5             // number of the sharks population at start

.const BREEDTIMEMAX = 100


.const REC_MSK_OCCUPIED     = %00000001
.const REC_MSK_REUSED       = %00000010

.const RNDVOICE3= $D41B
BasicUpstart2(Entry)

/*
    FishCounter         2 bytes Max 1000
    Sharkcounter        2 bytes Max 1000

    First empties records:
    n Fish
    n Shark
    pick from last
    EmptyFishRecord     2 bytes * 1000
    SharkFishRecord     2 bytes * 1000

    Fish:
        address         2 bytes // empty if void
        breeding time   1 byte
        empty           1 byte

    shark:
        address         2 bytes // empty if void
        energy          1 byte
        empty           1 byte

    add fish add a fish first looking in the empties slot
    del fish del a fish and add it to the empties slot
    same for shark


*/
// Cambiare il modo di gestire le tavole in modo che ogni record punti al successivo!


Entry:
            jsr InitWaTor

            lda #00                         // Pass the deleted record counter
            sta ZPFREECNTL
            sta ZPFREECNTH

            lda #<FishTab
            sta ZPTABL
            sta ZPTABLASTFL
            sta ZPTABLASTL

            lda #>FishTab
            sta ZPTABH
            sta ZPTABLASTFH
            sta ZPTABLASTH

            lda #<EmptyFRec
            sta ZPTABFREEL
            lda #>EmptyFRec
            sta ZPTABFREEH


            lda #<SCREEN
            sta ZPTABADL
            lda #>SCREEN
            sta ZPTABADH

            lda #01
            sta ZPTABVAL

            jsr RecordInsert
            inc ZPFCNTL
            inc ZPTABL
                
            lda #<SCREEN+1
            sta ZPTABADL
            lda #>SCREEN+1
            sta ZPTABADH

            lda #02
            sta ZPTABVAL
            
            jsr RecordInsert

            inc ZPFCNTL
            inc ZPTABL
                
            lda #<SCREEN+2
            sta ZPTABADL
            lda #>SCREEN+2
            sta ZPTABADH

            lda #03
            sta ZPTABVAL
            
            jsr RecordInsert
             
            lda #00
            sta ZPPREVL
            sta ZPPREVH
            lda #<FishTab
            sta ZPTABL
            sta ZPTABINIL

            lda #>FishTab
            sta ZPTABH
            sta ZPTABINIH

            jsr RecordDelete


            jsr RecordInsert

            jmp *


            jsr WaitForSpace
            jsr PopulateFishes
            //jsr PopulateSharks
!Entry_Loop:
            // jsr WaitForSpace
            jsr FishMoves
            //jsr FishClearReusedFlag
            lda #250
!WVB:
            cmp $D012
            bcs !WVB-
            jmp !Entry_Loop-

            lda #<FishTab
            sta ZPTABL
            lda #>FishTab
            sta ZPTABH

            jsr DelRec

            ldx #00
            stx ZPFCNTL 
            ldx #00
            stx ZPFCNTH 
            
            lda #<SCREEN
            sta ZPTABADL
            lda #>SCREEN
            sta ZPTABADH
            jsr InsFishRec

            ldx ZPFCNTL                             // Allocate a new record record
            inx
            stx ZPFCNTL
            bne !IFR_OK+
            inc ZPFCNTH
!IFR_OK:

            //jsr PopulateS
            lda ZPFCNTL
            sta $0400
            jmp *


            ldx #$05
            lda #$30
            sta ZPFB
            stx ZPFC

            lda #FISH
            ldy #00
            sta (ZPFB),y

            jsr WaitForSpace
            jsr FreeCellFish

Dummy_Loop:
            tya
            clc
            adc ZPFD
            sta ZPFB
            bcc Dummy_noinc
            inc ZPFE
Dummy_noinc:
            lda ZPFE
            sta ZPFC
            //jsr WaitForSpace
            jsr FreeCellFish
!loop:            
            lda $D012
            cmp #250
            bcc !loop-
            //inc $0400
            jmp Dummy_Loop

//////////////////////////////////
// FishMoves
//////////////////////////////////
FishMoves:
            lda #<FishTab
            sta ZPTABL
            lda #>FishTab
            sta ZPTABH

            lda ZPFCNTL                         // Place number of fishes on a generic counter
            sta ZPCNTL
            lda ZPFCNTH
            sta ZPCNTH
FM_Loop:
            jsr GetNextRecValues                // Load next fish address on ZPTADADL/H
            lda ZPTABADR                        // see the reused flag
            and #REC_MSK_REUSED                 // is a reused record ?
            bne FM_Next                         // if is a reused flag (inside thsi loop, ignore the fish)
            lda ZPTABADR                        // see the occupied flag
            and #REC_MSK_OCCUPIED               // is a free record ?
            beq FM_Next                         // if is a free record go to next
            lda ZPTABADL                        // Get the fish address
            sta ZPFB                            // and save it on ZPFB/ZPFC
            lda ZPTABADH
            sta ZPFC
            jsr FreeCellFish                    // FD/FE will be the next fish position ... if there was a move
            beq FM_Next                         // if there is no Moves then skip
FM_CkeckBreed:                  
                                                // Check breeding time
                                                // if breeding time is beyond BREEDTIMEMAX
                                                // do not delete old position
            inc ZPTABVAL                        // Increment the breeding index
            lda ZPTABVAL                        // check if 
            cmp #BREEDTIMEMAX                   // max breeding is reached
            bcc FM_DeleteAndMoveFish            // if max breeding is NOT reached then move only current fish ...
                                                // ... else create a copy of the current fish to another record
                                                // with half breeding to new fish and half to the current one 
                                                // and move the current fish
            lsr ZPTABVAL                        // Breeding = Breeding/2    
            jsr CopyCurrentFishRec              // duplicate the fish in current position
            jmp FM_MoveCurrentFish              // Move the fish to the next position
FM_DeleteAndMoveFish:
            ldy #00                             //
            lda #FREECELL                       // Free the currentfish cell
            sta (ZPFB),y                        //
FM_MoveCurrentFish:
            lda ZPFD                            // Move The current fish
            sta ZPTABADL                        // to new "random" location
            lda ZPFE
            sta ZPTABADH                        // place the new coordinates on transition buffer
            jsr IR_SaveValues                   // save values on current record
            ldy #00
            lda #FISH
            sta (ZPFD),y
FM_Next:
            lda ZPTABL
            clc
            adc #4
            sta ZPTABL
            bcc !FM_OK+
            inc ZPTABH
!FM_OK:            
            dec ZPCNTL                          // Decrement low byte by one
            bne FM_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne FM_Loop                         // if the hi byte is not zero then continue ...
            rts                                 // else if is it zero too then return to the caller
FM_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            bne FM_Loop                         // .. isn't FF then continue with next
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp FM_Loop                         // and continue with the next loop
            rts

//////////////////////////////////
// Insert a newFish
//////////////////////////////////
InsertANewFish:
            rts


//////////////////////////////////
// CopyCurrentFishRec
//////////////////////////////////
// Make a copy of current record to a new record
CopyCurrentFishRec:
            jsr SaveCurrentTABPOINTER               // Save tab pointers
            jsr InsFishRec                          // duplicate fish record
            jsr RestoreCurrentTABPOINTER            // restore tab pointers
            rts

//////////////////////////////////
// SaveCurrentTABPOINTER
//////////////////////////////////
SaveCurrentTABPOINTER:
            lda ZPTABL
            sta ZPTABLCOPY
            lda ZPTABH
            sta ZPTABHCOPY
            rts
//////////////////////////////////
// RestoreCurrentTABPOINTER
//////////////////////////////////
RestoreCurrentTABPOINTER:
            lda ZPTABLCOPY
            sta ZPTABL
            lda ZPTABHCOPY
            sta ZPTABH
            rts
//////////////////////////////////
// Place a Fish on XY
//////////////////////////////////
// x and y are coordinates, the cell MUST BE free
PlaceFishXY:
            jsr XYToScreen
            ldy #00
            lda #FISH                           // Display Fish to occupy the cell
            sta (ZPFB),y

            lda ZPFB
            sta ZPTABADL
            lda ZPFC
            sta ZPTABADH

            lda RNDVOICE3                       // Random Breeding time
            sta ZPTABVAL

                                                // Insert a new Fish in the tab
                                                // Data on ZPTAB...
                                                //
                                                // ZPTABADL/H contains the address of the fish
                                                // ZPTABVAL   contains the value of the breeding or the energy
            jsr InsFishRec
            rts



//////////////////////////////////
// Free for fish (FREECELL)
// looking around ZPFB/ZPFC
// output:
// ZPFD/ZPFE (new place)
// A free cell counter
//////////////////////////////////
FreeCellFish:
            ldy #00
            sty ZPCNTBYTE           // ZPCNTByte will contains the number of free cells
            lda ZPFB                // Copy the actual cell address to  
            ldx ZPFC                
            sta ZPFD                // vector ZPFD/ZPFE
            stx ZPFE
                                    // Position of (U)pper cell neighbour
            sec
            sbc #40
            sta ZPFD
            bcs FCF_NoDecHi
            dec ZPFE
            ldy #00

FCF_NoDecHi:       
            ldx #03                 // number of ZPFB/FC neighbour
FCF_Loop:
            ldy NeighbourY,x
            lda (ZPFD),y
            cmp #FREECELL
            bne FCF_Next
            txa
            ldy ZPCNTBYTE
            sta (ZPBUFL),y                      // save the freeoffset
            inc ZPCNTBYTE
FCF_Next: 
            dex
            bpl FCF_Loop
            lda ZPCNTBYTE                       // ZPCNTBYTE contain the number of free cell
            beq FCF_Exit                        // if there are no free neighbour then exit
                                                // we have to get a random number from 0 to ZPNCT-1
                                                // and pick the value of y from (ZPBUFL),y
FCF_RndLoop:
            lda ZPCNTBYTE
            cmp #1
            beq FCF_OnlyOne
            lda RNDVOICE3                       // random number from 0 to 255 -> a
            and #%00001100                      // max value for y is 3 pick up two bits
            lsr                                 // and shift it to the righ
            lsr                                 // to reach a value in the range 0-3
            cmp ZPCNTBYTE                           // compare with max value
            bcs FCF_RndLoop                     // if too much big then retry with another random number
            jmp FCF_LoadFromBuffer            
FCF_OnlyOne:
            lda #0            
FCF_LoadFromBuffer:            
            tay
            lda (ZPBUFL),y                      // Load offset on Acc
            tax
            lda NeighbourY,x            
            clc                                 // clear carry to add offset to base address ...
            adc ZPFD                            // ... on ZPFD/FE
            sta ZPFD
            bcc FCF_Exit                        // if carry is clear no more op
            inc ZPFE                            // else inc hi byte!
FCF_Exit:
            lda ZPCNTBYTE                       // number of free cells
            rts

            // Buffer
FreeCellOffset:
            .fill $00, 4


//////////////////////////////////
// Populate Sharks //
//////////////////////////////////
PopulateSharks:
            lda #<NUMSHARK
            sta ZPCNTL
            lda #>NUMSHARK
            sta ZPCNTH
PS_Populate:
            lda RNDVOICE3
            cmp #40
            bcs PS_Populate
            tax
PS_Pop_Y:
            lda RNDVOICE3
            cmp #25
            bcs PS_Pop_Y
            tay

            jsr XYToScreen
            ldy #00
            lda (ZPFB),y
            cmp #FREECELL
            bne PS_Populate

            lda #SHARK                           // Display Shark to occupy the cell
            sta (ZPFB),y

            lda ZPFB
            sta ZPTABADL
            lda ZPFC
            sta ZPTABADH

            lda RNDVOICE3                       // Random Energy
            //lda #00                             // for debug purpose only
            sta ZPTABVAL

                                                // Insert a new Shark in the tab
                                                // Data on ZPTAB...
                                                //
                                                // ZPTABADL/H contains the address of the fish
                                                // ZPTABVAL   contains the value of the breeding or the energy
            jsr InsSharkRec

            dec ZPCNTL                          // Decrement low byte by one
            bne PS_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne PS_Populate                     // if the hi byte is not zero then continue ...
            rts                                 // else if is it zero too then return to the caller
PS_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            bne PS_Populate                     // .. isn't FF then continue with next
PS_DecH:            
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp PS_Populate                     // and continue with the next loop

//////////////////////////////////
// Populate Fishes //
//////////////////////////////////
PopulateFishes:
            lda #<NUMFISH
            sta ZPCNTL
            lda #>NUMFISH
            sta ZPCNTH
PF_Populate:
            lda RNDVOICE3
            //lda #19
            cmp #40
            bcs PF_Populate
            tax
PF_Pop_Y:
            lda RNDVOICE3
            //lda #12
            cmp #25
            bcs PF_Pop_Y
            tay

            jsr XYToScreen
            ldy #00
            lda (ZPFB),y
            cmp #FREECELL
            bne PF_Populate

            lda #FISH                           // Display Fish to occupy the cell
            sta (ZPFB),y

            lda ZPFB
            sta ZPTABADL
            lda ZPFC
            sta ZPTABADH

            lda RNDVOICE3                       // Random Breeding time
            //lda #00                             // for debug purpose only
            sta ZPTABVAL

                                                // Insert a new Fish in the tab
                                                // Data on ZPTAB...
                                                //
                                                // ZPTABADL/H contains the address of the fish
                                                // ZPTABVAL   contains the value of the breeding or the energy
            jsr InsFishRec

            dec ZPCNTL                          // Decrement low byte by one
            bne PF_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne PF_Populate                     // if the hi byte is not zero then continue ...
            rts                                 // else if is it zero too then return to the caller
PF_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            bne PF_Populate                     // .. isn't FF then continue with next
PF_DecH:            
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp PF_Populate                     // and continue with the next loop        
//PF_Exit:
//            rts


//////////////////////////////////
// XY To Screen Address (SCREEN) //
//////////////////////////////////
// x is x coordinate (0-39)
// y is y coordinate (0-24)
// the output will be placed on FB/FC vector
XYToScreen:
            // Offset From Screen Base is Y*40+X
            lda #<SCREEN
            sta ZPFC
            tya
            asl
            asl
            asl         // Y*8 im sure that there is not overflow until now. (Max can be 24*8 192)
            sta ZPFB     // save Y*8 into $FB to complete the operation at the end
            rol         // Y*16
            rol ZPFC
            rol         // Y*32
            rol ZPFC
            clc
            adc ZPFB     // Y*32 + Y*8 (Stored in $FB) = Y*40
            sta ZPFB
            bcc XYS_AddX
            inc ZPFC
XYS_AddX:
            clc
            txa
            adc ZPFB
            sta ZPFB
            bcc XYS_Add04
            inc ZPFC
XYS_Add04:            
            lda ZPFC
            clc
            adc #>SCREEN    // $0400 + Y*40 + X
            sta ZPFC
            rts

/////////////////////////////////
// Fill Screen Address ($0400) //
/////////////////////////////////
// Input A
/////////////////////////////////
FillScr:
            sta FS_Value
            ldx #<SCREEN
            stx ZPFB
            ldx #>SCREEN
            stx ZPFC
FS_Loop:
            ldy #199
FS_LoopInner:
            lda FS_Value:#$FF
            sta (ZPFB),y
            dey
            cpy #$FF
            bne FS_LoopInner
            clc
            lda ZPFB
            adc #200
            sta ZPFB
            bcc FS_Dex
            inc ZPFC
FS_Dex:
            dex
            bpl FS_Loop
            rts

//////////////////////
// Banking out K&B
//////////////////////
BankingOutKB:
           sei             // disable interrupts (the maskerable ones)

        //-------------------------------------------------------------------------------------------------------------
        // Disable the CIA Interrupts and clear them too
        //-------------------------------------------------------------------------------------------------------------

            ldy #$7f        // $7f = %01111111 deactivation Mask for
                            // interrupts on chip CIA 1 e 2
                                                        
            sty $dc0d       // Disable IRQs CIA1
            sty $dd0d       // Disable IRQs CIA2
            lda $dc0d       // clear any queued IRQs on CIA1
                                                        
            lda $dd0d       // clear any queued IRQs on CIA2

        //-------------------------------------------------------------------------------------------------------------
        // Switching the Kernal and BASIC Rom Off
        //-------------------------------------------------------------------------------------------------------------

                            // KERNAL & BASIC banking out
                            // Loading the mask ($ 35) to configure how
                            // the C = 64 must see RAM memory
                            // putting 01 on bits 0 and 1 we exclude
                            // the ROM BASIC and KERNAL
            lda #%00110101                                            
                                                        
                                                        
                                                        
            sta $01         // byte $0001 is THE configuration byte

            cli             // Enabling interrupt
            rts

//////////////////////////////////////////
// Wait for Space bar to be pressed
/////////////////////////////////////////
WaitForSpace:
//        sei
            lda     #%11111111
            sta     $dc02
            lda     #%00000000
            sta     $dc03

            lda     #$7f    //%01111111 - only row 7 KB matrix
            sta     $dc00
WFS_Loop1:
            lda     $dc01
            and     #$10    //mask %00010000
            bne     WFS_Loop1
WFS_Loop2:
            lda     $dc01
            and     #$10    //mask %00010000
            beq     WFS_Loop2

//          cli
            rts
//////////////////////////////////////////////
// Enable Random Number From Voice 3        //
//////////////////////////////////////////////
EnableRandomVoice3:      
            lda #$FF                    // maximum frequency value
            sta $D40E                   // voice 3 frequency low byte
            sta $D40F                   // voice 3 frequency high byte
            lda #$80                    // noise waveform, gate bit off
            sta $D412                   // voice 3 control register
            rts

//////////////////////////////////////////////
// LDA Random Number From Voice 3        //
//////////////////////////////////////////////
GetRandom:
            lda $D41B                   // Load Random Number
            rts
            
//////////////////////////////////////////////
// InsFishRec                               //
//////////////////////////////////////////////
// Insert a new Fish in the tab
// Data on ZPTAB...
//
// ZPTABADL/H contains the address of the fish
// ZPTABVAL   contains the value of the breeding or the energy
InsFishRec:
                                                    // Looking for an empty record to reuse
            lda ZPFECNTL
            bne IFR_ReUseRec
            lda ZPFECNTH
            bne IFR_ReUseRec

            lda #<FishTab
            ldx #>FishTab
            sta ZPTABL
            stx ZPTABH
                                                    
            ldx ZPFCNTL                             // Allocate a new record record
            inx
            stx ZPFCNTL
            bne !IFR_OK+
            inc ZPFCNTH
!IFR_OK:
            stx ZPRCNTL                             // save the new value in pointers for adding rec
            ldx ZPFCNTH
            stx ZPRCNTH

            lda ZPTABADR
            and #~REC_MSK_REUSED                    // not reused
            ora #REC_MSK_OCCUPIED                   // occupied
            sta ZPTABADR                            // Flag the record as not reused
                                                    // but occupied
            jsr InsRec                              // insert the record
            rts

IFR_ReUseRec:
            lda ZPTABADR
            ora #REC_MSK_REUSED                         // reused record (bit 1) occupied (not empty) (bit 0)
            sta ZPTABADR                            // Flag the record as reused
                                                    // but occupied
            ldy #00                                 // load the first empty record address
            lda (ZPFEADL),y
            sta ZPTABL                              // and save to table pointers
            iny
            lda (ZPFEADL),y
            sta ZPTABH

            dec ZPFECNTL                            // decrent the stack counter for fishes
            ldy ZPFECNTL
            cpy #$FF
            bne !IFR_Continue+
            dec ZPFECNTH
!IFR_Continue:                                      // .......................................
                                                    // decrement the stack pointer for the fishese
            lda ZPFEADL
            sec
            sbc #2
            sta ZPFEADL
            lda ZPFEADH
            sbc #0
            sta ZPFEADH
!IFR_Continue:                                      // .......................................
            jsr IR_SaveValues


//////////////////////////////////////////////
// InsRec                                   //
//////////////////////////////////////////////
// Data on ZPTAB...
// ZPTABL/H is the base address of data table
// ZPRCNTL/H contains the number of record allocated where write a new record
//
InsRec:
            ldx ZPRCNTL
            dex
            txa
            asl    
            sta IR_SaveResult1
            rol ZPRCNTH
            lda IR_SaveResult1:#00
            asl        
            sta IR_SaveResult2
            rol ZPRCNTH                 // mulptiply by 4 to reach the right offset
            clc 
            lda IR_SaveResult2:#00
            adc ZPTABL                  // Add offset to the base address of the table
            sta ZPTABL
            lda ZPRCNTH
            adc ZPTABH
            sta ZPTABH

///////////////////////////
// Save the values from 
// ZPTABADL/H ZPTABVAL
// to record pointed by ZPTABL
///////////////////////////
IR_SaveValues:
            lda ZPTABADL                // save the low byte of the address
            ldy #00
            sta (ZPTABL),y
            lda ZPTABADH                // save the high byte of the address
            iny
            sta (ZPTABL),y              
            lda ZPTABVAL                // save the value of breeding or energy
            iny                         
            sta (ZPTABL),y
            lda ZPTABADR                // save the value of flag for reuse 00 or 01
            iny                         
            sta (ZPTABL),y
            rts            

//////////////////////////////////////////////
// GetNextRecValues                         //
//////////////////////////////////////////////
// Read the next rec Values of an NON Empty record
// Input: ZPTABL/H is the offset where read the record
// output: ZPTABADL/H is the address on the rec
//          ZPTABVAL Breeding or energy         
GetNextRecValues:
            ldy #03
            lda (ZPTABL),y              // read the flag byte
            sta ZPTABADR
            and #REC_MSK_OCCUPIED
            beq GRV_Exit                // if bit 1 is empty then is an empty record (go to next)
            //beq GRV_Next                // if bit 1 is empty then is an empty record (go to next)
            ldy #00                 
            lda (ZPTABL),y              // read the low byte of the address
            sta ZPTABADL
            ldy #01
            lda (ZPTABL),y              // read the high byte of the address
            sta ZPTABADH
            ldy #02
            lda (ZPTABL),y              // read the value of breeding or energy
            sta ZPTABVAL  

/*
            rts              
GRV_Next:            
            lda ZPTABL                  // point to the next record (4 bytes beyond)
            clc
            adc #4
            sta ZPTABL
            bcc GRV_Exit
            inc ZPTABH
*/
GRV_Exit:
            rts

//////////////////////////////////////////////
// DelFishRec                               //
//////////////////////////////////////////////
// ZPTABL/H are the offset ot the record to delete
//////////////////////////////////////////////
DelRec:
            // 1) increment stack pointer for free Fishese
            // 2) increment stack counter for free Fishese
            // 3) "free" the record on the ZPTABL/H

            inc ZPFEADL                     // Stack pointer++
            bne !DR_Continue+
            inc ZPFEADH
!DR_Continue:
            inc ZPFECNTL                    // Stack Cuonter++
            bne !DR_Continue+
            inc ZPFECNTH
!DR_Continue:
            ldy #00                         // Save the Record pointer
            lda ZPTABL
            sta (ZPFEADL),y
            iny
            lda ZPTABH
            sta (ZPFEADL),y

            ldy #03             
            lda #~REC_MSK_OCCUPIED          // "Free" the record on records table
            sta (ZPTABL),y
            rts


//////////////////////////////////
// InitWaTor
//////////////////////////////////
InitWaTor:
            lda #00
            sta ZPFECNTL                            // initilaize to 0 the number of eprtyies records for the fishes
            sta ZPFECNTH
            sta ZPSECNTL                            // initilaize to 0 the number of eprtyies records for the sharks
            sta ZPSECNTH

            sta ZPFCNTL                             // Inizialize to 0 the number of fish
            sta ZPFCNTH

            lda #<EmptyFRec-1
            ldx #>EmptyFRec-1
            sta ZPFEADL
            stx ZPFEADH

            lda #<NUMSHARK
            ldx #>NUMSHARK

            sta ZPSCNTL
            stx ZPSCNTH

            lda #<EmptySRec-1
            ldx #>EmptySRec-1
            sta ZPSEADL
            stx ZPSEADH



            jsr BankingOutKB
            jsr EnableRandomVoice3
            //jsr WaitForSpace

            lda #<FreeCellOffset
            ldx #>FreeCellOffset
            sta ZPBUFL
            stx ZPBUFH

            lda #FREECELL
            jsr FillScr

            jsr FillBorders


            rts
//////////////////////
// FillBorders
//////////////////////
FillBorders:
            ldx #39
            lda #BORDER_CHAR

FB_H:
            sta $0400,x
            sta $0400+(40*24),x
            dex
            bpl FB_H

            tay
            rts

NeighbourY:
    // Offset calculated for the neighbour
    // starting from the "first" neighbour, the upper one
    //  U
    // L R
    //  D
    .byte 80, 41, 39, 0

NeighbourL:
    .byte $D8,$01,$28,$FF
NeighbourH:
    .byte $FF,$00,$00,$FF

/*
    Fishes and Sharks:
        address         2 bytes // Address of a void records on fishes/sharks table
*/




/*
    Fish
        address         2 bytes // FF on high byte if void
        breeding time   1 byte
        empty           1 byte

    shark:
        address         2 bytes // FF on high byte if void
        energy          1 byte
        empty           1 byte

*/

//////////////////////////////////////////////
// FishClearReusedFlag                      //
//////////////////////////////////////////////
FishClearReusedFlag:
            lda #<FishTab
            sta ZPTABL
            lda #>FishTab
            sta ZPTABH

            lda ZPFCNTL                         // Place number of fishes on a generic counter
            sta ZPCNTL
            lda ZPFCNTH
            sta ZPCNTH
            jsr ClearReusedFlag

//////////////////////////////////////////////
// SharkClearReusedFlag                      //
//////////////////////////////////////////////


//////////////////////////////////////////////
// ClearReusedFlag                          //
//////////////////////////////////////////////
ClearReusedFlag:
CRF_Loop:
            ldy #03
            lda (ZPTABL),y                          // read the flag byte
            tax                                     // make a copy on x
            and #REC_MSK_OCCUPIED
            beq CRF_Next                            // if bit 1 is empty then is an empty record (go to next)
            txa                                     // restore flag value
            and #~REC_MSK_REUSED                    // clear reused record (bit 1) 
            //sta (ZPTABL),y                          // Flag the record as NOT reused
CRF_Next:            
            lda ZPTABL                              // point to the next record (4 bytes beyond)
            clc
            adc #4
            sta ZPTABL
            bcc CRF_NoIncH
            inc ZPTABH
CRF_NoIncH:       
            dec ZPCNTL                          // Decrement low byte by one
            bne CRF_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne CRF_Loop                         // if the hi byte is not zero then continue ...
            rts
CRF_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            bne CRF_Loop                         // .. isn't FF then continue with next
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp CRF_Loop                         // and continue with the next loop
//////////////////////////////////////////////
// InsSharkRec                               //
//////////////////////////////////////////////
// Insert a new Shark in the tab
// Data on ZPTAB...
//
// ZPTABADL/H contains the address of the fish
// ZPTABVAL   contains the value of the breeding or the energy
InsSharkRec:
                                                    // Looking for an empty record to reuse
            lda ZPSECNTL
            bne ISR_ReUseRec
            lda ZPSECNTH
            bne ISR_ReUseRec

            lda #<SharkTab
            ldx #>SharkTab
            sta ZPTABL
            stx ZPTABH
                                                    
            ldx ZPSCNTL                             // Allocate a new record record
            inx
            stx ZPSCNTL
            bne !ISR_OK+
            inc ZPSCNTH
!ISR_OK:
            stx ZPRCNTL                             // save the new value in pointers for adding rec
            ldx ZPSCNTH
            stx ZPRCNTH

            lda ZPTABADR
            and #~REC_MSK_REUSED                    // not reused
            ora #REC_MSK_OCCUPIED                   // occupied
            sta ZPTABADR                            // Flag the record as not reused
                                                    // but occupied
            jsr InsRec                              // insert the record
            rts

ISR_ReUseRec:
            lda ZPTABADR
            ora #REC_MSK_REUSED                         // reused record (bit 1) occupied (not empty) (bit 0)
            sta ZPTABADR                            // Flag the record as not reused
                                                    // but occupied
            ldy #00                                 // load the first empty record address
            lda (ZPSEADL),y
            sta ZPTABL                              // and save to table pointers
            iny
            lda (ZPSEADL),y
            sta ZPTABH

            dec ZPSECNTL                            // decrent the stack counter for sharks
            ldy ZPSECNTL
            cpy #$FF
            bne !ISR_Continue+
            dec ZPSECNTH
!ISR_Continue:                                      // .......................................
                                                    // decrement the stack pointer for the sharks
            lda ZPSEADL
            sec
            sbc #2
            sta ZPSEADL
            lda ZPSEADH
            sbc #0
            sta ZPSEADH
!ISR_Continue:                                      // .......................................
            jsr IR_SaveValues


//////////////////////////////////
// SharkMoves
//////////////////////////////////
SharkMoves:
            lda #<SharkTab
            sta ZPTABL
            lda #>SharkTab
            sta ZPTABH

            lda ZPSCNTL                         // Place number of sharks on a generic counter
            sta ZPCNTL
            lda ZPSCNTH
            sta ZPCNTH
SM_Loop:
            jsr GetNextRecValues                // Load next shark address on ZPTADADL/H
            lda ZPTABADR                        // see the reused flag
            and #REC_MSK_REUSED                 // is a reused record ?
            bne SM_Next                         // if is a reused flag (inside thsi loop, ignore the fish)
            lda ZPTABADR                        // see the occupied flag
            and #REC_MSK_OCCUPIED               // is a free record ?
            beq SM_Next                         // if is a free record go to next
            lda ZPTABADL                        // Get the fish address
            sta ZPFB                            // and save it on ZPFB/ZPFC
            lda ZPTABADH
            sta ZPFC
            jsr FreeCellFish                    // FD/FE will be the next fish position ... if there was a move
            beq SM_Next                         // if there is no Moves then skip
SM_CkeckBreed:                  
                                                // Check breeding time
                                                // if breeding time is beyond BREEDTIMEMAX
                                                // do not delete old position
            inc ZPTABVAL                        // Increment the breeding index
            lda ZPTABVAL                        // check if 
            cmp #BREEDTIMEMAX                   // max breeding is reached
            bcc SM_DeleteAndMoveFish            // if max breeding is NOT reached then move only current fish ...
                                                // ... else create a copy of the current fish to another record
                                                // with half breeding to new fish and half to the current one 
                                                // and move the current fish
            lsr ZPTABVAL                        // Breeding = Breeding/2    
            jsr CopyCurrentFishRec              // duplicate the fish in current position
            jmp SM_MoveCurrentFish              // Move the fish to the next position
SM_DeleteAndMoveFish:
            ldy #00                             //
            lda #FREECELL                       // Free the currentfish cell
            sta (ZPFB),y                        //
SM_MoveCurrentFish:
            lda ZPFD                            // Move The current fish
            sta ZPTABADL                        // to new "random" location
            lda ZPFE
            sta ZPTABADH                        // place the new coordinates on transition buffer
            jsr IR_SaveValues                   // save values on current record
            ldy #00
            lda #FISH
            sta (ZPFD),y
SM_Next:
            lda ZPTABL
            clc
            adc #4
            sta ZPTABL
            bcc !SM_OK+
            inc ZPTABH
!SM_OK:            
            dec ZPCNTL                          // Decrement low byte by one
            bne SM_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne SM_Loop                         // if the hi byte is not zero then continue ...
            rts                                 // else if is it zero too then return to the caller
SM_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            bne SM_Loop                         // .. isn't FF then continue with next
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp SM_Loop                         // and continue with the next loop
            rts


/*
    Video Address Table Manager
    Record :
        2 Bytes Video Address L/H
        1 Byte Value
        2 Bytes Next Record pointer L/H 
            Last rec has record Pointer H = $00

*/            
///////////////////////////////
// RecordInsert
// Insert a new record
// 1) check for empty records in the free records stack
// 2) if there aren't free records on stack the record will be append
// ZPTABL/H Current record
// ZPTABLASTL/H Firts free record on TAB
// ZPTABFREEL/H Pointer to the stack of deleted record
///////////////////////////////
RecordInsert:
            lda ZPFREECNTL                      // Check for deleted record
            bne RI_InsertFreeRecord
            lda ZPFREECNTH
            bne RI_InsertFreeRecord
RI_InsertAtTheEnd:                              // Append the record at the End of teh table
            lda ZPTABLASTL
            clc                                 // Save the next record pointer
            adc #5
            sta ZPTABNXL
            lda ZPTABLASTH
            adc #0
            sta ZPTABNXH                        //-----------------------------

            ldy #00
            lda ZPTABADL
            sta (ZPTABLASTL),y                  // Save low byte of screen address
            iny
            lda ZPTABADH
            sta (ZPTABLASTL),y                  // Save high byte of screen address
            iny
            lda ZPTABVAL
            sta (ZPTABLASTL),y                  // Save Record value (Breeding or energy)
            iny
            lda ZPTABNXL
            sta (ZPTABLASTL),y                  // Save next record pointer low byte
            iny
            lda ZPTABNXH
            sta (ZPTABLASTL),y                  // Save next record pointer hi byte
            lda #00
            sta (ZPTABNXL),y                    // save $00 to the last record
            lda ZPTABNXL

            sta ZPTABLASTL                      // Save the next pointer to the first free record
            lda ZPTABNXH
            sta ZPTABLASTH
            
            rts

RI_InsertFreeRecord:
            // Append a new record picking it from the deleted records stack 
      

            // +----------+         +----------+      +----------+
            // |          |  ---->  |   |      | ---> |   |      |--->
            // +----------*         +----------*      +----------*
            //                         ^
            //                             
            //


            // get an address from free record stack
            // ZPTABL/H point to record "before" the new one

            ldy #03                             // Get the next address of current record ...
            lda (ZPTABL),y                      // And place it on ZPFB/FC
            sta ZPFB
            iny
            lda (ZPTABL),y
            sta ZPFC

            ldy #00                             // get the address of record to reuse
            lda (ZPTABFREEL),y                  // and place it in ZPFD/FE
            sta ZPFD
            iny 
            lda (ZPTABFREEL),y
            sta ZPFE

            ldy #03                             // Save into record to reuse the values
            lda ZPFB                            // of the next record from the current record (ZPFB/C)
            sta (ZPFD),y
            iny
            lda ZPFC
            sta (ZPFD),y
                                                // Save the next record pointer
            ldy #00
            lda (ZPTABFREEL),y                  // low byte free record to reuse
            sta ZPTABNXL
            iny
            lda (ZPTABFREEL),y                  // high byte free record to reuse
            sta ZPTABNXH

            ldy #04                             // Save High and low byte of next record (the reused one)
            sta (ZPTABL),y
            dey
            lda ZPTABNXL
            sta (ZPTABL),y

            ldy #00
            lda ZPTABADL
            sta (ZPFB),y                  // Save low byte of screen address to new record
            iny
            lda ZPTABADH
            sta (ZPFB),y                  // Save high byte of screen address
            iny
            lda ZPTABVAL
            sta (ZPFB),y                  // Save Record value (Breeding or energy)

            dec ZPTABFREEL                      // decrement the "stack free record pointer"
            bne RI_IsertFreeExit
            dec ZPTABFREEH
RI_IsertFreeExit:
                                            // ZPTABL/H will be the next record
            ldy #03                         // Get the next record pointer that will
                                            // became the current record pointer
            lda (ZPTABL),y
            sta ZPTABNXL
            iny
            lda (ZPTABL),y
            //sta ZPTABNXH
            sta ZPTABH
            lda ZPTABNXL
            sta ZPTABL

            dec ZPFREECNTL
            cmp #$FF
            bne RI_NoDecH
            dec ZPFREECNTH
RI_NoDecH:
            rts
///////////////////////////////
// RecordDelete
// ZPTABL/H Record to delete
// ZPPREVL/H record pointer to previous $0000 if do not exist a previous rec
// 1) If exists a previous record
///////////////////////////////
RecordDelete:
            lda ZPPREVL                 // check for previous rec
            sta ZPFB
            lda ZPPREVH
            sta ZPFC
            bne RD_PreviousIsValid      // if exists a previous record then ok on ZPFB/FC
                                        // we are on the first logical record
            lda ZPTABINIL              // so Previous = Current = first logical record
            sta ZPFB
            lda ZPTABINIH
            sta ZPFC

            ldy #03                     // get the address of the next record and save it to the 
            lda (ZPTABL),y              // pointer of the first logical record
            sta ZPTABINIL
            iny
            lda (ZPTABL),y                
            sta ZPTABINIH               


RD_PreviousIsValid:
                                        // Save the address of current record on deleted stack
            lda ZPTABL
            ldy #00
            sta (ZPTABFREEL),y
            lda ZPTABH
            iny
            sta (ZPTABFREEL),y          

            inc ZPFREECNTL              // Incrementing the stack counter for deleted record
            bne !RD_NoIncH+
            inc ZPFREECNTH
!RD_NoIncH:
            ldy #03                     // get the address of the next record and save it to the previous
            lda (ZPTABL),y              // like next record on the previous rec
            sta (ZPFB),y                
            iny
            lda (ZPTABL),y                
            sta (ZPFB),y                

            ldy #03                     // get the the pointer of the next record (of previous)
            lda (ZPFB),y                // to current rector vector
            sta ZPTABL
            iny 
            lda (ZPFB),y
            sta ZPTABH

            rts


.align $100
FishTab:
    .fill 4000, $00
.align $100
EmptyFRec:                              // Stack for fishes Emptyies recs (Words)
    .fill 2000, $00
SharkTab:
    .fill 4000, $00
EmptySRec:
    .fill 2000, $00                     // stack For Sharks Emptyies recs (Words)

