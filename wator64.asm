//#define DEBUG

/*
    Emanuele Bonin (2022)
    Retro Programmers Inside facebook Group
    


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
no-Border left right 

Cell positions:
     U
    LCR
     D

(C)urrent cell
(U)pper neighbour
(L)eft neighbour
(R)ight neighbour
(D)own neighbour


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

.const ZPDELL=$39               // vector used during record deletion
.const ZPDELH=$3A

.const ZPBUFFL=$40               // buffer pointer for offset with fishes around
.const ZPBUFFH=$41

.const ZPCNTBYTE1 = $42              // Byte counter (1) for temporary purpose

.const ZPTEMPWORDL = $43               // temporary Word Low Byte
.const ZPTEMPWORDH = $44               // temporary Word High Byte

.const ZPTEMPWORD1L = $45               // temporary Word 1 Low Byte
.const ZPTEMPWORD1H = $46               // temporary Word 1 High Byte

.const ZPEATING     = $47               // flag use when shark must move


.const ZPFB = $FB
.const ZPFC = $FC
.const ZPFD = $FD
.const ZPFE = $FE



.const OP_BPL = $10             // Opcode for dynamic programming
.const OP_BNE = $D0


.const FREECELL = ' '+128       // Free cell's char
.const FISH     = $7E //'.'+128           // Fish's char
.const SHARK    = $7F //'>'+128           // Shark's char
.const BORDER_CHAR = '-'        // border char

#if DEBUG          
    // For Debug                      
    .const NUMFISH  = 2             // number of the fishes population at start
    .const NUMSHARK = 4             // number of the sharks population at start
#else 
    .const NUMFISH  = 200            // number of the fishes population at start
    .const NUMSHARK = 15 //6            // number of the sharks population at start
#endif 

// Fish
.const BREEDTIMEMAX = 150           // If reached this breeding time the fish duplicates himself

// Shark
.const ENERGYGAINED = 4             // Energy gained eating a fish
.const ENERGYMAX    = 120           // If reached this amount of energy the shark duplicates himself



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
        address         2 bytes 
        breeding time   1 byte
        next record     2 bytes

    shark:
        address         2 bytes
        energy          1 byte
        Next record     2 bytes

    add fish add a fish first looking in the empties slot
    del fish del a fish and add it to the empties slot
    same for shark


*/

Entry:
            jsr CharMapTo3000
            jsr WaitForSpace
Start:
            jsr InitWaTor
            jsr PopulateFishes   
            jsr PopulateSharks
            
            lda #$04
            sta DisplayFrom+1

!Entry_Loop:
            jsr FishMoves
            jsr SharkMoves
            lda ZPSCNTL
            sta value
            lda ZPSCNTH
            sta value+1
            lda #$07
            sta DisplayFrom
            jsr DisplayNumber

            lda ZPFCNTL
            sta value
            lda ZPFCNTH
            sta value+1
            lda #$16
            sta DisplayFrom
            jsr DisplayNumber


            lda ZPSCNTL
            bne !Continue+
            lda ZPSCNTH
            beq Fishes_Wins
!Continue:
            lda ZPFCNTL
            bne !Continue+
            lda ZPFCNTH
            beq Sharks_Wins
!Continue:
            lda #250
!WVB:
            cmp $D012
            bcs !WVB-

            jmp !Entry_Loop-

Fishes_Wins:
            lda #<Str_Fishes
            sta StrWin
            lda #>Str_Fishes
            sta StrWin+1
            jsr DisplayWinner
            jsr WaitForSpace
            jmp Start
Sharks_Wins:
            lda #<Str_Sharks
            sta StrWin
            lda #>Str_Sharks
            sta StrWin+1
            jsr DisplayWinner
            jsr WaitForSpace
            jmp Start

Str_Sharks:
        .text "the sharks have won!"
        .byte 0
Str_Fishes:
        .text "the fish have won!"
        .byte 0


Str_ScoreFish:
        .text "fishes:"
        .byte 0
Str_ScoreShark:
        .text "sharks:"
        .byte 0

//////////////////////////////////
// Display the winner
//////////////////////////////////
PrintStr:
            ldx #00
PS_LoopStr:
            lda StrSrc:$FFFF,x            
            beq PS_Exit
            sta StrPos:$FFFF,x
            inx
            jmp PS_LoopStr
PS_Exit:                      
            rts



//////////////////////////////////
// Display the winner
//////////////////////////////////
DisplayWinner:
            ldx #21
            lda #32
DW_Loop:
            sta $0549,x
            sta $0571,x
            sta $0599,x
            dex
            bpl DW_Loop

            ldx #00
DW_LoopStr:
            lda StrWin:$FFFF,x            
            beq DW_Exit
            sta $0572,x
            inx
            jmp DW_LoopStr
DW_Exit:                      
            rts


///////////////////////
// DisplayNumber
///////////////////////
DisplayNumber:
        jsr hex2dec

        lda DisplayFrom
        sta DisplayFrom2
        lda DisplayFrom+1
        sta DisplayFrom2+1

        lda #32
        ldx #05
DN_Loop:
        sta DisplayFrom:$FFFF,x
        dex
        bpl DN_Loop       

        ldx #9
l1:     lda result,x
        bne Print
        dex             // skip leading zeros
        bne l1
Print:
        ldy #00
l2:     lda result,x
        ora #$30
        sta DisplayFrom2:$FFFF,y
        iny 
        dex
        bpl l2
        rts

        // converts 10 digits (32 bit values have max. 10 decimal digits)
hex2dec:
        ldx #0
l3:     jsr div10
        sta result,x
        inx
        cpx #10
        bne l3
        rts

        // divides a 32 bit value by 10
        // remainder is returned in akku
div10:
        ldy #32         // 32 bits
        lda #0
        clc
l4:     
        rol
        cmp #10
        bcc skip
        sbc #10
skip:   
        rol value
        rol value+1
        rol value+2
        rol value+3
        dey
        bpl l4
        rts

value:
        //    L H L H    
        .byte 1,1,0,0

result:
        .byte 0,0,0,0,0,0,0,0,0,0

//////////////////////////////////
// FishMoves
//////////////////////////////////
FishMoves:

            lda ZPFEADL                             // Initialize deleted record stack free pointer                          
            sta ZPTABFREEL
            lda ZPFEADH
            sta ZPTABFREEH


            lda ZPFECNTL                            // Initialize deleted record stack pointer
            sta ZPFREECNTL
            lda ZPFECNTH
            sta ZPFREECNTH


            lda ZPTABINIFL                      // Place the first record pointer to the generic pointer
            sta ZPTABL
            sta ZPTABINIL
            lda ZPTABINIFH
            sta ZPTABH
            sta ZPTABINIH

                                                // inizitialize Previous record pointer too
            lda #00
            sta ZPPREVL
            sta ZPPREVH
            lda #02
            sta ZPCNTBYTE                       // intialize temporary counter to check if exists some sharks


            ldx ZPFCNTL                         // Place number of fishes on a generic counter
            stx ZPCNTL
            bne !FM_InitHigh+
            dec ZPCNTBYTE                       // mark on zpcntbyte that low byte of counter is 0
!FM_InitHigh:
            ldx ZPFCNTH
            stx ZPCNTH
            bne FM_Loop
            dec ZPCNTBYTE                       // mark on zpcntbyte that high byte of counter is 0
            bne FM_Loop                         // if zpcntbyte is not zero then is ok
            rts                                 // else exit from routine
FM_Loop:
            jsr GetNextRecValues                // Load next fish address on ZPTADADL/H
            lda ZPTABADL                        // Get the fish address
            sta ZPFB                            // and save it on ZPFB/ZPFC
            lda ZPTABADH
            bne !ok+                            // fix parachiappe fore ...
            rts
!ok: 
            sta ZPFC

                                                // Check if fish is alive
                                                // if there is a shark in it's position then is was
                                                // eated by a shark and it must be delted
            
            ldy #00
            lda (ZPFB),y
            cmp #SHARK                          // is a shark ?
            bne FM_itsAllOK                     // if the fish is not eated then continue
                                                // ... else delete the fish record

            jsr RecordDelete 

            lda ZPTABINIL                       //  rewrite initial logical pointers
            sta ZPTABINIFL                      // they could be changed after deletion
            lda ZPTABINIH
            sta ZPTABINIFH

            dec ZPFCNTL
            lda ZPFCNTL
            cmp #$FF
            bne FM_DecCounter
            dec ZPFCNTH
            jmp FM_DecCounter                   // ZPTABL/H contains yet the next record
            
FM_itsAllOK:
            jsr FreeCellFish                    // FD/FE will be the next fish position ... if there was a move
            beq FM_Next                         // if there is no Moves then skip
FM_CkeckBreed:  
            //jmp FM_DeleteAndMoveFish            // debug no reproduce                
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
            lda #00                             // set the breeding time to 0
            sta ZPTABVAL                        
            jsr INF_InsertIntialized            // duplicate the fish in current position
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
            lda ZPTABL                          // Current record is now the previous
            sta ZPPREVL
            lda ZPTABH
            sta ZPPREVH
            
            ldy #03
            lda (ZPTABL),y                      // Load next record address
            sta ZPTABNXL
            iny
            lda (ZPTABL),y                      // Load next record address
            sta ZPTABNXH
            
            lda ZPTABNXL
            sta ZPTABL
            lda ZPTABNXH
            sta ZPTABH
FM_DecCounter:
            dec ZPCNTL                          // Decrement low byte by one
            bne FM_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            bne FM_Loop                         // if the hi byte is not zero then continue ...
                                                // else if is it zero too then return to the caller

                                                // Update the deleted stack values
            lda ZPTABFREEL
            sta ZPFEADL                             
            lda ZPTABFREEH
            sta ZPFEADH


            lda ZPFREECNTL
            sta ZPFECNTL
            lda ZPFREECNTH
            sta ZPFECNTH

            rts                                 
FM_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            beq !FM_DecH+
            jmp FM_Loop                         // .. isn't FF then continue with next
!FM_DecH:
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp FM_Loop                         // and continue with the next loop

//////////////////////////////////
// Insert a newFish
//////////////////////////////////
InsertANewFish:
            lda ZPFEADL                             
            sta ZPTABFREEL
            lda ZPFEADH
            sta ZPTABFREEH


            lda ZPFECNTL                            // Initialize deleted record stack pointer
            sta ZPFREECNTL
            lda ZPFECNTH
            sta ZPFREECNTH
INF_InsertIntialized:
            lda ZPTABLASTFL                         // Init generic pointers with specific ones
            sta ZPTABLASTL
            lda ZPTABLASTFH
            sta ZPTABLASTH

            jsr RecordInsert

            inc ZPFCNTL                             // increment the fish counter
            bne !INF_NoIncH+
            inc ZPFCNTH
!INF_NoIncH:
            lda ZPTABLASTL                          // restore the fishes values
            sta ZPTABLASTFL
            lda ZPTABLASTH
            sta ZPTABLASTFH

            lda ZPFREECNTL                          // restore value of deleted record stack pointer
            sta ZPFECNTL
            lda ZPFREECNTH
            sta ZPFECNTH

            lda ZPTABFREEL
            sta ZPFEADL
            lda ZPTABFREEH
            sta ZPFEADH
            rts

//////////////////////////////////
// Insert a newShark
//////////////////////////////////
InsertANewShark:

            lda ZPSEADL
            sta ZPTABFREEL
            lda ZPSEADH
            sta ZPTABFREEH

            lda ZPSECNTL
            sta ZPFREECNTL
            lda ZPSECNTH
            sta ZPFREECNTH

INS_InsertIntialized:
            lda ZPTABLASTSL                         // Init generic pointers with specific ones
            sta ZPTABLASTL
            lda ZPTABLASTSH
            sta ZPTABLASTH

            jsr RecordInsert            
            inc ZPSCNTL                             // increment the sharks counter
            bne !INS_NoIncH+
            inc ZPSCNTH
!INS_NoIncH:
            lda ZPTABLASTL                          // restore the Sharks values
            sta ZPTABLASTSL
            lda ZPTABLASTH
            sta ZPTABLASTSH

            lda ZPTABFREEL
            sta ZPSEADL
            lda ZPTABFREEH
            sta ZPSEADH

            lda ZPFREECNTL
            sta ZPSECNTL
            lda ZPFREECNTH
            sta ZPSECNTH
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

            lda ZPCNTBYTE
            cmp #1
            beq FCF_OnlyOne
FCF_RndLoop:
            lda RNDVOICE3                       // random number from 0 to 255 -> a
            and #%00001100                      // max value for y is 3 pick up two bits
            lsr                                 // and shift it to the righ
            lsr                                 // to reach a value in the range 0-3
            cmp ZPCNTBYTE                       // compare with max value
            bcs FCF_RndLoop                     // if too much big then retry with another random number
            //bcc FCF_LoadFromBuffer              // if lower use the index
                                                // else use the max index
            //lsr
            //lda ZPCNTBYTE
            //sec
            //sbc #01
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

//////////////////////////////////
// Free for Shark
// looking around ZPFB/ZPFC
// 1) Looking for Food
// 2) Looking for Move
// output:
// ZPFD/ZPFE (new place)
// A free cell counter
//////////////////////////////////

FreeCellShark:
            ldy ZPBUFFL             // Initialize pointers to looking for Food
            sty ZPTEMPWORDL
            ldy ZPBUFFH
            sty ZPTEMPWORDH
            ldy #00
            sty ZPCNTBYTE           // ZPCNTByte will contains the number of free cells
            sty ZPCNTBYTE1           // ZPCNTByte1 will contains the number of food cell (Fish)
            lda ZPFB                // Copy the actual cell address to  
            ldx ZPFC                
            sta ZPFD                // vector ZPFD/ZPFE
            stx ZPFE
                                    // Position of (U)pper cell neighbour
            sec
            sbc #40
            sta ZPFD
            bcs FCS_NoDecHi
            dec ZPFE
            ldy #00

FCS_NoDecHi:       
            ldx #03                             // number of ZPFB/FC neighbour
FCS_Loop:
            ldy NeighbourY,x
            lda (ZPFD),y
            cmp #FREECELL
            bne FCS_Check4Fish
            txa                                 // Free Cell index of offsety
            ldy ZPCNTBYTE                       // load in y the next free index of buffer
            sta (ZPBUFL),y                      // save the freeoffset
            inc ZPCNTBYTE                       // Increments number of free cells
            jmp FCS_Next
FCS_Check4Fish:
            cmp #FISH
            bne FCS_Next                        // if is not a Fish (and is not a freecell) then is a shark or a border!
            txa                                 // else is a Cell with food (Fish)
            ldy ZPCNTBYTE1                      // load in y the the next free index for fishes buffer
            sta (ZPBUFFL),y                      // save the fishoffset
            inc ZPCNTBYTE1                      // increments number of fish eatable
FCS_Next: 
            dex
            bpl FCS_Loop

            lda ZPCNTBYTE1                      // ZPCNTBYTE1 contain the number of neighbour cell with a fish (food)  
            bne FCS_FeedShark                   // if there is some fish then eat it

            lda ZPCNTBYTE                       // else .. ZPCNTBYTE contain the number of free cell
                                                // simply move, but ...
            beq FCS_Exit                        // ... if there are no free neighbour then exit
            lda ZPBUFL
            sta ZPTEMPWORDL                                    
            lda ZPBUFH
            sta ZPTEMPWORDH
            jmp FCS_RndLoop
FCS_FeedShark:
            inc ZPEATING                        // Shark is eating so i mark this on the appropriate flag
            sta ZPCNTBYTE                       // A = ZPCNTBYTE1 store it to ZPCNTBYTE

FCS_RndLoop:
            lda ZPCNTBYTE
            cmp #1
            beq FCS_OnlyOne
            lda RNDVOICE3                       // random number from 0 to 255 -> a
            and #%00000011                      // max value for y is 3 pick up two bits
            //lsr                                 // and shift it to the righ
            //lsr                                 // to reach a value in the range 0-3
            cmp ZPCNTBYTE                       // compare with max value
            bcs FCS_RndLoop                     // if too much big then retry with another random number
            jmp FCS_LoadFromBuffer            
FCS_OnlyOne:
            lda #0            
FCS_LoadFromBuffer:            
            tay
            lda (ZPTEMPWORDL),y                 // Load offset on Acc
            tax
            lda NeighbourY,x            
            clc                                 // clear carry to add offset to base address ...
            adc ZPFD                            // ... on ZPFD/FE
            sta ZPFD
            bcc FCS_Exit                        // if carry is clear no more op
            inc ZPFE                            // else inc hi byte!
FCS_Exit:
            lda ZPCNTBYTE                       // number of Foodcell or free cell (if food cell  = 0)
            rts



            // Buffer
FreeCellOffset:
            .byte $00,$00,$00,$00 
FishCellOffset:
            .byte $00,$00,$00,$00


//////////////////////////////////
// Populate Fishes              //
//////////////////////////////////
PopulateFishes:
            lda #<NUMFISH
            sta ZPCNTL
            lda #>NUMFISH
            sta ZPCNTH
PF_Populate:
            
            #if DEBUG
                dec DebugCnt
                ldx DebugCnt
                lda DebugX,x
            #else
                lda RNDVOICE3
                cmp #40
                bcs PF_Populate
            #endif 
            
            tax
PF_Pop_Y:
            #if DEBUG
                ldy DebugCnt
                lda DebugY,y
            #else
                lda RNDVOICE3
                //lda #12
                cmp #25
                bcs PF_Pop_Y
            #endif 
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


            jsr InsertANewFish

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



//////////////////////////////////
// Populate Sharks
//////////////////////////////////
PopulateSharks:
            lda #<NUMSHARK
            sta ZPCNTL
            lda #>NUMSHARK
            sta ZPCNTH
PS_Populate:
            #if DEBUG
                dec DebugCnt
                ldx DebugCnt
                lda DebugX,x
            #else
                lda RNDVOICE3
                //lda #19
                cmp #40
                bcs PS_Populate
            #endif 
                
            tax
PS_Pop_Y:
            #if DEBUG
                ldy DebugCnt
                lda DebugY,y
            #else
                lda RNDVOICE3
                //lda #12
                cmp #25
                bcs PS_Pop_Y
            #endif 
            
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
            //lda #50                             // for debug purpose only
            sta ZPTABVAL

                                                // Insert a new Shark in the tab
                                                // Data on ZPTAB...
                                                //
                                                // ZPTABADL/H contains the address of the Shark
                                                // ZPTABVAL   contains the value of the breeding or the energy


            jsr InsertANewShark

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
            rts            

//////////////////////////////////////////////
// GetNextRecValues                         //
//////////////////////////////////////////////
// Read the next rec Values of an NON Empty record
// Input: ZPTABL/H is the offset where read the record
// output: ZPTABADL/H is the address on the rec
//          ZPTABVAL Breeding or energy         
GetNextRecValues:
            ldy #00                 
            lda (ZPTABL),y              // read the low byte of the address
            sta ZPTABADL
            ldy #01
            lda (ZPTABL),y              // read the high byte of the address
            sta ZPTABADH
            ldy #02
            lda (ZPTABL),y              // read the value of breeding or energy
            sta ZPTABVAL  
GRV_Exit:
            rts

//////////////////////////////////
// InitWaTor
//////////////////////////////////
InitWaTor:
            lda #00                                 

            sta ZPFREECNTL                          // reset the deleted record counter
            sta ZPFREECNTH
            sta ZPFECNTL                            // initilaize to 0 the number of eprtyies records for the fishes
            sta ZPFECNTH
            sta ZPSECNTL                            // initilaize to 0 the number of eprtyies records for the sharks
            sta ZPSECNTH

            sta ZPFCNTL                             // Inizialize to 0 the number of fish
            sta ZPFCNTH
            sta ZPSCNTL                             // Inizialize to 0 the number of shark
            sta ZPSCNTH

            lda #<EmptyFRec-2
            ldx #>EmptyFRec-2
            sta ZPFEADL
            stx ZPFEADH

            lda #<FishTab
            sta ZPTABLASTFL                         // Fish tab last pointer initialization
            sta ZPTABINIFL
            lda #>FishTab
            sta ZPTABLASTFH
            sta ZPTABINIFH





            lda #<SharkTab
            sta ZPTABLASTSL                         // Fish tab last pointer initialization
            sta ZPTABINISL
            lda #>SharkTab
            sta ZPTABLASTSH
            sta ZPTABINISH



            lda #<EmptySRec-2
            ldx #>EmptySRec-2
            sta ZPSEADL
            stx ZPSEADH

            jsr BankingOutKB
            jsr EnableRandomVoice3

            lda #<FreeCellOffset
            ldx #>FreeCellOffset
            sta ZPBUFL
            stx ZPBUFH

            lda #<FishCellOffset
            ldx #>FishCellOffset
            sta ZPBUFFL
            stx ZPBUFFH

            lda #FREECELL
            jsr FillScr

            jsr FillBorders

            lda #<Str_ScoreShark
            sta StrSrc
            lda #>Str_ScoreShark
            sta StrSrc+1
            lda #$00
            sta StrPos
            lda #$04
            sta StrPos+1

            jsr PrintStr

            lda #<Str_ScoreFish
            sta StrSrc
            lda #>Str_ScoreFish
            sta StrSrc+1
            lda #$0F
            sta StrPos
            lda #$04
            sta StrPos+1

            jsr PrintStr


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
    //.byte 80, 41, 39, 0
    .byte 80, 39, 0, 41

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



//////////////////////////////////
// SharkMoves
//////////////////////////////////
SharkMoves:

            lda ZPSEADL                             // Initialize deleted record stack free pointer                          
            sta ZPTABFREEL
            lda ZPSEADH
            sta ZPTABFREEH


            lda ZPSECNTL                            // Initialize deleted record stack pointer
            sta ZPFREECNTL
            lda ZPSECNTH
            sta ZPFREECNTH


            lda ZPTABINISL                      // Place the first record pointer to the generic pointer
            sta ZPTABINIL               
            sta ZPTABL
            lda ZPTABINISH
            sta ZPTABH
            sta ZPTABINIH
                                                // inizitialize Previous record pointer too
            lda #00
            sta ZPPREVL
            sta ZPPREVH
            lda #02
            sta ZPCNTBYTE                       // intialize temporary counter to check if exists some sharks


            ldx ZPSCNTL                         // Place number of sharks on a generic counter
            stx ZPCNTL
            bne !SM_InitHigh+
            dec ZPCNTBYTE                       // mark on zpcntbyte that low byte of counter is 0
!SM_InitHigh:
            ldx ZPSCNTH
            stx ZPCNTH
            bne SM_Loop
            dec ZPCNTBYTE                       // mark on zpcntbyte that high byte of counter is 0
            bne SM_Loop                         // if zpcntbyte is not zero then is ok
            rts                                 // else exit from routine
SM_Loop:
            jsr GetNextRecValues                // Load next shark address on ZPTADADL/H
            lda ZPTABADL                        // Get the shark address
            sta ZPFB                            // and save it on ZPFB/ZPFC
            lda ZPTABADH
            sta ZPFC

            lda #00
            sta ZPEATING                        // reset the "eating" flag
            jsr FreeCellShark                   // FD/FE will be the next shark position ... if there was a move                
            beq SM_Next                         // if there is no Moves or eats then skip
            lda ZPEATING                        // Sharks is eating ?
            bne SM_Eating                       // ... if so then go to SM_Eating
SM_CkeckEnergy:                  
                                                // Check breeding time
                                                // if breeding time is beyond BREEDTIMEMAX
                                                // do not delete old position
            dec ZPTABVAL                        // Descremente the energy index
                                                // check if 
                                                // energy is finished
            //jmp SM_MoveCurrentshark                   // debug only (no one sharks dead)                                         
            bne SM_MoveCurrentshark           // if there is energy then move only current shark ...
                                                // ... else the shark is dead

            jsr RecordDelete                    // delete current shark

            lda ZPTABINIL                       //  rewrite initial logical pointers
            sta ZPTABINISL                      // they could be changed after deletion
            lda ZPTABINIH
            sta ZPTABINISH

            dec ZPSCNTL
            lda ZPSCNTL
            cmp #$FF
            bne SM_Next_AfterDeletion
            dec ZPSCNTH
            jmp SM_Next_AfterDeletion           // ZPTABL/H contains yet the next record
SM_Eating:
            lda ZPTABVAL
            clc
            adc #ENERGYGAINED                   // Add the energy gained eating a fish
            sta ZPTABVAL                        // save it on tab val    
            cmp #ENERGYMAX                      // check the new energy with energy needed for duplication
            bcc SM_MoveCurrentshark             // ... if there is not enough energy then move shark only!
                                                // else duplicate itself to new location
            lsr ZPTABVAL                        // energy is splitted
            jsr INS_InsertIntialized
            inc ZPEATING
SM_MoveCurrentshark:
            lda ZPFD                            // Move The current shark
            sta ZPTABADL                        // to new "random" location
            lda ZPFE
            sta ZPTABADH                        // place the new coordinates on transition buffer
            jsr IR_SaveValues                   // save values on current record
            
            ldy #00
            lda #SHARK
            sta (ZPFD),y
SM_Next:
            lda ZPTABL                          // Current record is now the previous
            sta ZPPREVL
            lda ZPTABH
            sta ZPPREVH
            
            ldy #03
            lda (ZPTABL),y                      // Load next record address
            sta ZPTABNXL
            iny
            lda (ZPTABL),y                      // Load next record address
            sta ZPTABNXH
            
            lda ZPTABNXL
            sta ZPTABL
            lda ZPTABNXH
            sta ZPTABH

SM_Next_AfterDeletion:
            lda ZPEATING                        // Checking the eating flag
                                                // when it is 2 then a duplication occurs
                                                // so no position deletion
            cmp #2
            beq SM_NoDeleteShark                                    
                                                // If i come from deletion the ZPTABL/H is yet ok
            ldy #00                             //                                     

            lda #FREECELL                       // Free the currentshark cell
            sta (ZPFB),y                        //
SM_NoDeleteShark:
            dec ZPCNTL                          // Decrement low byte by one
            bne SM_CheckForFF                   // if not zero check if FF
            lda ZPCNTH                          // else if zero check hi byte
            beq !SM_NoLoop+
            jmp SM_Loop                         // if the hi byte is not zero then continue ...
                                                // else if is it zero too then return to the caller
                                                // Update the deleted stack value
!SM_NoLoop:
            lda ZPTABFREEL
            sta ZPSEADL                             
            lda ZPTABFREEH
            sta ZPSEADH


            lda ZPFREECNTL
            sta ZPSECNTL
            lda ZPFREECNTH
            sta ZPSECNTH


            rts
SM_CheckForFF:            
            lda ZPCNTL
            cmp #$FF                            // if low byte ...
            beq !SM_DecH+ 
            jmp SM_Loop                         // .. isn't FF then continue with next
!SM_DecH:
            dec ZPCNTH                          // low byte = $FF -> decrement the hi byte
            jmp SM_Loop                         // and continue with the next loop

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

            // Bug la gestione dei record deleted non funziona bene ....
            // problemi relativi all'errata gestione dei reinserimento di records.
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
            ldy #01                         // Fix:Parachiappe
            lda (ZPTABFREEL),y
            bne !Continue+
            sta ZPFREECNTL
            sta ZPFREECNTH
            jmp RI_InsertAtTheEnd
!Continue:            
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
            sta ZPTEMPWORD1L
            iny 
            lda (ZPTABFREEL),y
            sta ZPTEMPWORD1H

            ldy #03                             // Save into record to reuse the values
            lda ZPFB                            // of the next record from the current record (ZPFB/C)
            sta (ZPTEMPWORD1L),y
            iny
            lda ZPFC
            sta (ZPTEMPWORD1L),y
                                                // Save the next record pointer
            /*
            ldy #00
            lda (ZPTABFREEL),y                  // low byte free record to reuse
            sta ZPTABNXL
            iny
            lda (ZPTABFREEL),y                  // high byte free record to reuse
            sta ZPTABNXH
            */

            ldy #03                             // Save High and low byte of next record (the reused one)
            lda ZPTEMPWORD1L
            sta (ZPTABL),y
            iny
            lda ZPTEMPWORD1H
            sta (ZPTABL),y

            ldy #00
            lda ZPTABADL
            sta (ZPTEMPWORD1L),y                  // Save low byte of screen address to new record
            iny
            lda ZPTABADH
            sta (ZPTEMPWORD1L),y                  // Save high byte of screen address
            iny
            lda ZPTABVAL
            sta (ZPTEMPWORD1L),y                  // Save Record value (Breeding or energy)

            /*
                                            // ZPTABL/H will be the next record
            ldy #03                         // Get the next record pointer that will
                                            // became the current record pointer
            lda (ZPTABL),y
            sta ZPFD
            iny
            lda (ZPTABL),y
            sta ZPFE
            //sta ZPTABNXH
            */
            lda ZPTEMPWORD1L
            sta ZPTABL
            lda ZPTEMPWORD1H
            sta ZPTABH

            dec ZPFREECNTL                      // decrement the record deleted counter
            cmp #$FF
            bne !RI_NoDecH+
            dec ZPFREECNTH
!RI_NoDecH:
            sec                                 // decrement the "stack free record pointer"
            lda ZPTABFREEL
            sbc #02
            sta ZPTABFREEL
            bcs RI_InsertFreeExit
            dec ZPTABFREEH
RI_InsertFreeExit:
            rts
///////////////////////////////
// RecordDelete
// ZPTABL/H Record to delete
// ZPPREVL/H record pointer to previous $0000 if do not exist a previous rec
// 1) If exists a previous record
///////////////////////////////
RecordDelete:
            clc                         // Update deleted record pointer
            lda ZPTABFREEL
            adc #02
            sta ZPTABFREEL
            lda ZPTABFREEH
            adc #00
            sta ZPTABFREEH

            inc ZPFREECNTL              // Incrementing the stack counter for deleted record
            bne !RD_NoIncH+
            inc ZPFREECNTH
!RD_NoIncH:

            // ZPDELL/H is used to store the address of "previous" record
            // that will points to address forward
            lda ZPPREVL                 // check for previous rec
            sta ZPDELL                  
            lda ZPPREVH
            sta ZPDELH
            
            bne RD_PreviousIsValid      // if exists a previous record then ok on ZPFB/FC
                                        // we are on the first logical record
            lda ZPTABINIL              // so Previous = Current = first logical record
            sta ZPDELL
            lda ZPTABINIH
            sta ZPDELH

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

            ldy #03                     // get the address of the next record and save it to the previous
            lda (ZPTABL),y              // like next record on the previous rec
            sta (ZPDELL),y                
            iny
            lda (ZPTABL),y                
            sta (ZPDELL),y                

            ldy #03                     // get the the pointer of the next record (of previous)
            lda (ZPDELL),y                // to current  vector
            sta ZPTABL
            iny 
            lda (ZPDELL),y
            sta ZPTABH

            rts

.const VIC_MEM_PTRS = $D018
///////////////////////////////
// CharMapTo3000
///////////////////////////////
CharMapTo3000:
    lda VIC_MEM_PTRS    // Chars redefinition 
    and #$F0            // from $3000
    ora #$0C            // setting bit 2 and 3
    sta VIC_MEM_PTRS    // of $D018
    rts

    *=$3000
	.byte	$3c,$66,$6e,$6e,$60,$62,$3c,$00 // character 0
	.byte	$18,$3c,$66,$7e,$66,$66,$66,$00 // character 1
	.byte	$7c,$66,$66,$7c,$66,$66,$7c,$00 // character 2
	.byte	$3c,$66,$60,$60,$60,$66,$3c,$00 // character 3
	.byte	$78,$6c,$66,$66,$66,$6c,$78,$00 // character 4
	.byte	$7e,$60,$60,$78,$60,$60,$7e,$00 // character 5
	.byte	$7e,$60,$60,$78,$60,$60,$60,$00 // character 6
	.byte	$3c,$66,$60,$6e,$66,$66,$3c,$00 // character 7
	.byte	$66,$66,$66,$7e,$66,$66,$66,$00 // character 8
	.byte	$3c,$18,$18,$18,$18,$18,$3c,$00 // character 9
	.byte	$1e,$0c,$0c,$0c,$0c,$6c,$38,$00 // character 10
	.byte	$66,$6c,$78,$70,$78,$6c,$66,$00 // character 11
	.byte	$60,$60,$60,$60,$60,$60,$7e,$00 // character 12
	.byte	$63,$77,$7f,$6b,$63,$63,$63,$00 // character 13
	.byte	$66,$76,$7e,$7e,$6e,$66,$66,$00 // character 14
	.byte	$3c,$66,$66,$66,$66,$66,$3c,$00 // character 15
	.byte	$7c,$66,$66,$7c,$60,$60,$60,$00 // character 16
	.byte	$3c,$66,$66,$66,$66,$3c,$0e,$00 // character 17
	.byte	$7c,$66,$66,$7c,$78,$6c,$66,$00 // character 18
	.byte	$3c,$66,$60,$3c,$06,$66,$3c,$00 // character 19
	.byte	$7e,$18,$18,$18,$18,$18,$18,$00 // character 20
	.byte	$66,$66,$66,$66,$66,$66,$3c,$00 // character 21
	.byte	$66,$66,$66,$66,$66,$3c,$18,$00 // character 22
	.byte	$63,$63,$63,$6b,$7f,$77,$63,$00 // character 23
	.byte	$66,$66,$3c,$18,$3c,$66,$66,$00 // character 24
	.byte	$66,$66,$66,$3c,$18,$18,$18,$00 // character 25
	.byte	$7e,$06,$0c,$18,$30,$60,$7e,$00 // character 26
	.byte	$3c,$30,$30,$30,$30,$30,$3c,$00 // character 27
	.byte	$0c,$12,$30,$7c,$30,$62,$fc,$00 // character 28
	.byte	$3c,$0c,$0c,$0c,$0c,$0c,$3c,$00 // character 29
	.byte	$00,$18,$3c,$7e,$18,$18,$18,$18 // character 30
	.byte	$00,$10,$30,$7f,$7f,$30,$10,$00 // character 31
	.byte	$00,$00,$00,$00,$00,$00,$00,$00 // character 32
	.byte	$18,$18,$18,$18,$00,$00,$18,$00 // character 33
	.byte	$66,$66,$66,$00,$00,$00,$00,$00 // character 34
	.byte	$66,$66,$ff,$66,$ff,$66,$66,$00 // character 35
	.byte	$18,$3e,$60,$3c,$06,$7c,$18,$00 // character 36
	.byte	$62,$66,$0c,$18,$30,$66,$46,$00 // character 37
	.byte	$3c,$66,$3c,$38,$67,$66,$3f,$00 // character 38
	.byte	$06,$0c,$18,$00,$00,$00,$00,$00 // character 39
	.byte	$0c,$18,$30,$30,$30,$18,$0c,$00 // character 40
	.byte	$30,$18,$0c,$0c,$0c,$18,$30,$00 // character 41
	.byte	$00,$66,$3c,$ff,$3c,$66,$00,$00 // character 42
	.byte	$00,$18,$18,$7e,$18,$18,$00,$00 // character 43
	.byte	$00,$00,$00,$00,$00,$18,$18,$30 // character 44
	.byte	$00,$00,$00,$7e,$00,$00,$00,$00 // character 45
	.byte	$00,$00,$00,$00,$00,$18,$18,$00 // character 46
	.byte	$00,$03,$06,$0c,$18,$30,$60,$00 // character 47
	.byte	$3c,$66,$6e,$76,$66,$66,$3c,$00 // character 48
	.byte	$18,$18,$38,$18,$18,$18,$7e,$00 // character 49
	.byte	$3c,$66,$06,$0c,$30,$60,$7e,$00 // character 50
	.byte	$3c,$66,$06,$1c,$06,$66,$3c,$00 // character 51
	.byte	$06,$0e,$1e,$66,$7f,$06,$06,$00 // character 52
	.byte	$7e,$60,$7c,$06,$06,$66,$3c,$00 // character 53
	.byte	$3c,$66,$60,$7c,$66,$66,$3c,$00 // character 54
	.byte	$7e,$66,$0c,$18,$18,$18,$18,$00 // character 55
	.byte	$3c,$66,$66,$3c,$66,$66,$3c,$00 // character 56
	.byte	$3c,$66,$66,$3e,$06,$66,$3c,$00 // character 57
	.byte	$00,$00,$18,$00,$00,$18,$00,$00 // character 58
	.byte	$00,$00,$18,$00,$00,$18,$18,$30 // character 59
	.byte	$0e,$18,$30,$60,$30,$18,$0e,$00 // character 60
	.byte	$00,$00,$7e,$00,$7e,$00,$00,$00 // character 61
	.byte	$70,$18,$0c,$06,$0c,$18,$70,$00 // character 62
	.byte	$3c,$66,$06,$0c,$18,$00,$18,$00 // character 63
	.byte	$00,$00,$00,$ff,$ff,$00,$00,$00 // character 64
	.byte	$08,$1c,$3e,$7f,$7f,$1c,$3e,$00 // character 65
	.byte	$18,$18,$18,$18,$18,$18,$18,$18 // character 66
	.byte	$00,$00,$00,$ff,$ff,$00,$00,$00 // character 67
	.byte	$00,$00,$ff,$ff,$00,$00,$00,$00 // character 68
	.byte	$00,$ff,$ff,$00,$00,$00,$00,$00 // character 69
	.byte	$00,$00,$00,$00,$ff,$ff,$00,$00 // character 70
	.byte	$30,$30,$30,$30,$30,$30,$30,$30 // character 71
	.byte	$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c // character 72
	.byte	$00,$00,$00,$e0,$f0,$38,$18,$18 // character 73
	.byte	$18,$18,$1c,$0f,$07,$00,$00,$00 // character 74
	.byte	$18,$18,$38,$f0,$e0,$00,$00,$00 // character 75
	.byte	$c0,$c0,$c0,$c0,$c0,$c0,$ff,$ff // character 76
	.byte	$c0,$e0,$70,$38,$1c,$0e,$07,$03 // character 77
	.byte	$03,$07,$0e,$1c,$38,$70,$e0,$c0 // character 78
	.byte	$ff,$ff,$c0,$c0,$c0,$c0,$c0,$c0 // character 79
	.byte	$ff,$ff,$03,$03,$03,$03,$03,$03 // character 80
	.byte	$00,$3c,$7e,$7e,$7e,$7e,$3c,$00 // character 81
	.byte	$00,$00,$00,$00,$00,$ff,$ff,$00 // character 82
	.byte	$36,$7f,$7f,$7f,$3e,$1c,$08,$00 // character 83
	.byte	$60,$60,$60,$60,$60,$60,$60,$60 // character 84
	.byte	$00,$00,$00,$07,$0f,$1c,$18,$18 // character 85
	.byte	$c3,$e7,$7e,$3c,$3c,$7e,$e7,$c3 // character 86
	.byte	$00,$3c,$7e,$66,$66,$7e,$3c,$00 // character 87
	.byte	$18,$18,$66,$66,$18,$18,$3c,$00 // character 88
	.byte	$06,$06,$06,$06,$06,$06,$06,$06 // character 89
	.byte	$08,$1c,$3e,$7f,$3e,$1c,$08,$00 // character 90
	.byte	$18,$18,$18,$ff,$ff,$18,$18,$18 // character 91
	.byte	$c0,$c0,$30,$30,$c0,$c0,$30,$30 // character 92
	.byte	$18,$18,$18,$18,$18,$18,$18,$18 // character 93
	.byte	$00,$00,$03,$3e,$76,$36,$36,$00 // character 94
	.byte	$ff,$7f,$3f,$1f,$0f,$07,$03,$01 // character 95
	.byte	$00,$00,$00,$00,$00,$00,$00,$00 // character 96
	.byte	$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0 // character 97
	.byte	$00,$00,$00,$00,$ff,$ff,$ff,$ff // character 98
	.byte	$ff,$00,$00,$00,$00,$00,$00,$00 // character 99
	.byte	$00,$00,$00,$00,$00,$00,$00,$ff // character 100
	.byte	$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0 // character 101
	.byte	$cc,$cc,$33,$33,$cc,$cc,$33,$33 // character 102
	.byte	$03,$03,$03,$03,$03,$03,$03,$03 // character 103
	.byte	$00,$00,$00,$00,$cc,$cc,$33,$33 // character 104
	.byte	$ff,$fe,$fc,$f8,$f0,$e0,$c0,$80 // character 105
	.byte	$03,$03,$03,$03,$03,$03,$03,$03 // character 106
	.byte	$18,$18,$18,$1f,$1f,$18,$18,$18 // character 107
	.byte	$00,$00,$00,$00,$0f,$0f,$0f,$0f // character 108
	.byte	$18,$18,$18,$1f,$1f,$00,$00,$00 // character 109
	.byte	$00,$00,$00,$f8,$f8,$18,$18,$18 // character 110
	.byte	$00,$00,$00,$00,$00,$00,$ff,$ff // character 111
	.byte	$00,$00,$00,$1f,$1f,$18,$18,$18 // character 112
	.byte	$18,$18,$18,$ff,$ff,$00,$00,$00 // character 113
	.byte	$00,$00,$00,$ff,$ff,$18,$18,$18 // character 114
	.byte	$18,$18,$18,$f8,$f8,$18,$18,$18 // character 115
	.byte	$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0 // character 116
	.byte	$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0 // character 117
	.byte	$07,$07,$07,$07,$07,$07,$07,$07 // character 118
	.byte	$ff,$ff,$00,$00,$00,$00,$00,$00 // character 119
	.byte	$ff,$ff,$ff,$00,$00,$00,$00,$00 // character 120
	.byte	$00,$00,$00,$00,$00,$ff,$ff,$ff // character 121
	.byte	$03,$03,$03,$03,$03,$03,$ff,$ff // character 122
	.byte	$00,$00,$00,$00,$f0,$f0,$f0,$f0 // character 123
	.byte	$0f,$0f,$0f,$0f,$00,$00,$00,$00 // character 124
	.byte	$18,$18,$18,$f8,$f8,$00,$00,$00 // character 125
	.byte	$ff,$ff,$ff,$a7,$8b,$a7,$ff,$ff
	.byte	$ff,$e7,$ce,$a5,$03,$81,$ce,$f7    
    //.byte	$ff,$ff,$e7,$4b,$01,$43,$ff,$ff // character 126
	//.byte	$ff,$f3,$e7,$82,$20,$82,$ff,$ff // character 127

	.byte	$c3,$99,$91,$91,$9f,$99,$c3,$ff // character 128
	.byte	$e7,$c3,$99,$81,$99,$99,$99,$ff // character 129
	.byte	$83,$99,$99,$83,$99,$99,$83,$ff // character 130
	.byte	$c3,$99,$9f,$9f,$9f,$99,$c3,$ff // character 131
	.byte	$87,$93,$99,$99,$99,$93,$87,$ff // character 132
	.byte	$81,$9f,$9f,$87,$9f,$9f,$81,$ff // character 133
	.byte	$81,$9f,$9f,$87,$9f,$9f,$9f,$ff // character 134
	.byte	$c3,$99,$9f,$91,$99,$99,$c3,$ff // character 135
	.byte	$99,$99,$99,$81,$99,$99,$99,$ff // character 136
	.byte	$c3,$e7,$e7,$e7,$e7,$e7,$c3,$ff // character 137
	.byte	$e1,$f3,$f3,$f3,$f3,$93,$c7,$ff // character 138
	.byte	$99,$93,$87,$8f,$87,$93,$99,$ff // character 139
	.byte	$9f,$9f,$9f,$9f,$9f,$9f,$81,$ff // character 140
	.byte	$9c,$88,$80,$94,$9c,$9c,$9c,$ff // character 141
	.byte	$99,$89,$81,$81,$91,$99,$99,$ff // character 142
	.byte	$c3,$99,$99,$99,$99,$99,$c3,$ff // character 143
	.byte	$83,$99,$99,$83,$9f,$9f,$9f,$ff // character 144
	.byte	$c3,$99,$99,$99,$99,$c3,$f1,$ff // character 145
	.byte	$83,$99,$99,$83,$87,$93,$99,$ff // character 146
	.byte	$c3,$99,$9f,$c3,$f9,$99,$c3,$ff // character 147
	.byte	$81,$e7,$e7,$e7,$e7,$e7,$e7,$ff // character 148
	.byte	$99,$99,$99,$99,$99,$99,$c3,$ff // character 149
	.byte	$99,$99,$99,$99,$99,$c3,$e7,$ff // character 150
	.byte	$9c,$9c,$9c,$94,$80,$88,$9c,$ff // character 151
	.byte	$99,$99,$c3,$e7,$c3,$99,$99,$ff // character 152
	.byte	$99,$99,$99,$c3,$e7,$e7,$e7,$ff // character 153
	.byte	$81,$f9,$f3,$e7,$cf,$9f,$81,$ff // character 154
	.byte	$c3,$cf,$cf,$cf,$cf,$cf,$c3,$ff // character 155
	.byte	$f3,$ed,$cf,$83,$cf,$9d,$03,$ff // character 156
	.byte	$c3,$f3,$f3,$f3,$f3,$f3,$c3,$ff // character 157
	.byte	$ff,$e7,$c3,$81,$e7,$e7,$e7,$e7 // character 158
	.byte	$ff,$ef,$cf,$80,$80,$cf,$ef,$ff // character 159
	.byte	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff // character 160
	.byte	$e7,$e7,$e7,$e7,$ff,$ff,$e7,$ff // character 161
	.byte	$99,$99,$99,$ff,$ff,$ff,$ff,$ff // character 162
	.byte	$99,$99,$00,$99,$00,$99,$99,$ff // character 163
	.byte	$e7,$c1,$9f,$c3,$f9,$83,$e7,$ff // character 164
	.byte	$9d,$99,$f3,$e7,$cf,$99,$b9,$ff // character 165
	.byte	$c3,$99,$c3,$c7,$98,$99,$c0,$ff // character 166
	.byte	$f9,$f3,$e7,$ff,$ff,$ff,$ff,$ff // character 167
	.byte	$f3,$e7,$cf,$cf,$cf,$e7,$f3,$ff // character 168
	.byte	$cf,$e7,$f3,$f3,$f3,$e7,$cf,$ff // character 169
	.byte	$ff,$99,$c3,$00,$c3,$99,$ff,$ff // character 170
	.byte	$ff,$e7,$e7,$81,$e7,$e7,$ff,$ff // character 171
	.byte	$ff,$ff,$ff,$ff,$ff,$e7,$e7,$cf // character 172
	.byte	$ff,$ff,$ff,$81,$ff,$ff,$ff,$ff // character 173
	.byte	$ff,$ff,$ff,$ff,$ff,$e7,$e7,$ff // character 174
	.byte	$ff,$fc,$f9,$f3,$e7,$cf,$9f,$ff // character 175
	.byte	$c3,$99,$91,$89,$99,$99,$c3,$ff // character 176
	.byte	$e7,$e7,$c7,$e7,$e7,$e7,$81,$ff // character 177
	.byte	$c3,$99,$f9,$f3,$cf,$9f,$81,$ff // character 178
	.byte	$c3,$99,$f9,$e3,$f9,$99,$c3,$ff // character 179
	.byte	$f9,$f1,$e1,$99,$80,$f9,$f9,$ff // character 180
	.byte	$81,$9f,$83,$f9,$f9,$99,$c3,$ff // character 181
	.byte	$c3,$99,$9f,$83,$99,$99,$c3,$ff // character 182
	.byte	$81,$99,$f3,$e7,$e7,$e7,$e7,$ff // character 183
	.byte	$c3,$99,$99,$c3,$99,$99,$c3,$ff // character 184
	.byte	$c3,$99,$99,$c1,$f9,$99,$c3,$ff // character 185
	.byte	$ff,$ff,$e7,$ff,$ff,$e7,$ff,$ff // character 186
	.byte	$ff,$ff,$e7,$ff,$ff,$e7,$e7,$cf // character 187
	.byte	$f1,$e7,$cf,$9f,$cf,$e7,$f1,$ff // character 188
	.byte	$ff,$ff,$81,$ff,$81,$ff,$ff,$ff // character 189
	.byte	$8f,$e7,$f3,$f9,$f3,$e7,$8f,$ff // character 190
	.byte	$c3,$99,$f9,$f3,$e7,$ff,$e7,$ff // character 191
	.byte	$ff,$ff,$ff,$00,$00,$ff,$ff,$ff // character 192
	.byte	$f7,$e3,$c1,$80,$80,$e3,$c1,$ff // character 193
	.byte	$e7,$e7,$e7,$e7,$e7,$e7,$e7,$e7 // character 194
	.byte	$ff,$ff,$ff,$00,$00,$ff,$ff,$ff // character 195
	.byte	$ff,$ff,$00,$00,$ff,$ff,$ff,$ff // character 196
	.byte	$ff,$00,$00,$ff,$ff,$ff,$ff,$ff // character 197
	.byte	$ff,$ff,$ff,$ff,$00,$00,$ff,$ff // character 198
	.byte	$cf,$cf,$cf,$cf,$cf,$cf,$cf,$cf // character 199
	.byte	$f3,$f3,$f3,$f3,$f3,$f3,$f3,$f3 // character 200
	.byte	$ff,$ff,$ff,$1f,$0f,$c7,$e7,$e7 // character 201
	.byte	$e7,$e7,$e3,$f0,$f8,$ff,$ff,$ff // character 202
	.byte	$e7,$e7,$c7,$0f,$1f,$ff,$ff,$ff // character 203
	.byte	$3f,$3f,$3f,$3f,$3f,$3f,$00,$00 // character 204
	.byte	$3f,$1f,$8f,$c7,$e3,$f1,$f8,$fc // character 205
	.byte	$fc,$f8,$f1,$e3,$c7,$8f,$1f,$3f // character 206
	.byte	$00,$00,$3f,$3f,$3f,$3f,$3f,$3f // character 207
	.byte	$00,$00,$fc,$fc,$fc,$fc,$fc,$fc // character 208
	.byte	$ff,$c3,$81,$81,$81,$81,$c3,$ff // character 209
	.byte	$ff,$ff,$ff,$ff,$ff,$00,$00,$ff // character 210
	.byte	$c9,$80,$80,$80,$c1,$e3,$f7,$ff // character 211
	.byte	$9f,$9f,$9f,$9f,$9f,$9f,$9f,$9f // character 212
	.byte	$ff,$ff,$ff,$f8,$f0,$e3,$e7,$e7 // character 213
	.byte	$3c,$18,$81,$c3,$c3,$81,$18,$3c // character 214
	.byte	$ff,$c3,$81,$99,$99,$81,$c3,$ff // character 215
	.byte	$e7,$e7,$99,$99,$e7,$e7,$c3,$ff // character 216
	.byte	$f9,$f9,$f9,$f9,$f9,$f9,$f9,$f9 // character 217
	.byte	$f7,$e3,$c1,$80,$c1,$e3,$f7,$ff // character 218
	.byte	$e7,$e7,$e7,$00,$00,$e7,$e7,$e7 // character 219
	.byte	$3f,$3f,$cf,$cf,$3f,$3f,$cf,$cf // character 220
	.byte	$e7,$e7,$e7,$e7,$e7,$e7,$e7,$e7 // character 221
	.byte	$ff,$ff,$fc,$c1,$89,$c9,$c9,$ff // character 222
	.byte	$00,$80,$c0,$e0,$f0,$f8,$fc,$fe // character 223
	.byte	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff // character 224
	.byte	$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f // character 225
	.byte	$ff,$ff,$ff,$ff,$00,$00,$00,$00 // character 226
	.byte	$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff // character 227
	.byte	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00 // character 228
	.byte	$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f // character 229
	.byte	$33,$33,$cc,$cc,$33,$33,$cc,$cc // character 230
	.byte	$fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc // character 231
	.byte	$ff,$ff,$ff,$ff,$33,$33,$cc,$cc // character 232
	.byte	$00,$01,$03,$07,$0f,$1f,$3f,$7f // character 233
	.byte	$fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc // character 234
	.byte	$e7,$e7,$e7,$e0,$e0,$e7,$e7,$e7 // character 235
	.byte	$ff,$ff,$ff,$ff,$f0,$f0,$f0,$f0 // character 236
	.byte	$e7,$e7,$e7,$e0,$e0,$ff,$ff,$ff // character 237
	.byte	$ff,$ff,$ff,$07,$07,$e7,$e7,$e7 // character 238
	.byte	$ff,$ff,$ff,$ff,$ff,$ff,$00,$00 // character 239
	.byte	$ff,$ff,$ff,$e0,$e0,$e7,$e7,$e7 // character 240
	.byte	$e7,$e7,$e7,$00,$00,$ff,$ff,$ff // character 241
	.byte	$ff,$ff,$ff,$00,$00,$e7,$e7,$e7 // character 242
	.byte	$e7,$e7,$e7,$07,$07,$e7,$e7,$e7 // character 243
	.byte	$3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f // character 244
	.byte	$1f,$1f,$1f,$1f,$1f,$1f,$1f,$1f // character 245
	.byte	$f8,$f8,$f8,$f8,$f8,$f8,$f8,$f8 // character 246
	.byte	$00,$00,$ff,$ff,$ff,$ff,$ff,$ff // character 247
	.byte	$00,$00,$00,$ff,$ff,$ff,$ff,$ff // character 248
	.byte	$ff,$ff,$ff,$ff,$ff,$00,$00,$00 // character 249
	.byte	$fc,$fc,$fc,$fc,$fc,$fc,$00,$00 // character 250
	.byte	$ff,$ff,$ff,$ff,$0f,$0f,$0f,$0f // character 251
	.byte	$f0,$f0,$f0,$f0,$ff,$ff,$ff,$ff // character 252
	.byte	$e7,$e7,$e7,$07,$07,$ff,$ff,$ff // character 253
	.byte	$0f,$0f,$0f,$0f,$ff,$ff,$ff,$ff // character 254
	.byte	$0f,$0f,$0f,$0f,$f0,$f0,$f0,$f0 // character 255




    *=$5000
//.align $100
FishTab:
    .fill 4000, $00
//.align $100
EmptyFRec:                              // Stack for fishes Emptyies recs (Words)
    .fill 2000, $00
//.align $100
SharkTab:
    .fill 4000, $00
//.align $100
EmptySRec:
    .fill 2000, $00                     // stack For Sharks Emptyies recs (Words)

DebugCnt:
    .byte 6 // numero totale tra pesci e squali
    // numeri usati per determinare le x,y al posto della scelta randomica
DebugX:
    .byte 1, 2,3,7,8,5
DebugY:
    .byte 1, 2,3,7,8,5


