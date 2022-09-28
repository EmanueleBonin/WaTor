# C64-WaTor
 C64-WaTor
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

