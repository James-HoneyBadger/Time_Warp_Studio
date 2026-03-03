10  REM ================================================
20  REM  DUNGEON ADVENTURE - Text Adventure Game
30  REM  A fully playable dungeon crawler game written
40  REM  in classic BASIC. Navigate rooms, find items,
50  REM  fight monsters, and escape the dungeon!
60  REM ================================================
70  GOSUB 5000
80  REM -- Game Intro --
90  PRINT "╔══════════════════════════════════╗"
100 PRINT "║    DUNGEON OF ETERNAL SHADOW     ║"
110 PRINT "║    A Text Adventure in BASIC     ║"
120 PRINT "╚══════════════════════════════════╝"
130 PRINT ""
140 PRINT "You are an adventurer who has entered"
150 PRINT "the infamous Dungeon of Eternal Shadow."
160 PRINT "Your goal: find the Golden Key and escape."
170 PRINT ""
180 PRINT "Commands: NORTH SOUTH EAST WEST LOOK"
190 PRINT "          GET <item> USE <item> INV QUIT"
200 PRINT ""
210 REM -- Initialize Game State --
220 HP = 20
230 ROOM = 1
240 HAS_TORCH = 0
250 HAS_SWORD = 0
260 HAS_KEY = 0
270 HAS_SHIELD = 0
280 POTION = 2
290 MONSTER_DEAD = 0
300 GAME_OVER = 0
310 PRINT "PRESS ENTER TO BEGIN..."
320 INPUT DUMMY$
330 GOSUB 2000
340 REM ===============================
350 REM  MAIN GAME LOOP
360 REM ===============================
370 WHILE GAME_OVER = 0
380   PRINT ""
390   PRINT "HP:"; HP; "  Room:"; ROOM
400   PRINT "> ";
410   INPUT CMD$
420   REM Parse command
430   IF CMD$ = "LOOK" THEN GOSUB 2000 : GOTO 860
440   IF CMD$ = "NORTH" THEN GOSUB 3000 : GOTO 860
450   IF CMD$ = "SOUTH" THEN GOSUB 3100 : GOTO 860
460   IF CMD$ = "EAST" THEN GOSUB 3200 : GOTO 860
470   IF CMD$ = "WEST" THEN GOSUB 3300 : GOTO 860
480   IF CMD$ = "INV" THEN GOSUB 4000 : GOTO 860
490   IF CMD$ = "QUIT" THEN GAME_OVER = 1 : GOTO 860
500   IF LEFT$(CMD$, 4) = "GET " THEN ITEM$ = MID$(CMD$, 5, 20) : GOSUB 4100 : GOTO 860
510   IF LEFT$(CMD$, 4) = "USE " THEN ITEM$ = MID$(CMD$, 5, 20) : GOSUB 4200 : GOTO 860
520   IF CMD$ = "FIGHT" THEN GOSUB 4500 : GOTO 860
530   PRINT "I don't understand that command."
540   PRINT "Try: NORTH SOUTH EAST WEST LOOK GET USE INV"
860 WEND
870 PRINT "Thanks for playing Dungeon of Eternal Shadow!"
880 END

1000 REM ===============================
1010 REM  ROOM DATA
1020 REM ===============================
1030 REM  Room 1: Entrance
1040 REM  Room 2: Torch Chamber
1050 REM  Room 3: Armory
1060 REM  Room 4: Monster's Lair
1070 REM  Room 5: Treasure Vault
1080 REM  Room 6: Exit Corridor
1090 RETURN

2000 REM ===============================
2010 REM  DESCRIBE CURRENT ROOM
2020 REM ===============================
2030 PRINT ""
2040 PRINT "════════════════════════"
2050 IF ROOM = 1 THEN GOSUB 2100 : RETURN
2060 IF ROOM = 2 THEN GOSUB 2200 : RETURN
2070 IF ROOM = 3 THEN GOSUB 2300 : RETURN
2080 IF ROOM = 4 THEN GOSUB 2400 : RETURN
2090 IF ROOM = 5 THEN GOSUB 2500 : RETURN
2095 IF ROOM = 6 THEN GOSUB 2600 : RETURN
2096 RETURN

2100 REM Room 1: Entrance Hall
2110 PRINT "[ENTRANCE HALL]"
2120 PRINT "A dimly lit stone chamber. The heavy"
2130 PRINT "iron door you entered through is sealed."
2140 PRINT "Moss covers the damp walls."
2150 PRINT ""
2160 PRINT "Exits: EAST (deeper in), NORTH (dark passage)"
2170 RETURN

2200 REM Room 2: Torch Chamber
2210 PRINT "[TORCH CHAMBER]"
2220 PRINT "Flickering torches line the walls."
2230 PRINT "The light reveals strange inscriptions."
2240 IF HAS_TORCH = 0 THEN PRINT "A TORCH lies in a bracket on the wall."
2250 PRINT ""
2260 PRINT "Exits: SOUTH (entrance), EAST (armory)"
2270 RETURN

2300 REM Room 3: Armory
2310 PRINT "[ABANDONED ARMORY]"
2320 PRINT "Rusted weapons hang from the walls."
2330 PRINT "Most are too corroded to be useful."
2340 IF HAS_SWORD = 0 THEN PRINT "But one SWORD still gleams in the corner!"
2350 IF HAS_SHIELD = 0 THEN PRINT "A cracked SHIELD leans against the wall."
2360 PRINT ""
2370 PRINT "Exits: WEST (torch chamber), SOUTH (monster lair)"
2380 RETURN

2400 REM Room 4: Monster's Lair
2410 PRINT "[MONSTER'S LAIR]"
2420 PRINT "The stench of decay fills the air."
2430 PRINT "Bones litter the floor."
2440 IF MONSTER_DEAD = 0 THEN PRINT "A massive TROLL blocks the path EAST!"
2450 IF MONSTER_DEAD = 0 THEN PRINT "It looks hungry. You should FIGHT or flee."
2460 IF MONSTER_DEAD = 1 THEN PRINT "The dead troll's body lies here."
2470 IF MONSTER_DEAD = 1 THEN PRINT "The path EAST is now clear."
2480 PRINT ""
2490 IF MONSTER_DEAD = 0 THEN PRINT "Exits: NORTH (armory)"
2495 IF MONSTER_DEAD = 1 THEN PRINT "Exits: NORTH (armory), EAST (vault)"
2496 RETURN

2500 REM Room 5: Treasure Vault
2510 PRINT "[GOLDEN VAULT]"
2520 PRINT "You stand in a glittering chamber!"
2530 PRINT "Gold coins and gems cover the floor."
2540 IF HAS_KEY = 0 THEN PRINT "In the center: the legendary GOLDEN KEY!"
2550 IF HAS_KEY = 1 THEN PRINT "You've already taken the Golden Key."
2560 PRINT ""
2570 PRINT "Exits: WEST (monster lair), NORTH (exit corridor)"
2580 RETURN

2600 REM Room 6: Exit Corridor
2610 PRINT "[EXIT CORRIDOR]"
2620 PRINT "A long stone corridor leads to freedom!"
2630 PRINT "The exit door stands before you."
2640 IF HAS_KEY = 0 THEN PRINT "The door is LOCKED. You need the Golden Key!"
2650 IF HAS_KEY = 1 THEN PRINT "USE KEY to unlock the door and ESCAPE!"
2660 PRINT ""
2670 PRINT "Exits: SOUTH (treasure vault)"
2680 RETURN

3000 REM ===============================
3010 REM  MOVEMENT: NORTH
3020 REM ===============================
3030 IF ROOM = 2 THEN ROOM = 1 : GOSUB 2000 : RETURN
3040 IF ROOM = 4 THEN ROOM = 3 : GOSUB 2000 : RETURN
3050 IF ROOM = 5 THEN ROOM = 6 : GOSUB 2000 : RETURN
3060 PRINT "You can't go that way."
3070 RETURN

3100 REM MOVEMENT: SOUTH
3110 IF ROOM = 1 THEN ROOM = 2 : GOSUB 2000 : RETURN
3120 IF ROOM = 3 THEN ROOM = 4 : GOSUB 2000 : RETURN
3130 IF ROOM = 6 THEN ROOM = 5 : GOSUB 2000 : RETURN
3140 PRINT "You can't go that way."
3150 RETURN

3200 REM MOVEMENT: EAST
3210 IF ROOM = 1 THEN ROOM = 2 : GOSUB 2000 : RETURN
3220 IF ROOM = 2 THEN ROOM = 3 : GOSUB 2000 : RETURN
3230 IF ROOM = 4 AND MONSTER_DEAD = 1 THEN ROOM = 5 : GOSUB 2000 : RETURN
3240 IF ROOM = 4 AND MONSTER_DEAD = 0 THEN PRINT "The troll won't let you pass!" : RETURN
3250 PRINT "You can't go that way."
3260 RETURN

3300 REM MOVEMENT: WEST
3310 IF ROOM = 3 THEN ROOM = 2 : GOSUB 2000 : RETURN
3320 IF ROOM = 5 THEN ROOM = 4 : GOSUB 2000 : RETURN
3330 PRINT "You can't go that way."
3340 RETURN

4000 REM ===============================
4010 REM  INVENTORY
4020 REM ===============================
4030 PRINT "Your inventory:"
4040 IF HAS_TORCH = 1 THEN PRINT "  - Torch"
4050 IF HAS_SWORD = 1 THEN PRINT "  - Sword"
4060 IF HAS_SHIELD = 1 THEN PRINT "  - Shield"
4070 IF HAS_KEY = 1 THEN PRINT "  - Golden Key"
4080 PRINT "  - Healing Potions:"; POTION
4090 RETURN

4100 REM ===============================
4110 REM  GET ITEM
4120 REM ===============================
4130 IF ROOM = 2 AND ITEM$ = "TORCH" AND HAS_TORCH = 0 THEN HAS_TORCH = 1 : PRINT "You take the torch. Now you can see!" : RETURN
4140 IF ROOM = 3 AND ITEM$ = "SWORD" AND HAS_SWORD = 0 THEN HAS_SWORD = 1 : PRINT "You grab the gleaming sword!" : RETURN
4150 IF ROOM = 3 AND ITEM$ = "SHIELD" AND HAS_SHIELD = 0 THEN HAS_SHIELD = 1 : PRINT "You take the cracked shield." : RETURN
4160 IF ROOM = 5 AND ITEM$ = "KEY" AND HAS_KEY = 0 THEN HAS_KEY = 1 : PRINT "You grab the Golden Key! Now find the exit!" : RETURN
4170 PRINT "There's no "; ITEM$; " to take here."
4180 RETURN

4200 REM ===============================
4210 REM  USE ITEM
4220 REM ===============================
4230 IF ITEM$ = "POTION" AND POTION > 0 THEN GOSUB 4300 : RETURN
4240 IF ITEM$ = "KEY" AND ROOM = 6 AND HAS_KEY = 1 THEN GOSUB 4400 : RETURN
4250 IF ITEM$ = "TORCH" AND HAS_TORCH = 1 THEN PRINT "The torch lights your way." : RETURN
4260 PRINT "You can't use "; ITEM$; " right now."
4270 RETURN

4300 REM USE POTION
4310 POTION = POTION - 1
4320 HP = HP + 8
4330 IF HP > 20 THEN HP = 20
4340 PRINT "You drink the healing potion. +8 HP!"
4350 PRINT "HP is now"; HP
4360 RETURN

4400 REM USE KEY AT EXIT
4410 PRINT "You insert the Golden Key into the lock..."
4420 PRINT "The door creaks open with a thunderous groan!"
4430 PRINT ""
4440 PRINT "╔══════════════════════════════════════╗"
4450 PRINT "║   *** YOU ESCAPED THE DUNGEON! ***   ║"
4460 PRINT "║                                      ║"
4470 PRINT "║  You emerge into blinding daylight.  ║"
4480 PRINT "║  The Golden Key is yours forever!    ║"
4490 PRINT "╚══════════════════════════════════════╝"
4500 GAME_OVER = 1
4510 RETURN

4500 REM ===============================
4510 REM  COMBAT
4520 REM ===============================
4530 IF ROOM <> 4 THEN PRINT "There's nothing to fight here!" : RETURN
4540 IF MONSTER_DEAD = 1 THEN PRINT "The troll is already dead." : RETURN
4550 PRINT "You engage the TROLL in combat!"
4560 REM Calculate attack
4570 ATK = 3 + INT(RND * 4)
4580 IF HAS_SWORD = 1 THEN ATK = ATK + 4
4590 DEF_MOD = 0
4600 IF HAS_SHIELD = 1 THEN DEF_MOD = 2
4610 TROLL_ATK = 4 + INT(RND * 3) - DEF_MOD
4620 PRINT "You hit the troll for"; ATK; "damage!"
4630 PRINT "The troll hits you for"; TROLL_ATK; "damage!"
4640 HP = HP - TROLL_ATK
4650 TROLL_HP = 12 - ATK
4660 IF TROLL_HP <= 0 THEN GOSUB 4700 : RETURN
4670 PRINT "The troll is wounded but still standing!"
4680 PRINT "Your HP:"; HP
4690 IF HP <= 0 THEN PRINT "You have been defeated!" : GAME_OVER = 1
4695 RETURN

4700 REM Troll defeated
4710 PRINT "The troll ROARS and collapses!"
4720 PRINT "You defeated the troll!"
4730 MONSTER_DEAD = 1
4740 PRINT "The path EAST is now open!"
4750 RETURN

5000 REM Init RND seed
5010 RETURN
