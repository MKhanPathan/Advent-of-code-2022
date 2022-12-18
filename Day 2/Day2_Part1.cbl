000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 2 PROGRAM 1                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD2PG1.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT GAME-DATA   ASSIGN TO AOCDAY2                         00110003
001200         ORGANIZATION   IS SEQUENTIAL                             00120003
001300          ACCESS MODE   IS SEQUENTIAL                             00130003
001400          FILE STATUS   IS FILE-STATUS.                           00140003
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  GAME-DATA.                                                   00180000
001900 01  WS-ROUND-REC.                                                00190002
002000     05 ROUND               PIC X(03).                            00200000
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002500 01  WS-WORK-FIELDS.                                              00250000
002600     05 TOTAL-SCORE         PIC 9(10)   VALUE ZEROES.             00260000
002900                                                                  00290000
003000 01  WS-FILE-STATUS.                                              00300000
003100     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00310000
003200        88 SUCCESS          VALUE '00'.                           00320000
003300        88 EOF              VALUE '10'.                           00330000
003400                                                                  00340000
003500 PROCEDURE DIVISION.                                              00350000
003600                                                                  00360000
004300     OPEN INPUT GAME-DATA.                                        00430000
004400     IF SUCCESS                                                   00440000
004500        PERFORM UNTIL EOF                                         00450000
005200           READ GAME-DATA                                         00520000
005300                AT END                                            00530000
005400                   SET EOF TO TRUE                                00540000
005500                NOT AT END                                        00550000
005600                   PERFORM 1000-CALC-SCORE     THRU 1000-EXIT     00560000
005700           END-READ                                               00570000
005800        END-PERFORM                                               00580000
006000                                                                  00600000
007212        CLOSE GAME-DATA                                           00721200
007213        DISPLAY 'Total Score: ' TOTAL-SCORE                       00721303
007214     END-IF.                                                      00721400
007220                                                                  00722000
007221     STOP RUN.                                                    00722100
007222                                                                  00722200
007223 1000-CALC-SCORE.                                                 00722300
007224     EVALUATE ROUND                                               00722400
007225        WHEN 'A X'                                                00722500
007226           ADD 4      TO TOTAL-SCORE                              00722600
007227                                                                  00722701
007228        WHEN 'B X'                                                00722800
007229           ADD 1      TO TOTAL-SCORE                              00722900
007230                                                                  00723000
007231        WHEN 'C X'                                                00723100
007232           ADD 7      TO TOTAL-SCORE                              00723200
007233                                                                  00723301
007234        WHEN 'A Y'                                                00723400
007235           ADD 8      TO TOTAL-SCORE                              00723500
007236                                                                  00723600
007237        WHEN 'B Y'                                                00723700
007238           ADD 5      TO TOTAL-SCORE                              00723800
007239                                                                  00723901
007240        WHEN 'C Y'                                                00724000
007241           ADD 2      TO TOTAL-SCORE                              00724100
007242                                                                  00724200
007243        WHEN 'A Z'                                                00724300
007244           ADD 3      TO TOTAL-SCORE                              00724400
007245                                                                  00724501
007246        WHEN 'B Z'                                                00724600
007247           ADD 9      TO TOTAL-SCORE                              00724700
007248                                                                  00724801
007249        WHEN 'C Z'                                                00724900
007250           ADD 6      TO TOTAL-SCORE                              00725000
007251                                                                  00725100
007252        WHEN OTHER                                                00725200
007253           DISPLAY 'Invalid round: ' ROUND                        00725300
007254     END-EVALUATE.                                                00725400
007256 1000-EXIT.                                                       00725600
007260     EXIT.                                                        00726000
