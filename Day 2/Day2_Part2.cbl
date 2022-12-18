000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 2 PROGRAM 2                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD2PG2.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT GAME-DATA   ASSIGN TO AOCDAY2                         00110001
001200         ORGANIZATION   IS SEQUENTIAL                             00120001
001300          ACCESS MODE   IS SEQUENTIAL                             00130001
001400          FILE STATUS   IS FILE-STATUS.                           00140001
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  GAME-DATA.                                                   00180000
001900 01  WS-ROUND-REC.                                                00190000
002000     05 ROUND               PIC X(03).                            00200000
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002300 01  WS-WORK-FIELDS.                                              00230000
002400     05 TOTAL-SCORE         PIC 9(10)   VALUE ZEROES.             00240000
002500                                                                  00250000
002600 01  WS-FILE-STATUS.                                              00260000
002700     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00270000
002800        88 SUCCESS          VALUE '00'.                           00280000
002900        88 EOF              VALUE '10'.                           00290000
003000                                                                  00300000
003100 PROCEDURE DIVISION.                                              00310000
003200                                                                  00320000
003300     OPEN INPUT GAME-DATA.                                        00330000
003400     IF SUCCESS                                                   00340000
003500        PERFORM UNTIL EOF                                         00350000
003600           READ GAME-DATA                                         00360000
003700                AT END                                            00370000
003800                   SET EOF TO TRUE                                00380000
003900                NOT AT END                                        00390000
004000                   PERFORM 1000-CALC-SCORE     THRU 1000-EXIT     00400000
004100           END-READ                                               00410000
004200        END-PERFORM                                               00420000
004300                                                                  00430000
004400        CLOSE GAME-DATA                                           00440000
004500        DISPLAY 'Total Score: ' TOTAL-SCORE                       00450002
004600     END-IF.                                                      00460000
004700                                                                  00470000
004800     STOP RUN.                                                    00480000
004900                                                                  00490000
005000 1000-CALC-SCORE.                                                 00500000
005100     EVALUATE ROUND                                               00510000
005200        WHEN 'A X'                                                00520000
005300           ADD 3      TO TOTAL-SCORE                              00530000
005400                                                                  00540000
005500        WHEN 'B X'                                                00550000
005600           ADD 1      TO TOTAL-SCORE                              00560000
005700                                                                  00570000
005800        WHEN 'C X'                                                00580000
005900           ADD 2      TO TOTAL-SCORE                              00590000
006000                                                                  00600000
006100        WHEN 'A Y'                                                00610000
006200           ADD 4      TO TOTAL-SCORE                              00620000
006300                                                                  00630000
006400        WHEN 'B Y'                                                00640000
006500           ADD 5      TO TOTAL-SCORE                              00650000
006600                                                                  00660000
006700        WHEN 'C Y'                                                00670000
006800           ADD 6      TO TOTAL-SCORE                              00680000
006900                                                                  00690000
007000        WHEN 'A Z'                                                00700000
007100           ADD 8      TO TOTAL-SCORE                              00710000
007200                                                                  00720000
007300        WHEN 'B Z'                                                00730000
007400           ADD 9      TO TOTAL-SCORE                              00740000
007500                                                                  00750000
007600        WHEN 'C Z'                                                00760000
007700           ADD 7      TO TOTAL-SCORE                              00770000
007800                                                                  00780000
007900        WHEN OTHER                                                00790000
008000           DISPLAY 'Invalid round: ' ROUND                        00800000
008100     END-EVALUATE.                                                00810000
008200 1000-EXIT.                                                       00820000
008300     EXIT.                                                        00830000
