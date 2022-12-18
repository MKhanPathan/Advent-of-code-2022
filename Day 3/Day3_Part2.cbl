000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 3 PROGRAM 2                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD3PG2.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT RUCK-SACK    ASSIGN TO AOCDAY3                        00110004
001200         ORGANIZATION    IS SEQUENTIAL                            00120004
001300          ACCESS MODE    IS SEQUENTIAL                            00130004
001400          FILE STATUS    IS FILE-STATUS.                          00140004
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  RUCK-SACK.                                                   00180000
001900 01  WS-SACK-REC.                                                 00190000
002000     05 SACK-ITEM           PIC X(80).                            00200004
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002300 01  WS-WORK-FIELDS.                                              00230000
002400     05 TOTAL-PRIORITY      PIC 9(10)   VALUE ZEROES.             00240000
002700     05 WS-ITEM1            PIC X(80)   VALUE SPACES.             00270000
002900     05 WS-CNT              PIC 9(02)   VALUE ZEROES.             00290000
003000     05 WS-PRIORITY         PIC X(01)   VALUE SPACES.             00300000
003100     05 WS-L-PRTY           PIC 9(02)   VALUE ZEROES.             00310000
003110     05 WS-ITEM-CNT         PIC 9(01)   VALUE ZEROES.             00311000
003200                                                                  00320000
003300 01  WS-SACK-ITEM2.                                               00330004
003400     05 WS-ITEM2            PIC X(01) OCCURS 80 TIMES             00340000
003500                                      INDEXED BY IX2.             00350000
003600                                                                  00360000
003601 02  WS-SACK-ITEM3.                                               00360104
003610     05 WS-ITEM3            PIC X(01) OCCURS 80 TIMES             00361000
003620                                      INDEXED BY IX3.             00362000
003630                                                                  00363000
003700 01  WS-PRTY-LOWER          PIC X(78) VALUE                       00370000
003800-    "a01b02c03d04e05f06g07h08i09j10k11l12m13n14o15p16q17r18s19t2000380000
003900-    "u21v22w23x24y25z26".                                        00390000
004000                                                                  00400000
004100 01  WS-PRIORITY-LOWER REDEFINES WS-PRTY-LOWER.                   00410000
004200     05 WS-PRIORITY-LVAL    OCCURS 26 TIMES                       00420000
004300                            ASCENDING KEY IS WS-PRTY-L            00430000
004400                            INDEXED BY INDXL.                     00440004
004500        10 WS-PRTY-L        PIC X(01).                            00450000
004600        10 WS-PRTY-VAL-L    PIC X(02).                            00460000
004700                                                                  00470000
004800                                                                  00480000
004900 01  WS-PRTY-UPPER          PIC X(78)  VALUE                      00490000
005000-    "A27B28C29D30E31F32G33H34I35J36K37L38M39N40O41P42Q43R44S45T4600500004
005100-    "U47V48W49X50Y51Z52".                                        00510004
005200                                                                  00520000
005300 01  WS-PRIORITY-UPPER REDEFINES WS-PRTY-UPPER.                   00530000
005400     05 WS-PRIORITY-UVAL    OCCURS 26 TIMES                       00540000
005500                            ASCENDING KEY IS WS-PRTY-U            00550000
005600                            INDEXED BY INDXU.                     00560004
005700        10 WS-PRTY-U        PIC X(01).                            00570000
005800        10 WS-PRTY-VAL-U    PIC 9(02).                            00580000
005900                                                                  00590000
006000 01  WS-SWITCHES.                                                 00600000
006100     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00610000
006200        88 SUCCESS          VALUE '00'.                           00620000
006300        88 EOF              VALUE '10'.                           00630000
006400     05 TYPE-STATUS         PIC X(01)   VALUE SPACES.             00640000
006500        88 TYPE-FOUND       VALUE 'Y'.                            00650000
006600        88 TYPE-NOT-FOUND   VALUE 'N'.                            00660000
006700                                                                  00670000
006800 PROCEDURE DIVISION.                                              00680000
006900                                                                  00690000
007000     OPEN INPUT RUCK-SACK.                                        00700000
007100     IF SUCCESS                                                   00710000
007200        PERFORM UNTIL EOF                                         00720000
007300           READ RUCK-SACK                                         00730000
007400                AT END                                            00740000
007500                   SET EOF TO TRUE                                00750000
007600                NOT AT END                                        00760000
007610                   ADD 1 TO WS-ITEM-CNT                           00761000
007620                   PERFORM 1000-LOAD-ITEMS     THRU 1000-EXIT     00762000
007800           END-READ                                               00780000
007900        END-PERFORM                                               00790000
008000                                                                  00800000
008100        CLOSE RUCK-SACK                                           00810000
008200        DISPLAY 'Total Priority: ' TOTAL-PRIORITY                 00820004
008300     END-IF.                                                      00830000
008400                                                                  00840000
008500     STOP RUN.                                                    00850000
008600                                                                  00860000
008610 1000-LOAD-ITEMS.                                                 00861000
008620     EVALUATE WS-ITEM-CNT                                         00862000
008621         WHEN 1                                                   00862100
008622              MOVE SACK-ITEM           TO WS-ITEM1                00862204
008623         WHEN 2                                                   00862300
008624              MOVE SACK-ITEM           TO WS-SACK-ITEM2           00862404
008625         WHEN 3                                                   00862500
008626              MOVE SACK-ITEM           TO WS-SACK-ITEM3           00862604
008627              PERFORM 2000-FIND-BADGE  THRU 2000-EXIT             00862701
008628              INITIALIZE                  WS-ITEM-CNT             00862800
008629                                          TYPE-STATUS             00862900
008630                                          WS-ITEM1                00863000
008631                                          WS-SACK-ITEM2           00863105
008632                                          WS-SACK-ITEM3           00863205
008633     END-EVALUATE.                                                00863300
008634                                                                  00863400
008635 1000-EXIT.                                                       00863500
008640     EXIT.                                                        00864000
009900                                                                  00990000
010000 2000-FIND-BADGE.                                                 01000000
010100                                                                  01010000
010200     PERFORM VARYING WS-CNT FROM 1 BY 1                           01020000
010300       UNTIL TYPE-FOUND OR WS-ITEM1(WS-CNT:1) IS EQUAL SPACE      01030000
010400       SET IX2 TO 1                                               01040001
010500       SEARCH WS-ITEM2                                            01050000
010600           AT END                                                 01060000
010700              SET TYPE-NOT-FOUND TO TRUE                          01070000
010800           WHEN WS-ITEM2(IX2) = WS-ITEM1(WS-CNT:1)                01080000
010801              SET IX3 TO 1                                        01080101
010810              SEARCH WS-ITEM3                                     01081000
010820                  AT END                                          01082000
010830                     SET TYPE-NOT-FOUND   TO TRUE                 01083000
010840                  WHEN WS-ITEM3(IX3) = WS-ITEM1(WS-CNT:1)         01084000
010900                     MOVE WS-ITEM1(WS-CNT:1)     TO WS-PRIORITY   01090000
011000                     PERFORM 3000-GET-PRIORITY   THRU 3000-EXIT   01100000
011100                     SET TYPE-FOUND TO TRUE                       01110000
011110              END-SEARCH                                          01111000
011200       END-SEARCH                                                 01120000
011300     END-PERFORM.                                                 01130000
011400 2000-EXIT.                                                       01140000
011500     EXIT.                                                        01150000
011600                                                                  01160000
011700 3000-GET-PRIORITY.                                               01170000
011800     IF WS-PRIORITY IS ALPHABETIC-LOWER                           01180000
011900        SEARCH ALL WS-PRIORITY-LVAL                               01190000
012000            AT END                                                01200000
012100               DISPLAY 'Invalid Priority: ' WS-PRIORITY           01210000
012200            WHEN WS-PRTY-L(INDXL) = WS-PRIORITY                   01220004
012300               MOVE WS-PRTY-VAL-L(INDXL)   TO WS-L-PRTY           01230004
012400               ADD WS-L-PRTY               TO TOTAL-PRIORITY      01240000
012500        END-SEARCH                                                01250000
012600     ELSE                                                         01260000
012700        IF WS-PRIORITY IS ALPHABETIC-UPPER                        01270000
012800           SEARCH ALL WS-PRIORITY-UVAL                            01280000
012900               AT END                                             01290000
013000                  DISPLAY 'Invalid Priority: ' WS-PRIORITY        01300000
013100               WHEN WS-PRTY-U(INDXU) = WS-PRIORITY                01310004
013200                  ADD WS-PRTY-VAL-U(INDXU)    TO TOTAL-PRIORITY   01320004
013300           END-SEARCH                                             01330000
013400        ELSE                                                      01340000
013500           DISPLAY 'Invalid Priority: ' WS-PRIORITY               01350000
013600        END-IF                                                    01360000
013700     END-IF.                                                      01370000
013800 3000-EXIT.                                                       01380000
013900     EXIT.                                                        01390000
