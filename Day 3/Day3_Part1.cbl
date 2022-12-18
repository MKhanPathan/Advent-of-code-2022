000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 3 PROGRAM 1                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD3PG1.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT RUCK-SACK    ASSIGN TO AOCDAY3                        00110029
001200         ORGANIZATION    IS SEQUENTIAL                            00120029
001300          ACCESS MODE    IS SEQUENTIAL                            00130029
001400          FILE STATUS    IS FILE-STATUS.                          00140029
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  RUCK-SACK.                                                   00180001
001900 01  WS-SACK-REC.                                                 00190001
002000     05 SACK-ITEM           PIC X(80).                            00200029
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002300 01  WS-WORK-FIELDS.                                              00230000
002400     05 TOTAL-PRIORITY      PIC 9(10)   VALUE ZEROES.             00240001
002410     05 WS-REC-LEN          PIC 9(02)   VALUE ZEROES.             00241002
002420     05 WS-COMP-LEN         PIC 9(02)   VALUE ZEROES.             00242002
002430     05 WS-COMP1            PIC X(40)   VALUE SPACES.             00243002
002440     05 WS-COMP2            PIC X(40)   VALUE SPACES.             00244002
002450     05 WS-CNT              PIC 9(02)   VALUE ZEROES.             00245006
002460     05 WS-PRIORITY         PIC X(01)   VALUE SPACES.             00246002
002470     05 WS-L-PRTY           PIC 9(02)   VALUE ZEROES.             00247022
002500                                                                  00250000
002510 01  WS-TABLE.                                                    00251002
002520     05 WS-COMP2-ARRAY      PIC X(01) OCCURS 40 TIMES             00252029
002530                                      INDEXED BY INDX.            00253002
002531                                                                  00253102
002532 01  WS-PRTY-LOWER          PIC X(78) VALUE                       00253223
002533-    "a01b02c03d04e05f06g07h08i09j10k11l12m13n14o15p16q17r18s19t2000253327
002534-    "u21v22w23x24y25z26".                                        00253427
002540                                                                  00254002
002550 01  WS-PRIORITY-LOWER REDEFINES WS-PRTY-LOWER.                   00255010
002560     05 WS-PRIORITY-LVAL    OCCURS 26 TIMES                       00256010
002561                            ASCENDING KEY IS WS-PRTY-L            00256110
002570                            INDEXED BY INDXL.                     00257029
002590        10 WS-PRTY-L        PIC X(01).                            00259010
002591        10 WS-PRTY-VAL-L    PIC X(02).                            00259122
002592                                                                  00259222
002593                                                                  00259302
002594 01  WS-PRTY-UPPER          PIC X(78)  VALUE                      00259423
002595-    "A27B28C29D30E31F32G33H34I35J36K37L38M39N40O41P42Q43R44S45T4600259530
002596-    "U47V48W49X50Y51Z52".                                        00259630
002597                                                                  00259711
002598 01  WS-PRIORITY-UPPER REDEFINES WS-PRTY-UPPER.                   00259810
002599     05 WS-PRIORITY-UVAL    OCCURS 26 TIMES                       00259910
002600                            ASCENDING KEY IS WS-PRTY-U            00260010
002601                            INDEXED BY INDXU.                     00260129
002602        10 WS-PRTY-U        PIC X(01).                            00260210
002603        10 WS-PRTY-VAL-U    PIC 9(02).                            00260310
002604                                                                  00260410
002610 01  WS-SWITCHES.                                                 00261002
002700     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00270000
002800        88 SUCCESS          VALUE '00'.                           00280000
002900        88 EOF              VALUE '10'.                           00290000
002910     05 TYPE-STATUS         PIC X(01)   VALUE SPACES.             00291002
002920        88 TYPE-FOUND       VALUE 'Y'.                            00292002
002930        88 TYPE-NOT-FOUND   VALUE 'N'.                            00293002
003000                                                                  00300000
003100 PROCEDURE DIVISION.                                              00310000
003200                                                                  00320000
003300     OPEN INPUT RUCK-SACK.                                        00330001
003400     IF SUCCESS                                                   00340000
003500        PERFORM UNTIL EOF                                         00350000
003600           READ RUCK-SACK                                         00360001
003700                AT END                                            00370000
003800                   SET EOF TO TRUE                                00380000
003900                NOT AT END                                        00390000
004000                   PERFORM 1000-CALC-PRIORITY  THRU 1000-EXIT     00400001
004100           END-READ                                               00410000
004200        END-PERFORM                                               00420000
004300                                                                  00430000
004400        CLOSE RUCK-SACK                                           00440001
004500        DISPLAY 'Total Priority: ' TOTAL-PRIORITY                 00450029
004600     END-IF.                                                      00460000
004700                                                                  00470000
004800     STOP RUN.                                                    00480000
004900                                                                  00490000
005000 1000-CALC-PRIORITY.                                              00500001
005001     INITIALIZE WS-CNT, WS-REC-LEN, WS-COMP-LEN, TYPE-STATUS      00500118
005010     INSPECT FUNCTION REVERSE(SACK-ITEM) TALLYING WS-CNT          00501029
005020             FOR LEADING SPACES                                   00502009
005030     COMPUTE WS-REC-LEN = FUNCTION LENGTH (SACK-ITEM) - WS-CNT    00503029
005300     COMPUTE WS-COMP-LEN = WS-REC-LEN / 2                         00530002
005400     MOVE SACK-ITEM ( 1:WS-COMP-LEN )  TO WS-COMP1                00540029
005500     MOVE SACK-ITEM ( WS-COMP-LEN + 1 : ) TO WS-COMP2             00550029
005510     MOVE WS-COMP2                     TO WS-TABLE.               00551005
005600     PERFORM 2000-FIND-ITEM            THRU 2000-EXIT.            00560002
008200 1000-EXIT.                                                       00820000
008300     EXIT.                                                        00830000
008400                                                                  00840002
008500 2000-FIND-ITEM.                                                  00850002
008600                                                                  00860002
008610     PERFORM VARYING WS-CNT FROM 1 BY 1                           00861006
008620       UNTIL TYPE-FOUND OR WS-COMP1(WS-CNT:1) IS EQUAL SPACE      00862015
008630       SET INDX TO 1                                              00863007
008631       SEARCH WS-COMP2-ARRAY                                      00863129
008632           AT END                                                 00863202
008633              SET TYPE-NOT-FOUND TO TRUE                          00863302
008634           WHEN WS-COMP2-ARRAY(INDX) = WS-COMP1(WS-CNT:1)         00863429
008636              MOVE WS-COMP1(WS-CNT:1)     TO WS-PRIORITY          00863615
008638              PERFORM 3000-GET-PRIORITY   THRU 3000-EXIT          00863802
008639              SET TYPE-FOUND TO TRUE                              00863917
008640       END-SEARCH                                                 00864002
008650     END-PERFORM.                                                 00865002
008700 2000-EXIT.                                                       00870002
008800     EXIT.                                                        00880002
008900                                                                  00890002
009000 3000-GET-PRIORITY.                                               00900002
009010     IF WS-PRIORITY IS ALPHABETIC-LOWER                           00901010
009100        SEARCH ALL WS-PRIORITY-LVAL                               00910010
009110            AT END                                                00911010
009120               DISPLAY 'Invalid Priority: ' WS-PRIORITY           00912010
009130            WHEN WS-PRTY-L(INDXL) = WS-PRIORITY                   00913029
009140               MOVE WS-PRTY-VAL-L(INDXL)   TO WS-L-PRTY           00914029
009141               ADD WS-L-PRTY               TO TOTAL-PRIORITY      00914122
009150        END-SEARCH                                                00915010
009160     ELSE                                                         00916010
009161        IF WS-PRIORITY IS ALPHABETIC-UPPER                        00916121
009170           SEARCH ALL WS-PRIORITY-UVAL                            00917021
009180               AT END                                             00918021
009190                  DISPLAY 'Invalid Priority: ' WS-PRIORITY        00919021
009191               WHEN WS-PRTY-U(INDXU) = WS-PRIORITY                00919129
009192                  ADD WS-PRTY-VAL-U(INDXU)    TO TOTAL-PRIORITY   00919229
009193           END-SEARCH                                             00919321
009194        ELSE                                                      00919421
009195           DISPLAY 'Invalid Priority: ' WS-PRIORITY               00919521
009196        END-IF                                                    00919621
009197     END-IF.                                                      00919712
009200 3000-EXIT.                                                       00920002
009300     EXIT.                                                        00930002
