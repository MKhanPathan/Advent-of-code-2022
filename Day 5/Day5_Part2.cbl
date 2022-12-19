000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 5 PROGRAM 2                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD5PG2.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT STACKS-DATA  ASSIGN TO AOCDAY51                       00110000
001200         ORGANIZATION    IS SEQUENTIAL                            00120000
001300          ACCESS MODE    IS SEQUENTIAL                            00130000
001400          FILE STATUS    IS STACK-FILE-STATUS.                    00140000
001500                                                                  00150000
001600     SELECT PROCESS-DATA  ASSIGN TO AOCDAY52                      00160000
001700         ORGANIZATION    IS SEQUENTIAL                            00170000
001800          ACCESS MODE    IS SEQUENTIAL                            00180000
001900          FILE STATUS    IS PROCESS-FILE-STATUS.                  00190000
002000                                                                  00200000
002100 DATA DIVISION.                                                   00210000
002200 FILE SECTION.                                                    00220000
002300 FD  STACKS-DATA                                                  00230000
002400     DATA RECORD IS STACK-DATA-REC.                               00240000
002500 01  STACK-DATA-REC.                                              00250000
002600     05 STACK-REC              PIC X(10).                         00260000
002700                                                                  00270000
002800 FD  PROCESS-DATA                                                 00280000
002900     DATA RECORD IS PROCESS-DATA-REC.                             00290000
003000 01  PROCESS-DATA-REC.                                            00300000
003100     05 PROCESS-REC            PIC X(21).                         00310000
003200                                                                  00320000
003300 WORKING-STORAGE SECTION.                                         00330000
003400 01  WS-STACKS-ARRAY.                                             00340000
003500     05 STACK                  PIC X(100)  OCCURS 9 TIMES.        00350000
003600                                                                  00360000
003700 01  WS-WORK-FIELDS.                                              00370000
003800     05 WS-HOLD-STACK          PIC X(100).                        00380000
003900     05 WS-TOP-STACK           PIC X(009).                        00390000
004000     05 WS-CNT                 PIC 9(02)   VALUE ZEROES.          00400000
004100     05 WS-UNSTRING.                                              00410000
004200        10 WS-MOVE             PIC X(04).                         00420000
004300        10 WS-CRATES           PIC X(02).                         00430000
004400        10 WS-FROM             PIC X(04).                         00440000
004500        10 WS-FROM-STACK       PIC X(02).                         00450000
004600        10 WS-TO               PIC X(02).                         00460000
004700        10 WS-TO-STACK         PIC X(02).                         00470000
004800        10 CRATES              PIC 9(02).                         00480000
004900        10 FROM-STACK          PIC 9(02).                         00490000
005000        10 TO-STACK            PIC 9(02).                         00500000
005100     05 WS-CALC-FIELDS.                                           00510000
005200        10 WS-ACT-LEN          PIC 9(02).                         00520000
005300        10 WS-SPACES           PIC 9(02).                         00530000
005400        10 WS-REM              PIC 9(02).                         00540000
005500                                                                  00550000
005600 01  WS-SWITCHES.                                                 00560000
005700     05 STACK-FILE-STATUS      PIC X(02)   VALUE SPACES.          00570000
005800        88 STACK-SUCCESS       VALUE '00'.                        00580000
005900        88 STACK-EOF           VALUE '10'.                        00590000
006000                                                                  00600000
006100     05 PROCESS-FILE-STATUS    PIC X(02)   VALUE SPACES.          00610000
006200        88 PROCESS-SUCCESS     VALUE '00'.                        00620000
006300        88 PROCESS-EOF         VALUE '10'.                        00630000
006400                                                                  00640000
006500 PROCEDURE DIVISION.                                              00650000
006600                                                                  00660000
006700     INITIALIZE WS-STACKS-ARRAY.                                  00670000
006800     PERFORM 1000-READ-STACKS-DATA       THRU 1000-EXIT.          00680000
006900     PERFORM 2000-READ-PROCESS-DATA      THRU 2000-EXIT.          00690000
007000     PERFORM 3000-DISPLAY-TOP-CRATES     THRU 3000-EXIT.          00700000
007100                                                                  00710000
007200     STOP RUN.                                                    00720000
007300                                                                  00730000
007400 1000-READ-STACKS-DATA.                                           00740000
007500     OPEN INPUT STACKS-DATA.                                      00750000
007600     IF STACK-SUCCESS                                             00760000
007700        PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL STACK-EOF        00770000
007800           READ STACKS-DATA                                       00780000
007900             AT END                                               00790000
008000                CLOSE STACKS-DATA                                 00800000
008100                SET STACK-EOF       TO TRUE                       00810000
008200             NOT AT END                                           00820000
008300                MOVE STACK-REC      TO STACK(WS-CNT)              00830000
008400           END-READ                                               00840000
008500        END-PERFORM                                               00850000
008600     END-IF.                                                      00860000
008700 1000-EXIT.                                                       00870000
008800     EXIT.                                                        00880000
008900                                                                  00890000
009000 2000-READ-PROCESS-DATA.                                          00900000
009100     OPEN INPUT PROCESS-DATA.                                     00910000
009200     IF PROCESS-SUCCESS                                           00920000
009300        PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL PROCESS-EOF      00930000
009400           READ PROCESS-DATA                                      00940000
009500             AT END                                               00950000
009600                CLOSE PROCESS-DATA                                00960000
009700                SET PROCESS-EOF     TO TRUE                       00970000
009800             NOT AT END                                           00980000
009900                PERFORM 2100-GET-PROCESS   THRU 2100-EXIT         00990000
010000                PERFORM 2200-MOVE-CRATES   THRU 2200-EXIT         01000000
010100           END-READ                                               01010000
010200        END-PERFORM                                               01020000
010300     END-IF.                                                      01030000
010400 2000-EXIT.                                                       01040000
010500     EXIT.                                                        01050000
010600                                                                  01060000
010700 2100-GET-PROCESS.                                                01070000
010800     INITIALIZE WS-UNSTRING.                                      01080000
010900     UNSTRING PROCESS-REC DELIMITED BY SPACE                      01090000
011000                          INTO WS-MOVE                            01100000
011100                               WS-CRATES                          01110000
011200                               WS-FROM                            01120000
011300                               WS-FROM-STACK                      01130000
011400                               WS-TO                              01140000
011500                               WS-TO-STACK.                       01150000
011600     MOVE FUNCTION TRIM(WS-CRATES)         TO CRATES.             01160000
011700     MOVE FUNCTION TRIM(WS-FROM-STACK)     TO FROM-STACK.         01170000
011800     MOVE FUNCTION TRIM(WS-TO-STACK)       TO TO-STACK.           01180000
011900 2100-EXIT.                                                       01190000
012000     EXIT.                                                        01200000
012100                                                                  01210000
012200 2200-MOVE-CRATES.                                                01220000
012300     MOVE ZEROES        TO WS-ACT-LEN                             01230000
012400                           WS-SPACES.                             01240000
012500     INSPECT FUNCTION REVERSE(STACK(TO-STACK))                    01250000
012600             TALLYING WS-SPACES FOR LEADING SPACES.               01260000
012700                                                                  01270000
012800     COMPUTE WS-ACT-LEN = LENGTH OF STACK(TO-STACK) -             01280000
012900                          WS-SPACES + 1.                          01290000
013000     MOVE SPACES        TO WS-HOLD-STACK.                         01300000
013100     MOVE FUNCTION REVERSE(FUNCTION TRIM(STACK(FROM-STACK)))      01310000
013200                        TO WS-HOLD-STACK.                         01320000
013300     MOVE FUNCTION REVERSE(WS-HOLD-STACK(1:CRATES))               01330000
013400                        TO STACK(TO-STACK)(WS-ACT-LEN:CRATES).    01340000
013500     MOVE ZEROES        TO WS-ACT-LEN                             01350000
013600                           WS-SPACES                              01360000
013700                           WS-REM.                                01370000
013800     INSPECT FUNCTION REVERSE(STACK(FROM-STACK))                  01380000
013900             TALLYING WS-SPACES FOR LEADING SPACES.               01390000
014000     COMPUTE WS-ACT-LEN = LENGTH OF STACK(FROM-STACK) - WS-SPACES.01400000
014100     COMPUTE WS-REM = WS-ACT-LEN - CRATES + 1.                    01410000
014200     MOVE SPACES        TO STACK(FROM-STACK)(WS-REM:).            01420000
014300 2200-EXIT.                                                       01430000
014400     EXIT.                                                        01440000
014500                                                                  01450000
014600 3000-DISPLAY-TOP-CRATES.                                         01460000
014700     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > 9          01470000
014800        IF FUNCTION LENGTH (FUNCTION TRIM (STACK(WS-CNT))) > 0    01480000
014900        MOVE FUNCTION REVERSE (FUNCTION TRIM(STACK(WS-CNT)))(1:1) 01490000
015000                        TO WS-TOP-STACK(WS-CNT:1)                 01500000
015100        END-IF                                                    01510000
015200     END-PERFORM.                                                 01520000
015300     DISPLAY 'Top Crates from Stacks: 'WS-TOP-STACK.              01530000
015400 3000-EXIT.                                                       01540000
015500     EXIT.                                                        01550000
