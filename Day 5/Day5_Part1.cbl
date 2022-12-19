000100*----------------------------------------------------------------*00010004
000200*          ADVENT OF CODE - DAY 5 PROGRAM 1                      *00020004
000300*----------------------------------------------------------------*00030004
000400 IDENTIFICATION DIVISION.                                         00040004
000500 PROGRAM-ID. AOCD5PG1.                                            00050004
000600 AUTHOR. z/OS Mainframer.                                         00060004
000700                                                                  00070004
000800 ENVIRONMENT DIVISION.                                            00080004
000900 INPUT-OUTPUT SECTION.                                            00090004
001000 FILE-CONTROL.                                                    00100004
001100     SELECT STACKS-DATA  ASSIGN TO AOCDAY51                       00110004
001200         ORGANIZATION    IS SEQUENTIAL                            00120004
001300          ACCESS MODE    IS SEQUENTIAL                            00130004
001400          FILE STATUS    IS STACK-FILE-STATUS.                    00140004
001500                                                                  00150004
001600     SELECT PROCESS-DATA  ASSIGN TO AOCDAY52                      00160004
001700         ORGANIZATION    IS SEQUENTIAL                            00170004
001800          ACCESS MODE    IS SEQUENTIAL                            00180004
001900          FILE STATUS    IS PROCESS-FILE-STATUS.                  00190004
002000                                                                  00200004
002100 DATA DIVISION.                                                   00210004
002200 FILE SECTION.                                                    00220004
002300 FD  STACKS-DATA                                                  00230004
002400     DATA RECORD IS STACK-DATA-REC.                               00240004
002500 01  STACK-DATA-REC.                                              00250004
002600     05 STACK-REC              PIC X(10).                         00260004
002700                                                                  00270004
002800 FD  PROCESS-DATA                                                 00280004
002900     DATA RECORD IS PROCESS-DATA-REC.                             00290004
003000 01  PROCESS-DATA-REC.                                            00300004
003100     05 PROCESS-REC            PIC X(21).                         00310004
003200                                                                  00320004
003300 WORKING-STORAGE SECTION.                                         00330004
003400 01  WS-STACKS-ARRAY.                                             00340004
003500     05 STACK                  PIC X(100)  OCCURS 9 TIMES.        00350004
003600                                                                  00360004
003700 01  WS-WORK-FIELDS.                                              00370004
003800     05 WS-HOLD-STACK          PIC X(100).                        00380004
003900     05 WS-TOP-STACK           PIC X(009).                        00390004
004000     05 WS-CNT                 PIC 9(02)   VALUE ZEROES.          00400004
004100     05 WS-UNSTRING.                                              00410004
004200        10 WS-MOVE             PIC X(04).                         00420004
004300        10 WS-CRATES           PIC X(02).                         00430004
004400        10 WS-FROM             PIC X(04).                         00440004
004500        10 WS-FROM-STACK       PIC X(02).                         00450004
004600        10 WS-TO               PIC X(02).                         00460004
004700        10 WS-TO-STACK         PIC X(02).                         00470004
004800        10 CRATES              PIC 9(02).                         00480004
004900        10 FROM-STACK          PIC 9(02).                         00490004
005000        10 TO-STACK            PIC 9(02).                         00500004
005100     05 WS-CALC-FIELDS.                                           00510004
005200        10 WS-ACT-LEN          PIC 9(02).                         00520004
005300        10 WS-SPACES           PIC 9(02).                         00530004
005400        10 WS-REM              PIC 9(02).                         00540004
005500                                                                  00550004
005600 01  WS-SWITCHES.                                                 00560004
005700     05 STACK-FILE-STATUS      PIC X(02)   VALUE SPACES.          00570004
005800        88 STACK-SUCCESS       VALUE '00'.                        00580004
005900        88 STACK-EOF           VALUE '10'.                        00590004
006000                                                                  00600004
006100     05 PROCESS-FILE-STATUS    PIC X(02)   VALUE SPACES.          00610004
006200        88 PROCESS-SUCCESS     VALUE '00'.                        00620004
006300        88 PROCESS-EOF         VALUE '10'.                        00630004
006400                                                                  00640004
006500 PROCEDURE DIVISION.                                              00650004
006600                                                                  00660004
006700     INITIALIZE WS-STACKS-ARRAY.                                  00670004
006800     PERFORM 1000-READ-STACKS-DATA       THRU 1000-EXIT.          00680004
006900     PERFORM 2000-READ-PROCESS-DATA      THRU 2000-EXIT.          00690004
007000     PERFORM 3000-DISPLAY-TOP-CRATES     THRU 3000-EXIT.          00700004
007100                                                                  00710004
007200     STOP RUN.                                                    00720004
007300                                                                  00730004
007400 1000-READ-STACKS-DATA.                                           00740004
007500     OPEN INPUT STACKS-DATA.                                      00750004
007600     IF STACK-SUCCESS                                             00760004
007700        PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL STACK-EOF        00770004
007800           READ STACKS-DATA                                       00780004
007900             AT END                                               00790004
008000                CLOSE STACKS-DATA                                 00800004
008100                SET STACK-EOF       TO TRUE                       00810004
008200             NOT AT END                                           00820004
008300                MOVE STACK-REC      TO STACK(WS-CNT)              00830004
008400           END-READ                                               00840004
008500        END-PERFORM                                               00850004
008600     END-IF.                                                      00860004
008700 1000-EXIT.                                                       00870004
008800     EXIT.                                                        00880004
008900                                                                  00890004
009000 2000-READ-PROCESS-DATA.                                          00900004
009100     OPEN INPUT PROCESS-DATA.                                     00910004
009200     IF PROCESS-SUCCESS                                           00920004
009300        PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL PROCESS-EOF      00930004
009400           READ PROCESS-DATA                                      00940004
009500             AT END                                               00950004
009600                CLOSE PROCESS-DATA                                00960004
009700                SET PROCESS-EOF     TO TRUE                       00970004
009800             NOT AT END                                           00980004
009900                PERFORM 2100-GET-PROCESS   THRU 2100-EXIT         00990004
010000                PERFORM 2200-MOVE-CRATES   THRU 2200-EXIT         01000004
010100           END-READ                                               01010004
010200        END-PERFORM                                               01020004
010300     END-IF.                                                      01030004
010400 2000-EXIT.                                                       01040004
010500     EXIT.                                                        01050004
010600                                                                  01060004
010700 2100-GET-PROCESS.                                                01070004
010800     INITIALIZE WS-UNSTRING.                                      01080004
010900     UNSTRING PROCESS-REC DELIMITED BY SPACE                      01090004
011000                          INTO WS-MOVE                            01100004
011100                               WS-CRATES                          01110004
011200                               WS-FROM                            01120004
011300                               WS-FROM-STACK                      01130004
011400                               WS-TO                              01140004
011500                               WS-TO-STACK.                       01150004
011600     MOVE FUNCTION TRIM(WS-CRATES)         TO CRATES.             01160004
011700     MOVE FUNCTION TRIM(WS-FROM-STACK)     TO FROM-STACK.         01170004
011800     MOVE FUNCTION TRIM(WS-TO-STACK)       TO TO-STACK.           01180004
011900 2100-EXIT.                                                       01190004
012000     EXIT.                                                        01200004
012100                                                                  01210004
012200 2200-MOVE-CRATES.                                                01220004
012300     MOVE ZEROES        TO WS-ACT-LEN                             01230004
012400                           WS-SPACES.                             01240004
012500     INSPECT FUNCTION REVERSE(STACK(TO-STACK))                    01250004
012600             TALLYING WS-SPACES FOR LEADING SPACES.               01260004
012700                                                                  01270004
012800     COMPUTE WS-ACT-LEN = LENGTH OF STACK(TO-STACK) -             01280004
012900                          WS-SPACES + 1.                          01290004
013000     MOVE SPACES        TO WS-HOLD-STACK.                         01300004
013100     MOVE FUNCTION REVERSE(FUNCTION TRIM(STACK(FROM-STACK)))      01310004
013200                        TO WS-HOLD-STACK.                         01320004
013300     MOVE WS-HOLD-STACK(1:CRATES)                                 01330004
013400                        TO STACK(TO-STACK)(WS-ACT-LEN:CRATES).    01340004
013500     MOVE ZEROES        TO WS-ACT-LEN                             01350004
013600                           WS-SPACES                              01360004
013700                           WS-REM.                                01370004
013800     INSPECT FUNCTION REVERSE(STACK(FROM-STACK))                  01380004
013900             TALLYING WS-SPACES FOR LEADING SPACES.               01390004
014000     COMPUTE WS-ACT-LEN = LENGTH OF STACK(FROM-STACK) - WS-SPACES.01400004
014100     COMPUTE WS-REM = WS-ACT-LEN - CRATES + 1.                    01410004
014200     MOVE SPACES        TO STACK(FROM-STACK)(WS-REM:).            01420004
014300 2200-EXIT.                                                       01430004
014400     EXIT.                                                        01440004
014500                                                                  01450004
014600 3000-DISPLAY-TOP-CRATES.                                         01460004
014700     PERFORM VARYING WS-CNT FROM 1 BY 1 UNTIL WS-CNT > 9          01470004
014800        IF FUNCTION LENGTH (FUNCTION TRIM (STACK(WS-CNT))) > 0    01480004
014900        MOVE FUNCTION REVERSE (FUNCTION TRIM(STACK(WS-CNT)))(1:1) 01490004
015000                        TO WS-TOP-STACK(WS-CNT:1)                 01500004
015100        END-IF                                                    01510004
015200     END-PERFORM.                                                 01520004
015300     DISPLAY 'Top Crates from Stacks: 'WS-TOP-STACK.              01530004
015400 3000-EXIT.                                                       01540004
015500     EXIT.                                                        01550004
