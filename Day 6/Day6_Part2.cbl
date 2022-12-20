000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 6 PROGRAM 2                      *00020000
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD6PG2.                                            00050000
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT DATA-STREAM  ASSIGN TO AOCDAY6                        00110000
001200         ORGANIZATION    IS SEQUENTIAL                            00120000
001300          ACCESS MODE    IS SEQUENTIAL                            00130000
001400          FILE STATUS    IS FILE-STATUS.                          00140000
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  DATA-STREAM                                                  00180000
001900     DATA RECORD IS DATA-STREAM-BUFFER.                           00190000
002000 01  DATA-STREAM-BUFFER.                                          00200000
002100     05 DATA-STREAM-REC        PIC X(4095).                       00210000
002200                                                                  00220000
002300 WORKING-STORAGE SECTION.                                         00230000
002400 01  WS-WORK-FIELDS.                                              00240000
002500     05 WS-CNT                 PIC 9(05)   VALUE ZEROES.          00250000
002600     05 WS-SRCH                PIC 9(05)   VALUE ZEROES.          00260000
002700     05 WS-MTCH                PIC 9(05)   VALUE ZEROES.          00270000
002800     05 WS-SPACES              PIC 9(05)   VALUE ZEROES.          00280000
002900     05 WS-MARKER-ST           PIC 9(05)   VALUE ZEROES.          00290000
003000     05 WS-DATA-LEN            PIC 9(05)   VALUE ZEROES.          00300000
003100                                                                  00310000
003200 01  WS-FOURTEEN-CHARS.                                           00320000
003300     05 CHAR                   PIC X(01)   OCCURS 14 TIMES.       00330000
003400                                                                  00340000
003500 01  WS-SWITCHES.                                                 00350000
003600     05 FILE-STATUS            PIC X(02)   VALUE SPACES.          00360000
003700        88 SUCCESS             VALUE '00'.                        00370000
003800        88 EOF                 VALUE '10'.                        00380000
003900                                                                  00390000
004000     05 DUP-STAUS              PIC X(01)   VALUE SPACES.          00400000
004100        88 DUP-FOUND           VALUE 'Y'.                         00410000
004200        88 NO-DUP-FOUND        VALUE 'N'.                         00420000
004300                                                                  00430000
004400 PROCEDURE DIVISION.                                              00440000
004500                                                                  00450000
004600     PERFORM 1000-READ-DATA-STREAM       THRU 1000-EXIT.          00460000
004700                                                                  00470000
004800     STOP RUN.                                                    00480000
004900                                                                  00490000
005000 1000-READ-DATA-STREAM.                                           00500000
005100     OPEN INPUT DATA-STREAM.                                      00510000
005200     IF SUCCESS                                                   00520000
005300        PERFORM UNTIL EOF                                         00530000
005400           READ DATA-STREAM                                       00540000
005500             AT END                                               00550000
005600                CONTINUE                                          00560000
005700            NOT AT END                                            00570000
005800                PERFORM 1100-FIND-FIRST-MARKER  THRU 1100-EXIT    00580000
005900                DISPLAY 'Chars to be processed: ' WS-MARKER-ST    00590000
006000           END-READ                                               00600000
006100        END-PERFORM                                               00610000
006200     END-IF                                                       00620000
006300     CLOSE DATA-STREAM.                                           00630000
006400 1000-EXIT.                                                       00640000
006500     EXIT.                                                        00650000
006600                                                                  00660000
006700 1100-FIND-FIRST-MARKER.                                          00670000
006800     INITIALIZE WS-SRCH                                           00680000
006900                WS-MTCH                                           00690000
007000                WS-SPACES                                         00700000
007100                WS-FOURTEEN-CHARS.                                00710001
007200     INSPECT FUNCTION REVERSE(DATA-STREAM-REC)                    00720000
007300                      TALLYING WS-SPACES                          00730000
007400                      FOR LEADING SPACES.                         00740000
007500     COMPUTE WS-DATA-LEN = LENGTH OF DATA-STREAM-REC - WS-SPACES. 00750000
007600     PERFORM VARYING WS-CNT FROM 1 BY 1                           00760000
007700               UNTIL WS-CNT > WS-DATA-LEN                         00770000
007800         SET NO-DUP-FOUND TO TRUE                                 00780000
007900         MOVE DATA-STREAM-REC(WS-CNT:14) TO WS-FOURTEEN-CHARS     00790000
008000         PERFORM VARYING WS-SRCH FROM 1 BY 1                      00800000
008100                   UNTIL WS-SRCH > 14 OR DUP-FOUND                00810000
008200             PERFORM VARYING WS-MTCH FROM 1 BY 1                  00820000
008300                       UNTIL WS-MTCH > 14 OR DUP-FOUND            00830000
008400                  IF CHAR(WS-SRCH) = CHAR(WS-MTCH)                00840000
008500                     AND WS-SRCH NOT = WS-MTCH                    00850000
008600                     SET DUP-FOUND TO TRUE                        00860000
008700                  END-IF                                          00870000
008800             END-PERFORM                                          00880000
008900         END-PERFORM                                              00890000
009000         IF NO-DUP-FOUND                                          00900000
009100            MOVE ZEROES            TO WS-MARKER-ST                00910000
009200            COMPUTE WS-MARKER-ST = WS-CNT + 13                    00920000
009300            ADD WS-DATA-LEN        TO WS-CNT                      00930000
009400         END-IF                                                   00940000
009500     END-PERFORM.                                                 00950000
009600 1100-EXIT.                                                       00960000
009700     EXIT.                                                        00970000
