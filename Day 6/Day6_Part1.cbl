000100*----------------------------------------------------------------*00010003
000200*          ADVENT OF CODE - DAY 6 PROGRAM 1                      *00020003
000300*----------------------------------------------------------------*00030003
000400 IDENTIFICATION DIVISION.                                         00040003
000500 PROGRAM-ID. AOCD6PG1.                                            00050003
000600 AUTHOR. z/OS Mainframer.                                         00060003
000700                                                                  00070003
000800 ENVIRONMENT DIVISION.                                            00080003
000900 INPUT-OUTPUT SECTION.                                            00090003
001000 FILE-CONTROL.                                                    00100003
001100     SELECT DATA-STREAM  ASSIGN TO AOCDAY6                        00110003
001200         ORGANIZATION    IS SEQUENTIAL                            00120003
001300          ACCESS MODE    IS SEQUENTIAL                            00130003
001400          FILE STATUS    IS FILE-STATUS.                          00140003
001500                                                                  00150003
001600 DATA DIVISION.                                                   00160003
001700 FILE SECTION.                                                    00170003
001800 FD  DATA-STREAM                                                  00180003
001900     DATA RECORD IS DATA-STREAM-BUFFER.                           00190003
002000 01  DATA-STREAM-BUFFER.                                          00200003
002100     05 DATA-STREAM-REC        PIC X(4095).                       00210003
002200                                                                  00220003
002300 WORKING-STORAGE SECTION.                                         00230003
002400 01  WS-WORK-FIELDS.                                              00240003
002500     05 WS-CNT                 PIC 9(05)   VALUE ZEROES.          00250003
002600     05 WS-SRCH                PIC 9(05)   VALUE ZEROES.          00260003
002700     05 WS-MTCH                PIC 9(05)   VALUE ZEROES.          00270003
002800     05 WS-SPACES              PIC 9(05)   VALUE ZEROES.          00280003
002900     05 WS-MARKER-ST           PIC 9(05)   VALUE ZEROES.          00290003
003000     05 WS-DATA-LEN            PIC 9(05)   VALUE ZEROES.          00300003
003100                                                                  00310003
003200 01  WS-FOUR-CHARS.                                               00320003
003300     05 CHAR                   PIC X(01)   OCCURS 4 TIMES.        00330003
003400                                                                  00340003
003500 01  WS-SWITCHES.                                                 00350003
003600     05 FILE-STATUS            PIC X(02)   VALUE SPACES.          00360003
003700        88 SUCCESS             VALUE '00'.                        00370003
003800        88 EOF                 VALUE '10'.                        00380003
003900                                                                  00390003
004000     05 DUP-STAUS              PIC X(01)   VALUE SPACES.          00400003
004100        88 DUP-FOUND           VALUE 'Y'.                         00410003
004200        88 NO-DUP-FOUND        VALUE 'N'.                         00420003
004300                                                                  00430003
004400 PROCEDURE DIVISION.                                              00440003
004500                                                                  00450003
004600     PERFORM 1000-READ-DATA-STREAM       THRU 1000-EXIT.          00460003
004700                                                                  00470003
004800     STOP RUN.                                                    00480003
004900                                                                  00490003
005000 1000-READ-DATA-STREAM.                                           00500003
005100     OPEN INPUT DATA-STREAM.                                      00510003
005200     IF SUCCESS                                                   00520003
005300        PERFORM UNTIL EOF                                         00530003
005400           READ DATA-STREAM                                       00540003
005500             AT END                                               00550003
005600                CONTINUE                                          00560003
005700            NOT AT END                                            00570003
005800                PERFORM 1100-FIND-FIRST-MARKER  THRU 1100-EXIT    00580003
005900                DISPLAY 'Chars to be processed: ' WS-MARKER-ST    00590003
006000           END-READ                                               00600003
006100        END-PERFORM                                               00610003
006200     END-IF                                                       00620003
006300     CLOSE DATA-STREAM.                                           00630003
006400 1000-EXIT.                                                       00640003
006500     EXIT.                                                        00650003
006600                                                                  00660003
006700 1100-FIND-FIRST-MARKER.                                          00670003
006800     INITIALIZE WS-SRCH                                           00680003
006900                WS-MTCH                                           00690003
007000                WS-SPACES                                         00700003
007100                WS-FOUR-CHARS.                                    00710003
007200     INSPECT FUNCTION REVERSE(DATA-STREAM-REC)                    00720003
007300                      TALLYING WS-SPACES                          00730003
007400                      FOR LEADING SPACES.                         00740003
007500     COMPUTE WS-DATA-LEN = LENGTH OF DATA-STREAM-REC - WS-SPACES. 00750003
007600     PERFORM VARYING WS-CNT FROM 1 BY 1                           00760003
007700               UNTIL WS-CNT > WS-DATA-LEN                         00770003
007800         SET NO-DUP-FOUND TO TRUE                                 00780003
007900         MOVE DATA-STREAM-REC(WS-CNT:4) TO WS-FOUR-CHARS          00790003
008000         PERFORM VARYING WS-SRCH FROM 1 BY 1                      00800003
008100                   UNTIL WS-SRCH > 4 OR DUP-FOUND                 00810003
008200             PERFORM VARYING WS-MTCH FROM 1 BY 1                  00820003
008300                       UNTIL WS-MTCH > 4 OR DUP-FOUND             00830003
008400                  IF CHAR(WS-SRCH) = CHAR(WS-MTCH)                00840003
008500                     AND WS-SRCH NOT = WS-MTCH                    00850003
008600                     SET DUP-FOUND TO TRUE                        00860003
008700                  END-IF                                          00870003
008800             END-PERFORM                                          00880003
008900         END-PERFORM                                              00890003
009000         IF NO-DUP-FOUND                                          00900003
009100            MOVE ZEROES            TO WS-MARKER-ST                00910003
009200            COMPUTE WS-MARKER-ST = WS-CNT + 3                     00920003
009300            ADD WS-DATA-LEN        TO WS-CNT                      00930003
009400         END-IF                                                   00940003
009500     END-PERFORM.                                                 00950003
009600 1100-EXIT.                                                       00960003
009700     EXIT.                                                        00970003
