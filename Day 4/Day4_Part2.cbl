000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 4 PROGRAM 2                      *00020001
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD4PG2.                                            00050001
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001100     SELECT PAIR-RANGE   ASSIGN TO AOCDAY4                        00110000
001200         ORGANIZATION    IS SEQUENTIAL                            00120000
001300          ACCESS MODE    IS SEQUENTIAL                            00130000
001400          FILE STATUS    IS FILE-STATUS.                          00140000
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001800 FD  PAIR-RANGE.                                                  00180000
001900 01  WS-RANGE.                                                    00190000
002000     05 RANGE-REC              PIC X(11).                         00200000
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002300 01  WS-WORK-FIELDS.                                              00230000
002400     05 TOTAL-PAIRS            PIC 9(05)   VALUE ZEROES.          00240000
002600     05 WS-CNT                 PIC 9(02)   VALUE ZEROES.          00260000
002610     05 WS-H-RANGE-DATA.                                          00261000
002700        10 WS-H-RANGE-REC      PIC X(11)   VALUE SPACES.          00270000
002800        10 WS-FST-RANGE-ST     PIC X(02)   VALUE SPACES.          00280000
002900        10 WS-FST-RANGE-ED     PIC X(02)   VALUE SPACES.          00290000
002910        10 WS-NXT-RANGE-ST     PIC X(02)   VALUE SPACES.          00291000
002920        10 WS-NXT-RANGE-ED     PIC X(02)   VALUE SPACES.          00292000
002930        10 WS-FST-RANGE-ST-N   PIC 9(02)   VALUE ZEROES.          00293000
002940        10 WS-FST-RANGE-ED-N   PIC 9(02)   VALUE ZEROES.          00294000
002950        10 WS-NXT-RANGE-ST-N   PIC 9(02)   VALUE ZEROES.          00295000
002960        10 WS-NXT-RANGE-ED-N   PIC 9(02)   VALUE ZEROES.          00296000
003000                                                                  00300000
006200 01  WS-SWITCHES.                                                 00620000
006300     05 FILE-STATUS            PIC X(02)   VALUE SPACES.          00630000
006400        88 SUCCESS             VALUE '00'.                        00640000
006500        88 EOF                 VALUE '10'.                        00650000
006900                                                                  00690000
007000 PROCEDURE DIVISION.                                              00700000
007100                                                                  00710000
007200     OPEN INPUT PAIR-RANGE.                                       00720000
007300     IF SUCCESS                                                   00730000
007400        PERFORM UNTIL EOF                                         00740000
007500           READ PAIR-RANGE                                        00750000
007600                AT END                                            00760000
007700                   CONTINUE                                       00770000
007800                NOT AT END                                        00780000
007901                   INITIALIZE               WS-H-RANGE-DATA       00790100
007910                   MOVE RANGE-REC           TO WS-H-RANGE-REC     00791000
008000                   PERFORM 2000-GET-RANGE   THRU 2000-EXIT        00800000
008010                   PERFORM 3000-COUNT-PAIRS THRU 3000-EXIT        00801000
008100           END-READ                                               00810000
008200        END-PERFORM                                               00820000
008300                                                                  00830000
008400        CLOSE PAIR-RANGE                                          00840000
008500        DISPLAY 'Total Pairs: ' TOTAL-PAIRS                       00850001
008600     END-IF.                                                      00860000
008700                                                                  00870000
008800     STOP RUN.                                                    00880000
008900                                                                  00890000
009000 2000-GET-RANGE.                                                  00900000
009010     UNSTRING WS-H-RANGE-REC  DELIMITED BY '-' OR ','             00901000
009020         INTO WS-FST-RANGE-ST                                     00902000
009030              WS-FST-RANGE-ED                                     00903000
009040              WS-NXT-RANGE-ST                                     00904000
009050              WS-NXT-RANGE-ED                                     00905000
009100                                                                  00910000
009101     MOVE FUNCTION TRIM(WS-FST-RANGE-ST)   TO WS-FST-RANGE-ST-N   00910100
009102     MOVE FUNCTION TRIM(WS-FST-RANGE-ED)   TO WS-FST-RANGE-ED-N   00910200
009103     MOVE FUNCTION TRIM(WS-NXT-RANGE-ST)   TO WS-NXT-RANGE-ST-N   00910300
009104     MOVE FUNCTION TRIM(WS-NXT-RANGE-ED)   TO WS-NXT-RANGE-ED-N.  00910400
009200 2000-EXIT.                                                       00920000
009300     EXIT.                                                        00930000
009400                                                                  00940000
009500 3000-COUNT-PAIRS.                                                00950000
009520     IF ( WS-FST-RANGE-ST-N  <= WS-NXT-RANGE-ST-N OR              00952002
009521          WS-FST-RANGE-ST-N  <= WS-NXT-RANGE-ED-N ) AND           00952102
009530        ( WS-FST-RANGE-ED-N  >= WS-NXT-RANGE-ST-N OR              00953002
009540          WS-FST-RANGE-ED-N  >= WS-NXT-RANGE-ST-N )               00954002
009542        ADD 1 TO TOTAL-PAIRS                                      00954200
009550     ELSE                                                         00955000
009551        IF ( WS-FST-RANGE-ST-N  >= WS-NXT-RANGE-ST-N OR           00955102
009552             WS-FST-RANGE-ST-N  >= WS-NXT-RANGE-ED-N ) AND        00955202
009553           ( WS-FST-RANGE-ED-N  <= WS-NXT-RANGE-ST-N OR           00955302
009554             WS-FST-RANGE-ED-N  <= WS-NXT-RANGE-ST-N )            00955402
009555           ADD 1 TO TOTAL-PAIRS                                   00955502
009600        END-IF                                                    00960000
009610     END-IF.                                                      00961000
009700 3000-EXIT.                                                       00970000
009800     EXIT.                                                        00980000
