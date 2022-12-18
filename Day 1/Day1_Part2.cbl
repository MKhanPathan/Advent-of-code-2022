000100*----------------------------------------------------------------*00010000
000200*          ADVENT OF CODE - DAY 1 PROGRAM 2                      *00020013
000300*----------------------------------------------------------------*00030000
000400 IDENTIFICATION DIVISION.                                         00040000
000500 PROGRAM-ID. AOCD1PG2.                                            00050001
000600 AUTHOR. z/OS Mainframer.                                         00060000
000700                                                                  00070000
000800 ENVIRONMENT DIVISION.                                            00080000
000900 INPUT-OUTPUT SECTION.                                            00090000
001000 FILE-CONTROL.                                                    00100000
001010*----------------------------------------------------------------*00101014
001020* ASSIGNING THE LOGICAL FILE NAME TO BE REFERRED IN THE PROCESS  *00102014
001030*----------------------------------------------------------------*00103014
001100     SELECT CAL-DATA        ASSIGN TO AOCDAY1                     00110000
001200     ORGANIZATION IS SEQUENTIAL                                   00120000
001300     ACCESS MODE IS SEQUENTIAL                                    00130000
001400     FILE STATUS IS FILE-STATUS.                                  00140000
001500                                                                  00150000
001600 DATA DIVISION.                                                   00160000
001700 FILE SECTION.                                                    00170000
001710*----------------------------------------------------------------*00171015
001720* FILE DISCRIPTION LAYOUT FOR READING THE INPUT CALORIES DATA    *00172015
001730*----------------------------------------------------------------*00173015
001800 FD  CAL-DATA.                                                    00180000
001900 01  WS-CAL-REC.                                                  00190000
002000     05 CALORIES            PIC X(10).                            00200000
002100                                                                  00210000
002200 WORKING-STORAGE SECTION.                                         00220000
002300*----------------------------------------------------------------*00230015
002400* FIELDS TO TRACK THE TOTAL CALORIES OF EACH ELF AND THE HIGHEST *00240015
002401* CALORIES BY 3 ELFS                                             *00240115
002410*----------------------------------------------------------------*00241015
002500 01  WS-WORK-FIELDS.                                              00250000
002600     05 TOTAL-CALORIES      PIC 9(10)   VALUE ZEROES.             00260000
002700     05 CALORIES-NUM        PIC 9(10).                            00270000
002801     05 HIGHEST-CAL1        PIC 9(10)   VALUE ZEROES.             00280100
002810     05 HIGHEST-CAL2        PIC 9(10)   VALUE ZEROES.             00281000
002820     05 HIGHEST-CAL3        PIC 9(10)   VALUE ZEROES.             00282000
002900                                                                  00290000
002910*----------------------------------------------------------------*00291015
002920* FIELDS TO TRACK THE STATUS OF THE CALORIES FILE AFTER OPEN     *00292015
002930*----------------------------------------------------------------*00293015
003000 01  WS-FILE-STATUS.                                              00300000
003100     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00310000
003200        88 SUCCESS          VALUE '00'.                           00320000
003300        88 EOF              VALUE '10'.                           00330000
003400                                                                  00340000
003500 PROCEDURE DIVISION.                                              00350000
003600                                                                  00360000
003700     PERFORM 1000-OPEN-FILE                 THRU 1000-EXIT.       00370015
003710     PERFORM 4000-DISP-CALORIES-CLOSE       THRU 4000-EXIT.       00371015
003900                                                                  00390000
004000     STOP RUN.                                                    00400000
004100                                                                  00410000
004200 1000-OPEN-FILE.                                                  00420000
004210*----------------------------------------------------------------*00421015
004220* OPEN THE CALORIES FILE IN INPUT MODE AND READ THE DATA UNTIL   *00422015
004230* END OF FILE                                                    *00423015
004240*----------------------------------------------------------------*00424015
004300     OPEN INPUT CAL-DATA.                                         00430000
004400     IF SUCCESS                                                   00440000
004500        PERFORM 2000-READ-CALORIES          THRU 2000-EXIT        00450015
004600          UNTIL EOF                                               00460000
004700     END-IF.                                                      00470000
004800 1000-EXIT.                                                       00480000
004900     EXIT.                                                        00490000
005000                                                                  00500000
005100 2000-READ-CALORIES.                                              00510000
005110*----------------------------------------------------------------*00511015
005120* READ THE DATA AND PERFORM THE CALCULATE PARA TO FIND THE SUM   *00512015
005130*----------------------------------------------------------------*00513015
005200     READ CAL-DATA                                                00520015
005300          AT END                                                  00530000
005401             INITIALIZE CALORIES                                  00540115
005410             PERFORM 3500-FIND-HIGHEST-CAL  THRU 3500-EXIT        00541015
005500          NOT AT END                                              00550000
005600             PERFORM 3000-CALC-CALORIES     THRU 3000-EXIT        00560015
005700     END-READ.                                                    00570000
005800 2000-EXIT.                                                       00580000
005900     EXIT.                                                        00590000
006000                                                                  00600000
006100 3000-CALC-CALORIES.                                              00610000
006110*----------------------------------------------------------------*00611015
006120* DEFINED THE INPUT RECORD OF LENGTH=X(10) AND THERE MAY BE DATA *00612015
006130* CALORIES NUMBER LESS THAN LENGTH 10. SO REMOVING THE TRAILING  *00613015
006140* SPACES AND STORING IN THE NUMBER VARIABLE AND ADDING UP ALL THE*00614015
006150* CALORIES UNTIL WE FIND THE NEXT ELF. ONCE WE FIND THE NEXT ELFS*00615015
006160* CALORIES, COMPARE WITH PREVIOUS ONES TO TRACK THE HIGHEST 3    *00616015
006161* ELFS CALORIES TO GET THE DESIRED CALORIES NUMBER               *00616115
006170*----------------------------------------------------------------*00617015
006200     IF CALORIES NOT EQUAL SPACES                                 00620015
006300        INITIALIZE               CALORIES-NUM                     00630000
006400        MOVE FUNCTION TRIM(CALORIES)                              00640015
006500                                 TO CALORIES-NUM                  00650000
006600        ADD CALORIES-NUM         TO TOTAL-CALORIES                00660000
006700     ELSE                                                         00670000
006701        PERFORM 3500-FIND-HIGHEST-CAL    THRU 3500-EXIT           00670110
007100        INITIALIZE               TOTAL-CALORIES                   00710000
007200     END-IF.                                                      00720000
007300 3000-EXIT.                                                       00730000
007400     EXIT.                                                        00740000
007410                                                                  00741010
007420 3500-FIND-HIGHEST-CAL.                                           00742010
007421*----------------------------------------------------------------*00742115
007422* ONCE FIND THE NEXT ELF, COMPARING THE CALORIES OF AN ELF WITH  *00742215
007423* TRACKED 3 HIGHEST CALORIES TO GET ONLY THE HIGHEST CALORIES    *00742315
007424*----------------------------------------------------------------*00742415
007430                                                                  00743010
007435     IF TOTAL-CALORIES > HIGHEST-CAL1 OR HIGHEST-CAL2 OR          00743510
007436                         HIGHEST-CAL3                             00743610
007437        IF TOTAL-CALORIES > HIGHEST-CAL3                          00743710
007438           IF TOTAL-CALORIES > HIGHEST-CAL2                       00743810
007439              IF TOTAL-CALORIES > HIGHEST-CAL1                    00743910
007440                 MOVE HIGHEST-CAL1   TO HIGHEST-CAL2              00744010
007441                 MOVE HIGHEST-CAL2   TO HIGHEST-CAL3              00744110
007442                 MOVE TOTAL-CALORIES TO HIGHEST-CAL1              00744210
007443              ELSE                                                00744310
007444                 MOVE HIGHEST-CAL2   TO HIGHEST-CAL3              00744410
007445                 MOVE TOTAL-CALORIES TO HIGHEST-CAL2              00744510
007446              END-IF                                              00744610
007447           ELSE                                                   00744710
007448              MOVE TOTAL-CALORIES TO HIGHEST-CAL3                 00744810
007449           END-IF                                                 00744910
007450        END-IF                                                    00745010
007451                                                                  00745110
007452     END-IF.                                                      00745210
007453 3500-EXIT.                                                       00745310
007460     EXIT.                                                        00746010
007500                                                                  00750000
007600 4000-DISP-CALORIES-CLOSE.                                        00760015
007601*----------------------------------------------------------------*00760115
007602* CLOSE THE INPUT CALORIES FILE AT THE END, COMPUTE THE TOTAL    *00760215
007603* CALORIES BY THE HIGHEST 3 ELFS AND DISPLAY THE TOTAL CALORIES  *00760315
007604*----------------------------------------------------------------*00760415
007605     CLOSE CAL-DATA.                                              00760515
007606     INITIALIZE TOTAL-CALORIES.                                   00760600
007610     COMPUTE TOTAL-CALORIES = HIGHEST-CAL1 +                      00761000
007620                              HIGHEST-CAL2 +                      00762000
007630                              HIGHEST-CAL3                        00763000
007640                                                                  00764000
007700     DISPLAY 'HIGHEST CALORIES: ' TOTAL-CALORIES.                 00770000
007800 4000-EXIT.                                                       00780000
007900     EXIT.                                                        00790000
