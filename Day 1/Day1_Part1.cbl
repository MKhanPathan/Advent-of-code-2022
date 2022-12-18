000100*----------------------------------------------------------------*00010001
000200*          ADVENT OF CODE - DAY 1 PROGRAM 1                      *00020001
000300*----------------------------------------------------------------*00030001
000400 IDENTIFICATION DIVISION.                                         00040001
000500 PROGRAM-ID. AOCD1PG1.                                            00050001
000600 AUTHOR. z/OS Mainframer.                                         00060001
000700                                                                  00070001
000710 ENVIRONMENT DIVISION.                                            00071001
000720 INPUT-OUTPUT SECTION.                                            00072001
000730 FILE-CONTROL.                                                    00073001
000731*----------------------------------------------------------------*00073117
000732* ASSIGNING THE LOGICAL FILE NAME TO BE REFERRED IN THE PROCESS  *00073217
000733*----------------------------------------------------------------*00073317
000740     SELECT CAL-DATA    ASSIGN TO AOCDAY1                         00074016
000750        ORGANIZATION    IS SEQUENTIAL                             00075016
000760         ACCESS MODE    IS SEQUENTIAL                             00076016
000770         FILE STATUS    IS FILE-STATUS.                           00077016
000780                                                                  00078001
000800 DATA DIVISION.                                                   00080001
000810 FILE SECTION.                                                    00081001
000811*----------------------------------------------------------------*00081117
000812* FILE DISCRIPTION LAYOUT FOR READING THE INPUT CALORIES DATA    *00081217
000813*----------------------------------------------------------------*00081317
000820 FD  CAL-DATA.                                                    00082001
000830 01  WS-CAL-REC.                                                  00083001
000840     05 CALORIES            PIC X(10).                            00084011
000850                                                                  00085001
000900 WORKING-STORAGE SECTION.                                         00090001
000901*----------------------------------------------------------------*00090117
000902* FIELDS TO TRACK THE TOTAL CALORIES OF EACH ELF AND THE HIGHEST *00090217
000903*----------------------------------------------------------------*00090317
000910 01  WS-WORK-FIELDS.                                              00091001
000930     05 TOTAL-CALORIES      PIC 9(10)   VALUE ZEROES.             00093001
000931     05 CALORIES-NUM        PIC 9(10).                            00093108
000940     05 HIGHEST-CALORIES    PIC 9(10)   VALUE ZEROES.             00094001
000950                                                                  00095001
000951*----------------------------------------------------------------*00095117
000952* FIELDS TO TRACK THE STATUS OF THE CALORIES FILE AFTER OPEN     *00095217
000953*----------------------------------------------------------------*00095317
000960 01  WS-SWITCHES.                                                 00096016
000970     05 FILE-STATUS         PIC X(02)   VALUE SPACES.             00097001
000980        88 SUCCESS          VALUE '00'.                           00098001
001000        88 EOF              VALUE '10'.                           00100001
001100                                                                  00110001
001200 PROCEDURE DIVISION.                                              00120001
001300                                                                  00130001
001400     PERFORM 1000-OPEN-FILE            THRU 1000-EXIT.            00140016
001410     PERFORM 4000-DISP-CALORIES-CLOSE  THRU 4000-EXIT.            00141016
001500                                                                  00150001
001600     STOP RUN.                                                    00160001
001700                                                                  00170001
001800 1000-OPEN-FILE.                                                  00180001
001810*----------------------------------------------------------------*00181017
001820* OPEN THE CALORIES FILE IN INPUT MODE AND READ THE DATA UNTIL   *00182017
001830* END OF FILE                                                    *00183017
001840*----------------------------------------------------------------*00184017
001900     OPEN INPUT CAL-DATA.                                         00190001
002000     IF SUCCESS                                                   00200002
002300        PERFORM 2000-READ-CALORIES     THRU 2000-EXIT             00230001
002400          UNTIL EOF                                               00240001
002500     END-IF.                                                      00250003
002600 1000-EXIT.                                                       00260001
002700     EXIT.                                                        00270001
002800                                                                  00280001
002900 2000-READ-CALORIES.                                              00290001
002910*----------------------------------------------------------------*00291017
002920* READ THE DATA AND PERFORM THE CALCULATE PARA TO FIIND THE SUM  *00292017
002940*----------------------------------------------------------------*00294017
003000     READ CAL-DATA                                                00300016
003100          AT END                                                  00310001
003200             INITIALIZE CALORIES                                  00320016
003210             PERFORM 3000-CALC-CALORIES  THRU 3000-EXIT           00321016
003300          NOT AT END                                              00330001
003400             PERFORM 3000-CALC-CALORIES  THRU 3000-EXIT           00340001
003500     END-READ.                                                    00350003
003600 2000-EXIT.                                                       00360001
003700     EXIT.                                                        00370001
003800                                                                  00380001
003900 3000-CALC-CALORIES.                                              00390001
003910*----------------------------------------------------------------*00391017
003920* DEFINED THE INPUT RECORD OF LENGTH=X(10) AND THERE MAY BE DATA *00392017
003930* CALORIES NUMBER LESS THAN LENGTH 10. SO REMOVING THE TRAILING  *00393017
003931* SPACES AND STORING IN THE NUMBER VARIABLE AND ADDING UP ALL THE*00393117
003932* CALORIES UNTIL WE FIND THE NEXT ELF. ONCE WE FIND THE NEXT ELFS*00393217
003933* CALORIES, COMPARE WITH PREVIOUS ONE TRACK THE HIGHEST CALORIES *00393317
003940*----------------------------------------------------------------*00394017
004000     IF CALORIES NOT EQUAL SPACES                                 00400016
004100        INITIALIZE               CALORIES-NUM                     00410002
004110        MOVE FUNCTION TRIM(CALORIES)                              00411016
004120                                 TO CALORIES-NUM                  00412015
004200        ADD CALORIES-NUM         TO TOTAL-CALORIES                00420002
004300     ELSE                                                         00430001
004400        IF TOTAL-CALORIES > HIGHEST-CALORIES                      00440001
004500           MOVE TOTAL-CALORIES   TO HIGHEST-CALORIES              00450001
004600        END-IF                                                    00460001
004610        INITIALIZE               TOTAL-CALORIES                   00461002
004700     END-IF.                                                      00470001
004800 3000-EXIT.                                                       00480001
004900     EXIT.                                                        00490001
005000                                                                  00500001
005100 4000-DISP-CALORIES-CLOSE.                                        00510016
005101*----------------------------------------------------------------*00510117
005102* CLOSE THE INPUT CALORIES FILE AT THE END AND DISPLAY HIGHEST   *00510217
005103* CALORIES BY AN ELF                                             *00510317
005104*----------------------------------------------------------------*00510417
005110     CLOSE CAL-DATA.                                              00511016
005200     DISPLAY 'HIGHEST CALORIES: ' HIGHEST-CALORIES.               00520001
005300 4000-EXIT.                                                       00530001
005400     EXIT.                                                        00540001
