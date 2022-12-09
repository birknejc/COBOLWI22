       IDENTIFICATION DIVISION.
       PROGRAM-ID.             CBLJCB00.
       AUTHOR.                 JEFF BIRKNER.
       DATE-WRITTEN.           11/30/22.
       DATE-COMPILED.

      *****************************************************************
      * CREATES A STUDENT ROSTER REPORT FROM AN INOUT FILE OF STUDENT *
      * NAMES.                                                        *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT STUDENT-MASTER
               ASSIGN TO "C:\COBOLWI22\STDNTMST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO "C:\COBOLWI22\STDNTRPT.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 49 CHARACTERS.

       01  I-REC.
           05  I-ID            PIC X(7).
           05  I-NAME.
               10  I-LNAME     PIC X(15).
               10  I-FNAME     PIC X(15).
               10  I-INIT      PIC X.
           05  I-GPA           PIC 9V99.
           05  I-EX-STRT-SAL   PIC 9(6)V99.

       FD PRTOUT
           LABEL RECORD IS OMITTED
           DATA RECORD IS PRTLINE
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE            PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  MORE-RECS       PIC XXX.
           05  C-PCTR          PIC 99      VALUE 0.
           05  C-SCTR          PIC 999     VALUE ZERO.

       01  CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10 I-YEAR       PIC 9(4).
               10 I-MONTH      PIC 99.
               10 I-DAY        PIC 99.
           05 DTIME            PIC X(11).


       01  COMPANY-TITLE-LINE.
           05  FILLER          PIC X(6)    VALUE "DATE:".
           05  O-MONTH         PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  O-DAY           PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  O-YEAR          PIC 9(4).
           05  FILLER          PIC X(35)   VALUE SPACES.
           05  FILLER          PIC X(30)   VALUE "BIRKNER'S COBOL STUDEN
      -                                    "T ROSTER".
           05  FILLER          PIC X(43)   VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE "PAGE:".
           05  O-PCTR          PIC Z9.

       01 COL-HDG1.
           05  FILLER          PIC X(119)  VALUE SPACES.
           05  FILLER          PIC X(13)   VALUE "ANTICIPATED".

       01 COL-HDG2.
           05 FILLER           PIC X(4)    VALUE "  ID".
           05 FILLER           PIC X(23)   VALUE SPACES.
           05 FILLER           PIC X(9)    VALUE "LAST NAME".
           05 FILLER           PIC X(26)   VALUE SPACES.
           05 FILLER           PIC X(10)   VALUE "FIRST NAME".
           05 FILLER           PIC X(26)   VALUE SPACES.
           05 FILLER           PIC X(3)   VALUE "GPA".
           05 FILLER           PIC X(16)   VALUE " ".
           05 FILLER           PIC X(15)   VALUE "STARTING SALARY".

       01 DETAIL-LINE.
           05 O-ID             PIC X(7).
           05 FILLER           PIC X(20)   VALUE " ".
           05 O-LNAME          PIC X(15).
           05 FILLER           PIC X(20)   VALUE " ".
           05 O-FNAME          PIC X(15).
           05 FILLER           PIC X(20)   VALUE " ".
           05 O-GPA            PIC Z.99.
           05 FILLER           PIC X(18)   VALUE " ".
           05 O-EX-STRT-SAL    PIC $ZZZ,ZZZ.99.
           05 FILLER           PIC X(2)    VALUE " ".

       01  GT-LINE.
           05 FILLER           PIC X(54)   VALUE " ".
           05 FILLER           PIC X(15)   VALUE "STUDENT COUNT".
           05 O-SCTR           PIC ZZ9.
           05 FILLER           PIC X(60)   VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-DAY TO O-DAY.

           OPEN INPUT STUDENT-MASTER.
           OPEN OUTPUT PRTOUT.

           PERFORM 9000-READ.
           PERFORM 9100-HDG.

       2000-MAINLINE.
           PERFORM 2200-CALCS.
           PERFORM 2100-OUTPUT.
           PERFORM 9000-READ.

       2100-OUTPUT.
           MOVE I-ID TO O-ID.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-LNAME TO O-LNAME.
           MOVE I-GPA TO O-GPA.
           MOVE I-EX-STRT-SAL TO O-EX-STRT-SAL.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9100-HDG.

       2200-CALCS.
           COMPUTE C-SCTR = C-SCTR + 1.

       3000-CLOSING.
           MOVE C-SCTR TO O-SCTR.
           WRITE PRTLINE FROM GT-LINE
               AFTER ADVANCING 3 LINES.
           CLOSE STUDENT-MASTER.
           CLOSE PRTOUT.

       9000-READ.
           READ STUDENT-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

       9100-HDG.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COL-HDG1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HDG2
               AFTER ADVANCING 1 LINE.