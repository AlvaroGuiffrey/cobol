       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VENTURNO.
       AUTHOR.  H.A.S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   AMI.
       OBJECT-COMPUTER.   AMI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "MANEJO.SEL".
           COPY "EMPRE.SEL". 
           COPY "NUMERO.SEL".
           COPY "MARCAS.SEL".
           COPY "VENTURNO.SEL".

           SELECT LISTADO ASSIGN TO PRINT "PRINTER"
                  FILE STATUS IS ERRORES.
       DATA DIVISION.
       FILE SECTION.
           COPY "MANEJO.REG".
           COPY "NUMERO.REG".
           COPY "MARCAS.REG".
           COPY "VENTURNO.REG".
           COPY "EMPREBM.REG".
000000 FD LISTADO
                   RECORD CONTAINS 132 CHARACTERS.
       01 LINEA                    PIC X(132).
000000 WORKING-STORAGE SECTION.
       01  ESTA-CUENTA           PIC 9(6) VALUE 0.
       01  TOT                   PIC S9(12)V99 VALUE 0.
       01 TITULOS.
          02 TIT1.
             03 FILLER           PIC X(15) VALUE SPACES.
             03 FILLER           PIC X(10) VALUE "EMPRESA.:".
             03 EMP-ED           PIC X(55).
             03 FILLER           PIC X(13) VALUE "HOJA Nro.: ".
             03 HOJA-ED          PIC ZZZZ.
          02 TIT2.
             03 FILLER           PIC X(20) VALUE SPACES.
             03 FILLER           PIC X(50) VALUE 
                     "LISTADO VENTAS X TURNO".
             03 FILLER           PIC X(8) VALUE "FECHA:".
             03 FECHA-ED.
                04 DIA-ED        PIC 99/.
                04 MES-ED        PIC 99/.
                04 ANO-ED        PIC 99.
          02 TIT3.
             03  FILLER          PIC X(5) VALUE SPACES.
             03  FILLER          PIC X(10) VALUE "F E C H A ".
             03  FILLER          PIC X(1) VALUE SPACES.
             03  FILLER          PIC X(6) VALUE "COND.".
             03  FILLER          PIC X(17) VALUE "A R T I C U L O".
             03  FILLER          PIC X(18) VALUE SPACES.
             03  FILLER          PIC X(30) VALUE "M A R C A ".
             03  FILLER          PIC X(5) VALUE SPACES.
             03  FILLER          PIC X(12) VALUE "U. VENDIDAS".
             03  FILLER          PIC X(5) VALUE SPACES.
             03  FILLER          PIC X(14) VALUE "IMP. VENDIDO".
000000 01 RENGLONES.
          02 RENG1.
             03  FILLER          PIC X(6) VALUE SPACES.
             03  FECHA-VT-ED.
                 05  DIA-VT-ED   PIC ZZ/.
                 05  MES-VT-ED   PIC ZZ/.
                 05  ANO-VT-ED   PIC ZZ.
             03  FILLER          PIC X(5).
             03  COND-ED         PIC ZZ99BB.
             03  ART-ED          PIC ZZZZZZ.
             03  FILLER          PIC X.
             03  NOM-ART-ED      PIC X(20).
             03  FILLER          PIC X(3).
             03  MAR-ED      PIC ZZZZ.
             03  FILLER          PIC X.
             03  NOM-MAR-ED      PIC X(20).
             03  FILLER          PIC X(6).
             03  UNI-ED          PIC ZZZ,ZZ9.999.
             03  FILLER          PIC X(6).
             03  IMP-ED          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
          02 RENG2.
             03  FILLER          PIC X(74).
             03  FILLER          PIC X(30) VALUE " T O T A L ".
             03  TOT-ED          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 CONT-RENG              PIC 9(2) VALUE 0.
       01 CONT-HOJA              PIC 9999 VALUE 0.
       01  DED                   PIC X.
       01 FECHA-ESTA.
          02 AA                  PIC 99.
          02 MM                  PIC 99.
          02 DD                  PIC 99.
       01  FECHA-CAR.
           03 ANO                PIC 99 VALUE 0.
           03 MES                PIC 99 VALUE 0.
           03 DIA                PIC 99 VALUE 0.
       01  FECH-ED.
           03  DIA-EDI           PIC ZZ/.
           03  MES-EDI           PIC ZZ/.
           03  ANO-EDI           PIC ZZ.


       01  LASFECHAS.
           03  DESDE-FECHA           PIC 9(6) VALUE 0.
           03  R-D REDEFINES DESDE-FECHA.
               05  D-D               PIC 99.
               05  M-D               PIC 99.
               05  A-D               PIC 99.
                                   
           03  HASTA-FECHA           PIC 9(6) VALUE 0.
           03  R-H REDEFINES HASTA-FECHA.
               05  D-H               PIC 99.
               05  M-H               PIC 99.
               05  A-H               PIC 99.                     
          
       01 CAPTURA.
          03  EMPRESA                PIC 9(4).
          03  SUCURSAL               PIC 9(2).
          03  DESDE                  PIC 9(6).
          03  HASTA                  PIC 9(6).
          03  ESTA                   PIC 9(6).
          03  TURNO                  PIC 9.
          COPY "EFE.WOR".
       01  SW1          PIC 9.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "MANEJO.ERR".
           COPY "VENTURNO.ERR".
           COPY "EMPRE.ERR".
           COPY "MARCAS.ERR".
           COPY "NUMERO.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           OPEN INPUT EMPRESAS MARCAS NUMERO VENTURNO MANEJO
                      OUTPUT LISTADO.
           ACCEPT FECHA-CAR FROM DATE.           
           MOVE DIA TO DIA-EDI DIA-ED.
           MOVE MES TO MES-EDI MES-ED.
           MOVE ANO TO ANO-EDI ANO-ED.
           PERFORM ARMA-PANTALLA.
           DISPLAY FECH-ED LINE 2 POSITION 70.


       EMP.
           ACCEPT EMPRESA LINE 10 POSITION 30 NO BEEP TAB 
                   ON EXCEPTION CONDI MOVE 1 TO SW.
           IF ESCAPE-X GO TO FINAL-X.
           ACCEPT SUCURSAL LINE 12 POSITION 30 NO BEEP TAB 
                      ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO EMP.
           MOVE EMPRESA TO NUM-EMP MA-EMP.
           MOVE SUCURSAL TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO EMP.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                GO TO EMP.

           DISPLAY RAZ-SOC-EMP LINE 11 POSITION 50.
           MOVE RAZ-SOC-EMP TO EMP-ED.          
       TUR.              
           ACCEPT TURNO LINE 14 POSITION 30 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO EMP.
       DESFEC.
           ACCEPT DESDE-FECHA LINE 16 POSITION 28 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO TUR.
       HASFEC.
           ACCEPT HASTA-FECHA LINE 18 POSITION 28 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO DESFEC.                    
           COMPUTE DESDE = (A-D * 372) + (M-D * 31 ) + D-D
           COMPUTE HASTA = (A-H * 372) + (M-H * 31 ) + D-H
           PERFORM TITUL.   
000000 LEA.


           READ VENTURNO NEXT AT END GO TO FINAL-X.
            
           IF EMPRESA NOT  =  EMP-VT GO TO LEA.
           IF SUCURSAL NOT = SUC-VT  GO TO LEA.
           IF TURNO NOT   = TUR-VT  GO TO LEA.
           COMPUTE ESTA = (AA-VT * 372 ) + (MM-VT * 31) + 
                         DD-VT
           IF ESTA < DESDE OR ESTA > HASTA GO TO LEA.

           PERFORM ARMA
           PERFORM AVANCE
           PERFORM IMPRIMA
           GO TO LEA.
000000 ARMA.
           MOVE ART1-VT TO COD-NUM ART-ED.
           READ NUMERO INVALID KEY MOVE "NNN" TO DET-NUM.
           MOVE DET-NUM TO NOM-ART-ED.
           MOVE MAR-VT TO COD-MAR MAR-ED.
           READ MARCAS INVALID KEY MOVE "mmm" TO DET-MAR.
           MOVE DET-MAR TO NOM-MAR-ED. 
           MOVE FEC-VT TO FECHA-VT-ED.
           MOVE DD-VT TO DIA-VT-ED.
           MOVE MM-VT TO MES-VT-ED.
           MOVE AA-VT TO ANO-VT-ED.
           MOVE UNI-VT TO UNI-ED.
           MOVE IMP-VT TO IMP-ED.
           MOVE CON-VT TO COND-ED.
           COMPUTE TOT = TOT + IMP-VT.

000000 AVANCE.
           ADD 1 TO CONT-RENG
           IF CONT-RENG > 67 MOVE SPACES TO LINEA
                             WRITE LINEA  AFTER PAGE
                             PERFORM TITUL.
000000 TITUL.
           ADD 1 TO CONT-HOJA.
           MOVE CONT-HOJA TO HOJA-ED
           WRITE LINEA FROM TIT1 AFTER ADVANCING 2
           WRITE LINEA FROM TIT2 AFTER ADVANCING 2
           WRITE LINEA FROM TIT3 AFTER ADVANCING 2
           MOVE ALL "-" TO LINEA
           WRITE LINEA AFTER 1
           MOVE 7 TO CONT-RENG.
000000 IMPRIMA.
           WRITE LINEA FROM RENG1 AFTER  ADVANCING 1.
       EX-PGM.
           EXIT PROGRAM.

       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "-----------------------" LINE 4 POSITION 18.
           DISPLAY " LISTADO VENTAS X TURNO " LINE 5 POSITION 18.
           DISPLAY "----------------------- " LINE 6 POSITION 18.           

           DISPLAY "EMPRESA..........:" LINE 10 POSITION 08.
           DISPLAY "SUCURSAL.........:" LINE 12 POSITION 08.       
           DISPLAY "TURNO A LISTAR...:" LINE 14 POSITION 08.
           DISPLAY "DESDE FECHA......:" LINE 16 POSITION 08.
           DISPLAY "HASTA FECHA......:" LINE 18 POSITION 08.
       FINAL-X.
           MOVE TOT TO TOT-ED
           WRITE LINEA FROM RENG2 AFTER 2
           CLOSE EMPRESAS MARCAS NUMERO VENTURNO.
           GO TO FFFF.
       FFFF.
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
