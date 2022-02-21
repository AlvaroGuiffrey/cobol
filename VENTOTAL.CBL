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
       01  T-V                   PIC S9(12)V99 VALUE 0.
       01  T-100                 PIC S9(12)V99 VALUE 0.
       01  T-101                 PIC S9(12)V99 VALUE 0.
       01  T-102                 PIC S9(12)V99 VALUE 0.
       01  T-103                 PIC S9(12)V99 VALUE 0.
       
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
                     "LISTADO VENTAS POR TOTALES ".
             03 FILLER           PIC X(14) VALUE "DESDE FECHA:".
             03 FECHA-ED.
                04 DIA-ED        PIC 99/.
                04 MES-ED        PIC 99/.
                04 ANO-ED        PIC 99.
             03 FILLER           PIC X(14) VALUE "HASTA FECHA:".
             03 FECHAD-ED.
                04 DIAD-ED        PIC 99/.
                04 MESD-ED        PIC 99/.
                04 ANOD-ED        PIC 99.


          02 TIT3.
             03  FILLER                PIC X(30) VALUE SPACES.
             03  FILLER                PIC X(34) VALUE "ARTICULOS".
             03  FILLER                PIC X(20) VALUE "IMPORTE".

             
000000 01 RENGLONES.
          02 RENG1.
             03  FILLER          PIC X(30) VALUE SPACES.
             03  NOM-ART-ED      PIC X(20).
             03  FILLER          PIC X(3).
             03  IMP-ED          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
          02 RENG2.
             03  FILLER          PIC X(45).
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
      *     ACCEPT TURNO LINE 14 POSITION 30 NO BEEP TAB 
      *            ON EXCEPTION CONDI MOVE 1 TO SW1.
      *     IF ESCAPE-X GO TO EMP.
       DESFEC.
           ACCEPT DESDE-FECHA LINE 16 POSITION 28 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO TUR.
       HASFEC.
           ACCEPT HASTA-FECHA LINE 18 POSITION 28 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO DESFEC.                    
           COMPUTE DESDE = (A-D * 372) + (M-D * 31 ) + D-D
           COMPUTE HASTA = (A-H * 372) + (M-H * 31 ) + D-H.
      *     PERFORM TITUL.   
000000 LEA.


           READ VENTURNO NEXT AT END GO TO FINAL-X.
            
           IF EMPRESA NOT  =  EMP-VT GO TO LEA.
           IF SUCURSAL NOT = SUC-VT  GO TO LEA.
           COMPUTE ESTA = (AA-VT * 372 ) + (MM-VT * 31) + 
                         DD-VT
           IF ESTA < DESDE OR ESTA > HASTA GO TO LEA.

           PERFORM ARMA THRU ARMA-F.
      *     PERFORM AVANCE
      *     PERFORM IMPRIMA
           GO TO LEA.
000000 ARMA.
           MOVE ART1-VT TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "NNN" TO DET-NUM.
           MOVE DET-NUM TO NOM-ART-ED.
           MOVE MAR-VT TO COD-MAR.
           READ MARCAS INVALID KEY MOVE "mmm" TO DET-MAR.
           COMPUTE TOT = TOT + IMP-VT.

           IF ART1-VT < 100 OR ART1-VT  > 103 
                 COMPUTE T-V = T-V + IMP-VT
                 GO TO ARMA-F.

           IF ART1-VT = 100 COMPUTE T-100 = T-100 + IMP-VT.
           IF ART1-VT = 101 COMPUTE T-101 = T-101 + IMP-VT.
           IF ART1-VT = 102 COMPUTE T-102 = T-102 + IMP-VT.
           IF ART1-VT = 103 COMPUTE T-103 = T-103 + IMP-VT.
       ARMA-F.
           EXIT.
                 

000000 AVANCE.
           ADD 1 TO CONT-RENG
           IF CONT-RENG > 67 MOVE SPACES TO LINEA
                             WRITE LINEA  AFTER PAGE
                             PERFORM TITUL.
000000 TITUL.
           MOVE D-D       TO DIA-ED
           MOVE M-D       TO MES-ED
           MOVE A-D       TO ANO-ED
           MOVE D-H       TO DIAD-ED
           MOVE M-H       TO MESD-ED
           MOVE A-H       TO ANOD-ED

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
           DISPLAY "DESDE FECHA......:" LINE 16 POSITION 08.
           DISPLAY "HASTA FECHA......:" LINE 18 POSITION 08.
       FINAL-X.
           PERFORM TITUL
           MOVE "VARIOS "      TO NOM-ART-ED
           MOVE T-V            TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2
           MOVE "NAFTA NORMAL " TO NOM-ART-ED
           MOVE T-100          TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2
           MOVE "NAFTA EXTRA " TO NOM-ART-ED
           MOVE T-101          TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2
           MOVE "GAS OIL   " TO NOM-ART-ED
           MOVE T-102          TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2
           MOVE "EXTRA ECOLOGICA" TO NOM-ART-ED
           MOVE T-103          TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2

           MOVE ALL "-" TO LINEA
           WRITE LINEA AFTER 1

           MOVE "TOTAL DE VENTA " TO NOM-ART-ED
           MOVE TOT           TO IMP-ED
           WRITE LINEA FROM RENG1 AFTER 2
           
           CLOSE EMPRESAS MARCAS NUMERO VENTURNO.
           GO TO FFFF.
       FFFF.
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
