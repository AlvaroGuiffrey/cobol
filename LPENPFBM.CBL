       IDENTIFICATION DIVISION.
       PROGRAM-ID.  COMPPAGO.
       AUTHOR.  YO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
000000 SOURCE-COMPUTER.   AMI.
       OBJECT-COMPUTER.   AMI.
       SPECIAL-NAMES.
            DECIMAL-POINT  IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT EMPRESAS   ASSIGN TO RANDOM "EMPRESAS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS CLAVE-EMP.
            SELECT PENDIENTES ASSIGN TO RANDOM "PENDIEBM"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS CLAVE-PEND.
            SELECT PENDIENTES1 ASSIGN TO RANDOM "PENDIBM1"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CLAVE-PEND1.
            SELECT CUENTAS ASSIGN TO RANDOM "CUENTAS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS CLAVE-CTA.
            SELECT REMATE  ASSIGN TO RANDOM "REMATE"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS CLAVE-REMATE.
            SELECT LISTADO ASSIGN TO PRINT "PRINTER".
000000 DATA DIVISION.
       FILE SECTION.
       FD PENDIENTES.
      *   BLOCK  CONTAINS 63 CHARACTERS
      *   LABEL RECORDS ARE STANDARD.
       01 REG-PEND.
          02  CLAVE-PEND.
              03  EMP-PEND          PIC 9999.
              03  SUC-PEND          PIC 99.
              03  RUBRO-PEND        PIC 9(6).
              03  CUENTA-PEND      PIC 9(6).
              03  SUB-CUENTA-PEND   PIC 99 COMP-1.
              03  VENCIMIENTO.
                  05  ANO           PIC 9(2).
                  05  MES           PIC 9(2).
                  05  DIA           PIC 9(2).
              03  PENDIENTE        PIC 9(6).
          02  TIPO                  PIC 9(1).
          02  COMPRO                PIC 9(6).
          02  REMATE-R              PIC 9(6).
          02  IMPORTE-PEND PIC S9(13)V9(2).
          02  ACUENTA      PIC S9(13)V9(2).
          02  DETA-PEND             PIC X(30).
       FD PENDIENTES1.
      *   BLOCK  CONTAINS 63 CHARACTERS
      *   LABEL RECORDS ARE STANDARD.
       01 REG-PEND1.
          02  CLAVE-PEND1.
              03  EMP-PEND1         PIC 9999.
              03  SUC-PEND1         PIC 99.
              03  RUBRO-PEND1       PIC 9(6).
              03  VENCIMIENTO1.
                  05  ANO1          PIC 9(2).
                  05  MES1          PIC 9(2).
                  05  DIA1          PIC 9(2).
              03  CUENTA-PEND1     PIC 9(6).
              03  SUB-CUENTA-PEND1   PIC 99 COMP-1.
              03  PENDIENTE1       PIC 9(6).
          02  TIPO1                 PIC 9(1).
          02  COMPRO1               PIC 9(6).
          02  REMATE1               PIC 9(6).
          02  IMPORTE-PEND1 PIC S9(13)V99.
          02  ACUENTA1     PIC S9(13)V9(2).
           02  DETA-PEND1        PIC X(30).
             COPY "EMPRE2.REG".
              COPY "REMAT.REG".
              COPY "CUENTABM.REG".
       FD  LISTADO
           BLOCK  CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LINEA.
           02  FILLER   PIC X(132).
000000 WORKING-STORAGE SECTION.
       77  EOF             PIC X(2) VALUE "NO".
       77  RENG            PIC 9(2) VALUE 0.
       77  AUXHOJ          PIC 9(4) VALUE 0.
       77  TOTAL           PIC S9(13)V9(2) VALUE 0.
       77  SPA             PIC X(1) VALUE SPACE.
       01  LIN1.
           02  TAM1     PIC X.
           02  TAMA1    PIC X.
           02  EMPRE-ED    PIC X(50).
           02 FILLER       PIC X(59) VALUE SPACES.
           02  FILLER   PIC X(10) VALUE "FECHA:".
           02  FECHA    PIC 99/99/99.
       01  LIN2.
           02  TAM2        PIC X(41).
           02  TAMA2       PIC X.
           02  FILLER   PIC X(31) VALUE "COMPROMISOS DE PAGOS A FECHA".
           02  IMPZON      PIC Z9.
           02  FILLER      PIC X(37) VALUE SPACE.
           02  FILLER      PIC X(10) VALUE "HOJA Nro:".
           02  HOJA        PIC ZZZ9.
       01  LIN3.
           02  TAM3        PIC X.
           02  TAMA3       PIC X.
           02  FILLER PIC X(13) VALUE "CLIENTE".
           02  FILLER PIC X(32) VALUE "APELLIDO Y NOMBRES".
           02  FILLER PIC X(08) VALUE "TIPO".
           02  FILLER PIC X(12) VALUE "COMPROB.".
           02  FILLER PIC X(11) VALUE "VENCIM.".
           02  FILLER PIC X(43) VALUE "DESCRIPCION".
           02  FILLER PIC X(7) VALUE "IMPORTE".
       01  LIN4.
           02  TAM4        PIC X.
           02  TAMA4       PIC X.
           02  IMPCLI      PIC ZZZ.ZZZBB  BLANK WHEN ZERO.
           02  IMPCLI1      PIC 99BB.
           02  IMPNOM      PIC X(34).
           02  IMPTIP      PIC X(7).
           02  IMPCOM      PIC ZZZ.ZZZB(5).
           02  IMPVEN      PIC ZZ/ZZ/ZZB(3) BLANK WHEN ZERO.
           02  IMPDES      PIC X(35).
           02  IMPPAG      PIC ---.---.---.--9,99 BLANK WHEN ZERO.
       01  FIN              PIC X.
       01  IND              PIC 9(4) COMP VALUE 0.
       01  CAMPOS-AUXILIARES.
           02  ZON          PIC 9(2).
           02  YES          PIC X.
           02  COR          PIC X.
       01  FECHA1   PIC 9(6).
       01  FECHA2 REDEFINES FECHA1.
           02  DD  PIC 99.
           02  MM  PIC 99.
           02  AA  PIC 99.
000000 01  FECHA3  PIC 9(6).
       01  FECHA4 REDEFINES   FECHA3.
           02  AA     PIC 99.
           02  MM     PIC 99.
           02  DD     PIC 99.
       01  LISTA      PIC X(50) VALUE "PRINTER".
       01  FORMA      PIC X(50) VALUE SPACES.
       01  MENSAJE            PIC X(50).
       01  EMPRE              PIC 9999.
       01  SUC-CAP            PIC 99.
       01  RUB-CAP            PIC 9(6).
       01  IMPRE              PIC 99.
       77  DESDE-FECHA         PIC 9(6).
       77  HASTA-FECHA         PIC 9(6).
       77  ESTE-VENCIMIENTO    PIC 9(6) VALUE 0.
              COPY "UTILES.WOR".
000000***************************************************************
000000 PROCEDURE DIVISION.
       INICIO.
           ACCEPT FECHA3 FROM DATE.
           MOVE CORR FECHA4 TO FECHA2.
           MOVE FECHA1 TO FECHA.
           OPEN INPUT CUENTAS
                       REMATE
                OUTPUT LISTADO PENDIENTES1.
       REPETICION.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE
           OPEN INPUT PENDIENTES EMPRESAS.
           MOVE "NO" TO EOF.
           PERFORM ARMA-PANTALLA.
           PERFORM ARMA-D-PANTALLA.
           MOVE 0 TO TOTAL.
       REPETICION1.
           DISPLAY "LISTADO COMPROMISOS DE PAGOS A FECHA" LINE 6
                         POSITION 20
           ACCEPT EMPRE LINE 8 POSITION 30 NO BEEP TAB
               ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           ACCEPT SUC-CAP LINE 8 POSITION 38 NO BEEP TAB
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO REPETICION1.

           PERFORM BORRA-MENS.
           MOVE EMPRE TO NUM-EMP
           MOVE SUC-CAP TO SUC-EMP
           READ EMPRESAS INVALID KEY PERFORM BORRA-MENS
                        MOVE "EMPRESA INEXISTENTE" TO MENSAJE
                        PERFORM MENS
                        GO TO REPETICION1.
           MOVE RAZ-SOC-EMP TO EMPRE-ED.
       RUB.
           ACCEPT RUB-CAP LINE 10 POSITION 30 NO BEEP TAB CONVERT
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO REPETICION1.
       D-F.
           ACCEPT DESDE-FECHA LINE 14 POSITION 30 NO BEEP TAB
              ON EXCEPTION CONDI MOVE 1 TO SW1. 
           IF ESCAPE-X GO TO RUB.
       H-F.
           ACCEPT HASTA-FECHA LINE 16 POSITION 30 NO BEEP TAB
               ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO D-F.
           DISPLAY "DATOS  CORRECTOS ?  S/N " LINE 24 POSITION 10
           ACCEPT COR LINE 24 POSITION 60
           IF COR = "N" OR COR = "n" GO TO REPETICION1.
           PERFORM TITULO.
       LEO-PEND.
      *    DISPLAY "LEO PENDIENTES".
           READ PENDIENTES AT END GO TO SEGUNDO.
      *    IF NUM-EMP NOT = EMP-PEND OR
      *       SUC-EMP NOT = SUC-PEND GO TO LEO-PEND.
           IF RUB-CAP NOT = RUBRO-PEND GO TO LEO-PEND.
      *    DISPLAY "PASE CONTROL SUBCTA".
           IF TIPO < 3 GO TO LEO-PEND.
      *    DISPLAY "PASE CONTROL TIPO".
           IF VENCIMIENTO < DESDE-FECHA GO TO LEO-PEND.
           IF VENCIMIENTO > HASTA-FECHA GO TO LEO-PEND.
      *    DISPLAY "PASE CONTROL FECHA".
           MOVE REG-PEND TO REG-PEND1
           MOVE EMP-PEND TO EMP-PEND1
           MOVE SUC-PEND TO SUC-PEND1
           MOVE RUBRO-PEND TO RUBRO-PEND1
           MOVE CUENTA-PEND TO CUENTA-PEND1
           MOVE SUB-CUENTA-PEND TO SUB-CUENTA-PEND1
           MOVE VENCIMIENTO TO VENCIMIENTO1
           MOVE DETA-PEND TO DETA-PEND1
      *    DISPLAY CUENTA-PEND " " SUB-CUENTA-PEND " " VENCIMIENTO.
           WRITE REG-PEND1 INVALID KEY DISPLAY "DUP"
                                   GO TO FFF.
      *    DISPLAY "GRAVO PENDIENTES1".
           GO TO LEO-PEND.
       SEGUNDO.
           CLOSE PENDIENTES1
           OPEN INPUT PENDIENTES1.
       LEO-PEND1.
           READ PENDIENTES1  NEXT AT END PERFORM CORTE-DIA
                                         GO TO FF.
           MOVE REG-PEND1 TO REG-PEND
           MOVE CUENTA-PEND1 TO CUENTA-PEND
           MOVE SUB-CUENTA-PEND1 TO SUB-CUENTA-PEND
           MOVE EMP-PEND1 TO EMP-PEND
           MOVE SUC-PEND1 TO SUC-PEND
           MOVE DETA-PEND1 TO DETA-PEND
           MOVE VENCIMIENTO1 TO VENCIMIENTO.
       CHEQUEA.
           IF ESTE-VENCIMIENTO = 0 MOVE VENCIMIENTO TO ESTE-VENCIMIENTO.
           IF ESTE-VENCIMIENTO = VENCIMIENTO GO TO LISTA-OTRO.
           PERFORM CORTE-DIA
           GO TO CHEQUEA.
       LISTA-OTRO.
           MOVE EMP-PEND TO EMP-CTA
           MOVE SUC-PEND TO SUC-CTA
           MOVE RUBRO-PEND TO RUBRO-CTA
           MOVE CUENTA-PEND TO CUENTA IMPCLI.
           MOVE SUB-CUENTA-PEND TO SUB-CUENTA
           READ CUENTAS INVALID KEY DISPLAY "NO CUENTAS" 
                      LINE 24 POSITION 10
                            GO TO FF.
           ADD 1 TO RENG.
       PEPE.
           IF RENG > 67 MOVE SPACES TO LINEA
                        WRITE LINEA AFTER PAGE
                        PERFORM TITULO.
           COMPUTE IMPPAG = IMPORTE-PEND - ACUENTA.
           COMPUTE TOTAL = IMPORTE-PEND - ACUENTA + TOTAL.
      *    MOVE IMPORTE-PEND TO IMPPAG
           MOVE CUENTA-PEND TO IMPCLI.
           MOVE SUB-CUENTA TO IMPCLI1
           MOVE COMPRO TO IMPCOM.
           IF TIPO = 3 MOVE "CRED" TO IMPTIP.
           IF TIPO = 4 MOVE "LIQ." TO IMPTIP.
           MOVE NOMBRE-C TO IMPNOM.
           MOVE VENCIMIENTO TO FECHA3.
           MOVE CORR FECHA4 TO FECHA2.
           MOVE FECHA1 TO IMPVEN.
           MOVE SPACE TO IMPDES.
           IF REMATE-R NOT = 0 PERFORM PONGO-REMATE.
           WRITE LINEA FROM LIN4 AFTER ADVANCING 1.
      *    ADD 1 TO RENG.
            GO TO LEO-PEND1.
       CORTE-DIA.
            MOVE TOTAL TO IMPPAG
            MOVE "*** T O T A L  D I A ***" TO IMPDES
            MOVE 0 TO IMPCLI IMPVEN IMPCOM
            MOVE SPACES TO IMPNOM IMPTIP
            WRITE LINEA FROM LIN4 AFTER 2
            MOVE SPACES TO LINEA
            WRITE LINEA AFTER 1
            ADD 3 TO RENG
            MOVE 0 TO TOTAL ESTE-VENCIMIENTO.
            
000000 TITULO.
            MOVE SPACE TO LINEA.
            ADD 1 TO AUXHOJ.
            MOVE AUXHOJ TO HOJA.
      *     WRITE LINEA FROM SPA AFTER PAGE.
            WRITE LINEA FROM LIN1 AFTER ADVANCING 3.
            WRITE LINEA FROM LIN2 AFTER ADVANCING 1.
            WRITE LINEA FROM LIN3 AFTER ADVANCING 2.
            MOVE ALL "=" TO LINEA.
            WRITE LINEA AFTER ADVANCING 1.
            MOVE 7 TO RENG.
000000 PONGO-REMATE.
            MOVE REMATE-R TO NUMERO-REM.
            READ REMATE  INVALID KEY DISPLAY "NO REMATE"
                         LINE 24 POSITION 10 GO FF.
            MOVE DETA-PEND       TO IMPDES.
       EX-PGM.
            EXIT PROGRAM.
       FF.
            CLOSE CUENTAS PENDIENTES PENDIENTES1 REMATE.
            CLOSE EMPRESAS LISTADO.
       F1.
            EXIT PROGRAM.
       F2.  
            GO TO FFF.
       FFF.
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
       ARMA-PANTALLA.
            COPY "BORDES.PANT".
       ARMA-D-PANTALLA.
            DISPLAY "CUENTAS CORRIENTES" LINE 2 POSITION 3.
            DISPLAY "LISTADO DE PENDIENTES" LINE 2 POSITION 36.
           DISPLAY "EMPRESA              :" LINE 8 POSITION 3
           DISPLAY "RUBRO                :" LINE 10 POSITION 3
           DISPLAY "DESDE FECHA (AAMMDD) :" LINE 14 POSITION 3
           DISPLAY "HASTA FECHA (AAMMDD) :" LINE 16 POSITION 3
           DISPLAY " PRECAUCION:           " LINE 14 POSITION 40
                      REVERSE.
           DISPLAY " INGRESE INVERTIDA LAS " LINE 15 POSITION 40
                      REVERSE.
           DISPLAY " FECHAS (AAMMDD)       " LINE 16 POSITION 40
                      REVERSE.
       MENS.
           DISPLAY MENSAJE LINE 24 POSITION 3.
       BORRA-MENS.
           DISPLAY SPACES LINE 24 POSITION 3 SIZE 70.



