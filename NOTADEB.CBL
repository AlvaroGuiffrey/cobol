       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESCTA.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. DIC'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "MOVI.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "CUENTABM.REG".
       COPY "EMPREBM.REG".
       COPY "IMP02.REG".
       COPY "MOVI.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01  CAMPOS-CAPTURA.
      *     03  EMP-CAP          PIC 9(4) VALUE 0.
      *     03  SUC-CAP          PIC 99   VALUE 0.
           03  RUB-CAP          PIC 9(6) VALUE 0.
           03  CTA-CAP          PIC 9(6) VALUE 0.
           03  S-CTA-CAP        PIC 99   VALUE 0.
           03  CTA-CAP1         PIC 9(6) VALUE 0.
           03  S-CTA-CAP1       PIC 99   VALUE 0.
           03  FEC-CAP        PIC 9(6).
           03  R-FEC-CAP REDEFINES FEC-CAP.
               05  D-F          PIC 99.
               05  M-F          PIC 99.
               05  A-F          PIC 99.
           03  DIA-CAP          PIC 9(3)  VALUE 0.
           03  POR-CAP          PIC 99V99 VALUE 0.
           03  INT              PIC 9(12)V99 VALUE 0.
           03  SOB              PIC X.
           03  IMP              PIC 9(12)V99 VALUE 0.
           03  CONT-RENG        PIC 99 VALUE 0.
       01  TITULOS.
           03  TIT1.
               05  FILLER        PIC X(05) VALUE SPACES.
               05  EMP-ED        PIC X(50) VALUE SPACES.
           03  TIT2.
               05  FILLER        PIC X(20) VALUE SPACES.
               05  FILLER        PIC X(40) VALUE 
               "LISTADO CONTROL GASTOS CARGADOS".
               05  FILLER        PIC X(8) VALUE "FECHA :".
               05  DIA-ED        PIC 99/.
               05  MES-ED        PIC 99/.
               05  ANO-ED        PIC 99.
           03  TIT3.
               05  FILLER        PIC X(5) VALUE SPACES.
               05  FILLER        PIC X(13) VALUE " Nro.CUENTA ".
               05  FILLER        PIC X(40) VALUE "  N O M B R E ".
               05  FILLER        PIC X(15) VALUE "S A L D O ".
               05  FILLER        PIC X(20) VALUE " G A S T O S ".
       01  RENGLONES.
           03  RENG1.
               05  FILLER        PIC X(5) VALUE SPACES.
               05  CTA-ED        PIC ZZZZZZZZZB(3).
               05  NOM-ED        PIC X(38)BB.
               05  SAL-ED        PIC ZZZ.ZZZ.ZZ9,99BB.
               05  INT-ED        PIC ZZZ.ZZZ.ZZ9,99.

       01  GASTOS                PIC 9(10)V99.
       01  LEYENDA               PIC X(30).
       COPY "ABREV.WOR".
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       ABRO.
           OPEN INPUT EMPRESAS  MANEJO
                I-O   MOVI CUENTAS
                OUTPUT IMP02.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM ARMA-PANTALLA THRU ARMA-PANTALLA-F.
       CARGUE-EMP.
           ACCEPT EMP-CAP LINE 6 POSITION 30 NO BEEP TAB CONVERT
                 ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP CLOSE CUENTAS EMPRESAS MOVI IMP02
                    STOP RUN.
           IF EMP-CAP = 0 GO TO CARGUE-CTA.
           
       CARGUE-SUC.
           ACCEPT SUC-CAP LINE 6 POSITION 35 NO BEEP TAB CONVERT
                ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-EMP.
           MOVE EMP-CAP TO NUM-EMP MA-EMP
           MOVE SUC-CAP TO SUC-EMP MA-SUC
           READ EMPRESAS INVALID KEY GO TO CARGUE-EMP.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                GO TO CARGUE-SUC.
           DISPLAY RAZ-SOC-EMP LINE 6 POSITION 40 SIZE 30.
           MOVE RAZ-SOC-EMP TO EMP-ED.

       CARGUE-RUB.
           ACCEPT RUB-CAP LINE 8 POSITION 30 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-SUC.
           IF RUB-CAP = 0 GO TO CARGUE-SUC.

       CARGUE-CTA.
           ACCEPT CTA-CAP LINE 10 POSITION 30 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-RUB.
           IF CTA-CAP = 0 GO TO CARGUE-CTA.
       CARGUE-S-CTA.
           ACCEPT S-CTA-CAP LINE 10 POSITION 36 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-CTA.
           MOVE EMP-CAP TO EMP-CTA
           MOVE SUC-CAP TO SUC-CTA
           MOVE RUB-CAP TO RUBRO-CTA
           MOVE CTA-CAP TO CUENTA
           MOVE S-CTA-CAP TO SUB-CUENTA
           READ CUENTAS INVALID KEY GO TO CARGUE-CTA.
           DISPLAY NOMBRE-C LINE 10 POSITION 40 SIZE 30.
           
       CARGUE-CTA1.
           ACCEPT CTA-CAP1 LINE 12 POSITION 30 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-S-CTA.
           IF CTA-CAP1 = 0 GO TO CARGUE-CTA1.
       CARGUE-S-CTA1.
           ACCEPT S-CTA-CAP1 LINE 12 POSITION 36 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-CTA1.
           MOVE EMP-CAP TO EMP-CTA
           MOVE SUC-CAP TO SUC-CTA
           MOVE RUB-CAP TO RUBRO-CTA
           MOVE CTA-CAP1 TO CUENTA
           MOVE S-CTA-CAP1 TO SUB-CUENTA
           READ CUENTAS INVALID KEY GO TO CARGUE-CTA.
           DISPLAY NOMBRE-C LINE 12 POSITION 40 SIZE 30.
       CARGUE-IMP.
           MOVE 0 TO IMP
           ACCEPT GASTOS LINE 14 POSITION 30 NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-S-CTA1.  
       CARGUE-DIAS.    
           ACCEPT LEYENDA LINE 16 POSITION 30 NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-IMP.

       CARGUE-FEC.    
           ACCEPT FEC-CAP LINE 18 POSITION 30 NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE 1 TO SW1.
           IF ESCAP GO TO CARGUE-IMP.
           IF FEC-CAP = 0 GO TO CARGUE-FEC.
                  
           MOVE D-F   TO DIA-ED
           MOVE M-F   TO MES-ED
           MOVE A-F   TO ANO-ED
           PERFORM TITULOS THRU TITULOS-F.

           MOVE EMP-CAP TO EMP-CTA
           MOVE SUC-CAP TO SUC-CTA
           MOVE RUB-CAP TO RUBRO-CTA
           MOVE CTA-CAP TO CUENTA
           MOVE S-CTA-CAP TO SUB-CUENTA
           START CUENTAS KEY NOT LESS CLAVE-CTA INVALID KEY 
                  DISPLAY "NO ENTRO"
                  STOP RUN.
       LEA-CUENTA.
           READ CUENTAS NEXT AT END GO TO FF.
           IF CONDICION NOT = 51 GO TO LEA-CUENTA.
           IF RUBRO-CTA NOT = RUB-CAP GO TO LEA-CUENTA.
           IF CUENTA < CTA-CAP  GO TO LEA-CUENTA. 
           IF CUENTA > CTA-CAP1 GO TO LEA-CUENTA.
           MOVE GASTOS TO INT 
           MOVE INT TO INT-ED
           MOVE NOMBRE-C TO NOM-ED
      *     MOVE SALDO-CAN TO SAL-ED
           IF IMP = 0
                   MOVE SALDO-CAN TO SAL-ED
            ELSE   MOVE IMP       TO SAL-ED.

           MOVE EMP-CAP TO EMP-MOV
           MOVE SUC-CAP TO SUC-EMP-MOV
           MOVE RUB-CAP TO RUBRO-MOV
           MOVE CUENTA TO CUENTA-MOV CTA-ED
           MOVE SUB-CUENTA TO SUB-CUENTA-MOV
           COMPUTE PMOV = PMOV + 1
           MOVE PMOV TO MOVIMIENTO-MOV
           MOVE D-F   TO DIA-MOV 
           MOVE M-F   TO MES-MOV 
           MOVE A-F   TO ANO-MOV 
           MOVE 9     TO TIPO-MOV TIPO-C-MOV
           MOVE 99999 TO COMPRO-MOV SUC-C-MOV
           MOVE "X"   TO SUB-T-C-MOV
           MOVE 0     TO REMA-MOV IMP-EXT-MOV
           MOVE INT   TO IMPORTE-MOV
           MOVE LEYENDA TO   COMENTARIO-MOV
           MOVE 0     TO ASIENTO-MOV SUCURSAL-MOV.
           PERFORM AVANCE
           PERFORM IMPRIMA.
       GRABA.
           WRITE REG-MOV INVALID KEY ADD 1 TO MOVIMIENTO-MOV
                 GO TO GRABA.
           COMPUTE SALDO = SALDO + INT

           REWRITE REG-CTAS INVALID KEY DISPLAY "IMP".

           GO TO LEA-CUENTA.

       AVANCE.
           ADD 1 TO CONT-RENG
           IF CONT-RENG > 66 MOVE SPACES TO LIN-IMP02
                             WRITE LIN-IMP02 AFTER PAGE
                             PERFORM TITULOS THRU TITULOS-F.
       TITULOS.
           WRITE LIN-IMP02 FROM TIT1 AFTER 2
           WRITE LIN-IMP02 FROM TIT2 AFTER 2
           WRITE LIN-IMP02 FROM TIT3 AFTER 2
           MOVE 6 TO CONT-RENG.
       TITULOS-F.
           EXIT.
       IMPRIMA.
           WRITE LIN-IMP02 FROM RENG1 AFTER 1.


       FF.
           CLOSE CUENTAS MOVI IMP02.
       FF1.
           EXIT.
       FF2.
           STOP RUN.

       ARMA-PANTALLA.
           DISPLAY "CARGA DE GASTOS POR MANT. CTAS.CTES" LINE 2 
                    POSITION 20 REVERSE
           DISPLAY "Empresa.......:" LINE 6 POSITION 10
           DISPLAY "Rubro.........:" LINE 8 POSITION 10
           DISPLAY "Desde Cuenta..:" LINE 10 POSITION 10
                   "Hasta Cuenta..:" LINE 12 POSITION 10
                   "IMPORTE ......:" LINE 14 POSITION 10
                   "Leyenda ......:" LINE 16 POSITION 10 
                   "Fecha Emision.:" LINE 18 POSITION 10.
                   
       ARMA-PANTALLA-F.
           EXIT.



           

             
