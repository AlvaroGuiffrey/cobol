       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAPMOV.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. JUL'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "P-CTAS.SEL".
       COPY "MOVI.SEL".
       COPY "PENDI.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CUENTABM.REG".
       COPY "EMPREBM.REG".
       COPY "IMP02.REG".
       COPY "P-CTAS.REG".
       COPY "MOVI.REG".
       COPY "PENDI.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 LIN-1.
             03 FILLER     PIC 99 COMP-4 VALUE 18.
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X(7) VALUE "RUBRO: ".
             03 RUBRO-ED   PIC ZZZZZ9.
             03 FILLER     PIC X(3) VALUE " -".
             03 NOM-RUBRO-ED PIC X(30).
             03 FILLER     PIC 99 COMP-4 VALUE 20.
             03 FILLER     PIC 99 COMP-4 VALUE 15.
          02 LIN-2.
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 FILLER       PIC 99 COMP-4 VALUE 14.
             03 FILLER       PIC X(7) VALUE "FECHA: ".
             03 DIA-EMI-ED   PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MES-EMI-ED   PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ANO-EMI-ED   PIC 9999.
             03 FILLER       PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC 99 COMP-4 VALUE 15.
          02 LIN-5.
             03 FILLER       PIC 99 COMP-4 VALUE 15.
             03 FILLER       PIC X(13) VALUE "CUENTA  SUB.".
             03 FILLER       PIC X(12) VALUE "-------- NOM".
             03 FILLER       PIC X(13) VALUE "BRE --------".
             03 FILLER       PIC X(20) VALUE "-- COMPROBANTE ---".
             03 FILLER       PIC X(15) VALUE "--------- COMEN".
             03 FILLER       PIC X(17) VALUE "TARIO ---------".
             03 FILLER       PIC X(12) VALUE "-FEC.VEN.-".
             03 FILLER       PIC X(19) VALUE "---- IMPORTE ----".
             03 FILLER       PIC X(7) VALUE "MOV.INT".
          02 LIN-3.
             03 FILLER       PIC 99 COMP-4 VALUE 15.
             03 CUENTA-ED    PIC ZZZZZ9BB.
             03 SUB-CUENTA-ED PIC ZZZZ BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE SPACES.
             03 NOMBRE-C-ED  PIC X(24)B.
             03 TIPO-C-ED      PIC XXB.
             03 SUB-T-C-ED   PIC X.
             03 FILLER       PIC X VALUE "-".
             03 SUC-C-ED     PIC 9999.
             03 FILLER       PIC X VALUE "-".
             03 COMPRO-ED    PIC 9(8).
             03 FILLER       PIC XX VALUE SPACES.
             03 COMENTARIO-ED PIC X(30)BB.
             03 DIA-VEN-ED   PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MES-VEN-ED   PIC 99/.
             03 ANO-VEN-ED   PIC 9999BB.
             03 IMPORTE-ED   PIC --.---.---.--9,99BB.
             03 MOVIMIENTO-ED PIC Z(6).
          02 LIN-31.
             03 FILLER       PIC X(90) VALUE SPACES.
             03 DIA-VEN1-ED  PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MES-VEN1-ED  PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ANO-VEN1-ED  PIC 9999BB.
             03 IMPORTE1-ED  PIC --.---.---.--9,99.
          02 LIN-4.
             03 FILLER       PIC X(6) VALUE "SON :".
             03 CONT-G-ED    PIC ZZZZZ9B.
             03 FILLER       PIC X(23) VALUE "MOVIMIENTOS; EL DEBITO ".
             03 FILLER       PIC X(6) VALUE "ES : ".
             03 FILLER       PIC 99 COMP-4 VALUE 14. 
             03 TOT-DEB-ED   PIC ZZ.ZZZ.ZZZ.ZZ9,99BB.
             03 FILLER       PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(18) VALUE "Y EL CREDITO ES : ".
             03 FILLER       PIC 99 COMP-4 VALUE 14.
             03 TOT-CRE-ED   PIC ZZ.ZZZ.ZZZ.ZZ9,99.
             03 FILLER       PIC 99 COMP-4 VALUE 20.
       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 NOMBRE-CAP        PIC X(45).
          02 FECHA-CAP.
             03 DD-CAP         PIC 99 VALUE 0.
             03 MM-CAP         PIC 99 VALUE 0.
             03 ANO-CAP.
                05 AAAA-CAP    PIC 99 VALUE 0.
                05 AA-CAP      PIC 99 VALUE 0.
          02 TIPO-M-CAP        PIC 9 VALUE 0.
          02 TIPO-C-CAP        PIC 9 VALUE 0.
          02 SUB-T-C-CAP       PIC X.
          02 SUC-C-CAP         PIC 9999 VALUE 0.
          02 COMPRO-CAP        PIC 9(8) VALUE 0.
          02 COMENTARIO-CAP    PIC X(30).
          02 IMPORTE-CAP       PIC 9(13)V99 VALUE 0.
          02 FECHA-V-CAP.
             03 DD-V-CAP       PIC 99 VALUE 0.
             03 MM-V-CAP       PIC 99 VALUE 0.
             03 ANO-V-CAP.
                05 AAAA-V-CAP  PIC 99 VALUE 0.
                05 AA-V-CAP    PIC 99 VALUE 0.
          02 PEN-CAP           PIC 999 VALUE 0.
          02 TOTAL-P-CAP       PIC 9(13)V99 VALUE 0.
          02 IMPORTE-TOT       PIC 9(13)V99 VALUE 0.
          02 FECHA-V-INT       PIC 9(6) VALUE 0.
          02 IMPORTE-INT       PIC 9(13)V99 VALUE 0.
       01 EDICION.
          02 RAYA              PIC X(74) VALUE ALL "-".
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 SUB-CUENTA-EDI    PIC ZZ BLANK WHEN ZERO.
          02 NOMBRE-EDI        PIC X(45).
          02 FECHA-EDI.
             03 DD-EDI         PIC 99.
             03 FILLER         PIC X VALUE "/".
             03 MM-EDI         PIC 99.
             03 FILLER         PIC X VALUE "/".
             03 AA-EDI         PIC 9999.
          02 FECHA-V-EDI.
             03 DD-V-EDI       PIC 99.
             03 FILLER         PIC X VALUE "/".
             03 MM-V-EDI       PIC 99.
             03 FILLER         PIC X VALUE "/".
             03 AA-V-EDI       PIC 9999.
          02 IMPORTE-EDI       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 AUXILIARES.
          02 LISTO-N-D         PIC 999999 VALUE 0.
          02 LISTO-N-H         PIC 999999 VALUE 0.
          02 LISTO-Z           PIC 99.
          02 LISTO-A-D         PIC X(30).
          02 LISTO-A-H         PIC X(30).
          02 CONT-L            PIC 9(6) VALUE 0.
          02 ZONA-L            PIC 99 VALUE 0.
          02 RUBRO-L           PIC 9(7) VALUE 0.
          02 CUENTA-H          PIC 9(6) VALUE 0.
          02 SUB-CUENTA-H      PIC 9(4) VALUE 0.
          02 NOMBRE-H          PIC X(30).
          02 COD-TAREA         PIC 9 VALUE 0.
          02 IMPORTE-AUX       PIC S9(12)V99 VALUE 0.
          02 RAZ-SOC-CAP       PIC X(40).
          02 DOM-EMP-CAP       PIC X(30).
          02 COD-L-EMP-CAP     PIC 9(4) VALUE 0.
          02 NOM-L-EMP-CAP     PIC X(30).
          02 CONT-G            PIC 9(6) VALUE 0.
          02 TOT-DEB           PIC 9(13)V99 VALUE 0.
          02 TOT-CRE           PIC 9(13)V99 VALUE 0.
          02 SALDO-CON         PIC 9(13)V99 VALUE 0.
          02 FECHA-INT         PIC 9(8) VALUE 0.
          02 RUBRO-INT         PIC 9(7) VALUE 0.
          02 FEC-C-1.
             03 ANO-C-1        PIC 9999 VALUE 0.
             03 MES-C-1        PIC 99 VALUE 0.
             03 DIA-C-1        PIC 99 VALUE 0.
          02 ABREV-AUX.
             03 ABRE0          PIC X.
             03 ABRE1          PIC XX.
             03 ABRE2          PIC X.
          02 FEC-VEN-AUX.
             03 DIA-VEN        PIC 99 VALUE 0.
             03 FILLER         PIC X VALUE "/".
             03 MES-VEN        PIC 99 VALUE 0.
             03 FILLER         PIC X VALUE "/".
             03 ANO-VEN        PIC 9999 VALUE 0.
          02 FEC-EMI-AUX.
             03 DIA-EMI        PIC 99 VALUE 0.
             03 FILLER         PIC X VALUE "/".
             03 MES-EMI        PIC 99 VALUE 0.
             03 FILLER         PIC X VALUE "/".
             03 ANO-EMI        PIC 9999 VALUE 0.
          02 COMPRO-AUX.
             03 TIPO           PIC XX VALUE SPACES.
             03 FILLER         PIC X VALUE SPACES.
             03 SUB-TIPO       PIC X VALUE SPACES.
             03 FILLER         PIC X VALUE "-".
             03 SUC            PIC 9(4) VALUE 0.
             03 FILLER         PIC X VALUE "-".
             03 COMPRO         PIC 9(8) VALUE 0.
       COPY "ABREVIA.WOR".
       COPY "ABREV.WOR".
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CUENTA.ERR".
       COPY "P-CTA.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "MOVI.ERR".
       COPY "PENDI.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE. 
       PANTA-INI. 
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-CLAVE.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-CLAVE.
           IF CLAVE-1-CAP NOT = 0 GO TO SIGA-INI.
           PERFORM CLAVE-SYS.
           PERFORM BORRA-CLAVE.
       SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3 MEN-4 MEN-5.
           PERFORM MENS-PAN.
           PERFORM MEN-MODA-A.
       CARGO-MOD.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO TAREA1.
           GO TO CARGO-MOD.
       TAREA1.
           MOVE 0 TO FECHA-INT RUBRO-INT CONT-G TOT-DEB TOT-CRE.
           MOVE 0 TO CONT-P CONT-R.
           OPEN INPUT CUENTAS P-DE-CTAS EMPRESAS PENDI MOVI.
           OPEN OUTPUT IMP02.
           MOVE " CARGA MOVIMIENTOS " TO D20-CAP.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CARGA-RUB.
           PERFORM CARGA-FEC.
       TAREA1-1.
           MOVE 0 TO PEN-CAP IMPORTE-TOT. 
           PERFORM BORRA-PANTA-C.
           DISPLAY " 1 - DEBITO    " LINE 13 POSITION 25 REVERSE.
           DISPLAY "               " LINE 14 POSITION 25 REVERSE.
           DISPLAY " 2 - CREDITO   " LINE 15 POSITION 25 REVERSE.
           PERFORM CARGA-CUE THRU CARGA-CUE-F.
           MOVE 0 TO SUB-CUENTA.
           IF CUENTA = 0  GO TO TAREA2-1.
           READ CUENTAS INVALID KEY GO TO TAREA1-1.
           DISPLAY CUENTA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           GO TO ALTA.
       TAREA2-1.
           PERFORM CARGA-NOM.
           START CUENTAS KEY IS NOT < NOMBRE-C
                     INVALID KEY GO TO TAREA2-1.
       TAREA2-2.
           READ CUENTAS NEXT AT END GO TO TAREA2-1.
           IF NUM-EMP NOT = EMP-CTA GO TO TAREA2-2.
           IF SUC-EMP NOT = SUC-CTA GO TO TAREA2-2.
           IF P-RUBRO NOT = RUBRO-CTA GO TO TAREA2-2.          
           PERFORM DESP-CUE.
           PERFORM DESP-NOM.
           PERFORM MEN-CONS.
           MOVE CUENTA TO CUENTA-CAP.
           MOVE 0 TO SUB-CUENTA SUB-CUENTA-CAP.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           DISPLAY SPACES LINE 21 POSITION 10 SIZE 50.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 GO TO ALTA.
           IF F03 GO TO TAREA2-2.
           GO TO ALTA.
       ALTA.
           PERFORM DESP-NOM.
           PERFORM CARGA-T-MOV THRU CARGA-DET.
       ALTA1.
           PERFORM CARGA-FEC-V.
           PERFORM CARGA-IMP.
       ALTA1-1.
           MOVE "<F1>GRABA MOV. " TO MEN-1.
           MOVE "<F10>CARGA OTRO" TO MEN-2.
           MOVE " VENCIMIENTO   " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 28 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION 44 SIZE 1 TAB
                 ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           COMPUTE REMATE-PEND = PMOV + 1.
           IF ESCAP  GO TO  VEO-GRABO.
           IF F01 PERFORM IMPRE THRU IMPRE-F
                  PERFORM SIGO THRU SIGO-F
                  PERFORM GRABO
                  GO TO TAREA1-1.
           IF F10 PERFORM IMPRE THRU IMPRE-F
                  PERFORM SIGO THRU SIGO-F
                  GO TO ALTA1.
           GO TO ALTA1-1.
       GRABO.
           MOVE NUM-EMP TO EMP-MOV.
           MOVE SUC-EMP TO SUC-EMP-MOV.
           MOVE RUBRO-CAP TO RUBRO-MOV.
           MOVE CUENTA-CAP TO CUENTA-MOV.
      *=-=-=-=-=- INVALIDA SUB CUENTAS =-=-=-=-=-=-
           MOVE 0 TO SUB-CUENTA-MOV.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE ANO-CAP TO ANO-MOV.
           MOVE MM-CAP TO MES-MOV.
           MOVE DD-CAP TO DIA-MOV.
           COMPUTE PMOV = PMOV + 1.
           IF TIPO-M-CAP = 2
             COMPUTE SALDO = SALDO - IMPORTE-TOT
             COMPUTE TOT-CRE = TOT-CRE + IMPORTE-TOT.
           IF TIPO-M-CAP = 1
             COMPUTE SALDO = SALDO + IMPORTE-TOT
             COMPUTE TOT-DEB = TOT-DEB + IMPORTE-TOT.
           IF SALDO-CAN > 0 
             IF TIPO-M-CAP = 2
                IF SALDO-CAN > IMPORTE-TOT
                  COMPUTE SALDO-CAN = SALDO-CAN - IMPORTE-TOT
                ELSE MOVE 0 TO SALDO-CAN
                     MOVE "N" TO ESTADO.
      *=-=-=-=-=-=-= COLOCA NORMAL SIN CANCELAR SALDO -=-=-=-=
           COMPUTE SALDO-CON ROUNDED = SALDO-TOP * 5 / 100.
           IF SALDO-CON > SALDO-CAN MOVE "N" TO ESTADO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-== 
           CLOSE CUENTAS.
           OPEN I-O CUENTAS.
           REWRITE REG-CTAS INVALID KEY WRITE REG-CTAS.
           CLOSE CUENTAS.
           OPEN INPUT CUENTAS.
           MOVE PMOV TO MOVIMIENTO-MOV.
           MOVE TIPO-M-CAP TO TIPO-MOV.
           MOVE TIPO-C-CAP TO TIPO-C-MOV.
           MOVE SUB-T-C-CAP TO SUB-T-C-MOV.
           MOVE SUC-C-CAP TO SUC-C-MOV.
           MOVE COMPRO-CAP TO COMPRO-MOV.
           MOVE IMPORTE-TOT TO IMPORTE-MOV.
           MOVE COMENTARIO-CAP TO COMENTARIO-MOV.
           CLOSE MOVI.
           OPEN I-O MOVI.
           WRITE REG-MOV INVALID KEY DISPLAY "ERROR GRABACION".
           CLOSE MOVI.
           OPEN INPUT MOVI.
           ADD 1 TO CONT-G.
           MOVE 0 TO PEN-CAP IMPORTE-TOT.
       VEO-GRABO.
           IF PEN-CAP = 0 PERFORM PIE-P
                          GO TO VEO-GRABO-F.
           PERFORM IMPRE THRU IMPRE-F.
           PERFORM GRABO.
           PERFORM PIE-P.
       VEO-GRABO-F.
           PERFORM CIERRE-A.
           GO TO INI-MOD.           
       SIGO.
           ADD 1 TO PEN-CAP.
           COMPUTE IMPORTE-TOT = IMPORTE-TOT + IMPORTE-CAP.
           MOVE NUM-EMP TO EMP-PEND.
           MOVE SUC-EMP TO SUC-EMP-PEND.
           MOVE RUBRO-CAP TO RUBRO-PEND.
           MOVE CUENTA-CAP TO CUENTA-PEND.
      *=-=-=-=-= INVALIDA SUB CUENTA -=-=-=-=-=-=-=-=
           MOVE 0 TO SUB-CUENTA-PEND.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE 0 TO PENDIENTE-PEND.
           MOVE DD-V-CAP TO DIA-PEND.
           MOVE MM-V-CAP TO MES-PEND.
           MOVE ANO-V-CAP TO ANO-PEND.
           MOVE DD-CAP TO DIA-C-1.
           MOVE MM-CAP TO MES-C-1.
           MOVE ANO-CAP TO ANO-C-1.
           IF FEC-C-1 NOT = VENCIMIENTO-PEND GO TO SIGO-NUEVO.
           START PENDI KEY IS NOT < CLAVE-PEND
                  INVALID KEY GO TO SIGO-NUEVO.
       SIGO-1.
           READ PENDI NEXT AT END GO TO SIGO-2.
           IF EMP-PEND NOT = NUM-EMP
                 GO TO SIGO-2.
           IF SUC-EMP-PEND NOT = SUC-EMP
                 GO TO SIGO-2.
           IF RUBRO-PEND NOT = RUBRO-CAP 
                 GO TO SIGO-2.
           IF CUENTA-PEND NOT = CUENTA-CAP 
                 GO TO SIGO-2.
      *=-=-=-=-= INVALIDA SUB CUENTA =-=-=-=-=-=-=-=-=-
      *     IF SUB-CUENTA-PEND NOT = 0 GO TO SIGO-2.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           IF ANO-PEND NOT = ANO-V-CAP
                 GO TO SIGO-2.
           IF MES-PEND NOT = MM-V-CAP 
                 GO TO SIGO-2.
           IF DIA-PEND NOT = DD-V-CAP 
                 GO TO SIGO-2.
           IF D-H-PEND = TIPO-M-CAP GO TO SIGO-1.
           MOVE DIA-EMI-PEND TO DIA-C-1.
           MOVE MES-EMI-PEND TO MES-C-1.
           MOVE ANO-EMI-PEND TO ANO-C-1.
           IF FEC-C-1 NOT = VENCIMIENTO-PEND GO TO SIGO-1.
           COMPUTE TOTAL-P-CAP = IMPORTE-PEND - ACUENTA-PEND.
           IF TOTAL-P-CAP > IMPORTE-CAP
                COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-CAP
                CLOSE PENDI
                OPEN I-O PENDI
                REWRITE REG-PEND
                CLOSE PENDI
                OPEN INPUT PENDI
                GO TO SIGO-F.
           IF TOTAL-P-CAP < IMPORTE-CAP
                COMPUTE IMPORTE-CAP = IMPORTE-CAP - TOTAL-P-CAP
                CLOSE PENDI
                OPEN I-O PENDI
                DELETE PENDI 
                CLOSE PENDI
                OPEN INPUT PENDI
                GO TO SIGO-1.
           CLOSE PENDI.
           OPEN I-O PENDI.
           DELETE PENDI.
           CLOSE PENDI.
           OPEN INPUT PENDI.
           GO TO SIGO-F.
       SIGO-2.
           MOVE NUM-EMP TO EMP-PEND.
           MOVE SUC-EMP TO SUC-EMP-PEND.
           MOVE RUBRO-CAP TO RUBRO-PEND.
           MOVE CUENTA-CAP TO CUENTA-PEND.
      *=-=-=-=-= INVALIDA SUB CUENTA -=-=-=-=-=-=-=-=
           MOVE 0 TO SUB-CUENTA-PEND.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE 0 TO PENDIENTE-PEND DIA-PEND MES-PEND ANO-PEND.
           START PENDI KEY IS NOT < CLAVE-PEND
                  INVALID KEY GO TO SIGO-NUEVO.
       SIGO-21.
           READ PENDI NEXT AT END GO TO SIGO-NUEVO.
           IF EMP-PEND NOT = NUM-EMP GO TO SIGO-NUEVO.
           IF SUC-EMP-PEND NOT = SUC-EMP GO TO SIGO-NUEVO.
           IF RUBRO-PEND NOT = RUBRO-CAP GO TO SIGO-NUEVO.
           IF CUENTA-PEND NOT = CUENTA-CAP GO TO SIGO-NUEVO.
      *=-=-=-=-= INVALIDA SUB CUENTA =-=-=-=-=-=-=-=-=-
      *     IF SUB-CUENTA-PEND NOT = 0 GO TO SIGO-NUEVO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           IF D-H-PEND = TIPO-M-CAP GO TO SIGO-21.
           MOVE DIA-EMI-PEND TO DIA-C-1.
           MOVE MES-EMI-PEND TO MES-C-1.
           MOVE ANO-EMI-PEND TO ANO-C-1.
           IF FEC-C-1 NOT = VENCIMIENTO-PEND GO TO SIGO-21.
           COMPUTE TOTAL-P-CAP = IMPORTE-PEND - ACUENTA-PEND.
           IF TOTAL-P-CAP > IMPORTE-CAP
                COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-CAP
                CLOSE PENDI
                OPEN I-O PENDI
                REWRITE REG-PEND
                CLOSE PENDI
                OPEN INPUT PENDI
                GO TO SIGO-F.
           IF TOTAL-P-CAP < IMPORTE-CAP
                COMPUTE IMPORTE-CAP = IMPORTE-CAP - TOTAL-P-CAP
                CLOSE PENDI
                OPEN I-O PENDI
                DELETE PENDI 
                CLOSE PENDI
                OPEN INPUT PENDI
                GO TO SIGO-21.
           CLOSE PENDI.
           OPEN I-O PENDI.
           DELETE PENDI.
           CLOSE PENDI.
           OPEN INPUT PENDI.
           GO TO SIGO-F.
       SIGO-NUEVO.
           MOVE NUM-EMP TO EMP-PEND.
           MOVE SUC-EMP TO SUC-EMP-PEND.
           MOVE RUBRO-CAP TO RUBRO-PEND.
           MOVE CUENTA-CAP TO CUENTA-PEND.
      *=-=-=-=-= INVALIDA SUB CUENTA -=-=-=-=-=-=-=-=
           MOVE 0 TO SUB-CUENTA-PEND.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           COMPUTE PPEND = PPEND + 1
           CLOSE CUENTAS.
           OPEN I-O CUENTAS.
           REWRITE REG-CTAS INVALID KEY WRITE REG-CTAS.
           CLOSE CUENTAS.
           OPEN INPUT CUENTAS.
           MOVE PPEND TO PENDIENTE-PEND.
           MOVE DD-V-CAP TO DIA-PEND.
           MOVE MM-V-CAP TO MES-PEND.
           MOVE ANO-V-CAP TO ANO-PEND.
           MOVE TIPO-M-CAP TO D-H-PEND.
           MOVE TIPO-C-CAP TO TIPO-PEND.
           MOVE SUB-T-C-CAP TO SUB-T-PEND.
           MOVE SUC-C-CAP TO SUC-T-PEND.
           MOVE COMPRO-CAP TO COMPRO-PEND.
           MOVE COMENTARIO-CAP TO DETA-PEND.
           MOVE IMPORTE-CAP TO IMPORTE-PEND.
           MOVE 0 TO ACUENTA-PEND.
           MOVE DD-CAP TO DIA-EMI-PEND.
           MOVE MM-CAP TO MES-EMI-PEND.
           MOVE ANO-CAP TO ANO-EMI-PEND.
           CLOSE PENDI.
           OPEN I-O PENDI.
           WRITE REG-PEND INVALID KEY REWRITE REG-PEND.
           CLOSE PENDI.
           OPEN INPUT PENDI.
       SIGO-F.
           EXIT.
       IMPRE.
           IF CONT-P = 0 PERFORM TITU THRU TITU-F.
           IF CONT-R > 63 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU THRU TITU-F.
           IF RUBRO-INT NOT = RUBRO-CAP
                  PERFORM PIE-P
                  PERFORM TITU-RUB THRU TITU-FEC.
           IF FECHA-CAP NOT = FECHA-INT
                  PERFORM PIE-P
                  PERFORM TITU-RUB THRU TITU-FEC.
           IF PEN-CAP > 0 GO TO IMPRE-1.
           MOVE CUENTA TO CUENTA-ED.
           MOVE SUB-CUENTA TO SUB-CUENTA-ED.
           MOVE NOMBRE-C TO NOMBRE-C-ED.
           MOVE ABREV(TIPO-C-CAP) TO ABREV-AUX.
           MOVE ABRE1 TO TIPO-C-ED.
           MOVE SUB-T-C-CAP TO SUB-T-C-ED.
           MOVE SUC-C-CAP TO SUC-C-ED.
           MOVE COMPRO-CAP TO COMPRO-ED.
           MOVE COMENTARIO-CAP TO COMENTARIO-ED.
           MOVE DD-V-CAP TO DIA-VEN-ED.
           MOVE MM-V-CAP TO MES-VEN-ED.
           MOVE ANO-V-CAP TO ANO-VEN-ED.
           IF TIPO-M-CAP = 1
               MOVE IMPORTE-CAP TO IMPORTE-ED.
           IF TIPO-M-CAP = 2
               COMPUTE IMPORTE-AUX = IMPORTE-CAP * ( 0 - 1 )
               MOVE IMPORTE-AUX TO IMPORTE-ED.
           COMPUTE MOVIMIENTO-MOV = PMOV + 1.
           MOVE MOVIMIENTO-MOV TO MOVIMIENTO-ED.
           WRITE LIN-IMP02 FROM LIN-3 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO IMPRE-F.
       IMPRE-1.
           MOVE DD-V-CAP TO DIA-VEN1-ED.
           MOVE MM-V-CAP TO MES-VEN1-ED.
           MOVE ANO-V-CAP TO ANO-VEN1-ED.
           MOVE IMPORTE-CAP TO IMPORTE1-ED.
           WRITE LIN-IMP02 FROM LIN-31 AFTER 1.
           ADD 1 TO CONT-R.
       IMPRE-F.
           EXIT.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           PERFORM FECHA-ROT132.
           PERFORM HORA-ROT132.
           MOVE "<<< CAPTUR" TO TIT-2-EDI.
           MOVE "A  DE  MOV" TO TIT-3-EDI.
           MOVE "IMIENTOS >" TO TIT-4-EDI.
           MOVE ">>        " TO TIT-5-EDI.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           ADD 6 TO CONT-R.
       TITU-RUB.
           IF CONT-R > 60 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           MOVE RUBRO-CAP TO RUBRO-INT.
           WRITE LIN-IMP02 FROM LIN-1 AFTER 1.
           ADD 1 TO CONT-R.
       TITU-FEC.
           IF CONT-R > 61 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           MOVE FECHA-CAP TO FECHA-INT.
           WRITE LIN-IMP02 FROM LIN-2 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-5 AFTER 1.
           ADD 2 TO CONT-R.
       TITU-F.
           EXIT.
       PIE-P.
           MOVE CONT-G TO CONT-G-ED.
           MOVE TOT-DEB TO TOT-DEB-ED.
           MOVE TOT-CRE TO TOT-CRE-ED.
           WRITE LIN-IMP02 FROM LIN-4 AFTER 1.
           PERFORM ESPA.
           MOVE 0 TO CONT-G TOT-DEB TOT-CRE.
           ADD 1 TO CONT-R.
       ESPA.
           MOVE SPACES TO LIN-IMP02.
           WRITE LIN-IMP02 AFTER 1.
           ADD 1 TO CONT-R.
       DESP-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           DISPLAY NUM-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                 SIZE TAB-SIZ(SW3).
           MOVE 5 TO SW3.
           DISPLAY SUC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                 SIZE TAB-SIZ(SW3).
           MOVE 6 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                 SIZE TAB-SIZ(SW3). 
       CARGA-EMP.
           PERFORM MEN-MODA-S.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-CTA.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP SUC-CTA NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.       
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           MOVE VERSION-EMP TO P-VERSION.
       CARGA-EMP-F.
           EXIT.
       CARGA-RUB.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUC.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO P-RUBRO RUBRO-CTA RUBRO-CAP.
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
           MOVE P-RUBRO TO RUBRO-ED.
           MOVE P-NOMBRE TO NOM-RUBRO-ED.
       CARGA-FEC.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                SIZE TAB-SIZ(SW3) NO BEEP TAB
                ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           IF F03 MOVE DIA-MOV TO DD-CAP
                  MOVE MES-MOV TO MM-CAP
                  MOVE ANO-MOV TO ANO-CAP.
           IF ANO-CAP < 1999 GO TO CARGA-FEC.
           IF MM-CAP > 12 GO TO CARGA-FEC.
           IF DD-CAP = 0 OR DD-CAP > 31 GO TO CARGA-FEC.
           MOVE DD-CAP TO DIA-EMI DIA-EMI-ED.
           MOVE MM-CAP TO MES-EMI MES-EMI-ED.
           MOVE ANO-CAP TO ANO-EMI ANO-EMI-ED.
           DISPLAY FEC-EMI-AUX LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) BLINK REVERSE.
       CARGA-CUE.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-FEC.
           IF F03 MOVE CUENTA TO NUM-CAP.
           MOVE NUM-CAP TO CUENTA CUENTA-CAP CUENTA-EDI.
           MOVE 0 TO SUB-CUENTA SUB-CUENTA-CAP.
       CARGA-CUE-F.
           EXIT.
       CARGA-SUB-CUE.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO TAREA1-1.
      *-=-=-=-=-=- INVALIDA SUB CUENTAS =-=-=-=-=-=-=-=-=-=
           IF NUM-CAP NOT = 0 GO TO CARGA-SUB-CUE.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE NUM-CAP TO SUB-CUENTA.
       CARGA-NOM.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO TAREA1-1.
           MOVE ALF-CAP TO NOMBRE-C.
       CARGA-T-MOV.
           DISPLAY " 1 - DEBITO    " LINE 13 POSITION 25 REVERSE.
           DISPLAY "               " LINE 14 POSITION 25 REVERSE.
           DISPLAY " 2 - CREDITO   " LINE 15 POSITION 25 REVERSE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM MEN-ANTE-DES.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO TAREA1-1.
           IF F03 MOVE TIPO-M-CAP TO NUM-CAP.
           IF NUM-CAP < 1 OR NUM-CAP > 2 GO TO CARGA-T-MOV.
           IF NUM-CAP = 1
             DISPLAY " 1 - DEBITO    " LINE 13 POSITION 25 BLINK 
                    REVERSE.
           IF NUM-CAP = 2
             DISPLAY " 2 - CREDITO   " LINE 15 POSITION 25 BLINK 
                    REVERSE.
           MOVE NUM-CAP TO TIPO-M-CAP.
           DISPLAY TIPO-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       CARGA-T-COM.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-T-MOV.
           IF F03 MOVE TIPO-C-CAP TO NUM-CAP.
           IF NUM-CAP < 1 OR NUM-CAP > 9 GO TO CARGA-T-COM.
           MOVE NUM-CAP TO TIPO-C-CAP.
           DISPLAY TIPO-C-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
           MOVE 15 TO SW3.
           DISPLAY ABREVIA(TIPO-C-CAP) LINE TAB-LIN(SW3)
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       CARGA-SUB.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           ACCEPT SUB-T-C-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-T-COM.
           IF F03 MOVE SUB-T-C-MOV TO SUB-T-C-CAP.
           IF INSCRIPTO = "R" OR INSCRIPTO = "r"
             IF TIPO-C-CAP = 2 OR TIPO-C-CAP = 4 OR TIPO-C-CAP = 5
                IF SUB-T-C-CAP NOT = "A" GO TO  OTRO-SUB.
           IF INSCRIPTO = "N" OR INSCRIPTO = "n"
             IF TIPO-C-CAP = 2 OR TIPO-C-CAP = 4 OR TIPO-C-CAP = 5
                IF SUB-T-C-CAP NOT = "A" GO TO OTRO-SUB.
           IF INSCRIPTO = "E" OR INSCRIPTO = "e"
             IF TIPO-C-CAP = 2 OR TIPO-C-CAP = 4 OR TIPO-C-CAP = 5
                IF SUB-T-C-CAP NOT = "B" GO TO  OTRO-SUB.
           IF INSCRIPTO = "C" OR INSCRIPTO = "c"
             IF TIPO-C-CAP = 2 OR TIPO-C-CAP = 4 OR TIPO-C-CAP = 5
                IF SUB-T-C-CAP NOT = "B" GO TO  OTRO-SUB.
           IF TIPO-C-CAP = 1
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           IF TIPO-C-CAP = 3
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           IF TIPO-C-CAP = 6
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           IF TIPO-C-CAP = 7
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           IF TIPO-C-CAP = 8 
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           IF TIPO-C-CAP = 9
              IF SUB-T-C-CAP NOT = SPACES GO TO OTRO-SUB.
           DISPLAY SUB-T-C-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                SIZE TAB-SIZ(SW3) REVERSE.
           DISPLAY "-" LINE 17 POSITION 24 SIZE 1 REVERSE.
       CARGA-SUC-C.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-SUB.
           IF F03 MOVE SUC-C-CAP TO NUM-CAP.
           MOVE NUM-CAP TO SUC-C-CAP.
      *=-=-=-=-=- SOLO USA CASA CENTRAL =-=-=-=-=-=-=-=-=-=-=-
      *     IF SUC-C-CAP NOT = 0 GO TO OTRA-SUC.
      *=-=-=-=-=- USA SUCURSALES =-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     IF SUC-C-CAP NOT = SUC-EMP GO TO OTRA-SUC.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           DISPLAY SUC-C-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) REVERSE.
           DISPLAY "-" LINE 17 POSITION 29 SIZE 1 REVERSE.
       CARGA-COM.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-SUC-C.
           IF F03 MOVE COMPRO-CAP TO NUM-CAP.
           MOVE NUM-CAP  TO COMPRO-CAP.
           DISPLAY COMPRO-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
               SIZE TAB-SIZ(SW3) REVERSE.
       CARGA-DET.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-COM.
           IF F03 MOVE COMENTARIO-CAP TO ALF-CAP.
           MOVE ALF-CAP TO COMENTARIO-CAP.
           DISPLAY COMENTARIO-CAP LINE TAB-LIN(SW3)
             POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       CARGA-FEC-V.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-V-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-DET.
           IF F03 MOVE FECHA-V-INT TO FECHA-V-CAP.
           IF ANO-V-CAP < 1999 GO TO CARGA-FEC-V.
           IF TIPO-M-CAP = 1
             IF ANO-V-CAP < ANO-CAP GO TO CARGA-FEC-V.
           IF TIPO-M-CAP = 1
             IF ANO-V-CAP = ANO-CAP 
               IF MM-V-CAP < MM-CAP GO TO CARGA-FEC-V.
           IF TIPO-M-CAP = 1
             IF ANO-V-CAP = ANO-CAP
               IF MM-V-CAP = MM-CAP
                 IF DD-V-CAP < DD-CAP GO TO CARGA-FEC-V.
           MOVE FECHA-V-CAP TO FECHA-V-INT.
           MOVE ANO-V-CAP TO AA-V-EDI.
           MOVE MM-V-CAP TO MM-V-EDI.
           MOVE DD-V-CAP TO DD-V-EDI.
           DISPLAY FECHA-V-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                 SIZE TAB-SIZ(SW3) BLINK.
       CARGA-IMP.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           ACCEPT IMPORTE-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO VEO-GRABO.
           IF F02 GO TO CARGA-FEC-V.
           IF F03 MOVE IMPORTE-INT TO IMPORTE-CAP.
           MOVE IMPORTE-CAP TO IMPORTE-INT IMPORTE-EDI.
           DISPLAY IMPORTE-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       OTRO-SUB.
           DISPLAY " NO CORRESPONDE TIPO " LINE 22 POSITION 10 BLINK
                  REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP GO TO VEO-GRABO.
           IF F01 DISPLAY SPACES LINE 22 POSITION 10 SIZE 30
                  MOVE 16 TO SW3
                  DISPLAY SUB-T-C-CAP LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3) REVERSE
                  DISPLAY "-" LINE 17 POSITION 24 SIZE 1 REVERSE
                  GO TO CARGA-SUC-C.
           DISPLAY SPACES LINE 22 POSITION 10 SIZE 30.
           GO TO CARGA-SUB.
       OTRA-SUC.
           DISPLAY " ES OTRA SUCURSAL " LINE 22 POSITION 10 BLINK 
                   REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP GO TO VEO-GRABO.
           IF F01 DISPLAY SPACES LINE 22 POSITION 10 SIZE 30
                  MOVE 17 TO SW3
                  DISPLAY SUC-C-CAP LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3) REVERSE
                  DISPLAY "-" LINE 17 POSITION 29 SIZE 1 REVERSE
                  GO TO CARGA-COM.
           DISPLAY SPACES LINE 22 POSITION 10 SIZE 30.
           GO TO CARGA-SUC-C.                           
       DESP-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE RUBRO-CTA TO P-RUBRO.
           DISPLAY P-RUBRO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-CUE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE CUENTA TO CUENTA-EDI.
           DISPLAY CUENTA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
      *     MOVE 10 TO SW3.
      *     PERFORM BORRA.
      *     DISPLAY SUB-CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
      *                    SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       MEN-NO-CTA.
           MOVE "-- RUBRO NO HAB" TO MEN-1.
           MOVE "ILITADO PARA CU" TO MEN-2.
           MOVE "ENTAS CTES. -- " TO MEN-3.
           PERFORM MENS-PAN.    
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "FECHA    :" LINE 9 POSITION 5.
           DISPLAY RAYA LINE 10 POSITION 3.
           DISPLAY "CUENTA   :          SUB:  " LINE 12 POSITION 5.
           DISPLAY " 1 - DEBITO    " LINE 13 POSITION 25 REVERSE.
           DISPLAY "               " LINE 14 POSITION 25 REVERSE.
           DISPLAY " 2 - CREDITO   " LINE 15 POSITION 25 REVERSE.
           DISPLAY "TIPO MOVIMIENTO :" LINE 14 POSITION 5.
           DISPLAY "TIPO COMPROBANTE:" LINE 16 POSITION 5.
           DISPLAY "NRO. COMPROBANTE:" LINE 17 POSITION 5.
           DISPLAY "COMENTARIO      :" LINE 18 POSITION 5.
           DISPLAY "FECHA VENC.     : " LINE 19 POSITION 5.
           DISPLAY "IMPORTE         : " LINE 20 POSITION 5.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 081607 TO TAB-POS(7).
           MOVE 083030 TO TAB-POS(8).
           MOVE 121606 TO TAB-POS(9).
           MOVE 123004 TO TAB-POS(10).
           MOVE 123830 TO TAB-POS(11).
           MOVE 091612 TO TAB-POS(12).
           MOVE 142301 TO TAB-POS(13).
           MOVE 162301 TO TAB-POS(14).
           MOVE 162615 TO TAB-POS(15).
           MOVE 172301 TO TAB-POS(16).
           MOVE 172504 TO TAB-POS(17).
           MOVE 173008 TO TAB-POS(18).
           MOVE 182330 TO TAB-POS(19).
           MOVE 192310 TO TAB-POS(20).
           MOVE 202318 TO TAB-POS(21).
           MOVE 095919 TO TAB-POS(22).
           MOVE 132719 TO TAB-POS(23).
           MOVE 152719 TO TAB-POS(24).
           MOVE 172719 TO TAB-POS(25).
           MOVE 121606 TO TAB-POS(30).
           MOVE 123004 TO TAB-POS(31).
           MOVE 151606 TO TAB-POS(32).
           MOVE 153004 TO TAB-POS(33).
           MOVE 181602 TO TAB-POS(34).
           MOVE 121630 TO TAB-POS(35).
           MOVE 151630 TO TAB-POS(36).
           MOVE 211906 TO TAB-POS(37).
           MOVE 215006 TO TAB-POS(38).
           MOVE 181050 TO TAB-POS(40).
           MOVE 191050 TO TAB-POS(41).
           MOVE 201050 TO TAB-POS(42).
           MOVE 211050 TO TAB-POS(43).
           MOVE 121812 TO TAB-POS(44).
           MOVE 071908 TO TAB-POS(45).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO DE MOVI" TO TIT-2-PAN.
           MOVE "MIENTOS - " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  CARGA MOVIMIENTOS" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 17 POSITION 23 SIZE 40.
       CIERRE-A.
           CLOSE CUENTAS P-DE-CTAS MOVI PENDI EMPRESAS IMP02.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".