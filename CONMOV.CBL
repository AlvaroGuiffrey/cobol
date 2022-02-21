       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONMOV.
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
       COPY "MANEJO.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "CPOSTAL.SEL".
       COPY "IMP02.SEL".
       COPY "P-CTAS.SEL".
       COPY "MOVI.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "CUENTABM.REG".
       COPY "EMPREBM.REG".
       COPY "CPOSTAL.REG".
       COPY "IMP02.REG".
       COPY "P-CTAS.REG".
       COPY "MOVI.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 LIN-1.
             03 FILLER     PIC X(58) VALUE SPACES.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(70) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-2.
             03 RAZ-SOC-ED   PIC X(40).
             03 FILLER       PIC X(18) VALUE SPACES.
             03 FILLER       PIC X VALUE "|".
             03 FILLER       PIC X(19) VALUE " Se¤or/es :".
             03 NOMBRE-ED    PIC X(30).
             03 FILLER       PIC XX VALUE " (".
             03 CUENTA-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(14) VALUE ")".
             03 FILLER       PIC X(4) VALUE "   |".
          02 LIN-3.
             03 DOM-EMP-ED   PIC X(30).
             03 FILLER       PIC X(28) VALUE SPACES.
             03 FILLER       PIC X(20) VALUE "|".
             03 DIRE-ED      PIC X(30).
             03 FILLER       PIC X(21) VALUE SPACES.
             03 FILLER       PIC X VALUE "|".
          02 LIN-4.
             03 COD-L-EMP-ED PIC 9999 BLANK WHEN ZERO.
             03 FILLER       PIC XXX VALUE " -".
             03 NOM-L-EMP-ED PIC X(30).
             03 FILLER       PIC X(21) VALUE SPACES.
             03 FILLER       PIC X(20) VALUE "|".
             03 COD-L-ED     PIC 9999.
             03 FILLER       PIC XXX VALUE " -".
             03 NOM-L-ED     PIC X(30).
             03 FILLER       PIC X(14) VALUE SPACES.
             03 FILLER       PIC X VALUE "|".
          02 LIN-5.
             03 FILLER       PIC X(58) VALUE SPACES.
             03 FILLER       PIC X(20) VALUE "|".
             03 FILLER       PIC X(10) VALUE "TELEFONO:".
             03 TEL-ED       PIC X(20).
             03 FILLER       PIC X(21) VALUE SPACES.
             03 FILLER       PIC X VALUE "|".
          02 LIN-6.
             03 FILLER       PIC X(58) VALUE SPACES.
             03 FILLER       PIC X VALUE "*".
             03 FILLER       PIC X(70) VALUE ALL "-".
             03 FILLER       PIC X VALUE "*".
          02 LIN-7.
             03 FILLER       PIC X(53) VALUE SPACES.
             03 FILLER       PIC X(13) VALUE "- RESUMEN DE ".
             03 FILLER       PIC X(13) VALUE "MOVIMIENTOS -".
          02 LIN-8.
             03 FILLER       PIC X(130) VALUE ALL "-".
          02 LIN-9.
             03 FILLER       PIC X(7) VALUE SPACES.
             03 FILLER       PIC X(8) VALUE "FEC.ORIG".
             03 FILLER       PIC X(7) VALUE SPACES.
             03 FILLER       PIC X(18) VALUE "--- COMPROBANTE --".
             03 FILLER       PIC X(7) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(7) VALUE SPACES.
             03 FILLER       PIC X(17) VALUE "---- IMPORTE ----".
             03 FILLER       PIC X(7) VALUE SPACES.
             03 FILLER       PIC X(17) VALUE "----- SALDO -----".
          02 LIN-10.
             03 FILLER       PIC X(7) VALUE SPACES.
             03 DD-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MM-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 AA-ED        PIC 9999B(5).
             03 TIPO-C-ED    PIC XXB.
             03 SUB-T-C-ED   PIC X.
             03 FILLER       PIC X VALUE "-".
             03 SUC-C-ED     PIC 9999.
             03 FILLER       PIC X VALUE "-".
             03 COMPRO-ED    PIC 9(8)B(7).
             03 DET-ED       PIC X(30)B(7).
             03 IMPORTE-ED   PIC --.---.---.--9,99 BLANK WHEN ZERO.
             03 FILLER       PIC X(7) VALUE SPACES.
             03 SALDO-CON-ED PIC --.---.---.--9,99.

       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 NOMBRE-CAP        PIC X(45).
          02 ESTADO-CAP        PIC XX.
          02 CONDICION-CAP     PIC 999 VALUE 0.
          02 DIRE-CAP          PIC X(30).
          02 COD-LOCA-CAP      PIC 9(4) VALUE 0.
          02 INSCRIPTO-CAP     PIC X.
          02 ZONA-CAP          PIC 99 VALUE 0.
          02 IGAN-CAP          PIC X(15).
          02 SALDO-ANT-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-CAN-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-TOP-CAP     PIC S9(13)V99 VALUE 0. 
          02 SALDO-CAP         PIC S9(13)V99 VALUE 0.
          02 TEL-CAP           PIC X(10).
          02 PMOV-CAP          PIC 9(6) VALUE 0.
          02 PPEND-CAP         PIC 9(6) VALUE 0.
          02 ING-BRUTOS-CAP    PIC 9(8) VALUE 0.
          02 COM-1-CAP         PIC X(50).
          02 COM-2-CAP         PIC X(50).
          02 COM-3-CAP         PIC X(50).
          02 COM-4-CAP         PIC X(50).
          02 IMP-CAP           PIC S9(13)V99 VALUE 0.
          02 SALDO-CON         PIC S9(13)V99 VALUE 0.
          02 SAL-ANT-CON       PIC S9(13)V99 VALUE 0.
          02 IMPORTE-I-PEND    PIC S9(13)V99 VALUE 0.
          02 IMPORTE-I-MOV     PIC S9(13)V99 VALUE 0.
          02 ACUENTA-I-PEND    PIC S9(13)V99 VALUE 0.
          02 POS               PIC 99 VALUE 0.
       01 EDICION.
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 SUB-CUENTA-EDI    PIC ZZ BLANK WHEN ZERO.
          02 NOMBRE-EDI      PIC X(45).
          02 ESTADO-EDI        PIC XX.
          02 CONDICION-EDI     PIC ZZZ BLANK WHEN ZERO.
          02 DIRE-EDI          PIC X(30).
          02 LOCA-IDE          PIC X(29).
          02 INSCRIPTO-EDI     PIC X.
          02 ZONA-EDI          PIC ZZ BLANK WHEN ZERO.
          02 IGAN-EDI          PIC X(15).
          02 SAL-C-EDI         PIC ----.---.---.--9,99.
          02 SAL-EDI           PIC ----.---.---.--9,99.
          02 SAL-T-EDI         PIC ----.---.---.--9,99.
          02 TEL-EDI           PIC X(10).
          02 IMP-EDI           PIC ----.---.---.--9,99.
          02 SALDO-EDI         PIC ----.---.---.--9,99.
          02 FECHA-EDI         PIC X(10).
          02 COMPRO-EDI        PIC X(18).
       01 AUXILIARES.
          02 LISTO-N-D         PIC 999999 VALUE 0.
          02 LISTO-N-H         PIC 999999 VALUE 0.
          02 LISTO-Z           PIC 99.
          02 LISTO-A-D         PIC X(30).
          02 LISTO-A-H         PIC X(30).
          02 CONT-L            PIC 9(6) VALUE 0.
          02 ZONA-L            PIC 99 VALUE 0.
          02 FEC-V-CAP.
             03 DD-V-CAP       PIC 99.
             03 MM-V-CAP       PIC 99.
             03 AA-V-CAP       PIC 9999.
          02 RUBRO-L           PIC 9(7) VALUE 0.
          02 CUENTA-H          PIC 9(6) VALUE 0.
          02 SUB-CUENTA-H      PIC 9(4) VALUE 0.
          02 NOMBRE-H          PIC X(30).
          02 COD-TAREA         PIC 9 VALUE 0.
          02 RAZ-SOC-CAP       PIC X(40).
          02 DOM-EMP-CAP       PIC X(30).
          02 COD-L-EMP-CAP     PIC 9(4) VALUE 0.
          02 NOM-L-EMP-CAP     PIC X(30).
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
       COPY "ABREV.WOR".
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "CUENTA.ERR".
       COPY "P-CTA.ERR".
       COPY "CPOSTAL.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "MOVI.ERR".
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
           IF SW2 > 25 GO TO SIGA-INI.
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
           MOVE 0 TO COD-TAREA.
           IF F01 GO TO TAREA1.
           GO TO CARGO-MOD.
       TAREA1.
           OPEN INPUT CUENTAS P-DE-CTAS EMPRESAS MOVI CPOSTAL.
           OPEN OUTPUT IMP02.
           MOVE "CONSULTA MOVIMIENTOS" TO D20-CAP.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CARGA-RUB.
       TAREA1-1.
           PERFORM BORRA-PANTA-C.
           PERFORM CARGA-CUE THRU CARGA-CUE-F.
           IF NUM-CAP = 0 MOVE 0 TO CUENTA.
           IF CUENTA = 0  MOVE 0 TO SUB-CUENTA GO TO TAREA2-1.
           PERFORM CARGA-SUB-CUE.
           READ CUENTAS INVALID KEY GO TO TAREA1-1.
           GO TO ALTA.
       TAREA2-1.
           PERFORM CARGA-NOM.
           START CUENTAS KEY IS NOT < NOMBRE-C
                     INVALID KEY GO TO TAREA2-1.
       TAREA2-2.
           READ CUENTAS NEXT AT END GO TO TAREA2-1.
           PERFORM DESP-RUB.
           PERFORM DESP-CUE.
           PERFORM DESP-NOM THRU DESP-SAL-TOP.
           PERFORM MEN-CONS.
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
           PERFORM DESP-NOM THRU DESP-SAL-TOP.
           DISPLAY " CONTINUA CONSULTA ??? " LINE 23 POSITION 10
                     BLINK REVERSE.
           MOVE "<F1>SIN LISTAR " TO MEN-1.
           MOVE "<F5>SIN ROTULO " TO MEN-2.
           MOVE "<F6>CON ROTULO " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 28 TO SW3.
           ACCEPT COD1 LINE 23 POSITION 40 SIZE 1 TAB
                 ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           DISPLAY SPACES LINE 23 POSITION 10 SIZE 50.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 MOVE 0 TO COD-TAREA
                  GO TO SIGO.
           IF F05 MOVE 5 TO COD-TAREA
                  GO TO SIGO.
           IF F06 MOVE 6 TO COD-TAREA
                  GO TO SIGO.
           GO TO TAREA1-1.
       SIGO.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-2.
           MOVE 12 TO SW3.
           DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           MOVE 13 TO SW3.
           DISPLAY SUB-CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE 14 TO SW3.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
           MOVE 9 TO POS.
           PERFORM MEN-ANTE.
           MOVE 45 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-V-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE.
       SIGO-0.
           IF COD-TAREA = 0 GO TO SIGO-1.
           IF COD-TAREA = 5
            DISPLAY " COLOQUE FORM.CONTINUO " LINE 23 POSITION 10
                       BLINK REVERSE.
           IF COD-TAREA = 6
            DISPLAY " COLOQUE HOJA C/ROTULO " LINE 23 POSITION 10
                       BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           DISPLAY SPACES LINE 23 POSITION 10 SIZE 50.
       SIGO-1.
           MOVE EMP-CTA TO EMP-MOV.
           MOVE SUC-CTA TO SUC-EMP-MOV.
           MOVE RUBRO-CTA TO RUBRO-MOV.
           MOVE CUENTA TO CUENTA-MOV.
           MOVE SUB-CUENTA TO SUB-CUENTA-MOV.
           MOVE 0 TO ANO-MOV MES-MOV DIA-MOV MOVIMIENTO-MOV SALDO-CON.
           MOVE 0 TO SAL-ANT-CON CONT-L CONT-R.
           START MOVI KEY IS NOT < CLAVE-MOV 
                       INVALID KEY GO TO FINAL-C.
       SIGO-2.
           IF POS > 22 GO TO PREGUNTO.
           IF COD-TAREA = 5
              IF CONT-R > 60 GO TO SALTO-5.
           IF COD-TAREA = 6
              IF CONT-R > 28 GO TO SALTO-6.
           READ MOVI NEXT AT END GO TO FINAL-C.
           IF EMP-CTA NOT = EMP-MOV GO TO FINAL-C.
           IF SUC-CTA NOT = SUC-EMP-MOV GO TO FINAL-C.
           IF RUBRO-CTA NOT = RUBRO-MOV GO TO FINAL-C.
           IF CUENTA NOT = CUENTA-MOV GO TO FINAL-C.
           IF SUB-CUENTA NOT = SUB-CUENTA-MOV GO TO FINAL-C.
           IF ANO-MOV = AA-V-CAP
             IF MES-MOV = MM-V-CAP
               IF DIA-MOV < DD-V-CAP
                  PERFORM SUMO-SAL-A
                  GO TO SIGO-2.
           IF ANO-MOV = AA-V-CAP
             IF MES-MOV < MM-V-CAP
                  PERFORM SUMO-SAL-A
                  GO TO SIGO-2.
           IF ANO-MOV < AA-V-CAP
                  PERFORM SUMO-SAL-A
                  GO TO SIGO-2.
           IF CONT-L = 0
               PERFORM TITU THRU TITU-F.
           MOVE DIA-MOV TO DIA-VEN DD-ED.
           MOVE MES-MOV TO MES-VEN MM-ED.
           MOVE ANO-MOV TO ANO-VEN AA-ED.
      *     MOVE DIA-EMI-PEND TO DIA-EMI.
      *     MOVE MES-EMI-PEND TO MES-EMI.
      *     MOVE ANO-EMI-PEND TO ANO-EMI.
           MOVE ABREV(TIPO-C-MOV) TO ABREV-AUX.
           MOVE ABRE1 TO TIPO TIPO-C-ED.
           MOVE SUB-T-C-MOV TO SUB-TIPO SUB-T-C-ED.
           MOVE SUC-C-MOV TO SUC SUC-C-ED.
           MOVE COMPRO-MOV TO COMPRO COMPRO-ED.
           MOVE COMPRO-AUX TO COMPRO-EDI.
           MOVE COMENTARIO-MOV TO DET-ED.
           COMPUTE IMP-CAP = IMPORTE-MOV * 1.
           IF TIPO-MOV = 2
              COMPUTE IMP-CAP = IMP-CAP * ( 0 - 1 ).
           COMPUTE SALDO-CON = SALDO-CON + IMP-CAP.
           MOVE IMP-CAP TO IMP-EDI IMPORTE-ED.
           MOVE SALDO-CON TO SALDO-EDI SALDO-CON-ED.
           MOVE 18 TO SW3.
           DISPLAY FEC-VEN-AUX LINE POS POSITION TAB-COL(SW3) 
                   SIZE TAB-SIZ(SW3).
           MOVE 19 TO SW3.
           DISPLAY COMPRO-EDI LINE POS POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
      *     MOVE 20 TO SW3.
      *     DISPLAY FEC-EMI-AUX LINE POS POSITION TAB-COL(SW3)
      *             SIZE TAB-SIZ(SW3).
           MOVE 21 TO SW3.
           DISPLAY IMP-EDI LINE POS POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
           MOVE 22 TO SW3.
           DISPLAY SALDO-EDI LINE POS POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
           ADD 1 TO POS.
           IF COD-TAREA NOT = 0
             WRITE LIN-IMP02 FROM LIN-10 AFTER 1
             ADD 1 TO CONT-R.
           GO TO SIGO-2.
       SUMO-SAL-A.
           IF TIPO-MOV = 2
               COMPUTE SAL-ANT-CON = SAL-ANT-CON - IMPORTE-MOV
            ELSE COMPUTE SAL-ANT-CON = SAL-ANT-CON + IMPORTE-MOV.
            MOVE SAL-ANT-CON TO SALDO-CON.
       SALTO-5.
           WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
           ADD 1 TO CONT-R
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
           MOVE 0 TO CONT-R.
           PERFORM TITU THRU TITU-F.
           GO TO SIGO-2.
       SALTO-6.
           WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
           ADD 1 TO CONT-R.
       SALTO-61.
           DISPLAY " COLOQUE OTRA HOJA " LINE 23 POSITION 10
                BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           DISPLAY SPACES LINE 23 POSITION 10 SIZE 50.
           IF F01 GO TO SALTO-62.
           GO TO SALTO-61.
       SALTO-62.
           MOVE 0 TO CONT-R.
           PERFORM TITU THRU TITU-F.
           GO TO SIGO-2.
       TITU.
           IF COD-TAREA = 0
               GO TO TITU-99.
           IF COD-TAREA = 5
              MOVE RAZ-SOC-CAP TO RAZ-SOC-ED
              MOVE DOM-EMP-CAP TO DOM-EMP-ED
              MOVE COD-L-EMP-CAP TO COD-L-EMP-ED
              MOVE NOM-L-EMP-CAP TO NOM-L-EMP-ED.
           IF COD-TAREA = 6
              MOVE SPACES TO RAZ-SOC-ED DOM-EMP-ED NOM-L-EMP-ED
              MOVE 0 TO COD-L-EMP-ED.
           MOVE CUENTA TO CUENTA-ED.
           MOVE NOMBRE-C TO NOMBRE-ED.
           MOVE DIRE TO DIRE-ED.
           MOVE COD-LOCA TO C-POS COD-L-ED.
           MOVE NOM-POSTAL TO NOM-L-ED.
           MOVE TEL TO TEL-ED.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM LIN-1 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-2 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-3 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-4 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-5 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-6 AFTER 1.
           MOVE SPACES TO LIN-IMP02.
           WRITE LIN-IMP02 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-7 AFTER 1.
           MOVE SPACES TO LIN-IMP02.
           WRITE LIN-IMP02 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-9 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
           ADD 12 TO CONT-R.
       TITU-1.
           IF CONT-L NOT = 0
                 GO TO TITU-2.
           MOVE DD-V-CAP TO DD-ED.
           MOVE MM-V-CAP TO MM-ED.
           MOVE AA-V-CAP TO AA-ED.
           MOVE SPACES TO TIPO-C-ED SUB-T-C-ED.
           MOVE 0 TO SUC-C-ED COMPRO-ED IMPORTE-ED.
           MOVE "SALDO ANTERIOR " TO DET-ED.
           MOVE SAL-ANT-CON TO SALDO-CON-ED.
           WRITE LIN-IMP02 FROM LIN-10 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO TITU-99.
       TITU-2.
           MOVE SPACES TO TIPO-C-ED SUB-T-C-ED.
           MOVE 0 TO SUC-C-ED COMPRO-ED IMPORTE-ED.
           MOVE "TRANSPORTE DE ANTERIOR" TO DET-ED.
           WRITE LIN-IMP02 FROM LIN-10 AFTER 1.
           ADD 1 TO CONT-R.
       TITU-99.
           ADD 1 TO CONT-L.
       TITU-F.
           EXIT.
       PREGUNTO.
           DISPLAY " CONTINUA CONSULTA ??? " LINE 23 POSITION 10
                    BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           DISPLAY SPACES LINE 23 POSITION 10 SIZE 50.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 
                  PERFORM BORRA-P-C2 THRU BORRA-P-C2F
                  MOVE 9 TO POS
                  GO TO SIGO-2.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM DESP-EMP.
           PERFORM DESP-RUB.
           GO TO TAREA1-1.
       BORRA-P-C2.
           MOVE 9 TO POS.
       BORRA-P-C2-1.
           IF POS > 22 GO TO BORRA-P-C2F.
           DISPLAY SPACES LINE POS POSITION 2 SIZE 77.
           ADD 1 TO POS.
           GO TO BORRA-P-C2-1.
       BORRA-P-C2F.
           EXIT.    
       FINAL-C.
           DISPLAY " FINAL DATOS CLIENTE !!! " LINE 23 POSITION 10
                   BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM DESP-EMP.
           PERFORM DESP-RUB.
           GO TO TAREA1-1.           
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
           MOVE NUM-CAP TO NUM-EMP EMP-CTA MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           OPEN INPUT MANEJO
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE MANEJO
                GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132 RAZ-SOC-CAP.
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
           MOVE CALLE-EMP TO DOM-EMP-CAP.
           MOVE COD-LOC-EMP TO COD-L-EMP-CAP C-POS.
           READ CPOSTAL INVALID KEY MOVE "*****" TO NOM-POSTAL.
           MOVE NOM-POSTAL TO NOM-L-EMP-CAP.
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
           MOVE NUM-CAP TO P-RUBRO RUBRO-CTA.
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
       CARGA-CUE.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           IF NUM-CAP = 0 GO TO CARGA-CUE-F.
           MOVE NUM-CAP TO CUENTA.
           MOVE 0 TO SUB-CUENTA.
           READ CUENTAS INVALID KEY GO TO CARGA-SUB-CUE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       CARGA-CUE-F.
           EXIT.
       CARGA-SUB-CUE.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE.
      *-=-=-=-=-=- INVALIDA SUB CUENTAS =-=-=-=-=-=-=-=-=-=
           IF NUM-CAP NOT = 0 GO TO CARGA-SUB-CUE.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE NUM-CAP TO SUB-CUENTA.
       CARGA-NOM.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUB-CUE.
           MOVE ALF-CAP TO NOMBRE-C.
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
           DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY SUB-CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-EST.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY ESTADO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) BLINK REVERSE.
           MOVE 44 TO SW3.
           PERFORM BORRA.
           IF ESTADO = "N"
             DISPLAY "  NORMAL  " LINE TAB-LIN(SW3) 
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           IF ESTADO = "A"
             DISPLAY "** ATRASO **" LINE TAB-LIN(SW3)
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           IF ESTADO = "M" 
             DISPLAY "<< MOROSO >>" LINE TAB-LIN(SW3)
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-OTROS.
           MOVE COD-LOCA TO C-POS.
           READ CPOSTAL INVALID KEY MOVE "******" TO NOM-POSTAL.
           DISPLAY DIRE LINE 19 POSITION 19.
           DISPLAY COD-LOCA LINE 20 POSITION 19.
           DISPLAY NOM-POSTAL LINE 20 POSITION 25 SIZE 25.
           DISPLAY TEL LINE 21 POSITION 19.
       DESP-SAL.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE SALDO TO SAL-EDI.
           DISPLAY SAL-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) BLINK REVERSE.
       DESP-SAL-CAN.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE SALDO-CAN TO SAL-C-EDI.
           DISPLAY SAL-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-SAL-TOP.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           MOVE SALDO-TOP TO SAL-T-EDI.
           DISPLAY SAL-T-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       MEN-NO-CTA.
           MOVE "-- RUBRO NO HAB" TO MEN-1.
           MOVE "ILITADO PARA CU" TO MEN-2.
           MOVE "ENTAS CTES. -- " TO MEN-3.
           PERFORM MENS-PAN.    
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "CUENTA   :          SUB:  " LINE 10 POSITION 5.
           DISPLAY "NOMBRE   : " LINE 11 POSITION 5.
           DISPLAY "ESTADO   :" LINE 12 POSITION 5.
           DISPLAY "SALDO ACTUAL       :" LINE 13 POSITION 5 REVERSE.
           DISPLAY "SALDO CIERRE ANTER.:" LINE 15 POSITION 5.
           DISPLAY "TOPE DE CREDITO    :" LINE 17 POSITION 5.
           DISPLAY "DOMICILIO: " LINE 19 POSITION 5.
           DISPLAY "LOCALIDAD: " LINE 20 POSITION 5.
           DISPLAY "TELEFONO : " LINE 21 POSITION 5.
       PANTA-2.
           DISPLAY "CUENTA   :          SUB:  " LINE 6 POSITION 5.
           DISPLAY "NOMBRE   :" LINE 6 POSITION 35.
           DISPLAY "DESDE FECHA: " LINE 7 POSITION 5 REVERSE.
           DISPLAY "FEC.ORIGEN ----COMPROBANTE---         " LINE 8
                    POSITION 2.
           DISPLAY " ------IMPORTE------ -------SALDO-------" LINE 8
                    POSITION 38.
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
           MOVE 101606 TO TAB-POS(9).
           MOVE 103004 TO TAB-POS(10).
           MOVE 111630 TO TAB-POS(11).
           MOVE 061606 TO TAB-POS(12).
           MOVE 063004 TO TAB-POS(13).
           MOVE 064630 TO TAB-POS(14).
           MOVE 141610 TO TAB-POS(15).
           MOVE 151602 TO TAB-POS(16).
           MOVE 121601 TO TAB-POS(17).
           MOVE 090210 TO TAB-POS(18).
           MOVE 091318 TO TAB-POS(19).
           MOVE 093008 TO TAB-POS(20).
           MOVE 093919 TO TAB-POS(21).
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
           DISPLAY "<F1>  CONSULTA MOVIMIENTOS" LINE 14 POSITION 18.
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
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 44 TO SW3.
           PERFORM BORRA.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 40 TO SW3.
           PERFORM BORRA.
           MOVE 41 TO SW3.
           PERFORM BORRA.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 19 POSITION 19 SIZE 40.
           DISPLAY SPACES LINE 20 POSITION 19 SIZE 40.
           DISPLAY SPACES LINE 21 POSITION 19 SIZE 40.
       CIERRE-A.
           CLOSE CUENTAS P-DE-CTAS MOVI EMPRESAS CPOSTAL IMP02.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".