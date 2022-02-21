       IDENTIFICATION DIVISION.
       PROGRAM-ID. INFOPE3.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. DIC'96.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "OPERA.SEL".
       COPY "INFOPE3.SEL".
       COPY "LINEAS.SEL".
       COPY "PANTA.SEL".
       COPY "NUMERO.SEL".
       COPY "IMP02.SEL".
       COPY "ART01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "INFOPE3.REG".
       COPY "ART01.REG".
       COPY "LINEAS.REG".
       COPY "NUMERO.REG".
       COPY "OPERA.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA          PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 FILLER       PIC X(7) VALUE "RUBRO:".
             03 RUBRO-ED     PIC ZZZZZZ9.
             03 FILLER       PIC X(6) VALUE " ++++ ".
             03 FILLER       PIC X(9) VALUE "PERIODO: ".
             03 DIA-ED       PIC 99/.
             03 MES-ED       PIC 99/.
             03 ANO-ED       PIC 99.
          02 IMP-11.
             03 CONDI-ED     PIC X(20).
          02 IMP-2.
             03 FILLER       PIC 99 COMP-4 VALUE 15.
             03 FILLER       PIC X(8) VALUE "ARTIC.".
             03 FILLER       PIC X(6) VALUE "MARCA".
             03 FILLER       PIC X(32) VALUE " DETALLE".
             03 FILLER       PIC X(11) VALUE "RUBRO VTA".
             03 FILLER       PIC X(16) VALUE "---UNIDADES---".
             03 FILLER       PIC X(16) VALUE "----KILOS-----".
             03 FILLER       PIC X(17) VALUE "---IMPORTE----". 
          02 IMP-3.
             03 FILLER      PIC 99 COMP-4 VALUE 15.
             03 NUM-ED      PIC ZZZZZ9BB.
             03 MAR-ED      PIC 9999BB.
             03 DET-ED      PIC X(30)BB.
             03 RUBRO-V-ED  PIC 9(6)B(5).   Š             03 UNI-ED      PIC ----.--9,999BB.
             03 KILO-ED     PIC ----.--9,999BB.
             03 IMP-ED      PIC ------.---.--9,99.
          02 IMP-4. 
             03 FILLER        PIC 99 COMP-4 VALUE 15.
             03 FILLER        PIC X(12) VALUE "RUBRO VTA.:".
             03 T-RUB-V-ED    PIC 9(6).
             03 FILLER        PIC X(15) VALUE SPACES.
             03 FILLER        PIC X(24) VALUE "TOTALES AL CREDITO --->".
             03 T-UNI-ED      PIC ----.--9,999BB.
             03 T-KILO-ED     PIC ----.--9,999BB.
             03 T-IMP-ED      PIC ------.---.--9,99.
          02 IMP-5.
             03 FILLER        PIC 99 COMP-4 VALUE 15.
             03 FILLER        PIC X(24) VALUE "TOTALES AL DEBITO --->".
             03 T-D-IMP-ED    PIC ------.---.--9,99.
       01 T-UNI             PIC S9(6)V999 VALUE 0.
       01 T-KILO            PIC S9(6)V999 VALUE 0.
       01 T-IMP             PIC S9(12)V99 VALUE 0.
       01 T-D-IMP           PIC S9(12)V99 VALUE 0.
       01 CTA-D-CAP         PIC 9(6) VALUE 0.
       01 CTA-H-CAP         PIC 9(6) VALUE 0.
       01 RUBRO-CAP         PIC 9(7) VALUE 0.
       01 PER-CAP.
          02 DIA-PER-CAP    PIC 99 VALUE 0.
          02 MES-PER-CAP    PIC 99 VALUE 0.
          02 ANO-PER-CAP    PIC 99 VALUE 0.
       01 NUM-ART-CAP       PIC 9(6) VALUE 0.
       01 PER-EDI.
          02 DIA-PER-EDI    PIC 99.
          02 FILLER         PIC X VALUE "/".
          02 MES-PER-EDI    PIC 99.
          02 FILLER         PIC X VALUE "/".
          02 ANO-PER-EDI    PIC 99.                        
       01 CTA-INT            PIC 9(6) VALUE 0.
       01 RUB-INT            PIC 9(7) VALUE 0.
       01 CONDI-INT          PIC 99 VALUE 0.
       01 RUB-V-INT          PIC 9(7) VALUE 0.
       01 LISTO-D            PIC 999.
       01 LISTO-H            PIC 999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 ALI-CAP            PIC 9(3) VALUE 0.
       01 FEC-CAP            PIC 9(6) VALUE 0.
       01 FECHA-CAP.
          02 DD-CAP          PIC 99 VALUE 0.
          02 MM-CAP          PIC 99 VALUE 0.
          02 AA-CAP          PIC 99 VALUE 0.
       01 IVA-1-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-2-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-3-CAP          PIC 9(2)V99 VALUE 0.
       01 PESO-CAP           PIC 9(4)V999.
       01 ALI-EDI            PIC ZZ9.
       01 FECHA-EDI.
          02 DD-EDI          PIC 99/.
          02 MM-EDI          PIC 99/.
          02 AA-EDI          PIC 99.
       01 IVA-1-EDI          PIC Z9,99.
       01 IVA-2-EDI          PIC Z9,99.
       01 IVA-3-EDI          PIC Z9,99.
       01 QQ                 PIC X.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "OPERA.ERR".
       COPY "INFOPE3.ERR".
       COPY "LINEAS.ERR".
       COPY "NUMERO.ERR".
       COPY "ART01.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE.

       PANTA-INI.
           ADD 1 TO SW2.
      *     IF SW2 > 25 GO TO SIGA-CLAVE.
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
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM 
                   STOP RUN.
           IF F05 GO TO INFO.
           GO TO CARGO-MOD.
       INFO.
           MOVE " INFORME -3- OPER. " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       PANTA-INFO.
           DISPLAY "EMPRESA    :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 24.
           DISPLAY "PERIODO    :" LINE 10 POSITION 3.
           DISPLAY " (DDMMAA) " LINE 10 POSITION 28 REVERSE.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
      *     PERFORM CHEQUEA-CLAVE1.
      *     IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGA-INFO.
           OPEN INPUT OPERA INFOPE3 NUMERO ART01 LINEAS.
       CARGA-PER.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           ACCEPT PER-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 DISPLAY SPACES LINE 14 POSITION 30 SIZE 40
                  PERFORM CIERRE-A GO TO INI-MOD.
           IF PER-CAP = 0 GO TO CARGA-PER.
           IF MES-PER-CAP < 1 OR MES-PER-CAP > 12
               GO TO CARGA-PER.
           IF DIA-PER-CAP < 1 OR DIA-PER-CAP > 31
               GO TO CARGA-PER.
           MOVE DIA-PER-CAP TO DIA-PER-EDI.
           MOVE MES-PER-CAP TO MES-PER-EDI.
           MOVE ANO-PER-CAP TO ANO-PER-EDI.
           DISPLAY PER-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).           
       PRE-INFO.
           MOVE 0 TO RUB-OPE.
           MOVE DIA-PER-CAP TO DD-OPE.
           MOVE MES-PER-CAP TO MM-OPE.
           MOVE ANO-PER-CAP TO AA-OPE.
           CLOSE INFOPE3.
           OPEN OUTPUT INFOPE3.
           CLOSE INFOPE3.
           OPEN INPUT INFOPE3.
           START OPERA KEY IS NOT < CLAVE-OPERA1
                    INVALID KEY PERFORM CIERRE-A GO TO INI-MOD.
           DISPLAY " PASO 1 - PREPARA INFORME " LINE 20 POSITION 10
                     BLINK REVERSE.
       LEE-OPERA.
           READ OPERA NEXT AT END GO TO IMPRE-INFO.
           IF EMP-OPE NOT = NUM-EMP GO TO LEE-OPERA.
           IF SUC-OPE NOT = SUC-EMP GO TO LEE-OPERA.
           IF REN-OPE = 0 GO TO LEE-OPERA.
           IF AA-OPE NOT = ANO-PER-CAP GO TO LEE-OPERA.
           IF MM-OPE NOT = MES-PER-CAP GO TO LEE-OPERA.
           IF DD-OPE NOT = DIA-PER-CAP GO TO LEE-OPERA.
           MOVE EMP-OPE TO EMP-IO3 EMP-ART01.
           MOVE SUC-OPE TO SUC-IO3.
           MOVE RUB-OPE TO RUB-IO3.
           IF CONDI-OPE < 50 MOVE 1 TO CONDI-IO3
                  ELSE MOVE 2 TO CONDI-IO3.
           MOVE AA-OPE TO AA-IO3.
           MOVE MM-OPE TO MM-IO3.
           MOVE DD-OPE TO DD-IO3.
           MOVE ART-OPE TO ART-IO3 COD-ART01.
           READ ART01 INVALID KEY MOVE 0 TO RUB-V-IO3
                                  GO TO LEE-INFOPE3.
           MOVE LIN-1-ART01 TO COD-1-LIN.
           MOVE LIN-2-ART01 TO COD-2-LIN.
           MOVE LIN-3-ART01 TO COD-3-LIN.
           MOVE LIN-4-ART01 TO COD-4-LIN.
           READ LINEAS INVALID KEY MOVE 0 TO RUB-V-IO3
                                   GO TO LEE-INFOPE3.
           MOVE RUB-4-LIN TO RUB-V-IO3.
       LEE-INFOPE3.
           READ INFOPE3 INVALID KEY MOVE 0 TO UNI-IO3 KILO-IO3
                                              IMP-IO3.
           IF TIP-OPE = 1
             COMPUTE UNI-IO3 = UNI-IO3 + UNI-OPE
             COMPUTE KILO-IO3 = KILO-IO3 + KILO-OPE
             COMPUTE IMP-IO3 = IMP-IO3 + IMP-OPE.
           IF TIP-OPE = 2
             COMPUTE UNI-IO3 = UNI-IO3 - UNI-OPE
             COMPUTE KILO-IO3 = KILO-IO3 - KILO-OPE
             COMPUTE IMP-IO3 = IMP-IO3 - IMP-OPE.
           CLOSE INFOPE3.
           OPEN I-O INFOPE3.
           REWRITE REG-INFOPE3  INVALID KEY WRITE REG-INFOPE3.
           CLOSE INFOPE3.
           OPEN INPUT INFOPE3.  
           GO TO LEE-OPERA.

       IMPRE-INFO.
           DISPLAY " PASO 2 - IMPRIME INFORME " LINE 20 POSITION 10
                         BLINK REVERSE.
           MOVE NUM-EMP TO EMP-IO3.
           MOVE SUC-EMP TO SUC-IO3.
           MOVE 0 TO RUB-IO3.
           MOVE DIA-PER-CAP TO DD-IO3.
           MOVE MES-PER-CAP TO MM-IO3.
           MOVE ANO-PER-CAP TO AA-IO3.
           MOVE 0 TO NUM-IO3 RUB-V-IO3.
           MOVE 0 TO MAR-IO3 RUB-INT RUB-V-INT.
       L-N-POSI.
           START INFOPE3 KEY IS NOT < CLAVE-INFOPE3
                        INVALID KEY PERFORM CIERRE-A
                        GO TO INI-MOD.
           MOVE 0 TO CONT-P CONT-I CONT-R T-UNI T-KILO T-IMP T-D-IMP.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI COD3.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO DE OPER" TO TIT-3-EDI.
           MOVE "AC.POR DIA" TO TIT-4-EDI.
           MOVE " >>>      " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           OPEN OUTPUT IMP02.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ INFOPE3 NEXT AT END PERFORM CIERRE-A
                              PERFORM SUB-TITU THRU SUB-TITU1
                              PERFORM PIE-P
                              CLOSE IMP02
                              GO TO INI-MOD.
           IF CONDI-IO3 = 1 MOVE "<<< CONTADO >>>" TO CONDI-ED
               ELSE MOVE "<<< CREDITO >>> " TO CONDI-ED.
           IF RUB-INT = 0
               MOVE RUB-IO3 TO RUB-INT RUBRO-ED
               MOVE CONDI-IO3 TO CONDI-INT
               MOVE RUB-V-IO3 TO RUB-V-INT
               MOVE DD-IO3 TO DIA-ED
               MOVE MM-IO3 TO MES-ED
               MOVE AA-IO3 TO ANO-ED
               WRITE LIN-IMP02 FROM IMP-1 AFTER ADVANCING 1
               WRITE LIN-IMP02 FROM IMP-11 AFTER ADVANCING 1
               ADD 2 TO CONT-R.
           IF RUB-IO3 NOT = RUB-INT
               PERFORM SUB-TITU THRU SUB-TITU-F.
           IF CONDI-IO3 NOT = CONDI-INT
               PERFORM SUB-TITU THRU SUB-TITU-F.
           IF RUB-V-IO3 NOT = RUB-V-INT
               PERFORM SUB-TITU.
           MOVE NUM-IO3 TO NUM-ED COD-NUM.
           READ NUMERO INVALID KEY MOVE SPACES TO DET-NUM.
           MOVE DET-NUM TO DET-ED.
           MOVE RUB-V-IO3 TO RUBRO-V-ED.
           MOVE MAR-IO3 TO MAR-ED.
           MOVE UNI-IO3 TO UNI-ED.
           MOVE KILO-IO3 TO KILO-ED.
           MOVE IMP-IO3 TO IMP-ED.
           WRITE LIN-IMP02 FROM IMP-3 AFTER ADVANCING 1.
             COMPUTE T-UNI = T-UNI + UNI-IO3.
             COMPUTE  T-KILO = T-KILO + KILO-IO3.
             COMPUTE T-IMP = T-IMP + IMP-IO3.
             COMPUTE T-D-IMP = T-D-IMP + IMP-IO3.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
       SUB-TITU.
           MOVE T-UNI TO T-UNI-ED.
           MOVE T-KILO TO T-KILO-ED.
           MOVE T-IMP TO T-IMP-ED.
           MOVE RUB-V-INT TO T-RUB-V-ED.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO T-UNI T-KILO T-IMP.
           MOVE RUB-V-IO3 TO RUB-V-INT.
           MOVE CONDI-IO3 TO CONDI-INT.
           PERFORM ESPA 1 TIMES.
       SUB-TITU1.
           MOVE T-D-IMP TO T-D-IMP-ED.
           WRITE LIN-IMP02 FROM IMP-5 AFTER 1.
           PERFORM ESPA 1 TIMES.
           MOVE RUB-IO3 TO RUB-INT RUBRO-ED.
           MOVE 0 TO T-D-IMP.
       SUB-TITU2.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-11 AFTER 1.
           ADD 2 TO CONT-R.           
       SUB-TITU-F.
           EXIT.          
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           MOVE ANO TO AA-E132.
           MOVE MES TO MM-E132.
           MOVE DIA TO DD-E132.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E132.
           MOVE MINU TO MI-E132.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           PERFORM ESPA 1 TIMES.
           ADD 7 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       CIERRE-A.
           CLOSE INFOPE3 OPERA ART01 NUMERO LINEAS.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 091020 TO TAB-POS(3).
           MOVE 094720 TO TAB-POS(4).
           MOVE 101607 TO TAB-POS(5).
           MOVE 121606 TO TAB-POS(6).
           MOVE 141606 TO TAB-POS(7).
           MOVE 101608 TO TAB-POS(8).
           MOVE 181606 TO TAB-POS(9).
           MOVE 152503 TO TAB-POS(10).
           MOVE 162503 TO TAB-POS(11).
           MOVE 172503 TO TAB-POS(12)
           MOVE 182503 TO TAB-POS(13).
           MOVE 192503 TO TAB-POS(14).
           MOVE 255801 TO TAB-POS(15)
           MOVE 101503 TO TAB-POS(16).
           MOVE 121508 TO TAB-POS(17).
           MOVE 141505 TO TAB-POS(18).
           MOVE 161505 TO TAB-POS(19).
           MOVE 181505 TO TAB-POS(20).
           MOVE 201509 TO TAB-POS(21).
           MOVE 143320 TO TAB-POS(22).
           MOVE 153320 TO TAB-POS(23).
           MOVE 163320 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071504 TO TAB-POS(29).
           MOVE 073002 TO TAB-POS(30).
           MOVE 073830 TO TAB-POS(31).
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO DE OPE" TO TIT-2-PAN.
           MOVE "RACIONES- " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LIST.POR DIA  " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 15 POSITION 18.
       CARGA-EMP.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-EMP.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
       CARGA-SUC.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E132.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-SUC.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
       CARGA-EMP-F.
           EXIT.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 8 TO SW3.
           PERFORM BORRA.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
