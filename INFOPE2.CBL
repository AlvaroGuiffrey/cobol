       IDENTIFICATION DIVISION.
       PROGRAM-ID. INFOPE2.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. AGOï95.
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
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "OPERA.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA          PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "RUBRO:".
             03 RUBRO-ED     PIC ZZZZZZ9.
             03 FILLER       PIC X(7) VALUE "  CTA:".
             03 CUENTA-ED    PIC ZZZZZ9.
             03 FILLER       PIC X(6) VALUE " ++++ ".
             03 FILLER       PIC X(9) VALUE " DESDE : ".
             03 DIA-ED       PIC 99/.
             03 MES-ED       PIC 99/.
             03 ANO-ED       PIC 99.
          02 IMP-2.
             03 FILLER       PIC X(10) VALUE "TP-----FAC".
             03 FILLER       PIC X(11) VALUE "TURA-- RG ".
             03 FILLER       PIC X(7) VALUE "ARTIC.".
             03 FILLER       PIC X(6) VALUE "MARCA".
             03 FILLER       PIC X(15) VALUE "---UNIDADES---".
             03 FILLER       PIC X(15) VALUE "----KILOS-----".
             03 FILLER       PIC X(16) VALUE "---IMPORTE----". 
          02 IMP-3.
             03 T-COM-ED      PIC 9B.
             03 SUB-T-C-ED    PIC X.
             03 FILLER        PIC X VALUE "-".
             03 SUC-C-ED      PIC 9(4).
             03 FILLER        PIC X VALUE "-".
             03 COM-ED        PIC 9(8)B.
             03 REN-ED        PIC 99B.
             03 NUM-ED        PIC ZZZZZ9B.
             03 MAR-ED        PIC 9999BB.
             03 UNI-ED        PIC ----.--9,999B.
             03 KILO-ED       PIC ----.--9,999B.
             03 IMP-ED        PIC -----.---.--9,99.
          02 IMP-4.
             03 FILLER      PIC X(20) VALUE SPACES.
             03 FILLER      PIC X(14) VALUE "TOTALES --->".
             03 T-UNI-ED      PIC ----.--9,999B.
             03 T-KILO-ED     PIC ----.--9,999B.
             03 T-IMP-ED      PIC -----.---.--9,99.
          02 IMP-5.
             03 FILLER      PIC X(20) VALUE SPACES.
             03 FILLER      PIC X(14) VALUE "SUBTOTAL -->".
             03 S-UNI-ED      PIC ----.--9,999B.
             03 S-KILO-ED     PIC ----.--9,999B.
             03 S-IMP-ED      PIC -----.---.--9,99.
       01 T-UNI             PIC S9(6)V999 VALUE 0.
       01 T-KILO            PIC S9(6)V999 VALUE 0.
       01 T-IMP             PIC S9(12)V99 VALUE 0.
       01 S-UNI             PIC S9(6)V999 VALUE 0.
       01 S-KILO            PIC S9(6)V999 VALUE 0.
       01 S-IMP             PIC S9(12)V99 VALUE 0.
       01 CTA-D-CAP         PIC 9(6) VALUE 0.
       01 CTA-H-CAP         PIC 9(6) VALUE 0.
       01 RUBRO-CAP         PIC 9(7) VALUE 0.
       01 PER-CAP.
          02 DIA-PER-CAP    PIC 99 VALUE 0.
          02 MES-PER-CAP    PIC 99 VALUE 0.
          02 ANO-PER-CAP    PIC 99 VALUE 0.
       01 NUM-ART-CAP       PIC 9(6) VALUE 0.
       01 PER-EDI.
          02 DIA-PER-EDI    PIC 99/. 
          02 MES-PER-EDI    PIC 99/.
          02 ANO-PER-EDI    PIC 99.                        
       01 CTA-INT            PIC 9(6) VALUE 0.
       01 FAC-INT            PIC 9(8) VALUE 0.
       01 FAC-FAC            PIC 9(8) VALUE 0.
       01 IND-FAC            PIC X VALUE SPACES.
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
       COPY "IMP01.ERR".
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
           MOVE " INFORME -2- OPER. " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       PANTA-INFO.
           DISPLAY "EMPRESA    :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 24.
           DISPLAY "RUBRO      :" LINE 10 POSITION 3.
           DISPLAY " PROCESA 1 CUENTA POR VEZ " LINE 12 POSITION 15
                         REVERSE. 
           DISPLAY "CUENTA     :" LINE 14 POSITION 3.
           DISPLAY "FECHA DESDE:" LINE 16 POSITION 3.
           DISPLAY " (DDMMAA) " LINE 16 POSITION 26 REVERSE.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
      *     PERFORM CHEQUEA-CLAVE1.
      *     IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGA-INFO.
           OPEN INPUT OPERA.
       CARGA-RUB.
           PERFORM BORRA-PANTA-C.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO CARGA-EMP.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO RUBRO-CAP.
       CARGA-CTA-H.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           IF NUM-CAP = 0 GO TO CARGA-CTA-H.
           MOVE NUM-CAP TO CTA-H-CAP.
       CARGA-PER.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           ACCEPT PER-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 DISPLAY SPACES LINE 14 POSITION 30 SIZE 40
                  GO TO CARGA-CTA-H.
           IF PER-CAP = 0 GO TO CARGA-PER.
           IF DIA-PER-CAP < 1 OR DIA-PER-CAP > 31 
                          GO TO CARGA-PER.
           IF MES-PER-CAP < 1 OR MES-PER-CAP > 12
               GO TO CARGA-PER.
           MOVE DIA-PER-CAP TO DIA-PER-EDI.
           MOVE MES-PER-CAP TO MES-PER-EDI.
           MOVE ANO-PER-CAP TO ANO-PER-EDI.
           DISPLAY PER-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).           
      * CARGA-ART.
      *     MOVE 9 TO SW3.
      *     PERFORM BORRA.
      *     PERFORM CAP-NUM.
      *     IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
      *     IF F02 GO TO CARGA-PER.
      *     MOVE NUM-CAP TO NUM-ART-CAP.
       PRE-INFO.
           MOVE RUBRO-CAP TO RUB-OPE.
           MOVE CTA-H-CAP TO CTA-OPE.
           MOVE 0 TO SUB-CTA-OPE DD-OPE.
           MOVE DIA-PER-CAP TO DD-OPE.
           MOVE MES-PER-CAP TO MM-OPE.
           MOVE ANO-PER-CAP TO AA-OPE.
           START OPERA KEY IS NOT < CLAVE-OPERA1
                    INVALID KEY PERFORM CIERRE-A GO TO INI-MOD.
           DISPLAY " PASO 1 - IMPRIME INFORME " LINE 20 POSITION 10
                     BLINK REVERSE.
           MOVE 0 TO CONT-P CONT-I CONT-R CTA-INT FAC-INT.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI COD3.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO DE OPER" TO TIT-3-EDI.
           MOVE "AC.P/ARTIC" TO TIT-4-EDI.
           MOVE "ULOS >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           OPEN OUTPUT IMP01.
       LEE-OPERA.
           READ OPERA NEXT AT END GO TO FINAL-INFO.
           IF EMP-OPE NOT = NUM-EMP GO TO LEE-OPERA.
           IF SUC-OPE NOT = SUC-EMP GO TO LEE-OPERA.
           IF RUB-OPE NOT = RUBRO-CAP GO TO LEE-OPERA.
           IF CTA-OPE NOT = CTA-H-CAP GO TO LEE-OPERA.
           IF AA-OPE < ANO-PER-CAP GO TO LEE-OPERA.
           IF MM-OPE < MES-PER-CAP GO TO LEE-OPERA.
           IF DD-OPE < DIA-PER-CAP GO TO LEE-OPERA.
           IF REN-OPE = 0 GO TO LEE-OPERA.
           IF IND-OPE = "F" OR IND-OPE = "E"
                     GO TO LEE-OPERA.
           GO TO IMPRE-INFO.

       FINAL-INFO.
           PERFORM S-TITU THRU S-TITU-F.
           PERFORM SUB-TITU THRU SUB-TITU-F.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
           CLOSE IMP01.
           PERFORM CIERRE-A.
           GO TO INI-MOD.
       IMPRE-INFO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           IF FAC-INT NOT = COM-OPE PERFORM S-TITU THRU
                                            S-TITU-F.
           IF CTA-INT NOT = CTA-OPE PERFORM SUB-TITU THRU
                                         SUB-TITU-F.
           MOVE NUM-OPE TO NUM-ED.
           MOVE MAR-OPE TO MAR-ED.
           MOVE UNI-OPE TO UNI-ED.
           MOVE KILO-OPE TO KILO-ED.
           MOVE IMP-OPE TO IMP-ED.
           MOVE T-COM-OPE TO T-COM-ED.
           MOVE SUB-T-C-OPE TO SUB-T-C-ED.
           MOVE SUC-C-OPE TO SUC-C-ED.
           MOVE COM-OPE TO COM-ED.
           MOVE REN-OPE TO REN-ED.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
           IF TIP-OPE = 1
            COMPUTE T-UNI = T-UNI + UNI-OPE
            COMPUTE S-UNI = S-UNI + UNI-OPE
            COMPUTE T-KILO = T-KILO + KILO-OPE
            COMPUTE S-KILO = S-KILO + KILO-OPE
            COMPUTE T-IMP = T-IMP + IMP-OPE
            COMPUTE S-IMP = S-IMP + IMP-OPE.
           IF TIP-OPE = 2
            COMPUTE T-UNI = T-UNI - UNI-OPE
            COMPUTE S-UNI = S-UNI - UNI-OPE
            COMPUTE T-KILO = T-KILO - KILO-OPE
            COMPUTE S-KILO = S-KILO - KILO-OPE
            COMPUTE T-IMP = T-IMP - IMP-OPE
            COMPUTE S-IMP = S-IMP - IMP-OPE.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO LEE-OPERA.
       SUB-TITU.
           IF T-UNI = 0 AND T-KILO = 0 AND T-IMP = 0
                    GO TO SUB-TITU1.
           MOVE T-UNI TO T-UNI-ED.
           MOVE T-KILO TO T-KILO-ED.
           MOVE T-IMP TO T-IMP-ED.
           WRITE LIN-IMP01 FROM IMP-4 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO T-UNI T-KILO T-IMP.
       SUB-TITU1.
           IF CTA-OPE = CTA-INT GO TO SUB-TITU-F.
           MOVE CTA-OPE TO CUENTA-ED CTA-INT.
           MOVE COM-OPE TO FAC-INT.
           MOVE RUB-OPE TO RUBRO-ED.
           MOVE MM-OPE TO MES-ED.
           MOVE AA-OPE TO ANO-ED.
           MOVE DD-OPE TO DIA-ED. 
           PERFORM ESPA 1 TIMES.           
           WRITE LIN-IMP01 FROM IMP-1 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
       SUB-TITU-F.
           EXIT.
       S-TITU.
           MOVE COM-OPE TO FAC-INT.
           IF S-UNI = 0 AND S-KILO = 0 AND S-IMP = 0
                  GO TO S-TITU-F.
            MOVE S-UNI TO S-UNI-ED.
            MOVE S-KILO TO S-KILO-ED.
            MOVE S-IMP TO S-IMP-ED.
            WRITE LIN-IMP01 FROM IMP-5 AFTER 1.
            ADD 1 TO CONT-R.
            MOVE 0 TO S-UNI S-KILO S-IMP.
       S-TITU-F.
           EXIT.               
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E80.
           MOVE ANO TO AA-E80.
           MOVE MES TO MM-E80.
           MOVE DIA TO DD-E80.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E80.
           MOVE MINU TO MI-E80.
           WRITE LIN-IMP01 FROM NORMAL AFTER 0.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       CIERRE-A.
           CLOSE  OPERA.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 091020 TO TAB-POS(3).
           MOVE 094720 TO TAB-POS(4).
           MOVE 101607 TO TAB-POS(5).
           MOVE 121606 TO TAB-POS(6).
           MOVE 141606 TO TAB-POS(7).
           MOVE 161608 TO TAB-POS(8).
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
           DISPLAY "<F5>  LIST.SIN FACTURAR  " LINE 14 POSITION 18.
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
           MOVE RAZ-SOC-EMP TO EMP-E80.
       CARGA-SUC.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-SUC.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
       CARGA-EMP-F.
           EXIT.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
