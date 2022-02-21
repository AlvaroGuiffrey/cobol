       IDENTIFICATION DIVISION.
       PROGRAM-ID. INFOPE1.
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
       COPY "INFOPE1.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "INFOPE1.REG".
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
             03 FILLER       PIC X(9) VALUE "PERIODO: ".
             03 MES-ED       PIC 99/.
             03 ANO-ED       PIC 99.
             03 DETA-ED      PIC X(20).
          02 IMP-2.
             03 FILLER       PIC X(10) VALUE SPACES.
             03 FILLER       PIC X(8) VALUE "ARTIC.".
             03 FILLER       PIC X(6) VALUE "MARCA".
             03 FILLER       PIC X(16) VALUE "---UNIDADES---".
             03 FILLER       PIC X(16) VALUE "----KILOS-----".
             03 FILLER       PIC X(17) VALUE "---IMPORTE----". 
          02 IMP-3.
             03 FILLER      PIC X(10) VALUE SPACES.
             03 NUM-ED      PIC ZZZZZ9BB.
             03 MAR-ED      PIC 9999BB.
             03 UNI-ED      PIC ----.--9,999BB.
             03 KILO-ED     PIC ----.--9,999BB.
             03 IMP-ED      PIC ------.---.--9,99.
          02 IMP-4.
             03 FILLER      PIC X(10) VALUE SPACES.
             03 FILLER      PIC X(14) VALUE "TOTALES --->".
             03 T-UNI-ED      PIC ----.--9,999BB.
             03 T-KILO-ED     PIC ----.--9,999BB.
             03 T-IMP-ED      PIC ------.---.--9,99.
       01 T-UNI             PIC S9(6)V999 VALUE 0.
       01 T-KILO            PIC S9(6)V999 VALUE 0.
       01 T-IMP             PIC S9(12)V99 VALUE 0.
       01 CTA-D-CAP         PIC 9(6) VALUE 0.
       01 CTA-H-CAP         PIC 9(6) VALUE 0.
       01 RUBRO-CAP         PIC 9(7) VALUE 0.
       01 PER-CAP.
          02 MES-PER-CAP    PIC 99 VALUE 0.
          02 ANO-PER-CAP    PIC 99 VALUE 0.
       01 NUM-ART-CAP       PIC 9(6) VALUE 0.
       01 PER-EDI.
          02 MES-PER-EDI    PIC 99.
          02 FILLER         PIC X VALUE "/".
          02 ANO-PER-EDI    PIC 99.                        
       01 CTA-INT            PIC 9(6) VALUE 0.
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
       COPY "INFOPE1.ERR".
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
           MOVE " INFORME -1- OPER. " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       PANTA-INFO.
           DISPLAY "EMPRESA    :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 24.
           DISPLAY "RUBRO      :" LINE 10 POSITION 3. 
           DISPLAY "CTA. DESDE :" LINE 12 POSITION 3.
           DISPLAY "CTA. HASTA :" LINE 14 POSITION 3.
           DISPLAY "PERIODO    :" LINE 16 POSITION 3.
           DISPLAY " (MMAA) " LINE 16 POSITION 24 REVERSE.
           DISPLAY "ARTICULO   :" LINE 18 POSITION 3.
           DISPLAY " INGRESE 0 LISTA TODOS " LINE 18 POSITION 24 
                                   REVERSE.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           DISPLAY SPACES LINE 10 POSITION 25 SIZE 20.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
      *     PERFORM CHEQUEA-CLAVE1.
      *     IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGA-INFO.
           OPEN INPUT OPERA INFOPE1.
       CARGA-RUB.
           PERFORM BORRA-PANTA-C.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO CARGA-EMP.
      *     IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO RUBRO-CAP.
           IF RUBRO-CAP = 0 
             DISPLAY " INFORME UNIFICADO " LINE 10 POSITION 25
                             BLINK REVERSE.
       CARGA-CTA-D.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           MOVE NUM-CAP TO CTA-D-CAP.
       CARGA-CTA-H.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CTA-D.
           IF NUM-CAP = 0 DISPLAY " INFORME CONSOLIDADO " LINE 14
                      POSITION 30 BLINK REVERSE.
           MOVE NUM-CAP TO CTA-H-CAP.
           IF CTA-H-CAP < CTA-D-CAP GO TO CARGA-CTA-H.
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
           IF MES-PER-CAP < 1 OR MES-PER-CAP > 12
               GO TO CARGA-PER.
           MOVE MES-PER-CAP TO MES-PER-EDI.
           MOVE ANO-PER-CAP TO ANO-PER-EDI.
           DISPLAY PER-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).           
       CARGA-ART.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-PER.
           MOVE NUM-CAP TO NUM-ART-CAP.
       PRE-INFO.
           MOVE RUBRO-CAP TO RUB-OPE.
           MOVE CTA-D-CAP TO CTA-OPE.
           MOVE 0 TO SUB-CTA-OPE DD-OPE.
           MOVE MES-PER-CAP TO MM-OPE.
           MOVE ANO-PER-CAP TO AA-OPE.
           CLOSE INFOPE1.
           OPEN OUTPUT INFOPE1.
           CLOSE INFOPE1.
           OPEN INPUT INFOPE1.
           START OPERA KEY IS NOT < CLAVE-OPERA1
                    INVALID KEY PERFORM CIERRE-A GO TO INI-MOD.
           DISPLAY " PASO 1 - PREPARA INFORME " LINE 20 POSITION 10
                     BLINK REVERSE.
       LEE-OPERA.
           READ OPERA NEXT AT END GO TO IMPRE-INFO.
           IF EMP-OPE NOT = NUM-EMP GO TO LEE-OPERA.
           IF SUC-OPE NOT = SUC-EMP GO TO LEE-OPERA.
           IF REN-OPE = 0 GO TO LEE-OPERA.
           IF RUBRO-CAP > 0
              IF RUB-OPE > RUBRO-CAP GO TO IMPRE-INFO.
           IF RUBRO-CAP > 0
              IF RUB-OPE NOT = RUBRO-CAP GO TO LEE-OPERA.
           IF CTA-H-CAP NOT = 0
             IF CTA-OPE > CTA-H-CAP GO TO IMPRE-INFO.
           IF AA-OPE NOT = ANO-PER-CAP GO TO LEE-OPERA.
           IF MM-OPE NOT = MES-PER-CAP GO TO LEE-OPERA.
           IF NUM-ART-CAP NOT = 0
               IF NUM-ART-CAP NOT = NUM-OPE GO TO LEE-OPERA.
           MOVE EMP-OPE TO EMP-IO1.
           MOVE SUC-OPE TO SUC-IO1.
           IF RUBRO-CAP = 0
              MOVE 0 TO RUB-IO1
              ELSE MOVE RUB-OPE TO RUB-IO1.
           IF CTA-H-CAP = 0
             MOVE 0 TO CTA-IO1
             ELSE MOVE CTA-OPE TO CTA-IO1.
           MOVE SUB-CTA-OPE TO SUB-CTA-IO1.
           MOVE AA-OPE TO AA-IO1.
           MOVE MM-OPE TO MM-IO1.
           MOVE ART-OPE TO ART-IO1.
           READ INFOPE1 INVALID KEY MOVE 0 TO UNI-IO1 KILO-IO1
                                              IMP-IO1.
           IF TIP-OPE = 1
             COMPUTE UNI-IO1 = UNI-IO1 + UNI-OPE
             COMPUTE KILO-IO1 = KILO-IO1 + KILO-OPE
             COMPUTE IMP-IO1 = IMP-IO1 + IMP-OPE.
           IF TIP-OPE = 2
             COMPUTE UNI-IO1 = UNI-IO1 - UNI-OPE
             COMPUTE KILO-IO1 = KILO-IO1 - KILO-OPE
             COMPUTE IMP-IO1 = IMP-IO1 - IMP-OPE.
           CLOSE INFOPE1.
           OPEN I-O INFOPE1.
           REWRITE REG-INFOPE1  INVALID KEY WRITE REG-INFOPE1.
           CLOSE INFOPE1.
           OPEN INPUT INFOPE1.  
           GO TO LEE-OPERA.

       IMPRE-INFO.
           DISPLAY " PASO 2 - IMPRIME INFORME " LINE 20 POSITION 10
                         BLINK REVERSE.
           MOVE NUM-EMP TO EMP-IO1.
           MOVE SUC-EMP TO SUC-IO1.
           MOVE RUBRO-CAP TO RUB-IO1.
           MOVE CTA-D-CAP TO CTA-IO1.
           MOVE 0 TO SUB-CTA-IO1.
           MOVE MES-PER-CAP TO MM-IO1.
           MOVE ANO-PER-CAP TO AA-IO1.
           MOVE NUM-ART-CAP TO NUM-IO1.
           MOVE 0 TO MAR-IO1 CTA-INT.
       L-N-POSI.
           START INFOPE1 KEY IS NOT < CLAVE-INFOPE1
                        INVALID KEY PERFORM CIERRE-A
                        GO TO INI-MOD.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI COD3.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO DE OPER" TO TIT-3-EDI.
           MOVE "AC.P/ARTIC" TO TIT-4-EDI.
           MOVE "ULOS >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           OPEN OUTPUT IMP01.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ INFOPE1 NEXT AT END PERFORM CIERRE-A
                              PERFORM SUB-TITU THRU SUB-TITU-F
                              PERFORM PIE-P
                              CLOSE IMP01
                              GO TO INI-MOD.
           IF EMP-IO1 > NUM-EMP MOVE "F" TO COD3.
           IF SUC-IO1 > SUC-EMP MOVE "F" TO COD3.
           IF RUB-IO1 NOT = RUBRO-CAP MOVE "F" TO COD3.
           IF CTA-IO1 < CTA-D-CAP GO TO L-LEO.
           IF CTA-IO1 > CTA-H-CAP MOVE "F" TO COD3.
           IF MM-IO1 NOT = MES-PER-CAP GO TO L-LEO.
           IF AA-IO1 NOT = ANO-PER-CAP GO TO L-LEO.

           IF COD3 = "F" PERFORM SUB-TITU THRU SUB-TITU-F
                         PERFORM PIE-P
                         CLOSE IMP01 
                         GO TO INI-MOD.
           IF CTA-INT NOT = CTA-IO1 PERFORM SUB-TITU THRU
                                         SUB-TITU-F.
           MOVE NUM-IO1 TO NUM-ED.
           MOVE MAR-IO1 TO MAR-ED.
           MOVE UNI-IO1 TO UNI-ED.
           MOVE KILO-IO1 TO KILO-ED.
           MOVE IMP-IO1 TO IMP-ED.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
             COMPUTE T-UNI = T-UNI + UNI-IO1.
             COMPUTE  T-KILO = T-KILO + KILO-IO1.
             COMPUTE T-IMP = T-IMP + IMP-IO1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
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
           IF CTA-IO1 = CTA-INT GO TO SUB-TITU-F.
           MOVE CTA-IO1 TO CUENTA-ED CTA-INT.
           MOVE RUB-IO1 TO RUBRO-ED.
           MOVE MM-IO1 TO MES-ED.
           MOVE AA-IO1 TO ANO-ED.
           MOVE SPACES TO DETA-ED.
           PERFORM ESPA 1 TIMES.           
           WRITE LIN-IMP01 FROM IMP-1 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
       SUB-TITU-F.
           EXIT.
       SUB-TI.
           MOVE CTA-IO1 TO CUENTA-ED CTA-INT.
           MOVE RUB-IO1 TO RUBRO-ED.
           MOVE MM-IO1 TO MES-ED.
           MOVE AA-IO1 TO ANO-ED.
           MOVE SPACES TO DETA-ED.
           IF RUBRO-CAP = 0 MOVE " - UNIFICADO - " TO DETA-ED.
           IF CTA-IO1 = 0 MOVE " - CONSOLIDADO - " TO DETA-ED. 
           PERFORM ESPA 1 TIMES.           
           WRITE LIN-IMP01 FROM IMP-1 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
       SUB-TI-F.
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
           IF RUBRO-CAP = 0 OR CTA-D-CAP = 0 
                 PERFORM SUB-TI THRU SUB-TI-F.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       CIERRE-A.
           CLOSE INFOPE1 OPERA.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 091020 TO TAB-POS(3).
           MOVE 094720 TO TAB-POS(4).
           MOVE 101607 TO TAB-POS(5).
           MOVE 121606 TO TAB-POS(6).
           MOVE 141606 TO TAB-POS(7).
           MOVE 161605 TO TAB-POS(8).
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
           DISPLAY "<F5>  LIST.P/CLIENTES  " LINE 14 POSITION 18.
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
