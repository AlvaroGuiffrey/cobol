       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALICUO.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. NOV'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "ALICUO.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "ALICUO.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA          PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(18) VALUE SPACES.
             03 FILLER       PIC X(3) VALUE "NRO".
             03 FILLER       PIC X(4) VALUE SPACES.
             03 FILLER       PIC X(12) VALUE "--FECHA---".
             03 FILLER       PIC X(9) VALUE "IVA %".
             03 FILLER       PIC X(9) VALUE "SOB.%".
             03 FILLER       PIC X(5) VALUE "PER.%".
          02 IMP-2.
             03 FILLER       PIC X(18) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(10) VALUE " TASAS IVA".
          02 IMP-3.
             03 FILLER      PIC X(18) VALUE SPACES.
             03 ALI-ED      PIC ZZZ.
             03 FILLER      PIC X(4) VALUE SPACES.
             03 DD-ED       PIC 99/.
             03 MM-ED       PIC 99/.
             03 AA-ED       PIC 9999BB.
             03 IVA-1-ED    PIC Z9,99B(4).
             03 IVA-2-ED    PIC Z9,99B(4).
             03 IVA-3-ED    PIC Z9,99.
       01 LISTO-D            PIC 999.
       01 LISTO-H            PIC 999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 ALI-CAP            PIC 9(3) VALUE 0.
       01 FEC-CAP            PIC 9(8) VALUE 0.
       01 FECHA-CAP.
          02 DD-CAP          PIC 99 VALUE 0.
          02 MM-CAP          PIC 99 VALUE 0.
          02 ANO-CAP.
             03 AAAA-CAP     PIC 99 VALUE 0.  
             03 AA-CAP       PIC 99 VALUE 0.
       01 IVA-1-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-2-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-3-CAP          PIC 9(2)V99 VALUE 0.
       01 PESO-CAP           PIC 9(4)V999.
       01 ALI-EDI            PIC ZZ9.
       01 FECHA-EDI.
          02 DD-EDI          PIC 99/.
          02 MM-EDI          PIC 99/.
          02 AA-EDI          PIC 9999.
       01 IVA-1-EDI          PIC Z9,99.
       01 IVA-2-EDI          PIC Z9,99.
       01 IVA-3-EDI          PIC Z9,99.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "ALICUO.ERR".
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
           PERFORM MEN-MODA-A.
       CARGO-MOD.
           MOVE 28 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM 
                   STOP RUN.
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       PANTA-ACTU.
           DISPLAY "EMPRESA    :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 24. 
           DISPLAY "NRO TASA   :" LINE 10 POSITION 3.
           DISPLAY "FECHA VIG. :" LINE 12 POSITION 3.
           DISPLAY "TASA GRAL. :" LINE 14 POSITION 3.
           DISPLAY "SOBRE TASA :" LINE 16 POSITION 3.
           DISPLAY "TASA PERS. :" LINE 18 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGA-ACTU.
           OPEN INPUT ALICUO.
       CARGA-NUM.
           PERFORM BORRA-PANTA-C.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           IF NUM-CAP = 0 GO TO CARGA-NUM.
           MOVE NUM-CAP TO ALI-ALI.
           PERFORM DESP-ALI.
       CARGA-FEC.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F02 GO TO CARGA-NUM.
           MOVE FEC-CAP TO FECHA-CAP.
           MOVE ANO-CAP TO AA-ALI AA-EDI.
           MOVE MM-CAP TO MM-ALI MM-EDI.
           MOVE DD-CAP TO DD-ALI DD-EDI.
           PERFORM DESP-FEC.
       LEE-ALI.
           READ ALICUO INVALID KEY GO TO ALTA.
           PERFORM DESP-I-1 THRU DESP-I-3.
           PERFORM MEN-B-M.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-NUM.
       ALTA.
           PERFORM MEN-ANTE-DES.
           PERFORM CARGA-I-1 THRU CARGA-I-3.
           CLOSE ALICUO.
           OPEN I-O ALICUO.
           WRITE REG-ALICUO INVALID KEY REWRITE REG-ALICUO.
           CLOSE ALICUO.
           OPEN INPUT ALICUO.
           GO TO CARGA-NUM.
       CARGA-I-1.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           ACCEPT IVA-1-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F02 GO TO CARGA-FEC.
           IF F03 MOVE IVA-1-ALI TO IVA-1-CAP.
           MOVE IVA-1-CAP TO IVA-1-ALI IVA-1-EDI.
           PERFORM DESP-I-1.
       CARGA-I-2.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           ACCEPT IVA-2-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F02 GO TO CARGA-I-1.
           IF F03 MOVE IVA-2-ALI TO IVA-2-CAP.
           MOVE IVA-2-CAP TO IVA-2-ALI IVA-2-EDI.
           PERFORM DESP-I-2.
       CARGA-I-3.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT IVA-3-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                           ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F02 GO TO CARGA-I-2.
           IF F03 MOVE IVA-3-ALI TO IVA-3-CAP.
           MOVE IVA-3-CAP TO IVA-3-ALI IVA-3-EDI.
           PERFORM DESP-I-3.
       GRAVO.
           CLOSE ALICUO.
           OPEN I-O ALICUO.
           WRITE REG-ALICUO INVALID KEY REWRITE REG-ALICUO.
           CLOSE ALICUO.
           OPEN INPUT ALICUO.
           GO TO CARGA-NUM.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE ALICUO GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE ALICUO GO TO INI-MOD.
           IF F01 CLOSE ALICUO
                  OPEN I-O ALICUO
                  DELETE ALICUO 
                  CLOSE ALICUO
                  OPEN INPUT ALICUO
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK ALICUO RECORD
                  GO TO CARGA-NUM.
           PERFORM MEN-NO-BAJA 50 TIMES.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ALICUO RECORD.
           GO TO CARGA-NUM.
       MODI.
           PERFORM DESP-I-1 THRU DESP-I-3.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE ALICUO GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE ALICUO GO TO INI-MOD.
           UNLOCK ALICUO RECORD.
           PERFORM CARGA-I-1 THRU CARGA-I-3.
           MOVE 28 TO SW3.
           PERFORM MEN-CONF.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F01 GO TO GRAVO.
           PERFORM MEN-NO-MODI 50 TIMES.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ALICUO RECORD.
           GO TO CARGA-NUM.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           OPEN INPUT ALICUO.
       C-N-DESDE.
           PERFORM BORRA-PANTA-C.
           PERFORM MEN-F-CONS.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF F01 CLOSE ALICUO GO TO INI-MOD.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO C-N-DESDE.
           MOVE NUM-CAP TO ALI-ALI.
       C-N-FECHA.
           MOVE 17 TO SW3.
           ACCEPT FEC-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F02 GO TO C-N-DESDE.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           MOVE FEC-CAP TO FECHA-CAP.
           MOVE ANO-CAP TO AA-EDI AA-ALI.
           MOVE MM-CAP TO MM-EDI MM-ALI.
           MOVE DD-CAP TO DD-EDI DD-ALI.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
                      INVALID KEY GO TO C-N-DESDE.
       C-N-SIGO.
           READ ALICUO NEXT WITH NO LOCK AT END GO TO C-N-DESDE.
           PERFORM DESP-ALI THRU DESP-I-3.
           MOVE 28 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ALICUO GO TO INI-MOD.
           IF F01 CLOSE ALICUO GO TO INI-MOD.
           IF F03 GO TO C-N-SIGO.
           GO TO C-N-DESDE.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA   :" LINE 7 POSITION 3.
           DISPLAY "SUC.:       -" LINE 7 POSITION 24.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-N-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT ALICUO.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
                        INVALID KEY CLOSE ALICUO IMP01
                        GO TO LIST-NUM.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO NUMERIC" TO TIT-3-EDI.
           MOVE "O DE TASAS" TO TIT-4-EDI.
           MOVE " IVA >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-ALICUO.
           IF COD2 = "N" AND EMP-ALI > NUM-EMP MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP01 ALICUO
                         IF COD2 = "N" GO TO LIST-NUM.
           MOVE ALI-ALI TO ALI-ED.
           MOVE AA-ALI TO AA-ED.
           MOVE MM-ALI TO MM-ED.
           MOVE DD-ALI TO DD-ED.
           MOVE IVA-1-ALI TO IVA-1-ED.
           MOVE IVA-2-ALI TO IVA-2-ED.
           MOVE IVA-3-ALI TO IVA-3-ED.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
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
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       LEO-ALICUO.
           READ ALICUO NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 091020 TO TAB-POS(3).
           MOVE 094720 TO TAB-POS(4).
           MOVE 102503 TO TAB-POS(5).
           MOVE 112503 TO TAB-POS(6).
           MOVE 122503 TO TAB-POS(7).
           MOVE 132503 TO TAB-POS(8).
           MOVE 142503 TO TAB-POS(9).
           MOVE 152503 TO TAB-POS(10).
           MOVE 162503 TO TAB-POS(11).
           MOVE 172503 TO TAB-POS(12)
           MOVE 182503 TO TAB-POS(13).
           MOVE 192503 TO TAB-POS(14).
           MOVE 255801 TO TAB-POS(15)
           MOVE 101503 TO TAB-POS(16).
           MOVE 121510 TO TAB-POS(17).
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
           MOVE "IVO DE TAS" TO TIT-2-PAN.
           MOVE "AS IVA-   " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO  " LINE 16 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 17 POSITION 18.
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
           MOVE NUM-CAP TO NUM-EMP EMP-ALI.
           MOVE 0 TO SUC-EMP SUC-ALI.
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
           MOVE NUM-CAP TO SUC-EMP SUC-ALI NUM-SUC-E80.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-SUC.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
       CARGA-EMP-F.
           EXIT.
       DESP-ALI.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE ALI-ALI TO ALI-EDI.
           DISPLAY ALI-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-FEC.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE AA-ALI TO AA-EDI.
           MOVE MM-ALI TO MM-EDI.
           MOVE DD-ALI TO DD-EDI.
           DISPLAY FECHA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-I-1.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE IVA-1-ALI TO IVA-1-EDI.
           DISPLAY IVA-1-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-I-2.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE IVA-2-ALI TO IVA-2-EDI.
           DISPLAY IVA-2-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-I-3.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE IVA-3-ALI TO IVA-3-EDI.
           DISPLAY IVA-3-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
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
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
