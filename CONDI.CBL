       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDI.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. ENE'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "CONDI.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "CONDI.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(10) VALUE SPACES.
             03 FILLER       PIC X(3) VALUE "NRO".
             03 FILLER       PIC X(4) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "----- DETA".
             03 FILLER       PIC X(10) VALUE "LLE ------".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(1) VALUE "T".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(11) VALUE "PORCENTAJE".
          02 IMP-2.
             03 FILLER       PIC X(10) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(12) VALUE " CONDICIONES".
          02 IMP-3.
             03 FILLER      PIC X(10) VALUE SPACES.
             03 NUME-ED      PIC ZZ9.
             03 FILLER      PIC X(4) VALUE SPACES.
             03 NOM-ED      PIC X(20)B(5).
             03 TIP-ED     PIC 9B(5).
             03 POR-ED     PIC ZZ9,99.
       01 LISTO-D            PIC 9999.
       01 LISTO-H            PIC 9999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 NUME-CAP            PIC 9(2).
       01 NOM-CAP            PIC X(20).
       01 POR-CAP           PIC 9(3)V99.
       01 TIP-CAP           PIC 9.
       01 NUME-EDI            PIC ZZ9.
       01 NOM-EDI            PIC X(20).
       01 POR-EDI            PIC ZZ9,99.
       01 TIP-EDI            PIC 9.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "CONDI.ERR".
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
           MOVE 28 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP GO TO FFFFF.
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
           DISPLAY "SUC:       -" LINE 7 POSITION 25.
           DISPLAY "NRO CONDIC.:" LINE 10 POSITION 3.
           DISPLAY "DETALLE    :" LINE 12 POSITION 3.
           DISPLAY "TIPO COND. :" LINE 14 POSITION 3.
           DISPLAY " 0 - DESCUENTO " LINE 13 POSITION 20 LOW REVERSE.
           DISPLAY "               " LINE 14 POSITION 20 LOW REVERSE.
           DISPLAY " 1 - RECARGO   " LINE 15 POSITION 20 LOW REVERSE.
           DISPLAY "PORCENTAJE :" LINE 16 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           OPEN INPUT CONDI.
       CARGA-NUM.
           PERFORM BORRA-PANTA-C.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-NUM.
           MOVE NUM-CAP TO NUM-CONDI.
       LEE-ENV.
           READ CONDI WITH NO LOCK INVALID KEY GO TO ALTA.
           PERFORM DESP-DES THRU DESP-POR.
           PERFORM MEN-B-M.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-NUM.
       ALTA.
           PERFORM MEN-ANTE.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           ACCEPT D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF F02 GO TO CARGA-NUM.
           MOVE D20-CAP TO NOM-CONDI.
       CARGA-TIP.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           ACCEPT TIP-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF F02 GO TO ALTA.
           IF F03 PERFORM DESP-TIP GO TO CARGA-POR.
           IF TIP-CAP > 1 GO TO CARGA-TIP.
           MOVE TIP-CAP TO TIP-CONDI.
           PERFORM DESP-TIP.
       CARGA-POR.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           ACCEPT POR-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF F02 GO TO CARGA-TIP.
           IF F03 PERFORM DESP-POR GO TO GRAVO.
           MOVE POR-CAP TO POR-CONDI.
           PERFORM DESP-POR.
       GRAVO.
           CLOSE CONDI.
           OPEN I-O CONDI.
           WRITE REG-CONDI INVALID KEY REWRITE REG-CONDI.
           CLOSE CONDI.
           OPEN INPUT CONDI.
           GO TO CARGA-NUM.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE CONDI GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE CONDI GO TO INI-MOD.
           IF F01 CLOSE CONDI
                  OPEN I-O CONDI
                  DELETE CONDI 
                  CLOSE CONDI
                  OPEN INPUT CONDI
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK CONDI RECORD
                  GO TO CARGA-NUM.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK CONDI RECORD.
           GO TO CARGA-NUM.
       MODI.
           PERFORM DESP-DES THRU DESP-POR.
           UNLOCK CONDI RECORD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE CONDI GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE CONDI GO TO INI-MOD.    
           GO TO ALTA.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
       C-N-DESDE.
           PERFORM BORRA-PANTA-C.
           PERFORM MEN-F-CONS.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF F01 CLOSE CONDI GO TO INI-MOD.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO C-N-DESDE.
           MOVE NUM-CAP TO NUM-CONDI.
           START CONDI KEY IS NOT < CLAVE-CONDI
                      INVALID KEY GO TO C-N-DESDE.
       C-N-SIGO.
           READ CONDI NEXT WITH NO LOCK AT END GO TO C-N-DESDE.
           MOVE 16 TO SW3.
           DISPLAY NUM-CONDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
           PERFORM DESP-DES THRU DESP-POR.
           MOVE 28 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE CONDI GO TO INI-MOD.
           IF F03 GO TO C-N-SIGO.
           GO TO C-N-DESDE.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA    :" LINE 7 POSITION 3.
           DISPLAY "SUC:       -" LINE 7 POSITION 25.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "HASTA: " LINE 9 POSITION 40.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-N-DESDE.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP  GO TO INI-MOD.
           IF F01  GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D NUM-CONDI.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           MOVE "N" TO COD2.
       L-N-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT CONDI.
           START CONDI KEY IS NOT < CLAVE-CONDI
                        INVALID KEY CLOSE CONDI IMP01
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO NUMERIC" TO TIT-3-EDI.
           MOVE "O DE CONDI" TO TIT-4-EDI.
           MOVE "CIONES >>>" TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-ENVA.
           IF COD2 = "N" AND NUM-CONDI > LISTO-H MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP01 CONDI
                         IF COD2 = "N" GO TO L-N-DESDE.
           MOVE NUM-CONDI TO NUME-ED.
           MOVE NOM-CONDI TO NOM-ED.
           MOVE TIP-CONDI TO TIP-ED.
           MOVE POR-CONDI TO POR-ED.
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
       LEO-ENVA.
           READ CONDI NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
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
           MOVE 121520 TO TAB-POS(17).
           MOVE 141501 TO TAB-POS(18).
           MOVE 161506 TO TAB-POS(19).
           MOVE 181502 TO TAB-POS(20).
           MOVE 201509 TO TAB-POS(21).
           MOVE 143320 TO TAB-POS(22).
           MOVE 153320 TO TAB-POS(23).
           MOVE 163320 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(29).
           MOVE 073002 TO TAB-POS(30).
           MOVE 073830 TO TAB-POS(31).
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO DE CON" TO TIT-2-PAN.
           MOVE "DICIONES- " TO TIT-3-PAN.
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
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-EMP.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
       CARGA-EMP-F.
           EXIT.   
       DESP-DES.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE NOM-CONDI TO D20-CAP.
           DISPLAY D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-TIP.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE TIP-CONDI TO TIP-EDI.
           DISPLAY TIP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-POR.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE POR-CONDI TO POR-EDI.
           DISPLAY POR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
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
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".

       FFFFF.
           EXIT PROGRAM.
       FFFFF0.
           STOP RUN.
