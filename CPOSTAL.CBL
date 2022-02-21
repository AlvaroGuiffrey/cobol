000000 IDENTIFICATION DIVISION.
       PROGRAM-ID. CPOSTAL.
       AUTHOR. GUIFFREY ALVARO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. PC-XT.
       OBJECT-COMPUTER. PC-XT.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CPOSTAL.SEL".
       COPY "IMP01.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CPOSTAL.REG".
       COPY "IMP01.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       01  GRANDE.
           02 BB           PIC 99 COMP-4 VALUE 18.   
       01  LINEA1.
           02  FILLER      PIC X(80) VALUE ALL "=".
       01  LINEA2.
           02  FILLER      PIC X(80) VALUE SPACES.
       01  LINEA3.
           02  FILLER      PIC X(16) VALUE SPACES.
           02  FILLER      PIC X(8) VALUE "LISTADO ".
           02  TIT-ED5     PIC X(10).
           02  FILLER      PIC X(12) VALUE " DE CODIGOS ".
           02  FILLER      PIC X(8) VALUE "POSTALES".
           02  FILLER      PIC X(6) VALUE SPACES. 
           02  FILLER      PIC X(12) VALUE "VIGENTES AL:".
           02  DIA-E       PIC 99/.
           02  MES-E       PIC 99/.
           02  ANIO-E      PIC 99.
       01  LINEA4.
           02  FILLER      PIC X(4) VALUE SPACES.
           02  FILLER      PIC X(4) VALUE "C.P.".
           02  FILLER      PIC X(4) VALUE SPACES.
           02  FILLER      PIC X(22) VALUE "-------------- CIUDAD ".
           02  FILLER      PIC X(13) VALUE "-------------".
           02  FILLER      PIC X(4) VALUE SPACES.
           02  FILLER      PIC X(20) VALUE "---- PROVINCIA -----".
       01  LINEA5.
           02  FILLER      PIC X(4) VALUE SPACES.
           02  CPOSTAL-E   PIC 9999BBBB.
           02  NOM-P-E     PIC X(35)BBBB.
           02  PRO-P-E     PIC X(20).
       01  CAMPOS-DE-CAPTURA.
           03  FECHA-CAP.
               05  DIA-CAP  PIC 99.
               05  MES-CAP  PIC 99.
               05  ANIO-CAP PIC 99.
           03  COD-CAP      PIC 9999.
           03  NOM-CAP      PIC X(35).
           03  PRO-CAP      PIC X(20).
           03  AUXI-CAP     PIC XX.
       01  CAMPOS-AUXILIARES.
           03 C-POS-T      PIC 9(4).
           03 C-POS-1-T    PIC 99.
           03 NOM-POSTAL-T PIC X(35).
           03 MENS-A-1     PIC X(60).
           03 MENS-A-2     PIC X(60).
           03 MOD-ACT-A    PIC X(25).
       COPY "VARIOS.WOR".
000000 PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CPOSTAL.ERR".
       COPY "IMP01.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO SW2.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
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
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
           IF F05 GO TO LIST-NUM.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       ACTU.
           OPEN INPUT CPOSTAL.
           MOVE " ALTA, BAJA Y MODIF. " TO D20-CAP.
           PERFORM MODA-PAN.
       PANTA-CAP.
           PERFORM BORRA-PANTA.
           DISPLAY "CODIGO     :" LINE 10 POSITION 5.
           DISPLAY "LOCALIDAD  :" LINE 13 POSITION 5.
           DISPLAY "PROVINCIA  :" LINE 16 POSITION 5.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       CARGUE-COD.
           PERFORM BORRA-P-CAP.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 10 POSITION 26 SIZE 2
           ACCEPT C-POS LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) TAB NO BEEP
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE CPOSTAL GO TO INI-MOD.
           IF C-POS = 0 GO TO CARGUE-COD.

      *        SENA

           ACCEPT C-POS-1 LINE 10 POSITION 26 NO BEEP TAB CONVERT
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           


           READ CPOSTAL INVALID KEY GO TO CARGUE-NOM.
           PERFORM DESP-NOM.
           PERFORM DESP-PRO.
           PERFORM MEN-B-M.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F09 GO TO CARGUE-NOM.
           IF F10 GO TO BORRE.
           GO TO CARGUE-COD.                                  
       BORRE.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 CLOSE CPOSTAL
                  OPEN I-O CPOSTAL
                  DELETE CPOSTAL
                  CLOSE CPOSTAL
                  OPEN INPUT CPOSTAL
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK CPOSTAL RECORD
                  GO TO CARGUE-COD.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK CPOSTAL RECORD.
           GO TO CARGUE-COD.
       CARGUE-NOM.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COD.
           IF F03 GO TO CARGUE-PRO.
           MOVE ALF-CAP TO NOM-POSTAL.
       CARGUE-PRO.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-NOM.
           IF F03 GO TO GRAVO.
           MOVE ALF-CAP TO PRO-POSTAL.
       GRAVO.
           CLOSE CPOSTAL.
           OPEN I-O CPOSTAL.
           WRITE REG-C-POSTAL INVALID KEY REWRITE REG-C-POSTAL.
           CLOSE CPOSTAL.
           OPEN INPUT CPOSTAL.
           GO TO CARGUE-COD.
       DESP-COD.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           DISPLAY C-POS LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
           DISPLAY C-POS-1 LINE 10 POSITION 26.
       DESP-NOM. 
           MOVE 4 TO SW3.
           PERFORM BORRA.                                         
           MOVE NOM-POSTAL TO NOM-CAP.
           DISPLAY NOM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-PRO.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE PRO-POSTAL TO PRO-CAP
           DISPLAY PRO-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 132035 TO TAB-POS(4).
           MOVE 162020 TO TAB-POS(5).
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
           MOVE "  -ARCHIVO" TO TIT-1-PAN.
           MOVE " DE CODIGO" TO TIT-2-PAN.
           MOVE " POSTAL-  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA, BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA  " LINE 15 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABETICA" LINE 16 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO   " LINE 17 POSITION 18.
           DISPLAY "<F6>  LISTADO ALFABETICO " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU     " LINE 19 POSITION 18.
       BORRA-P-CAP.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       CIERRE-A.
           CLOSE CPOSTAL.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-CAP.
           OPEN INPUT CPOSTAL.
       C-N-DESDE.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           MOVE NUM-CAP TO C-POS.
           MOVE 0 TO C-POS-1.
       C-N-POSI.
           START CPOSTAL KEY IS NOT < CLAVE-C-POSTAL
                      INVALID KEY PERFORM CIERRE-A 
                              GO TO INI-MOD.
           MOVE "N" TO COD2.
           GO TO C-LEO.
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-CAP.
           OPEN INPUT CPOSTAL.
       C-A-DESDE.
           MOVE  4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           MOVE ALF-CAP TO NOM-POSTAL NOM-POSTAL-T.
           CALL "COB1203.COB" USING C-POS-T C-POS-1-T NOM-POSTAL-T.
           CANCEL "COB1203.COB".
           MOVE NOM-POSTAL-T TO NOM-POSTAL.
       C-A-POSI.
           START CPOSTAL KEY IS NOT < NOM-POSTAL
                    INVALID KEY PERFORM CIERRE-A
                                GO TO INI-MOD.
           MOVE "A" TO COD2.
           GO TO C-LEO.
       C-LEO.
           PERFORM MEN-CONS.
           READ CPOSTAL NEXT WITH NO LOCK AT END GO TO F-CONS.
           PERFORM DESP-COD THRU DESP-PRO.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F03 GO TO C-LEO.
           GO TO F-CONS.
       F-CONS.
           IF COD2 = "A" GO TO C-A-DESDE. 
           IF COD2 = "N" GO TO C-N-DESDE.
           PERFORM CIERRE-A.
           GO TO INI-MOD.  
       LIST-NUM.
           OPEN INPUT CPOSTAL.
           OPEN OUTPUT IMP01.
           MOVE 1 TO C-POS.
           MOVE 0 TO C-POS-1
           START CPOSTAL KEY NOT < CLAVE-C-POSTAL INVALID KEY
                                     CLOSE CPOSTAL IMP01
                                     GO TO INI-MOD.
           MOVE 0 TO CONT-R.
           MOVE "-NUMERICO-" TO TIT-ED5.
           PERFORM BORRA-PANTA.
           DISPLAY " ESTOY LISTANDO ... !!! " LINE 15 POSITION 27
                              BLINK REVERSE.
           PERFORM TITU.
           GO TO LISTO1.
       LIST-ALF.
           OPEN INPUT CPOSTAL.
           OPEN OUTPUT IMP01.
           MOVE " " TO NOM-POSTAL.
           START CPOSTAL KEY NOT < NOM-POSTAL INVALID KEY
                             CLOSE CPOSTAL IMP01
                             GO TO INI-MOD.
           MOVE 0 TO CONT-R.
           PERFORM BORRA-PANTA.
           DISPLAY " ESTOY LISTANDO ... !!! " LINE 15 POSITION 27
                             BLINK REVERSE.
           MOVE "ALFABETICO" TO TIT-ED5.
           PERFORM TITU.
           GO TO LISTO1.
       LISTO1.
           READ CPOSTAL NEXT WITH NO LOCK AT END 
                   WRITE LIN-IMP01 FROM LINEA1 AFTER 1
                   ADD 1 TO CONT-R
                   CLOSE CPOSTAL IMP01
                   GO TO INI-MOD.
           MOVE C-POS TO CPOSTAL-E.
           MOVE NOM-POSTAL TO NOM-P-E.
           MOVE PRO-POSTAL TO PRO-P-E.
           WRITE LIN-IMP01 FROM LINEA5 AFTER 1.
           ADD 1 TO CONT-R.
           IF CONT-R = 66 PERFORM ESPA 3 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           GO TO LISTO1.
       ESPA.
           WRITE LIN-IMP01 FROM LINEA2 AFTER 1.
           ADD 1 TO CONT-R.
       TITU.
           WRITE LIN-IMP01 FROM GRANDE AFTER 0.
           MOVE ANO TO ANIO-E.
           MOVE MES TO MES-E.
           MOVE DIA TO DIA-E.
           PERFORM ESPA 3 TIMES.
           WRITE LIN-IMP01 FROM LINEA1 AFTER 1.
           WRITE LIN-IMP01 FROM LINEA3 AFTER 1.
           WRITE LIN-IMP01 FROM LINEA1 AFTER 1.
           WRITE LIN-IMP01 FROM LINEA4 AFTER 1.
           WRITE LIN-IMP01 FROM LINEA2 AFTER 1.
           ADD 5 TO CONT-R.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
