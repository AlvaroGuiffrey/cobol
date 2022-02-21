       IDENTIFICATION DIVISION.
       PROGRAM-ID. MARCAS.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. JUL'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(6) VALUE "NUMERO".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "----- DETA".
             03 FILLER       PIC X(10) VALUE "LLE ------".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(7) VALUE " MARCAS".
          02 IMP-3.
             03 FILLER       PIC X(6) VALUE SPACES.
             03 COD-MAR-ED   PIC ZZZZ.
             03 FILLER       PIC X(6) VALUE SPACES.
             03 DET-MAR-ED   PIC X(20)B(6).
             03 OBS-MAR-ED   PIC X(10).
       01 LISTO-D            PIC 9999.
       01 LISTO-H            PIC 9999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "MARCAS.ERR".
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
           MOVE 0 TO SW2.
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
           IF ESCAP GO TO FFF.
                    
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
           IF F05 GO TO LIST-NUM.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       ACTU.
           OPEN INPUT MARCAS.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 23.
           DISPLAY "CODIGO   :" LINE 12 POSITION 3.
           DISPLAY "DETALLE  :" LINE 17 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 CLOSE MARCAS GO TO INI-MOD.
       CARGA-COD.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE MARCAS GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-COD.
           MOVE NUM-CAP TO COD-MAR.
       LEE-MAR.
           READ MARCAS INVALID KEY GO TO ALTA.
           PERFORM DESP-DES.
           PERFORM MEN-B-M.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-COD.
       ALTA.
           PERFORM MEN-ANTE.
           MOVE 17 TO SW3.
           ACCEPT D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP CLOSE MARCAS GO TO INI-MOD.
           IF F02 GO TO CARGA-COD.
           MOVE D20-CAP TO DET-MAR.
           CLOSE MARCAS.
           OPEN I-O MARCAS.
           WRITE REG-MARCAS INVALID KEY DISPLAY "ERROR 01".
           CLOSE MARCAS.
           OPEN INPUT MARCAS.
           GO TO CARGA-COD.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE MARCAS GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE MARCAS GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE MARCAS GO TO INI-MOD.
           IF F01 CLOSE MARCAS
                  OPEN I-O MARCAS
                  DELETE MARCAS 
                  CLOSE MARCAS
                  OPEN INPUT MARCAS
                  UNLOCK MARCAS RECORD
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  GO TO CARGA-COD.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK MARCAS RECORD
           GO TO CARGA-COD.
       MODI.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-D20.
           IF ESCAP CLOSE MARCAS GO TO INI-MOD.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE MARCAS GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE MARCAS GO TO INI-MOD.
           IF F01 MOVE D20-CAP TO DET-MAR
                  CLOSE MARCAS
                  OPEN I-O MARCAS
                  REWRITE REG-MARCAS
                  CLOSE MARCAS
                  OPEN INPUT MARCAS
             ELSE PERFORM MEN-NO-MODI.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK MARCAS RECORD.
           GO TO CARGA-COD.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 23.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "CODIGO" LINE 9 POSITION 23.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 33.
           OPEN INPUT EMPRESAS .
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       C-N-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           ACCEPT COD-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) 
                          NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F01 GO TO INI-MOD.
           IF ESCAP GO TO INI-MOD.
           PERFORM BORRA-PANTA-C.
       C-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
       C-N-POSI.
           OPEN INPUT MARCAS.
           START MARCAS KEY IS NOT < CLAVE-MARCAS
                         INVALID KEY GO TO C-N-DESDE.
           MOVE 0 TO SW2.
           MOVE 4 TO SW3.
           MOVE "N" TO COD2.
           GO TO C-LEO.
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 23.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "CODIGO" LINE 9 POSITION 23.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 33.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       C-A-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           ACCEPT DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
       C-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       C-A-POSI.
           OPEN INPUT MARCAS.
           START MARCAS KEY IS NOT < DET-MAR
                        INVALID KEY GO TO C-A-DESDE.
           MOVE 0 TO SW2.
           MOVE 4 TO SW3.
           MOVE "A" TO COD2.
           GO TO C-LEO.
       C-LEO.
           ADD 1 TO SW2.
           ADD 1 TO SW3.
           IF SW2 > 10 PERFORM MEN-CONS
                       MOVE 1 TO SW2
                       MOVE 28 TO SW3
                       PERFORM CAP-COD1
                       IF ESCAP CLOSE MARCAS GO TO INI-MOD
                       IF F03 GO TO C-SIGOLEO
                       PERFORM MEN-F-CONS
                       IF ESCAP CLOSE MARCAS GO TO INI-MOD
                       IF F01 GO TO F-CONS.
       C-SIGOLEO.
           IF SW3 > 14 MOVE 5 TO SW3.
           IF SW2 = 1 PERFORM BORRA-PANTA-C.
           MOVE "X" TO COD3.
           PERFORM LEO-MAR.
           IF COD3 = "F" CLOSE MARCAS
                  IF COD2 = "A" GO TO C-A-DESDE 
                  ELSE GO TO C-N-DESDE.
           IF COD2 = "A" IF DET-MAR > LISTO-H-A CLOSE MARCAS
                                                GO TO C-A-DESDE.
           IF COD2 = "N" IF COD-MAR > LISTO-H CLOSE MARCAS
                                              GO TO C-N-DESDE.
           DISPLAY COD-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           COMPUTE SW1 = SW3 + 13.
           IF COD-MAR = SPACES
                DISPLAY "INHAB" LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           DISPLAY DET-MAR LINE TAB-LIN(SW1) POSITION TAB-COL(SW1).
           GO TO C-LEO.
       F-CONS.
           CLOSE MARCAS.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 23.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
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
           PERFORM BORRA-PANTA-C.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-MAR.
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
           OPEN INPUT MARCAS.
           START MARCAS KEY IS NOT < CLAVE-MARCAS
                        INVALID KEY CLOSE MARCAS IMP01
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO NUMERIC" TO TIT-3-EDI.
           MOVE "O DE MARCA" TO TIT-4-EDI.
           MOVE "S >>>     " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 23.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO  INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO  INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-A-DESDE.
           PERFORM BORRA-PANTA-C.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO LISTO-D-A DET-MAR.
       L-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       L-A-POSI.
           OPEN INPUT MARCAS.
           OPEN OUTPUT IMP01.
           START MARCAS KEY IS NOT < DET-MAR
                        INVALID KEY CLOSE MARCAS IMP01
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-I CONT-P CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE "  <<< LIST" TO TIT-2-EDI.
           MOVE "ADO ALFABE" TO TIT-3-EDI.
           MOVE "TICO DE MA" TO TIT-4-EDI.
           MOVE "RCAS >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.                         
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-MAR.
           IF COD2 = "N" AND COD-MAR > LISTO-H MOVE "F" TO COD3.
           IF COD2 = "A" AND DET-MAR > LISTO-H-A MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP01 MARCAS
                         IF COD2 = "N" GO TO L-N-DESDE
                         ELSE GO TO L-A-DESDE.
           MOVE COD-MAR TO COD-MAR-ED.
           MOVE DET-MAR TO DET-MAR-ED.
           IF COD-MAR NOT = SPACES MOVE SPACES TO OBS-MAR-ED
             ELSE MOVE "INHABIL." TO OBS-MAR-ED.
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
      *     COMPUTE CONT-R = 72 - CONT-R.
      *     PERFORM ESPA CONT-R TIMES.
       LEO-MAR.
           READ MARCAS NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 081020 TO TAB-POS(3).
           MOVE 084720 TO TAB-POS(4).
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
           MOVE 121404 TO TAB-POS(16).
           MOVE 171420 TO TAB-POS(17).
           MOVE 103320 TO TAB-POS(18).
           MOVE 113320 TO TAB-POS(19).
           MOVE 123320 TO TAB-POS(20).
           MOVE 133320 TO TAB-POS(21).
           MOVE 143320 TO TAB-POS(22).
           MOVE 153320 TO TAB-POS(23).
           MOVE 163320 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071404 TO TAB-POS(29).
           MOVE 072802 TO TAB-POS(30).
           MOVE 073330 TO TAB-POS(31).
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO DE MAR" TO TIT-2-PAN.
           MOVE "CAS-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 16 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO  " LINE 17 POSITION 18.
           DISPLAY "<F6>  LISTADO ALFABET.  " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
       CARGA-EMP. 
           MOVE 30 TO SW3.
           PERFORM BORRA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) 
                POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E80.
           UNLOCK EMPRESAS RECORD.
       CARGA-SUC.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3)
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
           UNLOCK EMPRESAS RECORD.
       CARGA-EMP-F.
           EXIT.       
       DESP-DES.
           MOVE 17 TO SW3.
           MOVE DET-MAR TO D20-CAP.
           DISPLAY D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       FFF.
           EXIT PROGRAM.
       FFFF.
           STOP RUN.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
