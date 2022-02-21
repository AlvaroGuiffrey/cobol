       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTIZA.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. OCT'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "EMPRE.SEL".
       COPY "MONE.SEL".
       COPY "COTIZA.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "MONE.REG".
       COPY "COTIZA.REG".
       COPY "EMPREBM.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 IMP-10.
             03 FILLER       PIC X(8) VALUE "MONEDA: ".
             03 NUM-MON-ED       PIC Z9.
             03 FILLER       PIC XXX VALUE SPACES.
             03 NOM-MON-ED   PIC X(30).
          02 IMP-1.
             03 FILLER       PIC X(10) VALUE "--FECHA-  ".
             03 FILLER  PIC X(25) VALUE "------VALOR------        ".
             03 FILLER       PIC X(10) VALUE "--FECHA-  ".
             03 FILLER  PIC X(25) VALUE "------VALOR------        ".
             03 FILLER       PIC X(10) VALUE "--FECHA-  ".
             03 FILLER  PIC X(25) VALUE "------VALOR------        ".
             03 FILLER       PIC X(10) VALUE "--FECHA-  ".
             03 FILLER  PIC X(25) VALUE "------VALOR------        ".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(19) VALUE " COTIZAC. O INDICES".
          02 IMP-3.
             03 FEC-1-ED     PIC 99/99/99BB.
             03 VAL-1-ED     PIC Z.ZZZ.ZZZ.ZZ9,999B(8).
             03 FEC-2-ED     PIC 99/99/99BB.
             03 VAL-2-ED     PIC Z.ZZZ.ZZZ.ZZ9,999B(8).
             03 FEC-3-ED     PIC 99/99/99BB.
             03 VAL-3-ED     PIC Z.ZZZ.ZZZ.ZZ9,999B(8).
             03 FEC-4-ED     PIC 99/99/99BB.
             03 VAL-4-ED     PIC Z.ZZZ.ZZZ.ZZ9,999.
       01 LISTO-D            PIC 99999999.
       01 LISTO-H            PIC 99999999.
       01 COD-MONEDA         PIC 99.
       01 VAL-EDI            PIC Z.ZZZ.ZZZ.ZZ9,999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 C-FEC.
         02 FEC-I.
          03 DD-I         PIC 99.
          03 MM-I         PIC 99.
          03 ANO-I.
             05 AAAA-I       PIC 99.
             05 AA-I         PIC 99.
         02 FEC-C REDEFINES FEC-I  PIC 9(8).
       01 FEC-T.
          03 AA-T         PIC 9999.
          03 MM-T         PIC 99.
          03 DD-T         PIC 99.
        01 FEC-DAT.
           03 DD-DAT      PIC 99/.
           03 MM-DAT      PIC 99/.
           03 AA-DAT      PIC 9999.
        01 CLAVE-1-CAP.    
           02 CLAVE-11-CAP  PIC 9 VALUE 0.
           02 CLAVE-12-CAP  PIC 9 VALUE 0.
           02 CLAVE-13-CAP  PIC 9 VALUE 0.
        01 CLAVE-2-CAP.
           02 CLAVE-21-CAP  PIC 9 VALUE 0.
           02 CLAVE-22-CAP  PIC 9 VALUE 0.
           02 CLAVE-23-CAP  PIC 9 VALUE 0.
        01 CLAVE-3-CAP.
           02 CLAVE-31-CAP  PIC 9 VALUE 0.
           02 CLAVE-32-CAP  PIC 9 VALUE 0.
           02 CLAVE-33-CAP  PIC 9 VALUE 0.
        01 CLAVE-1-COM      PIC 999.
        01 CLAVE-2-COM      PIC 999.
        01 CLAVE-3-COM      PIC 999.
        01 COD-CLAVE        PIC 9 VALUE 0.
        01 VAL-DAT     PIC Z.ZZZ.ZZZ.ZZ9,999.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "MONE.ERR".
       COPY "COTIZA.ERR".
       COPY "IMP02.ERR".
       COPY "EMPRE.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CGMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-CLA.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-CLA.
           IF CLAVE-1-CAP NOT = 0 GO TO SIGA-INI.
           PERFORM CLAVE-SYS.
           PERFORM BORRA-CLAVE.
       SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
           MOVE 3 TO SW3.
       CARGO-MOD.
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
           MOVE " ALTA, BAJA O MODIF. " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       PANTA-CAP.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "MONEDA         :" LINE 12 POSITION 3.
           DISPLAY "FECHA(DDMMAAAA):" LINE 16 POSITION 3.
           DISPLAY "VALOR          :" LINE 20 POSITION 3.
       SIGA-ACTU.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           OPEN INPUT MONEDAS.
           OPEN I-O COTIZA.
       CARGA-MON.
           PERFORM MEN-ANTE.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO ACTU.
           IF NUM-CAP = 0 GO TO CARGA-MON.
           MOVE NUM-CAP TO MON-COTIZA NUM-MON COD-MONEDA.
           READ MONEDAS WITH NO LOCK INVALID KEY GO TO CARGA-MON.
           MOVE NOM-MON TO ALF-CAP.
           MOVE 7 TO SW3.
           PERFORM DES-ALF.
       CARGA-FEC.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           ACCEPT FEC-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MON.
           IF FEC-I = 0 GO TO CARGA-FEC.
           IF ANO-I < 1900 GO TO CARGA-FEC.
           MOVE DD-I TO DD-COTIZA.
           MOVE MM-I TO MM-COTIZA.
           MOVE ANO-I TO AA-COTIZA.
           MOVE FEC-I TO FEC-DAT.
           DISPLAY FEC-DAT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
       LEE-COT.
           READ COTIZA INVALID KEY GO TO ALTA.
           PERFORM DESP-VAL.
           PERFORM MEN-B-M.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-MON.
       ALTA.
           PERFORM MEN-ANTE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-COTIZA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MON.
           WRITE REG-COTIZA INVALID KEY DISPLAY "ERROR 01".
           GO TO CARGA-MON.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 DELETE COTIZA 
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK COTIZA RECORD
                  GO TO CARGA-MON.
           PERFORM MEN-NO-BAJA 50 TIMES.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK COTIZA RECORD.
           GO TO CARGA-MON.
       MODI.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-COTIZA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 REWRITE REG-COTIZA
             UNLOCK COTIZA RECORD
             ELSE PERFORM MEN-NO-MODI 50 TIMES.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           UNLOCK COTIZA RECORD.
           PERFORM MENS-PAN.
           GO TO CARGA-MON.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA POR FECHA" TO D20-CAP.
           PERFORM MODA-PAN.
       PANTA-C.
           DISPLAY "EMPRESA       : " LINE 8 POSITION 3.
           DISPLAY "MONEDA        : " LINE 9 POSITION 3.
           DISPLAY "FECHA DESDE   : " LINE 10 POSITION 3.
           DISPLAY "FECHA HASTA :"  LINE 10 POSITION 40.
       SIGA-C.
           DISPLAY "--FECHA-    " LINE 11 POSITION 28.
           DISPLAY "---VALOR---" LINE 11 POSITION 40.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           OPEN INPUT MONEDAS 
                INPUT COTIZA.
       C-N-MON.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM  CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO  CONS-NUM.
           IF NUM-CAP = 0 GO TO C-N-MON.
           MOVE NUM-CAP TO NUM-MON MON-COTIZA COD-MONEDA.
           READ MONEDAS WITH NO LOCK INVALID KEY GO TO C-N-MON.
           MOVE 11 TO SW3.
           MOVE NOM-MON TO ALF-CAP.
           PERFORM DES-ALF.         
       C-N-DESDE.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO C-N-MON.
           MOVE DD-I TO DD-COTIZA.
           MOVE MM-I TO MM-COTIZA.
           MOVE ANO-I TO AA-COTIZA.
           PERFORM BORRA-PANTA-C.
       C-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE DD-I TO DD-T.
           MOVE MM-I TO MM-T.
           MOVE ANO-I TO AA-T.
       C-N-POSI.
           START COTIZA KEY IS NOT < CLAVE-COTIZA
                         INVALID KEY GO TO C-N-DESDE.
           MOVE 0 TO SW2.
           MOVE 13 TO SW3.
           GO TO C-LEO.
       C-LEO.
           ADD 1 TO SW2.
           ADD 1 TO SW3.
           IF SW2 < 10 GO TO C-SIGOLEO.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           MOVE 1 TO SW2.
           MOVE 14 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F03 GO TO C-SIGOLEO.
           PERFORM MEN-F-CONS.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGOLEO.
           IF SW3 > 23 MOVE 14 TO SW3.
           IF SW2 = 1 PERFORM BORRA-PANTA-C.
           MOVE "X" TO COD3.
           PERFORM LEO-COT.
           IF COD3 = "F" GO TO C-N-DESDE.
           IF EMP-COTIZA > NUM-EMP GO TO C-N-DESDE.
           IF MON-COTIZA > COD-MONEDA GO TO C-N-DESDE.
           IF FEC-COTIZA > FEC-T GO TO C-N-DESDE.
           PERFORM MUEVO-DATO.
           PERFORM DES-DATO.
           GO TO C-LEO.
       MUEVO-DATO.
           MOVE DD-COTIZA TO DD-I.
           MOVE MM-COTIZA TO MM-I.
           MOVE AA-COTIZA TO ANO-I.
           MOVE FEC-I TO FEC-DAT.
           MOVE VAL-COTIZA TO VAL-DAT.
       DES-DATO.
           DISPLAY FEC-DAT LINE TAB-LIN(SW3) POSITION 28.
           DISPLAY VAL-DAT LINE TAB-LIN(SW3) POSITION 40.
       F-CONS.
           PERFORM CIERRE-A.
           GO TO C-N-DESDE.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO POR FECHA " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-C.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           OPEN INPUT MONEDAS
                INPUT COTIZA
                OUTPUT IMP02.
       L-N-MON.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A CLOSE IMP02 GO TO LIST-NUM.
           IF NUM-CAP = 0 GO TO L-N-MON.
           MOVE NUM-CAP TO NUM-MON MON-COTIZA NUM-MON-ED COD-MONEDA.
           READ MONEDAS WITH NO LOCK INVALID KEY GO TO L-N-MON.
           MOVE NOM-MON TO NOM-MON-ED ALF-CAP.
           MOVE 11 TO SW3.
           PERFORM DES-ALF.
       L-N-DESDE.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE IMP02 PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO L-N-MON.
           MOVE NUM-CAP TO LISTO-D FEC-I.
           MOVE DD-I TO DD-COTIZA.
           MOVE MM-I TO MM-COTIZA.
           MOVE ANO-I TO AA-COTIZA.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE IMP02 PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H FEC-I.
           MOVE DD-I TO DD-T.
           MOVE MM-I TO MM-T.
           MOVE ANO-I TO AA-T.
       L-N-POSI.
           START COTIZA KEY IS NOT < CLAVE-COTIZA
                        INVALID KEY PERFORM CIERRE-A CLOSE IMP02
                        GO TO L-N-DESDE.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO POR FEC" TO TIT-3-EDI.
           MOVE "HA DE COTI" TO TIT-4-EDI.
           MOVE "ZACIONES >" TO TIT-5-EDI.
           MOVE ">>        " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           PERFORM TITU.
           MOVE 0 TO SW2.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R > 68 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-COT.
           IF EMP-COTIZA > NUM-EMP GO TO L-N-DESDE.
           IF MON-COTIZA > COD-MONEDA GO TO L-N-DESDE.
           IF FEC-COTIZA > FEC-T GO TO L-N-DESDE.
           IF COD3 = "F" 
              IF SW2 > 0 PERFORM IMPRE
                         PERFORM PIE-P
                         CLOSE IMP02 
                         PERFORM CIERRE-A
                         GO TO L-N-DESDE.
           ADD 1 TO SW2.
           PERFORM ARMA-IMP.
           ADD 1 TO CONT-I.
           IF SW2 > 3 GO TO IMPRE.
           GO TO L-LEO.
       ARMA-IMP.
             MOVE DD-COTIZA TO DD-I.
             MOVE MM-COTIZA TO MM-I.
             MOVE AA-COTIZA TO ANO-I.
           IF SW2 = 1
             MOVE FEC-COTIZA TO FEC-1-ED
             MOVE VAL-COTIZA TO VAL-1-ED.
           IF SW2 = 2
             MOVE FEC-COTIZA TO FEC-2-ED
             MOVE VAL-COTIZA TO VAL-2-ED.
           IF SW2 = 3
             MOVE FEC-COTIZA TO FEC-3-ED
             MOVE VAL-COTIZA TO VAL-3-ED.
           IF SW2 = 4
             MOVE FEC-COTIZA TO FEC-4-ED
             MOVE VAL-COTIZA TO VAL-4-ED.
       IMPRE.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO SW2 FEC-1-ED FEC-2-ED FEC-3-ED FEC-4-ED.
           MOVE 0 TO VAL-1-ED VAL-2-ED VAL-3-ED.
           MOVE 0 TO VAL-4-ED.
           GO TO L-LEO.
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
           WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
       LEO-COT.
           READ COTIZA NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CIERRE-A.
           CLOSE MONEDAS.
           CLOSE COTIZA.
       CARGA-EMP.
           OPEN INPUT EMPRESAS.
           MOVE 4 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF NUM-CAP = 0 CLOSE EMPRESAS GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-COTIZA MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS WITH NO LOCK INVALID KEY CLOSE EMPRESAS
                         GO TO CARGA-EMP.
           OPEN INPUT MANEJO
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE MANEJO
                GO TO CARGA-EMP.
           CLOSE MANEJO
           MOVE RAZ-SOC-EMP TO EMP-E132 ALF-CAP.
           MOVE 0 TO NUM-SUC-E132.
           MOVE SPACES TO SUC-E132.
           MOVE 5 TO SW3.
           PERFORM DES-ALF.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       CARGA-EMP-F.
           EXIT.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 082002 TO TAB-POS(4).
           MOVE 083030 TO TAB-POS(5).
           MOVE 122002 TO TAB-POS(6).
           MOVE 123030 TO TAB-POS(7).
           MOVE 162010 TO TAB-POS(8).
           MOVE 202020 TO TAB-POS(9).
           MOVE 092002 TO TAB-POS(10).
           MOVE 093030 TO TAB-POS(11).
           MOVE 102008 TO TAB-POS(12)
           MOVE 105508 TO TAB-POS(13).
           MOVE 122823 TO TAB-POS(14).
           MOVE 132823 TO TAB-POS(15)
           MOVE 142823 TO TAB-POS(16).
           MOVE 152823 TO TAB-POS(17).
           MOVE 162823 TO TAB-POS(18).
           MOVE 172823 TO TAB-POS(19).
           MOVE 182823 TO TAB-POS(20).
           MOVE 192823 TO TAB-POS(21).
           MOVE 202823 TO TAB-POS(22).
           MOVE 212823 TO TAB-POS(23).
       TIT-PAN.
           MOVE " -ARCHIVO " TO TIT-1-PAN.
           MOVE " DE  COTIZ" TO TIT-2-PAN.
           MOVE "ACIONES-  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA POR FECHA " LINE 15 POSITION 18.
           DISPLAY "<F5>  LISTADO POR FECHA  " LINE 16 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 17 POSITION 18.
       CLAVE-SYS.
           MOVE 0 TO COD-CLAVE.
       BORRA-CLAVE.
           DISPLAY SPACES LINE 14 POSITION 18 SIZE 15.
           DISPLAY SPACES LINE 16 POSITION 18 SIZE 15.
           DISPLAY SPACES LINE 17 POSITION 18 SIZE 15.
           DISPLAY SPACES LINE 18 POSITION 18 SIZE 15.
       CHEQUEA-CLAVE1.
           MOVE 0 TO COD-CLAVE.
       CHEQUEA-CLAVE2.
           MOVE 0 TO COD-CLAVE.
       CHEQUEA-CLAVE3.
           MOVE 0 TO COD-CLAVE.
       DESP-VAL.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE VAL-COTIZA TO VAL-EDI.
           DISPLAY VAL-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 12 BY 1
                             UNTIL SW1 > 21.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
