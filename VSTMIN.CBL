       IDENTIFICATION DIVISION.
       PROGRAM-ID. VSTOCK.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. SET'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "EMPRE.SEL".
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       COPY "ENVASE.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "EMPREBM.REG".
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       COPY "ENVASE.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(12) VALUE "NUMERO MARCA".
             03 FILLER       PIC X(15) VALUE "-----------DETA".
             03 FILLER       PIC X(16) VALUE "LLE------------ ".
             03 FILLER       PIC X(16) VALUE "-----ENVASE----".
             03 FILLER       PIC X(12) VALUE "---ST.ACT-- ".
             03 FILLER       PIC X(4) VALUE "MON".
             03 FILLER       PIC X(20) VALUE "---VALOR S/COSTO-- ".
             03 FILLER       PIC X(18) VALUE "---VALOR S/VENTA--".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(10) VALUE " ARTICULOS".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999B.
             03 DET-ED       PIC X(30)B.
             03 ENV-ED       PIC X(15)B.
             03 STO-ED       PIC ------9,999B.
             03 MON-ED       PIC XXXB.
             03 COS-ED       PIC ZZZ.ZZZ.ZZZ.ZZ9,99BB.
             03 VEN-ED       PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
          02 TOT-1.
             03 FILLER       PIC X(60) VALUE SPACES.
             03 FILLER       PIC X(11) VALUE "TOTAL EN : ".
             03 MON-T-ED     PIC XXXB.
             03 COS-T-ED     PIC ZZZ.ZZZ.ZZZ.ZZ9,99BB.
             03 VEN-T-ED     PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 MARCA-L            PIC 9999.
       01 LIN-1-CAP          PIC 999 VALUE 0.
       01 LIN-2-CAP          PIC 999 VALUE 0.
       01 LIN-3-CAP          PIC 999 VALUE 0.
       01 LIN-4-CAP          PIC 999 VALUE 0.
       01 LISTO-D-A          PIC X(10).
       01 REP                PIC 9.
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 VEN-CAP            PIC 9(13)V99 VALUE 0.
       01 COS-CAP            PIC 9(13)V99 VALUE 0.
       01 MOVI-CAP           PIC S9(6)V999.
       01 STOC-CAP           PIC S9(6)V999.
       01 REST-CAP           PIC S9(6)V999.
       01 PEN-CAP            PIC S9(6)V999.
       01 MINI-CAP           PIC S9(6).
       01 STOC-EDI           PIC ------9,999.
       01 REST-EDI           PIC ------9,999.
       01 PEN-EDI            PIC ------9.
       01 TOTAL-1.
          02 COS-T-1         PIC 9(13)V99 VALUE 0.
          02 VEN-T-1         PIC 9(13)V99 VALUE 0.
       01 TOTAL-2.
          02 COS-T-2         PIC 9(13)V99 VALUE 0.
          02 VEN-T-2         PIC 9(13)V99 VALUE 0.
       01 NUM-MAR.
          02 N-ART02         PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02    PIC 9(3).
             03 N-2-ART02    PIC 9(3).
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "EMPRE.ERR".
       COPY "ART02.ERR".
       COPY "NUMERO.ERR".
       COPY "ENVASE.ERR".
       COPY "MARCAS.ERR".
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
           MOVE 3 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F05 MOVE 0 TO REP GO TO LIST-NUM.
           GO TO CARGO-MOD.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LIST.STOCK VALORIZADO " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-IMP.
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE "X" TO COD2 COD3.
       L-N-DESDE.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
           DISPLAY SPACES LINE 15 POSITION 32 SIZE 25.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-ART02.
       L-N-HASTA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
       L-N-POSI.
           OPEN OUTPUT IMP02.
           OPEN INPUT ENVASE
                INPUT ART02
                INPUT ART01
                INPUT NUMERO.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY PERFORM CIERRA-A
                        GO TO L-N-DESDE.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI
           MOVE "    <<< LI" TO TIT-2-EDI
           MOVE "STADO VALO" TO TIT-3-EDI
           MOVE "RIZADO DEL" TO TIT-4-EDI
           MOVE " STOCK >>>" TO TIT-5-EDI
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           MOVE 0 TO CONT-R CONT-P CONT-I.
           MOVE "X" TO COD3.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R > 68 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ ART02 NEXT AT END MOVE "F" TO COD3.
           IF COD-ART02 > LISTO-H MOVE "F" TO COD3.
           IF EMP-ART02 NOT = NUM-EMP MOVE "F" TO COD3.
           IF SUC-EMP NOT = SUC-ART02 MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         PERFORM CIERRA-A
                         GO TO L-N-DESDE.
           IF MIN-S-ART02 < 0,001 GO TO L-LEO.
           IF MIN-S-ART02 < 0 GO TO L-LEO.
           MOVE NUM-ART02 TO COD-NUM.
           MOVE COD-ART02 TO COD-ART01.
           READ ART01 INVALID KEY MOVE 0 TO COSTO-STOCK-ART01.
           MOVE ENV-ART01 TO NUM-ENVA.
           READ ENVASE INVALID KEY MOVE "*******" TO NOM-ENVA.
           READ NUMERO INVALID KEY MOVE "*******" TO DET-NUM.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           MOVE MIN-S-ART02 TO STO-ED STOC-CAP.
           MOVE NOM-ENVA TO ENV-ED.
           MOVE SPACES TO MON-ED.
           MOVE 0 TO VEN-CAP COS-CAP.
           IF PRE-V-ART02 NOT = 0 MOVE " $ " TO MON-ED
             COMPUTE VEN-CAP ROUNDED = PRE-V-ART02 * STOC-CAP.
           IF PRE-E-ART02 NOT = 0 MOVE "U$S" TO MON-ED
             COMPUTE VEN-CAP ROUNDED = STOC-CAP * PRE-E-ART02.
           COMPUTE COS-CAP ROUNDED = STOC-CAP * COSTO-STOCK-ART01.
           MOVE VEN-CAP TO VEN-ED.
           MOVE COS-CAP TO COS-ED.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           IF MON-ED = " $ " 
             COMPUTE VEN-T-1 = VEN-T-1 + VEN-CAP
             COMPUTE COS-T-1 = COS-T-1 + COS-CAP.
           IF MON-ED = "U$S"
             COMPUTE VEN-T-2 = VEN-T-2 + VEN-CAP
             COMPUTE COS-T-2 = COS-T-2 + COS-CAP.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
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
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 7 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           MOVE VEN-T-1 TO VEN-T-ED.
           MOVE COS-T-1 TO COS-T-ED.
           MOVE " $ " TO MON-T-ED.
           WRITE LIN-IMP02 FROM TOT-1 AFTER 1.
           MOVE VEN-T-2 TO VEN-T-ED.
           MOVE COS-T-2 TO COS-T-ED.
           MOVE "U$S" TO MON-T-ED.
           WRITE LIN-IMP02 FROM TOT-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 081502 TO TAB-POS(4).
           MOVE 082620 TO TAB-POS(5).
           MOVE 082302 TO TAB-POS(6).
           MOVE 082620 TO TAB-POS(7).
           MOVE 101510 TO TAB-POS(8).
           MOVE 102730 TO TAB-POS(9).
           MOVE 105915 TO TAB-POS(10).
           MOVE 121511 TO TAB-POS(11).
           MOVE 141511 TO TAB-POS(12)
           MOVE 161511 TO TAB-POS(13).
           MOVE 121511 TO TAB-POS(14).
           MOVE 141511 TO TAB-POS(15)
           MOVE 161511 TO TAB-POS(16).
           MOVE 101610 TO TAB-POS(17).
           MOVE 121610 TO TAB-POS(18).
           MOVE 102003 TO TAB-POS(19).
           MOVE 102503 TO TAB-POS(20).
           MOVE 103003 TO TAB-POS(21).
           MOVE 103503 TO TAB-POS(22).
       TIT-PAN.
           MOVE "  -ARCHIVO" TO TIT-1-PAN.
           MOVE " DE ARTICU" TO TIT-2-PAN.
           MOVE "LOS-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LIST.STOCK VALORIZADO" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 15 POSITION 18.
       DESP-DES.
           MOVE 9 TO SW3.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-MAR.
           MOVE 10 TO SW3.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-STO.
           MOVE 11 TO SW3.
           MOVE MIN-S-ART02 TO STOC-EDI.
           DISPLAY STOC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-ACT.
           MOVE 13 TO SW3.
           MOVE REST-CAP TO REST-EDI.
           DISPLAY REST-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       PANTA-IMP.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "DESDE ART. :" LINE 10 POSITION 3.
           DISPLAY "HASTA ART. :" LINE 12 POSITION 3.
       CARGO-EMP.
           OPEN INPUT EMPRESAS MANEJO.
       CARGO-EMP-1.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRA-A CLOSE EMPRESAS
                         GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGO-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-ART02 EMP-ART01 MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO CARGO-EMP-1.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                GO TO CARGO-EMP-1.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E132.
           MOVE 5 TO SW3.
           PERFORM DES-ALF.
           PERFORM MEN-ANTE.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           MOVE NUM-CAP TO SUC-ART02. 
           IF ESCAP PERFORM CIERRA-A CLOSE EMPRESAS 
                            GO TO INI-MOD.
           IF F02 GO TO CARGO-EMP-1.
           IF NUM-CAP = 0 MOVE SPACES TO SUC-E132
                          MOVE 0 TO NUM-SUC-E132
                          CLOSE EMPRESAS
                          GO TO CARGO-EMP-F
             ELSE MOVE NUM-CAP TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGO-EMP.
           MOVE RAZ-SOC-EMP TO ALF-CAP SUC-E132.
           MOVE SUC-EMP TO NUM-SUC-E132.
           MOVE 5 TO SW3.
           PERFORM DES-ALF.
           CLOSE EMPRESAS.
       CARGO-EMP-F.
           EXIT.
       CIERRA-A.
           CLOSE ART02 ART01 NUMERO ENVASE IMP02.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-CAP.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
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
