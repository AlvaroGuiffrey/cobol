       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISPRE11.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. SET'93.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 IMP-0.
             03 Z1        PIC 99 COMP-4 VALUE 18.
          02 IMP-10.
             03 FILLER       PIC X(20) VALUE SPACES.
             03 FILLER       PIC X(8) VALUE "MARCA : ".
             03 A2           PIC 99 COMP-4 VALUE 14.
             03 MARCA-ED     PIC X(20).
             03 A3           PIC 99 COMP-4 VALUE 20.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(6) VALUE "MARCA ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE ------- MON".
             03 FILLER       PIC X(18) VALUE "  --PRECIO VENTA--".
             03 FILLER       PIC X(12) VALUE " FECHA PREC ".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999BB.
             03 DET-ED       PIC X(26)B.
             03 MON-ED       PIC XXXBB.
             03 PRE-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 FEC-V-ED     PIC 99/99/99BB.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 LIS-COD            PIC XX.
       01 MARCA-L            PIC 9(4).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 QQ                 PIC X.
       01 DATOS.
          02 DATO     PIC X(71) OCCURS 10 TIMES.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART01.ERR".
       COPY "ART02.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP01.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           MOVE 0 TO CLAVE-1-CAP.
           PERFORM CONF-CAMPOS.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
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
           OPEN OUTPUT IMP01.
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
           IF ESCAP  CLOSE IMP01 
                     STOP RUN.
           IF F07 MOVE "NT" TO LIS-COD GO TO LIST-T-N.
           IF F08 MOVE "AT" TO LIS-COD GO TO LIST-T-A.
           GO TO CARGO-MOD.
       LIST-T-A.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LIST.ALFAB.TOTAL " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANT-LIS.
           MOVE 0 TO CONT-R CONT-I CONT-P.
       L-A-DESDE.
           DISPLAY " INGRESE NOMBRE   " LINE 9 POSITION 30 REVERSE.
           DISPLAY "  DEL ARTICULO    " LINE 10 POSITION 30 REVERSE.
           DISPLAY " ( 10 DIGITOS )   " LINE 11 POSITION 30 REVERSE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO LISTO-D-A DET-NUM. 
       L-A-HASTA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
           MOVE 0 TO CONT-P CONT-R CONT-I.
       L-A-POSI.
           WRITE LIN-IMP01 FROM IMP-0 AFTER 0.
           OPEN INPUT ART02 ART01.
           OPEN INPUT NUMERO.
           MOVE "X" TO COD3.
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY PERFORM CIERRE-A
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<< LISTA " TO TIT-2-EDI.
           MOVE "ALFABETICA" TO TIT-3-EDI.
           MOVE " DE -PRECI" TO TIT-4-EDI.
           MOVE "OS- >>>   " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO-A.
       L-LEO-A.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
           IF DET-NUM > LISTO-H-A MOVE "F" TO COD3.
           IF CONT-R = 0 PERFORM TITU.
           IF COD3 = "F"
                         PERFORM PIE-P
                         PERFORM CIERRE-A
                         GO TO L-A-DESDE.
           IF COD-NUM = SPACES GO TO L-LEO-A.
                MOVE COD-NUM TO NUM-ART02
                MOVE COD-MAR TO MAR-ART02.
           READ ART02 INVALID KEY GO TO L-LEO-A.
           IF SUC-ART02 NOT = SUC-EMP GO TO L-LEO-A.
           PERFORM MUEVO-IMP.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.      
           GO TO L-LEO-A.
       LIST-T-N.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE "LIST.NUMER.TOTAL " TO D20-CAP.
           PERFORM MODA-PAN.
           MOVE 0 TO CONT-R CONT-I CONT-P.
       PANT-LIS.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "HASTA: " LINE 11 POSITION 3.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       L-N-DESDE.
           DISPLAY " INGRESE CODIGO DE " LINE 9 POSITION 30 REVERSE.
           DISPLAY "    ARTICULO       " LINE 10 POSITION 30 REVERSE.
           DISPLAY " (NUMERO Y MARCA)  " LINE 11 POSITION 30 REVERSE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           DISPLAY SPACES LINE 23 POSITION 3 SIZE 30
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-ART02.
           MOVE COD-ART02 TO LISTO-H.
       L-N-HASTA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
       L-N-POSI.
           WRITE LIN-IMP01 FROM IMP-0 AFTER 0.
           OPEN INPUT ART02 ART01.
           OPEN INPUT NUMERO.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY PERFORM CIERRE-A
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE "X" TO COD3.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<< LISTA " TO TIT-2-EDI.
           MOVE "NUMERICA D" TO TIT-3-EDI.
           MOVE "E -PRECIOS" TO TIT-4-EDI.
           MOVE "- >>>     " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-ART02.
           MOVE NUM-ART02 TO COD-NUM.
           PERFORM LEO-NUM-R.
           IF COD-ART02 > LISTO-H MOVE "F" TO COD3.
           IF COD3 = "F" 
                         PERFORM PIE-P
                         PERFORM CIERRE-A
                         GO TO L-N-DESDE.
           IF SUC-ART02 NOT = SUC-EMP GO TO L-LEO.
           MOVE NUM-ART02 TO N-ART02.
           IF CONT-R = 0 PERFORM TITU.     
           GO TO SIGO-L.
       SIGO-L.               
           PERFORM MUEVO-IMP.
           IF COD-ART02 = SPACES GO TO L-LEO.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
       LEO-MAR.
           READ MARCAS NEXT AT END PERFORM PIE-P
                                   PERFORM CIERRE-A
                                   CLOSE MARCAS
                                   GO TO INI-MOD.
           IF DET-MAR = SPACES GO TO LEO-MAR.
           MOVE COD-MAR TO LISTO-H.
           MOVE COD-MAR TO MAR-ART02.
           MOVE DET-MAR TO MARCA-ED.
       CARGA-MARCA.
           OPEN INPUT MARCAS.
           MOVE SPACES TO DET-MAR.
           MOVE MAR-ART02 TO COD-MAR.
           READ MARCAS INVALID KEY MOVE "********" TO MARCA-ED.
           IF DET-MAR NOT = SPACES MOVE DET-MAR TO MARCA-ED.
           MOVE COD-MAR TO MARCA-L.
           CLOSE MARCAS.
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
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           ADD 8 TO CONT-R.
       SUBTITU.
           PERFORM ESPA.
           IF CONT-R > 64 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU
                          GO TO SUBTITU-F.
           WRITE LIN-IMP01 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           ADD 2 TO CONT-R.
       SUBTITU-F.
           EXIT.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
       LEO-ART02.
           READ ART02 NEXT AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO INVALID KEY MOVE "F" TO COD3.
       LEO-MAR-R.
           READ MARCAS INVALID KEY MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 091010 TO TAB-POS(4).
           MOVE 094710 TO TAB-POS(5).
           MOVE 091020 TO TAB-POS(6).
           MOVE 074720 TO TAB-POS(7).
           MOVE 082104 TO TAB-POS(8).
           MOVE 083402 TO TAB-POS(9).
           MOVE 083920 TO TAB-POS(10).
           MOVE 102110 TO TAB-POS(11).
           MOVE 122115 TO TAB-POS(12)
           MOVE 142106 TO TAB-POS(13).
           MOVE 152115 TO TAB-POS(14).
           MOVE 162115 TO TAB-POS(15)
           MOVE 172115 TO TAB-POS(16).
           MOVE 182108 TO TAB-POS(17).
           MOVE 192106 TO TAB-POS(18).
           MOVE 202106 TO TAB-POS(19).
           MOVE 222103 TO TAB-POS(20).
           MOVE 222503 TO TAB-POS(21).
           MOVE 222903 TO TAB-POS(22).
           MOVE 212103 TO TAB-POS(23).
           MOVE 212503 TO TAB-POS(24).
           MOVE 212903 TO TAB-POS(25).
           MOVE 213303 TO TAB-POS(26).
           MOVE 213703 TO TAB-POS(27).
           MOVE 103530 TO TAB-POS(28).
           MOVE 132108 TO TAB-POS(29).
           MOVE 092020 TO TAB-POS(30).
           MOVE 111010 TO TAB-POS(31).
       TIT-PAN.
           MOVE "     LISTA" TO TIT-1-PAN.
           MOVE "DO DE PREC" TO TIT-2-PAN.
           MOVE "IOS       " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F7>  LIST.NUMERICO TOTAL " LINE 14 POSITION 18.
           DISPLAY "<F8>  LIST.ALFABET. TOTAL " LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       DES-MAR.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       LEA-EMPRE.
           OPEN INPUT EMPRESAS.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                     GO TO LEA-EMPRE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E80.
           PERFORM DES-ALF.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 CLOSE EMPRESAS GO TO LEA-EMPRE.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           IF NUM-CAP = 0 GO TO F-LEA-EMPRE.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                 GO TO LEA-EMPRE.
           MOVE 10 TO SW3.
           MOVE RAZ-SOC-EMP TO ALF-CAP SUC-E80.
           PERFORM DES-ALF.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       MUEVO-IMP.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           IF PRE-V-ART02 = 0 MOVE 0 TO PRE-V-ED.
           IF PRE-V-ART02 NOT = 0
             MOVE PRE-V-ART02 TO PRE-V-ED
             MOVE " $ " TO MON-ED
             MOVE FEC-V-ART02 TO FEC-V-ED.
           IF PRE-E-ART02 NOT = 0
             MOVE PRE-E-ART02 TO PRE-V-ED
             MOVE "U$S" TO MON-ED
             MOVE FEC-E-ART02 TO FEC-V-ED.
           IF PRE-V-ED = 0
             MOVE EMP-ART02 TO EMP-ART01
             MOVE NUM-ART02 TO NUM-ART01
             MOVE MAR-ART02 TO MAR-ART01
             MOVE "COS" TO MON-ED
             READ ART01 INVALID KEY MOVE 0 TO PRE-V-ED
             MOVE COSTO-STOCK-ART01 TO PRE-V-ED
             MOVE FEC-U-COMP-ART01 TO FEC-V-ED.
       CIERRE-A.
           CLOSE ART02 NUMERO ART01.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
