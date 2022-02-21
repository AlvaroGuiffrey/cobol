       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISPSOT.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. ENE'93.
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
       COPY "NUME1.SEL".

       COPY "MARCAS.SEL".
       COPY "ENVASE.SEL".
       COPY "ALICUO.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "NUME1.REG".
       COPY "ENVASE.REG".
       COPY "MARCAS.REG".
       COPY "ALICUO.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 IMP-0.
             03 Z1        PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(6) VALUE "MARCA ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(22) VALUE " --COMENTARIO--".
             03 FILLER       PIC X(11) VALUE "------ PREC".
             03 FILLER       PIC X(10) VALUE "IOS ------".
          02 IMP-11.
             03 FILLER       PIC X(59) VALUE SPACES.
             03 FILLER       PIC X(11) VALUE " INSCRIPTO".
      ***       03 FILLER       PIC X(10) VALUE " NO INSCR.".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999BB.
             03 DET-ED       PIC X(30)B.
             03 COMENT-ED    PIC X(21)B.
             03 PRE-V-ED     PIC ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE SPACES.
      ***       03 PRE-N-ED     PIC ZZZ.ZZ9,99 BLANK WHEN ZERO.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(1).
       01 LISTO-H-A          PIC X(1).
       01 ACA-DESG.
          03  DESGLOSE       PIC X(30).
          03  R-DESGLOSE REDEFINES DESGLOSE.
              05  DESG1      PIC X.
              05  FILLER     PIC X(29).
       01 PRE-N-CAP          PIC 9(10)V99 VALUE 0.
       01 NETO-CAP           PIC 9(10)V99 VALUE 0.
       01 IVA-1-CAP          PIC 99V99 VALUE 0.
       01 IVA-2-CAP          PIC 99V99 VALUE 0.
       01 IND-ORD            PIC 9.
       01 LIS-COD            PIC XX.
       01 MARCA-L            PIC 9(4).
       01 CONT               PIC 9(6) VALUE 0.
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
       COPY "ALICUO.ERR".
       COPY "NUMERO.ERR".
       COPY "NUME1.ERR".
       COPY "EMPRE.ERR".
       COPY "ENVASE.ERR".
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
           IF SW2 = 25 GO TO SIGA-CLAVE.
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
           IF ESCAP  EXIT PROGRAM 
                     STOP RUN.
           IF F05 MOVE "NM" TO LIS-COD GO TO LIST-NUM.

           GO TO CARGO-MOD.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           MOVE 0 TO CONT-R CONT-I CONT-P.
       PANT-LIS.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01 EMP-ALI.
           MOVE SUC-EMP TO SUC-ART02 SUC-ALI.
           PERFORM CARGA-ALI THRU CARGA-ALI-F. 
       L-N-DESDE.
           PERFORM BORRA-PANTA-C.
           DISPLAY "DESDE LETRA :" LINE 10 POSITION 3.
           DISPLAY "HASTA LETRA :" LINE 12 POSITION 3.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           ACCEPT LISTO-D-A LINE 10 POSITION 30 NO BEEP TAB
           ACCEPT LISTO-H-A LINE 12 POSITION 30 NO BEEP TAB

      *     PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-NUM.
           MOVE NUM-CAP TO LISTO-H.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
      *     PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
      *     IF LISTO-H > LISTO-D GO TO L-N-POSI.
           GO TO L-N-POSI.
       L-N-POSI.
           OPEN OUTPUT IMP01.
           WRITE LIN-IMP01 FROM IMP-0 AFTER 0.
           OPEN INPUT ART02 ART01.
           OPEN INPUT NUMERO NUME1 ENVASE.
      *     MOVE "A"     TO DET-NUM
           MOVE LISTO-D-A     TO DET-NUM

           START NUMERO KEY NOT LESS DET-NUM INVALID KEY 
                        PERFORM CIERRE-A
                        GO TO L-N-DESDE.

           MOVE "N" TO COD2.
           MOVE "X" TO COD3.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<<LISTA A" TO TIT-2-EDI.
           MOVE "LFABETICA " TO TIT-3-EDI.
           MOVE "DE PRECIOS" TO TIT-4-EDI.
           MOVE "- >>>     " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       L-LEO.
           IF COD3 = "F" 
                         PERFORM PIE-P
                         PERFORM CIERRE-A
                         GO TO L-N-DESDE.



           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-NUM.

           MOVE DET-NUM TO DESGLOSE
           IF DESG1 < LISTO-D-A GO TO L-LEO.
           IF DESG1 > LISTO-H-A MOVE "F" TO COD3.

      *     IF COD-NUM > LISTO-H MOVE "F" TO COD3.
           MOVE COD-NUM TO NUM-ART02 NUM-ART01 COD-NUME1.
           MOVE 0 TO MAR-ART02 MAR-ART01.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.

           START ART02 KEY IS NOT < CLAVE-ART02
                INVALID KEY GO TO L-LEO.
           READ ART02 NEXT AT END GO TO L-LEO.
           IF COD-NUM NOT = NUM-ART02 GO TO L-LEO.
           MOVE NUM-ART02 TO NUM-ART01.
           MOVE MAR-ART02 TO MAR-ART01.
           IF NUM-EMP NOT = EMP-ART02 GO TO L-LEO.
           IF NUM-EMP = EMP-ART02
              IF SUC-EMP NOT = SUC-ART02 GO TO L-LEO.
           IF COD3 = "F" 
                         PERFORM PIE-P
                         PERFORM CIERRE-A
                         GO TO L-N-DESDE.
           IF CONT-R = 0 PERFORM TITU. 
           READ ART01 INVALID KEY GO TO L-LEO.    
           GO TO SIGO-L.
       SIGO-L.               
           PERFORM MUEVO-IMP.
           IF COD-ART02 = SPACES GO TO L-LEO.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
           IF COND-ART02(1) = 98
             WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 0.
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
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-11 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
           MOVE 0 TO CONT-R.
       CARGA-ALI.
           MOVE 1 TO ALI-ALI.
           MOVE 0 TO DD-ALI MM-ALI AA-ALI IVA-1-ALI IVA-2-ALI.
           OPEN INPUT ALICUO.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
                   INVALID KEY CLOSE ALICUO
                   GO TO INI-MOD.
       SIGA-ALI.
           READ ALICUO NEXT AT END GO TO SIGA-1-ALI.
           IF EMP-ALI NOT = NUM-EMP GO TO SIGA-1-ALI.
           IF SUC-ALI NOT = SUC-EMP GO TO SIGA-1-ALI.
           IF ALI-ALI NOT = 1 GO TO SIGA-1-ALI.
           IF AA-ALI > ANO GO TO SIGA-1-ALI.
           IF AA-ALI = ANO
              IF MM-ALI > MES GO TO SIGA-1-ALI.
           IF AA-ALI = ANO
              IF MM-ALI = MES
                 IF DD-ALI > DIA GO TO SIGA-1-ALI.
           MOVE IVA-1-ALI TO IVA-1-CAP.
           MOVE IVA-2-ALI TO IVA-2-CAP.
           GO TO SIGA-ALI.
       SIGA-1-ALI.
           CLOSE ALICUO.
       CARGA-ALI-F.
           EXIT.
       LEO-ART02.
           READ ART02 NEXT AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO INVALID KEY MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 102006 TO TAB-POS(4).
           MOVE 122006 TO TAB-POS(5).
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
       TIT-PAN.
           MOVE "     LISTA" TO TIT-1-PAN.
           MOVE "DO DE PREC" TO TIT-2-PAN.
           MOVE "IOS       " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LIST.ALFABETICO" LINE 14 POSITION 18.
           DISPLAY " Sin Titulos         " LINE 15 POSITION 23.
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
           IF IMP-II-ART01 = 1
              COMPUTE PRE-V-ART02 = PRE-V-ART02 - PRE-E-ART02.
           COMPUTE NETO-CAP ROUNDED = PRE-V-ART02 /  
                 ( ( IVA-1-CAP / 100 ) + 1 ).
           COMPUTE PRE-N-CAP ROUNDED = PRE-V-ART02 + ( NETO-CAP 
                            * IVA-2-CAP / 100 ).
           IF IMP-II-ART01 = 1
              COMPUTE PRE-V-ART02 = PRE-V-ART02 + PRE-E-ART02
              COMPUTE PRE-N-CAP = PRE-N-CAP + PRE-E-ART02.
           IF IMP-IVA-ART01 = 999
              MOVE PRE-V-ART02 TO PRE-N-CAP.
           MOVE PRE-V-ART02 TO PRE-V-ED.
      *     MOVE PRE-N-CAP TO PRE-N-ED.
           MOVE ENV-ART01 TO NUM-ENVA.
           READ ENVASE INVALID KEY MOVE SPACES TO NOM-ENVA.
      *     MOVE NOM-ENVA TO ENV-ED.
           READ NUME1 INVALID KEY MOVE SPACES TO DET-NUME1.
           MOVE DET-NUME1 TO COMENT-ED.


       CIERRE-A.
           CLOSE ENVASE ART02 NUMERO ART01 IMP01.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
