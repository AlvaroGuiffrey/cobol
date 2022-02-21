       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISPRE9.
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
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "LINEAS.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "ENVASE.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
           SELECT PRELIS3 ASSIGN TO RANDOM "PRELIS3"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLAVE-PRELIS3.
       DATA DIVISION.
       FILE SECTION.
       COPY "ART01.REG".
       COPY "LINEAS.REG".
       COPY "ART02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "ENVASE.REG".
       COPY "PANTA.REG".
       COPY "PRELIS3.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 XXX       PIC 99 COMP-4 VALUE 18.
       01 TITULO-1.
          02 FILLER       PIC X(7) VALUE "CODIGO".
          02 FILLER       PIC X(15) VALUE "---------- ARTI".
          02 FILLER       PIC X(16) VALUE "CULO ----------".
          02 FILLER       PIC X(4) VALUE "MON".
          02 FILLER       PIC X(13) VALUE "-- COSTO ---".
          02 FILLER       PIC X(13) VALUE "PRECIO C/IVA".
          02 FILLER       PIC X(12) VALUE "-- ENVASE --".
       01 TIT-MAR.
          02 FILLER       PIC X(8) VALUE "MARCA:".
          02 MAR-ED       PIC ZZZ9BB.
          02 FILLER       PIC X(3) VALUE "-".
          02 FILLER       PIC 99 COMP-4 VALUE 14.
          02 DET-MAR-ED   PIC X(30).
          02 FILLER       PIC 99 COMP-4 VALUE 20.          
       01 TIT-LIN1.
          02 FILLER       PIC 99 COMP-4 VALUE 14.
          02 DET-LIN1-ED  PIC X(30).
          02 FILLER       PIC 99 COMP-4 VALUE 20.
       01 TIT-LIN2.
          02 FILLER       PIC 99 COMP-4 VALUE 15.
          02 FILLER       PIC 99 COMP-4 VALUE 14.
          02 DET-LIN2-ED  PIC X(30).
          02 FILLER       PIC 99 COMP-4 VALUE 18.
          02 FILLER       PIC 99 COMP-4 VALUE 20.
       01 IMP-1.
          02 NUM-ED       PIC ZZZZZZB.
          02 DET-ED       PIC X(30)B.
          02 MON-ED       PIC XXXB.
          02 SIN-ED       PIC Z.ZZZ.ZZ9,99B.
          02 CON-ED       PIC Z.ZZZ.ZZ9,99B.
          02 ENV-ED       PIC X(12).
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 LISTO-M            PIC 9(4).
       01 IND-ORD            PIC 9.
       01 SAL-COD            PIC 9 VALUE 0.
       01 PRE-COD            PIC X.
       01 COD-LIS            PIC 9.
       01 PRE-CAP            PIC 9(12)V99.
       01 PRE-U-CAP          PIC 9(12)V99.
       01 PRE-SIN            PIC 9(12)V99.
       01 PRE-CON            PIC 9(12)V99.
       01 MARCA-L            PIC 9(4).
       01 CANT-LIS           PIC 99 VALUE 0.
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 DET-MAR-EDI        PIC X(30).
       01 VAL-COT            PIC 9(10)V999 VALUE 0.
       01 LIN-1-AUX          PIC 999 VALUE 0.
       01 LIN-2-AUX          PIC 999 VALUE 0.
       01 LIN-3-AUX          PIC 999 VALUE 0.
       01 LIN-4-AUX          PIC 999 VALUE 0.
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 DET.
          02 DET-1           PIC X(29).
          02 DET-2           PIC X.
       01 QQ                 PIC X.
       01 CONT-V             PIC 999 VALUE ZERO.
       01 CONT-I1            PIC 999 VALUE  ZERO.
       01 CONT-I2            PIC 999 VALUE ZERO.
       01 CONT-LEO           PIC 9(6) VALUE 0.
       01 FEC-I.
          03 DD-I            PIC 99.
          03 FEC-V-I         PIC 9999.
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
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
       INI-MOD.
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
           IF ESCAP STOP RUN.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANT-LIS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE 0 TO CONT-R CONT-I.
           PERFORM CARGA-PRE.
           PERFORM CARGA-MAR.
       L-A-DESDE.
           DISPLAY "ART.LEIDOS: " LINE 17 POSITION 3.
           DISPLAY "ART.IMPRESOS: " LINE 17 POSITION 25.
       L-A-POSI.
           OPEN INPUT ART02 ART01 ENVASE.
           OPEN INPUT NUMERO MARCAS.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           READ MARCAS INVALID KEY PERFORM CIERRE-A GO TO INI-MOD.
           MOVE DET-MAR TO DET-MAR-EDI DET-MAR-ED.
           DISPLAY DET-MAR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE 1 TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
           MOVE "X" TO COD3.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY PERFORM CIERRE-A
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE 0 TO CONT-P CONT-R CONT-I CONT-LEO.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<< LISTA " TO TIT-2-EDI.
           MOVE "ALFABETICA" TO TIT-3-EDI.
           MOVE " DE -PRECI" TO TIT-4-EDI.
           MOVE "OS- >>>   " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           IF PRE-COD = "S" OR PRE-COD = "s" 
                            OPEN OUTPUT PRELIS3
                            CLOSE PRELIS3
                            OPEN I-O PRELIS3
                            GO TO L-LEO-A.
           IF PRE-COD = "N" OR PRE-COD = "n"
                                  OPEN INPUT PRELIS3 
                                   PERFORM CIERRE-A
                                  GO TO IMPRE.
           GO TO LIST-ALF.
       L-LEO-A.
           READ ART02 NEXT WITH NO LOCK AT END PERFORM CIERRE-A
                                   GO TO IMPRE.
           ADD 1 TO CONT-LEO.
           DISPLAY CONT-LEO LINE 17 POSITION 15.
           IF EMP-ART02 > NUM-EMP PERFORM CIERRE-A
                                  GO TO IMPRE.
           IF SUC-ART02 > SUC-EMP PERFORM CIERRE-A 
                          GO TO IMPRE.
           IF MAR-ART02 NOT = LISTO-M GO TO L-LEO-A.
      *=-=-=-=-=-=-= NO LISTA SIN STOCK =-=-=-=-=-=-=-=-
      *     IF SAL-S-ART02 < 1 GO TO L-LEO-A.
      *=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           MOVE COD-ART02 TO COD-ART01.
           READ ART01 WITH NO LOCK INVALID KEY
                   GO TO L-LEO-A.
           MOVE MAR-ART02 TO MAR-PRELIS3.
           MOVE NUM-ART02 TO NUM-PRELIS3.
           MOVE LIN-1-ART01 TO LIN-1-PRELIS3.
           MOVE LIN-2-ART01 TO LIN-2-PRELIS3.
           MOVE LIN-3-ART01 TO LIN-3-PRELIS3.
           MOVE LIN-4-ART01 TO LIN-4-PRELIS3.
      *-=-=-=-=-= PRECIO DE LISTA SIN IVA -=-=-=-=-=-=-=-=-=-=-=-=-
      *     IF PRE-V-ART02 = 0 MOVE "U$S" TO MON-PRELIS3
      *                 MOVE PRE-E-ART02 TO UNI-PRELIS3
      *                 COMPUTE PRE-PRELIS3 ROUNDED = UNI-PRELIS3
      *                         * 1,18
      *          ELSE MOVE " $ " TO MON-PRELIS3
      *               MOVE PRE-V-ART02 TO UNI-PRELIS3
      *               COMPUTE PRE-PRELIS3 ROUNDED = UNI-PRELIS3
      *                         * 1,18.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      *=-=-=-=-=-=-=-= PRECIO DE LISTA CON IVA -=-=-=-=-=-=-=-=-=-=-
           IF PRE-V-ART02 = 0 MOVE "U$S" TO MON-PRELIS3
                     MOVE PRE-E-ART02 TO PRE-PRELIS3
                     COMPUTE UNI-PRELIS3 ROUNDED = COSTO-STOCK-ART01 
                                 * 1,21
                ELSE MOVE " $ " TO MON-PRELIS3
                     MOVE PRE-V-ART02 TO PRE-PRELIS3
                     COMPUTE UNI-PRELIS3 ROUNDED = COSTO-STOCK-ART01
                               * 1,21.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=     
           IF PRE-V-ART02 = 0 AND PRE-E-ART02 = 0 
                 MOVE SPACES TO MON-PRELIS3
                 MOVE 0 TO PRE-PRELIS3
                 COMPUTE UNI-PRELIS3 ROUNDED = COSTO-STOCK-ART01 *
                                1,21.
           MOVE NUM-ART02 TO COD-NUM.
           READ NUMERO WITH NO LOCK INVALID KEY 
                           MOVE "*******" TO DET-NUM.
           MOVE DET-NUM TO DET-PRELIS3.
           MOVE ENV-ART01 TO NUM-ENVA.
           READ ENVASE WITH NO LOCK INVALID KEY
                           MOVE "*******" TO NOM-ENVA.
           MOVE NOM-ENVA TO ENV-PRELIS3.      
           WRITE REG-PRELIS3 INVALID KEY REWRITE REG-PRELIS3.
           GO TO L-LEO-A.
       IMPRE.
           MOVE 0 TO CONT-R.
           PERFORM PANT-LIS-S.
           OPEN INPUT PRELIS3 LINEAS.
           OPEN OUTPUT IMP01.
           MOVE 0 TO LIN-1-AUX CONT-P.
           MOVE 0 TO LIN-2-AUX.
           MOVE 0 TO LIN-3-AUX.
           MOVE 0 TO LIN-4-AUX.
       IMPRE-SIGA.
           MOVE 0 TO LIN-1-PRELIS3 LIN-2-PRELIS3 LIN-3-PRELIS3.
           MOVE 0 TO LIN-4-PRELIS3.
           MOVE SPACES TO DET-PRELIS3.
           START PRELIS3 KEY IS NOT < CLAVE-PRELIS3
                      INVALID KEY PERFORM CIERRE-B
                      STOP RUN.
       IMPRE-S.
           IF CANT-LIS = 0 MOVE 0 TO CONT-R
                           PERFORM CIERRE-B
                           GO TO IMPRE.
           IF CONT-R = 0  PERFORM TITU.
           READ PRELIS3 NEXT WITH NO LOCK AT END 
                        PERFORM PIE-P
                        COMPUTE CANT-LIS = CANT-LIS - 1
                        COMPUTE CONT-R = 72 - CONT-R
                        PERFORM ESPA CONT-R TIMES
                        MOVE 0 TO CONT-R CONT-I CONT-P
                        GO TO IMPRE-SIGA.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           IF MAR-PRELIS3 NOT = LISTO-M GO TO IMPRE-S.
           IF LIN-1-AUX = 0 MOVE LIN-1-PRELIS3 TO LIN-1-AUX 
                            MOVE LIN-2-PRELIS3 TO LIN-2-AUX 
                            MOVE LIN-3-PRELIS3 TO LIN-3-AUX
                            MOVE LIN-4-PRELIS3 TO LIN-4-AUX
                            PERFORM IMP-LIN1 THRU IMP-LIN-F.
           IF LIN-1-PRELIS3 NOT = LIN-1-AUX
                         MOVE LIN-1-PRELIS3 TO LIN-1-AUX
                         MOVE LIN-2-PRELIS3 TO LIN-2-AUX
                         MOVE LIN-3-PRELIS3 TO LIN-3-AUX
                         MOVE LIN-4-PRELIS3 TO LIN-4-AUX
                         PERFORM IMP-LIN1 THRU IMP-LIN-F.
           IF LIN-2-PRELIS3 NOT = LIN-2-AUX
                         MOVE LIN-2-PRELIS3 TO LIN-2-AUX
                         MOVE LIN-3-PRELIS3 TO LIN-3-AUX
                         MOVE LIN-4-PRELIS3 TO LIN-4-AUX
                         PERFORM IMP-LIN2 THRU IMP-LIN-F.
           IF LIN-3-PRELIS3 NOT = LIN-3-AUX
                         MOVE LIN-3-PRELIS3 TO LIN-3-AUX
                         MOVE LIN-4-PRELIS3 TO LIN-4-AUX
                         PERFORM IMP-LIN3 THRU IMP-LIN-F.
           IF LIN-4-PRELIS3 NOT = LIN-4-AUX
                         MOVE LIN-4-PRELIS3 TO LIN-4-AUX
                         PERFORM IMP-LIN4 THRU IMP-LIN-F.
           GO TO IMPRE-1.
       IMP-LIN1.
           MOVE LIN-1-PRELIS3 TO COD-1-LIN.
           MOVE 0 TO COD-2-LIN COD-3-LIN COD-4-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY 
                    MOVE "*******" TO DET-LIN.
           MOVE DET-LIN TO DET-LIN1-ED.
           WRITE LIN-IMP01 FROM TIT-LIN1 AFTER 1.
           WRITE LIN-IMP01 FROM TIT-LIN1 AFTER 0.
           ADD 1 TO CONT-R.
       IMP-LIN2.
           IF LIN-2-PRELIS3 = 0 GO TO IMP-LIN-F.
           MOVE LIN-2-PRELIS3 TO COD-2-LIN.
           MOVE 0 TO COD-3-LIN COD-4-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY
                    MOVE "*******" TO DET-LIN.
           MOVE DET-LIN TO DET-LIN2-ED.
           WRITE LIN-IMP01 FROM TIT-LIN2 AFTER 1.
           ADD 1 TO CONT-R. 
       IMP-LIN3.
           IF LIN-3-PRELIS3 = 0 GO TO IMP-LIN-F.
           MOVE LIN-3-PRELIS3 TO COD-3-LIN.
           MOVE 0 TO COD-4-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY
                    MOVE "******" TO DET-LIN.
           MOVE DET-LIN TO DET-LIN2-ED.
           WRITE LIN-IMP01 FROM TIT-LIN2 AFTER 1.
           ADD 1 TO CONT-R.
       IMP-LIN4.
           IF LIN-4-PRELIS3 = 0 GO TO IMP-LIN-F.
           MOVE LIN-4-PRELIS3 TO COD-4-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY
                   MOVE "*******" TO DET-LIN.
           MOVE DET-LIN TO DET-LIN2-ED.
           WRITE LIN-IMP01 FROM TIT-LIN2 AFTER 1.
           ADD 1 TO CONT-R.
       IMP-LIN-F.
           EXIT.
       IMPRE-1.
           MOVE NUM-PRELIS3 TO NUM-ED.
           MOVE DET-PRELIS3 TO DET-ED.
           MOVE MON-PRELIS3 TO MON-ED.
           MOVE UNI-PRELIS3 TO SIN-ED.
           MOVE PRE-PRELIS3 TO CON-ED.
           MOVE ENV-PRELIS3 TO ENV-ED.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           DISPLAY CONT-I LINE 17 POSITION 39.
           GO TO IMPRE-S.
       IMPRE-1-F.
           EXIT.
       PANT-LIS.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "PREPARA ARCHIVO:" LINE 10 POSITION 3.
           DISPLAY "<S> Si - <N> No" LINE 10 POSITION 25.
           DISPLAY "MARCA A LISTAR :" LINE 13 POSITION 3.
      *=-=-=-=-=-=-=-=-=- MENSAJE SIN O CON STOCK =-=-=-=-=-=-=-
           DISPLAY " PRECAUCION: Lista sin STOCK " LINE 15 
                  POSITION 25 BLINK REVERSE.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           DISPLAY "CANTIDAD LISTAS:" LINE 19 POSITION 3. 
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
       PANT-LIS-S.
      *     PERFORM CARGA-COD.
           DISPLAY "INGRESE <0> PARA PESOS" LINE 16 POSITION 25.
      *     PERFORM CARGA-MON THRU C-MON-F.
           DISPLAY "                          " LINE 16 POSITION 25.
           PERFORM CARGA-CANT.
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
           WRITE LIN-IMP01 FROM TIT-MAR AFTER 1.
           WRITE LIN-IMP01 FROM TITULO-1 AFTER 1.
           PERFORM ESPA.
           ADD 7 TO CONT-R.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
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
           MOVE 122101 TO TAB-POS(12)
           MOVE 132104 TO TAB-POS(13).
           MOVE 133030 TO TAB-POS(14).
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
           MOVE 152002 TO TAB-POS(31).
           MOVE 192002 TO TAB-POS(32).
           MOVE 102001 TO TAB-POS(33).
       POSI. 
           START NUMERO KEY IS NOT < DET-NUM INVALID KEY
                            DISPLAY "ERROR POSI" LINE 17 POSITION 5
                            ACCEPT QQ
                            GO TO L-LEO-A.
           GO TO L-LEO-A.
       TIT-PAN.
           MOVE "     LISTA" TO TIT-1-PAN.
           MOVE "DO DE PREC" TO TIT-2-PAN.
           MOVE "IOS       " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F6>  LISTADO ALFABETICO " LINE 14 POSITION 18.
           DISPLAY "      POR MARCAS y LINEAS" LINE 15 POSITION 18.
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
           MOVE NUM-EMP TO EMP-ART02 EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       CARGA-MAR.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO LIST-ALF.
           MOVE NUM-CAP TO LISTO-M COD-MAR MAR-ED.
       CARGA-PRE.
           MOVE 33 TO SW3.
           ACCEPT PRE-COD LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
       CARGA-CANT.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO IMPRE.
           MOVE NUM-CAP TO CANT-LIS.
           IF CANT-LIS = 0 GO TO CARGA-CANT.
       CIERRE-A.
           CLOSE ART02 ART01 MARCAS NUMERO PRELIS3 ENVASE.
       CIERRE-B.
           CLOSE PRELIS3 IMP01 LINEAS.
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