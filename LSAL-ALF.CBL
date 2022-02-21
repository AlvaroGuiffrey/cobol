       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALDOS.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. AGO'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "CONDI.SEL".
       COPY "P-CTAS.SEL".
       COPY "CPOSTAL.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CUENTABM.REG".
       COPY "EMPREBM.REG".
       COPY "CPOSTAL.REG".
       COPY "IMP02.REG".
       COPY "P-CTAS.REG".
       COPY "CONDI.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER     PIC X(65) VALUE " ".
             03 FILLER     PIC X(20) VALUE "SALDOS CORRIENTES".
             03 FILLER     PIC X(20) VALUE "SALDOS ATRASADOS".
             03 FILLER     PIC X(20) VALUE "SALDOS MOROSOS ".
          02 IMP-2.
             03 FILLER     PIC X(7) VALUE "RUBRO: ".
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 RUBRO-ED   PIC Z(6)B.
             03 FILLER     PIC XX VALUE "- ".
             03 FILLER     PIC 99 COMP-4 VALUE 18.
             03 DET-RUBRO-ED  PIC X(30).
             03 FILLER     PIC 99 COMP-4 VALUE 20.
             03 FILLER     PIC 99 COMP-4 VALUE 15.
          02 IMP-3.
             03 CUENTA-ED      PIC Z(6)BB.
             03 SUB-CUENTA-ED  PIC ZZBB.
             03 NOMBRE-ED    PIC X(28)BB.
             03 ZONA-ED        PIC ZZBB.
             03 TEL-ED         PIC X(10)BB.
             03 SALDO-TOP-ED   PIC ----.---.---.--9,99BB.
             03 SALDO-CAN-ED   PIC ----.---.---.--9,99BB.
             03 SALDO-ED       PIC ----.---.---.--9,99BB.
             03 ESTADO-ED      PIC XX.
          02 IMP-4.
             03 FILLER     PIC X(5) VALUE "SON: ".
             03 CONT-I-ED  PIC Z(6)BB.
             03 FILLER     PIC X(21) VALUE "CUENTAS, QUE SUMAN $ ".
             03 TOTAL-ED   PIC -.---.---.--9,99.
             03 ATRASO-ED  PIC -.---.---.--9,99.
             03 MOROSO-ED  PIC -.---.---.--9,99.
       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 NOMBRE-CAP      PIC X(45).
          02 ESTADO-CAP        PIC XX.
          02 CONDICION-CAP     PIC 999 VALUE 0.
          02 DIRE-CAP          PIC X(30).
          02 COD-LOCA-CAP      PIC 9(4) VALUE 0.
          02 INSCRIPTO-CAP     PIC X.
          02 ZONA-CAP          PIC 99 VALUE 0.
          02 IGAN-CAP          PIC X(15).
          02 SALDO-ANT-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-CAN-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-TOP-CAP     PIC S9(13)V99 VALUE 0. 
          02 SALDO-CAP         PIC S9(13)V99 VALUE 0.
          02 TEL-CAP           PIC X(10).
          02 PMOV-CAP          PIC 9(6) VALUE 0.
          02 PPEND-CAP         PIC 9(6) VALUE 0.
          02 ING-BRUTOS-CAP    PIC 9(8) VALUE 0.
          02 TOTAL             PIC S9(13)V99 VALUE 0.
          02 ATRASO            PIC S9(13)V99 VALUE 0.
          02 MOROSO            PIC S9(13)V99 VALUE 0.
       01 EDICION.
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 SUB-CUENTA-EDI    PIC ZZ BLANK WHEN ZERO.
          02 NOMBRE-EDI      PIC X(45).
          02 ESTADO-EDI        PIC XX.
          02 CONDICION-EDI     PIC ZZZ BLANK WHEN ZERO.
          02 DIRE-EDI          PIC X(30).
          02 LOCA-IDE          PIC X(29).
          02 INSCRIPTO-EDI     PIC X.
          02 ZONA-EDI          PIC ZZ BLANK WHEN ZERO.
          02 IGAN-EDI          PIC X(15).
          02 SAL-C-EDI         PIC ----.---.---.--9,99.
          02 SAL-EDI           PIC ----.---.---.--9,99.
          02 SAL-T-EDI         PIC ----.---.---.--9,99.
          02 TEL-EDI           PIC X(10).
       01 AUXILIARES.
          02 LISTO-N-D         PIC 999999 VALUE 0.
          02 LISTO-N-H         PIC 999999 VALUE 0.
          02 LISTO-Z           PIC 99.
          02 LISTO-A-D         PIC X(30).
          02 LISTO-A-H         PIC X(30).
          02 CONT-L            PIC 9(6) VALUE 0.
          02 ZONA-L            PIC 99 VALUE 0.
          02 RUBRO-L           PIC 9(7) VALUE 0.
          02 CUENTA-H          PIC 9(6) VALUE 0.
          02 SUB-CUENTA-H      PIC 9(4) VALUE 0.
          02 NOMBRE-H          PIC X(30).
          02 TIPO              PIC 9 VALUE 0.
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CUENTA.ERR".
       COPY "P-CTA.ERR".
       COPY "CPOSTAL.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "CONDI.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CCMODA" TO NOM-PAN.
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
       CARGO-MOD.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) 
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.

           GO TO LIST-NUM.

           IF F05 GO TO LIST-NUM.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       LIST-NUM.
           OPEN INPUT CUENTAS P-DE-CTAS EMPRESAS CONDI CPOSTAL.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE "<< LISTADO" TO TIT-2-EDI.
           MOVE " SALDOS TO" TO TIT-3-EDI.
           MOVE "TALES DE C" TO TIT-4-EDI.
           MOVE "TAS.CTES >>" TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-2.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
       LIST-NUM0.
           PERFORM CARGA-RUB.
           OPEN OUTPUT IMP02.
       LIST-NUM1.
           MOVE 0 TO TOTAL.
           MOVE "N" TO COD2.
           PERFORM BORRA-PANTA-C1.
           PERFORM CARGA-CUE-D THRU CARGA-SUB-CUE-H.
           PERFORM CARGA-ZONA-L.
       RETOMA-TIPO.
           ACCEPT TIPO LINE 20 POSITION 50 NO BEEP TAB CONVERT
      *     MOVE 1 TO TIPO.

           IF TIPO < 1 OR TIPO > 3 GO TO RETOMA-TIPO.

           START CUENTAS KEY IS NOT < CLAVE-CTA
                  INVALID KEY GO TO LIST-NUM1.
           MOVE 0 TO CONT-R CONT-I CONT-P CONT-L.
       LIST-NUM2.
           IF CONT-P = 0 PERFORM TITU.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ CUENTAS NEXT AT END PERFORM PIE-P
                          PERFORM CIERRE-A
                          CLOSE IMP02
                          GO TO LIST-NUM.
           ADD 1 TO CONT-L.
           MOVE 37 TO SW3.
           DISPLAY CONT-L LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE.
           IF TIPO = 2 AND ESTADO NOT = "A" GO TO LIST-NUM2. 
           IF TIPO = 3 AND ESTADO NOT = "M" GO TO LIST-NUM2.

           IF RUBRO-CTA > RUBRO-L PERFORM PIE-P
                                PERFORM CIERRE-A
                                CLOSE IMP02
                                GO TO LIST-NUM.
           IF CUENTA > CUENTA-H PERFORM PIE-P
                                PERFORM CIERRE-A 
                                CLOSE IMP02
                                GO TO LIST-NUM.
           IF CUENTA = CUENTA-H AND SUB-CUENTA > SUB-CUENTA-H
                                PERFORM PIE-P
                                PERFORM CIERRE-A
                                CLOSE IMP02
                                GO TO LIST-NUM.
           IF ZONA-L NOT = 0
               IF ZONA NOT = ZONA-L GO TO LIST-NUM2.
           IF SALDO = 0 GO TO LIST-NUM2.
           ADD 1 TO CONT-I.
           MOVE 38 TO SW3.
           DISPLAY CONT-I LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE. 
           MOVE CUENTA TO CUENTA-ED.
           MOVE SUB-CUENTA TO SUB-CUENTA-ED.
           MOVE NOMBRE-C TO NOMBRE-ED.
           MOVE ZONA TO ZONA-ED.
           MOVE TEL TO TEL-ED.
           MOVE ESTADO TO ESTADO-ED.
           MOVE SALDO TO SALDO-ED.
           MOVE SALDO-TOP TO SALDO-TOP-ED.
           MOVE SALDO-CAN TO SALDO-CAN-ED.
           IF ESTADO = "A" COMPUTE ATRASO = ATRASO + SALDO.
           IF ESTADO = "M" COMPUTE MOROSO = MOROSO + SALDO.
           IF ESTADO NOT = "A" AND ESTADO NOT = "M"
                           COMPUTE TOTAL = TOTAL + SALDO.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           IF ESTADO NOT = "N"
               WRITE LIN-IMP02 FROM IMP-3 AFTER 0.
           ADD 1 TO CONT-R.
           GO TO LIST-NUM2.
       LIST-ALF.
           OPEN INPUT CUENTAS P-DE-CTAS EMPRESAS CONDI CPOSTAL.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE "  <<< LIST" TO TIT-2-EDI.
           MOVE "ADO ALFABE" TO TIT-3-EDI.
           MOVE "TICO DE SA" TO TIT-4-EDI.
           MOVE "LDOS >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-3.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
       LIST-ALF0.
           PERFORM CARGA-RUB.
           OPEN OUTPUT IMP02.
       LIST-ALF1.
           MOVE 0 TO TOTAL.
           MOVE "A" TO COD2.
           PERFORM BORRA-PANTA-C2.
           PERFORM CARGA-NOM-D THRU CARGA-NOM-H.
           PERFORM CARGA-ZONA-L.
       RETOMA-TIPO1.
           ACCEPT TIPO LINE 20 POSITION 50 NO BEEP TAB CONVERT
           IF TIPO < 1 OR TIPO > 3 GO TO RETOMA-TIPO1.
           START CUENTAS KEY IS NOT < NOMBRE-C
                  INVALID KEY GO TO LIST-ALF1.
           MOVE 0 TO CONT-R CONT-I CONT-P CONT-L.
       LIST-ALF2.
           IF CONT-P = 0 PERFORM TITU.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ CUENTAS NEXT AT END PERFORM PIE-P
                          PERFORM CIERRE-A
                          CLOSE IMP02
                          GO TO LIST-ALF.
           IF TIPO = 2 AND ESTADO NOT = "A" GO TO LIST-ALF2.
           IF TIPO = 3 AND ESTADO NOT = "M" GO TO LIST-ALF2.

           ADD 1 TO CONT-L.
           MOVE 37 TO SW3.
           DISPLAY CONT-L LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE.
           IF RUBRO-CTA NOT = RUBRO-L GO TO LIST-ALF2.
           IF NOMBRE-C > NOMBRE-H PERFORM PIE-P
                                PERFORM CIERRE-A 
                                CLOSE IMP02
                                GO TO LIST-ALF.
           IF ZONA-L NOT = 0
              IF ZONA NOT = ZONA-L GO TO LIST-ALF2.
           IF SALDO = 0 GO TO LIST-ALF2.
           ADD 1 TO CONT-I.
           MOVE 38 TO SW3.
           DISPLAY CONT-I LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE. 
           MOVE CUENTA TO CUENTA-ED.
           MOVE SUB-CUENTA TO SUB-CUENTA-ED.
           MOVE NOMBRE-C TO NOMBRE-ED.
           MOVE ZONA TO ZONA-ED.
           MOVE TEL TO TEL-ED.
           MOVE SALDO TO SALDO-ED.
           MOVE SALDO-TOP TO SALDO-TOP-ED.
           MOVE SALDO-CAN TO SALDO-CAN-ED.
           MOVE ESTADO TO ESTADO-ED.
           COMPUTE TOTAL = TOTAL + SALDO.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           IF ESTADO NOT = "N"
               WRITE LIN-IMP02 FROM IMP-3 AFTER 0.
           ADD 1 TO CONT-R.
           GO TO LIST-ALF2.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           PERFORM FECHA-ROT132.
           PERFORM HORA-ROT132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           ADD 8 TO CONT-R.
       PIE-P.
           MOVE CONT-I TO CONT-I-ED.
           MOVE TOTAL TO TOTAL-ED.
           MOVE ATRASO TO ATRASO-ED
           MOVE MOROSO TO MOROSO-ED

           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
           ADD 2 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       ESPA.
           MOVE SPACES TO LIN-IMP02.
           WRITE LIN-IMP02 AFTER 1.
           ADD 1 TO CONT-R.
       CARGA-EMP.
           PERFORM MEN-MODA-S.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-CTA.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP SUC-CTA NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.       
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           MOVE VERSION-EMP TO P-VERSION.
       CARGA-EMP-F.
           EXIT.
       CARGA-RUB.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUC.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO P-RUBRO RUBRO-CTA RUBRO-ED RUBRO-L.
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
           MOVE P-NOMBRE TO DET-RUBRO-ED.
       CARGA-CUE.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           IF NUM-CAP = 0 GO TO CARGA-CUE.
           MOVE NUM-CAP TO CUENTA.
           MOVE 0 TO SUB-CUENTA.
           READ CUENTAS INVALID KEY GO TO CARGA-SUB-CUE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       CARGA-SUB-CUE.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE.
      *-=-=-=-=-=- INVALIDA SUB CUENTAS =-=-=-=-=-=-=-=-=-=
           IF NUM-CAP NOT = 0 GO TO CARGA-SUB-CUE.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE NUM-CAP TO SUB-CUENTA.
       CARGA-CUE-F.
           EXIT.    
       CARGA-NOM.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUB-CUE.
           IF F03 PERFORM DESP-NOM GO TO CARGA-DIR.
           MOVE ALF-CAP TO NOMBRE-C.
       CARGA-DIR.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-NOM.
           IF F03 PERFORM DESP-DIR GO TO CARGA-COD-L.
           MOVE ALF-CAP TO DIRE.
       CARGA-COD-L.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-DIR.
           IF F03 PERFORM DESP-COD-L GO TO CARGA-TEL.
           IF NUM-CAP = 0 GO TO CARGA-COD-L.
           MOVE NUM-CAP TO C-POS COD-LOCA.
           PERFORM DESP-COD-L.
       CARGA-TEL.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COD-L.
           IF F03 PERFORM DESP-TEL GO TO CARGA-ZON.
           MOVE ALF-CAP TO TEL.
       CARGA-ZON.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO  INI-MOD.
           IF F02 GO TO CARGA-TEL.
           IF F03 PERFORM DESP-ZON GO TO CARGA-EST.
           MOVE NUM-CAP TO ZONA.
       CARGA-EST.
           MOVE " <N> NORMAL ; <" TO MEN-1.
           MOVE "A> ATRASO y <M>" TO MEN-2.
           MOVE " MOROSO        " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-ZON.
           IF F03 PERFORM DESP-EST GO TO CARGA-CON.
           MOVE ALF-CAP TO ESTADO.
           PERFORM DESP-EST.
           PERFORM MEN-ANTE-DES.
       CARGA-CON.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EST.
           IF F03 PERFORM DESP-CON GO TO CARGA-INS.
           MOVE NUM-CAP TO CONDICION NUM-CONDI.
           PERFORM DESP-CON.
       CARGA-INS.
           MOVE "<R> RESP.INSC. " TO MEN-1.
           MOVE "<N> R.NO INSC. " TO MEN-2.
           MOVE "<C> CONS.FINAL " TO MEN-3.
           MOVE "<E> EXENTO     " TO MEN-4.
           MOVE SPACES TO MEN-5.
           PERFORM MENS-PAN.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CON.
           IF F03 PERFORM DESP-IGA.
           MOVE ALF-CAP TO INSCRIPTO.
           PERFORM MEN-MODA-S.
           PERFORM MEN-ANTE-DES.
       CARGA-IGA.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           IF INSCRIPTO = "C" MOVE SPACES TO IGAN 
                GO TO CARGA-SAL-TOP.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-INS.
           IF F03 PERFORM DESP-IGA GO TO CARGA-SAL-TOP.
           MOVE ALF-CAP TO IGAN.
       CARGA-SAL-TOP.
           PERFORM MEN-ANTE.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           ACCEPT SALDO-TOP-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-IGA.
           MOVE SALDO-TOP-CAP TO SALDO-TOP.
           PERFORM DESP-SAL-TOP.
       CARGA-CUE-D.
           PERFORM MEN-ANTE.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO LIST-NUM.
           IF F02 CLOSE IMP02 GO TO LIST-NUM0.
           MOVE NUM-CAP TO CUENTA.
       CARGA-SUB-CUE-D.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO LIST-NUM.
           IF F02 GO TO CARGA-CUE-D.
           MOVE NUM-CAP TO SUB-CUENTA.
       CARGA-CUE-H.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO LIST-NUM.
           IF F02 GO TO CARGA-SUB-CUE-D.
           MOVE NUM-CAP TO CUENTA-H.
           IF CUENTA-H < CUENTA GO TO CARGA-CUE-H.
       CARGA-SUB-CUE-H.
           MOVE 33 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO LIST-NUM.
           IF F02 GO TO CARGA-CUE-H.
           MOVE NUM-CAP TO SUB-CUENTA-H.
           IF SUB-CUENTA-H < SUB-CUENTA GO TO CARGA-SUB-CUE-H.
       CARGA-ZONA-L.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO INI-MOD.
           IF F02 GO TO VEO-DONDE.
           MOVE NUM-CAP TO ZONA-L.
       VEO-DONDE.
           IF COD2 = "N" GO TO CARGA-SUB-CUE-H.
           IF COD2 = "A" GO TO CARGA-NOM-H.
       CARGA-NOM-D.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO INI-MOD.
           IF F02 CLOSE IMP02 GO TO LIST-ALF0.
           MOVE ALF-CAP TO NOMBRE-C.
       CARGA-NOM-H.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP02 GO TO INI-MOD.
           IF F02 GO TO CARGA-NOM-D.
           MOVE ALF-CAP TO NOMBRE-H.
           IF NOMBRE-H < NOMBRE-C GO TO CARGA-NOM-H.    
       DESP-CUE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY SUB-CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-DIR.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY DIRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
       DESP-COD-L.
           MOVE COD-LOCA TO C-POS.
           READ CPOSTAL INVALID KEY MOVE "******" TO NOM-POSTAL.
           MOVE 13 TO SW3.
           DISPLAY COD-LOCA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE 14 TO SW3.
           DISPLAY NOM-POSTAL LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-TEL.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY TEL LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-ZON.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY ZONA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
       DESP-EST.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY ESTADO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) BLINK REVERSE.
       DESP-CON.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE CONDICION TO NUM-CONDI.
           READ CONDI INVALID KEY
                MOVE "NO EXISTE CONDICION" TO NOM-CONDI.
           IF NUM-CONDI = 0
              MOVE "- SIN CONDICION - " TO NOM-CONDI.
           DISPLAY CONDICION LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE 19 TO SW3.
           PERFORM BORRA.
           DISPLAY NOM-CONDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
       DESP-INS.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           DISPLAY INSCRIPTO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-IGA.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           DISPLAY IGAN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
       DESP-SAL-TOP.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE SALDO-TOP TO SAL-T-EDI.
           DISPLAY SAL-T-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-SAL.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE SALDO TO SAL-EDI.
           DISPLAY SAL-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-SAL-CAN.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE SALDO-CAN TO SAL-C-EDI.
           DISPLAY SAL-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       MEN-NO-CTA.
           MOVE "-- RUBRO NO HAB" TO MEN-1.
           MOVE "ILITADO PARA CU" TO MEN-2.
           MOVE "ENTAS CTES. -- " TO MEN-3.
           PERFORM MENS-PAN.    
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "CUENTA   :          SUB:  " LINE 10 POSITION 5.
           DISPLAY "NOMBRE   : " LINE 11 POSITION 5.
           DISPLAY "DIRECCION:" LINE 12 POSITION 5.
           DISPLAY "C.POSTAL :          LOC: " LINE 13 POSITION 5.
           DISPLAY "TELEFONO :" LINE 14 POSITION 5.
           DISPLAY "ZONA     :" LINE 15 POSITION 5.
           DISPLAY "ESTADO   :" LINE 16 POSITION 5.
           DISPLAY "CONDICION:       - " LINE 16 POSITION 25.
           DISPLAY "C.U.I.T. :" LINE 17 POSITION 25.
           DISPLAY "INSCRIPTO:" LINE 17 POSITION 5.
           DISPLAY "SALDO TOPE CREDITO :" LINE 19 POSITION 5.
           DISPLAY "SALDO ACTUAL       :" LINE 20 POSITION 5.
           DISPLAY "SALDO CIERRE ANTER.:" LINE 21 POSITION 5.
       PANTA-2.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "------------ D E S D E -------------" LINE 11
                               POSITION 5.
           DISPLAY "CUENTA   :          SUB:  " LINE 12 POSITION 5.
           DISPLAY "------------ H A S T A -------------" LINE 14
                               POSITION 5.
           DISPLAY "CUENTA   :          SUB:  " LINE 15 POSITION 5.
           DISPLAY "ZONA     :" LINE 18 POSITION 5.
           DISPLAY " INGRESE -0- LISTA " LINE 17 POSITION 25 REVERSE.
           DISPLAY "      TODAS        " LINE 18 POSITION 25 REVERSE.
           DISPLAY "   LAS  ZONAS      " LINE 19 POSITION 25 REVERSE.
           DISPLAY "1-NORMAL 2-ATRASADOS 3-MOROSOS" LINE 20 
                    POSITION 5 REVERSE.
           DISPLAY "REG. LEIDOS :" LINE 21 POSITION 5.
           DISPLAY "REG. IMPRESOS :" LINE 21 POSITION 35.
       PANTA-3.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "------------ D E S D E -------------" LINE 11
                               POSITION 5.
           DISPLAY "NOMBRES  : " LINE 12 POSITION 5.
           DISPLAY "------------ H A S T A -------------" LINE 14
                               POSITION 5.
           DISPLAY "NOMBRES  : " LINE 15 POSITION 5.
           DISPLAY "ZONA     : " LINE 18 POSITION 5.
           DISPLAY " INGRESE -0- LISTA " LINE 17 POSITION 25 REVERSE.
           DISPLAY "      TODAS        " LINE 18 POSITION 25 REVERSE.
           DISPLAY "   LAS  ZONAS      " LINE 19 POSITION 25 REVERSE.
           DISPLAY "1-NORMAL 2-ATRASADOS 3-MOROSOS" LINE 20 
                      POSITION 5 REVERSE
           DISPLAY "REG. LEIDOS :" LINE 21 POSITION 5.
           DISPLAY "REG. IMPRESAS :" LINE 21 POSITION 35.             
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 081607 TO TAB-POS(7).
           MOVE 083030 TO TAB-POS(8).
           MOVE 101606 TO TAB-POS(9).
           MOVE 103004 TO TAB-POS(10).
           MOVE 111630 TO TAB-POS(11).
           MOVE 121630 TO TAB-POS(12).
           MOVE 131604 TO TAB-POS(13).
           MOVE 133030 TO TAB-POS(14).
           MOVE 141610 TO TAB-POS(15).
           MOVE 151602 TO TAB-POS(16).
           MOVE 161601 TO TAB-POS(17).
           MOVE 163703 TO TAB-POS(18).
           MOVE 164525 TO TAB-POS(19).
           MOVE 171601 TO TAB-POS(20).
           MOVE 173715 TO TAB-POS(21).
           MOVE 192719 TO TAB-POS(22).
           MOVE 202719 TO TAB-POS(23).
           MOVE 212719 TO TAB-POS(24).
           MOVE 121606 TO TAB-POS(30).
           MOVE 123004 TO TAB-POS(31).
           MOVE 151606 TO TAB-POS(32).
           MOVE 153004 TO TAB-POS(33).
           MOVE 181602 TO TAB-POS(34).
           MOVE 121630 TO TAB-POS(35).
           MOVE 151630 TO TAB-POS(36).
           MOVE 211906 TO TAB-POS(37).
           MOVE 215006 TO TAB-POS(38).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO  DE  CL" TO TIT-2-PAN.
           MOVE "IENTES -  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO NUMERICO " LINE 14 POSITION 18.
           DISPLAY "<F6>  LISTADO ALFABETICO" LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 16 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
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
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 15 TO SW3.
           PERFORM BORRA.
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
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
       BORRA-PANTA-C1.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           MOVE 33 TO SW3.
           PERFORM BORRA.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           MOVE 37 TO SW3.
           PERFORM BORRA.
           MOVE 38 TO SW3.
           PERFORM BORRA.
       BORRA-PANTA-C2.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           MOVE 37 TO SW3.
           PERFORM BORRA.
           MOVE 38 TO SW3.
           PERFORM BORRA.
       CIERRE-A.
           CLOSE CUENTAS P-DE-CTAS CONDI EMPRESAS CPOSTAL.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".
