       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRESA.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. SET'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "EMPRE.SEL".
           COPY "CPOSTAL.SEL".
           COPY "PANTA.SEL".
           COPY "IMP02.SEL".
           COPY "ACTI.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "EMPREBM.REG".
           COPY "CPOSTAL.REG".
           COPY "PANTA.REG".
           COPY "IMP02.REG".
           COPY "ACTI.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
      *...............................(LINEAS DE IMPRESION)
       01 IMPRESION.
          02 COMPRI.
             03 AA               PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 NUM-ED       PIC ZZZZB(4).
             03 SUC-ED       PIC ZZB(4).
             03 VERSION-ED   PIC 99B(4).
             03 RAZ-SOC-ED   PIC X(30)B(4).
             03 CALLE-ED     PIC X(30)B(4).
             03 COD-LOC-ED   PIC 9(4)B(4).
             03 NOM-POSTAL-ED PIC X(35).
          02 IMP-2.
             03 FILLER       PIC X(21).
             03 CUIT-ED      PIC X(15)B(4).
             03 IVA-ED       PIC X(15)B(4).
             03 ING-BRUTOS-ED PIC X(15)B(4).
             03 MUNI-ED      PIC X(15)B(4).
             03 JUB-ED       PIC X(15)B(4).
             03 AP-JUB-ED    PIC X(15).
          02 TIT-1.
             03 FILLER       PIC X(8) VALUE "NUM.".
             03 FILLER       PIC X(6) VALUE "SUC.".
             03 FILLER       PIC X(6) VALUE "VER.".
             03 FILLER       PIC X(15) VALUE "----------- NOM".
             03 FILLER       PIC X(19) VALUE "BRE -----------".
             03 FILLER       PIC X(15) VALUE "--------- DIREC".
             03 FILLER       PIC X(19) VALUE "CION ----------".
             03 FILLER       PIC X(8) VALUE "C.P.".
             03 FILLER       PIC X(15) VALUE "------------ LO".
             03 FILLER       PIC X(20) VALUE "CALIDAD ------------".
          02 TIT-2.
             03 FILLER       PIC X(21) VALUE SPACES.
             03 FILLER       PIC X(19) VALUE "-- C.U.I.T. ---".
             03 FILLER       PIC X(19) VALUE "---- I.V.A. ---".
             03 FILLER       PIC X(19) VALUE "INGRESOS BRUTOS".
             03 FILLER       PIC X(19) VALUE "INSC. MUNICIPAL".
             03 FILLER       PIC X(19) VALUE "C.N.R.P. - PROP".
             03 FILLER       PIC X(15) VALUE "C.N.R.P. - EMPL". 
      *...............................(CAMPOS DE CAPTURA)
      * 01 CAPTURA.
      *    02 @@@@@@@@@@@@
      *...............................(CAMPOS DE EDICION)
       01 EDICION.
          02 NUM-E-EDI         PIC 9999.
          02 SUC-EDI         PIC 99.
          02 COD-LOC-EDI     PIC 9999.
          02 MONEDA-EDI      PIC 99.
          02 VERSION-EDI     PIC 99.
          02 SEC-AUX-EDI     PIC X(15).
          02 SIT-AUX-EDI     PIC X(15).
      *...............................(CAMPOS AUXILIARES)
       01 AUXILIARES.
          02 CODI            PIC 9.
       COPY "VARIOS.WOR".
      *    02 @@@@@@@@@@@@
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CPOSTAL.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "ACTI.ERR".
      *...............................(*.ERR)
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
      *...............................(ABRE PANTALLA)
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
       CARGO-MOD.
      *...............................(SELECCIONA TAREAS)
           MOVE 3 TO SW3.
           PERFORM BORRA.
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
       
      *................................(PROGRAMA PRINCIPAL)
       ACTU.
           OPEN INPUT CPOSTAL EMPRESAS ACTI.
           MOVE " ALTA, BAJA Y MODIF. " TO D20-CAP.
           MOVE 0 TO CODI.
           PERFORM MODA-PAN.
       PANTA-CAP.
           PERFORM BORRA-PANTA.
           DISPLAY "NUMERO        :" LINE 8 POSITION 5.
           DISPLAY "SUCURSAL      :" LINE 9 POSITION 5.
           DISPLAY "NOMBRE        :" LINE 10 POSITION 5.
           DISPLAY "DOMICILIO     :" LINE 11 POSITION 5.
           DISPLAY "COD.POSTAL    :" LINE 11 POSITION 55.
           DISPLAY "COMENTARIO 1  :" LINE 13 POSITION 5.
           DISPLAY "COMENTARIO 2  :" LINE 14 POSITION 5.
           DISPLAY "COMENTARIO 3  :" LINE 15 POSITION 5.
           DISPLAY "COMENTARIO 4  :" LINE 16 POSITION 5.
           DISPLAY "C.U.I.T.      :" LINE 18 POSITION 5.
           DISPLAY "I.V.A.        :" LINE 18 POSITION 40.
           DISPLAY "ING.BRUTOS    :" LINE 19 POSITION 5.
           DISPLAY "INSC.MUNICIP. :" LINE 19 POSITION 40.
           DISPLAY "D.N.R.P.-PROP.:" LINE 20 POSITION 5.
           DISPLAY "D.N.R.P.-EMPL.:" LINE 20 POSITION 40.
           DISPLAY "ACT.PRINC.PRO.:" LINE 21 POSITION 5.
           DISPLAY "ACT.PRINC.MUN.:" LINE 21 POSITION 40.
           DISPLAY "VERSION P.CTA.:" LINE 22 POSITION 5.
           DISPLAY "MONEDA        :" LINE 22 POSITION 40.
           DISPLAY "SECTOR RES.127:" LINE 23 POSITION 5.
           DISPLAY "SITUACION I.BR:" LINE 23 POSITION 40
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       CARGUE-NUM.
           PERFORM BORRA-P-CAP.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGUE-NUM.
           MOVE NUM-CAP TO NUM-EMP.
           PERFORM DESP-NUM.
       CARGUE-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-NUM.
           MOVE NUM-CAP TO SUC-EMP.
           PERFORM DESP-SUC.
       SIGA-ACTU.
           READ EMPRESAS INVALID KEY GO TO ALTA.
           PERFORM DESP-NOM THRU DESP-SIT.
           PERFORM MEN-B-M.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F09 MOVE 9 TO CODI
                  PERFORM CARGUE-CAL THRU CARGUE-SIT
                  CLOSE EMPRESAS
                  OPEN I-O EMPRESAS
                  REWRITE REG-EMP INVALID KEY WRITE REG-EMP
                  CLOSE EMPRESAS
                  OPEN INPUT EMPRESAS
                  MOVE 0 TO CODI
                  UNLOCK EMPRESAS RECORD
                  GO TO CARGUE-NUM.
           UNLOCK EMPRESAS RECORD.
           IF F08 GO TO BORRE.
           GO TO CARGUE-NUM.
       ALTA.
           PERFORM CARGUE-NOM THRU CARGUE-SIT.
           MOVE 999 TO CLAVE-1-EMP.
           MOVE 999 TO CLAVE-2-EMP.
           MOVE 999 TO CLAVE-3-EMP.
           MOVE 0 TO REP-LOC-EMP.
           MOVE 0 TO IVA-C-EMP.
           CLOSE EMPRESAS.
           OPEN I-O EMPRESAS.
           WRITE REG-EMP INVALID KEY REWRITE REG-EMP.
           CLOSE EMPRESAS.
           OPEN INPUT EMPRESAS.
           GO TO CARGUE-NUM.
       BORRE.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 CLOSE EMPRESAS
                  OPEN I-O EMPRESAS
                  DELETE EMPRESAS
                  CLOSE EMPRESAS
                  OPEN INPUT EMPRESAS
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  GO TO CARGUE-NUM.
           PERFORM MEN-NO-BAJA 50 TIMES.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           GO TO CARGUE-NUM.    
       CONS-NUM.
           GO TO INI-MOD.
       LIST-NUM.
           GO TO INI-MOD.
       CIERRE-A.
           CLOSE CPOSTAL EMPRESAS ACTI.
      *................................(CARGA DATOS)
       CARGUE-NOM.
           IF CODI = 9 GO TO CARGUE-CAL.
           PERFORM MEN-ANTE-DES.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-SUC.
           IF F03 PERFORM DESP-NOM
                  GO TO CARGUE-CAL.
           MOVE ALF-CAP TO RAZ-SOC-EMP.
           PERFORM DESP-NOM.
       CARGUE-CAL.
           PERFORM MEN-ANTE-DES.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-NOM.
           IF F03 PERFORM DESP-CAL
                  GO TO CARGUE-COD.
           MOVE ALF-CAP TO CALLE-EMP.
           PERFORM DESP-CAL.
       CARGUE-COD.
           PERFORM MEN-ANTE-DES.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-CAL.
           IF F03 PERFORM DESP-COD
                  GO TO CARGUE-COM-1.
           IF NUM-CAP = 0 GO TO CARGUE-COD.
           MOVE NUM-CAP TO COD-LOC-EMP C-POS.
      *-=-=-=-= CODIGO POSTAL CON SUBINDICE =-=-=-=-=-=-==-
           ACCEPT  C-POS-1 LINE 11 POSITION 76 NO BEEP TAB CONVERT
           MOVE C-POS-1  TO REP-LOC-EMP           
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=
           READ CPOSTAL WITH NO LOCK INVALID KEY GO TO CARGUE-COD.
           PERFORM DESP-COD.
       CARGUE-COM-1.
           PERFORM MEN-ANTE-DES.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COD.
           IF F03 PERFORM DESP-COM-1
                  GO TO CARGUE-COM-2.
           MOVE ALF-CAP TO COM-1-EMP.
           PERFORM DESP-COM-1.
       CARGUE-COM-2.
           PERFORM MEN-ANTE-DES.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COM-1.
           IF F03 PERFORM DESP-COM-2
                  GO TO CARGUE-COM-3.
           MOVE ALF-CAP TO COM-2-EMP.
           PERFORM DESP-COM-2.
       CARGUE-COM-3.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COM-2.
           IF F03 PERFORM DESP-COM-3 
                  GO TO CARGUE-COM-4.
           MOVE ALF-CAP TO COM-3-EMP.
       CARGUE-COM-4.
           PERFORM MEN-ANTE-DES.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COM-3.
           IF F03 PERFORM DESP-COM-4
                  GO TO CARGUE-CUIT.
           MOVE ALF-CAP TO COM-4-EMP.
       CARGUE-CUIT.
           PERFORM MEN-ANTE-DES.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-COM-4.
           IF F03 PERFORM DESP-CUIT
                  GO TO CARGUE-IVA.
           MOVE ALF-CAP TO CUIT-EMP.
       CARGUE-IVA.
           MOVE "<R>RESP.INSC;<N" TO MEN-1.
           MOVE ">R.NO INSC;<E>E" TO MEN-2.
           MOVE "XTO;<NR>NO RESP" TO MEN-3.
           PERFORM MENS-PAN. 
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-CUIT.
           IF F03 PERFORM DESP-IVA GO TO CARGUE-IB.
           IF ALF-CAP = "R" MOVE "RESP. INSCRIPTO" TO ALF-CAP.
           IF ALF-CAP = "N" MOVE "RESP. NO INSCR." TO ALF-CAP.
           IF ALF-CAP = "E" MOVE "EXENTO" TO ALF-CAP.
           IF ALF-CAP = "NR" MOVE "NO RESPONSABLE" TO ALF-CAP.
           MOVE ALF-CAP TO IVA-EMP.
           PERFORM DESP-IVA.
       CARGUE-IB.
           PERFORM MEN-ANTE-DES.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-CUIT.
           IF F03 PERFORM DESP-IB GO TO CARGUE-MUNI.
           MOVE ALF-CAP TO ING-BRUTOS-EMP.
           PERFORM DESP-IB.
       CARGUE-MUNI.
           PERFORM MEN-ANTE-DES.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-IB.
           IF F03 PERFORM DESP-MUNI GO TO CARGUE-JUB.
           MOVE ALF-CAP TO MUNI-EMP.
           PERFORM DESP-MUNI.
       CARGUE-JUB.
           PERFORM MEN-ANTE-DES.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-MUNI.
           IF F03 PERFORM DESP-JUB GO TO CARGUE-AP-JUB.
           MOVE ALF-CAP TO JUB-EMP.
           PERFORM DESP-JUB.
       CARGUE-AP-JUB.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-JUB.
           IF F03 PERFORM DESP-AP-JUB GO TO CARGUE-PRO.
           MOVE ALF-CAP TO AP-JUB-EMP.
           PERFORM DESP-AP-JUB.
       CARGUE-PRO.
           PERFORM MEN-ANTE-DES.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-AP-JUB.
           IF F03 PERFORM DESP-PRO GO TO CARGUE-MUN.
           MOVE 2 TO IMP-ACT.
           MOVE NUM-CAP TO ACT-ACT.
           IF NUM-CAP NOT = 0
               READ ACTI INVALID KEY GO TO  CARGUE-PRO.
           MOVE NUM-CAP TO ACT-PRO-EMP.
           PERFORM DESP-PRO.
           PERFORM DESP-D-PRO.
       CARGUE-MUN.
           PERFORM MEN-ANTE-DES.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-PRO.
           IF F03 PERFORM DESP-MUN GO TO CARGUE-VER.
           MOVE 3 TO IMP-ACT.
           MOVE NUM-CAP TO ACT-ACT. 
           IF NUM-CAP NOT = 0
             READ ACTI INVALID KEY GO TO CARGUE-MUN.
           MOVE NUM-CAP TO ACT-MUN-EMP.
           PERFORM DESP-MUN.
           PERFORM DESP-D-MUN.
       CARGUE-VER.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-MUN.
           IF F03 PERFORM DESP-VER GO TO CARGUE-MON.
           MOVE NUM-CAP TO VERSION-EMP.
           PERFORM DESP-VER.
       CARGUE-MON.
           PERFORM MEN-ANTE.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGUE-VER.
           IF F03 PERFORM DESP-MON
                  GO TO CARGUE-SEC.
           MOVE NUM-CAP TO TIP-MO.
           PERFORM DESP-MON.      
       CARGUE-SEC.
           MOVE "1-MINOR.;2-MAYO" TO MEN-1.
           MOVE "R.;3-MANUF.;4-I" TO MEN-2.
           MOVE "NDUST.;5-MAN.NO" TO MEN-3.
           MOVE "OBL.;6-IND.NO O" TO MEN-4.
           PERFORM MENS-PAN.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-SEC
                  GO TO CARGUE-MON.
           IF F03 PERFORM DESP-SEC
                   GO TO CARGUE-SIT.
           PERFORM MEN-ANTE-DES.
           IF NUM-CAP = 0 OR NUM-CAP > 6 GO TO CARGUE-SEC.
           MOVE NUM-CAP TO SEC-PRO-EMP.
           PERFORM DESP-SEC.
           IF SEC-PRO-EMP = 1
              IF ACT-PRO-EMP > 299 GO TO CARGUE-PRO.
           IF SEC-PRO-EMP = 2
              IF ACT-PRO-EMP < 301 OR ACT-PRO-EMP > 599 
                    GO TO CARGUE-PRO.
           IF SEC-PRO-EMP > 2
              IF ACT-PRO-EMP < 601 OR ACT-PRO-EMP > 980
                    GO TO CARGUE-PRO.
                  
       CARGUE-SIT.
           MOVE "1-INSCRIPTO; 2-" TO MEN-1.
           MOVE "NO INSCR.; 3-EX" TO MEN-2.
           MOVE "ENTO; 4-OT.JURI" TO MEN-3.
           MOVE "SD.            " TO MEN-4.
           PERFORM MENS-PAN.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-SIT
                  GO TO CARGUE-SEC.
           PERFORM MEN-ANTE-DES.
           IF NUM-CAP = 0 OR NUM-CAP > 4 GO TO CARGUE-SIT.
           MOVE NUM-CAP TO SIT-PRO-EMP.
           PERFORM DESP-SIT.
      *................................(DESPLEGA DATOS)
       DESP-NUM.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           MOVE NUM-EMP TO NUM-E-EDI.
           DISPLAY NUM-E-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-SUC.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE SUC-EMP TO SUC-EDI.
           DISPLAY SUC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-CAL.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           DISPLAY CALLE-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-COD.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE COD-LOC-EMP TO COD-LOC-EDI.
           DISPLAY COD-LOC-EDI LINE TAB-LIN(SW3)  
                 POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           DISPLAY REP-LOC-EMP LINE 11 POSITION 76 .
       DESP-COM-1.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY COM-1-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-COM-2.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY COM-2-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-COM-3.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY COM-3-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-COM-4.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY COM-4-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DESP-CUIT.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           DISPLAY CUIT-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-IVA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           DISPLAY IVA-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-IB.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY ING-BRUTOS-EMP LINE TAB-LIN(SW3)
                    POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MUNI.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY MUNI-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-JUB.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY JUB-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-AP-JUB.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           DISPLAY AP-JUB-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-PRO.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           DISPLAY ACT-PRO-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-D-PRO.
           MOVE 2 TO IMP-ACT.
           MOVE ACT-PRO-EMP TO ACT-ACT.
           READ ACTI INVALID KEY MOVE "*******" TO DET-ACT.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-MUN.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           DISPLAY ACT-MUN-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-D-MUN.
           MOVE 3 TO IMP-ACT.
           MOVE ACT-MUN-EMP TO ACT-ACT.
           READ ACTI INVALID KEY MOVE "********" TO DET-ACT.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-VER.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE VERSION-EMP TO VERSION-EDI
           DISPLAY VERSION-EDI LINE TAB-LIN(SW3)
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MON.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE TIP-MO TO MONEDA-EDI
           DISPLAY MONEDA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-SEC.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           DISPLAY SEC-PRO-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE SPACES TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 1 MOVE " MINORISTA " TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 2 MOVE " MAYORISTA " TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 3 MOVE "MANUFACTURERO" TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 4 MOVE "INDUSTRIAL" TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 5 MOVE "MANUF.NO OBLIG." TO SEC-AUX-EDI.
           IF SEC-PRO-EMP = 6 MOVE "IND.NO OBLIG." TO SEC-AUX-EDI.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           DISPLAY SEC-AUX-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) BLINK.
       DESP-SIT.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           DISPLAY SIT-PRO-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE SPACES TO SIT-AUX-EDI.
           IF SIT-PRO-EMP = 1 MOVE " INSCRIPTO " TO SIT-AUX-EDI.
           IF SIT-PRO-EMP = 2 MOVE " NO INSCR. " TO SIT-AUX-EDI.
           IF SIT-PRO-EMP = 3 MOVE " EXENTO  " TO SIT-AUX-EDI.
           IF SIT-PRO-EMP = 4 MOVE " OT.JURISD." TO SIT-AUX-EDI.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           DISPLAY SIT-AUX-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) BLINK.
       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 082104 TO TAB-POS(4).
           MOVE 092102 TO TAB-POS(5).
           MOVE 102130 TO TAB-POS(6).
           MOVE 112130 TO TAB-POS(7).
           MOVE 117104 TO TAB-POS(8).
           MOVE 132130 TO TAB-POS(9).
           MOVE 142130 TO TAB-POS(10).
           MOVE 152130 TO TAB-POS(11).
           MOVE 162130 TO TAB-POS(12).
           MOVE 182115 TO TAB-POS(13).
           MOVE 185615 TO TAB-POS(14).
           MOVE 192115 TO TAB-POS(15).
           MOVE 195615 TO TAB-POS(16).
           MOVE 202115 TO TAB-POS(17).
           MOVE 205615 TO TAB-POS(18).
           MOVE 222102 TO TAB-POS(19).
           MOVE 225602 TO TAB-POS(20).
           MOVE 212103 TO TAB-POS(21).
           MOVE 215603 TO TAB-POS(22).
           MOVE 212514 TO TAB-POS(23).
           MOVE 216014 TO TAB-POS(24).
           MOVE 232102 TO TAB-POS(25).
           MOVE 232515 TO TAB-POS(26).
           MOVE 235602 TO TAB-POS(27).
           MOVE 236015 TO TAB-POS(28).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE " - ARCHIVO" TO TIT-1-PAN.
           MOVE "  DE  EMPR" TO TIT-2-PAN.
           MOVE "ESAS -    " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MOD." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONS. NUMERICO" LINE 15 POSITION 18.
           DISPLAY "<F5>  LIST. NUMERICO" LINE 16 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 17 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-P-CAP.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO  SW3.
           PERFORM BORRA.
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
           MOVE 25 TO SW3.
           PERFORM BORRA.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           MOVE 28 TO SW3.
           PERFORM BORRA.
      * PANTA-1.
      *     ...... DETALLAR PANTALLAS.........
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "ROT132.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
  
