       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISRES.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. ENE'94.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *...............................(*.SEL)
       COPY "MANEJO.SEL".
       COPY "RES127.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
      *...............................(*.REG)
       COPY "MANEJO.REG".
       COPY "RES127.REG".
       COPY "EMPREBM.REG".
       COPY "CUENTA.REG".
       COPY "IMP02.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
      *...............................(ROTULO DE IMPRESION)
       COPY "ROT132.IMP".
      *...............................(LINEAS DE IMPRESION)
       01 IMPRESION.
          02 COMPRI.
             03 A01        PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 I-BRU-ED   PIC X(15)BB.
             03 DESC-ED    PIC X(20)BB.
             03 DIA-ED     PIC 99/.
             03 MES-ED     PIC 99/.
             03 ANO-ED     PIC 99BB.
             03 CAT-ED     PIC 999BB.
             03 TIP-COM-ED PIC 9BB.
             03 SUB-T-C-ED PIC X.
             03 FILLER     PIC X VALUE "-".
             03 SUC-C-ED   PIC 9(4).
             03 FILLER     PIC X VALUE "-".
             03 COM-ED     PIC 9(8)BB.
             03 IMP-ED     PIC ---.---.--9,99BB.
             03 PER-ED     PIC ---.---.--9,99BB.
             03 POR-ED     PIC 99,999B.
             03 FILLER     PIC X VALUE "%".
       01 IMP-2.
             03 FILLER     PIC X(74) VALUE SPACES.
             03 IMP-T-ED   PIC ---.---.--9,99BB.
             03 PER-T-ED   PIC ---.---.--9,99.
       01 CAMPOS-AUXILIARES.
          02 CONT-P-AUX   PIC 999 VALUE 0.
          02 LIS          PIC X VALUE SPACES.
          02 IMP-T        PIC S9(9)V99 VALUE 0.
          02 PER-T        PIC S9(9)V99 VALUE 0.
          02 IMP-S        PIC S9(9)V99 VALUE 0.
          02 PER-S        PIC S9(9)V99 VALUE 0.
          02 LIB-CAP      PIC 9 VALUE 0.
          02 COD-IMP      PIC X VALUE SPACES.
          02 ANO-INT      PIC 99 VALUE 0.
          02 MES-INT      PIC 99 VALUE 0.
          02 DIA-INT      PIC 99 VALUE 0.
          02 FECHA-H-C.
             03 DIA-H-C   PIC 99 VALUE 0.
             03 MES-H-C   PIC 99 VALUE 0.
             03 ANO-H-C   PIC 99 VALUE 0.
          02 FECHA-H.
             03 ANO-H     PIC 99 VALUE 0.
             03 MES-H     PIC 99 VALUE 0.
             03 DIA-H     PIC 99 VALUE 0.
          02 FECHA-CAP.
             03 DIA-CAP   PIC 99 VALUE 0.
             03 MES-CAP   PIC 99 VALUE 0.
             03 ANO-CAP   PIC 99 VALUE 0. 
          02 D-IVA-EDI    PIC X(10).  
          02 IVA-H        PIC 9 VALUE 0.
          02 XX           PIC X.
       COPY "VARIOS.WOR".    
       COPY "CLAVES.WOR".
       COPY "ABREV.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
      *...............................(*.ERR)
       COPY "MANEJO.ERR".
       COPY "RES127.ERR".
       COPY "EMPRE.ERR".
       COPY "CUENTA.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
      *...............................(ABRE PANTALLA)
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
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
       CARGO-MOD.
      *...............................(SELECCIONA TAREAS)
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "S" TO COD-IMP.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F05 MOVE "S" TO COD-IMP
                  MOVE " LISTADO LIBRO " TO D20-CAP
                  GO TO LISTO.
           GO TO CARGO-MOD.
       
      *................................(PROGRAMA PRINCIPAL)
       LISTO.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           OPEN INPUT EMPRESAS RES127 MANEJO.
           OPEN OUTPUT IMP02.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD. 
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       CARGA-LIB.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           MOVE SPACES TO LIS.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP < 1 GO TO CARGA-LIB.
           IF NUM-CAP > 3 GO TO CARGA-LIB.
           MOVE NUM-CAP TO IVA-RES IVA-H.
           IF IVA-RES = 1 MOVE "-VENTAS-" TO D-IVA-EDI
              MOVE "LIBRO RES." TO TIT-3-EDI
              MOVE " 127/83 DG" TO TIT-4-EDI
              MOVE "R -       " TO TIT-5-EDI
              MOVE SPACES TO TIT-1-EDI TIT-2-EDI TIT-6-EDI.
           IF IVA-RES = 2 MOVE "-COMPRAS-" TO D-IVA-EDI
              MOVE "LIBRO RES." TO TIT-3-EDI
              MOVE " 127/83 DG" TO TIT-4-EDI
              MOVE "R -       " TO TIT-5-EDI
              MOVE SPACES TO TIT-1-EDI TIT-2-EDI TIT-6-EDI.
           IF IVA-RES = 3 MOVE "-GASTOS-" TO D-IVA-EDI
              MOVE "LIBRO RES." TO TIT-3-EDI
              MOVE " 127/83 DG" TO TIT-4-EDI
              MOVE "R -       " TO TIT-5-EDI
              MOVE SPACES TO TIT-1-EDI TIT-2-EDI TIT-6-EDI.
           MOVE 8 TO SW3.
           DISPLAY D-IVA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) REVERSE.
           MOVE TITULO-EDI TO TIT-E132.
       CARGA-F-D.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           MOVE 9 TO SW3. 
           PERFORM BORRA.
           ACCEPT FECHA-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIB.
           IF DIA-CAP = 0 OR DIA-CAP > 31 GO TO CARGA-F-D.
           IF MES-CAP = 0 OR MES-CAP > 12 GO TO CARGA-F-D.
           MOVE DIA-CAP TO DIA-RES.
           MOVE MES-CAP TO MES-RES MES-INT.
           MOVE ANO-CAP TO ANO-RES ANO-INT.
           MOVE 0 TO DIA-INT.
       CARGA-F-H.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-H-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-F-D.
           MOVE DIA-H-C TO DIA-H.
           MOVE MES-H-C TO MES-H.
           MOVE ANO-H-C TO ANO-H.
           IF DIA-H = 0 OR DIA-H > 31 GO TO CARGA-F-H.
           IF MES-H = 0 OR MES-H > 12 GO TO CARGA-F-H.
           IF ANO-H < ANO-RES GO TO CARGA-F-H.
           IF ANO-H = ANO-RES
               IF MES-H < MES-RES GO TO CARGA-F-H.
           IF ANO-H = ANO-RES
               IF MES-H = MES-RES
                  IF DIA-H < DIA-RES GO TO CARGA-F-H.
       SIGA-L.
           MOVE 0 TO DIA-INT CONT-P-AUX.
           MOVE 0 TO MOV-RES IMP-T PER-T IMP-S PER-S.
           MOVE SPACES TO I-BRU-RES DESC-RES.
           MOVE NUM-EMP TO EMP-RES.
           MOVE SUC-EMP TO SUC-RES.
           PERFORM TITU.         
           START RES127 KEY IS NOT < CLAVE-RES
                 INVALID KEY PERFORM CIERRE-A
                             GO TO INI-MOD.
       LISTO-1.
           IF CONT-R > 67 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES.
           READ RES127 NEXT AT END GO TO CIERRO.
           IF EMP-RES NOT = NUM-EMP GO TO CIERRO.
           IF SUC-RES NOT = SUC-EMP GO TO CIERRO.
           IF IVA-RES NOT = IVA-H
              GO TO CIERRO.
           IF ANO-RES < ANO-CAP GO TO LISTO-1.
           IF ANO-RES = ANO-CAP
             IF MES-RES < MES-CAP GO TO LISTO-1.
           IF ANO-RES = ANO-CAP
             IF MES-RES = MES-CAP
               IF DIA-RES < DIA-CAP GO TO LISTO-1.
           IF ANO-RES > ANO-H GO TO LISTO-1.
           IF ANO-RES = ANO-H
              IF MES-RES > MES-H GO TO LISTO-1.
           IF ANO-RES = ANO-H
              IF MES-RES = MES-H
                 IF DIA-RES > DIA-H GO TO LISTO-1.
           MOVE ANO-RES TO ANO-ED.
           MOVE MES-RES TO MES-ED
           MOVE DIA-RES TO DIA-ED.
           MOVE DESC-RES TO DESC-ED.
           MOVE I-BRU-RES TO I-BRU-ED.
           MOVE TIP-COM-RES TO TIP-COM-ED.
           MOVE SUB-T-C-RES TO SUB-T-C-ED.
           MOVE SUC-C-RES TO SUC-C-ED.
           MOVE COM-RES TO COM-ED.
           MOVE CAT-RES TO CAT-ED.
           MOVE IMP-RES TO IMP-ED.
           MOVE PER-RES TO PER-ED.
           MOVE POR-RES TO POR-ED.
           COMPUTE IMP-T = IMP-T + IMP-RES.
           COMPUTE IMP-S = IMP-S + IMP-RES.
           COMPUTE PER-T = PER-T + PER-RES.
           COMPUTE PER-S = PER-S + PER-RES.
           PERFORM IMPRE-LIN.
           GO TO LISTO-1.
       IMPRE-LIN.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           ADD 1 TO CONT-R.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           ACCEPT FECHA FROM DATE.
           MOVE ANO TO AA-E132.
           MOVE MES TO MM-E132.
           MOVE DIA TO DD-E132.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E132.
           MOVE MINU TO MI-E132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
            WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-4 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-5 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
            ADD 4 TO CONT-R.
           PERFORM ESPA.
           ADD 1 TO CONT-R.
           IF CONT-P-AUX > 0
              PERFORM TRANS.
           ADD 1 TO CONT-P-AUX.
       TRANS.
           MOVE PER-S TO PER-T-ED.
           MOVE IMP-S TO IMP-T-ED.
           MOVE 0 TO PER-S.
           MOVE 0 TO IMP-S.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 0.
           ADD 1 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       CIERRO.
           MOVE IMP-T TO IMP-T-ED.
           MOVE PER-T TO PER-T-ED.
           MOVE 0 TO PER-T PER-S IMP-T IMP-S.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           ADD 3 TO CONT-R.
              COMPUTE CONT-R = 72 - CONT-R
              PERFORM ESPA CONT-R TIMES.
       CIERRO-F.
           GO TO CARGA-LIB.
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
           MOVE NUM-CAP TO NUM-EMP EMP-CAP EMP-RES MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           READ MANEJO INVALID KEY DISPLAY " ".
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
           MOVE NUM-CAP TO SUC-EMP SUC-RES NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
       CARGA-EMP-F.
           EXIT.
       CIERRE-A.
           CLOSE IMP02 RES127 EMPRESAS.
      *................................(CARGA DATOS)

      *................................(DESPLEGA DATOS)

       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 101601 TO TAB-POS(7).
           MOVE 102010 TO TAB-POS(8).
           MOVE 142806 TO TAB-POS(9).
           MOVE 162806 TO TAB-POS(10).
           MOVE 182801 TO TAB-POS(11).
           MOVE 202806 TO TAB-POS(12).
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE " - ARCHIVO" TO TIT-1-PAN.
           MOVE " DE  RES. " TO TIT-2-PAN.
           MOVE " 127/83 - " TO TIT-3-PAN.
       PANTA-1.
           DISPLAY "EMPRESA  :           SUC:       -" LINE 7 POSITION 5.
           DISPLAY "LIBRO    : " LINE 10 POSITION 5.
           DISPLAY "FECHA DESDE <DDMMAA> : " LINE 14 POSITION 5.
           DISPLAY "FECHA HASTA <DDMMAA> : " LINE 16 POSITION 5.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO LIBRO  " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "ROT132.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
           COPY "CLAVES.PRO".
