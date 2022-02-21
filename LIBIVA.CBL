       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBIVA.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. DIC'91.
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
       COPY "IVA.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
      *...............................(*.REG)
       COPY "MANEJO.REG".
       COPY "IVA.REG".
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
             03 A02        PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X(7) VALUE "FECHA: ".
             03 DIA-ED     PIC 99/.
             03 MES-ED     PIC 99/.
             03 FILLER     PIC XX VALUE "19".
             03 ANO-ED     PIC 99.
             03 A03        PIC 99 COMP-4 VALUE 20.
          02 IMP-2.
             03 D-TIP-COM-ED   PIC X(4).
             03 SUB-T-C-ED     PIC X.
             03 FILLER         PIC X VALUE "-".
             03 SUC-C-ED       PIC 9(4).
             03 FILLER         PIC X VALUE "-".
             03 COM-ED         PIC 9(8)BB.
             03 DIA-VAL-ED     PIC 99/.
             03 MES-VAL-ED     PIC 99/.
             03 ANO-VAL-ED     PIC 99BB.
             03 NETO-ED        PIC ----.--9,99.
             03 NO-GRA-ED      PIC ----.--9,99.
             03 EXE-ED         PIC ----.--9,99.
             03 RET-ED         PIC ----.--9,99.
             03 IMP-1-ED       PIC ----.--9,99.
             03 IMP-2-ED       PIC ----.--9,99.
             03 PER-ED         PIC ----.--9,99.
             03 TOT-ED         PIC ----.--9,99.
             03 CRE-DEB-ED     PIC ----.--9,99.
          02 IMP-3.
             03 FILLER         PIC X(4) VALUE SPACES.
             03 DESCR-ED       PIC X(30)B.
             03 D-INS-ED       PIC X(10)B.
             03 CUIT-ED        PIC X(15).
          02 IMP-4.
             03 FILLER         PIC X(11) VALUE SPACES.
             03 D-TOT-ED       PIC X(20).
             03 NETO-T-ED      PIC ----.--9,99.
             03 NO-GRA-T-ED    PIC ----.--9,99.
             03 EXE-T-ED       PIC ----.--9,99.
             03 RET-T-ED       PIC ----.--9,99.
             03 IMP-1-T-ED     PIC ----.--9,99.
             03 IMP-2-T-ED     PIC ----.--9,99.
             03 PER-T-ED       PIC ----.--9,99.
             03 TOT-T-ED       PIC ----.--9,99.
             03 CRE-DEB-T-ED   PIC ----.--9,99.
          02 TIT-1.
             03 FILLER    PIC X(21) VALUE "----COMPROBANTE----".
             03 FILLER    PIC X(11) VALUE "FEC.VAL.".
             03 FILLER    PIC X(22) VALUE "NETO GRAV. -NO GRAV.-".
             03 FILLER    PIC X(22) VALUE "EXTO/NO IN -RETENIDO-".
             03 FILLER    PIC X(22) VALUE "IVA(Gral.) IVA no INS".
             03 FILLER    PIC X(22) VALUE " PERCEPC.  --TOTAL---".
             03 D-CRE-DEB-ED  PIC X(11).
          02 TIT-11.
             03 FILLER    PIC X(21) VALUE "----COMPROBANTE----".
             03 FILLER    PIC X(11) VALUE "FEC.VAL.".
             03 FILLER    PIC X(22) VALUE "NETO GRAV. -NO GRAV.-".
             03 FILLER    PIC X(22) VALUE "EXTO/NO IN -RETENIDO-".
             03 FILLER    PIC X(22) VALUE "IVA(Gral.) IVA no INS".
             03 FILLER    PIC X(22) VALUE " PERCEPC.  --TOTAL---".
             03 D-CRE-DEB-ED1  PIC X(11).
          02 TIT-2.
             03 FILLER    PIC X(4) VALUE SPACES. 
             03 FILLER    PIC X(15) VALUE "---------DESCRI".
             03 FILLER    PIC X(16) VALUE "PCION----------".
             03 FILLER    PIC X(11) VALUE " CATEGORIA".
             03 FILLER    PIC X(15) VALUE "---C.U.I.T.----".
       01 CAMPOS-AUXILIARES.
          02 CONT-P-AUX   PIC 999 VALUE 0.
          02 LIS          PIC X VALUE SPACES.
          02 NETO-T       PIC S9(6)V99 VALUE 0.
          02 NO-GRA-T     PIC S9(6)V99 VALUE 0.
          02 EXE-T        PIC S9(6)V99 VALUE 0.
          02 RET-T        PIC S9(6)V99 VALUE 0.
          02 IMP-1-T      PIC S9(6)V99 VALUE 0.
          02 IMP-2-T      PIC S9(6)V99 VALUE 0.
          02 TOT-T        PIC S9(6)V99 VALUE 0.
          02 PER-T        PIC S9(6)V99 VALUE 0.
          02 CRE-DEB-T    PIC S9(6)V99 VALUE 0.
          02 TOT-CAP      PIC S9(6)V99 VALUE 0.
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
       COPY "IVA.ERR".
       COPY "EMPRE.ERR".
       COPY "CUENTA.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
           OPEN INPUT MANEJO.
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
           IF F06 MOVE "R" TO COD-IMP
                  MOVE " LIST. RUBRICADO " TO D20-CAP
                  GO TO LISTO.
           GO TO CARGO-MOD.
       
      *................................(PROGRAMA PRINCIPAL)
       LISTO.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           IF COD-IMP = "S" 
            DISPLAY "LISTA SOLO TOTALES   : " LINE 18 POSITION 5
            DISPLAY "COMIENZA EN HOJA NRO.: " LINE 20 POSITION 5.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           OPEN INPUT EMPRESAS IVA.
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
           IF ESCAP PERFORM CIERRE-A 
                    GO TO INI-MOD.
           IF F02   PERFORM CIERRE-A
                    GO TO INI-MOD.
           IF NUM-CAP < 1 GO TO CARGA-LIB.
           IF NUM-CAP > 3 GO TO CARGA-LIB.
           MOVE NUM-CAP TO IVA-IVA IVA-H.
           IF IVA-IVA = 1 MOVE "-VENTAS-" TO D-IVA-EDI
              MOVE "LIBRO I.V." TO TIT-3-EDI
              MOVE "A. -VENTAS" TO TIT-4-EDI
              MOVE "-         " TO TIT-5-EDI
              MOVE "DEB.FISCAL" TO D-CRE-DEB-ED D-CRE-DEB-ED1.
              MOVE SPACES TO TIT-1-EDI TIT-2-EDI TIT-6-EDI.
           IF IVA-IVA = 2 MOVE "-COMPRAS-" TO D-IVA-EDI
              MOVE "LIBRO I.V." TO TIT-3-EDI
              MOVE "A. -COMPRA" TO TIT-4-EDI
              MOVE "S-        " TO TIT-5-EDI
              MOVE "CRED.FISCAL" TO D-CRE-DEB-ED D-CRE-DEB-ED1.
              MOVE SPACES TO TIT-1-EDI TIT-2-EDI TIT-6-EDI.
           IF IVA-IVA = 3 MOVE "-GASTOS-" TO D-IVA-EDI
              MOVE "LIBRO I.V." TO TIT-3-EDI
              MOVE "A. -GASTOS" TO TIT-4-EDI
              MOVE "-         " TO TIT-5-EDI
              MOVE "CRED.FISCAL" TO D-CRE-DEB-ED D-CRE-DEB-ED1.
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
           MOVE DIA-CAP TO DIA-IVA.
           MOVE MES-CAP TO MES-IVA MES-INT.
           MOVE ANO-CAP TO ANO-IVA ANO-INT.
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
           IF ANO-H < ANO-IVA GO TO CARGA-F-H.
           IF ANO-H = ANO-IVA
               IF MES-H < MES-IVA GO TO CARGA-F-H.
           IF ANO-H = ANO-IVA
               IF MES-H = MES-IVA
                  IF DIA-H < DIA-IVA GO TO CARGA-F-H.
           IF COD-IMP = "R" GO TO SIGA-L.
       CARGA-IMP.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE SPACES TO ALF-CAP.
           DISPLAY " <S> Si - <N> No " LINE 18 POSITION 30 BLINK REVERSE.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-F-H.
           MOVE ALF-CAP TO LIS.
           IF LIS = SPACES MOVE "S" TO LIS.
           IF LIS = "s" MOVE "S" TO LIS.
           IF LIS = "n" MOVE "N" TO LIS.
           IF LIS = "S" 
              DISPLAY " IMPRIME SOLO TOTALES " LINE 18 POSITION 30 
                   REVERSE
             ELSE DISPLAY " IMPRIME TODOS LOS DATOS " LINE 18 
                  POSITION 30 REVERSE.
       CARGA-PAG.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE 0 TO CONT-P.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-IMP.
           COMPUTE NUM-CAP = NUM-CAP - 1.
           MOVE NUM-CAP TO CONT-P.
       SIGA-L.
           MOVE 0 TO TOT-CAP TOT-T NETO-T NO-GRA-T EXE-T RET-T.
           MOVE 0 TO IMP-1-T IMP-2-T PER-T CRE-DEB-T CONT-R.
           MOVE 0 TO DIA-INT CONT-P-AUX.
           MOVE 0 TO MOV-IVA.
           MOVE NUM-EMP TO EMP-IVA.
           MOVE SUC-EMP TO SUC-IVA.
           PERFORM TITU.         
           START IVA KEY IS NOT < CLAVE-IVA
                 INVALID KEY PERFORM CIERRE-A
                             GO TO INI-MOD.
       LISTO-1.
           IF CONT-R > 63 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           READ IVA NEXT AT END GO TO CIERRO.
           IF EMP-IVA NOT = NUM-EMP GO TO CIERRO.
           IF SUC-IVA NOT = SUC-EMP GO TO CIERRO.
           IF IVA-IVA NOT = IVA-H
              GO TO CIERRO.
           IF ANO-IVA > ANO-H GO TO CIERRO.
           IF ANO-IVA = ANO-H
              IF MES-IVA > MES-H GO TO CIERRO.
           IF ANO-IVA = ANO-H
              IF MES-IVA = MES-H
                 IF DIA-IVA > DIA-H GO TO CIERRO.
           MOVE ANO-IVA TO ANO-ED.
           MOVE MES-IVA TO MES-ED
           MOVE DIA-IVA TO DIA-ED.
           IF ANO-IVA NOT = ANO-INT PERFORM CAMBIO-F THRU CAMBIO-F-F.
           IF MES-IVA NOT = MES-INT PERFORM CAMBIO-F THRU CAMBIO-F-F.
           IF DIA-IVA NOT = DIA-INT PERFORM CAMBIO-F-1 THRU CAMBIO-F-F.
           MOVE "CONS.FINAL" TO D-INS-ED.
           IF INS-IVA = "R"
                MOVE "RESP.INSC." TO D-INS-ED.
           IF INS-IVA = "N"
                MOVE "R.NO INSC." TO D-INS-ED.
           IF INS-IVA = "E"
                MOVE "EXTO/NO INS" TO D-INS-ED.
           IF INS-IVA = "C"
                MOVE "CONS.FINAL" TO D-INS-ED.
           MOVE DESCR-IVA TO DESCR-ED.
           MOVE CUIT-IVA TO CUIT-ED.
           MOVE ABREV(TIP-COM-IVA) TO D-TIP-COM-ED.
           MOVE SUB-T-C-IVA TO SUB-T-C-ED.
           MOVE SUC-C-IVA TO SUC-C-ED.
           MOVE COM-IVA TO COM-ED.
           MOVE NETO-IVA TO NETO-ED.
           MOVE NO-GRA-IVA TO NO-GRA-ED.
           MOVE EXE-IVA TO EXE-ED.
           MOVE RET-IVA TO RET-ED.
           MOVE IMP-1-IVA TO IMP-1-ED.
           MOVE IMP-2-IVA TO IMP-2-ED.
           MOVE CRE-DEB-IVA TO CRE-DEB-ED.
           MOVE PER-IVA TO PER-ED.
           MOVE DIA-VAL-IVA TO DIA-VAL-ED.
           MOVE MES-VAL-IVA TO MES-VAL-ED.
           MOVE ANO-VAL-IVA TO ANO-VAL-ED.
           COMPUTE TOT-CAP = NETO-IVA + NO-GRA-IVA + EXE-IVA + RET-IVA
                              + IMP-1-IVA + IMP-2-IVA + PER-IVA.
           MOVE TOT-CAP TO TOT-ED.
           COMPUTE TOT-T = TOT-T + TOT-CAP.
           COMPUTE NETO-T = NETO-T + NETO-IVA.
           COMPUTE NO-GRA-T = NO-GRA-T + NO-GRA-IVA.
           COMPUTE EXE-T = EXE-T + EXE-IVA.
           COMPUTE RET-T = RET-T + RET-IVA.
           COMPUTE IMP-1-T = IMP-1-T + IMP-1-IVA.
           COMPUTE IMP-2-T = IMP-2-T + IMP-2-IVA.
           COMPUTE PER-T = PER-T + PER-IVA.
           COMPUTE CRE-DEB-T = CRE-DEB-T + CRE-DEB-IVA.
           IF COD-IMP = "R" PERFORM IMPRE-LIN.
           IF COD-IMP NOT = "R"
              IF LIS = "N" PERFORM IMPRE-LIN. 
           GO TO LISTO-1.
       CAMBIO-F.
           PERFORM CIERRO.
           IF COD-IMP = "R" PERFORM TITU.
           IF COD-IMP NOT = "R" 
              IF LIS = "N" PERFORM TITU.
       CAMBIO-F-1.
           MOVE ANO-IVA TO ANO-INT.
           MOVE MES-IVA TO MES-INT.
           MOVE DIA-IVA TO DIA-INT.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 0.
           ADD 1 TO CONT-R.
       CAMBIO-F-F.
           EXIT.
       IMPRE-LIN.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           ADD 2 TO CONT-R.
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
           IF COD-IMP = "S"
            WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-4 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-5 AFTER 1
            WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
            ADD 4 TO CONT-R.
           IF COD-IMP = "R" PERFORM ESPA 4 TIMES.
           IF IVA-IVA NOT = 1
              WRITE LIN-IMP02 FROM TIT-11 AFTER 1
              ELSE WRITE LIN-IMP02 FROM TIT-1 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-2 AFTER 1.
           PERFORM ESPA.
           ADD 2 TO CONT-R.
           IF CONT-P-AUX > 0
              PERFORM TRANS.
           ADD 1 TO CONT-P-AUX.
       TRANS.
           MOVE NETO-T TO NETO-T-ED.
           MOVE NO-GRA-T TO NO-GRA-T-ED.
           MOVE EXE-T TO EXE-T-ED.
           MOVE RET-T TO RET-T-ED.
           MOVE IMP-1-T TO IMP-1-T-ED.
           MOVE IMP-2-T TO IMP-2-T-ED.
           MOVE PER-T TO PER-T-ED.
           MOVE TOT-T TO TOT-T-ED.
           MOVE CRE-DEB-T TO CRE-DEB-T-ED.
           MOVE "TRANSPORTE" TO D-TOT-ED.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
      **     WRITE LIN-IMP02 FROM IMP-4 AFTER 0.
           ADD 1 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       CIERRO.
           MOVE NETO-T TO NETO-T-ED.
           MOVE NO-GRA-T TO NO-GRA-T-ED.
           MOVE EXE-T TO EXE-T-ED. 
           MOVE RET-T TO RET-T-ED.
           MOVE IMP-1-T TO IMP-1-T-ED.
           MOVE IMP-2-T TO IMP-2-T-ED.
           MOVE PER-T TO PER-T-ED.
           MOVE TOT-T TO TOT-T-ED.
           MOVE CRE-DEB-T TO CRE-DEB-T-ED.
           MOVE "TOTALES" TO D-TOT-ED.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
      **     WRITE LIN-IMP02 FROM IMP-4 AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           ADD 3 TO CONT-R.
           MOVE 0 TO NETO-T NO-GRA-T EXE-T RET-T IMP-1-T IMP-2-T.
           MOVE 0 TO PER-T TOT-T CRE-DEB-T CONT-P-AUX.
           IF COD-IMP = "R"
              COMPUTE CONT-R = 72 - CONT-R
              PERFORM ESPA CONT-R TIMES.
           IF COD-IMP NOT = "R"
              IF LIS = "N" 
                COMPUTE CONT-R = 72 - CONT-R
                PERFORM ESPA CONT-R TIMES
              ELSE PERFORM ESPA.
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
           IF NUM-CAP = 0 GO TO CARGA-EMP 
           MOVE NUM-CAP TO NUM-EMP EMP-CAP EMP-IVA MA-EMP.
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
           MOVE NUM-CAP TO SUC-EMP SUC-IVA NUM-SUC-E132.
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
           CLOSE IMP02 IVA EMPRESAS.
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
           MOVE " DE  IVA -" TO TIT-2-PAN.
           MOVE "          " TO TIT-3-PAN.
       PANTA-1.
           DISPLAY "EMPRESA  :           SUC:       -" LINE 7 POSITION 5.
           DISPLAY "LIBRO    : " LINE 10 POSITION 5.
           DISPLAY "FECHA DESDE <DDMMAA> : " LINE 14 POSITION 5.
           DISPLAY "FECHA HASTA <DDMMAA> : " LINE 16 POSITION 5.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO LIBRO  " LINE 14 POSITION 18.
           DISPLAY "<F6>  LISTADO RUBRICADO" LINE 15 POSITION 18
           DISPLAY "<ESC> VUELVE AL MENU" LINE 16 POSITION 18.
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
