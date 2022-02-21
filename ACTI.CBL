       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTI.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. OCT'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ACTI.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ACTI.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 15.
          02 IMP-0.
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 FILLER       PIC X(30) VALUE SPACES.
             03 TIT-IMP      PIC X(20).
             03 FILLER       PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(4) VALUE SPACES.
             03 FILLER       PIC X(6) VALUE "ACTIV.".
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "--------- COMEN".
             03 FILLER       PIC X(15) VALUE "TARIO ---------".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "PORCENTAJE".
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "CALC.".
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(12) VALUE "-- VALOR 1 -".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(12) VALUE "-- VALOR 2 -".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(15) VALUE " ACTIVIDADES".
          02 IMP-3.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 ACT-ED       PIC ZZZ.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 DET-ED       PIC X(30)B(5).
             03 COM-ED       PIC X(30)B(5).
             03 POR-ED       PIC Z9,999B(5).
             03 CAL-ED       PIC 9B(5).
             03 VAL-1-ED     PIC ZZZZZ.ZZ9,99B(5).
             03 VAL-2-ED     PIC ZZZZZ.ZZ9,99.
       01 COD-OPE            PIC X.
       01 LISTO-I-D          PIC 9.
       01 LISTO-D            PIC 999.
       01 LISTO-H            PIC 999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IMP-INT            PIC 999.
       01 POR-INT            PIC 99V999.
       01 VAL-1-INT          PIC 9(12)V99 VALUE 0.
       01 VAL-2-INT          PIC 9(12)V99 VALUE 0.
       01 POR-EDI            PIC Z9,999.
       01 VAL-1-EDI          PIC ZZZZZ.ZZ9,99.
       01 VAL-2-EDI          PIC ZZZZZ.ZZ9,99.
       01 MEN-C-EDI          PIC X(30).
       01 IND-ORD            PIC 9.
       01 A                  PIC 99 VALUE 0.
       01 DONDE              PIC X.
       01 ANTE.
          03 ANT             PIC X OCCURS 30 TIMES.
       01 CAPTU.
          03 CAP             PIC X OCCURS 30 TIMES.
       01 CONTA-AUX.
          02 C-A-1          PIC 99 VALUE 0.
          02 C-A-2          PIC 99 VALUE 0.
          02 C-A-3          PIC 99 VALUE 0.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ACTI.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
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
           MOVE 28 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
           IF F05 GO TO LIST-NUM.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
           DISPLAY "IMPUESTO " LINE 7 POSITION 3.
           DISPLAY "/TASA/OT.:" LINE 8 POSITION 3.
           DISPLAY "                    " LINE 7 POSITION 40 REVERSE.
           DISPLAY "                    " LINE 8 POSITION 40 REVERSE.
           DISPLAY "                    " LINE 9 POSITION 40 REVERSE.
           DISPLAY "                    " LINE 10 POSITION 40 REVERSE.
           DISPLAY "ACTIVIDAD:" LINE 12 POSITION 3.
           DISPLAY "DETALLE  :" LINE 14 POSITION 3.
           DISPLAY "COMENT.  :" LINE 15 POSITION 3.
           DISPLAY "CALCULO  :" LINE 16 POSITION 3.
           DISPLAY "PORCENT. :" LINE 17 POSITION 3.
           DISPLAY "M.FIJO   :" LINE 18 POSITION 3.
           DISPLAY "MINIMO   :" LINE 19 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT ACTI.
           MOVE SPACES TO DET-ACT.
       CARGA-COD.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           DISPLAY " 2 - ING.BRUTOS-DGR " LINE 7 POSITION 40 REVERSE.
           DISPLAY " 3 - TASA MUNICIPAL " LINE 8 POSITION 40 REVERSE.
           DISPLAY " 8 - COMPRAS        " LINE 9 POSITION 40 REVERSE.
           DISPLAY " 9 - OTROS GASTOS   " LINE 10 POSITION 40 REVERSE. 
           PERFORM MENS-PAN.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP = 2 GO TO CARGA-COD-S.
           IF NUM-CAP = 3 GO TO CARGA-COD-S.
           IF NUM-CAP = 8 GO TO CARGA-COD-S.
           IF NUM-CAP = 9 GO TO CARGA-COD-S.
           GO TO CARGA-COD.
       CARGA-COD-S.
           MOVE NUM-CAP TO IMP-ACT.
           IF IMP-ACT = 2 
             DISPLAY " 2 - ING.BRUTOS-DGR " LINE 7 POSITION 40 
                      BLINK REVERSE.
           IF IMP-ACT = 3
             DISPLAY " 3 - TASA MUNICIPAL " LINE 8 POSITION 40
                      BLINK REVERSE.
           IF IMP-ACT = 8
             DISPLAY " 8 - COMPRAS        " LINE 9 POSITION 40
                      BLINK REVERSE.
           IF IMP-ACT = 9
             DISPLAY " 9 - OTROS GASTOS   " LINE 10 POSITION 40
                      BLINK REVERSE.
       CARGA-ACT.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           IF IMP-ACT NOT = 2
              DISPLAY SPACES LINE 20 POSITION 10 SIZE 50
              DISPLAY SPACES LINE 21 POSITION 10 SIZE 50
              DISPLAY SPACES LINE 22 POSITION 10 SIZE 50.
           IF IMP-ACT = 2
            DISPLAY " ACTIVIDADES: Minoristas 1 a 299, Ins.Direc- "
                LINE 20 POSITION 10 REVERSE
            DISPLAY "   tos 300, Mayoristas 301 a 599, Manufac. e "
                LINE 21 POSITION 10 REVERSE
            DISPLAY "   Indust. 601 a 899, Percep. > de 980       "
                LINE 22 POSITION 10 REVERSE.
           DISPLAY SPACES LINE 16 POSITION 30 SIZE 30.
           DISPLAY SPACES LINE 12 POSITION 28 SIZE 25.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO CARGA-COD.
           IF NUM-CAP = 0 GO TO CARGA-ACT.
           MOVE NUM-CAP TO ACT-ACT.
           IF IMP-ACT = 2 
              IF ACT-ACT > 899 AND ACT-ACT < 981
                    GO TO CARGA-ACT.
           IF IMP-ACT = 2
             IF ACT-ACT < 300
               DISPLAY "ALIC.CONSUMIDOR FINAL" LINE 12 POSITION 28
                    BLINK REVERSE.
           IF IMP-ACT = 2
             IF ACT-ACT = 300
               DISPLAY "ALIC.INSUMOS DIRECTOS" LINE 12 POSITION 28
                     BLINK REVERSE.
           IF IMP-ACT = 2
             IF ACT-ACT > 300 AND ACT-ACT < 600
               DISPLAY "ALIC.MAYORISTA" LINE 12 POSITION 28
                     BLINK REVERSE.
           IF IMP-ACT = 2
             IF ACT-ACT > 600 AND ACT-ACT < 900
               DISPLAY "ALIC.MANUF.e IND" LINE 12 POSITION 28
                     BLINK REVERSE.
           IF IMP-ACT = 2
             IF ACT-ACT > 980
               DISPLAY "ALIC. PERCEPCION" LINE 12 POSITION 28
                     BLINK REVERSE.
       LEE-ACT.
           MOVE DET-ACT TO ANTE.
           READ ACTI INVALID KEY MOVE "A" TO DONDE
                                   GO TO ALTA.
           PERFORM DESP-DES THRU DESP-VAL2.
           PERFORM MEN-B-M.
           MOVE "M" TO DONDE.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           MOVE DET-ACT TO ANTE.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F09 MOVE "M" TO COD-OPE
                  GO TO MODI.
           IF F10 MOVE "B" TO COD-OPE
                  GO TO BAJA.
           GO TO CARGA-ACT.
       ALTA.
           MOVE "A" TO COD-OPE.
           MOVE "<F2>ANTER.<F3>P" TO MEN-1.
           MOVE "ROX.<F10>REPITE" TO MEN-2.
           MOVE "               " TO MEN-3.
           PERFORM MENS-PAN.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F10 GO TO CARGA-UNO.
           
           IF F02 GO TO CARGA-ACT.
           IF F03 MOVE DET-ACT TO ALF-CAP.
           MOVE ALF-CAP TO DET-ACT.
       CARGA-COM.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO ALTA.
           IF F03 MOVE COM-ACT TO ALF-CAP.
           MOVE ALF-CAP TO COM-ACT.
           PERFORM DESP-COM.
           GO TO CARGA-CAL.
       CARGA-POR.
           PERFORM MEN-ANTE-DES.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           IF CAL-ACT > 1 MOVE 0 TO POR-ACT
                          PERFORM DESP-POR
                          GO TO CARGA-VAL1.
           ACCEPT POR-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3) NO BEEP TAB
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO CARGA-CAL.
           IF F03 MOVE POR-ACT TO POR-INT.
           MOVE POR-INT TO POR-ACT.
           PERFORM DESP-POR.
           GO TO CARGA-VAL1.
       CARGA-CAL.
           PERFORM MEN-ANTE-DES.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF NUM-CAP > 4 GO TO CARGA-CAL.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO CARGA-COM.
           IF F03 MOVE CAL-ACT TO NUM-CAP.
           MOVE NUM-CAP TO CAL-ACT.
           PERFORM DESP-CAL.
           GO TO CARGA-POR.
       CARGA-VAL1.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           IF CAL-ACT < 2 MOVE 0 TO VAL-1-ACT
                        PERFORM DESP-VAL1
                        GO TO CARGA-VAL2.
           ACCEPT VAL-1-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO CARGA-POR.
           IF F03 MOVE VAL-1-ACT TO VAL-1-INT.
           MOVE VAL-1-INT TO VAL-1-ACT.
           PERFORM DESP-VAL1.
       CARGA-VAL2.
           PERFORM MEN-ANTE-DES.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           IF CAL-ACT = 1 OR CAL-ACT = 2 OR CAL-ACT = 4
                      MOVE 0 TO VAL-2-ACT
                      PERFORM DESP-VAL2
                      GO TO CARGA-VAL2-S.
           ACCEPT VAL-2-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO CARGA-VAL1.
           IF F03 MOVE VAL-2-ACT TO VAL-2-INT.
           MOVE VAL-2-INT TO VAL-2-ACT.
           PERFORM DESP-VAL2.
       CARGA-VAL2-S.  
           CLOSE ACTI.
           OPEN I-O ACTI.
           WRITE REG-ACTI INVALID KEY REWRITE REG-ACTI.
           CLOSE ACTI.
           OPEN INPUT ACTI.
           GO TO CARGA-ACT.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 CLOSE ACTI
                  OPEN I-O ACTI
                  DELETE ACTI 
                  CLOSE ACTI
                  OPEN INPUT ACTI
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK ACTI RECORD
                  GO TO CARGA-ACT.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ACTI RECORD.
           GO TO CARGA-ACT.
       MODI.
           MOVE "<F2>ANTER.<F3>P" TO MEN-1.
           MOVE "ROX.<F4>MOD.MIN" TO MEN-2.
           MOVE "IMO<F10>REPITE " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW1 TO SW1.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F10 GO TO CARGA-UNO.
           IF F03 PERFORM DESP-DES
                  GO TO MODI1.
           IF F02 GO TO CARGA-ACT.
           IF F04 GO TO MODI5.
           MOVE ALF-CAP TO DET-ACT.
       MODI1.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 PERFORM DESP-COM 
                 GO TO MODI.
           IF F03 MOVE COM-ACT TO ALF-CAP.
           MOVE ALF-CAP TO COM-ACT.
           PERFORM DESP-COM.
       MODI2.
           PERFORM MEN-ANTE-DES.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 PERFORM DESP-CAL
                  GO TO MODI1.
           IF F03 MOVE CAL-ACT TO NUM-CAP.
           MOVE NUM-CAP TO CAL-ACT.
           PERFORM DESP-CAL.
       MODI3.
           PERFORM MEN-ANTE-DES.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           ACCEPT POR-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW1 TO SW1.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 PERFORM DESP-POR
                  GO TO MODI2.
           IF F03 MOVE POR-ACT TO POR-INT.
           MOVE POR-INT TO POR-ACT.
           PERFORM DESP-POR.
           IF CAL-ACT = 0 GO TO MODI5.
       MODI4.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-1-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 PERFORM DESP-VAL1
                   GO TO MODI2.
           IF F03 MOVE VAL-1-ACT TO VAL-1-INT.
           MOVE VAL-1-INT TO VAL-1-ACT.
           IF CAL-ACT = 4 GO TO MODI5.
           GO TO MODI6.
       MODI5.
           PERFORM MEN-ANTE-DES.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-2-INT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 PERFORM DESP-VAL1
                  GO TO MODI2.
           IF F03 MOVE VAL-2-ACT TO VAL-2-INT.
           MOVE VAL-2-INT TO VAL-2-ACT.
           PERFORM DESP-VAL2.
       MODI6.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF F01 CLOSE ACTI
                  OPEN I-O ACTI
                  REWRITE REG-ACTI 
                  CLOSE ACTI
                  OPEN INPUT ACTI
              ELSE PERFORM MEN-NO-MODI.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ACTI RECORD.
           GO TO CARGA-ACT.
       CARGA-UNO.
           MOVE "<F2>IZQ.<F3>DER" TO MEN-1.
           MOVE ".<F4>INS.<F10>B" TO MEN-2.
           MOVE "ORRA.<F1>CONF. " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE SPACES TO CAPTU.
           MOVE ANTE TO DET-ACT.
           PERFORM DESP-DES.
           MOVE ANT(1) TO CAP(1).
           MOVE TAB-COL(SW3) TO A. 
           MOVE 1 TO SW2.
       SIGA-UNO.
           ADD 1 TO SW2.
           ADD 1 TO A.
           IF SW2 > 30 MOVE CAPTU TO DET-ACT
                       GO TO CARGA-UNO-F.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION A 
                       SIZE 1 NO BEEP 
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 MOVE CAPTU TO DET-ACT
                  GO TO CARGA-UNO-F.
           IF F02 COMPUTE SW2 = SW2 - 2
                  COMPUTE A = A - 2 
                  GO TO SIGA-UNO.
           IF F03 MOVE ANT(SW2) TO CAP(SW2)
                  GO TO SIGA-UNO.
           IF F04 GO TO CORRA.
           IF F10 GO TO SACA.
           MOVE ALF-CAP TO CAP(SW2).
           DISPLAY CAP(SW2) LINE TAB-LIN(SW3) POSITION A SIZE 1.
           GO TO SIGA-UNO.
       CARGA-UNO-F.
           IF COD-OPE = "A" GO TO CARGA-POR.
           IF COD-OPE = "M" GO TO MODI1.
           GO TO CARGA-ACT.
       CORRA.
           COMPUTE C-A-1 = TAB-COL(SW3) + TAB-SIZ(SW3) - 1.
           MOVE 30 TO C-A-3.
           MOVE 29 TO C-A-2.
       CORRA-1.
           IF C-A-2 < SW2 GO TO CORRA-2.
           MOVE ANT(C-A-2) TO CAP(C-A-3).
           MOVE ANT(C-A-2) TO ANT(C-A-3).
           DISPLAY CAP(C-A-3) LINE TAB-LIN(SW3) POSITION C-A-1
                            SIZE 1.
           COMPUTE C-A-2 = C-A-2 - 1.
           COMPUTE C-A-3 = C-A-3 - 1.
           COMPUTE C-A-1 = C-A-1 - 1.
           GO TO CORRA-1.  
        CORRA-2.
           DISPLAY SPACES LINE TAB-LIN(SW3) POSITION A SIZE 1.
           ACCEPT CAP(SW2) LINE TAB-LIN(SW3) POSITION A SIZE 1
                    NO BEEP.
           MOVE CAP(SW2) TO ANT(SW2).
        CORRA-F.
           GO TO SIGA-UNO.
        SACA.
           MOVE SW2 TO C-A-2.
           MOVE A TO C-A-1.
           COMPUTE C-A-3 = SW2 + 1.
        SACA-1.
           IF C-A-3 > TAB-SIZ(SW3) GO TO SACA-2.
           MOVE ANT(C-A-3) TO CAP(C-A-2).
           MOVE ANT(C-A-3) TO ANT(C-A-2).
           DISPLAY CAP(C-A-2) LINE TAB-LIN(SW3) POSITION C-A-1 SIZE 1.
           ADD 1 TO C-A-2.
           ADD 1 TO C-A-3.
           ADD 1 TO C-A-1.
           GO TO SACA-1.
        SACA-2.
           MOVE SPACES TO CAP(C-A-2) ANT(C-A-2).
           DISPLAY CAP(C-A-2) LINE TAB-LIN(SW3) POSITION C-A-1 SIZE 1.
           COMPUTE A = A - 1.
           COMPUTE SW2 = SW2 - 1.
           GO TO SIGA-UNO.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "IMPUESTO/TASA :" LINE 7 POSITION 3.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "CODIGO" LINE 9 POSITION 10.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 17.
           DISPLAY "PORC." LINE 9 POSITION 50.
           DISPLAY "CALC." LINE 9 POSITION 60.
           OPEN INPUT ACTI.
       C-N-COD.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP < 2 OR NUM-CAP > 3 GO TO C-N-COD.
           MOVE NUM-CAP TO IMP-ACT IMP-INT.
           IF IMP-ACT = 2 
             MOVE 9 TO SW3
             PERFORM BORRA
             DISPLAY " ING.BRUTOS-DGR " LINE TAB-LIN(SW3) 
                POSITION TAB-COL(SW3) BLINK REVERSE.
           IF IMP-ACT = 3
             MOVE 9 TO SW3    
             PERFORM BORRA
             DISPLAY " TASA MUNICIPAL " LINE TAB-LIN(SW3) 
                POSITION TAB-COL(SW3) BLINK REVERSE.
       C-N-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           ACCEPT ACT-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F01 CLOSE ACTI GO TO INI-MOD.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           PERFORM BORRA-PANTA-C.
       C-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP  CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
       C-N-POSI.
           START ACTI KEY IS NOT < CLAVE-ACTI
                         INVALID KEY GO TO C-N-DESDE.
           MOVE 0 TO SW2.
           MOVE 10 TO SW1.
           MOVE 4 TO SW3.
           MOVE "N" TO COD2.
           GO TO C-LEO.
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "IMPUESTO/TASA :" LINE 7 POSITION 3.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "CODIGO" LINE 9 POSITION 10.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 17.
           DISPLAY "PORC." LINE 9 POSITION 50.
           DISPLAY "CALC." LINE 9 POSITION 60.
           OPEN INPUT ACTI.
       C-A-COD.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP < 2 OR NUM-CAP > 3 GO TO C-A-COD.
           MOVE NUM-CAP TO IMP-ACT IMP-INT.
           IF NUM-CAP = 2 
             MOVE 9 TO SW3
             PERFORM BORRA
             DISPLAY " ING.BRUTOS-DGR " LINE TAB-LIN(SW3) 
                POSITION TAB-COL(SW3) BLINK REVERSE.
           IF NUM-CAP = 3
             MOVE 9 TO SW3    
             PERFORM BORRA
             DISPLAY " TASA MUNICIPAL " LINE TAB-LIN(SW3) 
                POSITION TAB-COL(SW3) BLINK REVERSE.
       C-A-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT DET-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 CLOSE ACTI GO TO INI-MOD.
       C-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO C-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       C-A-POSI.
           START ACTI KEY IS NOT < DET-ACT
                        INVALID KEY GO TO C-A-DESDE.
           MOVE 0 TO SW2.
           MOVE 10 TO SW1.
           MOVE 4 TO SW3.
           MOVE "A" TO COD2.
           GO TO C-LEO.
       C-LEO.
           ADD 1 TO SW2.
           ADD 1 TO SW1.
           IF SW2 < 11 GO TO C-SIGOLEO. 
           PERFORM MEN-CONS.
           MOVE 1 TO SW2.
           MOVE 10 TO SW1.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F03 GO TO C-SIGOLEO.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 GO TO F-CONS.
           CLOSE ACTI GO TO INI-MOD.
       C-SIGOLEO.
           IF SW2 = 1 PERFORM BORRA-PANTA-C.
           MOVE "X" TO COD3.
           PERFORM LEO-MAR.
           IF COD3 = "F" 
                  IF COD2 = "A" GO TO C-A-DESDE 
                  ELSE GO TO C-N-DESDE.
           IF IMP-INT NOT = IMP-ACT
              IF COD2 = "A" GO TO C-SIGOLEO.
           IF IMP-INT NOT = IMP-ACT
              IF COD2 = "N" GO TO C-N-DESDE. 
           IF COD2 = "A" IF DET-ACT > LISTO-H-A 
                                       GO TO C-A-DESDE.
           IF COD2 = "N" IF ACT-ACT > LISTO-H 
                                       GO TO C-N-DESDE.
           IF ACT-ACT = SPACES PERFORM LEO-MAR.
           DISPLAY ACT-ACT LINE SW1 POSITION 10.
           DISPLAY DET-ACT LINE SW1 POSITION 17.
           MOVE POR-ACT TO POR-EDI.
           DISPLAY POR-EDI LINE SW1 POSITION 50.
           DISPLAY CAL-ACT LINE SW1 POSITION 62.   
           GO TO C-LEO.
       F-CONS.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LIST-NUM.
           OPEN INPUT ACTI.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "IMPUESTO\TASA :" LINE 7 POSITION 3.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
       L-N-COD.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP < 2 OR NUM-CAP > 3 GO TO L-N-COD.
           MOVE NUM-CAP TO IMP-ACT IMP-INT.
           MOVE 9 TO SW3.
           IF IMP-ACT = 2
              MOVE "2 : ING.BRUTOS-DGR" TO TIT-IMP 
              DISPLAY " ING.BRUTOS-DGR " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.
           IF IMP-ACT = 3
              MOVE "3 : TASA MUNICIPAL" TO TIT-IMP
              DISPLAY " TASA MUNICIPAL " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.   
       L-N-DESDE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO L-N-COD.
           MOVE NUM-CAP TO LISTO-D ACT-ACT.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           MOVE "N" TO COD2.
       L-N-POSI.
           OPEN OUTPUT IMP02.
           START ACTI KEY IS NOT < CLAVE-ACTI
                        INVALID KEY CLOSE IMP02
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "ACTIVIDADE" TO TIT-4-EDI.
           MOVE "S-IMPUESTO " TO TIT-5-EDI.
           MOVE "S >>>      " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "IMPUESTO\TASA:" LINE 7 POSITION 3.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           OPEN INPUT ACTI.
       L-A-COD.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP < 2 OR NUM-CAP > 3 GO TO L-A-COD.
           MOVE NUM-CAP TO IMP-ACT IMP-INT.
           MOVE 9 TO SW3.
           IF IMP-ACT = 2 
              DISPLAY " ING.BRUTOS-DGR " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.
           IF IMP-ACT = 3
              DISPLAY " TASA MUNICIPAL " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE. 
       L-A-DESDE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F01 CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO L-A-COD.
           MOVE ALF-CAP TO LISTO-D-A DET-ACT.
       L-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF F02 GO TO L-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       L-A-POSI.
           OPEN OUTPUT IMP02.
           START ACTI KEY IS NOT < DET-ACT
                        INVALID KEY CLOSE IMP02
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-I CONT-P CONT-R.
           MOVE "     <<< L" TO TIT-1-EDI.
           MOVE "ISTADO ALF" TO TIT-2-EDI.
           MOVE "ABETICO DE" TO TIT-3-EDI.
           MOVE " ACTIVIDAD" TO TIT-4-EDI.
           MOVE "ES-IMPUEST" TO TIT-5-EDI.
           MOVE "OS >>>    " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.                         
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-MAR.
           IF IMP-INT NOT = IMP-ACT GO TO L-LEO.
           IF COD2 = "N" AND ACT-ACT > LISTO-H MOVE "F" TO COD3.
           IF COD2 = "A" AND DET-ACT > LISTO-H-A MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP02 
                         IF COD2 = "N" GO TO L-N-DESDE
                         ELSE GO TO L-A-DESDE.
           MOVE ACT-ACT TO ACT-ED.
           MOVE DET-ACT TO DET-ED.
           MOVE COM-ACT TO COM-ED.
           MOVE POR-ACT TO POR-ED.
           MOVE CAL-ACT TO CAL-ED.
           MOVE VAL-1-ACT TO VAL-1-ED.
           MOVE VAL-2-ACT TO VAL-2-ED.
           WRITE LIN-IMP02 FROM IMP-3 AFTER ADVANCING 1.
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
           WRITE LIN-IMP02 FROM NORMAL AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-0 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 9 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       LEO-MAR.
           READ ACTI NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 082301 TO TAB-POS(3).
           MOVE 122303 TO TAB-POS(4).
           MOVE 142330 TO TAB-POS(5).
           MOVE 172306 TO TAB-POS(6).
           MOVE 162301 TO TAB-POS(7).
           MOVE 071901 TO TAB-POS(8).
           MOVE 072230 TO TAB-POS(9).
           MOVE 081003 TO TAB-POS(10).
           MOVE 084703 TO TAB-POS(11).
           MOVE 081010 TO TAB-POS(12)
           MOVE 084710 TO TAB-POS(13).
           MOVE 192306 TO TAB-POS(14).
           MOVE 255801 TO TAB-POS(15)
           MOVE 121406 TO TAB-POS(16).
           MOVE 171430 TO TAB-POS(17).
           MOVE 152330 TO TAB-POS(18).
           MOVE 182312 TO TAB-POS(19).
           MOVE 192312 TO TAB-POS(20).
           MOVE 133330 TO TAB-POS(21).
           MOVE 143330 TO TAB-POS(22).
           MOVE 153330 TO TAB-POS(23).
           MOVE 163330 TO TAB-POS(24).
           MOVE 173330 TO TAB-POS(25).
           MOVE 183330 TO TAB-POS(26).
           MOVE 193330 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071404 TO TAB-POS(29).
           MOVE 072602 TO TAB-POS(30).
           MOVE 073230 TO TAB-POS(31).
           MOVE 081006 TO TAB-POS(34).
           MOVE 084706 TO TAB-POS(35).
           MOVE 191430 TO TAB-POS(36).
       TIT-PAN.
           MOVE "-ARCHIVO D" TO TIT-1-PAN.
           MOVE "E ACTIVIDA" TO TIT-2-PAN.
           MOVE "DES-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 16 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO" LINE 17 POSITION 18.
           DISPLAY "<F6>  LISTADO ALFABET.  " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
       DESP-ACT.
           MOVE 4 TO SW3.
           DISPLAY ACT-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-DES.
           MOVE 5 TO SW3.
           MOVE DET-ACT TO ALF-CAP.
           DISPLAY ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-COM.
           MOVE 18 TO SW3.
           MOVE COM-ACT TO ALF-CAP.
           DISPLAY ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-POR.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE POR-ACT TO POR-EDI.
           DISPLAY POR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
       DESP-CAL.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           DISPLAY CAL-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
           IF CAL-ACT = 0 
               MOVE "PORC. C/MINIMO P/PERIODO" TO MEN-C-EDI.
           IF CAL-ACT = 1
               MOVE "PORC. SIN MINIMO" TO MEN-C-EDI.
           IF CAL-ACT = 2
               MOVE "M.FIJO P/PERIODO" TO MEN-C-EDI.
           IF CAL-ACT = 3
               MOVE "M.FIJO P/TRANS. C/MINIMO" TO MEN-C-EDI.
           IF CAL-ACT = 4
               MOVE "M.FIJO P/TRANS. S/MINIMO" TO MEN-C-EDI.
           DISPLAY MEN-C-EDI LINE 16 POSITION 30 SIZE 30.
       DESP-VAL1.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE VAL-1-ACT TO VAL-1-EDI.
           DISPLAY VAL-1-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
       DESP-VAL2.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE VAL-2-ACT TO VAL-2-EDI.
           DISPLAY VAL-2-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 20.
           MOVE 11 TO SW1.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".


