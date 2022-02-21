       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTI.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. JUL'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "PARTI.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "PARTI.REG".
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA           PIC 99 COMP-4 VALUE 15.
          02 IMP-10.
             03 FILLER       PIC X(8) VALUE "MARCA : ".
             03 BB           PIC 99 COMP-4 VALUE 18.
             03 CC           PIC 99 COMP-4 VALUE 14.
             03 MARCA-ED     PIC X(20).
             03 DD           PIC 99 COMP-4 VALUE 20.
             03 EE           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(6) VALUE "MARCA ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(9) VALUE "PARTIDA  ".
             03 FILLER       PIC X(17) VALUE " --PRECIO VENTA--".
             03 FILLER       PIC X(12) VALUE " FECHA PREC ".
             03 FILLER       PIC X(18) VALUE "--PRECIO COSTO--".
             03 FILLER       PIC X(16) VALUE "---IMP.INTERNO--".
          02 IMP-11.Š             03 FILLER       PIC X(13) VALUE SPACES.
             03 FILLER       PIC X(13) VALUE "SALDO STOCK  ".
             03 FILLER       PIC X(13) VALUE "PEND.ENTREG. ".
             03 FILLER       PIC X(13) VALUE "KILOS ORIGEN ".
             03 FILLER       PIC X(13) VALUE "-KILOS BAJA- ".
             03 FILLER       PIC X(13) VALUE "DIFER. KILOS ". 
             03 FILLER       PIC X(12) VALUE " --POSICION-".       
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999BB.
             03 DET-ED       PIC X(30)B.
             03 PAR-ED       PIC Z(8)B.
             03 PRE-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE SPACES.
             03 FEC-ED       PIC 99/99/99B.
             03 PRE-C-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 II-ED        PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
          02 IMP-31.
             03 FILLER       PIC X(13) VALUE SPACES.
             03 SAL-S-ED     PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC X(2) VALUE SPACES.
             03 ENT-S-ED     PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 KILO-S-ED    PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 KILO-B-ED    PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 DIF-K-ED     PIC ---.--9,999BB.
             03 SEC-1-ED     PIC XXX.
             03 FILLER       PIC X VALUE "-".
             03 SEC-2-ED     PIC XXX.
             03 FILLER       PIC X VALUE "-".
             03 SEC-3-ED     PIC XXX.
          02 IMP-900.
             03 BBB          PIC 99 COMP-4 VALUE 14.      
          02 IMP-90.
             03 FILLER       PIC X(36) VALUE ALL "-".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(36) VALUE ALL "-".
          02 IMP-91.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 ED-NUM-1     PIC ZZZZZ9B.
             03 FILLER       PIC XX VALUE "-".
             03 ED-MAR-1     PIC 9999.
             03 FILLER       PIC X(28) VALUE SPACES.
             03 ED-NUM-2     PIC ZZZZZ9B.
             03 FILLER       PIC XX VALUE "-".
             03 ED-MAR-2     PIC 9999.
          02 IMP-92.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 ED-DET-1     PIC X(30)B(11).
             03 ED-DET-2     PIC X(30).
          02 IMP-93.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(7) VALUE "FECHA: ".
             03 CCC          PIC 99 COMP-4 VALUE 18.
             03 ED-DIA-1     PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ED-MES-1     PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ED-ANO-1     PIC 99.
             03 DDD          PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(12) VALUE SPACES.
             03 FILLER       PIC X(7) VALUE "FECHA: ".
             03 EEE          PIC 99 COMP-4 VALUE 18.
             03 ED-DIA-2     PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ED-MES-2     PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ED-ANO-2     PIC 99.
             03 FFF          PIC 99 COMP-4 VALUE 20. 
          02 IMP-94.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(9) VALUE "PARTIDA: ".
             03 ED-PAR-1     PIC 9(8).
             03 FILLER       PIC X(26) VALUE SPACES.
             03 FILLER       PIC X(9) VALUE "PARTIDA: ".
             03 ED-PAR-2     PIC 9(8).
          02 IMP-95.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "CANTIDAD: ".
             03 ED-CAN-1     PIC ---.--9,999.
             03 FILLER       PIC X(22) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "CANTIDAD: ".
             03 ED-CAN-2     PIC ---.--9,999.
          02 IMP-96.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "BANDEJAS: ".
             03 ED-BAN-1     PIC Z9B.
             03 FILLER       PIC XXX VALUE "de".
             03 ED-TOT-1     PIC Z9. 
             03 FILLER       PIC X(25) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "BANDEJAS: ".
             03 ED-BAN-2     PIC Z9B.
             03 FILLER       PIC XXX VALUE "de".
             03 ED-TOT-2     PIC Z9.               
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 SAL-S              PIC S9(6)V999 VALUE 0.
       01 SAL-E              PIC S9(6)V999 VALUE 0.
       01 DIF-K              PIC S9(6)V999 VALUE 0.
       01 SAL-B              PIC S9(6)V999 VALUE 0.
       01 KILO-S             PIC S9(6)V999 VALUE 0.
       01 KILO-B             PIC S9(6)V999 VALUE 0.
       01 STOCK              PIC S9(6)V999 VALUE 0.
       01 UNI-V              PIC 9(6)V999.
       01 IND-ORD            PIC 9.
       01 MARCA-L            PIC 9(3).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 SAL-S-EDI          PIC -----9,999.Š       01 SAL-M-EDI          PIC ZZZZZZ.
       01 UNI-V-EDI          PIC ZZZZZ9,999.
       01 SAL-E-EDI          PIC -----9,999.
       01 KILO-S-EDI         PIC -----9,999.
       01 KILO-B-EDI         PIC -----9,999.
       01 C-STOCK            PIC X VALUE SPACES. 
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 DATOS.
          02 DATO     PIC X(71) OCCURS 10 TIMES.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART02.ERR".
       COPY "ART01.ERR".
       COPY "PARTI.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "MANEJO.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO CLAVE-1-CAP.
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
       CARGO-MOD.
            MOVE 3 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.Š       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F02 MOVE "S" TO C-STOCK
                  GO TO CONS-NUM.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN.
       PANTA-ACTU.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "ARTICULO        :" LINE 10 POSITION  3.
           DISPLAY "NRO. PARTIDA    :" LINE 12 POSITION  3.
           DISPLAY "PRECIO VENTA    :" LINE 13 POSITION  3.
           DISPLAY "FECHA PARTIDA   :" LINE 14 POSITION  3.
           DISPLAY "PRECIO COSTO    :" LINE 15 POSITION  3.
           DISPLAY "IMPUESTO INTERNO:" LINE 16 POSITION  3.
           DISPLAY "SALDO STOCK     :" LINE 17 POSITION  3.
           DISPLAY "PEND.DE ENTREGA :" LINE 18 POSITION  3.
           DISPLAY "KILOS STOCK ORI.:" LINE 19 POSITION  3.
           DISPLAY "KILOS BAJA STOCK:" LINE 20 POSITION  3.
           DISPLAY "POSICION        :" LINE 21 POSITION  3.
           DISPLAY "-   -" LINE 21 POSITION 24.
           DISPLAY "CANT.DE BANDEJAS:" LINE 22 POSITION  3.
           DISPLAY " DIGITE: 0 NO IMPRIME " LINE 22 POSITION 30 
                           REVERSE.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       SIGA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.

           OPEN INPUT ART02
                INPUT ART01
                INPUT MARCAS
                INPUT NUMERO
                INPUT PARTI.
       CARGA-ART.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           PERFORM MEN-ANTE.
           PERFORM BORRA-C.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM LEA-EMPRE THRU F-LEA-EMP.
           IF NUM-CAP = 0 GO TO CARGA-ART.
           MOVE NUM-CAP TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02 INVALID KEY
                       GO TO CARGA-ART.
           READ ART02 NEXT AT END PERFORM E-ART02 100 TIMES
                                 GO TO CARGA-ART.
           IF NUM-EMP NOT = EMP-ART02 GO TO CARGA-ART.
           IF SUC-EMP NOT = SUC-ART02 GO TO CARGA-ART.
           IF NUM-ART02 NOT = NUM-CAP GO TO CARGA-ART.
           MOVE NUM-ART02 TO COD-NUM.
      *     IF ACT-PRI-EMP = 99999
      *            MOVE NUM-ART02 TO N-ART02
      *            COMPUTE COD-MAR = N-1-ART02 * 1
      *       ELSE MOVE MAR-ART02 TO COD-MAR.
           MOVE MAR-ART02 TO COD-MAR.
           READ NUMERO WITH NO LOCK INVALID KEY GO TO CARGA-ART.
           PERFORM DES-DET.
           READ MARCAS WITH NO LOCK INVALID KEY GO TO CARGA-ART.
           PERFORM DES-MAR.
           GO TO LEE-ART.
       LEE-ART.
           MOVE EMP-ART02 TO EMP-PARTI EMP-ART01.
           MOVE SUC-ART02 TO SUC-PARTI.
           MOVE COD-ART02 TO COD-PARTI COD-ART01.
           READ ART01 INVALID KEY PERFORM E-ART01 100 TIMES
                                  GO TO CARGA-ART.
           IF REGI-ART01 NOT = "P" GO TO CARGA-ART.
       CARGA-PAR.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 GO TO CARGA-ART.
           MOVE NUM-CAP TO PAR-PARTI.
           IF PAR-PARTI = 0 GO TO POSI-PAR.
           READ PARTI INVALID KEY GO TO ALTA.
           GO TO MIRO-PAR.
       POSI-PAR.
           START PARTI KEY IS NOT < CLAVE-PARTI
                   INVALID KEY GO TO CARGA-PAR.
       SIGO-PAR.
           READ PARTI NEXT AT END GO TO CARGA-PAR.
           IF EMP-PARTI NOT = EMP-ART02 GO TO ALTA.
           IF SUC-PARTI NOT = SUC-ART02 GO TO ALTA.
           IF COD-PARTI NOT = COD-ART02 GO TO ALTA.
       MIRO-PAR.         
           PERFORM DES-PRE THRU DES-POS.
           PERFORM MEN-B-M.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-ART.
       ALTA.
           MOVE EMP-ART02 TO EMP-PARTI.
           MOVE SUC-ART02 TO SUC-PARTI.
           MOVE COD-ART02 TO COD-PARTI.
           MOVE NUM-CAP TO PAR-PARTI.
           IF PAR-PARTI = 0 GO TO CARGA-PAR.
           MOVE 0 TO BAJ-S-PARTI.
           GO TO CARGA-PRE.
       CARGA-PRE.
           PERFORM MEN-ANTE-DES.
           MOVE 13 TO SW3.
           PERFORM CAP-DEC10.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 IF COD2 = "M" GO TO CARGA-PRE
                  ELSE GO TO CARGA-ART.
           IF F03 PERFORM DES-PRE GO TO CARGA-F-P.
           MOVE DEC10-CAP TO PRE-V-PARTI.
           PERFORM DES-PRE.
       CARGA-F-P.
           PERFORM MEN-ANTE-DES.
           MOVE 14 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-F-V GO TO CARGA-PRE.
           IF F03 PERFORM DES-F-V GO TO CARGA-COS.
           MOVE NUM-CAP TO FEC-PARTI.
           PERFORM DES-F-V.
       CARGA-COS.
           PERFORM MEN-ANTE-DES.
           MOVE 15 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-COS GO TO CARGA-F-P.
           IF F03 PERFORM DES-COS GO TO CARGA-II.
           MOVE DEC10-CAP TO PRE-C-PARTI.
           PERFORM DES-COS.
       CARGA-II.
           PERFORM MEN-ANTE-DES.
           MOVE 16 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-II GO TO CARGA-COS.
           IF F03 PERFORM DES-II GO TO CARGA-SS.
           MOVE DEC10-CAP TO II-PARTI.
           PERFORM DES-II.
       CARGA-SS.
           PERFORM MEN-ANTE-DES.
           MOVE 17 TO SW3.
           ACCEPT SAL-S LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-SS GO TO CARGA-II.
           IF F03 PERFORM DES-SS GO TO CARGA-ES.
           COMPUTE SAL-S-PARTI = SAL-S + BAJ-S-PARTI.
           PERFORM DES-SS.
       CARGA-ES.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           ACCEPT SAL-E LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-ES GO TO CARGA-SS.
           IF F03 PERFORM DES-ES GO TO CARGA-KS.
           MOVE SAL-E TO ENT-S-PARTI.
           PERFORM DES-ES.
       CARGA-KS.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           ACCEPT KILO-S LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-KS GO TO CARGA-ES.
           IF F03 PERFORM DES-KS GO TO CARGA-KB.
           MOVE KILO-S TO KILO-S-PARTI.
           PERFORM DES-KS.
       CARGA-KB.
           PERFORM MEN-ANTE-DES.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT KILO-B LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-KB GO TO CARGA-KS.
           IF F03 PERFORM DES-KB GO TO CARGA-P1.
           MOVE KILO-B TO KILO-B-PARTI.
           PERFORM DES-KB.           
       CARGA-P1.
           PERFORM MEN-ANTE-DES.
           MOVE 21 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-P1 GO TO CARGA-KB.
           IF F03 PERFORM DES-P1 GO TO CARGA-P2.
           MOVE ALF-CAP TO SEC-1-PARTI.
           PERFORM DES-P1.
       CARGA-P2.
           PERFORM MEN-ANTE-DES.
           MOVE 22 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-P2 GO TO CARGA-P1.
           IF F03 PERFORM DES-P2 GO TO CARGA-P3.
           MOVE ALF-CAP TO SEC-2-PARTI.
           PERFORM DES-P2.
       CARGA-P3.
           PERFORM MEN-ANTE-DES.
           MOVE 23 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-POS GO TO CARGA-P2.
           IF F03 PERFORM DES-POS GO TO GRAVO.
           MOVE ALF-CAP TO SEC-3-PARTI.
           PERFORM DES-POS.
       GRAVO.
           CLOSE PARTI.
           OPEN I-O PARTI.

           WRITE REG-PARTI INVALID KEY REWRITE REG-PARTI.
           CLOSE PARTI.
           OPEN INPUT PARTI.
           IF COD2 = "M" MOVE "X" TO COD2.
           GO TO CARGA-ART.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           IF F01 CLOSE PARTI 
                  OPEN I-O PARTI
                  DELETE PARTI 
                  CLOSE PARTI
                  OPEN INPUT PARTI
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK PARTI RECORD
                  GO TO CARGA-ART.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK PARTI RECORD.
           GO TO CARGA-ART.
       MODI.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           UNLOCK PARTI RECORD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           IF F01 MOVE "M" TO COD2 GO TO CARGA-PRE.
           GO TO CARGA-ART.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       C-N-DESDE.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01 EMP-PARTI.
           MOVE SUC-EMP TO SUC-ART02 SUC-PARTI.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 11 TO SW3.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
       C-N-POSI.
           OPEN INPUT ART02
                INPUT NUMERO
                INPUT MARCAS
                INPUT ART01
                INPUT PARTI.
           START ART02 KEY IS NOT < CLAVE-ART02
                         INVALID KEY PERFORM CIERRE-A
                                     GO TO C-N-DESDE.
           MOVE "N" TO COD2.
           GO TO C-SIGO-N.
       C-LEO-N.
           MOVE 3 TO SW3.
           PERFORM MEN-ANTE-DES.
           DISPLAY " <F1> ARREGLA SALDO EN 0 " LINE 23 POSITION 10
                    BLINK REVERSE. 
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 MOVE SAL-S-PARTI TO BAJ-S-PARTI
                  MOVE 0 TO ENT-S-PARTI
                  CLOSE PARTI
                  OPEN I-O PARTI
                  REWRITE REG-PARTI INVALID KEY WRITE REG-PARTI
                        END REWRITE
                  CLOSE PARTI
                  OPEN INPUT PARTI.
          
           IF F03 GO TO C-N-PAR1.
           IF F02 GO TO C-N-PAR2.
           DISPLAY SPACES LINE 23 POSITION 10 SIZE 30.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-N.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART02.
           IF COD3 = "F" GO TO F-CONS.
           IF EMP-ART02 > NUM-EMP GO TO F-CONS.
           IF SUC-ART02 > SUC-EMP GO TO F-CONS.
           MOVE COD-ART02 TO COD-ART01 COD-PARTI.
           READ ART01 INVALID KEY PERFORM E-ART01 100 TIMES
                              PERFORM CIERRE-A
                              GO TO C-N-DESDE.
           IF REGI-ART01 NOT = "P" PERFORM E-PARTI 100 TIMES
                                   PERFORM CIERRE-A
                                   GO TO C-N-DESDE.
           MOVE NUM-ART02 TO COD-NUM.
           PERFORM LEO-NUM-R.
      *     IF ACT-PRI-EMP = 99999
      *            MOVE NUM-ART02 TO N-ART02
      *            COMPUTE COD-MAR = N-1-ART02 * 1
      *      ELSE MOVE MAR-ART02 TO COD-MAR.
           MOVE MAR-ART02 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM DES-DET THRU DES-MAR.
       C-N-PARTI.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A GO TO C-N-DESDE.
           MOVE NUM-CAP TO PAR-PARTI.
           MOVE EMP-ART02 TO EMP-PARTI.
           MOVE SUC-ART02 TO SUC-PARTI.
           MOVE COD-ART02 TO COD-PARTI.
           START PARTI KEY IS NOT < CLAVE-PARTI
                    INVALID KEY PERFORM CIERRE-A GO TO C-N-DESDE.
       C-N-PAR1. 
           READ PARTI NEXT AT END PERFORM CIERRE-A 
                                  GO TO C-N-DESDE.
           IF EMP-PARTI NOT = EMP-ART02 GO TO F-CONS.
           IF SUC-PARTI NOT = SUC-ART02 GO TO F-CONS.
           IF COD-PARTI NOT = COD-ART02 GO TO F-CONS.
           IF C-STOCK = "S"
              COMPUTE STOCK = SAL-S-PARTI - BAJ-S-PARTI
              IF STOCK = 0 GO TO C-N-PAR1. 
           PERFORM DES-COD THRU DES-POS.
           GO TO C-LEO-N.
       C-N-PAR2.
           READ PARTI PREVIOUS AT END PERFORM CIERRE-A 
                      GO TO C-N-DESDE.
           IF EMP-PARTI NOT = EMP-ART02 GO TO F-CONS.
           IF SUC-PARTI NOT = SUC-ART02 GO TO F-CONS.
           IF COD-PARTI NOT = COD-ART02 GO TO F-CONS. 
           IF C-STOCK = "S"
              COMPUTE STOCK = SAL-S-PARTI - BAJ-S-PARTI
              IF STOCK = 0 GO TO C-N-PAR2.
           PERFORM DES-COD THRU DES-POS.
           GO TO C-LEO-N.
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           DISPLAY "DESDE:" LINE 9 POSITION 3.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       C-A-DESDE.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO DET-NUM.
       C-A-POSI.
           OPEN INPUT NUMERO.
           OPEN INPUT ART02.
           OPEN INPUT MARCAS.
           OPEN INPUT ART01 PARTI.
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY PERFORM CIERRE-A
                                    GO TO C-A-DESDE.
           MOVE 0 TO LISTO-H.
           MOVE "A" TO COD2.
           GO TO C-SIGO-A.
       C-SIGO-A.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-NUM.
           IF COD-NUM = SPACES GO TO C-SIGO-A.
       C-SIGO-A1.
           IF COD3 = "F" GO TO F-CONS.
           MOVE COD-NUM TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02
                      INVALID KEY PERFORM CIERRE-A
                                  GO TO C-A-DESDE.
           GO TO C-SIGO-A2.
       C-LEO-A.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F03 GO TO C-A-PARTI.
           IF F02 GO TO C-A-PAR2.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART02 MARCAS GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-A2.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART02.
           IF EMP-ART02 > NUM-EMP GO TO F-CONS.
           IF SUC-ART02 > SUC-EMP GO TO F-CONS.
           IF NUM-ART02 NOT = COD-NUM
                   GO TO C-SIGO-A.
           PERFORM DES-DET.
           MOVE EMP-ART02 TO EMP-ART01 EMP-PARTI.
           MOVE SUC-ART02 TO SUC-PARTI.
           MOVE COD-ART02 TO COD-ART01 COD-PARTI.
           READ ART01 INVALID KEY PERFORM E-ARTO1 100 TIMES
                        PERFORM CIERRE-A 
                        GO TO C-A-DESDE.
           IF REGI-ART01 NOT = "P" PERFORM E-PARTI 100 TIMES
                         PERFORM CIERRE-A
                         GO TO C-A-DESDE.
      *     IF ACT-PRI-EMP = 99999
      *           MOVE NUM-ART02 TO N-ART02
      *           COMPUTE COD-MAR = N-1-ART02 * 1
      *       ELSE MOVE MAR-ART02 TO COD-MAR.
           MOVE MAR-ART02 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM DES-MAR.
           MOVE 0 TO PAR-PARTI.
           START PARTI KEY IS NOT < CLAVE-PARTI
                      INVALID KEY PERFORM CIERRE-A
                      GO TO C-A-DESDE.
       C-A-PARTI.
           READ PARTI NEXT AT END GO TO F-CONS.
           IF EMP-ART02 NOT = EMP-PARTI GO TO F-CONS.
           IF SUC-ART02 NOT = SUC-PARTI GO TO F-CONS.
           IF COD-ART02 NOT = COD-PARTI GO TO C-SIGO-A2.
           PERFORM DES-COD THRU DES-POS.
           GO TO C-LEO-A.
       C-A-PAR2.
           READ PARTI PREVIOUS AT END GO TO F-CONS.
           IF EMP-ART02 NOT = EMP-PARTI GO TO F-CONS.
           IF SUC-ART02 NOT = SUC-PARTI GO TO F-CONS.
           IF COD-ART02 NOT = COD-PARTI GO TO C-SIGO-A2.
           PERFORM DES-COD THRU DES-POS.
           GO TO C-LEO-A.
       F-CONS.
           CLOSE NUMERO ART02 MARCAS ART01 PARTI.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.
       CIERRE-A.
           CLOSE NUMERO ART02 ART01 MARCAS PARTI.
       CIERRE-I.
           CLOSE NUMERO ART02 ART01 MARCAS PARTI IMP02.     
      *---- CORREGIR POR MARCA , CON STOCK O TODAS LAS PARTIDAS,
      *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "HASTA: " LINE 9 POSITION 40.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       L-N-DESDE.
           PERFORM BORRA-PANTA-C.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-ART02.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           MOVE "N" TO COD2.
       L-N-POSI.
           OPEN OUTPUT IMP02.
           OPEN INPUT ART02.
           OPEN INPUT NUMERO.
           OPEN INPUT MARCAS ART01 PARTI.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY PERFORM CIERRE-I
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "ARTICULOS " TO TIT-4-EDI.
           MOVE "-P/PARTIDA" TO TIT-5-EDI.
           MOVE "- >>>     " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           MOVE 0 TO MARCA-L
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
           IF COD3 = "F" PERFORM PIE-P
                         PERFORM CIERRE-I
                         GO TO L-N-DESDE.
           MOVE NUM-ART02 TO N-ART02.
      *     IF MARCA-L = 0 PERFORM CARGA-MARCA.
           IF CONT-R = 0 PERFORM TITU.     
      *     IF ACT-PRI-EMP = 99999
      *           IF N-1-ART02 NOT = MARCA-L
      *                 GO TO C-MARCA
      *           ELSE GO TO SIGO-L.
      *     IF MAR-ART02 = MARCA-L GO TO SIGO-L.
           GO TO SIGO-L.
       C-MARCA.
           PERFORM PIE-P
           COMPUTE CONT-R = 72 - CONT-R
           PERFORM ESPA CONT-R TIMES
           PERFORM CARGA-MARCA
           MOVE 0 TO CONT-R CONT-P
           PERFORM TITU.
       SIGO-L.               
           PERFORM MUEVO-IMP.
           IF COD-ART02 = SPACES GO TO L-LEO.
           WRITE LIN-IMP02 FROM IMP-3 AFTER ADVANCING 1.
           WRITE LIN-IMP02 FROM IMP-31 AFTER ADVANCING 1.
           ADD 2 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
       CARGA-MARCA.
           MOVE SPACES TO DET-MAR.
      *     IF ACT-PRI-EMP = 99999 MOVE N-1-ART02 TO COD-MAR
      *        ELSE MOVE MAR-ART02 TO COD-MAR.
           READ MARCAS WITH NO LOCK INVALID KEY 
                             MOVE "********" TO MARCA-ED.
           IF DET-MAR NOT = SPACES MOVE DET-MAR TO MARCA-ED.
           MOVE COD-MAR TO MARCA-L.
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
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
      *     WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
      *     WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-11 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
      *     ADD 10 TO CONT-R.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       LEO-ART02.
           READ ART02 NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO WITH NO LOCK INVALID KEY MOVE "F" TO COD3.
       LEO-MAR-R.
           READ MARCAS WITH NO LOCK INVALID KEY MOVE "F" TO COD3.
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
           MOVE 122108 TO TAB-POS(12)
           MOVE 132115 TO TAB-POS(13).
           MOVE 142108 TO TAB-POS(14).
           MOVE 152115 TO TAB-POS(15)
           MOVE 162115 TO TAB-POS(16).
           MOVE 172110 TO TAB-POS(17).
           MOVE 182110 TO TAB-POS(18).
           MOVE 192110 TO TAB-POS(19).
           MOVE 202110 TO TAB-POS(20).
           MOVE 212103 TO TAB-POS(21)
           MOVE 212503 TO TAB-POS(22).
           MOVE 212903 TO TAB-POS(23).
           MOVE 222503 TO TAB-POS(24).
           MOVE 222903 TO TAB-POS(25).
           MOVE 223303 TO TAB-POS(26).
           MOVE 223703 TO TAB-POS(27).
           MOVE 103530 TO TAB-POS(28).
           MOVE 132108 TO TAB-POS(29).
           MOVE 113520 TO TAB-POS(30).
           MOVE 212110 TO TAB-POS(31).
       TIT-PAN.
           MOVE "  ARCHIVO " TO TIT-1-PAN.
           MOVE "DE ART. P/" TO TIT-2-PAN.
           MOVE "PARTIDA   " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F2>  ARREGLA STOCK PARTIDAS" LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
       DES-DET.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-MAR.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       DES-COD.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-PARTI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-PAR.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY PAR-PARTI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
       DES-PRE.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE PRE-V-PARTI TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-F-V.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE FEC-PARTI TO FEC-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE 8.
       DES-COS.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE PRE-C-PARTI TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-II.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE II-PARTI TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-SS.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           COMPUTE SAL-S = SAL-S-PARTI - BAJ-S-PARTI.
           MOVE SAL-S TO SAL-S-EDI.
           DISPLAY SAL-S-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE 10.
       DES-ES.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE ENT-S-PARTI TO SAL-E-EDI.
           DISPLAY SAL-E-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE 10. 
       DES-KS.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE KILO-S-PARTI TO KILO-S-EDI.
           DISPLAY KILO-S-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                                 SIZE TAB-SIZ(SW3).
       DES-KB.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE KILO-B-PARTI TO KILO-B-EDI.
           DISPLAY KILO-B-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                                 SIZE TAB-SIZ(SW3).
       DES-P1.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE SEC-1-PARTI TO ALF-CAP.
           PERFORM DES-ALF.
       DES-P2.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE SEC-2-PARTI TO ALF-CAP.
           PERFORM DES-ALF.
       DES-POS.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE SEC-3-PARTI TO ALF-CAP.
           PERFORM DES-ALF.
       E-ART01.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "CARGUE ARCH.ART.-COMPRA-" TO ALF-CAP.
           PERFORM DES-ALF.
       E-ART02.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "CARGUE ARCH.ART.-VENTA-" TO ALF-CAP.
           PERFORM DES-ALF.
       E-PARTI.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "ARTICULO SIN PARTIDA" TO ALF-CAP.
           PERFORM DES-ALF.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-C.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE 30 TO SW3.
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
       LEA-EMPRE.
           OPEN INPUT EMPRESAS MANEJO.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS MANEJO GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS MANEJO
                                     GO TO LEA-EMPRE.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE MANEJO EMPRESAS
                GO TO LEA-EMPRE.
           CLOSE MANEJO
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E132.
           PERFORM DES-ALF.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 CLOSE EMPRESAS GO TO LEA-EMPRE.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E132.
           IF NUM-CAP = 0 GO TO F-LEA-EMPRE.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                 GO TO LEA-EMPRE.
           MOVE 10 TO SW3.
           MOVE RAZ-SOC-EMP TO ALF-CAP SUC-E132.
           PERFORM DES-ALF.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       MUEVO-IMP.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           MOVE PRE-V-ART02 TO PRE-V-ED.
           MOVE FEC-V-ART02 TO FEC-V-ED.
           MOVE UNI-V-ART02 TO UNI-V-ED.
           MOVE VAL-V-ART02 TO VAL-V-ED.
           MOVE COS-V-ART02 TO COS-V-ED.
           MOVE PRE-E-ART02 TO PRE-E-ED.
           MOVE FEC-E-ART02 TO FEC-E-ED.
           MOVE SAL-S-ART02 TO SAL-S-ED.
           MOVE MIN-S-ART02 TO MIN-S-ED.
           MOVE ENT-S-ART02 TO ENT-S-ED.
           MOVE COND-ART02(1) TO COND-1-ED.
           MOVE COND-ART02(2) TO COND-2-ED.
           MOVE COND-ART02(3) TO COND-3-ED.
           MOVE COND-ART02(4) TO COND-4-ED.
           MOVE COND-ART02(5) TO COND-5-ED.
           MOVE SEC-1-ART02 TO SEC-1-ED.
           MOVE SEC-2-ART02 TO SEC-2-ED.
           MOVE SEC-3-ART02 TO SEC-3-ED.
       C-ACTU.
           CLOSE ART02 ART01 PARTI NUMERO MARCAS.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".