       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUMERO.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. JUL'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "MARCAS.SEL".
       COPY "IMP02.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "NUME1.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "MARCAS.REG".
       COPY "IMP02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "NUME1.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(6) VALUE "NUMERO".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "-------- COMENT".
             03 FILLER       PIC X(15) VALUE "ARIO ----------".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(7) VALUE " tipos".
          02 IMP-3.
             03 COD-NUM-ED   PIC ZZZZZZ.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 DET-NUM-ED   PIC X(30)B(5).
             03 DET-NUME1-ED PIC X(30).
       01 COD-OPE            PIC X.
       01 LISTO-D            PIC 999999.
       01 LISTO-H            PIC 999999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 A                  PIC 99 VALUE 0.
       01 DONDE              PIC X.
       01 MARCA              PIC 9(6) VALUE 0.
       01 ANTE.
          03 ANT             PIC X OCCURS 30 TIMES.
       01 CAPTU.
          03 CAP             PIC X OCCURS 30 TIMES.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC Z(6).
             03 DET-DAT     PIC X(30).
       01 CONTA-AUX.
          02 C-A-1          PIC 99 VALUE 0.
          02 C-A-2          PIC 99 VALUE 0.
          02 C-A-3          PIC 99 VALUE 0.
       01 LALI              PIC 99 VALUE 0.
       01 PRECIO-ED         PIC ZZZ,99.
       01 SAL-ED            PIC ZZZ,99.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART02.ERR".
       COPY "ART01.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "EMPRE.ERR".
       COPY "NUME1.ERR".
       COPY "IMP01.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2.
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
           MOVE 28 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.

       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
      *     IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
      *     IF F05 GO TO LIST-NUM.
      *     IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 20.
           DISPLAY "CODIGO   :" LINE 12 POSITION 3.
           DISPLAY "DETALLE  :" LINE 17 POSITION 3.
           DISPLAY "COMENT.  :" LINE 19 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           OPEN INPUT NUMERO NUME1.
           MOVE SPACES TO DET-NUM.
       CARGA-COD.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-COD.
           MOVE NUM-CAP TO COD-NUM COD-NUME1.
       LEE-MAR.
           MOVE DET-NUM TO ANTE.
           READ NUMERO INVALID KEY MOVE "A" TO DONDE
                                   GO TO ALTA.
           READ NUME1 INVALID KEY MOVE SPACES TO DET-NUME1.
           PERFORM DESP-DES THRU DESP-DES1.
           PERFORM MEN-B-M.
           MOVE "M" TO DONDE.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           MOVE DET-NUM TO ANTE.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F09 MOVE "M" TO COD-OPE
                  GO TO MODI.
           IF F10 MOVE "B" TO COD-OPE
                  GO TO BAJA.
           GO TO CARGA-COD.
       ALTA.
           MOVE "A" TO COD-OPE.
           MOVE "<F2>VUELVE ANT." TO MEN-1.
           MOVE " - <F3>REPITE D" TO MEN-2.
           MOVE "ETALLE         " TO MEN-3.
           PERFORM MENS-PAN.
           PERFORM MEN-ANTE.
           MOVE 17 TO SW3.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP CLOSE NUMERO  NUME1 GO TO INI-MOD.
           IF F03 GO TO CARGA-UNO.
           IF F02 GO TO CARGA-COD.
           MOVE ALF-CAP TO DET-NUM.
           CLOSE NUMERO.
           OPEN I-O NUMERO.
           WRITE REG-NUMERO INVALID KEY REWRITE REG-NUMERO.
           CLOSE NUMERO.
           OPEN INPUT NUMERO.
       ALTA1.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F02 GO TO ALTA.
           IF F03 MOVE DET-NUME1 TO ALF-CAP
               DISPLAY DET-NUME1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE ALF-CAP TO DET-NUME1.
           CLOSE NUME1.
           OPEN I-O NUME1.
           WRITE REG-NUME1 INVALID KEY DISPLAY "ERROR 02".
           CLOSE NUME1.
           OPEN INPUT NUME1.
           GO TO CARGA-COD.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE NUMERO NUME1 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F01 CLOSE NUMERO NUME1
                  OPEN I-O NUMERO NUME1
                  DELETE NUMERO 
                  DELETE NUME1
                  CLOSE NUMERO NUME1
                  OPEN INPUT NUMERO NUME1
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK NUMERO RECORD
                  UNLOCK NUME1 RECORD
                  GO TO CARGA-COD.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK NUMERO RECORD.
           UNLOCK NUME1 RECORD.
           GO TO CARGA-COD.
       MODI.
           MOVE "<F2>VUELVE ANT." TO MEN-1.
           MOVE " <F3>REPITE DET" TO MEN-2.
           MOVE ". <F10>PROXIMO " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 CLOSE NUMERO NUME1 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 CLOSE NUMERO NUME1 GO TO INI-MOD.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW1 TO SW1.
           IF ESCAP CLOSE NUMERO  NUME1 GO TO INI-MOD.
           IF F03 GO TO CARGA-UNO.
           IF F10 PERFORM DESP-DES
                  GO TO MODI1.
           MOVE ALF-CAP TO DET-NUM.
       MODI1.
           PERFORM MEN-ANTE-DES.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW1 TO SW1.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F02 GO TO MODI.
           IF F03 MOVE DET-NUME1 TO ALF-CAP
              DISPLAY DET-NUME1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE ALF-CAP TO DET-NUME1.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF F01 CLOSE NUMERO NUME1
                  OPEN I-O NUMERO NUME1
                  REWRITE REG-NUMERO 
                  REWRITE REG-NUME1 INVALID KEY WRITE REG-NUME1
                  CLOSE NUMERO NUME1
                  OPEN INPUT NUMERO NUME1
              ELSE PERFORM MEN-NO-MODI.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK NUMERO RECORD.
           UNLOCK NUME1 RECORD.
           GO TO CARGA-COD.
       CARGA-UNO.
           MOVE "<F2>IZQ.<F3>DER" TO MEN-1.
           MOVE ".<F4>INS.<F10>B" TO MEN-2.
           MOVE "ORRA.<F1>CONF. " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE SPACES TO CAPTU.
           MOVE ANTE TO DET-NUM.
           PERFORM DESP-DES.
           MOVE ANT(1) TO CAP(1).
           MOVE TAB-COL(SW3) TO A. 
           MOVE 1 TO SW2.
       SIGA-UNO.
           ADD 1 TO SW2.
           ADD 1 TO A.
           IF SW2 > 30 MOVE CAPTU TO DET-NUM
                       GO TO CARGA-UNO-F.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION A 
                       SIZE 1 NO BEEP 
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F01 MOVE CAPTU TO DET-NUM
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
           CLOSE NUMERO.
           OPEN I-O NUMERO.
           WRITE REG-NUMERO INVALID KEY REWRITE REG-NUMERO.
           CLOSE NUMERO.
           OPEN INPUT NUMERO.
           IF COD-OPE = "A" GO TO ALTA1.
           IF COD-OPE = "M" GO TO MODI1.
           GO TO CARGA-COD.
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
      **********************************************
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      -" LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "MARCA:"  LINE 8 POSITION 60
           DISPLAY "CODIGO" LINE 9 POSITION 2.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 12.
           DISPLAY "COMENTARIO" LINE 9 POSITION 44.
           DISPLAY "P/VENTA" LINE 9 POSITION 65.
           DISPLAY "STOCK " LINE 9 POSITION 73.
           OPEN INPUT EMPRESAS NUME1. 
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 CLOSE NUME1 GO TO INI-MOD.
       C-N-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           ACCEPT COD-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                         ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F01 GO TO INI-MOD.
           IF ESCAP GO TO INI-MOD.
           PERFORM BORRA-PANTA-C.
       C-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           ACCEPT MARCA LINE 8 POSITION 70 NO BEEP TAB CONVERT.
       C-N-POSI.
           OPEN INPUT NUMERO.
           START NUMERO KEY IS NOT < CLAVE-NUMERO
                         INVALID KEY GO TO C-N-DESDE.
           MOVE 0 TO SW2.
           MOVE 4 TO SW3.
           MOVE "N" TO COD2.
           GO TO C-LEO.
      * **************************************************
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC.:      - " LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "MARCA: " LINE 8 POSITION 60.
           DISPLAY "CODIGO" LINE 9 POSITION 2.
           DISPLAY "DESCRIPCION" LINE 9 POSITION 12.
           DISPLAY "COMENTARIO" LINE 9 POSITION 44.
           DISPLAY "P/VENTA" LINE 9 POSITION 65.
           DISPLAY "STOCK "  LINE 9 POSITION 73.
           OPEN INPUT EMPRESAS NUME1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 CLOSE NUME1 GO TO INI-MOD.
       C-A-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           ACCEPT DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
       C-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
           DISPLAY "MARCA:" LINE 8 POSITION 60
           ACCEPT MARCA LINE 8 POSITION 70 NO BEEP TAB CONVERT.
       C-A-POSI.
           OPEN INPUT NUMERO.
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY GO TO C-A-DESDE.
           MOVE 0 TO SW2.
           MOVE 4 TO SW3.
           MOVE "A" TO COD2.
           GO TO C-LEO.
       C-LEO.
           ADD 1 TO SW2.
           ADD 1 TO SW3.
           IF SW2 < 10 GO TO C-SIGOLEO. 
           PERFORM MEN-CONS.
           MOVE 1 TO SW2.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F03 GO TO C-SIGOLEO.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO NUME1 GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGOLEO.
           IF SW3 > 14 MOVE 5 TO SW3.
           IF SW2 = 1 PERFORM BORRA-PANTA-C.
           MOVE "X" TO COD3.
           PERFORM LEO-MAR.
           IF COD3 = "F" CLOSE NUMERO
                  IF COD2 = "A" GO TO C-A-DESDE 
                  ELSE GO TO C-N-DESDE.
           IF COD2 = "A" IF DET-NUM > LISTO-H-A CLOSE NUMERO
                                                GO TO C-A-DESDE.
           IF COD2 = "N" IF COD-NUM > LISTO-H CLOSE NUMERO
                                              GO TO C-N-DESDE.
      * +++++++++  SOLO POR MARCA ++++++++++++++
           MOVE NUM-EMP  TO EMP-ART02
           MOVE SUC-EMP  TO SUC-ART02
           MOVE COD-NUM TO COD-NUME1 NUM-ART02
           MOVE 0 TO MAR-ART02
           OPEN INPUT ART02
           START ART02 KEY NOT LESS CLAVE-ART02 INVALID KEY 
                  MOVE 0 TO PRE-V-ART02
                  COMPUTE SW2 = SW2 - 1
                  COMPUTE SW3 = SW3 - 1
                  CLOSE ART02
                  GO TO C-LEO.
           READ ART02 NEXT AT END MOVE 0 TO PRE-V-ART02
                  COMPUTE SW2 = SW2 - 1
                  COMPUTE SW3 = SW3 - 1
                  CLOSE ART02
                  GO TO C-LEO.
           IF MAR-ART02 NOT = MARCA 
                  COMPUTE SW2 = SW2 - 1
                  COMPUTE SW3 = SW3 - 1
                  CLOSE ART02 
                  GO TO C-LEO.
      ***************************OSV**********    
           READ ART02 NEXT AT END MOVE 0 TO SAL-S-ART02
                  COMPUTE SW2 = SW2 - 1
                  COMPUTE SW3 = SW3 - 1
                  CLOSE ART02
                  GO TO C-LEO.
           CLOSE ART02.

           IF COD-NUM = SPACES GO TO LEO-MAR.  
           COMPUTE LALI = SW2 + 10.

           DISPLAY COD-NUM LINE LALI POSITION 3.
           COMPUTE SW1 = SW3 + 13.
           DISPLAY DET-NUM LINE LALI POSITION 12.

           MOVE NUM-EMP  TO EMP-ART02
           MOVE SUC-EMP  TO SUC-ART02
           MOVE COD-NUM TO COD-NUME1 NUM-ART02
           MOVE 0 TO MAR-ART02
           OPEN INPUT ART02
           START ART02 KEY NOT LESS CLAVE-ART02 INVALID KEY 
                  MOVE 0 TO PRE-V-ART02
                  GO TO VAYA.
           READ ART02 NEXT AT END MOVE 0 TO PRE-V-ART02
                                  GO TO VAYA.
           IF NUM-ART02 NOT = COD-NUM MOVE 0 TO PRE-V-ART02.                     
      *     IF MAR-ART02 NOT = MARCA CLOSE ART02 GO TO C-LEO.
       VAYA.           
           READ NUME1 INVALID KEY MOVE "  " TO DET-NUME1.
           DISPLAY DET-NUME1 LINE LALI POSITION 45.
           MOVE PRE-V-ART02  TO PRECIO-ED
           DISPLAY PRECIO-ED LINE LALI POSITION 65.
           MOVE SAL-S-ART02 TO SAL-ED
           DISPLAY SAL-ED LINE LALI POSITION 72.
           CLOSE ART02           
           
           ADD 1 TO LALI SW2
           DISPLAY SEC-1-ART02 LINE LALI POSITION 20
           DISPLAY SEC-2-ART02 LINE LALI POSITION 30
           DISPLAY SEC-3-ART02 LINE LALI POSITION 40


           GO TO C-LEO.
       F-CONS.
           CLOSE NUMERO.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:      - " LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "MARCA: " LINE 8 POSITION 60.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-N-DESDE.
           PERFORM BORRA-PANTA-C.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-NUM COD-NUME1.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           MOVE "N" TO COD2.
       L-N-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT NUMERO NUME1.
           START NUMERO KEY IS NOT < CLAVE-NUMERO
                        INVALID KEY CLOSE NUMERO NUME1 IMP01
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "NUMEROS DE" TO TIT-4-EDI.
           MOVE " ARTICULO " TO TIT-5-EDI.
           MOVE ">>>       " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:      - " LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "MARCA: " LINE 8 POSITION 60.
           OPEN INPUT EMPRESAS.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-A-DESDE.
           PERFORM BORRA-PANTA-C.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO LISTO-D-A DET-NUM.
       L-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       L-A-POSI.
           OPEN INPUT NUMERO NUME1.
           OPEN OUTPUT IMP01.
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY CLOSE NUMERO NUME1 IMP01
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-I CONT-P CONT-R.
           MOVE "     <<< L" TO TIT-1-EDI.
           MOVE "ISTADO ALF" TO TIT-2-EDI.
           MOVE "ABETICO DE" TO TIT-3-EDI.
           MOVE " NUMEROS D" TO TIT-4-EDI.
           MOVE "E ARTICULO" TO TIT-5-EDI.
           MOVE " >>>      " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.                         
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-MAR.
           IF COD2 = "N" AND COD-NUM > LISTO-H MOVE "F" TO COD3.
           IF COD2 = "A" AND DET-NUM > LISTO-H-A MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP01 NUMERO NUME1
                         IF COD2 = "N" GO TO L-N-DESDE
                         ELSE GO TO L-A-DESDE.
           MOVE COD-NUM TO COD-NUME1.
           READ NUME1 INVALID KEY MOVE SPACES TO DET-NUME1.
           MOVE COD-NUM TO COD-NUM-ED.
           MOVE DET-NUM TO DET-NUM-ED.
           MOVE DET-NUME1 TO DET-NUME1-ED.
           IF COD-NUM NOT = SPACES MOVE SPACES TO DET-NUME1
             ELSE GO TO LEO-MAR.
           WRITE LIN-IMP01 FROM IMP-3 AFTER ADVANCING 1.
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
           WRITE LIN-IMP01 FROM NORMAL AFTER 0.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       LEO-MAR.
           READ NUMERO NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 081020 TO TAB-POS(3).
           MOVE 084720 TO TAB-POS(4).
           MOVE 100306 TO TAB-POS(5).
           MOVE 112306 TO TAB-POS(6).
           MOVE 122306 TO TAB-POS(7).
           MOVE 132306 TO TAB-POS(8).
           MOVE 142306 TO TAB-POS(9).
           MOVE 152306 TO TAB-POS(10).
           MOVE 162306 TO TAB-POS(11).
           MOVE 172306 TO TAB-POS(12)
           MOVE 182306 TO TAB-POS(13).
           MOVE 192306 TO TAB-POS(14).
           MOVE 255801 TO TAB-POS(15)
           MOVE 121406 TO TAB-POS(16).
           MOVE 171430 TO TAB-POS(17).
           MOVE 102030 TO TAB-POS(18).
           MOVE 113330 TO TAB-POS(19).
           MOVE 123330 TO TAB-POS(20).
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
           MOVE 104720 TO TAB-POS(37).
       
       TIT-PAN.
           MOVE "-ARCHIVO D" TO TIT-1-PAN.
           MOVE "E NUMEROS " TO TIT-2-PAN.
           MOVE "DE ARTIC.-" TO TIT-3-PAN.
       OPC-MODA.
      *     DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 14 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 16 POSITION 18.
      *     DISPLAY "<F5>  LISTADO NUMERICO" LINE 17 POSITION 18.
      *     DISPLAY "<F6>  LISTADO ALFABET.  " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
       CARGA-EMP.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS WITH NO LOCK INVALID KEY
                     GO TO CARGA-EMP.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E80.
       CARGA-SUC.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-SUC.
           MOVE 31 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
       CARGA-EMP-F.
           EXIT.
       DESP-DES.
           MOVE 17 TO SW3.
           MOVE DET-NUM TO ALF-CAP.
           DISPLAY ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-DES1.
           MOVE 36 TO SW3.
           MOVE DET-NUME1 TO ALF-CAP.
           DISPLAY ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
