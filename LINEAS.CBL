       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINEAS.
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
       COPY "EMPRE.SEL".
       COPY "LINEAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "EMPREBM.REG".
       COPY "LINEAS.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(18) VALUE "---- NUMEROS -----".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(10) VALUE "----- DETA".
             03 FILLER       PIC X(10) VALUE "LLE ------".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "------ CUENTAS ".
             03 FILLER       PIC X(15) VALUE "CONTABL. ------".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(7) VALUE " LINEAS".
          02 IMP-3.
             03 COD-1-LIN-ED   PIC ZZZBB.
             03 COD-2-LIN-ED   PIC ZZZBB.
             03 COD-3-LIN-ED   PIC ZZZBB.
             03 COD-4-LIN-ED   PIC ZZZB(5).
             03 DET-LIN-ED   PIC X(20)B(5).
             03 RUB-1-LIN-ED   PIC Z(6).
             03 RUB-2-LIN-ED   PIC Z(6).
             03 RUB-3-LIN-ED   PIC Z(6).
             03 RUB-4-LIN-ED   PIC Z(6).
       01 LISTO-D.
          02 LIS-D          PIC 999 OCCURS 4 TIMES.
       01 LISTO-H.  
          02 LIS-H          PIC 999 OCCURS 4 TIMES.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-1-DAT     PIC Z(3)B.
             03 COD-2-DAT     PIC Z(3)B.
             03 COD-3-DAT     PIC Z(3)B.
             03 COD-4-DAT     PIC Z(3)B(5).
             03 DET-DAT     PIC X(20)B(5).
             03 RUB-1-DAT     PIC Z(6)B.
             03 RUB-2-DAT     PIC Z(6)B.
             03 RUB-3-DAT     PIC Z(6)B.
             03 RUB-4-DAT     PIC Z(6).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "EMPRE.ERR".
       COPY "LINEAS.ERR".
       COPY "IMP01.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
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
           MOVE 3 TO SW3.
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
           DISPLAY "EMPRESA  :" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 20
           DISPLAY "CODIGOS  :" LINE 10 POSITION 3.
           DISPLAY "DETALLE  :" LINE 12 POSITION 3.
           DISPLAY "----- CUENTAS -----" LINE 14 POSITION 3.
           DISPLAY "COMPRA   :" LINE 16 POSITION 3.
           DISPLAY "CONS.REC.:" LINE 18 POSITION 3.
           DISPLAY "VENTA    :" LINE 20 POSITION 3.
           DISPLAY "CONS.ENT.:" LINE 22 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           OPEN INPUT EMPRESAS MANEJO. 
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGO-ACTU.
           OPEN INPUT LINEAS.
       CARGA-COD-1.
           MOVE "S" TO COD2.
           MOVE 0 TO COD-1-LIN COD-2-LIN COD-3-LIN COD-4-LIN.
           PERFORM BORRA-CAP.
           MOVE 7 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-COD-1.
           MOVE NUM-CAP TO COD-1-LIN.
           PERFORM LEE-LIN.
           IF COD2 = "N" GO TO ALTA.
           UNLOCK LINEAS RECORD.
       CARGA-COD-2.
           MOVE "S" TO COD2.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM MEN-ANTE.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-COD-1.
           IF NUM-CAP = 0 GO TO BAJA-MODI.
           MOVE NUM-CAP TO COD-2-LIN.
           PERFORM LEE-LIN.
           IF COD2 = "N" GO TO ALTA.
           UNLOCK LINEAS RECORD.
       CARGA-COD-3.
           MOVE "S" TO COD2.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM MEN-ANTE.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-COD-2.
           IF NUM-CAP = 0 GO TO BAJA-MODI.
           MOVE NUM-CAP TO COD-3-LIN.
           PERFORM LEE-LIN.
           IF COD2 = "N" GO TO ALTA.
           UNLOCK LINEAS RECORD.
       CARGA-COD-4.
           MOVE "S" TO COD2.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM MEN-ANTE.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-COD-3.
           IF NUM-CAP = 0 GO TO BAJA-MODI.
           MOVE NUM-CAP TO COD-4-LIN.
           PERFORM LEE-LIN.
           IF COD2 = "N" GO TO ALTA.
           GO TO BAJA-MODI.
       ALTA.
           UNLOCK LINEAS RECORD.
           MOVE SW3 TO SW3.
       CARGA-DET.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           ACCEPT D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO VERI-CARGA.
           IF F03 GO TO CARGA-RUB-1.
           MOVE D20-CAP TO DET-LIN.
       CARGA-RUB-1.
           PERFORM MEN-ANTE-DES.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-DET.
           IF F03 GO TO CARGA-RUB-2.
           MOVE NUM-CAP TO RUB-1-LIN.
       CARGA-RUB-2.
           PERFORM MEN-ANTE-DES.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB-1.
           IF F03 GO TO CARGA-RUB-3.
           MOVE NUM-CAP TO RUB-2-LIN.
       CARGA-RUB-3.
           PERFORM MEN-ANTE-DES.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB-2.
           IF F03 GO TO CARGA-RUB-4.
           MOVE NUM-CAP TO RUB-3-LIN.
       CARGA-RUB-4.
           PERFORM MEN-ANTE.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB-3.
           IF F03 GO TO FIN-CARGA.
           MOVE NUM-CAP TO RUB-4-LIN.
       FIN-CARGA.
           CLOSE LINEAS.
           OPEN I-O LINEAS.
           WRITE REG-LINEAS INVALID KEY REWRITE REG-LINEAS.
           CLOSE LINEAS.
           OPEN INPUT LINEAS.
       ALTA-F.
           GO TO CARGA-COD-1.
       LEE-LIN.
           READ LINEAS INVALID KEY MOVE "N" TO COD2.
       BAJA-MODI.
           PERFORM DESP-DET THRU DESP-RUB-4.
           PERFORM MEN-B-M.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO CARGA-COD-1.
       VERI-CARGA.
           IF COD3 = "M" DISPLAY "NO MODIFICA CLAVE" LINE 25 
                                        POSITION 2 BLINK REVERSE
                         GO TO CARGA-DET.
           GO TO CARGA-COD-4.
       GRAVO. 
           CLOSE LINEAS.
           OPEN I-O LINEAS.
           WRITE REG-LINEAS INVALID KEY DISPLAY "ERROR 01".
           CLOSE LINEAS.
           OPEN INPUT LINEAS.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F01 CLOSE LINEAS
                  OPEN I-O LINEAS
                  DELETE LINEAS 
                  CLOSE LINEAS
                  OPEN INPUT LINEAS
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK LINEAS RECORD
                  GO TO CARGA-COD-1.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK LINEAS RECORD.
           GO TO CARGA-COD-1.
       MODI.
           MOVE "M" TO COD3.
           UNLOCK LINEAS RECORD.
           GO TO CARGA-DET.
       DESP-DET.
           MOVE DET-LIN TO ALF-CAP.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DESP-RUB-1.
           MOVE 12 TO SW3.
           MOVE RUB-1-LIN TO ALF-CAP.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DESP-RUB-2.
           MOVE RUB-2-LIN TO ALF-CAP.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DESP-RUB-3.
           MOVE RUB-3-LIN TO ALF-CAP.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DESP-RUB-4.
           MOVE RUB-4-LIN TO ALF-CAP.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       BORRA-CAP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
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
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
       TIT-PANTA.
           DISPLAY "EMPRESA: " LINE 7 POSITION 3.
           DISPLAY "SUC:     - " LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           DISPLAY "--- CODIGOS ---" LINE 9 POSITION 3.
           DISPLAY "--- DESCRIPCION ----" LINE 9 POSITION 23.
           DISPLAY "---- CUENTAS CONTABIL. ----" LINE 9 POSITION 48.
           OPEN INPUT EMPRESAS MANEJO. 
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           CLOSE EMPRESAS. 
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       C-N-DESDE.
           DISPLAY "COMPLETE CAMPOS" LINE 8 POSITION 63 BLINK REVERSE.
           PERFORM MEN-F-CONS.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           MOVE 26 TO SW3.
           PERFORM CAP-NUM.
           IF F01 GO TO INI-MOD.
           IF ESCAP GO TO INI-MOD.
           MOVE NUM-CAP TO COD-1-LIN.
           PERFORM BORRA-PANTA-C.
       C-N-D2.
           PERFORM MEN-ANTE.
           MOVE 27 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE NUM-CAP TO COD-2-LIN.
       C-N-D3.
           PERFORM MEN-ANTE.
           MOVE 28 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-D2.
           MOVE NUM-CAP TO COD-3-LIN.
       C-N-D4.
           PERFORM MEN-ANTE.
           MOVE 29 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-D3.
           MOVE NUM-CAP TO COD-4-LIN.        
       C-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           MOVE 30 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           MOVE NUM-CAP TO LIS-H(1).
       C-N-H2.
           PERFORM MEN-ANTE.
           MOVE 31 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-HASTA.
           MOVE NUM-CAP TO LIS-H(2).
       C-N-H3.
           PERFORM MEN-ANTE.
           MOVE 32 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-H2.
           MOVE NUM-CAP TO LIS-H(3).
       C-N-H4.
           PERFORM MEN-ANTE.
           MOVE 33 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-H3.
           MOVE NUM-CAP TO LIS-H(4).
       C-N-POSI.
           OPEN INPUT LINEAS.
           START LINEAS KEY IS NOT < CLAVE-LINEAS
                         INVALID KEY GO TO C-N-DESDE.
           MOVE 0 TO SW2.
           MOVE 15 TO SW3.
           MOVE "N" TO COD2.
           GO TO C-LEO.
       CONS-ALF.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM TIT-PANTA.
       C-A-DESDE.
           PERFORM MEN-F-CONS.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO DET-LIN.
       C-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       C-A-POSI.
           OPEN INPUT LINEAS.
           START LINEAS KEY IS NOT < DET-LIN
                        INVALID KEY GO TO C-A-DESDE.
           MOVE 0 TO SW2.
           MOVE 15 TO SW3.
           MOVE "A" TO COD2.
           GO TO C-LEO.
       C-LEO.
           ADD 1 TO SW2.
           ADD 1 TO SW3.
           IF SW2 < 11 GO TO C-SIGOLEO.
           PERFORM MEN-CONS.
           MOVE 1 TO SW2.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F03 GO TO C-SIGOLEO.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE LINEAS GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGOLEO.
           IF SW3 < 16 MOVE 16 TO SW3.
           IF SW2 = 1 PERFORM BORRA-PANTA-C.
           MOVE "X" TO COD3.
           PERFORM LEO-MAR.
           IF COD3 = "F" CLOSE LINEAS
                  IF COD2 = "A" GO TO C-A-DESDE 
                  ELSE GO TO C-N-DESDE.
           IF COD2 = "A" IF DET-LIN > LISTO-H-A CLOSE LINEAS
                                                GO TO C-A-DESDE.
           IF COD2 = "N" IF CLAVE-LINEAS > LISTO-H CLOSE LINEAS
                                              GO TO C-N-DESDE.
           IF CLAVE-LINEAS = SPACES GO TO C-SIGOLEO.
           MOVE COD-1-LIN TO COD-1-DAT(1).
           MOVE COD-2-LIN TO COD-2-DAT(1).
           MOVE COD-3-LIN TO COD-3-DAT(1).
           MOVE COD-4-LIN TO COD-4-DAT(1).
           MOVE DET-LIN TO DET-DAT(1).
           MOVE RUB-1-LIN TO RUB-1-DAT(1).
           MOVE RUB-2-LIN TO RUB-2-DAT(1).
           MOVE RUB-3-LIN TO RUB-3-DAT(1).
           MOVE RUB-4-LIN TO RUB-4-DAT(1).
           DISPLAY DATO(1) LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           GO TO C-LEO.
       F-CONS.
           CLOSE LINEAS.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA:" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           OPEN INPUT EMPRESAS MANEJO. 
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
           MOVE 26 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO COD-1-LIN.
       L-N-D2.
           PERFORM MEN-ANTE.
           MOVE 27 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO COD-2-LIN.
       L-N-D3.
           PERFORM MEN-ANTE.
           MOVE 28 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-D2.
           MOVE NUM-CAP TO COD-3-LIN.
       L-N-D4.
           PERFORM MEN-ANTE.
           MOVE 29 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-D3.
           MOVE NUM-CAP TO COD-4-LIN.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           MOVE 30 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LIS-H(1).
           MOVE "N" TO COD2.
       L-N-H2.
           PERFORM MEN-ANTE.
           MOVE 31 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-HASTA.
           MOVE NUM-CAP TO LIS-H(2).
       L-N-H3.
           PERFORM MEN-ANTE.
           MOVE 32 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-H2.
           MOVE NUM-CAP TO LIS-H(3).
       L-N-H4.
           PERFORM MEN-ANTE.
           MOVE 33 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-H3.
           MOVE NUM-CAP TO LIS-H(4).           
       L-N-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT LINEAS.
           START LINEAS KEY IS NOT < CLAVE-LINEAS
                        INVALID KEY CLOSE LINEAS IMP01
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO NUMERIC" TO TIT-3-EDI.
           MOVE "O DE LINEA" TO TIT-4-EDI.
           MOVE "S >>>     " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA:" LINE 7 POSITION 3.
           DISPLAY "SUC:     -" LINE 7 POSITION 20.
           DISPLAY "DESDE: " LINE 8 POSITION 3.
           DISPLAY "HASTA: " LINE 8 POSITION 40.
           OPEN INPUT EMPRESAS MANEJO.
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
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO LISTO-D-A DET-LIN.
       L-A-HASTA.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-A-DESDE.
           MOVE ALF-CAP TO LISTO-H-A.
       L-A-POSI.
           OPEN INPUT LINEAS.
           OPEN OUTPUT IMP01.
           START LINEAS KEY IS NOT < DET-LIN
                        INVALID KEY CLOSE LINEAS IMP01
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE SPACES TO COD3.
           MOVE 0 TO CONT-I CONT-P CONT-R.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE "  <<< LIST" TO TIT-2-EDI.
           MOVE "ADO ALFABE" TO TIT-3-EDI.
           MOVE "TICO DE LI" TO TIT-4-EDI.
           MOVE "NEAS >>>  " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.                         
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 68 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-MAR.
           IF COD2 = "N" AND CLAVE-LINEAS > LISTO-H MOVE "F" TO COD3.
           IF COD2 = "A" AND DET-LIN > LISTO-H-A MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP01 LINEAS
                         IF COD2 = "N" GO TO L-N-DESDE
                         ELSE GO TO L-A-DESDE.
           MOVE COD-1-LIN TO COD-1-LIN-ED.
           MOVE COD-2-LIN TO COD-2-LIN-ED.
           MOVE COD-3-LIN TO COD-3-LIN-ED.
           MOVE COD-4-LIN TO COD-4-LIN-ED.
           MOVE DET-LIN TO DET-LIN-ED.
           MOVE RUB-1-LIN TO RUB-1-LIN-ED.
           MOVE RUB-2-LIN TO RUB-2-LIN-ED.
           MOVE RUB-3-LIN TO RUB-3-LIN-ED.
           MOVE RUB-4-LIN TO RUB-4-LIN-ED.
           IF CLAVE-LINEAS = SPACES GO TO L-LEO.
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
           ADD 7 TO CONT-R.
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
           READ LINEAS NEXT AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 071020 TO TAB-POS(4).
           MOVE 074720 TO TAB-POS(5).
           MOVE 255801 TO TAB-POS(6).
           MOVE 101403 TO TAB-POS(7).
           MOVE 101903 TO TAB-POS(8).
           MOVE 102403 TO TAB-POS(9).
           MOVE 102903 TO TAB-POS(10).
           MOVE 121420 TO TAB-POS(11).
           MOVE 161406 TO TAB-POS(12).
           MOVE 181406 TO TAB-POS(13).
           MOVE 201406 TO TAB-POS(14).
           MOVE 221406 TO TAB-POS(15)
           MOVE 100372 TO TAB-POS(16).
           MOVE 110372 TO TAB-POS(17).
           MOVE 120372 TO TAB-POS(18).
           MOVE 130372 TO TAB-POS(19).
           MOVE 140372 TO TAB-POS(20).
           MOVE 150372 TO TAB-POS(21).
           MOVE 160372 TO TAB-POS(22).
           MOVE 170372 TO TAB-POS(23).
           MOVE 180372 TO TAB-POS(24).
           MOVE 190372 TO TAB-POS(25).
           MOVE 081003 TO TAB-POS(26).
           MOVE 081403 TO TAB-POS(27).
           MOVE 081803 TO TAB-POS(28).
           MOVE 082203 TO TAB-POS(29).
           MOVE 084703 TO TAB-POS(30).
           MOVE 085103 TO TAB-POS(31).
           MOVE 085503 TO TAB-POS(32).
           MOVE 085903 TO TAB-POS(33).
           MOVE 081016 TO TAB-POS(34).
           MOVE 084716 TO TAB-POS(35).
           MOVE 071204 TO TAB-POS(36).
           MOVE 072502 TO TAB-POS(37).
           MOVE 073230 TO TAB-POS(38).
       CARGA-EMP.
           MOVE 38 TO SW3.
           PERFORM BORRA.
           MOVE 37 TO SW3.
           PERFORM BORRA.
           MOVE 36 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS  MANEJO GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-EMP.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                GO TO CARGA-EMP.
           CLOSE MANEJO.     
           MOVE 38 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E80.
       CARGA-SUC.
           MOVE 37 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           READ EMPRESAS WITH NO LOCK INVALID KEY GO TO CARGA-SUC.
           MOVE 38 TO SW3.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80. 
       CARGA-EMP-F.
           EXIT.
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO DE LIN" TO TIT-2-PAN.
           MOVE "EAS-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 16 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO  " LINE 17 POSITION 18.
           DISPLAY "<F6>  LISTADO ALFABET.  " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
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
