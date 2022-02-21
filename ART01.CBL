       IDENTIFICATION DIVISION.
       PROGRAM-ID. ART01.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. SEPT'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "ART01.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "LINEAS.SEL".
       COPY "PANTA.SEL".
       COPY "ENVASE.SEL".
       COPY "ACTI.SEL".
       COPY "ALICUO.SEL". 
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "ART01.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "LINEAS.REG".
       COPY "PANTA.REG".
       COPY "ENVASE.REG".
       COPY "ACTI.REG".
       COPY "ALICUO.REG".
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
             03 FILLER       PIC X(10) VALUE "----- DETA".
             03 FILLER       PIC X(10) VALUE "LLE ------".
             03 FILLER       PIC X(17) VALUE "  -----LINEAS----".
             03 FILLER       PIC X(18) VALUE "  PRECIO DE COSTO ".
             03 FILLER       PIC X(12) VALUE "  F.COMPRA  ".
             03 FILLER       PIC X(8) VALUE "UNIDAD  ".
             03 FILLER       PIC X(18) VALUE "PRECIO PROMEDIO.  ".
             03 FILLER       PIC X(16) VALUE "---IMPUESTOS----".
             03 FILLER       PIC X(8) VALUE "  % GAN.".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999BB.
             03 DET-ED       PIC X(20)BB.
             03 LIN-1-ED     PIC ZZZ.
             03 FILLER       PIC X VALUE "-".
             03 LIN-2-ED     PIC ZZZ.
             03 FILLER       PIC X VALUE "-".
             03 LIN-3-ED     PIC ZZZ.
             03 FILLER       PIC X VALUE "-".
             03 LIN-4-ED     PIC ZZZBB.
             03 COS-ED       PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 DD-ED        PIC 99/.
             03 MM-ED        PIC 99/.
             03 AA-ED        PIC 99BB.
             03 UNI-ED       PIC ZZZZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 VAL-ED       PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 IMP-1-ED     PIC ZZZ.
             03 FILLER       PIC X VALUE "-".
             03 IMP-2-ED     PIC Z.
             03 FILLER       PIC X VALUE "-".
             03 IMP-3-ED     PIC ZZZ.
             03 FILLER       PIC X VALUE "-".
             03 IMP-4-ED     PIC ZZZBBB.
             03 POR-ED       PIC ZZ9,99.
             03 ENV-ED       PIC ZZ9.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 PORC-CAP           PIC 999V99 VALUE 0.
       01 ENV-CAP            PIC 999 VALUE 0.
       01 PARTI-CAP          PIC X VALUE SPACES.
       01 MARCA-L            PIC 9(3).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 COM-IMP-EDI        PIC X(30).
       01 NUM-MAR.
          02 N-ART01            PIC 9(6).
          02 NU-ART01 REDEFINES N-ART01.
             03 N-1-ART01       PIC 9(3).
             03 N-2-ART01       PIC 9(3).
       01 DATOS.
          02 DATO     PIC X(71) OCCURS 10 TIMES.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "ART01.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "LINEAS.ERR".
       COPY "EMPRE.ERR".
       COPY "ACTI.ERR".
       COPY "ALICUO.ERR".
       COPY "ENVASE.ERR".
       COPY "IMP02.ERR".
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
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ALTA, BAJA Y MODIF " TO D20-CAP.
           PERFORM MODA-PAN.
       PANTA-ACTU.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "ARTICULO        :" LINE 10 POSITION  3.
           DISPLAY "LINEA (1)       :" LINE 11 POSITION  3.
           DISPLAY "LINEA (2)       :" LINE 12 POSITION  3.
           DISPLAY "LINEA (3)       :" LINE 13 POSITION  3.
           DISPLAY "LINEA (4)       :" LINE 14 POSITION  3.
           DISPLAY "PRECIO DE COSTO :" LINE 15 POSITION  3.
           DISPLAY "FECHA DEL COSTO :" LINE 16 POSITION  3.
           DISPLAY "UNIDADES COMPR. :" LINE 17 POSITION  3.
           DISPLAY "PRECIO PROM.COMP:" LINE 18 POSITION  3.
           DISPLAY "IMPUESTOS       :" LINE 19 POSITION  3.
           DISPLAY " - -   -  " LINE 19 POSITION 22.
           DISPLAY "PORC.DE MARGEN  :" LINE 20 POSITION  3.
           DISPLAY "ENVASE ARTICULO :" LINE 21 POSITION  3.
           DISPLAY "REGISTRO TIPO   :" LINE 22 POSITION  3. 
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       SIGA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           OPEN INPUT ART01
                INPUT MARCAS
                INPUT NUMERO
                INPUT ENVASE
                INPUT LINEAS.
       CARGA-ART.
           PERFORM MEN-ANTE.
           PERFORM BORRA-C.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM LEA-EMPRE THRU F-LEA-EMP.
           IF NUM-CAP = 0 GO TO CARGA-ART.
           MOVE NUM-CAP TO COD-ART01.
           MOVE NUM-ART01 TO COD-NUM.
           MOVE MAR-ART01 TO COD-MAR.
      *     IF ACT-PRI-EMP = 99999
      *            MOVE NUM-ART01 TO N-ART01
      *            COMPUTE COD-MAR = N-1-ART01 * 1
      *       ELSE MOVE MAR-ART01 TO COD-MAR.
           READ NUMERO WITH NO LOCK INVALID KEY PERFORM E-NUM 50 TIMES
                                   GO TO CARGA-ART.
           PERFORM DES-DET.
           READ MARCAS WITH NO LOCK INVALID KEY PERFORM E-MAR 50 TIMES
                                   MOVE "INEXISTENTE" TO DET-MAR.
           PERFORM DES-MAR.
       LEE-ART.
           READ ART01 INVALID KEY GO TO ALTA.
           PERFORM DES-LIN1 THRU DES-PAR.
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
           GO TO CARGA-LIN1.
       CARGA-LIN1.
           PERFORM MEN-ANTE-DES.
           MOVE 9 TO SW3.
           PERFORM CAP-NUM.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 IF COD2 = "M" GO TO CARGA-LIN1
                  ELSE GO TO CARGA-ART.
           IF F03 PERFORM DES-LIN1 GO TO CARGA-LIN2.
           MOVE NUM-CAP TO COD-1-LIN LIN-1-ART01.
           MOVE 0 TO COD-2-LIN COD-3-LIN COD-4-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY GO TO CARGA-LIN1.
           PERFORM DES-LIN1.
       CARGA-LIN2.
           PERFORM MEN-ANTE-DES.
           MOVE 10 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-LIN2 GO TO CARGA-LIN1.
           IF F03 PERFORM DES-LIN2 GO TO CARGA-LIN3.
           MOVE NUM-CAP TO COD-2-LIN LIN-2-ART01.
           READ LINEAS WITH NO LOCK INVALID KEY GO TO CARGA-LIN2.
           PERFORM DES-LIN2.
       CARGA-LIN3.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-LIN3 GO TO CARGA-LIN2.
           IF F03 PERFORM DES-LIN3 GO TO CARGA-LIN4.
           MOVE NUM-CAP TO COD-3-LIN LIN-3-ART01.
           READ LINEAS WITH NO LOCK INVALID KEY GO TO CARGA-LIN3.
           PERFORM DES-LIN3.
       CARGA-LIN4.
           PERFORM MEN-ANTE-DES.
           MOVE 12 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-LIN4 GO TO CARGA-LIN3.
           IF F03 PERFORM DES-LIN4 GO TO CARGA-PRE.
           MOVE NUM-CAP TO COD-4-LIN LIN-4-ART01.
           READ LINEAS WITH NO LOCK INVALID KEY GO TO CARGA-LIN4.
           PERFORM DES-LIN4.
       CARGA-PRE.
           PERFORM MEN-ANTE-DES.
           MOVE 17 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-COS GO TO CARGA-LIN4.
           IF F03 PERFORM DES-COS GO TO CARGA-F-P.
           MOVE DEC10-CAP TO COSTO-STOCK-ART01.
       CARGA-F-P.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-FEC GO TO CARGA-PRE.
           IF F03 PERFORM DES-FEC GO TO CARGA-UNI.
           MOVE NUM-CAP TO FEC-U-COMP-ART01.
       CARGA-UNI.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-UNI GO TO CARGA-F-P.
           IF F03 PERFORM DES-UNI GO TO CARGA-VPC.
           MOVE NUM-CAP TO UNI-COMP-ART01.
       CARGA-VPC.
           PERFORM MEN-ANTE-DES.
           MOVE 20 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-VAL GO TO CARGA-UNI.
           IF F03 PERFORM DES-VAL GO TO CARGA-IMP1.
           MOVE DEC10-CAP TO VAL-COMP-ART01.
       CARGA-IMP1.
           PERFORM MEN-ANTE-DES.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE " I.V.A. - Nro. de ALICUOTA " TO COM-IMP-EDI.
           DISPLAY COM-IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           MOVE 21 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-IMP1
                  MOVE 42 TO SW3
                  PERFORM BORRA 
                  GO TO CARGA-VPC.
           IF F03 MOVE IMP-IVA-ART01 TO NUM-CAP
                  PERFORM DES-IMP1.
           MOVE NUM-CAP TO IMP-IVA-ART01.
           IF NUM-CAP = 0 GO TO CARGA-IMP2.
           MOVE NUM-CAP TO ALI-ALI.
           MOVE 0 TO AA-ALI MM-ALI DD-ALI.
           MOVE NUM-EMP TO EMP-ALI.
           MOVE 0 TO SUC-ALI.
           OPEN INPUT ALICUO.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
               INVALID KEY CLOSE ALICUO GO TO CARGA-IMP1.
           READ ALICUO NEXT AT END CLOSE ALICUO GO TO CARGA-IMP1.
           CLOSE ALICUO.
           IF EMP-ALI NOT = NUM-EMP GO TO CARGA-IMP1.
           IF ALI-ALI NOT = IMP-IVA-ART01 GO TO CARGA-IMP1. 
       CARGA-IMP2.
           PERFORM MEN-ANTE-DES.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE " IMP.INTERNO - <0>:NO  <1>:SI " TO COM-IMP-EDI.
           DISPLAY COM-IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           MOVE 22 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-IMP2
                  MOVE 42 TO SW3
                  PERFORM BORRA 
                  GO TO CARGA-IMP1.
           IF F03 MOVE IMP-II-ART01 TO NUM-CAP
                  PERFORM DES-IMP2.
           IF NUM-CAP < 0 OR NUM-CAP > 1 GO TO CARGA-IMP2.
           MOVE NUM-CAP TO IMP-II-ART01.
       CARGA-IMP3.
           PERFORM MEN-ANTE-DES.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE " D.G.R. - ING.BRUTOS - Nro.ACTIVIDAD " TO COM-IMP-EDI.
           DISPLAY COM-IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           MOVE 23 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-IMP3
                  MOVE 42 TO SW3
                  PERFORM BORRA 
                  GO TO CARGA-IMP2.
           IF F03 MOVE IMP-PRO-ART01 TO NUM-CAP
                  PERFORM DES-IMP3.
           MOVE NUM-CAP TO IMP-PRO-ART01.
           IF NUM-CAP = 0 GO TO CARGA-IMP4.
           MOVE 2 TO IMP-ACT.
           MOVE NUM-CAP TO ACT-ACT.
           OPEN INPUT ACTI.
           READ ACTI INVALID KEY CLOSE ACTI GO TO CARGA-IMP3.
           CLOSE ACTI.
       CARGA-IMP4.
           PERFORM MEN-ANTE-DES.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE " TASA MUNIC. - Nro.ACTIVIDAD " TO COM-IMP-EDI.
           DISPLAY COM-IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
           MOVE 24 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-IMP4 
                MOVE 42 TO SW3
                PERFORM BORRA
                GO TO CARGA-IMP3.
           IF F03 MOVE IMP-MUN-ART01 TO NUM-CAP
                  PERFORM DES-IMP4.
           MOVE NUM-CAP TO IMP-MUN-ART01.
           IF NUM-CAP = 0 GO TO CARGA-POR.
           MOVE 3 TO IMP-ACT.
           MOVE NUM-CAP TO ACT-ACT.
           OPEN INPUT ACTI.
           READ ACTI INVALID KEY CLOSE ACTI GO TO CARGA-IMP4.
           CLOSE ACTI.
           MOVE SPACES TO COM-IMP-EDI.
           MOVE 42 TO SW3.
           DISPLAY COM-IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
       CARGA-POR.
           PERFORM MEN-ANTE-DES.
           MOVE 26 TO SW3.
           ACCEPT PORC-CAP LINE 20 POSITION 20 SIZE 6 
                             NO BEEP TAB CONVERT
                             ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 MOVE PORC-ART01 TO PORC-CAP
                  PERFORM DES-POR GO TO CARGA-IMP4.
           IF F03 MOVE PORC-ART01 TO PORC-CAP
                  PERFORM DES-POR GO TO CARGA-ENV.
           MOVE PORC-CAP TO PORC-ART01.
       CARGA-ENV.
           PERFORM MEN-ANTE-DES.
           MOVE 40 TO SW3.
           ACCEPT ENV-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                            ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-ENV GO TO CARGA-POR.
           IF F03 PERFORM DES-ENV GO TO CARGA-PAR.
           MOVE ENV-CAP TO NUM-ENVA.
           READ ENVASE WITH NO LOCK INVALID KEY GO TO CARGA-ENV.
           MOVE ENV-CAP TO ENV-ART01.
           PERFORM DES-ENV.
       CARGA-PAR.
           PERFORM MEN-ANTE-DES.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY " ING.: P (POR PARTIDA) " LINE 22 POSITION 25
                        BLINK.
           MOVE SPACES TO PARTI-CAP.
           ACCEPT PARTI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-PAR GO TO CARGA-ENV.
           IF F03 PERFORM DES-PAR GO TO GRAVO.
           IF PARTI-CAP = "p" MOVE "P" TO PARTI-CAP.
           IF PARTI-CAP = " " GO TO S-PAR.
           IF PARTI-CAP = "P" DISPLAY "  REGISTRO P/PARTIDA  "
               LINE 22 POSITION 25 BLINK REVERSE
               GO TO S-PAR.
           GO TO CARGA-PAR.
       S-PAR.
           MOVE PARTI-CAP TO REGI-ART01.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * FALTA AGREGAR EL CONTROL DE CONDICIONES
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       GRAVO.
           CLOSE ART01.
           OPEN I-O ART01.
           WRITE REG-ART01 INVALID KEY REWRITE REG-ART01.
           CLOSE ART01.
           OPEN INPUT ART01.
           IF COD2 = "M" MOVE "X" TO COD2.
           GO TO CARGA-ART.
       BAJA.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           IF F01 CLOSE ART01
                  OPEN I-O ART01
                  DELETE ART01 
                  CLOSE ART01
                  OPEN INPUT ART01
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK ART01 RECORD
                  GO TO CARGA-ART.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ART01 RECORD.
           GO TO CARGA-ART.
       MODI.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           UNLOCK ART01 RECORD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM C-ACTU GO TO INI-MOD.
           IF F01 MOVE "M" TO COD2 GO TO CARGA-LIN1.
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
           MOVE NUM-EMP TO EMP-ART01.
       C-N-DESDE.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 8 TO SW3.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO COD-ART01.
       C-N-POSI.
           OPEN INPUT ART01
                INPUT ENVASE
                INPUT NUMERO
                INPUT MARCAS
                INPUT LINEAS.
           START ART01 KEY IS NOT < CLAVE-ART01
                INVALID KEY CLOSE NUMERO ART01 MARCAS LINEAS ENVASE
                                     GO TO C-N-DESDE.
           MOVE "N" TO COD2.
           GO TO C-SIGO-N.
       C-LEO-N.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART01 MARCAS LINEAS ENVASE
                      GO TO INI-MOD.
           IF F03 GO TO C-SIGO-N.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART01 MARCAS LINEAS ENVASE
                    GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-N.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART01.
           IF COD3 = "F" GO TO F-CONS.
           IF EMP-ART01 > NUM-EMP GO TO F-CONS.
           MOVE NUM-ART01 TO COD-NUM.
           PERFORM LEO-NUM-R.
      *     IF ACT-PRI-EMP = 99999
      *            MOVE NUM-ART01 TO N-ART01
      *            COMPUTE COD-MAR = N-1-ART01 * 1
      *      ELSE MOVE MAR-ART01 TO COD-MAR.
           MOVE MAR-ART01 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS. 
           PERFORM DES-DET THRU DES-PAR.
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
           MOVE NUM-EMP TO EMP-ART01.
       C-A-DESDE.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE ALF-CAP TO DET-NUM.
       C-A-POSI.
           OPEN INPUT NUMERO.
           OPEN INPUT ART01.
           OPEN INPUT ENVASE.
           OPEN INPUT MARCAS.
           OPEN INPUT LINEAS.
           START NUMERO KEY IS NOT < DET-NUM
                   INVALID KEY CLOSE NUMERO ART01 MARCAS LINEAS ENVASE
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
           MOVE COD-NUM TO NUM-ART01.
           MOVE 0 TO MAR-ART01.
           START ART01 KEY IS NOT < CLAVE-ART01
                      INVALID KEY CLOSE NUMERO MARCAS ART01 LINEAS
                                  CLOSE ENVASE 
                                  GO TO C-A-DESDE.
           GO TO C-SIGO-A2.
       C-LEO-A.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART01 MARCAS LINEAS ENVASE
                    GO TO INI-MOD.
           IF F03 GO TO C-SIGO-A.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART01 MARCAS LINEAS ENVASE 
                     GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-A2.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART01.
           IF EMP-ART01 > NUM-EMP GO TO F-CONS.
           IF NUM-ART01 NOT = COD-NUM
                   GO TO C-SIGO-A.
           PERFORM DES-DET.
      *     IF ACT-PRI-EMP = 99999
      *           MOVE NUM-ART01 TO N-ART01
      *           COMPUTE COD-MAR = N-1-ART01 * 1
      *       ELSE MOVE MAR-ART01 TO COD-MAR.
           MOVE MAR-ART01 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM DES-MAR.
           PERFORM DES-COD THRU DES-PAR.
           GO TO C-LEO-A.
       F-CONS.
           CLOSE NUMERO ART01 MARCAS LINEAS ENVASE.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "HASTA: " LINE 9 POSITION 40.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART01.
       L-N-DESDE.
           PERFORM BORRA-PANTA-C.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-ART01.
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
           OPEN INPUT ART01.
           OPEN INPUT NUMERO.
           OPEN INPUT MARCAS.
           OPEN INPUT ENVASE. 
           OPEN INPUT LINEAS.
           START ART01 KEY IS NOT < CLAVE-ART01
                 INVALID KEY CLOSE ART01 MARCAS NUMERO LINEAS IMP02
                 CLOSE ENVASE
                 GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "ARTICULOS " TO TIT-4-EDI.
           MOVE "-COMPRA- >" TO TIT-5-EDI.
           MOVE ">>        " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           MOVE 0 TO MARCA-L
           GO TO L-LEO.
       L-LEO.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-ART01.
           MOVE NUM-ART01 TO COD-NUM.
           PERFORM LEO-NUM-R.
           IF COD-ART01 > LISTO-H MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP02 NUMERO MARCAS LINEAS ART01
                         CLOSE ENVASE
                         GO TO L-N-DESDE.
           MOVE NUM-ART01 TO N-ART01.
           IF MARCA-L = 0 PERFORM CARGA-MARCA.
           IF CONT-R = 0 PERFORM TITU.     
      *     IF ACT-PRI-EMP = 99999
      *           IF N-1-ART01 NOT = MARCA-L
      *                 GO TO C-MARCA
      *           ELSE GO TO SIGO-L.
           IF MAR-ART01 = MARCA-L GO TO SIGO-L.
           GO TO L-LEO.
       C-MARCA.
           PERFORM PIE-P
           COMPUTE CONT-R = 72 - CONT-R
           PERFORM ESPA CONT-R TIMES
           PERFORM CARGA-MARCA
           MOVE 0 TO CONT-R CONT-P
           PERFORM TITU.
       SIGO-L.               
           PERFORM MUEVO-IMP.
           IF COD-ART01 = SPACES GO TO L-LEO.
           WRITE LIN-IMP02 FROM IMP-3 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
       CARGA-MARCA.
           MOVE SPACES TO DET-MAR.
      *     IF ACT-PRI-EMP = 99999 MOVE N-1-ART01 TO COD-MAR
      *        ELSE MOVE MAR-ART01 TO COD-MAR.
           MOVE MAR-ART01 TO COD-MAR.
           READ MARCAS INVALID KEY MOVE "********" TO MARCA-ED.
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
           WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-10 AFTER 0.
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
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       LEO-ART01.
           READ ART01 NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO WITH NO LOCK INVALID KEY PERFORM E-NUM 50 TIMES
                                   MOVE "INEXISTENTE" TO DET-NUM.
       LEO-MAR-R.
           READ MARCAS WITH NO LOCK INVALID KEY PERFORM E-MAR 50 TIMES
                                   MOVE "INEXISTENTE" TO DET-MAR.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 091020 TO TAB-POS(4).
           MOVE 094720 TO TAB-POS(5).
           MOVE 082002 TO TAB-POS(6).
           MOVE 082520 TO TAB-POS(7).
           MOVE 102010 TO TAB-POS(8).
           MOVE 112003 TO TAB-POS(9).
           MOVE 122003 TO TAB-POS(10).
           MOVE 132003 TO TAB-POS(11).
           MOVE 142003 TO TAB-POS(12)
           MOVE 112520 TO TAB-POS(13).
           MOVE 122520 TO TAB-POS(14).
           MOVE 132520 TO TAB-POS(15)
           MOVE 142520 TO TAB-POS(16).
           MOVE 152016 TO TAB-POS(17).
           MOVE 162008 TO TAB-POS(18).
           MOVE 172006 TO TAB-POS(19).
           MOVE 182016 TO TAB-POS(20).
           MOVE 192003 TO TAB-POS(21).
           MOVE 192401 TO TAB-POS(22).
           MOVE 192603 TO TAB-POS(23).
           MOVE 193003 TO TAB-POS(24).
           MOVE 193202 TO TAB-POS(25).
           MOVE 202006 TO TAB-POS(26).
           MOVE 105015 TO TAB-POS(27).
           MOVE 103215 TO TAB-POS(28).
           MOVE 230320 TO TAB-POS(29).
           MOVE 212003 TO TAB-POS(40).
           MOVE 212520 TO TAB-POS(41).
           MOVE 194030 TO TAB-POS(42).
           MOVE 222001 TO TAB-POS(43).
            
       TIT-PAN.
           MOVE " ARCHIVO D" TO TIT-1-PAN.
           MOVE "E ARTICULO" TO TIT-2-PAN.
           MOVE "S -COMPRA-" TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 16 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO  " LINE 17 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 18 POSITION 18.
       DES-DET.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-MAR.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       DES-COD.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-LIN1.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY LIN-1-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE LIN-1-ART01 TO COD-1-LIN.
           MOVE 0 TO COD-2-LIN COD-3-LIN COD-4-LIN.
           READ LINEAS INVALID KEY MOVE "*********" TO DET-LIN.
           IF COD-1-LIN = 0 MOVE SPACES TO DET-LIN.
           MOVE 13 TO SW3.
           DISPLAY DET-LIN  LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DES-LIN2.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY LIN-2-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE LIN-2-ART01 TO COD-2-LIN.
           READ LINEAS INVALID KEY MOVE "*********" TO DET-LIN.
           IF COD-2-LIN = 0 MOVE SPACES TO DET-LIN.
           MOVE 14 TO SW3.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-LIN3.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY LIN-3-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE LIN-3-ART01 TO COD-3-LIN.
           READ LINEAS INVALID KEY MOVE "*********" TO DET-LIN.
           IF COD-3-LIN = 0 MOVE SPACES TO DET-LIN.
           MOVE 15 TO SW3.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-LIN4.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY LIN-4-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE LIN-4-ART01 TO COD-4-LIN.
           READ LINEAS INVALID KEY MOVE "*********" TO DET-LIN.
           IF COD-4-LIN = 0 MOVE SPACES TO DET-LIN.
           MOVE 16 TO SW3.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-COS.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE COSTO-STOCK-ART01 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-FEC.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           DISPLAY DD-U-C-ART01 LINE 16 POSITION 20 SIZE 2.
           DISPLAY "/" LINE 16 POSITION 22.
           DISPLAY MM-U-C-ART01 LINE 16 POSITION 23 SIZE 2.
           DISPLAY "/" LINE 16 POSITION 25.
           DISPLAY AA-U-C-ART01 LINE 16 POSITION 26 SIZE 2.
       DES-UNI.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           DISPLAY UNI-COMP-ART01 LINE TAB-LIN(SW3) 
                                  POSITION TAB-COL(SW3)
                                  SIZE TAB-SIZ(SW3).
       DES-VAL.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE VAL-COMP-ART01 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-IMP1.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           DISPLAY IMP-IVA-ART01 LINE TAB-LIN(SW3) 
                                POSITION TAB-COL(SW3)
                                SIZE TAB-SIZ(SW3).
       DES-IMP2.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           DISPLAY IMP-II-ART01 LINE TAB-LIN(SW3) 
                                POSITION TAB-COL(SW3)
                                SIZE TAB-SIZ(SW3).
       DES-IMP3.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           DISPLAY IMP-PRO-ART01 LINE TAB-LIN(SW3)  
                                POSITION TAB-COL(SW3)
                                SIZE TAB-SIZ(SW3).
       DES-IMP4.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           DISPLAY IMP-MUN-ART01 LINE TAB-LIN(SW3) 
                                POSITION TAB-COL(SW3)
                                SIZE TAB-SIZ(SW3).
       DES-POR.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           MOVE PORC-ART01 TO POR-ED.
           DISPLAY POR-ED LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE 8.
       DES-ENV.
           MOVE 40 TO SW3.
           PERFORM BORRA.
           MOVE ENV-ART01 TO ENV-ED NUM-ENVA.
           DISPLAY ENV-ED LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE 3. 
           READ ENVASE INVALID KEY MOVE "******" TO NOM-ENVA.
           MOVE 41 TO SW3.
           PERFORM BORRA.
           DISPLAY NOM-ENVA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3). 
       DES-PAR.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 22 POSITION 25 SIZE 30.
           DISPLAY REGI-ART01 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
           IF REGI-ART01 = "P"
                DISPLAY "  REGISTRO P/PARTIDA  " LINE 22 POSITION 25
                             BLINK REVERSE.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-C.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           MOVE 27 TO SW3.
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
           MOVE 26 TO SW3.
           PERFORM BORRA.
           MOVE 40 TO SW3.
           PERFORM BORRA.
           MOVE 41 TO SW3.
           PERFORM BORRA.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 22 POSITION 25 SIZE 30.
       LEA-EMPRE.
           OPEN INPUT EMPRESAS MANEJO.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS MANEJO GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP EMP-ART01 MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS WITH NO LOCK INVALID KEY CLOSE EMPRESAS
                                                  MANEJO
                                     GO TO LEA-EMPRE.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE EMPRESAS MANEJO
                GO TO LEA-EMPRE.
           CLOSE MANEJO

           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E132.
           PERFORM DES-ALF.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.
       E-MAR.
           MOVE 29 TO SW3.
           MOVE "ERROR MARCAS" TO ALF-CAP.
           PERFORM DES-ALF.
       E-NUM.
           MOVE 29 TO SW3.
           MOVE "ERROR NUMERO" TO ALF-CAP.
           PERFORM DES-ALF.
     
       MUEVO-IMP.
           MOVE NUM-ART01 TO NUM-ED.
           MOVE MAR-ART01 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           MOVE LIN-1-ART01 TO LIN-1-ED.
           MOVE LIN-2-ART01 TO LIN-2-ED.
           MOVE LIN-3-ART01 TO LIN-3-ED.
           MOVE LIN-4-ART01 TO LIN-4-ED.
           MOVE COSTO-STOCK-ART01 TO COS-ED.
           MOVE DD-U-C-ART01 TO DD-ED.
           MOVE MM-U-C-ART01 TO MM-ED.
           MOVE AA-U-C-ART01 TO AA-ED.
           MOVE UNI-COMP-ART01 TO UNI-ED.
           MOVE VAL-COMP-ART01 TO VAL-ED.
           MOVE IMP-IVA-ART01 TO IMP-1-ED.
           MOVE IMP-II-ART01 TO IMP-2-ED.
           MOVE IMP-PRO-ART01 TO IMP-3-ED.
           MOVE IMP-MUN-ART01 TO IMP-4-ED.
           MOVE PORC-ART01 TO POR-ED.
       C-ACTU.
           CLOSE ART01 NUMERO MARCAS LINEAS ENVASE.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".