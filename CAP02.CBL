       IDENTIFICATION DIVISION.
       PROGRAM-ID. ART02.
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
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
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
             03 FILLER       PIC X(17) VALUE " --PRECIO VENTA--".
             03 FILLER       PIC X(12) VALUE " FECHA PREC ".
             03 FILLER       PIC X(9) VALUE " UNIDAD  ".
             03 FILLER       PIC X(18) VALUE "PRECIO PROMEDIO.  ".
             03 FILLER       PIC X(16) VALUE "-COSTO PROMEDIO-".
          02 IMP-11.
             03 FILLER       PIC X(13) VALUE SPACES.
             03 FILLER       PIC X(18) VALUE "-PRECIO ESPEC.--  ".
             03 FILLER       PIC X(10) VALUE "-FECHA--  ".
             03 FILLER       PIC X(13) VALUE "SALDO STOCK  ".
             03 FILLER       PIC X(13) VALUE "MINIMO STOCK ".
             03 FILLER       PIC X(13) VALUE "PEND.ENTREG. ".
             03 FILLER       PIC X(21) VALUE " --COND. DE VENTAS-- ".
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
             03 PRE-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE SPACES.
             03 FEC-V-ED     PIC 99/99/99B.
             03 UNI-V-ED     PIC ZZZZZ9,999 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE SPACES.
             03 VAL-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 COS-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
          02 IMP-31.
             03 FILLER       PIC X(13) VALUE SPACES.
             03 PRE-E-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 FEC-E-ED     PIC 99/99/99B(6).
             03 SAL-S-ED     PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC X(3) VALUE SPACES.
             03 MIN-S-ED     PIC ZZZ.ZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 ENT-S-ED     PIC ---.--9,999 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 COND-1-ED    PIC ZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE "-".
             03 COND-2-ED    PIC ZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC X VALUE "-".
             03 COND-3-ED    PIC ZZ9.
             03 FILLER       PIC X VALUE "-".
             03 COND-4-ED    PIC ZZ9.
             03 FILLER       PIC X VALUE "-".
             03 COND-5-ED    PIC ZZ9BB.
             03 SEC-1-ED     PIC XXX.
             03 FILLER       PIC X VALUE "-".
             03 SEC-2-ED     PIC XXX.
             03 FILLER       PIC X VALUE "-".
             03 SEC-3-ED     PIC XXX.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 SAL-S              PIC S9(6)V999.
       01 SAL-E              PIC S9(6)V999.
       01 UNI-V              PIC 9(6)V999.
       01 IND-ORD            PIC 9.
       01 MARCA-L            PIC 9(3).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 SAL-S-EDI          PIC -----9,999.
       01 SAL-M-EDI          PIC ZZZZZZ.
       01 UNI-V-EDI          PIC ZZZZZ9,999.
       01 SAL-E-EDI          PIC -----9,999. 
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
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "ARTICULO        :" LINE 10 POSITION  3.
           DISPLAY "PRECIO VENTA    :" LINE 12 POSITION  3.
           DISPLAY "FECHA PRECIO VTA:" LINE 13 POSITION  3.
           DISPLAY "UNIDADES VEND.  :" LINE 14 POSITION  3.
           DISPLAY "PRECIO PROM.VTA.:" LINE 15 POSITION  3.
           DISPLAY "COSTO PROM. VTA.:" LINE 16 POSITION  3.
           DISPLAY "PRECIO ESPECIAL :" LINE 17 POSITION  3.
           DISPLAY "FECHA ESPECIAL  :" LINE 18 POSITION  3.
           DISPLAY "SALDO STOCK     :" LINE 19 POSITION  3.
           DISPLAY "STOCK MINIMO    :" LINE 20 POSITION  3.
           DISPLAY "PEND.DE ENTREGA :" LINE 21 POSITION  3.
           DISPLAY "CONDICIONES VTA.:" LINE 22 POSITION  3.
           DISPLAY "-   -   -   -" LINE 22 POSITION 24.
           DISPLAY "POSICION        :" LINE 23 POSITION  3.
           DISPLAY "-   -" LINE 23 POSITION 24.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       SIGA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           OPEN INPUT ART02
                INPUT ART01
                INPUT MARCAS
                INPUT NUMERO.
       CARGA-ART.
           PERFORM MEN-ANTE.
           PERFORM BORRA-C.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM LEA-EMPRE THRU F-LEA-EMP.
           IF NUM-CAP = 0 GO TO CARGA-ART.
           MOVE NUM-CAP TO COD-ART02.
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
       LEE-ART.
           READ ART02 INVALID KEY GO TO ALTA.
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
           MOVE EMP-ART02 TO EMP-ART01.
           MOVE COD-ART02 TO COD-ART01.
           READ ART01 WITH NO LOCK INVALID KEY 
                                  PERFORM E-ART01 50 TIMES
                                  GO TO CARGA-ART.
           GO TO CARGA-PRE.
       CARGA-PRE.
           PERFORM MEN-ANTE-DES.
           MOVE 12 TO SW3.
           PERFORM CAP-DEC10.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
            PERFORM MENS-PAN.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 IF COD2 = "M" GO TO CARGA-PRE
                  ELSE GO TO CARGA-ART.
           IF F03 PERFORM DES-PRE GO TO CARGA-F-P.
           MOVE DEC10-CAP TO PRE-V-ART02.
       CARGA-F-P.
           PERFORM MEN-ANTE-DES.
           MOVE 29 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-F-V GO TO CARGA-PRE.
           IF F03 PERFORM DES-F-V GO TO CARGA-UNI.
           MOVE NUM-CAP TO FEC-V-ART02.

           GO TO GRAVO.



       CARGA-UNI.
           PERFORM MEN-ANTE-DES.
           MOVE 13 TO SW3.
           ACCEPT UNI-V LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-UNI GO TO CARGA-F-P.
           IF F03 PERFORM DES-UNI GO TO CARGA-PP.
           MOVE UNI-V TO UNI-V-ART02.
       CARGA-PP.
           PERFORM MEN-ANTE-DES.
           MOVE 14 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-VAL GO TO CARGA-UNI.
           IF F03 PERFORM DES-VAL GO TO CARGA-CP.
           MOVE DEC10-CAP TO VAL-V-ART02.
       CARGA-CP.
           PERFORM MEN-ANTE-DES.
           MOVE 15 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-COS GO TO CARGA-PP.
           IF F03 PERFORM DES-COS GO TO CARGA-PO.
           MOVE DEC10-CAP TO COS-V-ART02.
       CARGA-PO.
           PERFORM MEN-ANTE-DES.
           MOVE 16 TO SW3.
           PERFORM CAP-DEC10.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-PO GO TO CARGA-CP.
           IF F03 PERFORM DES-PO GO TO CARGA-FO.
           MOVE DEC10-CAP TO PRE-E-ART02.
       CARGA-FO.
           PERFORM MEN-ANTE-DES.
           MOVE 17 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-FO GO TO CARGA-PO.
           IF F03 PERFORM DES-FO GO TO CARGA-SS.
           MOVE NUM-CAP TO FEC-E-ART02.
       CARGA-SS.
           PERFORM MEN-ANTE-DES.
           MOVE 18 TO SW3.
           ACCEPT SAL-S LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-SS GO TO CARGA-FO.
           IF F03 PERFORM DES-SS GO TO CARGA-SM.
           MOVE SAL-S TO SAL-S-ART02.
       CARGA-SM.
           PERFORM MEN-ANTE-DES.
           MOVE 19 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-MS GO TO CARGA-SS.
           IF F03 PERFORM DES-MS GO TO CARGA-ES.
           MOVE NUM-CAP TO MIN-S-ART02.
       CARGA-ES.
           PERFORM MEN-ANTE-DES.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           ACCEPT SAL-E LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-ES GO TO CARGA-SM.
           IF F03 PERFORM DES-ES GO TO CARGA-C1.
           MOVE SAL-E TO ENT-S-ART02.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * FALTA AGREGAR EL CONTROL DE CONDICIONES
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       CARGA-C1.
           PERFORM MEN-ANTE-DES.
           MOVE 23 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-C1 GO TO CARGA-ES.
           IF F03 PERFORM DES-C1 GO TO CARGA-C2.
           MOVE NUM-CAP TO COND-ART02(1).
       CARGA-C2.
           PERFORM MEN-ANTE-DES.
           MOVE 24 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-C2 GO TO CARGA-C1.
           IF F03 PERFORM DES-C2 GO TO CARGA-C3.
           MOVE NUM-CAP TO COND-ART02(2).
       CARGA-C3.
           PERFORM MEN-ANTE-DES.
           MOVE 25 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-C3 GO TO CARGA-C2.
           IF F03 PERFORM DES-C3 GO TO CARGA-C4.
           MOVE NUM-CAP TO COND-ART02(3).
       CARGA-C4.
           PERFORM MEN-ANTE-DES.
           MOVE 26 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-C4 GO TO CARGA-C3.
           IF F03 PERFORM DES-C4 GO TO CARGA-C5.
           MOVE NUM-CAP TO COND-ART02(4).
       CARGA-C5.
           PERFORM MEN-ANTE-DES.
           MOVE 27 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-C5 GO TO CARGA-C4.
           IF F03 PERFORM DES-C5 GO TO CARGA-P1.
           MOVE NUM-CAP TO COND-ART02(5).
       CARGA-P1.
           PERFORM MEN-ANTE-DES.
           MOVE 20 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-P1 GO TO CARGA-C5.
           IF F03 PERFORM DES-P1 GO TO CARGA-P2.
           MOVE ALF-CAP TO SEC-1-ART02.
       CARGA-P2.
           PERFORM MEN-ANTE-DES.
           MOVE 21 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-P2 GO TO CARGA-P1.
           IF F03 PERFORM DES-P2 GO TO CARGA-P3.
           MOVE ALF-CAP TO SEC-2-ART02.
       CARGA-P3.
           PERFORM MEN-ANTE-DES.
           MOVE 22 TO SW3.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           IF F02 PERFORM DES-POS GO TO CARGA-P2.
           IF F03 PERFORM DES-POS GO TO GRAVO.
           MOVE ALF-CAP TO SEC-3-ART02.
       GRAVO.
           CLOSE ART02.
           OPEN I-O ART02.
           WRITE REG-ART02 INVALID KEY REWRITE REG-ART02.
           CLOSE ART02.
           OPEN INPUT ART02.
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
           IF F01 CLOSE ART02
                  OPEN I-O ART02
                  DELETE ART02 
                  CLOSE ART02
                  OPEN INPUT ART02
                  MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  UNLOCK ART02 RECORD
                  GO TO CARGA-ART.
           PERFORM MEN-NO-BAJA.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ART02 RECORD.
           GO TO CARGA-ART.
       MODI.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM C-ACTU GO TO INI-MOD.
           UNLOCK ART02 RECORD.
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
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       C-N-DESDE.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 11 TO SW3.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO COD-ART02.
       C-N-POSI.
           OPEN INPUT ART02
                INPUT NUMERO
                INPUT MARCAS.
           START ART02 KEY IS NOT < CLAVE-ART02
                         INVALID KEY CLOSE NUMERO ART02 MARCAS
                                     GO TO C-N-DESDE.
           MOVE "N" TO COD2.
           GO TO C-SIGO-N.
       C-LEO-N.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART02 MARCAS GO TO INI-MOD.
           IF F03 GO TO C-SIGO-N.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART02 MARCAS GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-N.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART02.
           IF COD3 = "F" GO TO F-CONS.
           IF EMP-ART02 > NUM-EMP GO TO F-CONS.
           IF SUC-ART02 > SUC-EMP GO TO F-CONS.
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
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY CLOSE NUMERO ART02 MARCAS
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
                      INVALID KEY CLOSE NUMERO MARCAS ART02
                                  GO TO C-A-DESDE.
           GO TO C-SIGO-A2.
       C-LEO-A.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO ART02 MARCAS GO TO INI-MOD.
           IF F03 GO TO C-SIGO-A.
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
      *     IF ACT-PRI-EMP = 99999
      *           MOVE NUM-ART02 TO N-ART02
      *           COMPUTE COD-MAR = N-1-ART02 * 1
      *       ELSE MOVE MAR-ART02 TO COD-MAR.
           MOVE MAR-ART02 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM DES-MAR.
           PERFORM DES-COD THRU DES-POS.
           GO TO C-LEO-A.
       F-CONS.
           CLOSE NUMERO ART02 MARCAS.
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
           OPEN INPUT MARCAS.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY CLOSE ART02 MARCAS NUMERO IMP02
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "ARTICULOS " TO TIT-4-EDI.
           MOVE "-VENTA- >>" TO TIT-5-EDI.
           MOVE ">         " TO TIT-6-EDI.
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
                         CLOSE IMP02 NUMERO MARCAS ART02
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
           MOVE 122115 TO TAB-POS(12)
           MOVE 142110 TO TAB-POS(13).
           MOVE 152115 TO TAB-POS(14).
           MOVE 162115 TO TAB-POS(15)
           MOVE 172115 TO TAB-POS(16).
           MOVE 182108 TO TAB-POS(17).
           MOVE 192110 TO TAB-POS(18).
           MOVE 202106 TO TAB-POS(19).
           MOVE 232103 TO TAB-POS(20).
           MOVE 232503 TO TAB-POS(21).
           MOVE 232903 TO TAB-POS(22).
           MOVE 222103 TO TAB-POS(23).
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
           MOVE "DE ARTICUL" TO TIT-2-PAN.
           MOVE "OS -VENTA-" TO TIT-3-PAN.
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
           MOVE 30 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       DES-COD.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-ART02 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-PRE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE PRE-V-ART02 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-F-V.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           MOVE FEC-V-ART02 TO FEC-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE 8.
       DES-UNI.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE UNI-V-ART02 TO UNI-V-EDI.
           DISPLAY UNI-V-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE 10.
       DES-VAL.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE VAL-V-ART02 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-COS.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE COS-V-ART02 TO DEC10-EDI.
           PERFORM DES-DEC10. 
       DES-PO.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE PRE-E-ART02 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-FO.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE FEC-E-ART02 TO FEC-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE 8.
       DES-SS.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE SAL-S-ART02 TO SAL-S-EDI.
           DISPLAY SAL-S-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE 10.
       DES-MS.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE MIN-S-ART02 TO SAL-M-EDI.
           DISPLAY SAL-M-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                                SIZE 6.
       DES-ES.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE ENT-S-ART02 TO SAL-E-EDI.
           DISPLAY SAL-E-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE 10. 
       DES-C1.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           DISPLAY COND-ART02(1) LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                                 SIZE 3.
       DES-C2.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           DISPLAY COND-ART02(2) LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                                 SIZE 3.
       DES-C3.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           DISPLAY COND-ART02(3) LINE TAB-LIN(SW3) 
                                 POSITION TAB-COL(SW3).
       DES-C4.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           DISPLAY COND-ART02(4) LINE TAB-LIN(SW3) 
                                 POSITION TAB-COL(SW3).
       DES-C5.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           DISPLAY COND-ART02(5) LINE TAB-LIN(SW3) 
                                 POSITION TAB-COL(SW3).
       DES-P1.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE SEC-1-ART02 TO ALF-CAP.
           PERFORM DES-ALF.
       DES-P2.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE SEC-2-ART02 TO ALF-CAP.
           PERFORM DES-ALF.
       DES-POS.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE SEC-3-ART02 TO ALF-CAP.
           PERFORM DES-ALF.
       E-ART01.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "CARGUE ARCH.ART.-COMPRA-" TO ALF-CAP.
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
           MOVE 29 TO SW3.
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
           MOVE 31 TO SW3.
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
           CLOSE ART02 ART01 NUMERO MARCAS.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
