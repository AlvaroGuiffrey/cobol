       IDENTIFICATION DIVISION.
       PROGRAM-ID. PREFAR.
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
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "ART01.SEL".
       COPY "LINEAS.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "ART01.REG".
       COPY "LINEAS.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
      * SIN MODIFICAR
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       01 IMPRESION.
          02 IMP-10.
             03 FILLER       PIC X(8) VALUE "MARCA : ".
             03 MARCA-ED     PIC X(20).
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(6) VALUE "MARCA ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(18) VALUE "  --PRECIO VENTA--".
             03 FILLER       PIC X(12) VALUE " FECHA PREC ".
             03 FILLER       PIC X(8) VALUE "UNIDAD  ".
             03 FILLER       PIC X(18) VALUE "PRECIO PROMEDIO.  ".
             03 FILLER       PIC X(16) VALUE "-COSTO PROMEDIO-".
          02 IMP-11.
             03 FILLER       PIC X(13) VALUE SPACES.
             03 FILLER       PIC X(18) VALUE "-PRECIO OFERTA--  ".
             03 FILLER       PIC X(10) VALUE "-HASTA--  ".
             03 FILLER       PIC X(13) VALUE "SALDO STOCK  ".
             03 FILLER       PIC X(13) VALUE "MINIMO STOCK ".
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
             03 DET-ED       PIC X(30)BB.
             03 PRE-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 FEC-V-ED     PIC 99/99/99BB.
             03 UNI-V-ED     PIC ZZZZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 VAL-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 COS-V-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
          02 IMP-31.
             03 FILLER       PIC X(13) VALUE SPACES.
             03 PRE-E-ED     PIC Z.ZZZ.ZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER       PIC XX VALUE SPACES.
             03 FEC-E-ED     PIC 99/99/99B(6).
             03 SAL-S-ED     PIC ZZZ.ZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC X(7) VALUE SPACES.
             03 MIN-S-ED     PIC ZZZ.ZZ9 BLANK WHEN ZERO.
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
       01 IND-ORD            PIC 9.
       01 MARCA-L            PIC 9(3).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 SAL-I              PIC 9(6)V999.
       01 SAL-I-R REDEFINES SAL-I.
          02 SAL-1-I       PIC 9(6).
          02 SAL-2-I       PIC 999.
       01 SAL-1-EDI       PIC ZZZZZ9.
       01 SAL-2-EDI       PIC 999.
       01 COD-SAL         PIC X.
       01 SAL-S          PIC S999999V999.
       01 SAL-S-R REDEFINES SAL-S.
          02 SAL-1-S     PIC 9(6).
          02 SAL-2-S     PIC 999.
          02 COD-S       PIC X.
   
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 PRE-EDIT              PIC Z.ZZZ.ZZZ.ZZ9,99.
       01 DATOS.
          02 DATO     PIC X(71) OCCURS 10 TIMES.
       01 COD                   PIC X.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART02.ERR".
       COPY "ART01.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "EMPRE.ERR".
       COPY "LINEAS.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
        
           PERFORM CONF-CAMPOS.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           OPEN INPUT MARCAS.
           OPEN INPUT NUMERO.
           OPEN INPUT LINEAS.
           OPEN INPUT ART02.
           OPEN INPUT ART01.
       INI-MOD.
           MOVE 0 TO SW2.
       PANTA-INI.    
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-INI.
           CLOSE ART02.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
           MOVE 3 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO SIGA-INI.
       TAREAS.
           IF ESCAP CLOSE MARCAS NUMERO LINEAS ART01
               STOP RUN.
           IF F01 GO TO ACT-NUM.
           IF F02 GO TO ACT-ALF.
           IF F03 GO TO CONS-NUM.
           IF F04 GO TO CONS-ALF.
           GO TO CARGO-MOD.
       PANTA-ACTU.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "ARTICULO        :" LINE 12 POSITION  3.
           DISPLAY "PRECIO VENTA    :" LINE 14 POSITION  3.
           DISPLAY "FECHA PRECIO VTA:" LINE 16 POSITION  3.
           DISPLAY "STOCK EN SUCUR. :" LINE 22 POSITION  3.
           DISPLAY "POSICION        :" LINE 23 POSITION  3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       CONS-NUM.
           OPEN INPUT ART02.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           MOVE "X" TO COD.
           GO TO C-N-DESDE.
       ACT-NUM.
           OPEN I-O ART02.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "MODIF. NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           MOVE "A" TO COD.
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
           START ART02 KEY IS NOT < CLAVE-ART02
                         INVALID KEY GO TO C-N-DESDE.
           MOVE "N" TO COD2.
           GO TO C-SIGO-N.
       C-LEO-N.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP GO TO INI-MOD.
           IF F03 GO TO C-SIGO-N.
           GO TO F-CONS.
       C-SIGO-N.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 30.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART02.
           MOVE COD-ART02 TO COD-ART01.
           PERFORM LEO-ART01.
           IF COD3 = "F" GO TO F-CONS.
           IF EMP-ART02 > NUM-EMP GO TO F-CONS.
           IF SUC-ART02 > SUC-EMP GO TO F-CONS.
           MOVE NUM-ART02 TO COD-NUM.
           PERFORM LEO-NUM-R.
      *     IF ACT-PRI-EMP = 99999
      *           MOVE NUM-ART02 TO N-ART02
      *           COMPUTE COD-MAR = N-1-ART02 * 1
      *     ELSE 
           MOVE MAR-ART02 TO COD-MAR.
           MOVE 0 TO MAR-ART02.
           MOVE NUM-CAP TO NUM-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02 INVALID KEY
           GO TO C-SIGO-N.
           READ ART02 NEXT WITH NO LOCK AT END GO TO C-SIGO-N.
           DISPLAY NUM-ART02 LINE TAB-LIN(SW3)
           POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
           IF COD-MAR > 199 MOVE 500 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM DES-DET THRU DES-MAR.
           IF PRE-V-ART02 = 0 GO TO COSTO-S-N.
           PERFORM DES-COD THRU DES-POS.
           IF COD NOT = "A" GO TO C-LEO-N.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 PERFORM CARGA-PRE THRU CARGA-FEC-V
                  REWRITE REG-ART02 INVALID KEY GO TO C-LEO-N.
           GO TO C-LEO-N.
       COSTO-S-N.
           PERFORM DES-MAR THRU DES-COD.
           PERFORM DES-COS THRU DES-F-COS.
           PERFORM DES-L1 THRU DES-POS.
           GO TO C-LEO-N.           
       CONS-ALF.
           OPEN INPUT ART02.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           DISPLAY "DESDE:" LINE 10 POSITION 3.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           MOVE "X" TO COD.
           GO TO C-A-DESDE.
       ACT-ALF.
           OPEN I-O ART02.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "MODIF. ALFABETICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-ACTU.
           DISPLAY "DESDE:" LINE 10 POSITION 3.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02.
           MOVE "A" TO COD.
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
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY GO TO C-A-DESDE.
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
                      INVALID KEY GO TO C-A-DESDE.
           GO TO C-SIGO-A2.
       C-LEO-A.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP GO TO INI-MOD.
           IF F03 GO TO C-SIGO-A.
           GO TO F-CONS.
       C-SIGO-A2.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 30.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-ART02.
           IF EMP-ART02 > NUM-EMP GO TO F-CONS.
           IF SUC-ART02 > SUC-EMP GO TO F-CONS.
           IF NUM-ART02 NOT = COD-NUM
                   GO TO C-SIGO-A.
           MOVE COD-ART02 TO COD-ART01.
           PERFORM LEO-ART01.
           PERFORM DES-DET.
      ***     IF ACT-PRI-EMP = 99999
      ***           MOVE NUM-ART02 TO N-ART02
      ***           COMPUTE COD-MAR = N-1-ART02 * 1
      ***       ELSE 
                   MOVE MAR-ART02 TO COD-MAR.
           IF COD-MAR > 199 MOVE 500 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           IF PRE-V-ART02 = 0 GO TO COSTO-S-A.
           PERFORM DES-MAR.
           PERFORM DES-COD THRU DES-POS.
           IF COD NOT = "A" GO TO C-LEO-A.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 PERFORM CARGA-PRE THRU CARGA-FEC-V
                  REWRITE REG-ART02 INVALID KEY GO TO C-LEO-A.
           GO TO C-LEO-A.
       COSTO-S-A.
           COMPUTE COSTO-STOCK-ART01 = COSTO-STOCK-ART01 * 1156 / 1000.
           PERFORM DES-MAR THRU DES-COD.
           PERFORM DES-COS THRU DES-F-COS.
           PERFORM DES-L1 THRU DES-POS.
           GO TO C-LEO-A.
       F-CONS.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-A-DESDE.     
       LEO-ART02.
           READ ART02 NEXT AT END MOVE "F" TO COD3.
       LEO-ART01.
           READ ART01 INVALID KEY PERFORM E-ART01 50 TIMES.
       LEO-NUM.
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO INVALID KEY MOVE "NOMBRE INEX." TO DET-NUM
                                   PERFORM E-NUM 50 TIMES.
       LEO-MAR-R.
           READ MARCAS INVALID KEY MOVE "MARCA INEX." TO DET-MAR
                                   PERFORM E-MAR 50 TIMES.
       LEO-LIN-R.
           READ LINEAS INVALID KEY MOVE "****" TO DET-LIN.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 091010 TO TAB-POS(4).
           MOVE 094710 TO TAB-POS(5).
           MOVE 101020 TO TAB-POS(6).
           MOVE 074720 TO TAB-POS(7).
           MOVE 082104 TO TAB-POS(8).
           MOVE 083402 TO TAB-POS(9).
           MOVE 083920 TO TAB-POS(10).
           MOVE 122110 TO TAB-POS(11).
           MOVE 142116 TO TAB-POS(12)
           MOVE 182120 TO TAB-POS(13).
           MOVE 192120 TO TAB-POS(14).
           MOVE 202120 TO TAB-POS(15)
           MOVE 212120 TO TAB-POS(16).
           MOVE 182108 TO TAB-POS(17).
           MOVE 192106 TO TAB-POS(18).
           MOVE 202106 TO TAB-POS(19).
           MOVE 232103 TO TAB-POS(20).
           MOVE 232503 TO TAB-POS(21).
           MOVE 232903 TO TAB-POS(22).
           MOVE 212103 TO TAB-POS(23).
           MOVE 212503 TO TAB-POS(24).
           MOVE 212903 TO TAB-POS(25).
           MOVE 213303 TO TAB-POS(26).
           MOVE 213703 TO TAB-POS(27).
           MOVE 123530 TO TAB-POS(28).
           MOVE 162108 TO TAB-POS(29).
           MOVE 133520 TO TAB-POS(30).
           MOVE 235020 TO TAB-POS(31).
           MOVE 222115 TO TAB-POS(32).
       TIT-PAN.
           MOVE " CONSULTA " TO TIT-1-PAN.
           MOVE "Y MODIF. D" TO TIT-2-PAN.
           MOVE "E PRECIOS " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  MODIFIC. NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F2>  MODIFIC. ALFABET. " LINE 16 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA " LINE 17 POSITION 18.
           DISPLAY "<F4>  CONSULTA ALFABET. " LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
       DES-COS.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE COSTO-STOCK-ART01 TO DEC10-EDI.
           PERFORM DES-DEC10.
           DISPLAY " PRECIO COSTO + IVA " LINE 14 POSITION 40
                                BLINK.
       DES-F-COS.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           MOVE FEC-U-COMP-ART01 TO FEC-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE 8.            
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
                           SIZE TAB-SIZ(SW3) .
       DES-PRE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE PRE-V-ART02 TO PRE-EDIT.
           DISPLAY PRE-EDIT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3) REVERSE BLINK.
       DES-F-V.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           MOVE FEC-V-ART02 TO FEC-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE 8 .
       DES-L1.
           IF LIN-1-ART01 = 0 PERFORM ESP-L1 THRU ESP-L4
                              GO TO DES-ST.
           MOVE LIN-1-ART01 TO COD-1-LIN.
           MOVE 0 TO COD-2-LIN COD-3-LIN COD-4-LIN.
           PERFORM LEO-LIN-R.
           MOVE DET-LIN TO ALF-CAP.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DES-L2.
           IF LIN-2-ART01 = 0 PERFORM ESP-L2 THRU ESP-L4
                              GO TO DES-ST.
           MOVE LIN-2-ART01 TO COD-2-LIN.
           PERFORM LEO-LIN-R.
           MOVE DET-LIN TO ALF-CAP.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DES-L3.
           IF LIN-3-ART01 = 0 PERFORM ESP-L3 THRU ESP-L4
                              GO TO DES-ST.
           MOVE LIN-3-ART01 TO COD-3-LIN.
           PERFORM LEO-LIN-R.
           MOVE DET-LIN TO ALF-CAP.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DES-L4.
           IF LIN-4-ART01 = 0 PERFORM ESP-L4
                              GO TO DES-ST.
           MOVE LIN-4-ART01 TO COD-4-LIN.
           PERFORM LEO-LIN-R.
           MOVE DET-LIN TO ALF-CAP.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM DES-ALF.
       DES-ST.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           MOVE SAL-S-ART02 TO SAL-I SAL-S.
           IF SAL-S-ART02 < 0 MOVE "-" TO COD-SAL
               ELSE MOVE SPACES TO COD-SAL.
           MOVE SAL-1-I TO SAL-1-EDI.
           DISPLAY SAL-1-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE 6 .
           DISPLAY COD-S LINE TAB-LIN(SW3) POSITION 32 SIZE 1.
           IF SAL-2-I = 0 GO TO DES-P1.
           MOVE SAL-2-I TO SAL-2-EDI.
           DISPLAY "." LINE TAB-LIN(SW3) POSITION 27 SIZE 1 .
           DISPLAY SAL-2-EDI LINE TAB-LIN(SW3) POSITION 28 SIZE 3.
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
       CARGA-PRE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           MOVE NUM-CAP TO PRE-V-ART02.
           PERFORM DES-PRE.
       CARGA-FEC-V.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           MOVE NUM-CAP TO FEC-V-ART02.
           PERFORM DES-F-V.
       ESP-L1.
           MOVE 13 TO SW3.
           PERFORM BORRA.
       ESP-L2.
           MOVE 14 TO SW3.
           PERFORM BORRA.
       ESP-L3.
           MOVE 15 TO SW3.
           PERFORM BORRA.
       ESP-L4.
           MOVE 16 TO SW3.
           PERFORM BORRA.
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
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 30 .
       E-ART01.
           MOVE 31 TO SW3.
           MOVE "ERROR ART01" TO ALF-CAP.
           PERFORM DES-ALF.
       E-NUM.
           MOVE 31 TO SW3.
           MOVE "ERROR NUMERO" TO ALF-CAP.
           PERFORM DES-ALF.
       E-MAR.
           MOVE 31 TO SW3.
           MOVE "ERROR MARCAS" TO ALF-CAP.
           PERFORM DES-ALF.
       LEA-EMPRE.
           OPEN INPUT EMPRESAS.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                     GO TO LEA-EMPRE.
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
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76 .
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
