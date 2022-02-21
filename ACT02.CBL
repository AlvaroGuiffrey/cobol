       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACT02.
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
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "ART02C.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "ART02C.REG".
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
       01 R-LEI              PIC 9(6) VALUE 0.
       01 R-NUE              PIC 9(6) VALUE 0.
       01 R-ACT              PIC 9(6) VALUE 0.
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
       COPY "ART02C.ERR".
       COPY "ART01.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "EMPRE.ERR".
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
           IF ESCAP STOP RUN.
           IF F01 GO TO ACTU.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ACT.ARCH.VENTA " TO D20-CAP.
           PERFORM MODA-PAN.
       PANTA-ACTU.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "REGISTROS LEIDOS  :" LINE 12 POSITION 3.
           DISPLAY "REGISTROS NUEVOS  :" LINE 14 POSITION 3.
           DISPLAY "REGISTROS ACTUAL. :" LINE 16 POSITION 3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       SIGA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART02C.
           MOVE SUC-EMP TO SUC-ART02.
           DISPLAY "VOY".
           OPEN I-O ART02 ART02C.
           DISPLAY "PASE".
           MOVE 0 TO R-LEI R-NUE R-ACT.
           MOVE 0 TO NUM-ART02C MAR-ART02C SUC-ART02C.
           START ART02C KEY IS NOT < CLAVE-ART02C
                       INVALID KEY PERFORM C-ACTU
                                   STOP RUN.
       SIGA-A1.
           READ ART02C NEXT AT END PERFORM C-ACTU
                                  STOP RUN.
           ADD 1 TO R-LEI.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY R-LEI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE NUM-ART02C TO NUM-ART02.
           MOVE MAR-ART02C TO MAR-ART02.
           READ ART02 INVALID KEY GO TO ALTA-A1.
           MOVE PRE-V-ART02C TO PRE-V-ART02.
           MOVE FEC-V-ART02C TO FEC-V-ART02.
           MOVE PRE-E-ART02C TO PRE-E-ART02.
           MOVE FEC-E-ART02C TO FEC-E-ART02.
           MOVE 3 TO SUC-ART02.
           REWRITE REG-ART02 INVALID KEY GO TO ALTA-A1.
           ADD 1 TO R-ACT.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           DISPLAY R-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
           GO TO SIGA-A1.
       ALTA-A1.
           MOVE 0 TO UNI-V-ART02 VAL-V-ART02 COS-V-ART02.
           MOVE 0 TO SAL-S-ART02 MIN-S-ART02 ENT-S-ART02.
           MOVE 0 TO COND-ART02(1) COND-ART02(2) COND-ART02(3).
           MOVE 0 TO COND-ART02(4) COND-ART02(5).
           MOVE PRE-V-ART02C TO PRE-V-ART02.
           MOVE FEC-V-ART02C TO FEC-V-ART02.
           MOVE PRE-E-ART02C TO PRE-E-ART02.
           MOVE FEC-E-ART02C TO FEC-E-ART02.
           MOVE SPACES TO SEC-1-ART02 SEC-2-ART02 SEC-3-ART02.
           MOVE 3 TO SUC-ART02.
           WRITE REG-ART02 INVALID KEY DISPLAY "ERROR111"
                                       PERFORM C-ACTU
                                       STOP RUN.
           ADD 1 TO R-ACT.
           ADD 1 TO R-NUE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY R-NUE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
           MOVE 13 TO SW3. 
           PERFORM BORRA.
           DISPLAY R-ACT LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
           GO TO SIGA-A1.
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
           MOVE 122306 TO TAB-POS(11).
           MOVE 142306 TO TAB-POS(12)
           MOVE 162306 TO TAB-POS(13).
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
           DISPLAY "<F1>  ACT.ARCH.VENTAS" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 15 POSITION 18.
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
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.
       C-ACTU.     
           CLOSE ART02 ART02C.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
