       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTPOR.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. SET'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "MARCAS.SEL".
       COPY "EMPRE.SEL".
       COPY "PANTA.SEL".
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "LINEAS.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "LINEAS.REG".
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "MARCAS.REG".
       COPY "EMPREBM.REG".
       COPY "PANTA.REG".
       COPY "MANEJO.REG".
       WORKING-STORAGE SECTION.
       01 LINEA1-D           PIC 999.
       01 LINEA2-D           PIC 999.
       01 LINEA3-D           PIC 999.
       01 LINEA4-D           PIC 999.
       01 MARCA-D            PIC 9999.
       01 INDICE-D           PIC 9(4)V99.
       01 FECHA-D            PIC 9(6).
       01 DD-E80             PIC 99.
       01 MM-E80             PIC 99.
       01 AA-E80             PIC 99.
       01 HH-E80             PIC 99.
       01 MI-E80             PIC 99.
       01 NUM-MAR.
          02 N-ART02         PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02    PIC 9(3).
             03 N-2-ART02    PIC 9(3).
       01 PRECIO.
          02 N-PRECIO        PIC 9(10)V99.
          02 NU-PRECIO REDEFINES N-PRECIO.
             03 N-1-PRECIO   PIC 9(10).
             03 N-2-PRECIO   PIC 99. 
       01 PRECIO1.
          02 1-PRECIO        PIC 9(10)V99.
          02 N1-PRECIO REDEFINES 1-PRECIO.
             03 N1-1-PRECIO  PIC 9(8).
             03 N1-2-PRECIO  PIC 9(2).
             03 N1-3-PRECIO  PIC 99.
       01 LISTO-D.
          02 LISTO-D1        PIC XXXXXX.
          02 LISTO-D2        PIC 99.
       01 LISTO-H.
          02 LISTO-H1        PIC XXXXXX.
          02 LISTO-H2        PIC 99.
       01 IND-ORD            PIC 9.
       01 ACT                PIC 9.
       01 REDO               PIC 9.
       01 TIP-POR            PIC 9.
       01 CONT-1             PIC 9(5).
       01 CONT-2             PIC 9(5).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "MARCAS.ERR".
       COPY "ART01.ERR".
       COPY "ART02.ERR".
       COPY "LINEAS.ERR".
       COPY "MANEJO.ERR".
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
           MOVE 3 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP STOP RUN.
           IF F01 MOVE 0 TO ACT GO TO ACTU.
           GO TO CARGO-MOD.
       ACTU.
           IF ACT = 0
             MOVE " ACTUAL. PORCENTAJES " TO D20-CAP.
           IF ACT = 9
             MOVE " ACTUAL. PRECIOS EN U$S " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA      :" LINE 8 POSITION 5.  
           DISPLAY "- SUC. :" LINE 8 POSITION 25.
           DISPLAY "REDONDEO :" LINE 10 POSITION 5.
           DISPLAY "0-CON CENTAVOS" LINE 9 POSITION 25.
           DISPLAY "1-SIN CENTAVOS" LINE 10 POSITION 25.
           DISPLAY "2-EN CENTENAS" LINE 11 POSITION 25.
           DISPLAY "- % DE MARGEN : " LINE 10 POSITION 40.
           DISPLAY "- 1 <+>y 2 <->: " LINE 11 POSITION 40.
           DISPLAY "MARCA        :" LINE 12 POSITION 5.
           DISPLAY "LINEA -1-    :" LINE 14 POSITION 5.
           DISPLAY "LINEA -2-    :" LINE 16 POSITION 5.
           DISPLAY "LINEA -3-    :" LINE 18 POSITION 5.
           DISPLAY "LINEA -4-    :" LINE 20 POSITION 5.
           DISPLAY "FECHA DE ACT.:" LINE 21 POSITION 5.
           DISPLAY "REGISTROS LEIDOS:" LINE 22 POSITION 5.
           DISPLAY "ACTUALIZADOS: " LINE 22 POSITION 30.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           OPEN I-O ART02.
           OPEN INPUT MARCAS
                I-O ART01
                INPUT LINEAS.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           MOVE 0 TO CONT-1.
           MOVE 0 TO CONT-2.
      * CARGA-CEN.
      *     MOVE 19 TO SW3.
      *     PERFORM BORRA.
      *     ACCEPT REDO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) TAB
      *                 ON EXCEPTION EXCEP MOVE SW3 TO SW3.
      *     IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
       CARGA-IND.
           PERFORM BORRA-C.
           PERFORM MEN-ANTE.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT INDICE-D LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3) TAB CONVERT REVERSE
                           ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CEN.
      * CARGA-TIP.
      *     DISPLAY SPACES LINE 11 POSITION 56 SIZE 3.
      *     ACCEPT TIP-POR LINE 11 POSITION 56 SIZE 1 TAB CONVERT
      *                ON EXCEPTION EXCEP MOVE SW3 TO SW3.
      *     IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
      *     IF F02 GO TO CARGA-IND.
      *     IF TIP-POR < 1 OR TIP-POR > 2 GO TO CARGA-TIP.
       CARGA-MAR.
           PERFORM MEN-ANTE.
           MOVE  7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-TIP.
           IF NUM-CAP = 0 MOVE "TODAS LAS MARCAS" TO DET-MAR
                          PERFORM DESP-MAR
                          MOVE 0 TO MARCA-D
                          GO TO CARGA-LIN1.
           MOVE NUM-CAP TO COD-MAR MARCA-D.
           PERFORM LEA-MAR.
           PERFORM DESP-MAR.
       CARGA-LIN1.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAR.
           IF NUM-CAP = 0 MOVE "TODAS LAS LINEAS" TO DET-LIN
                          PERFORM DESP-LIN1
                          MOVE 0 TO LINEA1-D LINEA2-D LINEA3-D
                          MOVE 0 TO LINEA4-D
                          MOVE 0 TO COD-1-LIN COD-2-LIN COD-3-LIN
                          MOVE 0 TO COD-4-LIN
                          GO TO CARGA-LIN2.
           MOVE NUM-CAP TO COD-1-LIN LINEA1-D.
           MOVE 0 TO COD-2-LIN COD-3-LIN COD-4-LIN.
           MOVE 0 TO LINEA2-D LINEA3-D LINEA4-D.
           PERFORM LEA-LIN.
           PERFORM DESP-LIN1.
       CARGA-LIN2.
           PERFORM MEN-ANTE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIN1.
           IF NUM-CAP = 0 MOVE "TODAS LAS LINEAS" TO DET-LIN
                          PERFORM DESP-LIN2
                          GO TO CARGA-LIN3.
           MOVE NUM-CAP TO COD-2-LIN LINEA2-D.
           PERFORM LEA-LIN.
           PERFORM DESP-LIN2.
       CARGA-LIN3.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIN2.
           IF NUM-CAP = 0 MOVE "TODAS LAS LINEAS" TO DET-LIN
                          PERFORM DESP-LIN3
                          GO TO CARGA-LIN4.
           MOVE NUM-CAP TO COD-3-LIN LINEA3-D.
           PERFORM LEA-LIN.
           PERFORM DESP-LIN3.
       CARGA-LIN4.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIN3.
           IF NUM-CAP = 0 MOVE "TODAS LAS LINEAS" TO DET-LIN
                          PERFORM DESP-LIN4
                          GO TO ACT-PRE.
           MOVE NUM-CAP TO COD-4-LIN LINEA4-D.
           PERFORM LEA-LIN.
           PERFORM DESP-LIN4.
       ACT-PRE.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIN4.
           MOVE NUM-CAP TO FECHA-D
           IF FECHA-D = 0 GO TO ACT-PRE.
           IF MARCA-D = 0 MOVE 0 TO NUM-ART02 MAR-ART02
                          GO TO ACT-POS.
      *     IF ACT-PRI-EMP = 99999 MOVE MARCA-D TO N-1-ART02
      *                            MOVE 0 TO N-2-ART02
      *                            MOVE N-ART02 TO NUM-ART02
      *                            MOVE 0 TO MAR-ART02
      *                            GO TO ACT-POS.
           MOVE 0 TO NUM-ART02.
           MOVE MARCA-D TO MAR-ART02.
       ACT-POS.
           START ART02 KEY IS NOT < CLAVE-ART02
                       INVALID KEY GO TO CARGA-MAR.
       ACT-1.
           READ ART02 NEXT AT END PERFORM DESP-FIN
                                  UNLOCK ART02 RECORD
                                  GO TO CARGA-CEN.
           ADD 1 TO CONT-1.
           PERFORM DESP-C-1.
           IF EMP-ART02 > NUM-EMP PERFORM DESP-FIN GO TO CARGA-CEN.
           IF MARCA-D = 0 GO TO ACT-LIN1.
      *     IF ACT-PRI-EMP = 99999 MOVE NUM-ART02 TO N-ART02
      *                 IF N-1-ART02 > MARCA-D PERFORM DESP-FIN
      *                                      GO TO CARGA-CEN
      *                 ELSE GO TO ACT-LIN1.
           IF MAR-ART02 NOT = MARCA-D GO TO ACT-1.
       ACT-LIN1.
           IF LINEA1-D = 0 GO TO ACT-LIN2.
           MOVE COD-ART02 TO COD-ART01.
           READ ART01 WITH NO LOCK INVALID KEY GO TO ACT-1.
           IF LIN-1-ART01 NOT = LINEA1-D GO TO ACT-1.
       ACT-LIN2.
           IF LINEA2-D = 0 GO TO ACT-LIN3.
           IF LIN-2-ART01 NOT = LINEA2-D GO TO ACT-1.
       ACT-LIN3.
           IF LINEA3-D = 0 GO TO ACT-LIN4.
           IF LIN-3-ART01 NOT = LINEA3-D GO TO ACT-1.
       ACT-LIN4.
           IF LINEA4-D = 0 GO TO ACT-POR.
           IF LIN-4-ART01 NOT = LINEA4-D GO TO ACT-1.
      *-=-=-=-= ENTRO A LA ACTUALIZACION POR PORCENTAJE -=-=-=-=-=-=
       ACT-POR.
      *     IF REDO > 0 GO TO ACT-POR-S.
      *     IF ACT = 0
      *       IF TIP-POR = 1 
      *         COMPUTE PRE-V-ART02 ROUNDED = ( PRE-V-ART02 * INDICE-D /
      *                            100) + PRE-V-ART02
      *         MOVE FECHA-D TO FEC-V-ART02.
      *     IF ACT = 0
      *       IF TIP-POR = 2
      *          COMPUTE PRE-V-ART02 ROUNDED = PRE-V-ART02 - 
      *                 ( PRE-V-ART02 * INDICE-D / 100 )
      *            MOVE FECHA-D TO FEC-V-ART02.
      *     IF ACT = 9
      *       IF TIP-POR = 1
      *          COMPUTE PRE-E-ART02 ROUNDED = ( PRE-E-ART02 * INDICE-D /
      *                           100) + PRE-E-ART02
      *           MOVE FECHA-D TO FEC-E-ART02.
      *     IF ACT = 9
      *       IF TIP-POR = 2
      *          COMPUTE PRE-E-ART02 ROUNDED = PRE-E-ART02 - 
      *                 ( PRE-E-ART02 * INDICE-D / 100)
      *           MOVE FECHA-D TO FEC-E-ART02.
           MOVE INDICE-D TO PORC-ART01.
           REWRITE REG-ART01 INVALID KEY UNLOCK ART01 RECORD
                                         GO TO ACT-1.
           UNLOCK ART01 RECORD.
           ADD 1 TO CONT-2.
           PERFORM DESP-C-2.
           GO TO ACT-1.     
       ACT-POR-S.
           IF ACT = 0
             IF TIP-POR = 1
                COMPUTE N-1-PRECIO ROUNDED = ( PRE-V-ART02 * INDICE-D /
                                 100 ) + PRE-V-ART02
                MOVE 0 TO PRE-V-ART02
                COMPUTE PRE-V-ART02 = N-1-PRECIO * 1
                MOVE FECHA-D TO FEC-V-ART02.
           IF ACT = 0
             IF TIP-POR = 2
               COMPUTE N-1-PRECIO ROUNDED = PRE-V-ART02 - 
                     ( PRE-V-ART02 * INDICE-D / 100 )
                MOVE 0 TO PRE-V-ART02
                COMPUTE PRE-V-ART02 = N-1-PRECIO * 1
                MOVE FECHA-D TO FEC-V-ART02. 
           MOVE PRE-V-ART02 TO N1-PRECIO.
           IF N1-2-PRECIO > 50 
                 COMPUTE N1-1-PRECIO = N1-1-PRECIO + 1.
           IF N1-2-PRECIO < 50 MOVE 0 TO N1-2-PRECIO.
           IF REDO = 2 COMPUTE PRE-V-ART02 = N1-1-PRECIO * 100.     
           IF ACT = 9
             IF TIP-POR = 1
               COMPUTE N-1-PRECIO ROUNDED = ( PRE-E-ART02 * INDICE-D /
                                 100 ) + PRE-E-ART02
                MOVE 0 TO PRE-E-ART02
                COMPUTE PRE-E-ART02 = N-1-PRECIO * 1
                MOVE FECHA-D TO FEC-E-ART02.
           IF ACT = 9
             IF TIP-POR = 2
               COMPUTE N-1-PRECIO ROUNDED = PRE-E-ART02 - 
                    ( PRE-E-ART02 * INDICE-D / 100 )
                MOVE 0 TO PRE-E-ART02
                COMPUTE PRE-E-ART02 = N-1-PRECIO * 1
                MOVE FECHA-D TO FEC-E-ART02.
           REWRITE REG-ART02 INVALID KEY GO TO ACT-1.
           ADD 1 TO CONT-2.
           PERFORM DESP-C-2.
           GO TO ACT-1. 
       LEA-LIN.
           READ LINEAS WITH NO LOCK INVALID KEY 
                            MOVE "********" TO DET-LIN.
       LEA-MAR.
           READ MARCAS WITH NO LOCK INVALID KEY 
                            MOVE "********" TO DET-MAR.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 082004 TO TAB-POS(4).
           MOVE 083402 TO TAB-POS(5).
           MOVE 084030 TO TAB-POS(6).
           MOVE 122004 TO TAB-POS(7).
           MOVE 142003 TO TAB-POS(8).
           MOVE 162003 TO TAB-POS(9).
           MOVE 182003 TO TAB-POS(10).
           MOVE 202003 TO TAB-POS(11).
           MOVE 142520 TO TAB-POS(12)
           MOVE 162520 TO TAB-POS(13).
           MOVE 182520 TO TAB-POS(14).
           MOVE 202520 TO TAB-POS(15)
           MOVE 123030 TO TAB-POS(16).
           MOVE 222305 TO TAB-POS(17).
           MOVE 225005 TO TAB-POS(18).
           MOVE 102001 TO TAB-POS(19).
           MOVE 105610 TO TAB-POS(20).
           MOVE 212006 TO TAB-POS(21).
       TIT-PAN.
           MOVE "-ARCHIVO D" TO TIT-1-PAN.
           MOVE "E ARTICULO" TO TIT-2-PAN.
           MOVE "S -VENTA- " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  MODIF.PORCENTAJES" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       CIERRE-A.
           CLOSE MARCAS.
           CLOSE ART02.
           CLOSE ART01.
           CLOSE LINEAS.
       DESP-MAR.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-LIN1.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-LIN2.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-LIN3.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-LIN4.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-LIN LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-FIN.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE "*** FINALICE ACTUALIZACION ***" TO D20-CAP.
           DISPLAY D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-C-1.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY CONT-1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-C-2.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           DISPLAY CONT-2 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       CARGA-EMP.
           OPEN INPUT EMPRESAS MANEJO.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS MANEJO GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP EMP-ART01 EMP-ART02 MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS WITH NO LOCK INVALID KEY 
                       CLOSE EMPRESAS GO TO CARGA-EMP.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"           
                 CLOSE EMPRESAS MANEJO GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE 5 TO SW3.
           PERFORM MEN-ANTE.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP.
           READ EMPRESAS WITH NO LOCK INVALID KEY CLOSE EMPRESAS
                                      GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           CLOSE EMPRESAS.
       CARGA-EMP-F.
           EXIT.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 11 BY 1
                             UNTIL SW1 > 20.
       BORRA-C.
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
           MOVE 12 TO  SW3.
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
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
