       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSTOCK.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. FEB'97.
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
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "LINEAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       COPY "BSTOCK.SEL".
       COPY "PARTI.SEL".
       COPY "ORDEN.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "EMPREBM.REG".
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "LINEAS.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       COPY "PARTI.REG".
       COPY "ORDEN.REG".
       COPY "BSTOCK.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 18.
          02 IMP-10.
             03 FILLER       PIC X(7) VALUE "MARCA: ".
             03 BB           PIC 99 COMP-4 VALUE 14.
             03 DET-MAR-ED       PIC X(20).
             03 CC           PIC 99 COMP-4 VALUE 20.
          02 IMP-11.
             03 FILLER       PIC X(12) VALUE "NUMERO MARCA".
             03 FILLER       PIC X(16) VALUE " ------ DETALLE ".
             03 FILLER       PIC X(5) VALUE "---- ".
             03 FILLER       PIC X(12) VALUE "---MOV.ST.- ".
             03 FILLER       PIC X(12) VALUE "--IMPORTE-- ".
             03 FILLER       PIC X(8) VALUE " PARTIDA".
             03 FILLER       PIC X(9) VALUE " OBSERV.".
          02 IMP-112.
             03 FILLER       PIC X(10) VALUE "<<< FECHA:".
             03 DD-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MM-ED        PIC 99.
             03 FILLER       PIC XXX VALUE "/19".
             03 AA-ED        PIC 99.
             03 FILLER       PIC X(4) VALUE " >>>".

          02 IMP-1.
             03 FILLER       PIC X(14) VALUE "NUMERO MARCA".
             03 FILLER       PIC X(14) VALUE "--------- DETA".
             03 FILLER       PIC X(14) VALUE "LLE --------- ".
             03 FILLER       PIC X(7) VALUE "ST.MIN ".
             03 FILLER       PIC X(12) VALUE "---ST.ACT-- ".
             03 FILLER       PIC X(12) VALUE "---DIFER.-- ".
             03 FILLER       PIC X(9) VALUE " OBSERV. ".
          02 IMP-12.
             03 FILLER       PIC X(14) VALUE "NUMERO MARCA".
             03 FILLER       PIC X(14) VALUE "--------- DETA".
             03 FILLER       PIC X(14) VALUE "LLE --------- ".
             03 FILLER       PIC X(12) VALUE "---ST.PEN-- ".
             03 FILLER       PIC X(12) VALUE "---ST.ACT-- ".
             03 FILLER       PIC X(12) VALUE "---DIFER.-- ".
             03 FILLER       PIC X(9) VALUE " OBSERV. ".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(10) VALUE " ARTICULOS".
          02 IMP-21.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L1-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(19) VALUE " ACTUALIZACIONES ".
             03 FILLER       PIC X(6) VALUE "POR $ ".
             03 TOT-IMP-ED   PIC ---.--9,99.
          02 IMP-3.
             03 NUM-ED       PIC X(6)B.
             03 MAR-ED       PIC 9999B.
             03 DET-ED       PIC X(27)B.
             03 MIN-ED       PIC ZZZZZ9B.
             03 STO-ED       PIC ------9,999B.
             03 RES-ED       PIC ------9,999B.
             03 OBS-ED       PIC X(9).
          02 IMP-31.
             03 NUM-1-ED     PIC X(8)B.
             03 MAR-1-ED     PIC 9999B.
             03 DET-1-ED     PIC X(23)B.
             03 STO-1-ED     PIC ------9,999B.
             03 MOV-1-ED     PIC ------9,999B.
             03 RES-1-ED     PIC ------9,999B.
             03 OBS-1-ED     PIC X(8).
          02 IMP-32.
             03 NUM-2-ED     PIC X(8)B.
             03 MAR-2-ED     PIC 9999B.
             03 DET-2-ED     PIC X(27)B.
             03 PEN-2-ED     PIC ------9,999B.
             03 STO-2-ED     PIC ------9,999B.
             03 RES-2-ED     PIC ------9,999B.
             03 OBS-2-ED     PIC X(8).
          02 IMP-41.
             03 NUM-41-ED     PIC X(6)B.
             03 MAR-41-ED     PIC 9999BB.
             03 DET-41-ED     PIC X(19)B.
             03 MOV-41-ED     PIC ------9,999B.
             03 IMP-41-ED     PIC ----.--9,99B.
             03 PAR-41-ED     PIC 9(8)B.
             03 OBS-41-ED     PIC X(8).
          02 IMP-5.
             03 FILLER        PIC X(12) VALUE "- RUBRO --".
             03 FILLER        PIC X(19) VALUE "-- DESCRIPCION --".
             03 FILLER        PIC X(15) VALUE "- DEBITO -".
             03 FILLER        PIC X(15) VALUE "- CREDITO-".
          02 IMP-51.
             03 RUB-D-BS-ED   PIC Z(7).
             03 FILLER        PIC X(5) VALUE SPACES.
             03 DES-D-BS-ED   PIC X(17)BB.
             03 DEB-D-BS-ED   PIC ---.--9,99.
          02 IMP-52.
             03 FILLER        PIC X(3) VALUE "a".
             03 RUB-C-BS-ED   PIC Z(7)BB.
             03 DES-C-BS-ED   PIC X(17)BB.
             03 FILLER        PIC X(15) VALUE SPACES.
             03 CRE-C-BS-ED   PIC ---.--9,99.
          02 IMP-53.
             03 FILLER        PIC X(14) VALUE "  TOTALES --> ".
             03 FILLER        PIC X(17) VALUE SPACES.
             03 TOT-DEB-ED    PIC ---.--9,99B(5).
             03 TOT-CRE-ED    PIC ---.--9,99. 
       01 LISTO-D            PIC 9999.
       01 LISTO-H            PIC 9999.
       01 MARCA-L            PIC 9999.
       01 LIN-1-CAP          PIC 999 VALUE 0.
       01 LIN-2-CAP          PIC 999 VALUE 0.
       01 LIN-3-CAP          PIC 999 VALUE 0.
       01 LIN-4-CAP          PIC 999 VALUE 0.
       01 LISTO-D-A          PIC X(10).
       01 REP                PIC 9.
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 TOT-DEB-CAP        PIC S9(6)V99 VALUE 0.
       01 TOT-CRE-CAP        PIC S9(6)V99 VALUE 0.
       01 COMEN-CAP          PIC X(29) VALUE SPACES.
       01 CANT-CAP           PIC S9(6)V999 VALUE 0.
       01 PAR-CAP            PIC 9(8) VALUE 0.
       01 IMP-CAP            PIC S9(6)V99 VALUE 0.
       01 MOVI-CAP           PIC S9(6)V999.
       01 STOC-CAP           PIC S9(6)V999.
       01 REST-CAP           PIC S9(6)V999.
       01 TOT-IMP-CAP        PIC S9(6)V99.
       01 PEN-CAP            PIC S9(6)V999.
       01 MINI-CAP           PIC S9(6).
       01 STOC-EDI           PIC ------9,999.
       01 REST-EDI           PIC ------9,999.
       01 MOVI-EDI           PIC ------9,999.
       01 PEN-EDI            PIC ------9.
       01 RUB-EDI            PIC Z(7).
       01 TAREA              PIC X VALUE SPACES.
       01 NUM-MAR.
          02 N-ART02         PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02    PIC 9(3).
             03 N-2-ART02    PIC 9(3).
       01 FEC-CAP.
          03 DD-CAP      PIC 99.
          03 MM-CAP      PIC 99.
          03 AA-CAP      PIC 99.
       01 FEC-EDI.
          03 DD-EDI      PIC 99/.
          03 MM-EDI      PIC 99/.
          03 AA-EDI      PIC 99.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "EMPRE.ERR".
       COPY "ART02.ERR".
       COPY "LINEAS.ERR".
       COPY "NUMERO.ERR".
       COPY "MARCAS.ERR".
       COPY "ART01.ERR".
       COPY "PARTI.ERR".
       COPY "BSTOCK.ERR".
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
           MOVE 3 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F02 GO TO CONSU.
           GO TO CARGO-MOD.
       ACTU.
           MOVE "V" TO TAREA.
           GO TO SIGA.
       CONSU.
           MOVE "C" TO TAREA.
       SIGA.       
           OPEN INPUT ART02.
           OPEN INPUT MARCAS
                INPUT NUMERO
                OUTPUT IMP01.
           MOVE " MOD.STOCK P/VENTAS " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
           DISPLAY "                    " LINE 10 POSITION 30 REVERSE.
           DISPLAY " PRECAUCION:        " LINE 11 POSITION 30 
                                                     REVERSE BLINK. 
           DISPLAY "    CONECTE  LA     " LINE 12 POSITION 30
                                                     REVERSE.
           DISPLAY "     IMPRESORA      " LINE 13 POSITION 30 REVERSE.
           DISPLAY "                    " LINE 14 POSITION 30 REVERSE.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 GO TO SIGA-ACTU.
           IF ESCAP GO TO INI-MOD.
           GO TO CARGO-MOD.
       SIGA-ACTU.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-MOD
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE 0 TO CONT-P CONT-I.
           PERFORM TITU-1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           MOVE 0 TO TOT-IMP-CAP.
       CARGA-COD.
           PERFORM BORRA-CAP.
           MOVE 8 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP 
                     PERFORM PIE-P-1 THRU PIE-P-1-F
                     PERFORM CIERRA-A CLOSE IMP01 GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-COD.
           MOVE NUM-CAP TO COD-ART02.
           IF NUM-ART02 = 0 MOVE MAR-ART02 TO NUM-ART02
                            MOVE 0 TO MAR-ART02.
           START ART02 KEY NOT LESS CLAVE-ART02 INVALID KEY 
                                          GO TO CARGA-COD.
           READ ART02 NEXT AT END GO TO CARGA-COD.

      *     IF ACT-PRI-EMP = 99999
      *          MOVE NUM-ART02 TO N-ART02
      *          MOVE N-1-ART02 TO COD-MAR
      *          MOVE NUM-ART02 TO COD-NUM
      *       ELSE MOVE NUM-ART02 TO COD-NUM
      *            MOVE MAR-ART02 TO COD-MAR.
           MOVE EMP-ART02 TO EMP-ART01.
           MOVE NUM-ART02 TO COD-NUM NUM-ART01.
           MOVE MAR-ART02 TO COD-MAR MAR-ART01.
           READ NUMERO WITH NO LOCK INVALID KEY 
                       MOVE "*******" TO DET-NUM.
           READ MARCAS WITH NO LOCK INVALID KEY
                       MOVE "*******" TO DET-MAR.
           PERFORM DESP-DES THRU DESP-MAR.
           OPEN INPUT ART01.
           READ ART01 INVALID KEY 
                MOVE " ERROR ART01 " TO COMEN-CAP
                PERFORM DESP-COMEN.
           CLOSE ART01.
           MOVE "DIGITE <F9> MOD" TO MEN-1.
           MOVE "IFICA STOCK    " TO MEN-2.
           MOVE "               " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP 
                           PERFORM PIE-P-1 THRU PIE-P-1-F 
                           CLOSE IMP01 
                           UNLOCK ART02 RECORD
                           PERFORM CIERRA-A GO TO INI-MOD.
           IF F09 GO TO VENTA.
           UNLOCK ART02 RECORD
           GO TO CARGA-COD.
       VENTA.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT MOVI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3) TAB CONVERT
                           ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM PIE-P-1 THRU PIE-P-1-F
                    UNLOCK ART02 RECORD
                    PERFORM CIERRA-A CLOSE IMP01 GO TO INI-MOD.
           MOVE MOVI-CAP TO MOVI-EDI.
           DISPLAY MOVI-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       B-PARTI.
           IF REGI-ART01 = "P" OR REGI-ART01 = "p"
                   GO TO B-PARTI1
              ELSE MOVE "SIN PAR." TO OBS-41-ED
                   GO TO B-PARTI8.
       B-PARTI1.
           MOVE EMP-ART02 TO EMP-PARTI.
           MOVE SUC-ART02 TO SUC-PARTI.
           MOVE COD-ART02 TO COD-PARTI.
           MOVE 0 TO PAR-PARTI.
           OPEN INPUT PARTI.
           START PARTI KEY IS NOT < CLAVE-PARTI
                  INVALID KEY GO TO B-PARTI9.
       B-PARTI2.
           READ PARTI NEXT AT END 
                      GO TO B-PARTI9.
           IF EMP-PARTI NOT = EMP-ART02
                         GO TO B-PARTI9.
           IF SUC-PARTI NOT = SUC-ART02
                         GO TO B-PARTI9.
           IF COD-PARTI NOT = COD-ART02
                         GO TO B-PARTI9.
           IF SAL-S-PARTI > 0
               IF SAL-S-PARTI NOT > BAJ-S-PARTI
                    GO TO B-PARTI2.      
       B-PARTI3.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           DISPLAY PAR-PARTI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE PAR-PARTI TO PAR-CAP.
           COMPUTE STOC-CAP = SAL-S-PARTI - BAJ-S-PARTI.
           PERFORM DESP-STO.
           COMPUTE SAL-S-ART02 = SAL-S-ART02 - MOVI-CAP.
           IF MOVI-CAP < STOC-CAP OR MOVI-CAP = STOC-CAP
              MOVE MOVI-CAP TO CANT-CAP
              COMPUTE BAJ-S-PARTI = BAJ-S-PARTI + MOVI-CAP
              COMPUTE IMP-CAP ROUNDED = CANT-CAP * PRE-C-PARTI
              MOVE 0 TO REST-CAP
              PERFORM DESP-ACT
              GO TO  B-PARTI31.
           COMPUTE CANT-CAP = MOVI-CAP - STOC-CAP.
           COMPUTE MOVI-CAP = MOVI-CAP - CANT-CAP.
           COMPUTE BAJ-S-PARTI = BAJ-S-PARTI + CANT-CAP.
           COMPUTE REST-CAP = MOVI-CAP.
           COMPUTE IMP-CAP ROUNDED = CANT-CAP * PRE-C-PARTI.
           PERFORM DESP-ACT.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO OBS-41-ED.
           IF F01 PERFORM IMPRE-1
                  CLOSE PARTI
                  OPEN I-O PARTI
                  REWRITE REG-PARTI
                  CLOSE PARTI
                  OPEN INPUT PARTI
                  PERFORM ART THRU ART-F
                  PERFORM GRAVO-B THRU GRAVO-B-F.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           GO TO B-PARTI2.
       B-PARTI31.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO OBS-41-ED.
           IF F01 PERFORM IMPRE-1
                  CLOSE PARTI
                  OPEN I-O PARTI
                  REWRITE REG-PARTI
                  CLOSE PARTI
                  OPEN INPUT PARTI
                  PERFORM ART THRU ART-F
                  PERFORM GRAVO-B THRU GRAVO-B-F.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           CLOSE PARTI.
           GO TO CARGA-COD.
       B-PARTI8.
           MOVE " ART.SIN PARTIDA " TO COMEN-CAP
           PERFORM DESP-COMEN.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           COMPUTE SAL-S-ART02 = SAL-S-ART02 - MOVI-CAP.
           MOVE MOVI-CAP TO CANT-CAP.
           MOVE 0 TO REST-CAP.
           COMPUTE IMP-CAP ROUNDED = CANT-CAP * COSTO-STOCK-ART01.  
           IF F01 PERFORM IMPRE-1
                  PERFORM ART THRU ART-F
                  PERFORM GRAVO-B THRU GRAVO-B-F.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           GO TO CARGA-COD.
       B-PARTI9.
           MOVE " NO EXISTE PARTIDA - VERIF. " TO COMEN-CAP.
           PERFORM DESP-COMEN.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  GO TO CARGA-COD.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           GO TO CARGA-COD.
       GRAVO-B.
           MOVE EMP-ART02 TO EMP-BS.
           MOVE SUC-ART02 TO SUC-BS.
           MOVE AA-CAP TO AA-BS.
           MOVE MM-CAP TO MM-BS.
           MOVE DD-CAP TO DD-BS.
           MOVE LIN-1-ART01 TO COD-1-LIN.
           MOVE LIN-2-ART01 TO COD-2-LIN.
           MOVE LIN-3-ART01 TO COD-3-LIN.
           MOVE LIN-4-ART01 TO COD-4-LIN.
           OPEN INPUT LINEAS.
           READ LINEAS INVALID KEY MOVE 0 TO RUB-3-LIN RUB-2-LIN.
           IF TAREA = "V"
               MOVE RUB-3-LIN TO RUB-BS RUB-EDI
               ELSE MOVE RUB-2-LIN TO RUB-BS RUB-EDI.
           PERFORM DESP-RUB.
           IF TAREA = "V" GO TO GRAVO-V1.
           DISPLAY " - ENTER - MODIFICA RUBRO " LINE 22 POSITION 30
                         BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           DISPLAY SPACES LINE 22 POSITION 30 SIZE 30.
           IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
                  PERFORM MENS-PAN
                  GO TO GRAVO-V1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           PERFORM CARGO-RUB.           
       GRAVO-V1. 
           CLOSE LINEAS.
           MOVE 1 TO CONDI-BS.
           OPEN I-O BSTOCK.
           READ BSTOCK INVALID KEY MOVE 0 TO IMP-BS.
           COMPUTE IMP-BS = IMP-BS + IMP-CAP.
           REWRITE REG-BSTOCK INVALID KEY WRITE REG-BSTOCK.
           MOVE RUB-1-LIN TO RUB-BS.
           MOVE 2 TO CONDI-BS.
           READ BSTOCK INVALID KEY MOVE 0 TO IMP-BS.
           COMPUTE IMP-BS = IMP-BS + IMP-CAP.
           REWRITE REG-BSTOCK INVALID KEY WRITE REG-BSTOCK.
           CLOSE BSTOCK.
       GRAVO-B-F.
           EXIT.
       ART.
           IF TAREA = "V" GO TO ART-F.
           CLOSE ART02.
           OPEN I-O ART02.
           REWRITE REG-ART02 INVALID KEY WRITE REG-ART02.
           CLOSE ART02.
           OPEN INPUT ART02.
       ART-F.
           EXIT.    
       IMPRE-1.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU-1.
           MOVE DET-NUM TO DET-41-ED.
           MOVE NUM-ART02 TO NUM-41-ED.
           MOVE MAR-ART02 TO MAR-41-ED.
           MOVE CANT-CAP TO MOV-41-ED.
           MOVE PAR-CAP TO PAR-41-ED.
           MOVE IMP-CAP TO IMP-41-ED.
           COMPUTE TOT-IMP-CAP = IMP-CAP + TOT-IMP-CAP.
           IF REST-CAP < 0 MOVE "VERIFIQ." TO OBS-41-ED
              ELSE MOVE SPACES TO OBS-41-ED.
           WRITE LIN-IMP01 FROM IMP-41 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
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
           MOVE DET-MAR TO DET-MAR-ED
           WRITE LIN-IMP01 FROM NORMAL AFTER 0.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 10 TO CONT-R.
       TITU-1.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           IF TAREA = "V"
             MOVE "<<< MODIFI" TO TIT-2-EDI
             MOVE "CACION DE " TO TIT-3-EDI
             MOVE "STOCK P/VE" TO TIT-4-EDI
             MOVE "NTAS >>>  " TO TIT-5-EDI
             MOVE TITULO-EDI TO TIT-E80
             MOVE "Por Ventas Vs." TO DES-D-BS-ED DES-C-BS-ED
             ELSE MOVE "<<< MODIFI" TO TIT-2-EDI
                  MOVE "CACION DE " TO TIT-3-EDI
                  MOVE "STOCK P/CO" TO TIT-4-EDI
                  MOVE "NSUMO >>> " TO TIT-5-EDI
                  MOVE TITULO-EDI TO TIT-E80
                  MOVE "Por Consumo" TO DES-D-BS-ED DES-C-BS-ED.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E80.
           MOVE ANO TO AA-E80.
           MOVE MES TO MM-E80.
           MOVE DIA TO DD-E80.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E80.
           MOVE MINU TO MI-E80.
           MOVE DD-CAP TO DD-ED.
           MOVE MM-CAP TO MM-ED.
           MOVE AA-CAP TO AA-ED.
           WRITE LIN-IMP01 FROM NORMAL AFTER 0.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-112 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-11 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 9 TO CONT-R. 
       ESPA.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP01 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P-1.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L1-ED.
           MOVE TOT-IMP-CAP TO TOT-IMP-ED.
           WRITE LIN-IMP01 FROM IMP-21 AFTER 1.
           MOVE 0 TO TOT-IMP-CAP.
           ADD 1 TO CONT-R.
       PIE-11.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES.
           PERFORM ESPA.
           WRITE LIN-IMP01 FROM IMP-5 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO RUB-BS CONDI-BS FEC-BS TOT-DEB-CAP TOT-CRE-CAP.
           OPEN INPUT BSTOCK.
           START BSTOCK KEY IS NOT < CLAVE-BSTOCK
                  INVALID KEY CLOSE BSTOCK
                              PERFORM CIERRA-A
                              GO TO INI-MOD.
       PIE-12.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          WRITE LIN-IMP01 FROM IMP-5 AFTER 1
                          ADD 1 TO CONT-R.
           READ BSTOCK NEXT AT END GO TO PIE-15.
           IF CONDI-BS = 1
                  GO TO PIE-13.
           GO TO PIE-14.
       PIE-13.
           MOVE RUB-BS TO RUB-D-BS-ED.
           MOVE IMP-BS TO DEB-D-BS-ED.
           COMPUTE TOT-DEB-CAP = TOT-DEB-CAP + IMP-BS.
           WRITE LIN-IMP01 FROM IMP-51 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO PIE-12.
       PIE-14.
           MOVE RUB-BS TO RUB-C-BS-ED.
           MOVE IMP-BS TO CRE-C-BS-ED.
           COMPUTE TOT-CRE-CAP = TOT-CRE-CAP + IMP-BS.
           WRITE LIN-IMP01 FROM IMP-52 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO PIE-12.
       PIE-15.
           MOVE TOT-DEB-CAP TO TOT-DEB-ED.
           MOVE TOT-CRE-CAP TO TOT-CRE-ED.
           WRITE LIN-IMP01 FROM IMP-53 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO TOT-DEB-CAP TOT-CRE-CAP.
           CLOSE BSTOCK.
           OPEN OUTPUT BSTOCK.
           CLOSE BSTOCK.
       PIE-P-1-F.
           EXIT. 
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 081502 TO TAB-POS(4).
           MOVE 082620 TO TAB-POS(5).
           MOVE 082302 TO TAB-POS(6).
           MOVE 082620 TO TAB-POS(7).
           MOVE 121510 TO TAB-POS(8).
           MOVE 122730 TO TAB-POS(9).
           MOVE 125915 TO TAB-POS(10).
           MOVE 181511 TO TAB-POS(11).
           MOVE 161511 TO TAB-POS(12)
           MOVE 201511 TO TAB-POS(13).
           MOVE 121511 TO TAB-POS(14).
           MOVE 141511 TO TAB-POS(15)
           MOVE 161511 TO TAB-POS(16).
           MOVE 101504 TO TAB-POS(17).
           MOVE 104504 TO TAB-POS(18).
           MOVE 102003 TO TAB-POS(19).
           MOVE 102503 TO TAB-POS(20).
           MOVE 103003 TO TAB-POS(21).
           MOVE 103503 TO TAB-POS(22).
           MOVE 141508 TO TAB-POS(23).
           MOVE 143020 TO TAB-POS(24).
           MOVE 101508 TO TAB-POS(25).
           MOVE 221507 TO TAB-POS(26).
       TIT-PAN.
           MOVE "  -ARCHIVO" TO TIT-1-PAN.
           MOVE " DE ARTICU" TO TIT-2-PAN.
           MOVE "LOS-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  MODIF.STOCK P/VENTAS" LINE 14 POSITION 18.
           DISPLAY "<F2>  MODIF.STOCK P/USO" LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       DESP-DES.
           MOVE 9 TO SW3.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE   TAB-SIZ(SW3).
       DESP-MAR.
           MOVE 10 TO SW3.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-STO.
           MOVE 11 TO SW3.
           MOVE STOC-CAP TO STOC-EDI.
           DISPLAY STOC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-ACT.
           MOVE 13 TO SW3.
           MOVE REST-CAP TO REST-EDI.
           DISPLAY REST-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-COMEN.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           DISPLAY COMEN-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) BLINK REVERSE.           
       DESP-RUB.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           DISPLAY RUB-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       PANTA-MOD.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "FECHA BAJA :" LINE 10 POSITION 3.
           DISPLAY "NRO.ARTIC. :" LINE 12 POSITION 3.
           DISPLAY "PARTIDA    :" LINE 14 POSITION 3.
           DISPLAY "STOCK ART. :" LINE 18 POSITION 3.
           DISPLAY "MODIF.STOCK:" LINE 16 POSITION 3.
           DISPLAY "RESTA MODIF:" LINE 20 POSITION 3.
           DISPLAY "RUBRO DEB. :" LINE 22 POSITION 3.           
       CARGO-EMP.
           OPEN INPUT EMPRESAS.
       CARGO-EMP-1.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRA-A CLOSE IMP01 EMPRESAS
                         GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGO-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-ART02 EMP-ART01 MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO CARGO-EMP-1.
           OPEN INPUT MANEJO
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE MANEJO
                GO TO CARGO-EMP-1.
           CLOSE MANEJO.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E80.
           MOVE 5 TO SW3.
           PERFORM DES-ALF.
           PERFORM MEN-ANTE.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           MOVE NUM-CAP TO SUC-ART02. 
           IF ESCAP PERFORM CIERRA-A CLOSE IMP01 EMPRESAS 
                            GO TO INI-MOD.
           IF F02 GO TO CARGO-EMP-1.
           IF NUM-CAP = 0 MOVE SPACES TO SUC-E80
                          MOVE 0 TO NUM-SUC-E80
                          CLOSE EMPRESAS
                          GO TO CARGO-EMP-F
             ELSE MOVE NUM-CAP TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGO-EMP.
           MOVE RAZ-SOC-EMP TO ALF-CAP SUC-E80.
           MOVE SUC-EMP TO NUM-SUC-E80.
           MOVE 5 TO SW3.
           PERFORM DES-ALF.
           CLOSE EMPRESAS.
       CARGO-FEC.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   NO BEEP TAB CONVERT
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRA-A CLOSE IMP01 
                    GO TO INI-MOD.
           IF F02 GO TO CARGO-EMP-1.
           MOVE DD-CAP TO DD-EDI.
           MOVE MM-CAP TO MM-EDI.
           MOVE AA-CAP TO AA-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
       CARGO-EMP-F.
           EXIT.
       CARGO-RUB.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           ACCEPT RUB-BS LINE 22 POSITION 15 SIZE 7
                        NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP 
                           PERFORM PIE-P-1 THRU PIE-P-1-F 
                           CLOSE IMP01 
                           UNLOCK ART02 RECORD
                           PERFORM CIERRA-A GO TO INI-MOD.
           MOVE RUB-BS TO RUB-EDI.
           PERFORM DESP-RUB.
       CIERRA-A.
           CLOSE MARCAS ART02 NUMERO.
       CIERRE-A1.
           CLOSE IMP01 ART02 ART01 NUMERO MARCAS.   
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-CAP.
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
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE 26 TO SW3.
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
