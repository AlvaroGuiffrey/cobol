       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIMO.
       AUTHOR. OSVALDO COPIA.
       DATE-WRITTEN. ABRIL 95.
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
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       COPY "ORDEN.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "EMPREBM.REG".
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       COPY "ORDEN.REG".
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
             03 FILLER       PIC X(15) VALUE "------ DETALLE ".
             03 FILLER       PIC X(9) VALUE "-------- ".
             03 FILLER       PIC X(12) VALUE "---ST.ANT-- ".
             03 FILLER       PIC X(12) VALUE "---MOV.ST.- ".
             03 FILLER       PIC X(12) VALUE "---ST.ACT-- ".
             03 FILLER       PIC X(8) VALUE "OBSERVAC".
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
             03 FILLER       PIC X(16) VALUE " ACTUALIZACIONES".
          02 IMP-3.
             03 NUM-ED       PIC X(8)B.
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
             03 ACT-1-ED     PIC ------9,999B.
             03 OBS-1-ED     PIC X(8).
          02 IMP-32.
             03 NUM-2-ED     PIC X(8)B.
             03 MAR-2-ED     PIC 9999B.
             03 DET-2-ED     PIC X(27)B.
             03 PEN-2-ED     PIC ------9,999B.
             03 STO-2-ED     PIC ------9,999B.
             03 RES-2-ED     PIC ------9,999B.
             03 OBS-2-ED     PIC X(8).
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
       01 MOVI-CAP           PIC S9(6)V999.
       01 STOC-CAP           PIC S9(6)V999.
       01 REST-CAP           PIC S9(6)V999.
       01 PEN-CAP            PIC S9(6)V999.
       01 MINI-CAP           PIC S9(6).
       01 STOC-EDI           PIC ------9,999.
       01 REST-EDI           PIC ------9,999.
       01 PEN-EDI            PIC ------9.
       01 NUM-MAR.
          02 N-ART02         PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02    PIC 9(3).
             03 N-2-ART02    PIC 9(3).
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
       COPY "NUMERO.ERR".
       COPY "MARCAS.ERR".
       COPY "ART01.ERR".
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
           IF F05 MOVE 0 TO REP GO TO LIST-NUM.
           IF F06 MOVE 9 TO REP GO TO LIST-NUM.
           IF F07 GO TO LIST-LIN.
           IF F08 GO TO LIST-PEN.
           GO TO CARGO-MOD.
       ACTU.
           OPEN INPUT ART02.
           OPEN INPUT MARCAS
                INPUT NUMERO
                OUTPUT IMP01.
           MOVE " MODIFICACION STOCKMIN " TO D20-CAP.
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
......            IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE 0 TO CONT-P CONT-I.
           PERFORM TITU-1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       CARGA-COD.
           PERFORM BORRA-CAP.
           MOVE 8 TO SW3.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM PIE-P-1
                     PERFORM CIERRA-A CLOSE IMP01 GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-COD.
           MOVE NUM-CAP TO COD-ART02.
           READ ART02 INVALID KEY GO TO CARGA-COD.
      *     IF ACT-PRI-EMP = 99999
      *          MOVE NUM-ART02 TO N-ART02
      *          MOVE N-1-ART02 TO COD-MAR
      *          MOVE NUM-ART02 TO COD-NUM
      *       ELSE MOVE NUM-ART02 TO COD-NUM
      *            MOVE MAR-ART02 TO COD-MAR.
           MOVE NUM-ART02 TO COD-NUM.
           MOVE MAR-ART02 TO COD-MAR.
           READ NUMERO WITH NO LOCK INVALID KEY 
                       MOVE "*******" TO DET-NUM.
           READ MARCAS WITH NO LOCK INVALID KEY
                       MOVE "*******" TO DET-MAR.
           PERFORM DESP-DES THRU DESP-STO.
           MOVE "DIGITE <F9> MOD" TO MEN-1.
           MOVE "IFICA STOCKMIN " TO MEN-2.
           MOVE "               " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM PIE-P-1 CLOSE IMP01 
                           UNLOCK ART02 RECORD
                           PERFORM CIERRA-A GO TO INI-MOD.
           IF F09 GO TO MODI.
           UNLOCK ART02 RECORD
           GO TO CARGA-COD.
       MODI.
           MOVE MIN-S-ART02 TO STOC-CAP.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT MOVI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3) TAB CONVERT
                           ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM PIE-P-1
                    UNLOCK ART02 RECORD
                    PERFORM CIERRA-A CLOSE IMP01 GO TO INI-MOD.
           COMPUTE REST-CAP = STOC-CAP + MOVI-CAP.
           PERFORM DESP-ACT.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF F01 MOVE REST-CAP TO MIN-S-ART02
                  CLOSE ART02
                  OPEN I-O ART02
                  REWRITE REG-ART02
                  CLOSE ART02
                  OPEN INPUT ART02
                  UNLOCK ART02 RECORD
                  PERFORM IMPRE-1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           UNLOCK ART02 RECORD
           GO TO CARGA-COD.
       IMPRE-1.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU-1.
           MOVE DET-NUM TO DET-1-ED.
           MOVE NUM-ART02 TO NUM-1-ED.
           MOVE MAR-ART02 TO MAR-1-ED.
           MOVE STOC-CAP TO STO-1-ED.
           MOVE MOVI-CAP TO MOV-1-ED.
           MOVE REST-CAP TO ACT-1-ED.
           IF REST-CAP < 0 MOVE "VERIFIQ." TO OBS-1-ED
              ELSE MOVE SPACES TO OBS-1-ED.
           WRITE LIN-IMP01 FROM IMP-31 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           IF REP = 0
              MOVE " LISTADO DEL STOCKMIN " TO D20-CAP.
           IF REP = 9
              MOVE " LISTADO DE REPOSIC. " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-IMP.
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
           MOVE "X" TO COD2 COD3.
       L-N-DESDE.
           DISPLAY SPACES LINE 15 POSITION 32 SIZE 25.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-MAR LISTO-H.
       L-N-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT MARCAS
                INPUT ART02
                INPUT NUMERO.
           START MARCAS KEY IS NOT < CLAVE-MARCAS
                        INVALID KEY CLOSE IMP01 PERFORM CIERRA-A
                        GO TO L-N-DESDE.
           MOVE COD-MAR TO MARCA-L.
           READ MARCAS INVALID KEY GO TO L-N-DESDE.
      *     IF ACT-PRI-EMP = 99999 
      *           MOVE COD-MAR TO N-1-ART02
      *           MOVE 0 TO N-2-ART02
      *           MOVE N-ART02 TO NUM-ART02
      *           MOVE 0 TO MAR-ART02
      *       ELSE MOVE 0 TO NUM-ART02
      *            MOVE COD-MAR TO MAR-ART02. 
           MOVE 0 TO NUM-ART02.
           MOVE COD-MAR TO MAR-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02
                       INVALID KEY CLOSE IMP01 PERFORM CIERRA-A
                       GO TO L-N-DESDE.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           IF REP = 0
              MOVE SPACES TO TIT-1-EDI TIT-6-EDI
              MOVE "    <<< LI" TO TIT-2-EDI
              MOVE "STADO DEL " TO TIT-3-EDI
              MOVE "STOCKMIN >>> " TO TIT-4-EDI
              MOVE "          " TO TIT-5-EDI
              MOVE TITULO-EDI TO TIT-E80.
           IF REP = 9
              MOVE SPACES TO TIT-1-EDI TIT-6-EDI
              MOVE " <<< LISTA" TO TIT-2-EDI
              MOVE "DO DE REPO" TO TIT-3-EDI
              MOVE "SICION >>>" TO TIT-4-EDI
              MOVE "          " TO TIT-5-EDI
              MOVE TITULO-EDI TO TIT-E80.
           PERFORM MEN-LIS.
           OPEN OUTPUT ORDEN.
           GO TO L-LEO.
       L-LEO.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R > 67 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.

           READ ART02 NEXT AT END CLOSE ORDEN
                                  DISPLAY "UNO"
                                  GO TO RETOME. 
           
      *                     AT END PERFORM PIE-P
      *                     CLOSE IMP01 PERFORM CIERRA-A
      *                     GO TO L-N-DESDE.

           IF SUC-EMP NOT = SUC-ART02 MOVE "F" TO COD3.
           IF COD3 = "F" CLOSE ORDEN
                         GO TO RETOME.

      *                  PERFORM PIE-P
      *                  CLOSE IMP01 PERFORM CIERRA-A
      *                  GO TO L-N-DESDE.


      *     IF ACT-PRI-EMP = 99999 MOVE NUM-ART02 TO N-ART02
      *           IF N-1-ART02 NOT = LISTO-H 
      *                      GO TO L-LEO.
      *     IF ACT-PRI-EMP NOT = 99999
      *              IF MAR-ART02 NOT = LISTO-H GO TO L-LEO.

            IF MAR-ART02 NOT = LISTO-H DISPLAY "MAR"
                                       GO TO L-LEO.
            MOVE NUM-ART02 TO COD-NUM
            READ NUMERO INVALID KEY DISPLAY "NUM"
                                    GO TO L-LEO.

            MOVE EMP-ART02 TO EMP-ORD
            MOVE SUC-ART02 TO SUC-ORD
            MOVE NUM-ART02 TO NUM-ORD
            MOVE MAR-ART02 TO MAR-ORD
            MOVE DET-NUM   TO DET-ORD
            WRITE REG-ORDEN INVALID KEY DISPLAY "IMP".
            GO TO L-LEO.

       RETOME.
            OPEN INPUT ORDEN.
       LEA-ORDEN.
            READ ORDEN NEXT AT END PERFORM PIE-P
                                   CLOSE IMP01  ORDEN 
                                   PERFORM CIERRA-A
                                   GO TO L-N-DESDE.
            MOVE EMP-ORD TO EMP-ART02
            MOVE SUC-ORD TO SUC-ART02
            MOVE NUM-ORD TO NUM-ART02
            MOVE MAR-ORD TO MAR-ART02
            READ ART02 INVALID KEY GO TO LEA-ORDEN.



           MOVE NUM-ART02 TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "*******" TO DET-NUM.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           IF MIN-S-ART02 = 0 AND
                MIN-S-ART02 = 0 GO TO LEA-ORDEN.
           MOVE MIN-S-ART02 TO MIN-ED MINI-CAP.
           MOVE MIN-S-ART02 TO STO-ED STOC-CAP.
           COMPUTE REST-CAP = STOC-CAP - MINI-CAP.
           MOVE REST-CAP TO RES-ED.
           MOVE SPACES TO OBS-ED.
           IF REP = 9 
              IF STOC-CAP > MINI-CAP OR STOC-CAP = MINI-CAP
                              GO TO LEA-ORDEN.
           IF STOC-CAP < MINI-CAP MOVE "REPONER" TO OBS-ED.
           IF STOC-CAP < 0 MOVE "VERIF." TO OBS-ED.
           WRITE LIN-IMP01 FROM IMP-3 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.

           IF CONT-R > 67 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.

           GO TO LEA-ORDEN.

       LIST-LIN.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE "LIST.STOCKMIN P/LINEAS" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-LIN.
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
       L-L-LIN1.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO LIST-LIN.
           IF NUM-CAP = 0 GO TO L-L-LIN1.
           MOVE NUM-CAP TO LIN-1-CAP.
       L-L-LIN2.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-L-LIN1.
           MOVE NUM-CAP TO LIN-2-CAP.
       L-L-LIN3.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-L-LIN2.
           MOVE NUM-CAP TO LIN-3-CAP.
       L-L-LIN4.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-L-LIN3.
           MOVE NUM-CAP TO LIN-4-CAP.
       L-L-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT MARCAS
                INPUT ART01
                INPUT ART02
                INPUT NUMERO.
           MOVE 0 TO COD-ART02.
           MOVE " " TO DET-NUM.
           MOVE 0 TO CONT-R.
           START NUMERO KEY IS NOT < DET-NUM INVALID KEY
                       PERFORM CIERRE-A1
                       GO TO INI-MOD.
       L-L-LEO.
           READ NUMERO NEXT AT END PERFORM CIERRE-A1
                               GO TO INI-MOD.
           MOVE COD-NUM TO NUM-ART01.
           MOVE 0 TO MAR-ART01.
           MOVE NUM-EMP TO EMP-ART01.
           START ART01 KEY IS NOT < CLAVE-ART01 INVALID KEY
                          GO TO L-L-LEO.
       L-L-LEO1.
           READ ART01 NEXT AT END GO TO L-L-LEO.
           IF NUM-ART01 > COD-NUM GO TO L-L-LEO.
           IF EMP-ART01 NOT = NUM-EMP GO TO L-L-LEO.
           IF LIN-1-CAP NOT = LIN-1-ART01 GO TO L-L-LEO.
           IF LIN-2-CAP NOT = 0
              IF LIN-2-CAP NOT = LIN-2-ART01 GO TO L-L-LEO.
           IF LIN-3-CAP NOT = 0
              IF LIN-3-CAP NOT = LIN-3-ART01 GO TO L-L-LEO.
           IF LIN-4-CAP NOT = 0
              IF LIN-4-CAP NOT = LIN-4-ART01 GO TO L-L-LEO.
           MOVE COD-ART01 TO COD-ART02.
           MOVE "TODAS P/LINEAS" TO DET-MAR.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R > 67 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ ART02 INVALID KEY GO TO L-L-LEO1.
           IF SUC-ART02 NOT = SUC-EMP GO TO L-L-LEO.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           MOVE MIN-S-ART02 TO MIN-ED MINI-CAP.
           MOVE MIN-S-ART02 TO STO-ED STOC-CAP.
           COMPUTE REST-CAP = STOC-CAP - MINI-CAP.
           MOVE REST-CAP TO RES-ED.
           MOVE SPACES TO OBS-ED.
           IF STOC-CAP < MINI-CAP MOVE "REPONER" TO OBS-ED.
           IF STOC-CAP < 0 MOVE "VERIF." TO OBS-ED.
           WRITE LIN-IMP01 FROM IMP-3 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-L-LEO1.
       LIST-PEN.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE "LIST.PENDIENTES" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-PEN.
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-P-LIN1.
           MOVE 0 TO COD-MAR LISTO-D LISTO-H.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO LIST-LIN.
           IF NUM-CAP = 0 GO TO L-P-LIN1.
           MOVE NUM-CAP TO LIN-1-CAP.
       L-P-LIN2.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-P-LIN1.
           MOVE NUM-CAP TO LIN-2-CAP.
       L-P-LIN3.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-P-LIN2.
           MOVE NUM-CAP TO LIN-3-CAP.
       L-P-LIN4.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-P-LIN3.
           MOVE NUM-CAP TO LIN-4-CAP.
           GO TO L-P-POSI.
       L-P-POSI.
           OPEN OUTPUT IMP01.
           OPEN INPUT MARCAS
                INPUT ART01
                INPUT ART02
                INPUT NUMERO.
           MOVE 0 TO COD-ART02.
           MOVE " " TO DET-NUM.
           MOVE 0 TO CONT-R.
           START NUMERO KEY IS NOT < DET-NUM INVALID KEY
                       PERFORM CIERRE-A1
                       GO TO INI-MOD.
       L-P-LEO.
           READ NUMERO NEXT WITH NO LOCK AT END PERFORM CIERRE-A1
                               GO TO INI-MOD.
           MOVE COD-NUM TO NUM-ART01.
           MOVE 0 TO MAR-ART01.
           MOVE NUM-EMP TO EMP-ART01.
           START ART01 KEY IS NOT < CLAVE-ART01 INVALID KEY
                          GO TO L-P-LEO.
       L-P-LEO1.
           READ ART01 NEXT WITH NO LOCK AT END GO TO L-P-LEO.
           IF NUM-ART01 > COD-NUM GO TO L-P-LEO.
           IF EMP-ART01 NOT = NUM-EMP GO TO L-P-LEO.
           IF LIN-1-CAP NOT = LIN-1-ART01 GO TO L-P-LEO.
           IF LIN-2-CAP NOT = 0
              IF LIN-2-CAP NOT = LIN-2-ART01 GO TO L-P-LEO.
           IF LIN-3-CAP NOT = 0
              IF LIN-3-CAP NOT = LIN-3-ART01 GO TO L-P-LEO.
           IF LIN-4-CAP NOT = 0
              IF LIN-4-CAP NOT = LIN-4-ART01 GO TO L-P-LEO.
           MOVE COD-ART01 TO COD-ART02.
           MOVE "TODAS P/LINEAS" TO DET-MAR.
           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R > 67 PERFORM ESPA 4 TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           READ ART02 INVALID KEY GO TO L-P-LEO1.
           IF SUC-ART02 NOT = SUC-EMP GO TO L-P-LEO.
           MOVE NUM-ART02 TO NUM-2-ED.
           MOVE MAR-ART02 TO MAR-2-ED.
           MOVE DET-NUM TO DET-2-ED.
           MOVE ENT-S-ART02 TO PEN-2-ED PEN-CAP.
           MOVE MIN-S-ART02 TO STO-2-ED STOC-CAP.
           COMPUTE REST-CAP = STOC-CAP + PEN-CAP.
           MOVE REST-CAP TO RES-2-ED.
           MOVE SPACES TO OBS-2-ED.
           IF REST-CAP < 0 MOVE "DEFICIT" TO OBS-ED.
           WRITE LIN-IMP01 FROM IMP-32 AFTER 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-L-LEO1.                   
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
           MOVE "<<< ACTUAL" TO TIT-2-EDI.
           MOVE "IZACION DE" TO TIT-3-EDI.
           MOVE "L STOCKMIN >>" TO TIT-4-EDI.
           MOVE ">         " TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
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
           WRITE LIN-IMP01 FROM IMP-11 AFTER 1.
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
       PIE-P-1.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L1-ED.
           WRITE LIN-IMP01 FROM IMP-21 AFTER 1.
           ADD 1 TO CONT-R.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 081502 TO TAB-POS(4).
           MOVE 082620 TO TAB-POS(5).
           MOVE 082302 TO TAB-POS(6).
           MOVE 082620 TO TAB-POS(7).
           MOVE 101510 TO TAB-POS(8).
           MOVE 102730 TO TAB-POS(9).
           MOVE 105915 TO TAB-POS(10).
           MOVE 121511 TO TAB-POS(11).
           MOVE 141511 TO TAB-POS(12)
           MOVE 161511 TO TAB-POS(13).
           MOVE 121511 TO TAB-POS(14).
           MOVE 141511 TO TAB-POS(15)
           MOVE 161511 TO TAB-POS(16).
           MOVE 101504 TO TAB-POS(17).
           MOVE 104504 TO TAB-POS(18).
           MOVE 102003 TO TAB-POS(19).
           MOVE 102503 TO TAB-POS(20).
           MOVE 103003 TO TAB-POS(21).
           MOVE 103503 TO TAB-POS(22).
       TIT-PAN.
           MOVE "  -ARCHIVO" TO TIT-1-PAN.
           MOVE " DE ARTICU" TO TIT-2-PAN.
           MOVE "LOS-      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  MODIFICA STOCKMIN " LINE 14 POSITION 18.
           DISPLAY "<F5>  LIST. STOCKMIN P/MARCAS" LINE 15 POSITION 18.
           DISPLAY "<F6>  LIST. REPOSIC. P/MARCAS" LINE 16 POSITION 18.
           DISPLAY "<F7>  LIST. STOCKMIN P/LINEAS" LINE 17 POSITION 18.
      *     DISPLAY "<F8>  LIST. STOCKMIN PENDIENTE" LINE 18 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 19 POSITION 18.
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
           MOVE MIN-S-ART02 TO STOC-EDI.
           DISPLAY STOC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       DESP-ACT.
           MOVE 13 TO SW3.
           MOVE REST-CAP TO REST-EDI.
           DISPLAY REST-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       PANTA-MOD.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "NRO.ARTIC. :" LINE 10 POSITION 3.
           DISPLAY "STOCKMIN ART. :" LINE 12 POSITION 3.
           DISPLAY "MODIF.        :" LINE 14 POSITION 3.
           DISPLAY "STOCKMIN ACT. :" LINE 16 POSITION 3.
       PANTA-IMP.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "DESDE MARCA:" LINE 10 POSITION 3.
       PANTA-LIN.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "DESDE LINEA:" LINE 10 POSITION 3.
       PANTA-PEN.
           DISPLAY "EMPRESA:" LINE 8 POSITION 3.
           DISPLAY "-SUC:" LINE 8 POSITION 18.
           DISPLAY "DESDE LINEA:" LINE 10 POSITION 3.
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
       CARGO-EMP-F.
           EXIT.
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
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT80.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
