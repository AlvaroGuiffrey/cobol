       IDENTIFICATION DIVISION.
       PROGRAM-ID.   P-CTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
            SELECT P-DE-CTAS ASSIGN TO RANDOM "P-DE-CTAS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CLAVE-P-DE-CTAS.
            SELECT MATRIZ ASSIGN TO RANDOM "MATRIZ"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CLAVE-MATRIZ.
       DATA DIVISION.
       FILE SECTION.
            COPY "P-CTAS.REG".
            COPY "MATRIZ.REG".
            COPY "EMPREBM.REG".
            COPY "PANTA.REG".
            COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 NORMAL.
          02 BB                PIC 99 COMP-4 VALUE 18.
       01 TITULO.
          02 MARGEN                     PIC X(80).
       01 TITULO1.
          02 FILLER            PIC X(9) VALUE  "-RUBRO-  ".
          02 FILLER            PIC X(7) VALUE  "AUX.   ".
          02 FILLER            PIC X(34) VALUE "DETALLE".
          02 FILLER            PIC X(24) VALUE "OBSERVACION".
          02 FILLER            PIC X(6) VALUE "MOVIM.".
       01 RENGLON-C.
          02 FILLER            PIC X(80) VALUE ALL "-".
       01 RENGLON-M.
          02 NOM-MAT-ED                PIC X(40).
       01 RENGLON.
          02 RUBRO-ED                  PIC ZZZZZZZBB.
          02 RUBRO-A-ED                PIC ZZZBBBB BLANK WHEN ZERO.
          02 NOMBRE-ED                 PIC X(30) VALUE SPACES.
          02 FILLER                    PIC X(4) VALUE SPACES. 
          02 DET-ED                    PIC X(24) VALUE SPACES.
          02 CONTADOR-ED               PIC ZZZZZZ BLANK WHEN ZERO.
       01 TITULO-2.
          02 FILLER          PIC X(5) VALUE "SON: ".
          02 CONT-I-ED       PIC ZZZZZ9BB.
          02 FILLER          PIC X(23) VALUE "Cuentas del Mayor con".
          02 CONT-A-ED       PIC ZZZZZ9.
          02 FILLER          PIC X(11) VALUE " Auxiliares".
       01 IND                             PIC 9(4) COMP VALUE 0.
       01 FIN                             PIC X.
       01 DED                             PIC 9 VALUE 0.
       01 MENSA                           PIC X(60).
       01 LIMPIADOR                       PIC X(60) VALUE SPACES.
       01 RUBRO                           PIC 9(7) VALUE 0.
       01 RUBRO-A                         PIC 9(3) VALUE 0.
       01 RUBRO-B                         PIC 99 VALUE 0.
       01 VERSION                         PIC 9(3) VALUE 0.
       01 CONT-L                    PIC 999999 VALUE 0.
       01 CONT-A                    PIC 999999 VALUE 0.
       01 RUBRO1.
          03 RUB1                         PIC 9(3) VALUE 0.
          03 RUB2                         PIC 99 VALUE 0.
          03 RUB3                         PIC 99V999 VALUE 0.
       01 CONTADOR                        PIC 9(6) VALUE 0.
       01 CODIGO                          PIC 99 VALUE 0.
       01 FECHA-ED.
          03 DIA-ED                       PIC ZZ/.
          03 MES-ED                       PIC ZZ/.
          03 ANIO-ED                      PIC ZZ.
       01 O                                PIC 99 VALUE 0.
       01 K                                PIC 99 VALUE 0.
       01 C-M                             PIC 999 VALUE 0.
       01 E-NOMBRE                        PIC X(30).
       01 SEFORMA                         PIC X(50) VALUE SPACES.
       01 ML1                  PIC 999 VALUE 0.
       01 ML2                  PIC 999 VALUE 0.
       01 ML3                  PIC 999 VALUE 0.
       01 ML4                  PIC 999 VALUE 0.
       01 ML5                  PIC 999 VALUE 0.
       01 ML6                  PIC 999 VALUE 0.
       01 P-VERSION-AU  PIC 99 VALUE 0.
       01 P-RUBRO-AU    PIC 9(7) VALUE 0.
       01 P-RUBRO-A-AU  PIC 999 VALUE 0.
       01 M1-A          PIC 999 VALUE 0.
       01 M2-A          PIC 999 VALUE 0.
       01 M3-A          PIC 999 VALUE 0.
       01 M4-A          PIC 999 VALUE 0.
       01 M5-A          PIC 999 VALUE 0.
       01 M6-A          PIC 999 VALUE 0.
       01 CODIGO-ED     PIC 9 VALUE 0.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "P-CTA.ERR".
       COPY "MATRIZ.ERR".
       COPY "IMP01.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CGMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
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
           MOVE 28 TO SW3.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP STOP RUN.
           IF F01 GO TO TAREA1.
           IF F03 GO TO CONS-NUM.
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       TAREA1.
           OPEN INPUT EMPRESAS P-DE-CTAS MATRIZ.
           MOVE " ALTA, BAJA Y MODIF. " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
       TAREA1-1.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA-C.
           PERFORM CARGA-RUB.
           MOVE VERSION-EMP TO P-VERSION.
           MOVE 0 TO P-RUBRO-A.
           MOVE "X" TO COD2.
           READ P-DE-CTAS INVALID KEY GO TO ALTA.
           MOVE M1 TO M1-A.
           MOVE M2 TO M2-A.
           MOVE M3 TO M3-A.
           MOVE M4 TO M4-A.
           MOVE M5 TO M5-A.
           MOVE M6 TO M6-A.
           PERFORM DESP-NOM.
           PERFORM DESP-COD THRU DESP-MAT-6.
           MOVE P-CODIGO TO CODIGO-ED.
           IF CODIGO-ED NOT = 8 GO TO TAREA1-10.
           PERFORM CARGA-RUB-A.
           READ P-DE-CTAS INVALID KEY GO TO ALTA-A.
           PERFORM DESP-NOM-A THRU DESP-MAT-6.
       TAREA1-10.    
           PERFORM MEN-B-M.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F09 GO TO MODI.
           IF F10 GO TO BAJA.
           GO TO TAREA1-1.
       ALTA.
           MOVE "A" TO COD2.
           PERFORM CARGA-NOM.
           PERFORM CARGA-COD THRU CARGA-MAT-F.
           MOVE 0 TO P-CONTADOR.
           CLOSE P-DE-CTAS.
           OPEN I-O P-DE-CTAS.
           WRITE REG-P-DE-CTAS INVALID KEY REWRITE REG-P-DE-CTAS.
           CLOSE P-DE-CTAS.
           OPEN INPUT P-DE-CTAS.
           GO TO TAREA1-1.  
       ALTA-A.
           MOVE "A" TO COD2.
           PERFORM CARGA-NOM-A.
           MOVE 8 TO P-CODIGO.
           MOVE 0 TO P-CONTADOR.
           MOVE M1-A TO M1.
           MOVE M2-A TO M2.
           MOVE M3-A TO M3.
           MOVE M4-A TO M4.
           MOVE M5-A TO M5.
           MOVE M6-A TO M6.
           CLOSE P-DE-CTAS.
           OPEN I-O P-DE-CTAS.
           WRITE REG-P-DE-CTAS INVALID KEY REWRITE REG-P-DE-CTAS.
           CLOSE P-DE-CTAS.
           OPEN INPUT P-DE-CTAS.
           GO TO TAREA1-1.
       MODI.
           IF P-RUBRO-A = 0 PERFORM CARGA-NOM.
           IF P-RUBRO-A NOT = 0 PERFORM CARGA-NOM-A.
           PERFORM CARGA-MAT-1 THRU CARGA-MAT-F.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           CLOSE P-DE-CTAS.
           OPEN I-O P-DE-CTAS.
           REWRITE REG-P-DE-CTAS INVALID KEY WRITE REG-P-DE-CTAS.
           CLOSE P-DE-CTAS.
           OPEN INPUT P-DE-CTAS.
           GO TO TAREA1-1.
       BAJA.
           MOVE "X" TO COD2.
           IF P-RUBRO-A = 0 PERFORM VERI-BAJA THRU VERI-BAJA-F.
           IF P-CONTADOR NOT = 0 MOVE "N" TO COD2.
           IF COD2 NOT = "X" PERFORM MEN-NO-BAJA 50 TIMES
                             MOVE SPACES TO MEN-1 MEN-2 MEN-3
                             PERFORM MENS-PAN
                             GO TO TAREA1-1.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           CLOSE P-DE-CTAS.
           OPEN I-O P-DE-CTAS.
           DELETE P-DE-CTAS.
           CLOSE P-DE-CTAS.
           OPEN INPUT P-DE-CTAS.
           GO TO TAREA1-1.
       VERI-BAJA.
           MOVE P-VERSION TO P-VERSION-AU.
           MOVE P-RUBRO TO P-RUBRO-AU.
           MOVE P-RUBRO-A TO P-RUBRO-A-AU.
           MOVE 1 TO P-RUBRO-A.
           START P-DE-CTAS KEY IS NOT < CLAVE-P-DE-CTAS
                    INVALID KEY GO TO VERI-BAJA-AU.
       VERI-BAJA-1.
           READ P-DE-CTAS NEXT AT END GO TO VERI-BAJA-AU.
           IF P-VERSION NOT = P-VERSION-AU GO TO VERI-BAJA-AU.
           IF P-RUBRO NOT = P-RUBRO-AU GO TO VERI-BAJA-AU.
           MOVE "N" TO COD2.
       VERI-BAJA-AU.
           MOVE P-VERSION-AU TO P-VERSION.
           MOVE P-RUBRO-AU TO P-RUBRO.
           MOVE P-RUBRO-A-AU TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY MOVE "N" TO COD2.
       VERI-BAJA-F.
           EXIT.
       CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E80.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E80.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           MOVE VERSION-EMP TO P-VERSION.
       CARGA-EMP-F.
           EXIT.
       CARGA-RUB.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUC.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE 0 TO P-RUBRO-A.
           MOVE NUM-CAP TO P-RUBRO.
           PERFORM DESP-RUB.
       CARGA-NOM.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           MOVE ALF-CAP TO P-NOMBRE.
       CARGA-RUB-A.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           MOVE NUM-CAP TO P-RUBRO-A.
           PERFORM DESP-RUB-A.
       CARGA-NOM-A.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB-A.
           MOVE ALF-CAP TO P-NOMBRE.
       CARGA-COD.
           IF COD2 NOT = "A" PERFORM DESP-COD
                             GO TO CARGA-COD-F.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO VERI-DONDE.
           IF F03 PERFORM DESP-COD GO TO CARGA-MAT-1.
           IF NUM-CAP < 8 
                 MOVE 0 TO NUM-CAP.
           MOVE NUM-CAP TO P-CODIGO.
           PERFORM DESP-COD.
       CARGA-COD-F.
       CARGA-MAT-1.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO VERI-DONDE.
           IF F03 PERFORM DESP-MAT-1 GO TO CARGA-MAT-2.
           IF NUM-CAP = 0 GO TO CARGA-MAT-1.
           MOVE NUM-CAP TO M1.
           PERFORM DESP-MAT-1.
       CARGA-MAT-2.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAT-1.
           IF F03 PERFORM DESP-MAT-2 GO TO CARGA-MAT-3.
           IF NUM-CAP = 0 MOVE 0 TO M2 M3 M4 M5 M6
                     GO TO CARGA-MAT-F.
           MOVE NUM-CAP TO M2.
           PERFORM DESP-MAT-2.
       CARGA-MAT-3.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAT-2.
           IF F03 PERFORM DESP-MAT-3 GO TO CARGA-MAT-4.
           IF NUM-CAP = 0 MOVE 0 TO M3 M4 M5 M6
                          GO TO CARGA-MAT-F.
           MOVE NUM-CAP TO M3.
           PERFORM DESP-MAT-3.
       CARGA-MAT-4.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAT-3.
           IF F03 PERFORM DESP-MAT-4 GO TO CARGA-MAT-5.
           IF NUM-CAP = 0 MOVE 0 TO M4 M5 M6 GO TO CARGA-MAT-F.
           MOVE NUM-CAP TO M4.
           PERFORM DESP-MAT-4.
       CARGA-MAT-5.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAT-4.
           IF F03 PERFORM DESP-MAT-5 GO TO CARGA-MAT-6.
           IF NUM-CAP = 0 MOVE 0 TO M5 M6 GO TO CARGA-MAT-F.
           MOVE NUM-CAP TO M5.
           PERFORM  DESP-MAT-5.
       CARGA-MAT-6.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-MAT-5.
           IF F03 PERFORM DESP-MAT-6 GO TO CARGA-MAT-F.
           IF NUM-CAP = 0 MOVE 0 TO M6 GO TO CARGA-MAT-F.
           MOVE NUM-CAP TO M6.
           PERFORM DESP-MAT-6.
       CARGA-MAT-F.
           EXIT.
       VERI-DONDE.
           IF P-RUBRO-A = 0 GO TO CARGA-NOM.
           IF P-RUBRO-A NOT = 0 GO TO CARGA-NOM-A.
           GO TO CARGA-NOM.
       DESP-RUB.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           DISPLAY P-RUBRO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-RUB-A.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY P-RUBRO-A LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-NOM-A.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-COD.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE P-CODIGO TO CODIGO-ED.
           DISPLAY CODIGO-ED LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-MAT-1.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE M1 TO ML1.
           DISPLAY ML1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML1 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           IF ML1 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MAT-2.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE M2 TO ML2.
           DISPLAY ML2 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML2 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           IF ML2 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MAT-3.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE M3 TO ML3.
           DISPLAY ML3 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML3 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           IF ML3 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MAT-4.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE M4 TO ML4.
           DISPLAY ML4 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML4 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           IF ML4 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MAT-5.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE M5 TO ML5.
           DISPLAY ML5 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML5 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           IF ML5 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       DESP-MAT-6.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE M6 TO ML6.
           DISPLAY ML6 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE ML6 TO NUMERO-MATRIZ.
           READ MATRIZ INVALID KEY 
                 MOVE "*******" TO DESCRIPCION-MATRIZ.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           IF ML6 = 0 MOVE SPACES TO DESCRIPCION-MATRIZ.
           DISPLAY DESCRIPCION-MATRIZ LINE TAB-LIN(SW3) 
                   POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       CONS-NUM.
           GO TO INI-MOD.      
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 8 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 10 POSITION 5.
           DISPLAY "RUBRO AUX:          - " LINE 11 POSITION 5.
           DISPLAY "CODIGO   : " LINE 14 POSITION 5.
           DISPLAY " INGRESE:               " LINE 13 POSITION 27
                             REVERSE.
           DISPLAY " 8 - RUBRO C/AUXILIAR   " LINE 14 POSITION 27
                             REVERSE.
           DISPLAY " 9 - RUBRO C/CTAS.CTES. " LINE 15 POSITION 27
                             REVERSE.
           DISPLAY "MATRIZ 1 : " LINE 17 POSITION 5.
           DISPLAY "MATRIZ 2 : " LINE 18 POSITION 5.
           DISPLAY "MATRIZ 3 : " LINE 19 POSITION 5.
           DISPLAY "MATRIZ 4 : " LINE 20 POSITION 5.
           DISPLAY "MATRIZ 5 : " LINE 21 POSITION 5.
           DISPLAY "MATRIZ 6 : " LINE 22 POSITION 5.
       PANTA-2.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 8 POSITION 5.
           DISPLAY "REG. LEIDOS: " LINE 21 POSITION 5.
           DISPLAY "REG. IMPRESOS: " LINE 21 POSITION 35.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 081604 TO TAB-POS(4).
           MOVE 083002 TO TAB-POS(5).
           MOVE 083830 TO TAB-POS(6).
           MOVE 101607 TO TAB-POS(7).
           MOVE 102730 TO TAB-POS(8).
           MOVE 111607 TO TAB-POS(9).
           MOVE 112730 TO TAB-POS(10).
           MOVE 141601 TO TAB-POS(11).
           MOVE 171603 TO TAB-POS(12).
           MOVE 172530 TO TAB-POS(13).
           MOVE 181603 TO TAB-POS(14).
           MOVE 182530 TO TAB-POS(15).
           MOVE 191603 TO TAB-POS(16).
           MOVE 192530 TO TAB-POS(17).
           MOVE 201603 TO TAB-POS(18).
           MOVE 202530 TO TAB-POS(19).
           MOVE 211603 TO TAB-POS(20).
           MOVE 212530 TO TAB-POS(21).
           MOVE 221603 TO TAB-POS(22).
           MOVE 222530 TO TAB-POS(23).
           MOVE 211906 TO TAB-POS(24).
           MOVE 215006 TO TAB-POS(25).
       TIT-PAN.
           MOVE " - ARCHIVO" TO TIT-1-PAN.
           MOVE " PLAN DE C" TO TIT-2-PAN.
           MOVE "UENTAS -  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA, BAJA Y MODIF." LINE 14 POSITION 18.
           DISPLAY "<F3>  CONSULTA NUMERICA  " LINE 15 POSITION 18.
           DISPLAY "<F5>  LISTADO NUMERICO   " LINE 16 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU     " LINE 17 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
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
       CIERRE-A.
           CLOSE P-DE-CTAS MATRIZ EMPRESAS.
       FINAL1.
           EXIT PROGRAM.
       FF.
           STOP RUN.
       LIST-NUM.
           OPEN INPUT P-DE-CTAS MATRIZ EMPRESAS.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-2.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           MOVE 0 TO CONT-P CONT-I CONT-A CONT-R CONT-L.
           MOVE VERSION-EMP TO P-VERSION.
           MOVE 0 TO P-RUBRO.
           MOVE 0 TO P-RUBRO-A. 
           START P-DE-CTAS KEY IS NOT < CLAVE-P-DE-CTAS INVALID KEY
                                  PERFORM CIERRE-A GO TO LIST-NUM.
           OPEN OUTPUT IMP01.
           MOVE 0 TO ML1 ML2 ML3 ML4 ML5 ML6.
           PERFORM TITULOS.
       LEA.
           READ P-DE-CTAS NEXT AT END
                          GO TO CORTE.
           ADD 1 TO CONT-L.
           MOVE 24 TO SW3.
           DISPLAY CONT-L LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE.  
           PERFORM CHEQUEO-MAT.
           IF P-VERSION NOT = VERSION-EMP GO TO CORTE.
           MOVE P-RUBRO TO RUBRO-ED.
           MOVE P-RUBRO-A TO RUBRO-A-ED.
           MOVE P-NOMBRE TO NOMBRE-ED.
           MOVE SPACES TO DET-ED.
           IF P-CODIGO = 9 MOVE "C/CTAS.CTES." TO DET-ED.
           MOVE P-CONTADOR TO CONTADOR-ED.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITULOS.
           WRITE LIN-IMP01 FROM RENGLON AFTER 1.
           ADD 1 TO CONT-R.
           IF P-RUBRO-A = 0
                   ADD 1 TO CONT-I
                   ELSE ADD 1 TO CONT-A.
           MOVE 25 TO SW3.
           DISPLAY CONT-I LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) REVERSE. 
           GO TO LEA.
       TITULOS.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E80.
           PERFORM FECHA-ROT80.
           PERFORM HORA-ROT80.
           MOVE "    <<< LI" TO TIT-1-EDI.
           MOVE "STADO NUME" TO TIT-2-EDI.
           MOVE "RICO DEL P" TO TIT-3-EDI.
           MOVE "LAN DE CUE" TO TIT-4-EDI.
           MOVE "NTAS >>>  " TO TIT-5-EDI.
           MOVE SPACES TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           WRITE LIN-IMP01 FROM NORMAL AFTER 0.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-4 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM TITULO1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           ADD 7 TO CONT-R.
       CORTE.
           MOVE CONT-I TO CONT-I-ED.
           MOVE CONT-A TO CONT-A-ED.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM TITULO-2 AFTER 1.
           ADD 2 TO CONT-R.
       SIGA-CORTE.
           CLOSE IMP01.
           PERFORM CIERRE-A.
           GO TO LIST-NUM.
       CHEQUEO-MAT.
           IF ML1 = 0 MOVE M1 TO ML1
                      MOVE M2 TO ML2
                      MOVE M3 TO ML3
                      MOVE M4 TO ML4
                      MOVE M5 TO ML5
                      MOVE M6 TO ML6
                      PERFORM LISTO-MAT1.
           IF M1 NOT = 0 AND ML1 NOT = M1
                  MOVE M1 TO NUMERO-MATRIZ ML1
                  WRITE LIN-IMP01 FROM ROT80-1 AFTER 1
                  ADD 1 TO CONT-R
                  PERFORM IMPRIMA-MAT THRU I-MAT-F.
           IF M2 NOT = 0 AND ML2 NOT = M2
                 MOVE M2 TO NUMERO-MATRIZ ML2
                 PERFORM IMPRIMA-MAT THRU I-MAT-F.
           IF M3 NOT = 0 AND ML3 NOT = M3
                 MOVE M3 TO NUMERO-MATRIZ ML3
                 PERFORM IMPRIMA-MAT THRU I-MAT-F.
           IF M4 NOT = 0 AND ML4 NOT = M4
                 MOVE M4 TO NUMERO-MATRIZ ML4
                 PERFORM IMPRIMA-MAT THRU I-MAT-F.
           IF M5 NOT = 0 AND ML5 NOT = M5
                 MOVE M5 TO NUMERO-MATRIZ ML5
                 PERFORM IMPRIMA-MAT THRU I-MAT-F.
           IF M6 NOT = 0 AND ML6 NOT = M6
                 MOVE M6 TO NUMERO-MATRIZ ML6
                 PERFORM IMPRIMA-MAT THRU I-MAT-F.
       LISTO-MAT1.
           IF ML1 NOT = 0 MOVE ML1 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
           IF ML2 NOT = 0 MOVE ML2 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
           IF ML3 NOT = 0 MOVE ML3 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
           IF ML4 NOT = 0 MOVE ML4 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
           IF ML5 NOT = 0 MOVE ML5 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
           IF ML6 NOT = 0 MOVE ML6 TO NUMERO-MATRIZ
                  PERFORM IMPRIMA-MAT.
       IMPRIMA-MAT.
           IF NUMERO-MATRIZ = 0 GO TO I-MAT-F.
           READ MATRIZ INVALID KEY 
                    MOVE "************" TO DESCRIPCION-MATRIZ.
           MOVE DESCRIPCION-MATRIZ TO NOM-MAT-ED.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITULOS.
           ADD 1 TO CONT-R.
           WRITE LIN-IMP01 FROM RENGLON-M AFTER ADVANCING 1.
       I-MAT-F.
           EXIT.
       ESPA.
           MOVE SPACES TO LIN-IMP01.
           WRITE LIN-IMP01 AFTER ADVANCING 1.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
       COPY "ROT80.PRO".
      
