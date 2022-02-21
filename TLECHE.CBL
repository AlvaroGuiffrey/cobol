       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLECHE.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. SEP'96.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "CUENTA.SEL".
           COPY "EMPRE.SEL".
           COPY "IMP01.SEL".
           COPY "CONDI.SEL".
           COPY "P-CTAS.SEL".
           COPY "LECHE.SEL".
           COPY "CPOSTAL.SEL".
           COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "CUENTABM.REG".
           COPY "EMPREBM.REG".
           COPY "CPOSTAL.REG".
           COPY "IMP01.REG".
           COPY "P-CTAS.REG".
           COPY "LECHE.REG".
           COPY "CONDI.REG".
           COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
           COPY "ROT80.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER     PIC X(25) VALUE SPACES.
             03 FILLER     PIC X(18) VALUE "- DETALLE DE LECHE".
             03 FILLER     PIC X(11) VALUE " RECIBIDA -".
          02 IMP-2.
             03 FILLER     PIC X(14) VALUE "TAMBO Nro. :".
             03 CUENTA-ED  PIC ZZZZZ9BB.
             03 FILLER     PIC X(3) VALUE "-  ".
             03 NOMBRE-C-ED  PIC X(30).
          02 IMP-3.
             03 FILLER     PIC X(10) VALUE "PERIODO : ".
             03 DIA-D-ED    PIC 99/.
             03 MES-D-ED    PIC 99/.
             03 FILLER      PIC XX VALUE "19".
             03 ANO-D-ED    PIC 99BBB.
             03 FILLER      PIC XXXX VALUE "al".
             03 DIA-H-ED    PIC 99/.
             03 MES-H-ED    PIC 99/.
             03 FILLER      PIC XX VALUE "19".
             03 ANO-H-ED    PIC 99.
          02 IMP-4.
             03 FILLER     PIC X(11) VALUE "--FECHA-".
             03 FILLER     PIC X(10) VALUE "-LECHE-".
             03 FILLER     PIC X(10) VALUE "-LECHE-".
             03 FILLER     PIC X(7) VALUE "ACIDEZ".
             03 FILLER     PIC X(10) VALUE "-LECHE-".
             03 FILLER     PIC X(7) VALUE "ACIDEZ".
             03 FILLER     PIC X(7) VALUE "DENS.".
             03 FILLER     PIC X(15) VALUE "OBSERVACIONES".
          02 IMP-5.
             03 FILLER     PIC X(11) VALUE SPACES.
             03 FILLER     PIC X(10) VALUE " RECIB.".
             03 FILLER     PIC X(10) VALUE " NORMAL".
             03 FILLER     PIC X(7) VALUE "NORMAL".
             03 FILLER     PIC X(10) VALUE " ACIDA ".
             03 FILLER     PIC X(7) VALUE " ACIDA".
             03 FILLER     PIC X(7) VALUE "LECHE".
          02 IMP-6.
             03 DIA-REC-ED   PIC 99/.
             03 MES-REC-ED   PIC 99/.
             03 ANO-REC-ED   PIC 99BBB.
             03 REC-ED       PIC ZZZ.ZZ9BBB.
             03 NOR-ED       PIC ZZZ.ZZ9BBB.
             03 A-NOR-ED     PIC Z9,9BBB.
             03 ACI-ED       PIC ZZZ.ZZ9BBB.
             03 A-ACI-ED     PIC Z9,9BBB.
             03 DEN-ED       PIC ZZZ9 BLANK WHEN ZERO.
             03 FILLER       PIC XXX VALUE SPACES.
             03 OBS-ED       PIC X(15).
          02 IMP-7.
             03 FILLER      PIC X(11) VALUE "TOTALES :".
             03 TOT-REC-ED  PIC ZZZ.ZZ9BBB.
             03 TOT-NOR-ED  PIC ZZZ.ZZ9B(11).
             03 TOT-ACI-ED  PIC ZZZ.ZZ9.
          02 IMP-8.
             03 FILLER        PIC X(11) VALUE "PROMEDIOS: ".
             03 PRO-REC-ED    PIC ZZZ.ZZ9BBB.
             03 PRO-NOR-ED    PIC ZZZ.ZZ9BBB.
             03 PRO-A-NOR-ED  PIC Z9,9BBB.
             03 PRO-ACI-ED    PIC ZZZ.ZZ9BBB.
             03 PRO-A-ACI-ED  PIC Z9,9BBB.
             03 PRO-DEN-ED    PIC ZZZ9.
          02 IMP-9.
             03 FILLER        PIC X(25) VALUE SPACES.
             03 FILLER        PIC X(15) VALUE "- RESUMEN   LEC".
             03 FILLER        PIC X(15) VALUE "HE   RECIBIDA -".
          02 IMP-10.
             03 FILLER        PIC X(16) VALUE "LECHE NORMAL ->".
             03 T-NOR-ED      PIC ZZZ.ZZ9BB.
             03 FILLER        PIC X(9) VALUE "Lts x $".
             03 IMP-NOR-ED    PIC ZZZ.ZZ9,999BB.
             03 FILLER        PIC X(5) VALUE "= $ ".
             03 TOT-I-NOR-ED  PIC ZZZ.ZZ9,99.
          02 IMP-11.
             03 FILLER        PIC X(16) VALUE "LECHE ACIDA -->".
             03 T-ACI-ED      PIC ZZZ.ZZ9BB.
             03 FILLER        PIC X(9) VALUE "Lts x $ ".
             03 IMP-ACI-ED    PIC ZZZ.ZZ9,999BB.
             03 FILLER        PIC X(5) VALUE "= $ ".
             03 TOT-I-ACI-ED  PIC ZZZ.ZZ9,99.
          02 IMP-12.
             03 FILLER        PIC X(48) VALUE SPACES.
             03 FILLER        PIC X(16) VALUE ALL "-".
          02 IMP-13.
             03 FILLER        PIC X(48) VALUE "TOTAL --------->".
             03 FILLER        PIC X(4) VALUE " $ ".
             03 TOT-ED        PIC ZZZ.ZZ9,99.
       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(7) VALUE 0.
          02 REC-CAP           PIC 9(6) VALUE 0.
          02 NOR-CAP           PIC 9(6) VALUE 0.
          02 ACI-CAP           PIC 9(6) VALUE 0.
          02 A-NOR-CAP         PIC 99V9 VALUE 0.
          02 A-ACI-CAP         PIC 99V9 VALUE 0.
          02 DEN-CAP           PIC 9999 VALUE 0.
          02 OBS-CAP           PIC X(15) VALUE SPACES.
          02 PH-NOR-CAP        PIC 99V99 VALUE 0.
          02 PH-ACI-CAP        PIC 99V99 VALUE 0. 
          02 RED-CAP           PIC X(10) VALUE SPACES.
          02 IMP-ACI-CAP       PIC 9(6)V999 VALUE 0.
          02 IMP-NOR-CAP       PIC 9(6)V999 VALUE 0.
          02 TOT-ACI-CAP       PIC 9(6)V99 VALUE 0.
          02 TOT-NOR-CAP       PIC 9(6)V99 VALUE 0.
          02 FECHA-D-CAP.
             03 DIA-D-CAP      PIC 99 VALUE 0.
             03 MES-D-CAP      PIC 99 VALUE 0.
             03 ANO-D-CAP      PIC 99 VALUE 0.
          02 FECHA-H-CAP.
             03 DIA-H-CAP      PIC 99 VALUE 0.
             03 MES-H-CAP      PIC 99 VALUE 0.
             03 ANO-H-CAP      PIC 99 VALUE 0.
          02 FECHA-REC-CAP.
             03 DIA-REC-CAP    PIC 99 VALUE 0. 
             03 MES-REC-CAP    PIC 99 VALUE 0.
             03 ANO-REC-CAP    PIC 99 VALUE 0.
          02 CUENTA-D-CAP      PIC 9(6) VALUE 0.
          02 CUENTA-H-CAP      PIC 9(6) VALUE 0.
       01 EDICION.
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 NOMBRE-C-EDI      PIC X(30).
          02 FECHA-EDI.
             03 DIA-REC-EDI    PIC 99.
             03 FILLER         PIC X VALUE "/".
             03 MES-REC-EDI    PIC 99. 
             03 FILLER         PIC X VALUE "/".
             03 ANO-REC-EDI    PIC 99.
          02 REC-EDI           PIC ZZZ.ZZ9.
          02 NOR-EDI           PIC ZZZ.ZZ9.
          02 A-NOR-EDI         PIC Z9,9.
          02 ACI-EDI           PIC ZZZ.ZZ9.
          02 A-ACI-EDI         PIC Z9,9.
          02 DEN-EDI           PIC ZZZ9.
          02 OBS-EDI           PIC X(15).
          02 IMP-ACI-EDI       PIC ZZZ.ZZ9,999.
          02 IMP-NOR-EDI       PIC ZZZ.ZZ9,999..
       01 AUXILIARES.
          02 COD-TAREA         PIC 9 VALUE 0.
          02 LISTO-N-D         PIC 999999 VALUE 0.
          02 LISTO-N-H         PIC 999999 VALUE 0.
          02 LISTO-Z           PIC 99.
          02 LISTO-A-D         PIC X(30).
          02 LISTO-A-H         PIC X(30).
          02 CONT-L            PIC 9(6) VALUE 0.
          02 ZONA-L            PIC 99 VALUE 0.
          02 RUBRO-L           PIC 9(7) VALUE 0.
          02 CUENTA-H          PIC 9(6) VALUE 0.
          02 SUB-CUENTA-H      PIC 9(4) VALUE 0.
          02 NOMBRE-H          PIC X(30).
          02 C-AUX             PIC X(15).

          02 TOTAL             PIC 9(6) VALUE 0.
          02 TOT-REC           PIC 9(6) VALUE 0.
          02 TOT-NOR           PIC 9(6) VALUE 0.
          02 TOT-ACI           PIC 9(6) VALUE 0.
          02 TOT-I-ACI         PIC 9(6)V99 VALUE 0.
          02 TOT-I-NOR         PIC 9(6)V99 VALUE 0.
          02 TOT               PIC 9(6)V99 VALUE 0.
     *-=-=-=-=-=-=-=- TOTALES AUXILIARES -=-=-=-=-=-=-=-==-
          02 PRO-REC           PIC 9(6) VALUE 0.
          02 PRO-NOR           PIC 9(6) VALUE 0.
          02 PRO-ACI           PIC 9(6) VALUE 0.
          02 PRO-A-NOR         PIC 9(2)V9 VALUE 0.
          02 PRO-A-ACI         PIC 9(2)V9 VALUE 0.
          02 PRO-DEN           PIC 9(4) VALUE 0.
          02 TOT-REC-TBO       PIC 9(6) VALUE 0.
          02 TOT-NOR-TBO       PIC 9(6) VALUE 0.
          02 TOT-ACI-TBO       PIC 9(6) VALUE 0.
          02 A-NOR-TBO         PIC 9(12)V99 VALUE 0.
          02 A-ACI-TBO         PIC 9(12)V99 VALUE 0.
          02 DEN-TBO           PIC 9(15) VALUE 0.
          02 TOT-REC-GRL       PIC 9(6) VALUE 0.
          02 TOT-NOR-GRL       PIC 9(6) VALUE 0.
          02 TOT-ACI-GRL       PIC 9(6) VALUE 0.
          02 A-NOR-GRL         PIC 9(12)V99 VALUE 0.
          02 A-ACI-GRL         PIC 9(12)V99 VALUE 0.
          02 DEN-GRL           PIC 9(15) VALUE 0.
          02 DIAS-TBO          PIC 999 VALUE 0.
          02 DIAS-GRL          PIC 999 VALUE 0.
          02 MES-C             PIC 99 VALUE 0.
          02 ANO-C             PIC 99 VALUE 0.
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CUENTA.ERR".
       COPY "P-CTA.ERR".
       COPY "CPOSTAL.ERR".
       COPY "EMPRE.ERR".
       COPY "LECHE.ERR".
       COPY "IMP01.ERR".
       COPY "CONDI.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CCMODA" TO NOM-PAN.
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
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       LIST-NUM.
           OPEN INPUT CUENTAS P-DE-CTAS EMPRESAS LECHE CPOSTAL.
           MOVE " LISTADO RESUMEN " TO D20-CAP.
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE " <<< LISTA" TO TIT-2-EDI.
           MOVE "DO RESUMEN" TO TIT-3-EDI.
           MOVE " DE RECEP." TO TIT-4-EDI.
           MOVE " LECHE >>>" TO TIT-5-EDI.
           MOVE TITULO-EDI TO TIT-E80.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-2.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 PERFORM CIERRE-A
                            GO TO INI-MOD.
       LIST-NUM0.
           PERFORM CARGA-RUB.
           OPEN OUTPUT IMP01.
       LIST-NUM1.
           MOVE "N" TO COD2.
           PERFORM BORRA-PANTA-C1.
           PERFORM CARGA-CUE-D THRU CARGA-FEC-H.
      *           PERFORM CARGA-ZONA-L.
           PERFORM CARGA-I-NOR THRU CARGA-I-ACI.
           MOVE 0 TO SUB-CUENTA.
           MOVE NUM-EMP TO EMP-LEC.
           MOVE SUC-EMP TO SUC-LEC.
           MOVE RUBRO-L TO RUBRO-LEC RUBRO-CAP.
           MOVE CUENTA-D-CAP TO CUENTA-LEC CUENTA-CAP.
           START LECHE KEY IS NOT < CLAVE-LEC
                  INVALID KEY GO TO LIST-NUM1.
           MOVE 0 TO CONT-R CONT-I CONT-P CONT-L TOT-REC TOT-ACI.
           MOVE 0 TO TOT-NOR TOT-REC-TBO TOT-NOR-TBO TOT-ACI-TBO
                     A-ACI-TBO A-NOR-TBO DEN-TBO TOT-NOR-GRL
                     TOT-ACI-GRL TOT-REC-GRL A-NOR-GRL A-ACI-GRL
                     DEN-GRL DIAS-TBO DIAS-GRL MES-C ANO-C.
       LIST-NUM2.
           READ LECHE NEXT AT END PERFORM PIE-P
                          PERFORM CIERRE-A
                          CLOSE IMP01
                          GO TO LIST-NUM.
           IF CONT-P = 0 PERFORM TITU.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           ADD 1 TO CONT-L.
           MOVE 37 TO SW3.
           DISPLAY CONT-L LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE.
           IF CONT-I = 0 MOVE CUENTA-LEC TO CUENTA-CAP.
           IF EMP-LEC NOT = NUM-EMP PERFORM PIE-P
                                PERFORM CIERRE-A
                                CLOSE IMP01
                                GO TO LIST-NUM.
           IF SUC-LEC NOT = SUC-EMP PERFORM PIE-P
                                PERFORM CIERRE-A
                                CLOSE IMP01
                                GO TO LIST-NUM.
           IF RUBRO-LEC > RUBRO-L PERFORM PIE-P
                                PERFORM CIERRE-A
                                CLOSE IMP01
                                GO TO LIST-NUM.
           IF CUENTA-LEC < CUENTA-D-CAP GO TO LIST-NUM2.
           IF CUENTA-LEC > CUENTA-H-CAP PERFORM PIE-P
                                PERFORM CIERRE-A 
                                CLOSE IMP01
                                GO TO LIST-NUM.
           IF ANO-D-CAP > ANO-LEC GO TO LIST-NUM2.
           IF ANO-D-CAP = ANO-LEC
              IF MES-D-CAP > MES-LEC GO TO LIST-NUM2.
           IF ANO-D-CAP = ANO-LEC
              IF MES-D-CAP = MES-LEC
                 IF DIA-D-CAP > DIA-LEC GO TO LIST-NUM2.
           IF ANO-H-CAP < ANO-LEC GO TO LIST-NUM2.
           IF ANO-H-CAP = ANO-LEC
              IF MES-H-CAP < MES-LEC GO TO LIST-NUM2.
           IF ANO-H-CAP = ANO-LEC
              IF MES-H-CAP = MES-LEC
                 IF DIA-H-CAP < DIA-LEC GO TO LIST-NUM2.
           IF ANO-C = 0 MOVE ANO-LEC TO ANO-C
                        MOVE MES-LEC TO MES-C.
           IF MES-C = 0 MOVE MES-LEC TO MES-C.
      *     IF ZONA-L NOT = 0
      *         IF ZONA NOT = ZONA-L GO TO LIST-NUM2.
           IF CUENTA-CAP NOT = CUENTA-LEC
                       PERFORM PIE-T
                       MOVE 0 TO TOT-REC-TBO TOT-NOR-TBO TOT-ACI-TBO
                       MOVE 0 TO A-ACI-TBO A-NOR-TBO DEN-TBO
                       MOVE 0 TO MES-C ANO-C
                       MOVE CUENTA-LEC TO CUENTA-CAP
                       MOVE ANO-LEC TO ANO-C
                       MOVE MES-LEC TO MES-C
                       PERFORM TITU-1.
           ADD 1 TO CONT-I.
           MOVE 38 TO SW3.
           DISPLAY CONT-I LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) REVERSE. 
           MOVE 0 TO DIA-REC-ED.
           IF ANO-LEC NOT = ANO-C
              PERFORM IMPRE-RENG
              MOVE ANO-LEC TO ANO-C
              MOVE MES-LEC TO MES-C.
          IF ANO-LEC = ANO-C
            IF MES-LEC NOT = MES-C 
              PERFORM IMPRE-RENG
              MOVE ANO-LEC TO ANO-C
              MOVE MES-LEC TO MES-C.
           COMPUTE TOT-REC = TOT-REC + REC-LEC.
           COMPUTE TOT-NOR = TOT-NOR + NOR-LEC.
           COMPUTE TOT-ACI = TOT-ACI + ACI-LEC.
           COMPUTE TOT-REC-TBO = TOT-REC-TBO + REC-LEC.
           COMPUTE TOT-NOR-TBO = TOT-NOR-TBO + NOR-LEC.
           COMPUTE TOT-ACI-TBO = TOT-ACI-TBO + ACI-LEC.
           COMPUTE A-NOR-TBO = A-NOR-TBO + ( A-NOR-LEC * NOR-LEC ).
           COMPUTE A-ACI-TBO = A-ACI-TBO + ( A-ACI-TBO * ACI-LEC ).
           COMPUTE DEN-TBO = DEN-TBO + ( DEN-LEC * REC-LEC ).
           COMPUTE TOT-REC-GRL = TOT-REC-GRL + REC-LEC.
           COMPUTE TOT-NOR-GRL = TOT-NOR-GRL + NOR-LEC.
           COMPUTE TOT-ACI-GRL = TOT-ACI-GRL + ACI-LEC.
           COMPUTE A-NOR-GRL = A-NOR-GRL + ( A-NOR-LEC * NOR-LEC ).
           COMPUTE A-ACI-GRL = A-ACI-GRL + ( A-ACI-LEC * ACI-LEC ).
           COMPUTE DEN-GRL = DEN-GRL + ( DEN-LEC * REC-LEC ).
           ADD 1 TO DIAS-TBO.
           GO TO LIST-NUM2.
       IMPRE-RENG.
           MOVE 0 TO DIA-REC-ED.
           MOVE MES-C TO MES-REC-ED.
           MOVE ANO-C TO ANO-REC-ED.
           MOVE TOT-REC-TBO TO REC-ED.
           MOVE TOT-NOR-TBO TO NOR-ED.
           COMPUTE A-NOR-TBO = A-NOR-TBO / TOT-NOR-TBO.
           MOVE A-NOR-TBO TO A-NOR-ED.
           MOVE TOT-ACI-TBO TO ACI-ED.
           COMPUTE A-ACI-TBO = A-ACI-TBO / TOT-ACI-TBO.
           MOVE A-ACI-TBO TO A-ACI-ED.
           MOVE DEN-TBO = DEN-TBO / TOT-REC-TBO.
           MOVE DEN-TBO TO DEN-ED.
           MOVE SPACES TO OBS-ED.
           WRITE LIN-IMP01 FROM IMP-6 AFTER 1.
           COMPUTE PRO-REC = TOT-REC-TBO / DIAS-TBO.
           COMPUTE PRO-NOR = TOT-NOR-TBO / DIAS-TBO.
           COMPUTE PRO-ACI = TOT-ACI-TBO / DIAS-TBO.
           MOVE 0 TO PRO-A-NOR-ED PRO-A-ACI-ED PRO-DEN-ED.
           MOVE PRO-REC TO PRO-REC-ED.
           MOVE PRO-NOR TO PRO-NOR-ED.
           MOVE PRO-ACI TO PRO-ACI-ED.
           WRITE LIN-IMP01 FROM IMP-8 AFTER 1.
           MOVE 0 TO DIAS-TBO.
           ADD 1 TO CONT-R.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E80.
           PERFORM FECHA-ROT80.
           PERFORM HORA-ROT80.
           WRITE LIN-IMP01 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-3 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-2 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-4 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-5 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           ADD 8 TO CONT-R.
       TITU-1.
           MOVE EMP-LEC TO EMP-CTA.
           MOVE SUC-LEC TO SUC-CTA.
           MOVE RUBRO-LEC TO RUBRO-CTA.
           MOVE CUENTA-LEC TO CUENTA.
           MOVE 0 TO SUB-CUENTA.
           READ CUENTAS INVALID KEY MOVE "*****" TO NOMBRE-C.
           MOVE CUENTA TO CUENTA-ED.
           MOVE NOMBRE-C TO NOMBRE-C-ED.
           WRITE LIN-IMP01 FROM IMP-2.
       PIE-T.
>>>>>>>>>>>>>>>>>>
       PIE-P.
           MOVE TOT-REC TO TOT-REC-ED.
           MOVE TOT-NOR TO TOT-NOR-ED T-NOR-ED.
           MOVE TOT-ACI TO TOT-ACI-ED T-ACI-ED.
           COMPUTE TOT-NOR-CAP ROUNDED = TOT-NOR * IMP-NOR-CAP.
           COMPUTE TOT-ACI-CAP ROUNDED = TOT-ACI * IMP-ACI-CAP.
           COMPUTE TOT = TOT-NOR-CAP + TOT-ACI-CAP.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-7 AFTER 1.
           WRITE LIN-IMP01 FROM ROT80-1 AFTER 1.
           ADD 3 TO CONT-R.
           PERFORM ESPA.
           WRITE LIN-IMP01 FROM IMP-9 AFTER 1.
           PERFORM ESPA.
           MOVE TOT-NOR-CAP TO TOT-I-NOR-ED.
           MOVE TOT-ACI-CAP TO TOT-I-ACI-ED.
           MOVE TOT TO TOT-ED.
           WRITE LIN-IMP01 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-11 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-12 AFTER 1.
           WRITE LIN-IMP01 FROM IMP-13 AFTER 1.
           ADD 5 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       ESPA.
           MOVE SPACES TO LIN-IMP01.
           WRITE LIN-IMP01 AFTER 1.
           ADD 1 TO CONT-R.
       CARGA-EMP.
           PERFORM MEN-MODA-S.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-CTA EMP-LEC.
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
           MOVE NUM-CAP TO SUC-EMP SUC-CTA SUC-LEC NUM-SUC-E80.
            READ EMPRESAS INVALID KEY GO TO CARGA-SUC.       
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E80.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
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
           MOVE NUM-CAP TO P-RUBRO RUBRO-CTA RUBRO-LEC RUBRO-L.
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       CARGA-FEC.
           PERFORM MEN-ANTE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-REC-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3)  NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           MOVE DIA-REC-CAP TO DIA-LEC DIA-REC-EDI.
           MOVE MES-REC-CAP TO MES-LEC MES-REC-EDI.
           MOVE ANO-REC-CAP TO ANO-LEC ANO-REC-EDI.
           PERFORM DESP-FEC.
       CARGA-CUE.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-FEC.
           IF NUM-CAP = 0 GO TO CARGA-CUE.
           MOVE NUM-CAP TO CUENTA CUENTA-LEC.
           MOVE 0 TO SUB-CUENTA.
       CARGA-CUE-F.
           EXIT.    
       CARGA-REC.
           PERFORM MEN-ANTE-DES.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02  PERFORM DESP-REC GO TO CARGA-CUE.
           IF F03 MOVE REC-LEC TO NUM-CAP.
           MOVE NUM-CAP TO REC-LEC REC-CAP.
           PERFORM DESP-REC.
       CARGA-NOR.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-NOR GO TO CARGA-REC.
           IF F03 MOVE NOR-LEC TO NUM-CAP.
           MOVE NUM-CAP TO NOR-LEC NOR-CAP.
           PERFORM DESP-NOR.
       CARGA-A-NOR.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           ACCEPT A-NOR-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-A-NOR GO TO CARGA-NOR.
           IF F03 MOVE A-NOR-LEC TO A-NOR-CAP.
           MOVE A-NOR-CAP TO A-NOR-LEC.
           PERFORM DESP-A-NOR.
       CARGA-ACI.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO  INI-MOD.
           IF F02 PERFORM DESP-ACI GO TO CARGA-A-NOR.
           IF F03 MOVE ACI-LEC TO NUM-CAP.
           MOVE NUM-CAP TO ACI-LEC ACI-CAP.
           PERFORM DESP-ACI.
       CARGA-A-ACI.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           ACCEPT A-ACI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-A-ACI GO TO CARGA-ACI.
           IF F03 MOVE A-ACI-LEC TO A-ACI-CAP.
           MOVE A-ACI-CAP TO A-ACI-LEC.
           PERFORM DESP-A-ACI.
           COMPUTE TOTAL = ACI-LEC + NOR-LEC
           IF TOTAL NOT = REC-LEC GO TO CARGA-REC.
       CARGA-DEN.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-DEN GO TO CARGA-A-ACI.
           IF F03 MOVE DEN-LEC TO DEN-CAP.
           MOVE NUM-CAP TO DEN-LEC DEN-CAP.
           PERFORM DESP-DEN.
       CARGA-OBS.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM DESP-OBS GO TO CARGA-DEN.
           IF F03 MOVE OBS-LEC TO ALF-CAP.
           MOVE ALF-CAP TO OBS-LEC OBS-CAP.
           PERFORM DESP-OBS.
       CARGA-CUE-D.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO LIST-NUM.
           IF F02 CLOSE IMP01 GO TO LIST-NUM0.
           MOVE NUM-CAP TO CUENTA CUENTA-D-CAP.
       CARGA-CUE-H.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO LIST-NUM.
           IF F02 GO TO CARGA-CUE-D.
           MOVE NUM-CAP TO CUENTA-H-CAP.
           IF CUENTA-H-CAP < CUENTA GO TO CARGA-CUE-H.
       CARGA-FEC-D.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-D-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE-H.
           MOVE ANO-D-CAP TO ANO-LEC ANO-REC-EDI ANO-D-ED.
           MOVE MES-D-CAP TO MES-LEC MES-REC-EDI MES-D-ED.
           MOVE DIA-D-CAP TO DIA-LEC DIA-REC-EDI DIA-D-ED. 
           DISPLAY FECHA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
       CARGA-FEC-H.
           MOVE 33 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-H-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                        ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO INI-MOD.
           IF F02 GO TO CARGA-FEC-D.
           MOVE ANO-H-CAP TO ANO-REC-EDI ANO-H-ED.
           MOVE MES-H-CAP TO MES-REC-EDI MES-H-ED.
           MOVE DIA-H-CAP TO DIA-REC-EDI DIA-H-ED.
           DISPLAY FECHA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).       
       CARGA-ZONA-L.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO INI-MOD.
           IF F02 GO TO CARGA-FEC-H.
           MOVE NUM-CAP TO ZONA-L.
       CARGA-I-NOR.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           ACCEPT IMP-NOR-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO INI-MOD.
           IF F02 GO TO CARGA-ZONA-L.
           MOVE IMP-NOR-CAP TO IMP-NOR-EDI IMP-NOR-ED.
           DISPLAY IMP-NOR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            BLINK.
       CARGA-I-ACI.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           ACCEPT IMP-ACI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A CLOSE IMP01 GO TO INI-MOD.
           IF F02 GO TO CARGA-I-NOR.
           MOVE IMP-ACI-CAP TO IMP-ACI-EDI IMP-ACI-ED.
           DISPLAY IMP-ACI-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            BLINK.   
       DESP-CUE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-FEC.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE ANO-LEC TO ANO-REC-EDI.
           MOVE MES-LEC TO MES-REC-EDI.
           MOVE DIA-LEC TO DIA-REC-EDI.
           DISPLAY FECHA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
       DESP-REC.
           MOVE 13 TO SW3.
           MOVE REC-LEC TO REC-EDI.
           DISPLAY REC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-NOR.
           MOVE NOR-LEC TO NOR-EDI.
           MOVE 14 TO SW3.
           DISPLAY NOR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DESP-A-NOR.
           MOVE A-NOR-LEC TO A-NOR-EDI.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY A-NOR-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-ACI.
           MOVE ACI-LEC TO ACI-EDI.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY ACI-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) BLINK REVERSE.
       DESP-A-ACI.
           MOVE A-ACI-LEC TO A-ACI-EDI.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY A-ACI-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) BLINK REVERSE.
       DESP-DEN.
           MOVE DEN-LEC TO DEN-EDI.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           DISPLAY DEN-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
       DESP-OBS.
           MOVE OBS-LEC TO OBS-EDI.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           DISPLAY OBS-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3).
       MEN-NO-CTA.
           MOVE "-- RUBRO NO HAB" TO MEN-1.
           MOVE "ILITADO PARA CU" TO MEN-2.
           MOVE "ENTAS CTES. -- " TO MEN-3.
           PERFORM MENS-PAN.    
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "FECHA    :    " LINE 10 POSITION 5.
           DISPLAY "TAMBO    :          " LINE 12 POSITION 5.
           DISPLAY "NOMBRE   : " LINE 13 POSITION 5.
           DISPLAY "LECHE RECIBIDA : " LINE 15 POSITION 5.
           DISPLAY "LECHE NORMAL   : " LINE 16 POSITION 5.
           DISPLAY "ACIDEZ NORMAL  : " LINE 17 POSITION 5.
           DISPLAY "LECHE ACIDA    : " LINE 18 POSITION 5.
           DISPLAY "ACIDEZ ACIDA   : " LINE 19 POSITION 5.
           DISPLAY "DENSIDAD LECHE : " LINE 20 POSITION 5.
           DISPLAY "OBSERVACIONES  : " LINE 21 POSITION 5.
       PANTA-2.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "RUBRO    :          - " LINE 8 POSITION 5.
           DISPLAY "------------ D E S D E -------------" LINE 11
                               POSITION 5.
           DISPLAY "TAMBO    :           " LINE 12 POSITION 5.
           DISPLAY "------------ H A S T A -------------" LINE 13
                               POSITION 5.
           DISPLAY "TAMBO    :          " LINE 14 POSITION 5.
           DISPLAY "------------ D E S D E -------------" LINE 15
                               POSITION 5.
           DISPLAY "FECHA    :           " LINE 16 POSITION 5.
           DISPLAY "------------ H A S T A -------------" LINE 17
                               POSITION 5.
           DISPLAY "FECHA    :          " LINE 18 POSITION 5.
           DISPLAY "ZONA     :" LINE 20 POSITION 5.
           DISPLAY " INGRESE -0- LISTA " LINE 19 POSITION 25 REVERSE.
           DISPLAY "      TODAS        " LINE 20 POSITION 25 REVERSE.
           DISPLAY "   LAS  ZONAS      " LINE 21 POSITION 25 REVERSE.
           DISPLAY "$ NORMAL :" LINE 22 POSITION 5 BLINK.
           DISPLAY "$ ACIDA  :" LINE 22 POSITION 35 BLINK.
           DISPLAY "REG. LEIDOS :" LINE 23 POSITION 5.
           DISPLAY "REG. IMPRESOS :" LINE 23 POSITION 35.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 081607 TO TAB-POS(7).
           MOVE 083030 TO TAB-POS(8).
           MOVE 121606 TO TAB-POS(9).
           MOVE 103004 TO TAB-POS(10).
           MOVE 131630 TO TAB-POS(11).
           MOVE 101608 TO TAB-POS(12).
           MOVE 152307 TO TAB-POS(13).
           MOVE 162307 TO TAB-POS(14).
           MOVE 172304 TO TAB-POS(15).
           MOVE 182307 TO TAB-POS(16).
           MOVE 192304 TO TAB-POS(17).
           MOVE 202304 TO TAB-POS(18).
           MOVE 212315 TO TAB-POS(19).
           MOVE 171601 TO TAB-POS(20).
           MOVE 173715 TO TAB-POS(21).
           MOVE 192719 TO TAB-POS(22).
           MOVE 202719 TO TAB-POS(23).
           MOVE 212719 TO TAB-POS(24).
           MOVE 121606 TO TAB-POS(30).
           MOVE 161608 TO TAB-POS(31).
           MOVE 141606 TO TAB-POS(32).
           MOVE 181607 TO TAB-POS(33).
           MOVE 201602 TO TAB-POS(34).
           MOVE 121630 TO TAB-POS(35).
           MOVE 151630 TO TAB-POS(36).
           MOVE 231906 TO TAB-POS(37).
           MOVE 235006 TO TAB-POS(38).
           MOVE 181615 TO TAB-POS(39).
           MOVE 184603 TO TAB-POS(40).
           MOVE 185015 TO TAB-POS(41).
           MOVE 221611 TO TAB-POS(42).
           MOVE 224611 TO TAB-POS(43).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO DE RECE" TO TIT-2-PAN.
           MOVE "P.LECHE - " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO NUMERICO " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
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
       BORRA-PANTA-C1.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           MOVE 33 TO SW3.
           PERFORM  BORRA.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           MOVE 37 TO SW3.
           PERFORM BORRA.
           MOVE 38 TO SW3.
           PERFORM BORRA.
       CIERRE-A.
           CLOSE CUENTAS P-DE-CTAS LECHE EMPRESAS CPOSTAL.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT80.PRO".
       COPY "CLAVES.PRO".