       IDENTIFICATION DIVISION.
       PROGRAM-ID. AJU-PEN.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. JUL'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "EMPRE.SEL".
       COPY "PENDI.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "EMPREBM.REG".
       COPY "PENDI.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 NOMBRE-CAP        PIC X(45).
          02 ESTADO-CAP        PIC XX.
          02 CONDICION-CAP     PIC 999 VALUE 0.
          02 DIRE-CAP          PIC X(30).
          02 COD-LOCA-CAP      PIC 9(4) VALUE 0.
          02 INSCRIPTO-CAP     PIC X.
          02 ZONA-CAP          PIC 99 VALUE 0.
          02 IGAN-CAP          PIC X(15).
          02 SALDO-ANT-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-CAN-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-TOP-CAP     PIC S9(13)V99 VALUE 0. 
          02 SALDO-CAP         PIC S9(13)V99 VALUE 0.
          02 TEL-CAP           PIC X(10).
          02 PMOV-CAP          PIC 9(6) VALUE 0.
          02 PPEND-CAP         PIC 9(6) VALUE 0.
          02 ING-BRUTOS-CAP    PIC 9(8) VALUE 0.
          02 COM-1-CAP         PIC X(50).
          02 COM-2-CAP         PIC X(50).
          02 COM-3-CAP         PIC X(50).
          02 COM-4-CAP         PIC X(50).
          02 IMP-CAP           PIC 9(13)V99 VALUE 0.
          02 SALDO-CON         PIC 9(13)V99 VALUE 0.
          02 IMPORTE-C-PEND    PIC 9(13)V99 VALUE 0.
          02 IMPORTE-D-PEND     PIC 9(13)V99 VALUE 0.
          02 ACUENTA-I-PEND    PIC 9(13)V99 VALUE 0.
          02 AYUDA             PIC 9(13)V99 VALUE 0.
          02 C-VAL             PIC 9 VALUE 0.
          02 EMP-I      PIC 9(4) VALUE 0.
          02 SUC-EMP-I  PIC 9(2) VALUE 0.
          02 RUBRO-I    PIC 9(7) VALUE 0.
          02 CUENTA-I   PIC 9(6) VALUE 0.
          02 SUB-CUENTA-I PIC 9(4) VALUE 0.
          02 FEC-I.
            03 ANO-I    PIC 9999 VALUE 0.
            03 MES-I    PIC 99 VALUE 0. 
            03 DIA-I    PIC 99 VALUE 0.
          02 PENDIENTE-I PIC 9(6) VALUE 0.
      *-=-=-=-=-=- AGREGADO POR CAJA =-=-=-=-=-=-=-=-=
          02 IMP-P-CAP         PIC 9(13)V99 VALUE 0.
          02 BCO-CAP           PIC 999 VALUE 0.
          02 CH-CPOSTAL-CAP    PIC 9(4) VALUE 0.
          02 CH-NUM-CAP        PIC 9(10) VALUE 0.
          02 CH-FEC-VENC-CAP.
             03 CH-DIA-VENC-CAP   PIC 99 VALUE 0.
             03 CH-MES-VENC-CAP   PIC 99 VALUE 0.
             03 CH-ANO-VENC-CAP   PIC 9999 VALUE 0.
          02 TIPO-CAP        PIC X VALUE SPACES.
          02 CONT-CH-CAP         PIC 9 VALUE 0.
          02 IMP-P-ED            PIC ZZZZ.ZZZ.ZZZ.ZZ9,99.
          02 CH-FEC-VENC-ED.
             03 CH-DIA-VENC-ED   PIC 99/.
             03 CH-MES-VENC-ED   PIC 99/.
             03 CH-ANO-VENC-ED   PIC 9999.
          02 DATOS-CHEQUE.
             03 BCO-D-ED         PIC 9(4).
             03 FILLER           PIC X VALUE " ".
             03 BCO-NOM-ED       PIC X(15).
             03 FILLER           PIC XXX VALUE " -".
             03 CH-CPOSTAL-D-ED     PIC 9(4).
             03 FILLER           PIC X(7) VALUE " - N':".
             03 CH-NUM-D-ED      PIC 9(10).
          02 DATOS-CHEQUE1.
             03 FILLER           PIC X(9) VALUE "FECHA :".
             03 CH-FEC-VENC-D-ED.
                05 CH-DIA-VENC-D-ED  PIC 99.
                05 FILLER            PIC X VALUE "/".
                05 CH-MES-VENC-D-ED  PIC 99.
                05 FILLER            PIC X VALUE "/".
                05 CH-ANO-VENC-D-ED  PIC 9999.
          02 DATOS-CHEQUE2.
             03 BCO-1-ED       PIC 9999.
             03 FILLER         PIC X VALUE "-".
             03 CH-CPOSTAL-1-ED  PIC 9(4).
             03 FILLER         PIC X VALUE "-".
             03 CH-NUM-1-ED    PIC 9(10).
             03 FILLER         PIC X VALUE "-".
             03 FEC-1-ED.
                05 DIA-1-ED    PIC 99.
                05 FILLER      PIC X VALUE "/".
                05 MES-1-ED    PIC 99.
                05 FILLER      PIC X VALUE "/".
                05 ANO-1-ED    PIC 9999.
          02 TURNO-CAP          PIC 99 VALUE 0.
          02 FEC-C-1.
             03 ANO-C-1         PIC 9999.
             03 MES-C-1         PIC 99.
             03 DIA-C-1         PIC 99.
          02 FEC-C-2.
             03 ANO-C-2         PIC 9999.
             03 MES-C-2         PIC 99.
             03 DIA-C-2         PIC 99.          
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       01 EDICION.
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 SUB-CUENTA-EDI    PIC ZZ BLANK WHEN ZERO.
          02 NOMBRE-EDI      PIC X(45).
          02 ESTADO-EDI        PIC XX.
          02 CONDICION-EDI     PIC ZZZ BLANK WHEN ZERO.
          02 DIRE-EDI          PIC X(30).
          02 LOCA-IDE          PIC X(29).
          02 INSCRIPTO-EDI     PIC X.
          02 ZONA-EDI          PIC ZZ BLANK WHEN ZERO.
          02 IGAN-EDI          PIC X(15).
          02 SAL-C-EDI         PIC ----.---.---.--9,99.
          02 SAL-EDI           PIC ----.---.---.--9,99.
          02 SAL-T-EDI         PIC ----.---.---.--9,99.
          02 TEL-EDI           PIC X(10).
          02 IMP-EDI           PIC ZZZZ.ZZZ.ZZZ.ZZ9,99.
       01 AUXILIARES.
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
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "PENDI.ERR".
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
           IF SW2 > 25 GO TO SIGA-INI.
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
           IF F01 GO TO TAREA1.
           GO TO CARGO-MOD.
       TAREA1.
           OPEN INPUT EMPRESAS.
           OPEN I-O  PENDI.
           MOVE " AJUSTA PENDIENT. " TO D20-CAP.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
       TAREA1-1.
           PERFORM BORRA-PANTA-C.
           PERFORM CARGA-FEC-VENC.
       VEO-PENDI.
           MOVE 0 TO RUBRO-PEND CUENTA-PEND SUB-CUENTA-PEND.
           MOVE 0 TO PENDIENTE-PEND DIA-PEND MES-PEND ANO-PEND.
           MOVE 0 TO IMPORTE-D-PEND IMPORTE-C-PEND.
           MOVE CH-DIA-VENC-CAP TO DIA-C-1.
           MOVE CH-MES-VENC-CAP TO MES-C-1.
           MOVE CH-ANO-VENC-CAP TO ANO-C-1.
           START PENDI KEY IS NOT < CLAVE-PEND
                 INVALID KEY GO TO PENDI-F.
           GO TO ANALISO-PENDI.
       POSI-PENDI.
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           START PENDI KEY IS > CLAVE-PEND
                 INVALID KEY GO TO PENDI-F.
       ANALISO-PENDI.
           READ PENDI NEXT AT END GO TO  PENDI-F.
       ANALISO-0.
           MOVE 0 TO IMPORTE-C-PEND.
           IF EMP-PEND NOT = NUM-EMP GO TO  PENDI-F.
           IF SUC-EMP-PEND NOT = SUC-EMP GO TO PENDI-F.
           PERFORM MUESTRO-PENDI.
           IF VENCIMIENTO-PEND > FEC-C-1 GO TO ANALISO-PENDI. 
           IF D-H-PEND = 1 GO TO CRE-PENDI.
           MOVE 0 TO IMPORTE-C-PEND.
           MOVE EMP-PEND TO EMP-I.
           MOVE SUC-EMP-PEND TO SUC-EMP-I.
           MOVE RUBRO-PEND TO RUBRO-I.
           MOVE CUENTA-PEND TO CUENTA-I.
           MOVE SUB-CUENTA-PEND TO SUB-CUENTA-I.
           MOVE ANO-PEND TO ANO-I.
           MOVE MES-PEND TO MES-I. 
           MOVE DIA-PEND TO DIA-I.
           MOVE PENDIENTE-PEND TO PENDIENTE-I.
           COMPUTE IMPORTE-C-PEND = IMPORTE-PEND - ACUENTA-PEND.
       ANALISO-1.
           MOVE 1 TO D-H-PEND.
           MOVE 0 TO PENDIENTE-PEND ANO-PEND MES-PEND DIA-PEND.
           START PENDI KEY IS NOT < CLAVE-PEND 
                INVALID KEY GO TO PENDI-F.
       ANALISO-2.
           READ PENDI NEXT AT END GO TO PENDI-F.
       ANALISO-21.
           PERFORM MUESTRO-PENDI.
           IF NUM-EMP NOT = EMP-PEND GO TO PENDI-F.
           IF SUC-EMP NOT = SUC-EMP-PEND GO TO PENDI-F.
           IF RUBRO-I NOT = RUBRO-PEND GO TO POSI-PENDI.
           IF CUENTA-I NOT = CUENTA-PEND GO TO POSI-PENDI.
           IF SUB-CUENTA-I NOT = SUB-CUENTA-PEND GO TO POSI-PENDI.
           IF D-H-PEND NOT = 1 GO TO POSI-PENDI. 
           MOVE 0 TO IMPORTE-D-PEND.
           COMPUTE IMPORTE-D-PEND = IMPORTE-PEND - ACUENTA-PEND.              
           IF IMPORTE-D-PEND > IMPORTE-C-PEND GO TO MENOR-PENDI.
           IF IMPORTE-D-PEND < IMPORTE-C-PEND GO TO MAYOR-PENDI.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-111".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-222".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI.
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       MAYOR-PENDI.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           DELETE PENDI INVALID KEY DISPLAY "NO PUDE BORRAR 222".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           READ PENDI INVALID KEY DISPLAY "MO PUDE LEER 111".
           COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-D-PEND. 
           REWRITE REG-PEND INVALID KEY DISPLAY 
                            "NO PUDE REGRABAR PENDI".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI.
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       MENOR-PENDI.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-C-PEND.
           REWRITE REG-PEND INVALID KEY 
               DISPLAY "NO PUDE REGRADAR PENDI111".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-333".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI. 
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       CRE-PENDI.
           MOVE 0 TO IMPORTE-D-PEND.
           MOVE EMP-PEND TO EMP-I.
           MOVE SUC-EMP-PEND TO SUC-EMP-I.
           MOVE RUBRO-PEND TO RUBRO-I.
           MOVE CUENTA-PEND TO CUENTA-I.
           MOVE SUB-CUENTA-PEND TO SUB-CUENTA-I.
           MOVE ANO-PEND TO ANO-I.
           MOVE MES-PEND TO MES-I. 
           MOVE DIA-PEND TO DIA-I.
           MOVE PENDIENTE-PEND TO PENDIENTE-I.
           COMPUTE IMPORTE-D-PEND = IMPORTE-PEND - ACUENTA-PEND.
       CRE-PENDI-1.
           MOVE 2 TO D-H-PEND.
           MOVE 0 TO PENDIENTE-PEND ANO-PEND MES-PEND DIA-PEND.
           START PENDI KEY IS NOT < CLAVE-PEND 
                INVALID KEY GO TO PENDI-F.
       CRE-PENDI-2.
           READ PENDI NEXT AT END GO TO PENDI-F.
       CRE-PENDI-21.
           PERFORM MUESTRO-PENDI.
           IF NUM-EMP NOT = EMP-PEND GO TO PENDI-F.
           IF SUC-EMP NOT = SUC-EMP-PEND GO TO PENDI-F.
           IF RUBRO-I NOT = RUBRO-PEND GO TO POSI-PENDI.
           IF CUENTA-I NOT = CUENTA-PEND GO TO POSI-PENDI.
           IF SUB-CUENTA-I NOT = SUB-CUENTA-PEND GO TO POSI-PENDI.
           IF D-H-PEND NOT = 2 GO TO POSI-PENDI. 
           MOVE 0 TO IMPORTE-C-PEND.
           COMPUTE IMPORTE-C-PEND = IMPORTE-PEND - ACUENTA-PEND.              
           IF IMPORTE-C-PEND > IMPORTE-D-PEND GO TO MENOR-CRE.
           IF IMPORTE-C-PEND < IMPORTE-D-PEND GO TO MAYOR-CRE.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-111".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-222".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI.
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       MAYOR-CRE.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           DELETE PENDI INVALID KEY DISPLAY "NO PUDE BORRAR 222".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           READ PENDI INVALID KEY DISPLAY "MO PUDE LEER 111".
           COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-C-PEND. 
           REWRITE REG-PEND INVALID KEY DISPLAY 
                            "NO PUDE REGRABAR PENDI".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI.
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       MENOR-CRE.
      *     CLOSE PENDI.
      *     OPEN I-O PENDI.
           COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-D-PEND.
           REWRITE REG-PEND INVALID KEY 
               DISPLAY "NO PUDE REGRADAR PENDI111".
           MOVE EMP-I TO EMP-PEND.
           MOVE SUC-EMP-I TO SUC-EMP-PEND.
           MOVE RUBRO-I TO RUBRO-PEND.
           MOVE CUENTA-I TO CUENTA-PEND.
           MOVE SUB-CUENTA-I TO SUB-CUENTA-PEND.
           MOVE ANO-I TO ANO-PEND.
           MOVE MES-I TO MES-PEND. 
           MOVE DIA-I TO DIA-PEND.
           MOVE PENDIENTE-I TO PENDIENTE-PEND.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-333".
      *     CLOSE PENDI.
      *     OPEN INPUT PENDI. 
           START PENDI KEY IS NOT < CLAVE-PEND INVALID KEY
                       DISPLAY "NO PUDE POSI 2222".
           GO TO ANALISO-PENDI.
       PENDI-F.
           PERFORM CIERRE-A.
           EXIT PROGRAM.
           STOP RUN.             
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
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.       
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
       CARGA-EMP-F.
           EXIT.
       CARGA-FEC-VENC.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           ACCEPT CH-FEC-VENC-CAP LINE TAB-LIN(SW3) 
                  POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE CH-DIA-VENC-CAP TO CH-DIA-VENC-ED.
           MOVE CH-MES-VENC-CAP TO CH-MES-VENC-ED.
           MOVE CH-ANO-VENC-CAP TO CH-ANO-VENC-ED.
           DISPLAY CH-FEC-VENC-ED LINE TAB-LIN(SW3) 
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3) REVERSE.
       MUESTRO-PENDI.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY RUBRO-PEND LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3).
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY CUENTA-PEND LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY PENDIENTE-PEND LINE TAB-LIN(SW3)
                       POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY D-H-PEND LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).            
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "FECHA    :           " LINE 9 POSITION 5.
           DISPLAY "- CONTROL DE PROCESO -" LINE 12 POSITION 5.
           DISPLAY "RUBRO    : "  LINE 14 POSITION 5.
           DISPLAY "CUENTA   : "  LINE 15 POSITION 5.
           DISPLAY "PENDIENTE: "  LINE 16 POSITION 5.
           DISPLAY "TIPO PEND: "  LINE 17 POSITION 5.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 091610 TO TAB-POS(7).
           MOVE 141608 TO TAB-POS(8).
           MOVE 151608 TO TAB-POS(9).
           MOVE 161608 TO TAB-POS(10).
           MOVE 171608 TO TAB-POS(11).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO DE CTAS" TO TIT-2-PAN.
           MOVE ". CTES.-  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  AJUSTA PENDIENT." LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
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
       CIERRE-A.
           CLOSE EMPRESAS PENDI.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".