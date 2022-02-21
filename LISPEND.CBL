       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISPEND.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. DIC'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *...............................(*.SEL)
       COPY "PENDI.SEL".
       COPY "IMP02.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
      *...............................(*.REG)
       COPY "PENDI.REG".
       COPY "IMP02.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
      *...............................(ROTULO DE IMPRESION)
       COPY "ROT132.IMP".
      *...............................(LINEAS DE IMPRESION)
       01 IMPRESION.
          02 IMP-1.
             03 EMP-ED     PIC 9999B.
             03 SUC-EMP-ED PIC 99B.
             03 RUBRO-ED   PIC 9(7)B.
             03 CUENTA-ED PIC 9(6)B.
             03 SUB-CUENTA-ED  PIC 9999B.
             03 DIA-ED     PIC 99/.
             03 MES-ED     PIC 99/.
             03 ANO-ED     PIC 99B.
             03 PENDIENTE-ED  PIC 9(6)B.
             03 TIPO-ED   PIC 9B.
             03 SUB-T-ED PIC X.
             03 SUC-T-ED   PIC 9(4).
             03 COMPRO-ED     PIC 9(8)B.
             03 DETA-ED PIC X(24)B.
             03 D-H-ED  PIC 9.
             03 IMPORTE-ED PIC --------------9,99B.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
      *...............................(*.ERR)
       COPY "PENDI.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
      *...............................(ABRE PANTALLA)
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
        SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
       CARGO-MOD.
      *...............................(SELECCIONA TAREAS)
           MOVE 28 TO SW3.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP STOP RUN.
           IF F01 GO TO LISTO.
           GO TO CARGO-MOD.
       
      *................................(PROGRAMA PRINCIPAL)
       LISTO.
           MOVE 0 TO EMP-PEND RUBRO-PEND CUENTA-PEND.
           MOVE 0 TO SUC-EMP-PEND ANO-PEND MES-PEND DIA-PEND.
           MOVE 0 TO SUB-CUENTA-PEND PENDIENTE-PEND.
           OPEN INPUT PENDI.
           OPEN OUTPUT IMP02.
           START PENDI KEY IS NOT < CLAVE-PEND
                 INVALID KEY PERFORM CIERRE-A
                             STOP RUN.
       LISTO-1.
           READ PENDI NEXT AT END PERFORM CIERRE-A
                      GO TO INI-MOD.
           MOVE EMP-PEND TO EMP-ED.
           MOVE SUC-EMP-PEND TO SUC-EMP-ED.
           MOVE RUBRO-PEND TO RUBRO-ED.
           MOVE CUENTA-PEND TO CUENTA-ED.
           MOVE SUB-CUENTA-PEND TO SUB-CUENTA-ED
           MOVE ANO-PEND TO ANO-ED.
           MOVE MES-PEND TO MES-ED.
           MOVE DIA-PEND TO DIA-ED
           MOVE PENDIENTE-PEND TO PENDIENTE-ED
           MOVE TIPO-PEND TO TIPO-ED.
           MOVE SUB-T-PEND TO SUB-T-ED.
           MOVE SUC-T-PEND TO SUC-T-ED.
           MOVE COMPRO-PEND TO COMPRO-ED.
           MOVE DETA-PEND TO DETA-ED.
           MOVE D-H-PEND TO D-H-ED.
           MOVE IMPORTE-PEND TO IMPORTE-ED.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           GO TO LISTO-1.
       CIERRE-A.
           CLOSE IMP02 PENDI.
      *................................(CARGA DATOS)

      *................................(DESPLEGA DATOS)

       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE "  LISTADO " TO TIT-1-PAN.
           MOVE " DE  PENDI" TO TIT-2-PAN.
           MOVE "ENTES     " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  LISTADO " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "ROT132.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
  