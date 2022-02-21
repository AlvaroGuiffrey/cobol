       IDENTIFICATION DIVISION.
       PROGRAM-ID. TMOVI.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. DIC'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "IMP02.SEL".
       COPY "MOVI.SEL".
       COPY "TMOVI.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "IMP02.REG".
       COPY "TMOVI.REG".
       COPY "MOVI.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 LIN-1.
             03 RUBRO-ED     PIC X(7).
             03 FILLER       PIC X(4) VALUE SPACES.
             03 DD-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MM-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 AA-ED        PIC 99B(4).
             03 DEBITO-ED    PIC --.---.---.--9,99 BLANK WHEN ZERO.
             03 FILLER       PIC X(4) VALUE SPACES.
             03 CREDITO-ED   PIC --.---.---.--9,99 BLANK WHEN ZERO.
             03 SALDO-ED     PIC --.---.---.--9,99 BLANK WHEN ZERO.
       01 CAPTURA.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 FECHA-H.
              03 DIA-H    PIC 99 VALUE 0.
              03 MES-H    PIC 99 VALUE 0.
              03 ANO-H    PIC 99 VALUE 0.
       01 AUXILIARES.
          02 RUBRO-H           PIC 9(7) VALUE 0.
          02 CONT-LEO          PIC 9(7) VALUE 0.
          02 COD-TAREA         PIC 9 VALUE 0.
          02 SALDO             PIC S9(12)V99 VALUE 0.
       COPY "ABREV.WOR".
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "IMP02.ERR".
       COPY "MOVI.ERR".
       COPY "TMOVI.ERR".
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
           IF SW2 = 25 GO TO SIGA-INI.
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
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           MOVE 0 TO COD-TAREA.
           IF F01 GO TO TAREA1.
           GO TO CARGO-MOD.
       TAREA1.
           OPEN INPUT MOVI.
           OPEN OUTPUT IMP02 TMOVI.
           CLOSE TMOVI.
           OPEN I-O TMOVI.
           MOVE "RESUMEN DE MOVIM." TO D20-CAP.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-RUB.
       TAREA1-1.
           PERFORM BORRA-PANTA-C.
       TAREA1-12.
           MOVE 0 TO EMP-MOV SUC-EMP-MOV RUBRO-MOV CUENTA-MOV
                     SUB-CUENTA-MOV FECHA-MOV MOVIMIENTO-MOV
                     CONT-LEO.
           START MOVI KEY IS NOT < CLAVE-MOV
                 INVALID KEY PERFORM CIERRE-A
                             GO TO INI-MOD.
       TAREA2.
           READ MOVI NEXT AT END GO TO IMPRE.
           IF RUBRO-CAP NOT = RUBRO-MOV GO TO TAREA2.
           MOVE EMP-MOV TO EMP-TMOV.
           MOVE SUC-EMP-MOV TO SUC-EMP-TMOV.
           MOVE RUBRO-MOV TO RUBRO-TMOV.
           MOVE ANO-MOV TO ANO-TMOV.
           MOVE MES-MOV TO MES-TMOV.
           MOVE DIA-MOV TO DIA-TMOV.
           MOVE TIPO-MOV TO TIPO-TMOV.
           ADD 1 TO CONT-LEO.
           READ TMOVI INVALID KEY MOVE 0 TO IMPORTE-TMOV.
           COMPUTE IMPORTE-TMOV = IMPORTE-TMOV + IMPORTE-MOV.
           REWRITE REG-TMOV INVALID KEY WRITE REG-TMOV.
           DISPLAY CONT-LEO LINE 18 POSITION 20.
           GO TO TAREA2.
       IMPRE.
           CLOSE TMOVI.
           OPEN INPUT TMOVI.
           MOVE 0 TO EMP-TMOV SUC-EMP-TMOV RUBRO-TMOV FECHA-TMOV
                     TIPO-TMOV.
           WRITE LIN-IMP02 FROM COMPRI.
           START TMOVI KEY IS NOT < CLAVE-TMOV
                       INVALID KEY PERFORM CIERRE-A
                                   GO TO INI-MOD.
       IMPRE-1.
           READ TMOVI NEXT AT END PERFORM CIERRE-A
                                  GO TO INI-MOD.
           MOVE RUBRO-TMOV TO RUBRO-ED.
           MOVE ANO-TMOV TO AA-ED.
           MOVE MES-TMOV TO MM-ED.
           MOVE DIA-TMOV TO DD-ED.
           MOVE 0 TO DEBITO-ED CREDITO-ED SALDO-ED.
           IF TIPO-TMOV = 1 MOVE IMPORTE-TMOV TO DEBITO-ED
                  COMPUTE SALDO = SALDO + IMPORTE-TMOV
              ELSE MOVE IMPORTE-TMOV TO CREDITO-ED
                   COMPUTE SALDO = SALDO - IMPORTE-TMOV.
           MOVE SALDO TO SALDO-ED.
           IF ANO-TMOV < ANO-H GO TO IMPRE-1.
           IF ANO-TMOV = ANO-H
              IF MES-TMOV < MES-H GO TO IMPRE-1.
           IF ANO-TMOV = ANO-H
              IF MES-TMOV = MES-H
                 IF DIA-TMOV < DIA-H GO TO IMPRE-1. 
           WRITE LIN-IMP02 FROM LIN-1 AFTER 1.
           GO TO IMPRE-1.
       CARGA-RUB.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO RUBRO-CAP.
           ACCEPT FECHA-H LINE 10 POSITION 22 SIZE 6.
       PANTA-1.
           DISPLAY "RUBRO    :           " LINE 8 POSITION 5.
           DISPLAY "FECHA DESDE : " LINE 10 POSITION 5.           
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
           MOVE 101606 TO TAB-POS(9).
           MOVE 103004 TO TAB-POS(10).
           MOVE 111630 TO TAB-POS(11).
           MOVE 131606 TO TAB-POS(12).
           MOVE 133004 TO TAB-POS(13).
           MOVE 141630 TO TAB-POS(14).
           MOVE 161602 TO TAB-POS(15).
           MOVE 202006 TO TAB-POS(16).
           MOVE 182008 TO TAB-POS(17).
           MOVE 192008 TO TAB-POS(18).
           MOVE 091118 TO TAB-POS(19).
           MOVE 093008 TO TAB-POS(20).
           MOVE 093919 TO TAB-POS(21).
           MOVE 095919 TO TAB-POS(22).
           MOVE 132719 TO TAB-POS(23).
           MOVE 152719 TO TAB-POS(24).
           MOVE 172719 TO TAB-POS(25).
           MOVE 121606 TO TAB-POS(30).
           MOVE 123004 TO TAB-POS(31).
           MOVE 151606 TO TAB-POS(32).
           MOVE 153004 TO TAB-POS(33).
           MOVE 181602 TO TAB-POS(34).
           MOVE 121630 TO TAB-POS(35).
           MOVE 151630 TO TAB-POS(36).
           MOVE 211906 TO TAB-POS(37).
           MOVE 215006 TO TAB-POS(38).
           MOVE 181050 TO TAB-POS(40).
           MOVE 191050 TO TAB-POS(41).
           MOVE 201050 TO TAB-POS(42).
           MOVE 211050 TO TAB-POS(43).
           MOVE 121812 TO TAB-POS(44).
           MOVE 071908 TO TAB-POS(45).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO DE MOVI" TO TIT-2-PAN.
           MOVE "MIENTOS - " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  RESUMEN DE MOVIM." LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 44 TO SW3.
           PERFORM BORRA.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 19 POSITION 19 SIZE 40.
           DISPLAY SPACES LINE 20 POSITION 19 SIZE 40.
           DISPLAY SPACES LINE 21 POSITION 19 SIZE 40.
       CIERRE-A.
           CLOSE MOVI TMOVI IMP02.
           OPEN OUTPUT TMOVI.
           CLOSE TMOVI.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".