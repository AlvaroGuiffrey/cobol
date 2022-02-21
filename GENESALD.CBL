
000000 IDENTIFICATION DIVISION.
       PROGRAM-ID. RESCTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.         AMI.
       OBJECT-COMPUTER.         AMI.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            COPY "CUENTA.SEL".
            SELECT MOVI ASSIGN TO RANDOM "MOVI"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CLAVE-MOV.
       DATA DIVISION.
       FILE SECTION.
           COPY "MOVI.REG".
           COPY "CUENTABM.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
           03  EMP-CAP      PIC 9(4) VALUE 0.
           03  SUC-CAP      PIC 99   VALUE 0.
           03  RUB-CAP      PIC 9(6) VALUE 0.
           03  CTA-CAP      PIC 9(6) VALUE 0.
           03  S-C-CAP      PIC 9(4) VALUE 0.
           03  SALDO-A      PIC S9(12)V99 VALUE 0.
           03  NEUTRO       PIC 9(12)V99 VALUE 0.

      *     03  QQ           PIC X.
       PROCEDURE DIVISION.
       ABRO. 
           OPEN I-O CUENTAS  
                INPUT MOVI.  
       PONE-CERO.         
           READ CUENTAS NEXT AT END CLOSE CUENTAS
                                     OPEN I-O CUENTAS
                                     GO TO LEA-MOV.
           MOVE 0 TO SALDO
           REWRITE REG-CTAS INVALID KEY DISPLAY "NO REGRABO"
                                        ACCEPT QQ.
           GO TO PONE-CERO.

       LEA-MOV.
           READ MOVI NEXT AT END PERFORM CIERRE
                                 CLOSE MOVI CUENTAS
                                 STOP RUN.
       GIRA.
           IF EMP-CAP = 0 
                MOVE EMP-MOV        TO EMP-CAP
                MOVE SUC-EMP-MOV    TO SUC-CAP
                MOVE RUBRO-MOV      TO RUB-CAP
                MOVE CUENTA-MOV     TO CTA-CAP
                MOVE SUB-CUENTA-MOV TO S-C-CAP
                MOVE 0              TO SALDO-A.

           IF EMP-CAP  NOT = EMP-MOV OR
              SUC-CAP  NOT = SUC-EMP-MOV OR
              RUB-CAP  NOT = RUBRO-MOV OR
              CTA-CAP  NOT = CUENTA-MOV OR
              S-C-CAP  NOT = SUB-CUENTA-MOV GO TO CIERRE.

           MOVE IMPORTE-MOV TO NEUTRO
           IF TIPO-MOV = 1 COMPUTE SALDO-A = SALDO-A + NEUTRO.
           IF TIPO-MOV = 2 COMPUTE SALDO-A = SALDO-A - NEUTRO.
           GO TO LEA-MOV.

       CIERRE.
           MOVE EMP-CAP     TO EMP-CTA
           MOVE SUC-CAP     TO SUC-CTA
           MOVE RUB-CAP     TO RUBRO-CTA
           MOVE CTA-CAP     TO CUENTA
           MOVE S-C-CAP     TO SUB-CUENTA
           READ CUENTAS INVALID KEY DISPLAY "NO EXISTE CUENTA"
                                    ACCEPT QQ
                                    GO TO SALIENDO.
           MOVE SALDO-A   TO SALDO
           REWRITE REG-CTAS INVALID KEY DISPLAY "NO REGRABO"
                                        ACCEPT QQ.
       SALIENDO.
           MOVE 0 TO EMP-CAP
           GO TO GIRA.


