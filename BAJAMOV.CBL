       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESCTA.
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
       COPY "CUENTA.SEL".
       COPY "MOVI.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CUENTABM.REG".
       COPY "MOVI.REG".
       WORKING-STORAGE SECTION.
           COPY "VARIOS.WOR".
       01  ELREG             PIC X(300).

       PROCEDURE DIVISION.
       ABRO.  
            OPEN I-O CUENTAS MOVI.
            DISPLAY SPACES LINE 1 POSITION  1 ERASE
            DISPLAY "BORRADO DE MOVIMIENTOS" LINE 10 POSITION 20.
       LEA.
            READ MOVI NEXT AT END CLOSE MOVI CUENTAS
                                         STOP RUN.
            IF COMPRO-MOV NOT = 99999 GO TO LEA.
            IF TIPO-C-MOV NOT = 9     GO TO LEA.

            MOVE EMP-MOV           TO EMP-CTA
            MOVE SUC-EMP-MOV           TO SUC-CTA
            MOVE RUBRO-MOV         TO RUBRO-CTA
            MOVE CUENTA-MOV        TO CUENTA
            MOVE SUB-CUENTA-MOV    TO SUB-CUENTA
            
            READ CUENTAS INVALID KEY DISPLAY "NO CUENTAS"
                                     GO TO LEA.
            IF CONDICION = 51 GO TO LEA.

            MOVE REG-MOV TO ELREG
            COMPUTE SALDO = SALDO - IMPORTE-MOV 
            REWRITE REG-CTAS INVALID KEY DISPLAY "IMP-CTAS".
            MOVE REG-MOV TO ELREG
            DELETE MOVI INVALID KEY DISPLAY "NO BAJO".
            MOVE ELREG   TO REG-MOV
            START MOVI KEY NOT LESS CLAVE-MOV INVALID KEY
                              DISPLAY "NO REPO"
                              STOP RUN.
            GO TO LEA.
