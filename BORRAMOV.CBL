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
       COPY "MOVI.SEL".
       COPY "MOVIX.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MOVI.REG".
       COPY "MOVIX.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  ELREG             PIC X(300).
       01  DC                PIC 9(6) VALUE 0.
       01  HC                PIC 9(6) VALUE 0.
       01  ESTA-CUENTA       PIC 9(6) VALUE 0.
       01  ESTE-RUBRO        PIC 9(6) VALUE 0.
       01  ESTE              PIC 9(6) VALUE 0.
       01  IMPORTE           PIC S9(12)V99 VALUE 0.
       01  ELIMP             PIC 9(12)V99  VALUE 0.
       PROCEDURE DIVISION.
       ABRO.  
            OPEN INPUT MOVI
                 OUTPUT MOVIX.
            DISPLAY SPACES LINE 1 POSITION  1 ERASE
            DISPLAY "BORRADO DE MOVIMIENTOS" LINE 10 POSITION 20.
            DISPLAY "DESDE CUENTA ...:" LINE 12 POSITION 10
            DISPLAY "HASTA CUENTA ...:" LINE 14 POSITION 10
            DISPLAY "HASTA FECHA  ...:" LINE 16 POSITION 10.
            ACCEPT DC LINE 12 POSITION 30 NO BEEP TAB CONVERT
            ACCEPT HC LINE 14 POSITION 30 NO BEEP TAB CONVERT
            ACCEPT HF LINE 16 POSITION 30 NO BEEP TAB CONVERT
            COMPUTE HASTA = (H-A * 372) + ( H-M * 31) + H-D.

       LEA.
            READ MOVI NEXT AT END PERFORM CIERRA THRU CIERRA-F
                                  CLOSE MOVI MOVIX
                                  STOP RUN.
            IF CUENTA-MOV < DC OR
               CUENTA-MOV > HC PERFORM GRABA THRU GRABA-F
                               GO TO LEA.
       GIRA.

            IF ESTA-CUENTA = 0 MOVE CUENTA-MOV TO ESTA-CUENTA
                               MOVE RUBRO-MOV  TO ESTE-RUBRO.
            IF ESTA-CUENTA NOT = CUENTA-MOV OR 
               ESTE-RUBRO  NOT = RUBRO-MOV 
                                 PERFORM CIERRA THRU CIERRA-F
                                 MOVE 0 TO ESTA-CUENTA ESTE-RUBRO
                                           IMPORTE
                                 GO TO GIRA.

            MOVE REG-MOV TO REG-MOVX
            COMPUTE ESTE = (ANO-MOV * 372) + ( MES-MOV * 31) +
                            DIA-MOV
            IF HASTA < ESTE   
                              PERFORM CIERRA THRU CIERRA-F
                              MOVE 0 TO IMPORTE
                              PERFORM GRABA THRU GRABA-F
                              GO TO LEA.

            DISPLAY TIPO-MOV
            ACCEPT QQ
            IF TIPO-MOV < 2 COMPUTE IMPORTE = IMPORTE + IMPORTE-MOV
              ELSE          COMPUTE IMPORTE = IMPORTE - IMPORTE-MOV.

            GO TO LEA.
       CIERRA.
            IF IMPORTE = 0 GO TO CIERRA-F.
            MOVE IMPORTE TO ELIMP
            MOVE H-A TO ANO-MOVX
            MOVE H-M TO MES-MOVX
            MOVE H-D TO DIA-MOVX
            IF IMPORTE > 0 MOVE 1 TO TIPO-MOVX
               ELSE        MOVE 2 TO TIPO-MOVX.
            MOVE ELIMP TO IMPORTE-MOVX.
            MOVE "Saldo Movim.Anteriores " TO COMENTARIO-MOVX.
            MOVE 0 TO COMPRO-MOVX
            MOVE "X" TO SUB-T-C-MOVX.
       REINT.
            COMPUTE MOVIMIENTO-MOVX = MOVIMIENTO-MOVX + 1
            WRITE REG-MOVX INVALID KEY GO TO REINT. 
       CIERRA-F.
            EXIT.

       GRABA.
            WRITE REG-MOVX FROM REG-MOV INVALID KEY DISPLAY "IMP". 
       GRABA-F.    
            EXIT.

