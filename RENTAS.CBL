       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENVASE.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. ENE'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "RES127.SEL".
       COPY "RENTAS.SEL".
       COPY "EMPRE.SEL".
       COPY "CUENTA.SEL".

       DATA DIVISION.
       FILE SECTION.
       COPY "RES127.REG".
       COPY "RENTAS.REG".
       COPY "EMPREBM.REG".
       COPY "CUENTABM.REG".

       WORKING-STORAGE SECTION.
       COPY "UTILES.WOR".
       01  VARIOS.
           03  CANT       PIC 9(4) VALUE 0.
           03  ELTOTAL    PIC S9(12)V99 VALUE 0.
           03  IMP        PIC S9(6)V99.
           03  EMP        PIC 9(4) VALUE 0.
           03  SUC        PIC 9(2) VALUE 0.
           03  RUB        PIC 9(6) VALUE 0.
           03  CLAVE      PIC 9 VALUE 0.

       01  VA.
           03 PER         PIC X(15).
           03 R-PER REDEFINES PER.  
              05  R1      PIC 9(4).
              05  FILLER  PIC X.
              05  R2      PIC 9(3).
              05  FILLER  PIC X.
              05  R3      PIC 9(4).
              05  FILLER  PIC X.
              05  R4      PIC 9.
              
           03 ACA.
              05  R1X      PIC 9(4).
              05  R2X      PIC 9(3).
              05  R3X      PIC 9(4).
              05  R4X      PIC 9.
           03 PERIODO      PIC 9999.
           03 ESTE-ING     PIC X(15).

       PROCEDURE DIVISION.
       ABRO.
            PERFORM ARMA-PANTALLA.
            OPEN INPUT RES127 EMPRESAS CUENTAS
                 OUTPUT RENTAS.
            PERFORM CARGA-PARAMETROS THRU CARGA-PARAMETROS-F
            PERFORM PROCESO THRU PROCESO-F
            PERFORM ARMA-0 THRU ARMA-0-F
            CLOSE RES127 
            OPEN INPUT RES127
            PERFORM PROCESO THRU PROCESO-F
            CLOSE RES127 RENTAS
            STOP RUN.
       CARGA-PARAMETROS.
       C1.     
            ACCEPT EMP LINE 8 POSITION 30 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO FF.
            MOVE EMP TO NUM-EMP
            MOVE 0 TO SUC-EMP
            READ EMPRESAS INVALID KEY GO TO C1.
            MOVE RAZ-SOC-EMP           TO REN-NOM-AGP.
            MOVE ING-BRUTOS-EMP        TO ESTE-ING.
       C2.     
            ACCEPT SUC LINE 08 POSITION 36 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO C1.
            MOVE EMP TO NUM-EMP 
            MOVE SUC TO SUC-EMP
            READ EMPRESAS INVALID KEY GO TO C1.
            DISPLAY RAZ-SOC-EMP LINE 08 POSITION 42 SIZE 30.
       C3.     
            ACCEPT RUB LINE 10 POSITION 30 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO C2.
       C4.
            ACCEPT DF LINE 12 POSITION 30 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO C3.
            COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
       C5.
            ACCEPT HF LINE 14 POSITION 30 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO C4.
            COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
       C6.
            ACCEPT PERIODO LINE 16 POSITION 30 NO BEEP TAB CONVERT 
                 ON EXCEPTION CONDI MOVE 1 TO SW1.
            IF ESCAPE-X GO TO FF.
            IF FLE1     GO TO C2.

            
       CARGA-PARAMETROS-F.
            EXIT.

       ARMA-0.

      *      MOVE RAZ-SOC-EMP           TO REN-NOM-AGP
            MOVE CALLE-EMP             TO REN-DIR-AGP
            MOVE 0                     TO REN-CASA-AGP
            MOVE COD-LOC-EMP           TO REN-CP-AGP
            MOVE ESTE-ING              TO PER
            MOVE R1                    TO R1X
            MOVE R2                    TO R2X
            MOVE R3                    TO R3X
            MOVE R4                    TO R4X
            MOVE ACA                   TO REN-INSC
            MOVE PERIODO               TO REN-PER-INF
            MOVE "N"                   TO REN-MARCA-REG.
            MOVE CANT                  TO REN-CANT-PERC
            MOVE ELTOTAL               TO REN-ACUM-PERC.
            WRITE REG-RENTAS.
       ARMA-0-F.
            EXIT.

       PROCESO.
            READ RES127 NEXT AT END MOVE 9 TO CLAVE
                                    GO TO PROCESO-F.

            IF IVA-RES NOT = 1 GO TO PROCESO.

            COMPUTE ESTA = (ANO-RES * 372) + (MES-RES * 31) + DIA-RES
            IF ESTA < DESDE OR
               ESTA > HASTA GO TO PROCESO.
      *******      IF RUB NOT = RUBRO-RES GO TO PROCESO.
            IF CLAVE = 9 
               PERFORM ARMA THRU ARMA-F
               PERFORM GRABA THRU GRABA-F.
            ADD 1 TO CANT

            COMPUTE ELTOTAL = ELTOTAL + PER-RES

      **      IF CANT > 50 MOVE 9 TO CLAVE
      **                   MOVE 0 TO CANT
      **                   GO TO PROCESO-F.
            GO TO PROCESO.
       ARMA.
            MOVE EMP-RES          TO EMP-CTA
            MOVE SUC-RES          TO SUC-CTA
            MOVE RUBRO-RES        TO RUBRO-CTA
            MOVE CUE-RES          TO CUENTA
            MOVE 0                TO SUB-CUENTA
            MOVE IVA-RES          TO R1-MARCA-SUJ
            MOVE I-BRU-RES        TO PER
            MOVE R1               TO R1X
            MOVE R2               TO R2X
            MOVE R3               TO R3X
            MOVE R4               TO R4X
            MOVE ACA              TO R1-IDENTIF-SUJ
            READ CUENTAS INVALID KEY MOVE SPACES TO DIRE 
                                     MOVE 0 TO COD-LOCA.
            MOVE DIRE             TO R1-DOM-SUJ
            MOVE 0                TO R1-CASA-SUJ
            MOVE COD-LOCA         TO R1-CP-SUJ
            MOVE DESC-RES         TO R1-NOM-SUJ
            MOVE ANO-RES          TO R1-ANO
            MOVE MES-RES          TO R1-MES
            MOVE DIA-RES          TO R1-DIA
            IF TIP-COM-RES = 4
                    MOVE 2        TO R1-CODI-SUJ
              ELSE
                    MOVE 1        TO R1-CODI-SUJ.

            MOVE IMP-RES          TO R1-MONTO-SUJ
            MOVE PER-RES          TO R1-PERCEP-SUJ 
            MOVE 2                TO R1-MARCA1-SUJ.

       ARMA-F.
            EXIT.
       GRABA.
            WRITE REG-RENTAS.
       GRABA-F.
            EXIT.
       PROCESO-F.
            EXIT.
       ARMA-PANTALLA.
            DISPLAY SPACES LINE 1 POSITION 1 ERASE
            DISPLAY "GENERAR DISKETTES PARA RENTAS" LINE 5 POSITION 25
            DISPLAY "Empresa ......:" LINE 08 POSITION 5
            display "Rubro ........:" LINE 10 POSITION 5
            DISPLAY "Desde Fecha...:" LINE 12 POSITION 5
            DISPLAY "Hasta Fecha...:" LINE 14 POSITION 5.
            DISPLAY "Periodo Infor.(AAXX):" LINE 16 POSITION 5.

       FF.
            CLOSE RENTAS RES127
            STOP RUN.



