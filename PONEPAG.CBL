       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PONEPAG.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "EMPRE.SEL".
              COPY "MOVI.SEL".
              COPY "IMP-132.SEL".
       
       DATA DIVISION.
       FILE SECTION.
              COPY "EMPREBM.REG".
              COPY "MOVI.REG".              
              COPY "IMP-132.REG".
       
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
       01  DATOS-PARAMETROS.
           03  EMP                     PIC 9999 VALUE 0.
           03  SUC                     PIC 99 VALUE 0.
           03  RUBRO                   PIC 9(6) VALUE 0.
           03  DCTA                    PIC 9(6) VALUE 0.
           03  HCTA                    PIC 9(6) VALUE 0.
           03  CONTADOR                PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "EMPRE.ERR".
           COPY "MOVI.ERR".
           COPY "IMP-132.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    MOVI   
                INPUT  EMPRESAS  
                OUTPUT IMP-132.

      
       COMIENZO.
           PERFORM ARMA-PANTALLA.
       C1.
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 4.
           ACCEPT EMP LINE 10 POSITION 40 NO BEEP TAB CONVERT
               ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
       C2.
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 45 SIZE 2.
           ACCEPT SUC LINE 10 POSITION 45 NO BEEP TAB CONVERT 
                ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1.
           MOVE EMP TO NUM-EMP
           MOVE SUC TO SUC-EMP.
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 24 POSITION 15 SIZE 64
                ACCEPT QQ LINE 24 POSITION 70
                GO TO C1.
           DISPLAY RAZ-SOC-EMP LINE 10 POSITION 50 SIZE 30.
       C3.
           PERFORM MENS.
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 6.
           ACCEPT RUBRO LINE 12 POSITION 40 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C2.
       C4.
           PERFORM MENS.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 6.
           ACCEPT DCTA LINE 14 POSITION 40 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C3.
       C5.
           PERFORM MENS.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 6.
           ACCEPT HCTA LINE 16 POSITION 40 NO  BEEP TAB CONVERT
             ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C4.
       C6.
           PERFORM MENS.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE 6.
           ACCEPT DF LINE 18 POSITION 40 NO BEEP TAB 
               ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C5.
       C7.
           PERFORM MENS.
           DISPLAY SPACES LINE 20 POSITION 40 SIZE 6.
           ACCEPT HF LINE 20 POSITION 40 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C6.


           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
           
       
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY " Aguarde , Estoy trabajando "
                LINE 12 POSITION 23 REVERSE BLINK.
       LEE-EN-MOVI.
           READ MOVI NEXT AT END GO TO FINAL-X.        
           
           IF EMP-MOV       NOT = EMP   GO TO LEE-EN-MOVI.
           IF SUC-EMP-MOV   NOT = SUC   GO TO LEE-EN-MOVI.
           IF RUBRO-MOV     NOT = RUBRO GO TO LEE-EN-MOVI.
           IF CUENTA-MOV  < DCTA OR CUENTA-MOV > HCTA GO TO LEE-EN-MOVI.
           COMPUTE ESTA = (ANO-MOV * 372) + (MES-MOV * 31) + DIA-MOV.
           IF ESTA < DESDE OR ESTA > HASTA GO TO LEE-EN-MOVI.
           ADD 1 TO CONTADOR.
       REGRABA-MOV.
           MOVE IMPORTE-MOV TO IMP-EXT-MOV.
           REWRITE REG-MOV INVALID KEY DISPLAY "No Regrabo Mov"
                ACCEPT QQ 
                STOP RUN.
           DISPLAY CONTADOR LINE 20 POSITION 35.       
           GO TO LEE-EN-MOVI.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "  Asiento pago de FACTURAS  "
              LINE 2 POSITION 20 REVERSE.
           DISPLAY "Empresa ...:" LINE 10 POSITION 10.
           DISPLAY "Rubro .....:" LINE 12 POSITION 10.
           DISPLAY "Desde Cuenta .:" LINE 14 POSITION 10.
           DISPLAY "Hasta Cuenta .:" LINE 16 POSITION 10.
           DISPLAY "Desde Fecha ..:" LINE 18 POSITION 10.       
           DISPLAY "Hasta Fecha ..:" LINE 20 POSITION 10.
           DISPLAY "Mensajes :" LINE 24 POSITION 2.       
       
       MENS.
           DISPLAY "Con Flecha ^ Retrocede, ESC Sale" 
                LINE 24 POSITION 15 SIZE 64.
       FINAL-X.
           CLOSE MOVI EMPRESAS.
       FFF.
           STOP RUN.
