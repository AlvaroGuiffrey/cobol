       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CLAVES.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "CLAVES.SEL".
              COPY "EMPRE.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "CLAVES.REG".
              COPY "EMPREBM.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "CLA.WOR".
                                   
       01  A                      PIC X.
       77  EXISTE                 PIC X(2) VALUE "SI".       
       01  CAMPOS-EDITADOS.
           03  WCLAVE-EMP-ED      PIC ZZZZ.
           03  WCLAVE1-ED         PIC ZZZZZZZZZZ.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "CLAVES.ERR".
           COPY "EMPRE.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    CLAVES
                INPUT  EMPRESAS.
           PERFORM ARMA-PANTALLA.
              
       CAP-CLAVE.           
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WCLA-EMP LINE 10 POSITION 40 NO BEEP TAB
                  CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           MOVE WCLA-EMP TO WCLAVE-EMP-ED.
           DISPLAY WCLAVE-EMP-ED LINE 10 POSITION 40.
       CAP-CLAVE1.
           PERFORM MENS1.
           DISPLAY SPACES LINE 10 POSITION 45 SIZE 4 REVERSE.
           ACCEPT WCLA-SUC LINE 10 POSITION 45 NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI 
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE.
           DISPLAY WCLA-SUC LINE 10 POSITION 45 SIZE 4.
           
           MOVE WCLA-EMP TO NUM-EMP CLA-EMP
           MOVE WCLA-SUC TO SUC-EMP CLA-SUC
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 24 POSITION 15 SIZE 64
                ACCEPT QQ
                GO TO CAP-CLAVE.
           
           DISPLAY RAZ-SOC-EMP LINE 10 POSITION 51 SIZE 28.
           
           MOVE SPACES TO A.          
           MOVE "SI" TO EXISTE
           READ CLAVES INVALID KEY MOVE "NO" TO EXISTE 
                                    MOVE 0      TO WCLAVE1
                                    MOVE SPACES TO WCLAVE2 WCLAVE3
                                    GO TO C1.

           PERFORM CLAVES-MOVER.
           IF EXISTE = "SI" PERFORM MOSTRAR THRU MOSTRAR-F.
           DISPLAY SPACES LINE 23 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.                     
           DISPLAY "-M- Modifica -B- Baja" LINE 23 POSITION 15.
           ACCEPT DED LINE 23 POSITION 70 NO BEEP TAB ON 
                 EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FFFF.
                IF DED = "M" OR
                   DED = "m" GO TO SE-MODIF
                   ELSE   
                       IF DED = "b" OR
                          DED = "B" GO TO BAJA.
           GO TO CAP-CLAVE.


       C1.           
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WCLAVE1 LINE 14 POSITION 40 ECHO UPDATE 
               NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE1.
           MOVE WCLAVE1 TO WCLAVE1-ED.
           DISPLAY WCLAVE1 LINE 14 POSITION 40 SIZE 10.
                  
       C2.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WCLAVE2 LINE 16 POSITION 40 ECHO UPDATE 
               NO BEEP TAB 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C1.
           DISPLAY WCLAVE2 LINE 16 POSITION 40 SIZE 10.
           
       C3.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WCLAVE3 LINE 18 POSITION 40 ECHO UPDATE 
               NO BEEP TAB 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C2.
           DISPLAY WCLAVE3 LINE 18 POSITION 40 SIZE 10.

           
           PERFORM MOVER-CLAVES.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-CLAVES INVALID KEY DISPLAY "NO GRABO".                
                 GO TO CAP-CLAVE.

       REGRABE.
           REWRITE REG-CLAVES INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 24 POSITION 20
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE.
   
           COPY "CLAVES.RUT".
       MOSTRAR.
           MOVE WCLAVE1 TO WCLAVE1-ED.
           DISPLAY WCLAVE1 LINE 14 POSITION 40 SIZE 10.
           DISPLAY WCLAVE2 LINE 16 POSITION 40 SIZE 10.
           DISPLAY WCLAVE3 LINE 18 POSITION 40 SIZE 10.
       MOSTRAR-F.
           EXIT.
       
       FINAL1.
           CLOSE CLAVES EMPRESAS.
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Mantenimiento de Claves" LINE 2 POSITION 2.           
           DISPLAY "------------- -- ------" LINE 3 POSITION 3.
           DISPLAY "Empresa .....:" LINE 10 POSITION 20.
           DISPLAY "Detalle Clave1 ..:" LINE 14 POSITION 20.
           DISPLAY "Detalle Clave2 ..:" LINE 16 POSITION 20.
           DISPLAY "Detalle Clave3 ..:" LINE 18 POSITION 20.
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
       BAJA.
           DISPLAY "¨ Esta seguro de bajarlo (S/N) ? "
                   LINE 23 POSITION 15 SIZE 64.
           MOVE "N" TO DED.
           ACCEPT DED LINE 23 POSITION 70 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI 
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO CAP-CLAVE.
           IF DED = "S" OR DED = "s"
              GO TO SI-BAJA.
           GO TO CAP-CLAVE.
       SI-BAJA.
           DELETE CLAVES INVALID KEY DISPLAY "NO BAJO".
           GO TO CAP-CLAVE.
       SE-MODIF.
           MOVE "M" TO A
           GO TO C1.


       MENS.
           DISPLAY SPACES LINE 23 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.           
           DISPLAY "Con Escape o Flecha ^ Retrocede va al Menu" 
              LINE 23 POSITION 15.
       MENS1.
           DISPLAY SPACES LINE 23 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.           
           DISPLAY "Con Las Flechas Retrocede o Avanza"
                   LINE 23 POSITION 15.
