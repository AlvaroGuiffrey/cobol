       IDENTIFICATION DIVISION.
       PROGRAM-ID.       BCO.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "BCO.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "BCO.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "BCO.WOR".
       01  A                      PIC X.
       77  EXISTE                 PIC X(2) VALUE "SI".       
       01  WBCO-NUMERO-ED         PIC ZZZZ.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "BCO.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    BCO.
           PERFORM ARMA-PANTALLA.
              
       CAP-CLAVE.           
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 30 SIZE 4 REVERSE.
           ACCEPT WBCO-NUMERO LINE 10 POSITION 30 NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           MOVE WBCO-NUMERO TO WBCO-NUMERO-ED.
           DISPLAY WBCO-NUMERO-ED LINE 10 POSITION 30 SIZE 4.
           MOVE WBCO-NUMERO TO BCO-NUMERO.           
           MOVE SPACES TO A.          
           MOVE "SI" TO EXISTE
           READ BCO INVALID KEY MOVE "NO" TO EXISTE 
                                    MOVE SPACES TO WBCO-NOMBRE
                                                   GO TO C1.

           PERFORM BCO-MOVER.
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
           PERFORM MENS1.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 30 REVERSE.
           ACCEPT WBCO-NOMBRE LINE 15 POSITION 40 ECHO UPDATE 
             NO BEEP TAB REVERSE
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE.
           DISPLAY WBCO-NOMBRE LINE 15 POSITION 40 SIZE 30.    
           
           PERFORM MOVER-BCO.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-BCO INVALID KEY DISPLAY "NO GRABO".                
                 GO TO CAP-CLAVE.

       REGRABE.
           REWRITE REG-BCO INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 24 POSITION 20
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE.
   
           COPY "BCO.RUT".
       MOSTRAR.
           DISPLAY WBCO-NOMBRE LINE 15 POSITION 40 SIZE 30.       
       MOSTRAR-F.
           EXIT.
       
       FINAL1.
           CLOSE BCO.
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I FROM 1 BY 1 UNTIL I > 25

           DISPLAY RAYA LINE 1 POSITION 1.
           DISPLAY RAYA LINE 22 POSITION 1.
           DISPLAY RAYA LINE 25 POSITION 1.

           DISPLAY "É" LINE 1 POSITION 1.
           DISPLAY "Ì" LINE 22 POSITION 1.
           DISPLAY "È" LINE 25 POSITION 1.


           DISPLAY "»" LINE 1 POSITION 80.
           DISPLAY "¹" LINE 22 POSITION 80.
           DISPLAY "¼" LINE 25 POSITION 80.

           DISPLAY "Sistema de Cheques" LINE 2 POSITION 5.
           DISPLAY "------- -- -------" LINE 3 POSITION 5.
           DISPLAY "Mantenimiento de Bancos" LINE 4 POSITION 5.
           DISPLAY "Codigo de Banco  .:" LINE 10 POSITION 4.
           DISPLAY "Nombre del Banco .:" LINE 15 POSITION 20.
       R1.
           DISPLAY "º" LINE I POSITION 1.
           DISPLAY "º" LINE I POSITION 80.

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
           DELETE BCO INVALID KEY DISPLAY "NO BAJO".
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
   
