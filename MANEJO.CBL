       IDENTIFICATION DIVISION.
       PROGRAM-ID.       MANEJO.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "MANEJO.SEL".
              COPY "EMPRE.SEL".
              COPY "CLAVES.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "MANEJO.REG".
              COPY "EMPREBM.REG".
              COPY "CLAVES.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "MANEJO.WOR".
                                   
       01  CLAVES-X.
           03  CLA1               PIC 9(10).
           03  CLA2               PIC X(10).
           03  CLA3               PIC X(10).
       01  A                      PIC X.
       77  EXISTE                 PIC X(2) VALUE "SI".       
       01  CAMPOS-EDITADOS.
           03  WMA-EMP-ED      PIC ZZZZ.
           03  WCLAVE1-ED         PIC ZZZZZZZZZZ.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "MANEJO.ERR".
           COPY "EMPRE.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    MANEJO
                INPUT  EMPRESAS CLAVES.
       
       CAP-CLAVE.           
           PERFORM ARMA-PANTALLA.
           PERFORM LIMPIE THRU LIMPIE-F.
           PERFORM MENS.
           DISPLAY SPACES LINE 08 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WMA-EMP LINE 08 POSITION 40 NO BEEP TAB
                  CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           MOVE WMA-EMP TO WMA-EMP-ED.
           DISPLAY WMA-EMP-ED LINE 08 POSITION 40.
       CAP-CLAVE1.
           PERFORM MENS1.
           DISPLAY SPACES LINE 08 POSITION 45 SIZE 2 REVERSE.
           ACCEPT WMA-SUC LINE 08 POSITION 45 NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI 
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE.
           DISPLAY WMA-SUC LINE 08 POSITION 45 SIZE 2.
           
           MOVE WMA-EMP TO NUM-EMP CLA-EMP MA-EMP
           MOVE WMA-SUC TO SUC-EMP CLA-SUC MA-SUC
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 25 POSITION 15 SIZE 64
                ACCEPT QQ
                GO TO CAP-CLAVE.
           
           DISPLAY RAZ-SOC-EMP LINE 08 POSITION 49 SIZE 30.
           
           READ CLAVES INVALID KEY  DISPLAY
                "No Kay Claves Para esta Empresa"
                LINE 23 POSITION 15 SIZE 64 BLINK
                ACCEPT QQ
                GO TO CAP-CLAVE.
           
           DISPLAY "Clave 1 ..:" LINE 14 POSITION 20.
           DISPLAY "Clave 2 ..:" LINE 16 POSITION 20.
           DISPLAY "Clave 3 ..:" LINE 18 POSITION 20.
           PERFORM CLAVESS THRU CLAVES-F.
           
           IF CLAVE1 NOT = CLA1 OR
              CLAVE2 NOT = CLA2 OR
              CLAVE3 NOT = CLA3
              DISPLAY "            Clave no Existe" 
              LINE 23 POSITION 15 SIZE 64 BLINK
              DISPLAY SPACES LINE 14 POSITION 20 SIZE 28
              DISPLAY SPACES LINE 16 POSITION 20 SIZE 28
              DISPLAY SPACES LINE 18 POSITION 20 SIZE 28
              ACCEPT QQ
              GO TO CAP-CLAVE.

           DISPLAY SPACES LINE 14 POSITION 20 SIZE 28.
           DISPLAY SPACES LINE 16 POSITION 20 SIZE 28.
           DISPLAY SPACES LINE 18 POSITION 20 SIZE 28.
           
           MOVE SPACES TO A.          
           MOVE "SI" TO EXISTE
           READ MANEJO INVALID KEY MOVE "NO" TO EXISTE 
                                    MOVE SPACES TO WMA-CCT
                                                   WMA-PEND
                                                   WMA-IVA
                                                   WMA-CAJA
                                                   WMA-REM
                                                   WMA-PAGARE
                                                   WMA-ESTADIS
                                                   WMA-IMPUE
                                    GO TO C1.

           PERFORM MANEJO-MOVER.
           IF EXISTE = "SI" PERFORM MOSTRAR THRU MOSTRAR-F.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.                     
           DISPLAY "-M- Modifica -B- Baja" LINE 25 POSITION 15.
           ACCEPT DED LINE 25 POSITION 70 NO BEEP TAB ON 
                 EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FFFF.
                IF DED = "M" OR
                   DED = "m" GO TO SE-MODIF
                   ELSE   
                       IF DED = "b" OR
                          DED = "B" GO TO BAJA.
           GO TO CAP-CLAVE.


       C1.           
           DISPLAY 
              "¨ Que Opciones Maneja La Empresa ? (Constestar (S/N) )"
                 LINE 10 POSITION 2.
           PERFORM LIMPIE THRU LIMPIE-F.
           DISPLAY "Ctas.Ctes .......:" LINE 12 POSITION 20.           
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-CCT LINE 12 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE.
           DISPLAY WMA-CCT LINE 12 POSITION 40 SIZE  1.
           IF WMA-CCT   NOT = "S" AND WMA-CCT  NOT = "N"
              GO TO C1.
       C2.    
           DISPLAY "Pendientes ......:" LINE 13 POSITION 20.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-PEND LINE 13 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C1. 
           DISPLAY WMA-PEND LINE 13 POSITION 40 SIZE 1.
           IF WMA-PEND   NOT = "S" AND WMA-PEND  NOT = "N"
              GO TO C2.
           
       C3.    
           DISPLAY "I.V.A ...........:" LINE 14 POSITION 20.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-IVA LINE 14 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C2.
           DISPLAY WMA-IVA LINE 14 POSITION 40 SIZE 1.
           IF WMA-IVA   NOT = "S" AND WMA-IVA   NOT = "N"
              GO TO C3.
           
       C4.    
           DISPLAY "Caja ............:" LINE 15 POSITION 20.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-CAJA LINE 15 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C3.
           DISPLAY WMA-CAJA LINE 15 POSITION 40 SIZE 1.
           IF WMA-CAJA   NOT = "S" AND WMA-CAJA  NOT = "N"
              GO TO C4.
           
       C5.    
           DISPLAY "Remitos .........:" LINE 16 POSITION 20.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-REM LINE 16 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C4.
           DISPLAY WMA-REM LINE 16 POSITION 40 SIZE 1.
           IF WMA-REM   NOT = "S" AND WMA-REM  NOT = "N"
              GO TO C5.
           
       C6.    
           DISPLAY "Pagares..........:" LINE 17 POSITION 20. 
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-PAGARE LINE 17 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C5.
           DISPLAY WMA-PAGARE LINE 17 POSITION 40 SIZE 1.
           IF WMA-PAGARE   NOT = "S" AND WMA-PAGARE   NOT = "N"
              GO TO C6.
           
       C7.    
           DISPLAY "Ventas x Turnos..:" LINE 18 POSITION 20.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-ESTADIS LINE 18 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO  C6.
           DISPLAY WMA-ESTADIS LINE 18 POSITION 40 SIZE 1.
           IF WMA-ESTADIS   NOT = "S" AND WMA-ESTADIS  NOT = "N"
              GO TO C7.
           
       C8.    
           DISPLAY "Impuestos .......:" LINE 19 POSITION 20.
           DISPLAY SPACES LINE 19 POSITION 40 SIZE 1 REVERSE.           
           ACCEPT WMA-IMPUE LINE 19 POSITION 40 ECHO UPDATE
                  NO BEEP TAB REVERSE ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C7.
           DISPLAY WMA-IMPUE LINE 19 POSITION 40 SIZE 1.
           IF WMA-IMPUE   NOT = "S" AND WMA-IMPUE  NOT = "N"
              GO TO C8.
           
       C9.           
           DISPLAY "Stock Pendiente .:" LINE 20 POSITION 20.
           DISPLAY SPACES LINE 20 POSITION 40 SIZE 1 REVERSE.
           ACCEPT WMA-STOCK LINE 20 POSITION 40 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C8.
           DISPLAY WMA-STOCK LINE 20 POSITION 40 SIZE 1.
           IF WMA-STOCK   NOT = "S" AND WMA-STOCK  NOT = "N"
              GO TO C9.

       C10.           
           DISPLAY "Impr.Fact. -A- ..:" LINE 21 POSITION 20.
           DISPLAY SPACES LINE 21 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WMA-FACTURA-A LINE 21 POSITION 40 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C9.

       C11.           
           DISPLAY "Impr.Fact. -B- ..:" LINE 22 POSITION 20.
           DISPLAY SPACES LINE 22 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WMA-FACTURA-B LINE 22 POSITION 40 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C10.
       C12.           
           DISPLAY "Impr.Pagare .....:" LINE 23 POSITION 20.
           DISPLAY SPACES LINE 23 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WMA-IMP-PAGARE LINE 23 POSITION 40 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C11.

           
       C13.           
           DISPLAY "Rubro Ctas.Ctes..:" LINE 24 POSITION 20.
           DISPLAY SPACES LINE 24 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WMA-RUBRO-CC LINE 24 POSITION 40 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C12.

           PERFORM MOVER-MANEJO.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-MANEJO INVALID KEY DISPLAY "NO GRABO".                
                 GO TO CAP-CLAVE.

       REGRABE.
           REWRITE REG-MANEJO INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 25 POSITION 20
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE.
   
           COPY "MANEJO.RUT".
       MOSTRAR.
           DISPLAY 
              "Esta Empresa Maneja Las Siguientes opciones..."
                 LINE 10 POSITION 2 SIZE 68.
           
           DISPLAY "Ctas.Ctes .......:" LINE 12 POSITION 20.           
           DISPLAY "Pendientes ......:" LINE 13 POSITION 20.
           DISPLAY "I.V.A ...........:" LINE 14 POSITION 20.
           DISPLAY "Caja ............:" LINE 15 POSITION 20.
           DISPLAY "Remitos .........:" LINE 16 POSITION 20.
           DISPLAY "Pagares..........:" LINE 17 POSITION 20. 
           DISPLAY "Ventas x Turnos..:" LINE 18 POSITION 20.
           DISPLAY "Impuestos .......:" LINE 19 POSITION 20.
           DISPLAY "Stock Pendiente .:" LINE 20 POSITION 20.          
           DISPLAY "Impr.Fact. -A- ..:" LINE 21 POSITION 20.
           DISPLAY "Impr.Fact. -B-...:" LINE 22 POSITION 20.
           DISPLAY "Impr. Pagare ....:" LINE 23 POSITION 20.          
           DISPLAY "Rubro Ctas.Ctes. :" LINE 24 POSITION 20.
           

           DISPLAY WMA-CCT LINE 12 POSITION 40 SIZE  1.
           DISPLAY WMA-PEND LINE 13 POSITION 40 SIZE 1.
           DISPLAY WMA-IVA LINE 14 POSITION 40 SIZE 1.
           DISPLAY WMA-CAJA LINE 15 POSITION 40 SIZE 1.
           DISPLAY WMA-REM LINE 16 POSITION 40 SIZE 1.
           DISPLAY WMA-PAGARE LINE 17 POSITION 40 SIZE 1.
           DISPLAY WMA-ESTADIS LINE 18 POSITION 40 SIZE 1.
           DISPLAY WMA-IMPUE LINE 19 POSITION 40 SIZE 1.
           DISPLAY WMA-STOCK LINE 20 POSITION 40 SIZE 1.       
           DISPLAY WMA-FACTURA-A LINE 21 POSITION 40 SIZE 4.       
           DISPLAY WMA-FACTURA-B LINE 22 POSITION 40 SIZE 4.       
           DISPLAY WMA-IMP-PAGARE  LINE 23 POSITION 40 SIZE 4.       
           DISPLAY WMA-RUBRO-CC    LINE 24 POSITION 40 SIZE 4.

       MOSTRAR-F.
           EXIT.
       
       FINAL1.
           CLOSE MANEJO EMPRESAS CLAVES.
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Verificaion de Empresas" LINE 2 POSITION 2.           
           DISPLAY "----------- -- --------" LINE 3 POSITION 3.
           DISPLAY " ----------- -- --------" LINE 4 POSITION 3.
           DISPLAY "Empresa ....:" LINE 08 POSITION 20.
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
       BAJA.
           DISPLAY "¨ Esta seguro de bajarlo (S/N) ? "
                   LINE 25 POSITION 15 SIZE 64.
           MOVE "N" TO DED.
           ACCEPT DED LINE 25 POSITION 70 ECHO UPDATE
                  NO BEEP TAB ON EXCEPTION CONDI 
                  MOVE 1 TO SW1.
           IF ESCAPE-X GO TO CAP-CLAVE.
           IF DED = "S" OR DED = "s"
              GO TO SI-BAJA.
           GO TO CAP-CLAVE.
       SI-BAJA.
           DELETE MANEJO INVALID KEY DISPLAY "NO BAJO".
           GO TO CAP-CLAVE.
       SE-MODIF.
           MOVE "M" TO A
           GO TO C1.


       MENS.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.           
           DISPLAY "Con Escape o Flecha ^ Retrocede va al Menu" 
              LINE 25 POSITION 15.
       MENS1.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.           
           DISPLAY "Con Las Flechas Retrocede o Avanza"
                   LINE 25 POSITION 15.

       
       CLAVESS. 
           ACCEPT CLA1 LINE 14 POSITION 40 REVERSE OFF.
           ACCEPT CLA2 LINE 16 POSITION 40 REVERSE OFF.
           ACCEPT CLA3 LINE 18 POSITION 40 REVERSE OFF.
       CLAVES-F.
           EXIT.

       LIMPIE.
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 19 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 20 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 21 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 22 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 23 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 24 POSITION 40 SIZE 32.

       LIMPIE-F.
           EXIT.
