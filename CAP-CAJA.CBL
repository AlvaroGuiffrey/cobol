       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LACAJA.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

              COPY "MANEJO.SEL".
              COPY "LACAJA.SEL".
              COPY "EMPRE.SEL".
              COPY "CUENTA.SEL".
              COPY "TARJ.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "MANEJO.REG".
              COPY "LACAJA.REG".
              COPY "CUENTABM.REG".
              COPY "EMPREBM.REG".
              COPY "TARJ.REG".
       WORKING-STORAGE SECTION.
                  COPY "LACAJA.WOR".
                  COPY "UTILES.WOR".
       01  WREG-LACAJA-ED.
           03  WCLAVE-LACAJA-ED.
               05  WLAC-EMP-ED          PIC ZZZZ.
               05  WLAC-SUC-ED          PIC ZZ.
               05  WLAC-FECHA-ED.
                   07  WLAC-DIA-ED      PIC 99/.
                   07  WLAC-MES-ED      PIC 99/.
                   07  WLAC-ANO-ED      PIC 99.
               05  WLAC-CONDI-ED        PIC 99.
               05  WLAC-COMPROB-ED      PIC ZZZZZZ.
           03  WLAC-RUBRO-ED            PIC ZZZZZZZ.
           03  WLAC-CUENTA-ED           PIC ZZZZZZ.
           03  WLAC-SUB-CTA-ED          PIC ZZZZ.
           03  WLAC-IMPORTE-ED          PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           03  WLAC-IMP-CHEQ-ED         PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           03  WLAC-TARJ-UTI-ED         PIC ZZ.  
       01  A         PIC X.
       77  EXISTE    PIC X(2) VALUE "SI".

       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "LACAJA.ERR".
           COPY "EMPRE.ERR".
           COPY "CUENTA.ERR".
           COPY "TARJ.ERR".
           COPY "MANEJO.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O  LACAJA
                INPUT CUENTAS EMPRESAS TARJ MANEJO.
           PERFORM ARMA-PANTALLA.
              
       EMPRE.
           PERFORM MENS.
           DISPLAY SPACES LINE 7 POSITION 30 SIZE 4.
           ACCEPT WLAC-EMP LINE 7 POSITION 30 ECHO UPDATE
               NO BEEP TAB REVERSE ON EXCEPTION CONDI
               MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           DISPLAY SPACES LINE 7 POSITION 30 SIZE 4.
           MOVE WLAC-EMP TO WLAC-EMP-ED.
           DISPLAY WLAC-EMP-ED LINE 7 POSITION 30.
       SUC.
           PERFORM MENS1.
           DISPLAY SPACES LINE 8 POSITION 30 SIZE 2 REVERSE.
           ACCEPT WLAC-SUC LINE 8 POSITION 30 ECHO UPDATE
              NO BEEP TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO EMPRE.
           DISPLAY SPACES LINE 8 POSITION 30 SIZE 2.
           MOVE WLAC-SUC TO WLAC-SUC-ED.
           DISPLAY WLAC-SUC-ED LINE 8 POSITION 30.
           MOVE WLAC-EMP TO NUM-EMP MA-EMP
           MOVE WLAC-SUC TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO SUC.
           DISPLAY RAZ-SOC-EMP LINE 7 POSITION 40.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                LINE 24 POSITION 15 SIZE 64
                GO TO EMPRE.
       TURNO.
           PERFORM MENS1.
           DISPLAY SPACES LINE 9 POSITION 30 SIZE 1.
           ACCEPT WLAC-TURNO LINE 9 POSITION 30 ECHO UPDATE
               NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO SUC.
           DISPLAY SPACES LINE 9 POSITION 30 SIZE 1.
           DISPLAY WLAC-TURNO LINE 9 POSITION 30.
       FECHA.
           PERFORM MENS1.
           DISPLAY SPACES LINE 10 POSITION 30 SIZE 6 REVERSE.
           ACCEPT WLAC-FECHA LINE 10 POSITION 30 ECHO UPDATE
             NO BEEP TAB REVERSE ON EXCEPTION  
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO TURNO.
           DISPLAY SPACES LINE 10 POSITION 30 SIZE 8.
           MOVE WLAC-ANO TO WLAC-ANO-ED.
           MOVE WLAC-MES TO WLAC-MES-ED.
           MOVE WLAC-DIA TO WLAC-DIA-ED.
           DISPLAY WLAC-FECHA-ED LINE 10 POSITION 30.
       CONDICION.
           PERFORM MENS1.
           PERFORM CONDI-MENS.
           DISPLAY SPACES LINE 11 POSITION 30 SIZE 2 REVERSE.
           ACCEPT WLAC-CONDI LINE 11 POSITION 30 ECHO UPDATE
             NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FECHA.
           DISPLAY SPACES LINE 11 POSITION 30 SIZE 2.
           DISPLAY WLAC-CONDI LINE 11 POSITION 30.
           IF WLAC-CONDI > 9
              GO TO CONDICION.
           IF WLAC-CONDI = 2 DISPLAY "Saldo Ant" LINE 11
                POSITION 40 SIZE 15.
           IF WLAC-CONDI = 1 DISPLAY "Contado" LINE 11 
                POSITION 40 SIZE 15.
           IF WLAC-CONDI = 5 DISPLAY "Pagos  " LINE 11
                POSITION 40 SIZE 15.
           IF WLAC-CONDI = 6 DISPLAY "Creditos" LINE 11
                POSITION 40 SIZE 15.
           IF WLAC-CONDI = 9 DISPLAY "Cobros " LINE 11 
                POSITION 40 SIZE 15.                                

       COMPROB.
           PERFORM MENS1.           
           DISPLAY SPACES LINE 12 POSITION 30 SIZE 6 REVERSE.
           ACCEPT WLAC-COMPROB LINE 12 POSITION 30 ECHO UPDATE
             NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CONDICION.
           DISPLAY SPACES LINE 12 POSITION 30 SIZE 6.
           MOVE WLAC-COMPROB TO WLAC-COMPROB-ED.
           DISPLAY WLAC-COMPROB-ED LINE 12 POSITION 30.

           MOVE WLAC-EMP        TO LAC-EMP
           MOVE WLAC-SUC        TO LAC-SUC
           MOVE WLAC-TURNO      TO LAC-TURNO
           MOVE WLAC-ANO        TO LAC-ANO
           MOVE WLAC-MES        TO LAC-MES
           MOVE WLAC-DIA        TO LAC-DIA
           MOVE WLAC-CONDI      TO LAC-CONDI
           MOVE WLAC-COMPROB    TO LAC-COMPROB                      
           MOVE "SI" TO EXISTE
           READ LACAJA INVALID KEY MOVE "NO" TO EXISTE
                                   MOVE 0 TO WLAC-RUBRO WLAC-CUENTA
                                             WLAC-SUB-CTA WLAC-TIPO
                                             WLAC-IMPORTE WLAC-IMP-CHEQ 
                                             WLAC-TARJ-UTI
                                             GO TO C1.

           PERFORM LACAJA-MOVER.
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
           GO TO TURNO.

       C1.
           PERFORM MENS1.
           DISPLAY SPACES LINE 14 POSITION 30 SIZE 7 REVERSE.
           ACCEPT WLAC-RUBRO LINE 14 POSITION 30 ECHO UPDATE
               NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1
                  TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO COMPROB.
           DISPLAY SPACES LINE 14 POSITION 30 SIZE 7.
           MOVE WLAC-RUBRO TO WLAC-RUBRO-ED.
           DISPLAY WLAC-RUBRO-ED LINE 14 POSITION 30.
       C2.           
           PERFORM MENS1.           
           DISPLAY SPACES LINE 15 POSITION 30 SIZE 6 REVERSE.
           IF WLAC-RUBRO = 100001 GO TO C4.
           ACCEPT WLAC-CUENTA LINE 15 POSITION 30 ECHO UPDATE
               NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1
                  TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C1.
           DISPLAY SPACES LINE 15 POSITION 30 SIZE 6.
           MOVE WLAC-CUENTA TO WLAC-CUENTA-ED.
           DISPLAY WLAC-CUENTA-ED LINE 15 POSITION 30.
       C3.
           PERFORM MENS1.           
           DISPLAY SPACES LINE 16 POSITION 30 SIZE 4 REVERSE.
           ACCEPT WLAC-SUB-CTA LINE 16 POSITION 30 ECHO UPDATE
               NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1
                  TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C2.
           DISPLAY SPACES LINE 16 POSITION 30 SIZE 4.
           MOVE WLAC-SUB-CTA TO WLAC-SUB-CTA-ED.
           DISPLAY WLAC-SUB-CTA-ED LINE 16 POSITION 30.           
           MOVE WLAC-EMP     TO EMP-CTA
           MOVE WLAC-SUC     TO SUC-CTA
           MOVE WLAC-RUBRO   TO RUBRO-CTA
           MOVE WLAC-CUENTA  TO CUENTA
           MOVE WLAC-SUB-CTA TO SUB-CUENTA
           READ CUENTAS INVALID KEY DISPLAY "No exsite Cuenta" LINE 16
                POSITION 40 SIZE 39 
           GO TO C3.
           DISPLAY NOMBRE-C LINE 16 POSITION 40 SIZE 25.
           
       C4.
           PERFORM MENS1.           
           PERFORM C4-MENS.
           DISPLAY SPACES LINE 17 POSITION 30 SIZE 1 REVERSE.
           ACCEPT WLAC-TIPO LINE 17 POSITION 30 ECHO UPDATE
              NO BEEP TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C3.
           DISPLAY SPACES LINE 17 POSITION 30 SIZE 1.
           DISPLAY WLAC-TIPO LINE 17 POSITION 30.
           IF WLAC-TIPO NOT = "E" AND WLAC-TIPO NOT = "C"
              AND WLAC-TIPO NOT = "T" AND WLAC-TIPO NOT = "O"
                  GO TO C4.
           IF WLAC-TIPO = "E" DISPLAY "Efectivo" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "C" DISPLAY "Cheque" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "T" DISPLAY "Tarjeta" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "O" DISPLAY "Otros" LINE 17 POSITION 40 
              SIZE 15.                                          
       C5.
           PERFORM MENS1.           
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 12 REVERSE.
           ACCEPT WLAC-IMPORTE LINE 18 POSITION 30 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C4.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 12.
           MOVE WLAC-IMPORTE  TO WLAC-IMPORTE-ED.
           DISPLAY WLAC-IMPORTE-ED LINE 18 POSITION 30.           
           IF WLAC-TIPO = "E"
              MOVE 0 TO WLAC-IMP-CHEQ WLAC-TARJ-UTI
              GO TO MUEVE.
           IF WLAC-TIPO = "T" MOVE 0 TO WLAC-IMP-CHEQ
                              GO TO C7.
       C6.
           PERFORM MENS1.           
           DISPLAY SPACES LINE 19 POSITION 30 SIZE 12 REVERSE.
           ACCEPT WLAC-IMP-CHEQ LINE 19 POSITION 30 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C5.
           DISPLAY SPACES LINE 19 POSITION 30 SIZE 12.
           MOVE WLAC-IMP-CHEQ  TO WLAC-IMP-CHEQ-ED.
           DISPLAY WLAC-IMP-CHEQ-ED LINE 19 POSITION 30.
           MOVE 0 TO WLAC-TARJ-UTI.
           IF WLAC-TIPO = "C" GO TO MUEVE.

       C7.
           PERFORM MENS1.           
           DISPLAY SPACES LINE 20 POSITION 30 SIZE 2.
           ACCEPT WLAC-TARJ-UTI LINE 20 POSITION 30 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C6.
           DISPLAY WLAC-TARJ-UTI LINE 20 POSITION 30 SIZE 2.
           MOVE WLAC-TARJ-UTI TO WLAC-TARJ-UTI-ED TAR-NUM.
           READ TARJ INVALID KEY GO TO C7.

           DISPLAY TAR-NOM LINE 20 POSITION 34.
       MUEVE.
           ACCEPT LAC-SUMA-RESTA LINE 21 POSITION 30 ECHO UPDATE
              NO BEEP TAB CONVERT ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C7.
           IF LAC-SUMA-RESTA > 1 GO TO MUEVE.
           IF LAC-SUMA-RESTA = 0 DISPLAY "SUMA " LINE 21 POSITION 35
            ELSE                 DISPLAY "RESTA" LINE 21 POSITION 35.

           IF LAC-SUMA-RESTA = 1 MOVE 2 TO LAC-SUMA-RESTA.


           PERFORM MOVER-LACAJA.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-LACAJA INVALID KEY DISPLAY "NO GRABO".                
                 GO TO TURNO.


       REGRABE.
           REWRITE REG-LACAJA INVALID KEY DISPLAY " ERROR REGRABA " 
                          LINE 24 POSITION 20
                          ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO TURNO.
           COPY "LACAJA.RUT".
       MOSTRAR.
           DISPLAY SPACES LINE 14 POSITION 30 SIZE 7.
           MOVE WLAC-RUBRO      TO WLAC-RUBRO-ED.
           DISPLAY WLAC-RUBRO-ED LINE 14 POSITION 30.
           DISPLAY SPACES LINE 15 POSITION 30 SIZE 6.
           MOVE WLAC-CUENTA     TO WLAC-CUENTA-ED.
           DISPLAY WLAC-CUENTA-ED LINE 15 POSITION 30.
           DISPLAY SPACES LINE 16 POSITION 30 SIZE 4.
           MOVE WLAC-SUB-CTA    TO WLAC-SUB-CTA-ED.
           DISPLAY WLAC-SUB-CTA-ED LINE 16 POSITION 30.           
           MOVE WLAC-EMP        TO EMP-CTA
           MOVE WLAC-SUC        TO SUC-CTA
           MOVE WLAC-RUBRO      TO RUBRO-CTA
           MOVE WLAC-CUENTA     TO CUENTA
           MOVE WLAC-SUB-CTA    TO SUB-CUENTA
           READ CUENTAS INVALID MOVE "No exsite Cuenta" TO NOMBRE-C.
           DISPLAY NOMBRE-C LINE 16 POSITION 40.
           DISPLAY SPACES LINE 17 POSITION 30 SIZE 1.
           DISPLAY WLAC-TIPO LINE 17 POSITION 30.
           IF WLAC-TIPO = "E" DISPLAY "Efectivo" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "C" DISPLAY "Cheque" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "T" DISPLAY "Tarjeta" LINE 17 POSITION 40 
              SIZE 15.
           IF WLAC-TIPO = "O" DISPLAY "Otros" LINE 17 POSITION 40 
              SIZE 15.                                         
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 12.
           MOVE WLAC-IMPORTE    TO WLAC-IMPORTE-ED.
           DISPLAY WLAC-IMPORTE-ED LINE 18 POSITION 30.           
           DISPLAY SPACES LINE 19 POSITION 30 SIZE 12.
           MOVE WLAC-IMP-CHEQ   TO WLAC-IMP-CHEQ-ED.
           DISPLAY WLAC-IMP-CHEQ-ED LINE 19 POSITION 30.
           DISPLAY WLAC-TARJ-UTI LINE 20 POSITION 30 SIZE 2.
           MOVE WLAC-TARJ-UTI   TO WLAC-TARJ-UTI-ED TAR-NUM
           READ TARJ INVALID KEY MOVE "****" TO TAR-NOM.
           DISPLAY TAR-NOM LINE 20 POSITION 34.
       MOSTRAR-F.
           EXIT.
       FINAL1.
           CLOSE LACAJA CUENTAS EMPRESAS.
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

           DISPLAY "  SISTEMA DE CAJA" LINE 2 POSITION 30.
           DISPLAY "MANTENIMIENTO DE LA CAJA" LINE 5 POSITION 30.
           DISPLAY "  ------- -- ----" LINE 3 POSITION 30.
           DISPLAY "------------- -- -------" LINE 6 POSITION 30.
           DISPLAY "Empresa...:" LINE 7 POSITION 4.
           DISPLAY "Sucursal..:" LINE 8 POSITION 4.
           DISPLAY "Turno.....:" LINE 9 POSITION 4.
           DISPLAY "Fecha.....:" LINE 10 POSITION 4.
           DISPLAY "Condicion.:" LINE 11 POSITION 4.
           DISPLAY "Comprobante.......:" LINE 12 POSITION 4.
           DISPLAY "Rubro.............:" LINE 14 POSITION 4.
           DISPLAY "Cuenta............:" LINE 15 POSITION 4.
           DISPLAY "Sub Cuenta........:" LINE 16 POSITION 4.
           DISPLAY "Tipo Pago.........:" LINE 17 POSITION 4.
           DISPLAY "Importe...........:" LINE 18 POSITION 4.
           DISPLAY "Importe Cheque....:" LINE 19 POSITION 4.
           DISPLAY "Tarjeta Utilizada.:" LINE 20 POSITION 4.
           DISPLAY "0- Suma  1- Resta.:" LINE 21 POSITION 4.
           DISPLAY "Mensajes.:" LINE 23 POSITION 2.
       R1.
           DISPLAY "º" LINE I POSITION 1.
           DISPLAY "º" LINE I POSITION 80.
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
       BAJA.
           DELETE LACAJA INVALID KEY DISPLAY "NO BAJO".
           GO TO TURNO.
       SE-MODIF.
           MOVE "M" TO A
           GO TO C1.

       CONDI-MENS.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.
           DISPLAY "1.- Contado    9.- Cobros" 
                LINE 23 POSITION 15 SIZE 64.

       C4-MENS.
           DISPLAY "E.- Efectivo  C.- Cheque   T.-Tarjeta  O.- Otros"
                LINE 23 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.
       MENS.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.
           DISPLAY "Con Escape o Flecha ^ sale al Menu" LINE 23 
              POSITION 15 SIZE 64.                 
       MENS1.           
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.
           DISPLAY "Con las Flecha Retrocede o Avanza" LINE 23 
              POSITION 15 SIZE 64.


