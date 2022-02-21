       IDENTIFICATION DIVISION.
       PROGRAM-ID.       ENTREGA.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "CHEQUES.SEL".
              COPY "EMPRE.SEL".
              COPY "BCO.SEL".
              COPY "CPOSTAL.SEL".
              COPY "CUENTA.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "CHEQUES.REG".
              COPY "EMPREBM.REG".
              COPY "BCO.REG".
              COPY "CPOSTAL.REG".
              COPY "CUENTABM.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "CHEQUES.WOR".
       01  A                      PIC X.
       77  EXISTE                 PIC X(2) VALUE "SI".       
       01  NUMERO                 PIC 9(6).

       01  CAMPOS-EDITADOS.
          03  WCLAVE-WCHEQUES-ED.
              05  WCH-EMP-ED                PIC ZZZZ.
              05  WCH-SUC-ED                PIC ZZ.
              05  WCH-BCO-ED                PIC ZZZZ.
              05  WCH-CPOSTAL-ED            PIC ZZZZ.
              05  WCH-NUMERO-CH-ED          PIC ZZZZZZZZZZ.
          03  WCH-FEC-REC-ED.
              05  WCH-DIA-REC-ED            PIC ZZ/.
              05  WCH-MES-REC-ED            PIC ZZ/.
              05  WCH-ANO-REC-ED            PIC ZZZZ.
          03  WCH-CLAVE-FACTURA-ED.
              05  WCH-NRO-FACTURA-ED        PIC ZZZZZZZZ.
          03  WCH-NRO-CTA-ED                PIC ZZZZZZZZ.
          03  WCH-IMP-FACTURA-ED            PIC ZZ,ZZZ,ZZ9.99.
          03  WCH-CLAVE-FEC-VENC-ED.
              05  WCH-FEC-VENC-ED.
                  07  WCH-DIA-VENC-ED       PIC ZZ/.
                  07  WCH-MES-VENC-ED       PIC ZZ/.
                  07  WCH-ANO-VENC-ED       PIC ZZZZ.
          03  WCH-FECHA-ENTREGA-ED.
              05  WCH-DIA-ENT-ED            PIC ZZ/.
              05  WCH-MES-ENT-ED            PIC ZZ/.
              05  WCH-ANO-ENT-ED            PIC ZZZZ.
          03  WCH-RUBRO-ED                  PIC ZZZZZZZ.       
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "CHEQUES.ERR".
           COPY "EMPRE.ERR".
           COPY "BCO.ERR".
           COPY "CPOSTAL.ERR".
           COPY "CUENTA.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN INPUT  CHEQUES 
                INPUT  EMPRESAS BCO CPOSTAL CUENTAS.           
           PERFORM ARMA-PANTALLA.
              
       CAP-CLAVE.           
           PERFORM MENS.
           DISPLAY SPACES LINE 6 POSITION 15 SIZE 4 REVERSE
           ACCEPT WCH-EMP LINE 6 POSITION 15 NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI
                      MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           MOVE WCH-EMP TO WCH-EMP-ED.
           DISPLAY WCH-EMP-ED LINE 6 POSITION 15 SIZE 4.
       CAP-CLAVE1.
           PERFORM MENS1.
           DISPLAY SPACES LINE 6 POSITION 20 SIZE 2 REVERSE.
           ACCEPT WCH-SUC LINE 6 POSITION 20 NO BEEP TAB
                 CONVERT REVERSE ON EXCEPTION CONDI MOVE 1
                         TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE.
           DISPLAY WCH-SUC LINE 6 POSITION 20 SIZE 2. 
           MOVE WCH-EMP TO NUM-EMP.
           MOVE WCH-SUC TO SUC-EMP.
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                        LINE 6 POSITION 30 SIZE 40
                        ACCEPT QQ
                        GO TO CAP-CLAVE1.
           DISPLAY RAZ-SOC-EMP LINE 6 POSITION 30 SIZE 40.
       CAP-CLAVE2.
           PERFORM BORRA-CAP.
           PERFORM MENS1.
           DISPLAY SPACES LINE 7 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WCH-BCO LINE 7 POSITION 40 NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE1.
           DISPLAY WCH-BCO LINE 7 POSITION 40 SIZE 4.
           MOVE WCH-BCO TO BCO-NUMERO.
           READ BCO INVALID KEY MOVE "****No Existe Bco****" 
                TO BCO-NOMBRE.
           DISPLAY BCO-NOMBRE LINE 7 POSITION 45 SIZE 35.
       CAP-CLAVE3.
           PERFORM MENS1.
           DISPLAY SPACES LINE 8 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WCH-CPOSTAL LINE 8 POSITION 40 NO BEEP TAB 
               CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE2.
           DISPLAY WCH-CPOSTAL LINE 8 POSITION 40 SIZE 4.
           MOVE WCH-CPOSTAL TO C-POS.
           MOVE 0           TO C-POS-1
           READ CPOSTAL INVALID KEY MOVE "***No Existe Cpostal***"
                TO NOM-POSTAL.
           DISPLAY NOM-POSTAL LINE 8 POSITION 45 SIZE 30.
       CAP-CLAVE4.
           PERFORM MENS1.
           DISPLAY SPACES LINE 9 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WCH-NUMERO-CH LINE 9 POSITION 40 NO BEEP TAB 
                  CONVERT  REVERSE ON EXCEPTION CONDI MOVE 1 
                         TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE3.
           MOVE WCH-NUMERO-CH TO WCH-NUMERO-CH-ED.
           DISPLAY WCH-NUMERO-CH-ED LINE 9 POSITION 40 SIZE 10.
                    
           MOVE WCH-EMP      TO CH-EMP.        
           MOVE WCH-SUC      TO CH-SUC.
           MOVE WCH-BCO      TO CH-BCO.
           MOVE WCH-CPOSTAL  TO CH-CPOSTAL.
           MOVE WCH-NUMERO-CH TO CH-NUMERO-CH 
                                 CH-NRO-CHEQUE 
                                 CH-NUM-CHEQUE.
           MOVE SPACES TO A.          
           MOVE "SI" TO EXISTE.
           READ CHEQUES INVALID KEY DISPLAY "No Existe cheque"
                LINE 23 POSITION 15 SIZE 64 BLINK
                ACCEPT QQ
                GO TO CAP-CLAVE4. 
           
           PERFORM CHEQUES-MOVER.
           MOVE "SI" TO EXISTE.
           
           IF EXISTE = "SI" PERFORM MOSTRAR THRU MOSTRAR-F.
           IF CH-ANO-ENT = 0
              AND CH-MES-ENT = 0          
                  AND CH-DIA-ENT = 0
                      GO TO C6.
           DISPLAY SPACES LINE 23 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 24 POSITION 15 SIZE 64.                     
           DISPLAY "                -M- Modifica " LINE 23 POSITION 15.
           ACCEPT DED LINE 23 POSITION 70 NO BEEP TAB ON 
                 EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FFFF.
                IF DED = "M" OR
                   DED = "m" GO TO SE-MODIF.
           GO TO CAP-CLAVE4.


       C6.           
           PERFORM MENS1.
           DISPLAY SPACES  LINE 15 POSITION 40 SIZE 30 REVERSE.
           MOVE "M" TO A.
           ACCEPT WCH-ENTREGADO-A LINE 15 POSITION 40 
              ECHO UPDATE NO BEEP TAB 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           DISPLAY  WCH-ENTREGADO-A LINE 15 POSITION 40 SIZE 30.            
       C7.           
           PERFORM MENS1.
           DISPLAY SPACES  LINE  16 POSITION 40 SIZE 8 REVERSE.
           ACCEPT WCH-FECHA-ENTREGA LINE 16 POSITION 40 
               ECHO UPDATE NO BEEP TAB 
                   REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C6. 
           IF WCH-FECHA-ENTREGA = 0 
              GO TO C7.
           MOVE WCH-DIA-ENT TO WCH-DIA-ENT-ED.
           MOVE WCH-MES-ENT TO WCH-MES-ENT-ED.
           MOVE WCH-ANO-ENT TO WCH-ANO-ENT-ED.
           DISPLAY  WCH-FECHA-ENTREGA-ED LINE 16 POSITION 40 SIZE 10.            
       C8.           
           PERFORM MENS1.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 30 REVERSE.
           ACCEPT WCH-CONCEPTO-ENT LINE 17 POSITION 40 
                 ECHO UPDATE NO BEEP TAB 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO C7.
           DISPLAY WCH-CONCEPTO-ENT LINE 17 POSITION 40 SIZE 30.           
           
           PERFORM MOVER-CHEQUES.
       
       REGRABE.
      *     DISPLAY CH-DIA-ENT " " CH-MES-ENT " " CH-ANO-ENT
      *     ACCEPT QQ.
           CLOSE CHEQUES.
           OPEN I-O CHEQUES.
           REWRITE REG-CHEQUES INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 24 POSITION 20.
           CLOSE CHEQUES.
           OPEN INPUT CHEQUES.
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE2.
   
           COPY "CHEQUES.RUT".
       MOSTRAR.
           MOVE  WCH-DIA-REC TO WCH-DIA-REC-ED.
           MOVE  WCH-MES-REC TO WCH-MES-REC-ED.
           MOVE  WCH-ANO-REC TO WCH-ANO-REC-ED.
           DISPLAY WCH-FEC-REC-ED  LINE 10 POSITION 40 SIZE 10.           
           MOVE  WCH-NRO-FACTURA TO WCH-NRO-FACTURA-ED.
           DISPLAY  WCH-NRO-FACTURA-ED LINE 11 POSITION 40 SIZE 8.
           MOVE WCH-RUBRO TO WCH-RUBRO-ED.
           DISPLAY WCH-RUBRO-ED LINE 12 POSITION 40 SIZE 7.           
           MOVE WCH-NRO-CTA TO WCH-NRO-CTA-ED.
           DISPLAY  WCH-NRO-CTA-ED LINE 12 POSITION 48 SIZE 8.            
           MOVE WCH-EMP          TO EMP-CTA
           MOVE WCH-SUC          TO SUC-CTA
           MOVE WCH-RUBRO        TO RUBRO-CTA.
           MOVE WCH-NRO-CTA      TO CUENTA.
           MOVE 0                TO SUB-CUENTA.
           READ CUENTAS INVALID KEY MOVE "***No Existe Cuenta***"
                TO NOMBRE-C.
           DISPLAY NOMBRE-C LINE 12 POSITION 57 SIZE 22.
           MOVE  WCH-IMP-FACTURA TO WCH-IMP-FACTURA-ED.
           DISPLAY  WCH-IMP-FACTURA-ED LINE 13 POSITION 40 SIZE 13.           
           MOVE WCH-DIA-VENC TO WCH-DIA-VENC-ED.
           MOVE WCH-MES-VENC TO WCH-MES-VENC-ED.
           MOVE WCH-ANO-VENC TO WCH-ANO-VENC-ED.
           DISPLAY  WCH-FEC-VENC-ED LINE 14  POSITION 40 SIZE 10.            
           DISPLAY  CH-ENTREGADO-A LINE 15 POSITION 40 SIZE 30.            
           MOVE CH-DIA-ENT TO WCH-DIA-ENT-ED.
           MOVE CH-MES-ENT TO WCH-MES-ENT-ED.
           MOVE CH-ANO-ENT TO WCH-ANO-ENT-ED.
           DISPLAY  WCH-FECHA-ENTREGA-ED LINE 16 POSITION 40 SIZE 10.            
           DISPLAY CH-CONCEPTO-ENT LINE 17 POSITION 40 SIZE 30.           
       
       MOSTRAR-F.
           EXIT.
       
       FINAL1.
            CLOSE CHEQUES EMPRESAS CPOSTAL BCO CUENTAS.
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

           DISPLAY "Sistema de Cheques" LINE 2 POSITION 2.
           DISPLAY "------- -- -------" LINE 3 POSITION 2.
           DISPLAY "Entrega de Cheques en Cartera" LINE 4 POSITION 2.
           DISPLAY "Empresa.:" LINE 6 POSITION 2.
           DISPLAY "B a n c o .........:" LINE 7 POSITION 20.
           DISPLAY "Codigo Postal .....:" LINE 8 POSITION 20.
           DISPLAY "Numero Cheque .....:" LINE 9 POSITION 20.
           DISPLAY "Fecha Recibido ....:" LINE 10 POSITION 20.
           DISPLAY "N§ Factura ........:" LINE 11 POSITION 20.
           DISPLAY "Rubro/N§ Cuenta ...:" LINE 12 POSITION 20.
           DISPLAY "Importe Cheque ....:" LINE 13 POSITION 20.
           DISPLAY "Fecha Vencimiento .:" LINE 14 POSITION 20.
           DISPLAY "Entregado  A ? ....:" LINE 15 POSITION 20.
           DISPLAY "Fecha Entrega .....:" LINE 16 POSITION 20.
           DISPLAY "Concepto  Entrega .:" LINE 17 POSITION 20.

       R1.
           DISPLAY "º" LINE I POSITION 1.
           DISPLAY "º" LINE I POSITION 80.
       BORRA-CAP.
           DISPLAY SPACES LINE 7 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 8 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 9 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 11 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 39.
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
           IF ESCAPE-X GO TO CAP-CLAVE4.
           IF DED = "S" OR DED = "s"
              GO TO SI-BAJA.
           GO TO CAP-CLAVE4.
       SI-BAJA.
           CLOSE CHEQUES.
           OPEN I-O CHEQUES.
           DELETE CHEQUES INVALID KEY DISPLAY "NO BAJO".
           CLOSE CHEQUES.
           OPEN INPUT CHEQUES.
           GO TO CAP-CLAVE4.
       SE-MODIF.
           MOVE "M" TO A.
           GO TO C6.


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
   
