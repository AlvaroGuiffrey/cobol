       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CHCH.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "CHEQUES.SEL".
              COPY "EMPRE.SEL".
              COPY "CPOSTAL.SEL".
              COPY "BCO.SEL".
              COPY "CUENTA.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "CHEQUES.REG".
              COPY "EMPREBM.REG".
              COPY "CPOSTAL.REG".
              COPY "BCO.REG".
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
              05  WCH-ANO-REC-ED            PIC ZZ.
          03  WCH-CLAVE-FACTURA-ED.
              05  WCH-NRO-FACTURA-ED        PIC ZZZZZZZZ.
          03  WCH-NRO-CTA-ED                PIC ZZZZZZZZ.
          03  WCH-IMP-FACTURA-ED            PIC ZZ,ZZZ,ZZ9.99.
          03  WCH-CLAVE-FEC-VENC-ED.
              05  WCH-FEC-VENC-ED.
                  07  WCH-DIA-VENC-ED       PIC ZZ/.
                  07  WCH-MES-VENC-ED       PIC ZZ/.
                  07  WCH-ANO-VENC-ED       PIC ZZ.
          03  WCH-FECHA-ENTREGA-ED.
              05  WCH-DIA-ENT-ED            PIC ZZ/.
              05  WCH-MES-ENT-ED            PIC ZZ/.
              05  WCH-ANO-ENT-ED            PIC ZZ.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "CHEQUES.ERR".
           COPY "EMPRE.ERR".
           COPY "CPOSTAL.ERR".
           COPY "BCO.ERR".
           COPY "CUENTA.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    CHEQUES
                INPUT  EMPRESAS CPOSTAL BCO CUENTAS.
           PERFORM ARMA-PANTALLA.
              
       CAP-CLAVE.           
           PERFORM MENS.
           DISPLAY SPACES LINE 6 POSITION 15 SIZE 4 REVERSE
           ACCEPT WCH-EMP LINE 6 POSITION 15 ECHO UPDATE NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI
                      MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO FINAL1.
           MOVE WCH-EMP TO WCH-EMP-ED.
           DISPLAY WCH-EMP-ED LINE 6 POSITION 15 SIZE 4.
       CAP-CLAVE1.
           PERFORM MENS1.
           DISPLAY SPACES LINE 6 POSITION 20 SIZE 2 REVERSE.
           ACCEPT WCH-SUC LINE 6 POSITION 20 ECHO UPDATE NO BEEP TAB
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
           PERFORM MENS1.
           DISPLAY SPACES LINE 7 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WCH-BCO LINE 7 POSITION 40 ECHO UPDATE NO BEEP TAB 
                  CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE1.
           DISPLAY WCH-BCO LINE 7 POSITION 40 SIZE 4.
           MOVE WCH-BCO TO BCO-NUMERO.
           READ BCO INVALID KEY MOVE "****No Existe Bco.****"
                TO BCO-NOMBRE.
           DISPLAY BCO-NOMBRE LINE 7 POSITION 45 SIZE 30.
       CAP-CLAVE3.
           PERFORM MENS1.
           DISPLAY SPACES LINE 8 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WCH-CPOSTAL LINE 8 POSITION 40 ECHO UPDATE NO BEEP TAB 
               CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1 GO TO CAP-CLAVE2.
           DISPLAY WCH-CPOSTAL LINE 8 POSITION 40 SIZE 4.
           MOVE WCH-CPOSTAL TO C-POS.
           MOVE 0           TO C-POS-1.
           READ CPOSTAL INVALID KEY MOVE "***No Existe Cpostal***" 
                TO NOM-POSTAL.
           DISPLAY NOM-POSTAL LINE 8 POSITION 45 SIZE 35.
       CAP-CLAVE4.
           PERFORM MENS1.
           DISPLAY SPACES LINE 9 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WCH-NUMERO-CH LINE 9 POSITION 40 ECHO UPDATE 
             NO BEEP TAB CONVERT  REVERSE ON EXCEPTION 
                     CONDI MOVE 1 TO SW1.
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
           MOVE "SI" TO EXISTE
           DISPLAY SPACES LINE 23 POSITION 2 SIZE 79.
           READ CHEQUES INVALID KEY DISPLAY "No Existe Cheque"
                        LINE 23 POSITION 15
                        ACCEPT QQ
                        GO TO CAP-CLAVE.
           IF CH-DIA-ENT = 0 
              AND CH-MES-ENT = 0
                  AND CH-ANO-ENT = 0 
                      DISPLAY "Cheque No Entregado"
                              LINE 23 POSITION 15 SIZE 64
                              ACCEPT QQ
                              GO TO CAP-CLAVE.
           PERFORM CHEQUES-MOVER.
           IF EXISTE = "SI" PERFORM MOSTRAR THRU MOSTRAR-F.
           DISPLAY "ENTER Para Continuar" LINE 23 POSITION 15 SIZE 64.
           ACCEPT QQ LINE 23 POSITION 70 NO BEEP.
           GO TO CAP-CLAVE4.
           COPY "CHEQUES.RUT".
       MOSTRAR.
           MOVE  WCH-DIA-REC TO WCH-DIA-REC-ED.
           MOVE  WCH-MES-REC TO WCH-MES-REC-ED.
           MOVE  WCH-ANO-REC TO WCH-ANO-REC-ED.
           DISPLAY WCH-FEC-REC-ED  LINE 10 POSITION 40 SIZE 8.           
           MOVE  WCH-NRO-FACTURA TO WCH-NRO-FACTURA-ED.
           DISPLAY  WCH-NRO-FACTURA-ED LINE 11 POSITION 40 SIZE 8.           
           MOVE WCH-NRO-CTA TO WCH-NRO-CTA-ED.
           DISPLAY  WCH-NRO-CTA-ED LINE 12 POSITION 40 SIZE 8.            
           MOVE WCH-EMP      TO EMP-CTA
           MOVE WCH-SUC      TO SUC-CTA
           MOVE 2001            TO RUBRO-CTA
           MOVE WCH-NRO-CTA  TO CUENTA
           MOVE 0            TO SUB-CUENTA.
           READ CUENTAS INVALID KEY MOVE "***No existe Cuenta***"
                TO NOMBRE-C.
           DISPLAY NOMBRE-C LINE 12 POSITION 50 SIZE 29.
           
           MOVE  WCH-IMP-FACTURA TO WCH-IMP-FACTURA-ED.
           DISPLAY  WCH-IMP-FACTURA-ED LINE 13 POSITION 40 SIZE 13.           
           MOVE WCH-DIA-VENC TO WCH-DIA-VENC-ED.
           MOVE WCH-MES-VENC TO WCH-MES-VENC-ED.
           MOVE WCH-ANO-VENC TO WCH-ANO-VENC-ED.
           DISPLAY  WCH-FEC-VENC-ED LINE 14  POSITION 40 SIZE 8.            
           DISPLAY  WCH-ENTREGADO-A LINE 15 POSITION 40 SIZE 30.            
           MOVE WCH-DIA-ENT TO WCH-DIA-ENT-ED.
           MOVE WCH-MES-ENT TO WCH-MES-ENT-ED.
           MOVE WCH-ANO-ENT TO WCH-ANO-ENT-ED.
           DISPLAY  WCH-FECHA-ENTREGA-ED LINE 16 POSITION 40 SIZE 8.            
           DISPLAY WCH-CONCEPTO-ENT LINE 17 POSITION 40 SIZE 30.           
       
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
           DISPLAY "M a n t e n i m i e n t o" LINE 4 POSITION 2.
           DISPLAY "Empresa.:" LINE 6 POSITION 2.
           DISPLAY "B a n c o .........:" LINE 7 POSITION 20.
           DISPLAY "Codigo Postal .....:" LINE 8 POSITION 20.
           DISPLAY "Numero Cheque .....:" LINE 9 POSITION 20.
           DISPLAY "Fecha Recibido ....:" LINE 10 POSITION 20.
           DISPLAY "N§ Factura ........:" LINE 11 POSITION 20.
           DISPLAY "N§ Cuenta .........:" LINE 12 POSITION 20.
           DISPLAY "Importe Cheque ....:" LINE 13 POSITION 20.
           DISPLAY "Fecha Vencimiento .:" LINE 14 POSITION 20.
           DISPLAY "Entregado  A ? ....:" LINE 15 POSITION 20.
           DISPLAY "Fecha Entrega .....:" LINE 16 POSITION 20.
           DISPLAY "Concepto  Entrega .:" LINE 17 POSITION 20.
       R1.
           DISPLAY "º" LINE I POSITION 1.
           DISPLAY "º" LINE I POSITION 80.

       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
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
