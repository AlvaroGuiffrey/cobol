
       IDENTIFICATION DIVISION.
       PROGRAM-ID.       ARET.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "EMPRE.SEL".
              COPY "ARET.SEL".
              COPY "NUMERO.SEL".
              COPY "CUENTA.SEL".
       DATA DIVISION.
       FILE SECTION.
              
              COPY "EMPREBM.REG".
              COPY "NUMERO.REG".
              COPY "ARET.REG".
              COPY "CUENTABM.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "ARET.WOR".
       
       01  WREG-ARETIRAR-ED.
           03  WCLAVE-ARETIRAR-ED.
               05  WAR-EMP-ED         PIC ZZZZ.
               05  WAR-SUC-ED         PIC ZZ.
               05  WAR-RUB-ED         PIC ZZZZZZ.
               05  WAR-CTA-ED         PIC ZZZZZZ.
               05  WAR-S-CTA-ED       PIC ZZ.
               05  WAR-FECHA-ED.
                   07  WAR-DIA-ED     PIC 99/.
                   07  WAR-MES-ED     PIC 99/.
                   07  WAR-ANO-ED     PIC 99.
               05  WAR-COMPROB-ED     PIC ZZZZZZZZ.
               05  WAR-RENG-ED        PIC ZZZZZZ.
           03  WAR-ARTICULO-ED        PIC ZZZZZZ.
           03  WAR-CANTIDAD-ED        PIC ZZZZZZ.
           03  WAR-IMPORTE-ED         PIC ZZZ,ZZ9.99.
           03  WAR-IMP-ARE-ED         PIC ZZZ,ZZ9.99.
           03  WAR-RET-DEP-ED         PIC 9.
       
       
       
       01  RA       PIC X(80) VALUE ALL "ƒ".
       01  A         PIC X.
       77  EXISTE    PIC X(2) VALUE "SI".
       
       PROCEDURE DIVISION.
       DECLARATIVES.
              
              COPY "EMPRE.ERR".
              COPY "ARET.ERR".
              COPY "NUMERO.ERR".
              COPY "CS.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O  ARETIRAR
                INPUT EMPRESAS NUMERO CUENTAS.
           PERFORM ARMA-PANTALLA.
              
           
           
       CAP-CLAVE1.    
           PERFORM MENS.
           DISPLAY SPACES LINE 8 POSITION 40 SIZE 4 REVERSE.
           ACCEPT WAR-EMP LINE 8 POSITION 40 NO BEEP TAB CONVERT
                REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO FINAL-X.
           MOVE WAR-EMP TO WAR-EMP-ED.
           DISPLAY WAR-EMP-ED LINE 8 POSITION 40 SIZE 4.
       CAP-CLAVE2.
           PERFORM MENS1.
           DISPLAY SPACES LINE 8 POSITION 45 SIZE 2 REVERSE.
           ACCEPT WAR-SUC LINE 8 POSITION 45 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE1.
           MOVE WAR-SUC TO WAR-SUC-ED.
           DISPLAY WAR-SUC-ED LINE 8 POSITION 45 SIZE 2.
           MOVE WAR-EMP TO NUM-EMP
           MOVE WAR-SUC TO SUC-EMP
           READ EMPRESAS INVALID KEY 
              DISPLAY "No esta creada la empresa,ENTER para continuar"
               LINE 23 POSITION 15 SIZE 64
                ACCEPT QQ
                 GO TO CAP-CLAVE1.
           DISPLAY RAZ-SOC-EMP LINE 8 POSITION 50 SIZE 29.
           
           
       CAP-CLAVE2-0.           
           DISPLAY SPACES LINE 9 POSITION 40 SIZE 6 REVERSE.
           ACCEPT WAR-RUB LINE 9 POSITION 40 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE2.
           MOVE WAR-RUB TO WAR-RUB-ED.
           DISPLAY WAR-RUB-ED LINE 9 POSITION 40 SIZE 6.

       
       CAP-CLAVE3.           
           PERFORM LIMPIE THRU LIMPIE-F.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 6 REVERSE.
           ACCEPT WAR-CTA LINE 10 POSITION 40 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE2-0.
           MOVE WAR-CTA TO WAR-CTA-ED.
           DISPLAY WAR-CTA-ED LINE 10 POSITION 40 SIZE 6.


       CAP-CLAVE4.           
           DISPLAY SPACES LINE 10 POSITION 47 SIZE 2 REVERSE.
           ACCEPT WAR-S-CTA LINE 10 POSITION 47 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE3.
           MOVE WAR-S-CTA TO WAR-S-CTA-ED.
           DISPLAY WAR-S-CTA-ED LINE 10 POSITION 47 SIZE 2.
           MOVE WAR-EMP         TO EMP-CTA
           MOVE WAR-SUC         TO SUC-CTA
           MOVE WAR-RUB         TO RUBRO-CTA
           MOVE WAR-CTA         TO CUENTA
           MOVE WAR-S-CTA       TO SUB-CUENTA.
           READ CUENTAS INVALID KEY 
             DISPLAY "No existe cuenta" LINE 24 POSITION 15 SIZE 64
             ACCEPT QQ
             GO TO CAP-CLAVE3.
           DISPLAY NOMBRE-C LINE 10 POSITION  50 SIZE 29.

       
       CAP-CLAVE5.
           DISPLAY SPACES LINE 11 POSITION 40 SIZE 8 REVERSE.
           ACCEPT WAR-COMPROB LINE 11 POSITION 40 NO BEEP TAB 
                CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE4.
           MOVE WAR-COMPROB TO WAR-COMPROB-ED.
           DISPLAY WAR-COMPROB-ED LINE 11 POSITION 40 SIZE 8.
           
           
           MOVE WAR-EMP         TO AR-EMP
           MOVE WAR-SUC         TO AR-SUC
           MOVE WAR-RUB         TO AR-RUB
           MOVE WAR-CTA         TO AR-CTA
           MOVE WAR-S-CTA       TO AR-S-CTA
           MOVE WAR-COMPROB     TO AR-COMPROB           
           MOVE 1               TO AR-RENG.
           
           MOVE SPACES TO A.
           MOVE "SI" TO EXISTE.
           READ ARETIRAR INVALID KEY MOVE "NO" TO EXISTE 
                                      MOVE 0  TO  WAR-DIA
                                                  WAR-MES
                                                  WAR-ANO
                                                  WAR-ARTICULO   
                                                  WAR-CANTIDAD
                                                  WAR-IMPORTE
                                                  WAR-IMP-ARE
                                                  WAR-RET-DEP
                                                  GO TO C0.
           PERFORM ARET-MOVER.
           IF EXISTE = "SI" PERFORM MOSTRAR THRU MOSTRAR-F.
           DISPLAY "-M- Modifica -B- Baja" LINE 23 POSITION 15
                 SIZE 64.
           ACCEPT DED LINE 23 POSITION 70 NO BEEP TAB ON 
                 EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FFFF.
                IF DED = "M" 
                   OR DED = "m" GO TO SE-MODIF
                   ELSE   
                       IF DED = "B"
                          OR DED = "b" GO TO BAJA.
           GO TO CAP-CLAVE3.


       
       
       C0.
           PERFORM MENS1.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 8.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 6 REVERSE.
           ACCEPT WAR-FECHA LINE 13 POSITION 40 NO BEEP TAB 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE5.
           MOVE WAR-DIA    TO WAR-DIA-ED.
           MOVE WAR-MES    TO WAR-MES-ED.
           MOVE WAR-ANO    TO WAR-ANO-ED.
           DISPLAY WAR-FECHA-ED LINE 13 POSITION 40.
       
       C1.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 6 REVERSE .
           ACCEPT WAR-ARTICULO LINE 14 POSITION 40 ECHO UPDATE 
                  NO BEEP TAB CONVERT REVERSE ON EXCEPTION 
                     CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C0.
           MOVE WAR-ARTICULO TO WAR-ARTICULO-ED.
           DISPLAY WAR-ARTICULO-ED LINE 14 POSITION 40 SIZE 6.

           MOVE WAR-ARTICULO TO COD-NUM.
           READ NUMERO INVALID KEY 
              DISPLAY "No existe articulo"
               LINE 24 POSITION 15 SIZE 64
                ACCEPT QQ
                 GO TO C1.
           DISPLAY DET-NUM LINE 14 POSITION 50 SIZE 30.
       
       C2.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 6 REVERSE.
           ACCEPT WAR-CANTIDAD LINE 15 POSITION 40 ECHO UPDATE 
                  NO BEEP TAB CONVERT REVERSE ON EXCEPTION 
                     CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1.
           MOVE WAR-CANTIDAD TO WAR-CANTIDAD-ED.
           DISPLAY WAR-CANTIDAD-ED LINE 15 POSITION 40 SIZE 6.
        
       C3.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WAR-IMPORTE LINE 16 POSITION 40 ECHO UPDATE 
                  NO BEEP TAB CONVERT REVERSE ON EXCEPTION 
                     CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C2.
           MOVE WAR-IMPORTE TO WAR-IMPORTE-ED.
           DISPLAY WAR-IMPORTE-ED LINE 16 POSITION 40 SIZE 13.
       C4.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 10 REVERSE.
           ACCEPT WAR-IMP-ARE LINE 17 POSITION 40 ECHO UPDATE 
                  NO BEEP TAB CONVERT REVERSE ON EXCEPTION 
                     CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C3.
           MOVE WAR-IMP-ARE TO  WAR-IMP-ARE-ED.
           DISPLAY WAR-IMP-ARE-ED LINE 17 POSITION 40 SIZE 13.
           
           PERFORM MOVER-ARET.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-ARETIRAR INVALID KEY DISPLAY "NO GRABO".                
                 GO TO CAP-CLAVE3.


       REGRABE.
           REWRITE REG-ARETIRAR INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 24 POSITION 20
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE3.
        
           COPY "ARET.RUT".


       MOSTRAR.
           MOVE WAR-DIA    TO WAR-DIA-ED.
           MOVE WAR-MES    TO WAR-MES-ED.
           MOVE WAR-ANO    TO WAR-ANO-ED.
           DISPLAY WAR-FECHA-ED LINE 13 POSITION 40 .
           MOVE WAR-ARTICULO TO WAR-ARTICULO-ED.
           DISPLAY WAR-ARTICULO-ED LINE 14 POSITION 40 SIZE 6
                    .

           MOVE WAR-ARTICULO TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "**************" TO DET-NUM.
           DISPLAY DET-NUM LINE 14 POSITION 50 SIZE 30 .
       
           MOVE WAR-CANTIDAD TO WAR-CANTIDAD-ED.
           DISPLAY WAR-CANTIDAD-ED LINE 15 POSITION 40 SIZE 6 .
           MOVE WAR-IMPORTE TO WAR-IMPORTE-ED.
           DISPLAY WAR-IMPORTE-ED LINE 16 POSITION 40 SIZE 13 .
           MOVE WAR-IMP-ARE TO  WAR-IMP-ARE-ED.
           DISPLAY WAR-IMP-ARE-ED LINE 17 POSITION 40 SIZE 13 .
       
       MOSTRAR-F.
           EXIT.
       FINAL-X.
           CLOSE EMPRESAS ARETIRAR CUENTAS NUMERO. 
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I FROM 1 BY 1 UNTIL I > 25

           
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 6 POSITION 1.
           DISPLAY RA LINE 12 POSITION 1.
           DISPLAY RA LINE 22 POSITION 1.           
           DISPLAY RA LINE 25 POSITION 1.

           DISPLAY "⁄" LINE 1 POSITION 1.
           DISPLAY "√" LINE 6 POSITION 1.
           DISPLAY "√" LINE 12 POSITION 1.
           DISPLAY "√" LINE 22 POSITION 1.           
           DISPLAY "¿" LINE 25 POSITION 1.
           
           DISPLAY "ø" LINE 1 POSITION 80.
           DISPLAY "¥" LINE 6 POSITION 80.
           DISPLAY "¥" LINE 12 POSITION 80.
           DISPLAY "¥" LINE 22 POSITION 80.           
           DISPLAY "Ÿ" LINE 25 POSITION 80.


           DISPLAY "‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹"
                   LINE 1 POSITION 19.
           DISPLAY "∞€      Sistema Aministrativo Contable y    €∞"
                  LINE 2 POSITION 19.
           DISPLAY "∞€           de Gestion Comercial           €∞"
                  LINE 3 POSITION 19.
           DISPLAY "∞€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€∞"
                  LINE 4 POSITION 19.
           DISPLAY "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
                   LINE 5 POSITION 19.
           DISPLAY "¥ Captura de Articulos a Retirar √" 
                   LINE 6 POSITION 25 REVERSE.
           DISPLAY "Empresa ..:" LINE 8 POSITION 20.           
           DISPLAY "Rubro ............:" LINE 9 POSITION 20.
           DISPLAY "Cuenta ...........:" LINE 10 POSITION 20
                   "Nß Comprobante ...:" LINE 11 POSITION 20
                   "F e c h a ........:" LINE 13 POSITION 20
                   "Articulo .........:" LINE 14 POSITION 20
                   "Cantidad .........:" LINE 15 POSITION 20
                   "Importe ..........:" LINE 16 POSITION 20
                   "Importe Retirado .:" LINE 17 POSITION 20.
           DISPLAY "Mensajes :" LINE 23 POSITION 2. 
       R1.
           DISPLAY "≥" LINE I POSITION 1.
           DISPLAY "≥" LINE I POSITION 80.
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
       BAJA.
           DISPLAY "® Esta seguro de querer bajarlo (S/N) ?" 
                   LINE 23 POSITION 15 SIZE 64.
           MOVE "N" TO DED.
           ACCEPT DED LINE 23 POSITION 70 ECHO UPDATE 
              NO BEEP TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF DED = "S" OR DED = "s" GO TO SI-BAJA.
           GO TO CAP-CLAVE3.
       SI-BAJA.
           DELETE ARETIRAR INVALID KEY DISPLAY "NO BAJO".
           GO TO CAP-CLAVE3.
       SE-MODIF.
           MOVE "M" TO A
           GO TO C1.

       MENS.
           DISPLAY "Con Escape o Flecha ^ sale al Menu" LINE 23
                 POSITION 15 SIZE 64.
       MENS1.
           DISPLAY "Con Las Flechas Avanza o Retrocede" LINE 23
                 POSITION 15 SIZE 64.


       LIMPIE.    
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 15 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 39.
       LIMPIE-F.
           EXIT.
