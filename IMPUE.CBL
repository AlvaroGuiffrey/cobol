       IDENTIFICATION DIVISION.
       PROGRAM-ID.       IMPUE.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "EMPRE.SEL".
              COPY "IMPU.SEL".
              COPY "ACTI.SEL".              
       DATA DIVISION.
       FILE SECTION.
              COPY "EMPREBM.REG".
              COPY "IMPU.REG".
              COPY "ACTI.REG".              
       WORKING-STORAGE SECTION.
              COPY "UTILES.WOR".
              COPY "IMPU.WOR".
       01 REG-WIMP.
          03 CLAVE-WIMP-ED.
             05 EMP-ED        PIC ZZZZ.
             05 SUC-ED        PIC ZZ.
             05 IMP-ED        PIC Z.
             05 ACT-ED        PIC ZZZ.
             05 FEC-ED.
                07 DD-ED      PIC 99/.
                07 MM-ED      PIC 99/.
                07 AA-ED      PIC 99.
             05 COMPRO-ED     PIC ZZZZZZ.
      *-------------------------------------------------
          03 NET-ED           PIC ---,--9.99.
          03 CAL-ED           PIC Z.
          03 POR-ED           PIC Z9.999.
          03 MIN-ED           PIC ZZZ,ZZ9.99.
       
       01  RA       PIC X(80) VALUE ALL "ƒ".
       01  A         PIC X.
       77  EXISTE    PIC X(2) VALUE "SI".
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "EMPRE.ERR".
           COPY "IMPU.ERR".
           COPY "ACTI.ERR".           
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O  IMPU
                INPUT EMPRESAS ACTI.
           PERFORM ARMA-PANTALLA.
              
           
           
       CAP-CLAVE1.    
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 4 REVERSE.
           ACCEPT EMP LINE 10 POSITION 40 NO BEEP TAB CONVERT
                REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO FINAL-X.
           MOVE EMP TO EMP-ED.
           DISPLAY EMP-ED LINE 10 POSITION 40 SIZE 4.
       CAP-CLAVE2.
           PERFORM MENS1.
           DISPLAY SPACES LINE 10 POSITION 45 SIZE 2 REVERSE.
           ACCEPT SUC LINE 10 POSITION 45 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE1.
           MOVE SUC TO SUC-ED.
           DISPLAY SUC-ED LINE 10 POSITION 45 SIZE 2.
           MOVE EMP TO NUM-EMP
           MOVE SUC TO SUC-EMP
           READ EMPRESAS INVALID KEY 
              DISPLAY "No esta creada la empresa,ENTER para continuar"
               LINE 23 POSITION 15 SIZE 64
                ACCEPT QQ
                 GO TO CAP-CLAVE1.
           DISPLAY RAZ-SOC-EMP LINE 10 POSITION 50 SIZE 29.
       CAP-CLAVE3.
           PERFORM MENS1.
           DISPLAY SPACES LINE 11 POSITION 40 SIZE 1 REVERSE.
           ACCEPT IMP LINE 11 POSITION 40 NO BEEP TAB CONVERT 
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE2.
           DISPLAY IMP LINE 11 POSITION 40.
       CAP-CLAVE4.    
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 3 REVERSE.           
           ACCEPT ACT LINE 12 POSITION 40 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE3.           
           MOVE ACT TO ACT-ED.
           DISPLAY ACT-ED LINE 12 POSITION 40 SIZE 3.
           MOVE IMP TO IMP-ACT
           MOVE ACT TO ACT-ACT.
           READ ACTI INVALID KEY 
              DISPLAY "No existe actividad,ENTER para continuar"
               LINE 23 POSITION 15 SIZE 64
                ACCEPT QQ
                 GO TO CAP-CLAVE3.
           DISPLAY DET-ACT LINE 12 POSITION 45 SIZE 30.
       CAP-CLAVE5.
           
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 8.
           DISPLAY SPACES LINE 13 POSITION 40 SIZE 6 REVERSE.
           ACCEPT FEC LINE 13 POSITION 40 NO BEEP TAB 
             REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE3.
           MOVE DD TO DD-ED.
           MOVE MM TO MM-ED.
           MOVE AA TO AA-ED.
           DISPLAY FEC-ED LINE 13 POSITION 40 SIZE 8.           
           
       CAP-CLAVE6.    
           
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 6 REVERSE.
           ACCEPT COMPRO LINE 14 POSITION 40 NO BEEP TAB CONVERT
               REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE4.
           MOVE COMPRO TO COMPRO-ED.           
           DISPLAY COMPRO-ED LINE 14 POSITION 40.
            
           MOVE EMP     TO EMP-IMP
           MOVE SUC     TO SUC-IMP
           MOVE IMP     TO IMP-IMP
           MOVE ACT     TO ACT-IMP
           MOVE AA      TO AA-IMP
           MOVE MM      TO MM-IMP
           MOVE DD      TO DD-IMP
           MOVE COMPRO  TO COMPRO-IMP.

           MOVE SPACES TO A.
           MOVE "SI" TO EXISTE.
           READ IMPU INVALID KEY MOVE "NO" TO EXISTE 
                                 MOVE 0 TO NET
                                           CAL
                                           POR
                                           MIN
                                           GO TO C1.
           PERFORM IMPU-MOVER.
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

       C1.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 10 REVERSE.
           ACCEPT NET LINE 17 POSITION 40 ECHO UPDATE NO BEEP TAB 
               CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO CAP-CLAVE6.
           MOVE NET TO NET-ED.
           DISPLAY NET-ED LINE 17 POSITION 40 SIZE 20.
       C2.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE  1 REVERSE.
           ACCEPT CAL LINE 18 POSITION 40 ECHO UPDATE NO BEEP TAB 
                 CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1.
           MOVE CAL TO CAL-ED.
           DISPLAY CAL-ED LINE 18 POSITION 40 SIZE 20.
           
       C3.
           DISPLAY SPACES LINE 19 POSITION 40 SIZE 6 REVERSE.
           ACCEPT POR LINE 19 POSITION 40 ECHO UPDATE NO BEEP TAB 
                CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C2.
           MOVE POR TO POR-ED.
           DISPLAY POR-ED LINE 19 POSITION 40 SIZE 6.
           
       C4.
           DISPLAY SPACES LINE 20 POSITION 40 SIZE 10 REVERSE.
           ACCEPT MIN LINE 20 POSITION 40 ECHO UPDATE NO BEEP TAB 
               CONVERT REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C3.
           MOVE MIN TO MIN-ED.
           DISPLAY MIN-ED LINE 20 POSITION 40 SIZE 20.

           
           
           PERFORM MOVER-IMPU.
           IF A = "M" GO TO REGRABE.

       GRABA.
           WRITE REG-IMPU INVALID KEY DISPLAY "NO GRABO".                
                 GO TO CAP-CLAVE3.


       REGRABE.
           REWRITE REG-IMPU INVALID KEY DISPLAY " ERROR REGRABA " 
                   LINE 24 POSITION 20
           ACCEPT DED LINE 23 POSITION 70 NO BEEP.                
           MOVE SPACES TO A.
           GO TO CAP-CLAVE3.
        
           COPY "IMPU.RUT".


       MOSTRAR.
           MOVE NET TO NET-ED.
           DISPLAY NET-ED LINE 17 POSITION 40 SIZE 20.
           MOVE CAL TO CAL-ED.
           DISPLAY CAL-ED LINE 18 POSITION 40 SIZE 20.
           MOVE POR TO POR-ED.
           DISPLAY POR-ED LINE 19 POSITION 40 SIZE 6.
           MOVE MIN TO MIN-ED.
           DISPLAY MIN-ED LINE 20 POSITION 40 SIZE 20.
          
       MOSTRAR-F.
           EXIT.
       FINAL-X.
           CLOSE EMPRESAS IMPU ACTI . 
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I FROM 1 BY 1 UNTIL I > 25

           
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 5 POSITION 1.
           DISPLAY RA LINE 7 POSITION 1.
           DISPLAY RA LINE 15 POSITION 1.
           DISPLAY RA LINE 22 POSITION 1.           
           DISPLAY RA LINE 25 POSITION 1.

           DISPLAY "⁄" LINE 1 POSITION 1.
           DISPLAY "√" LINE 5 POSITION 1.
           DISPLAY "√" LINE 7 POSITION 1.
           DISPLAY "√" LINE 15 POSITION 1.
           DISPLAY "√" LINE 22 POSITION 1.           
           DISPLAY "¿" LINE 25 POSITION 1.
           
           DISPLAY "ø" LINE 1 POSITION 80.
           DISPLAY "¥" LINE 5 POSITION 80.
           DISPLAY "¥" LINE 7 POSITION 80.
           DISPLAY "¥" LINE 15 POSITION 80.
           DISPLAY "¥" LINE 22 POSITION 80.           
           DISPLAY "Ÿ" LINE 25 POSITION 80.


           DISPLAY "‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹"
                   LINE 1 POSITION 20.
           DISPLAY "€ Sistema Integral Administrativo Contable €∞"
                  LINE 2 POSITION 20.
           DISPLAY "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€∞"
                  LINE 3 POSITION 20.
           DISPLAY "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
                   LINE 4 POSITION 20.
           DISPLAY "Mantenimiento de Impuestos" 
                   LINE 6 POSITION 30.
           DISPLAY "Empresa .....:" LINE 10 POSITION 10.
           DISPLAY "Impuesto ....:" LINE 11 POSITION 10.
           DISPLAY "Actividad ...:" LINE 12 POSITION 10. 
           DISPLAY "F e c h a ...:" LINE 13 POSITION 10.
           DISPLAY "Comprobante .:" LINE 14 POSITION 10.
           DISPLAY "Importe Neto :" LINE 17 POSITION 20.
           DISPLAY "Calculo .....:" LINE 18 POSITION 20.
           DISPLAY "Porcentaje ..:" LINE 19 POSITION 20.
           DISPLAY "M i n i m o .:" LINE 20 POSITION 20.
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
           DELETE IMPU INVALID KEY DISPLAY "NO BAJO".
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
           
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 17 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 18 POSITION 40 SIZE 39.
           DISPLAY SPACES LINE 19 POSITION 40 SIZE 39.
       
       LIMPIE-F.
           EXIT.






