       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LRENTAS.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "RES127.SEL".
           COPY "CUENTA.SEL".
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "RES127.REG".
           COPY "CUENTABM.REG".
           COPY "IMP-132.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       
       01  VARIOS.
           03  COMP1            PIC 9(8) VALUE 0.
           03  IMP1             PIC 9(6)V99 VALUE 0.
           03  FEC1.
               05  DIA1         PIC 99 VALUE 0.
               05  MES1         PIC 99 VALUE 0.
               05  ANO1         PIC 99 VALUE 0.
           03  BANCO1           PIC X(20).
           03  MULTA1           PIC 9(6)V99 VALUE 0.       
           03  INTER1           PIC 9(3)V99 VALUE 0.
           03  COMP2            PIC 9(8) VALUE 0.
           03  IMP2             PIC 9(6)V99 VALUE 0.
           03  FEC2.
               05  DIA2         PIC 99 VALUE 0.
               05  MES2         PIC 99 VALUE 0.
               05  ANO2         PIC 99 VALUE 0.
           03  BANCO2           PIC X(20).
           03  MULTA2           PIC 9(6)V99 VALUE 0.       
           03  INTER2           PIC 9(3)V99 VALUE 0.
       
           03  FEC-ED.
               05  DIAX-ED       PIC ZZ/.
               05  MESX-ED       PIC ZZ/.
               05  ANOX-ED       PIC ZZ.
           03  IMPX-ED           PIC ZZZ,ZZ9.99.
       01  TITULOS.
           03  TIT1.
               05  FILLER               PIC X(5) VALUE SPACES.
               05  FILLER               PIC X(40) VALUE 
                   "IMPUESTO SOBRE  LOS INGRESOS BRUTOS ".
               05  FILLER               PIC X(40) VALUE 
                   "AGENTE DE PERCEPCION".
           03  TIT1-1.
               05  FILLER               PIC X(5) VALUE SPACES.
               05  FILLER               PIC X(50) VALUE 
                   "DECLARACION JUR ADA - Reemplaza form. DGR 87".

           03  TIT1-2.
               05  FILLER               PIC X(5) VALUE SPACES.
               05  FILLER               PIC X(55) VALUE 
               "SOTELO MIGUEL A. Y JORGE A.".
               05  FILLER               PIC X(30) VALUE 
               "Ing.Brutos : 0-40-220-0009/0".
               05  FILLER               PIC X(12) VALUE 
               "MAYORISTA".
           03  TIT1-3.
               05  FILLER               PIC X(5) VALUE SPACES.
               05  FILLER               PIC X(30) VALUE  
               "Calle : E.Francou 1131".
               05  FILLER               PIC X(45) VALUE 
               "Cod.Postal :3265 - Localidad : Villa Elisa".
               05  FILLER               PIC X(12) VALUE
               "Entre Rios".

           03  TIT1-4.
               05  filler               pic x(5).               
               05  FILLER               PIC X(8) VALUE "Comp.:".
               05  COMP1-ED             PIC ZZZZZZZZBB.
               05  FILLER               PIC X(11) VALUE "Importe :".
               05  IMP1-ED              PIC ZZZ,ZZ9.99BB.
               05  FILLER               PIC X(8) VALUE "Fecha:".                             
               05  FECHA1-ED.
                   07 DIA1-ED           PIC ZZ/.
                   07 MES1-ED           PIC ZZ/.
                   07 ANO1-ED           PIC ZZBB.
               05  FILLER               PIC X(8) VALUE "Banco:".
               05  BANCO1-ED            PIC X(22).
               05  FILLER               PIC X(7) VALUE "Multa:".
               05  MULTA1-ED            PIC ZZZ,ZZ9.99BB.
               05  FILLER               PIC X(11) VALUE "Interes :".
               05  INTER1-ED            PIC Z,ZZ9.99.
           03  TIT1-5.
               05  filler               pic x(5).               
               05  FILLER               PIC X(8) VALUE "Comp.:".
               05  COMP2-ED             PIC ZZZZZZZZBB.
               05  FILLER               PIC X(11) VALUE "Importe :".
               05  IMP2-ED              PIC ZZZ,ZZ9.99BB.
               05  FILLER               PIC X(8) VALUE "Fecha:".                             
               05  FECHA2-ED.
                   07 DIA2-ED           PIC ZZ/.
                   07 MES2-ED           PIC ZZ/.
                   07 ANO2-ED           PIC ZZBB.
               05  FILLER               PIC X(8) VALUE "Banco:".
               05  BANCO2-ED            PIC X(22).
               05  FILLER               PIC X(7) VALUE "Multa:".
               05  MULTA2-ED            PIC ZZZ,ZZ9.99BB.
               05  FILLER               PIC X(11) VALUE "Interes :".
               05  INTER2-ED            PIC Z,ZZ9.99.

           
           03  TIT2.                   
               05  FILLER            PIC X(8) VALUE "Dia-Mes".
               05  FILLER            PIC X(32) VALUE 
                  "N   o   m   b   r  e  ".
               05  FILLER            PIC X(22) VALUE 
                  "D o m i c i l i o ".
               05  FILLER            PIC X(6) VALUE "CP.".                

               05  FILLER            PIC X(17) VALUE    
                  "Insc.I.Brutos ".
               05  FILLER            PIC X(7) VALUE "T.Doc.".
               05  FILLER            PIC X(14) VALUE " Imp.S.Perc. ".
               05  FILLER            PIC X(6)  VALUE "Alic.".
               05  FILLER            PIC X(15) VALUE "Impuesto Perc.".
       01  RENGLONES.
           03  RENG1.
               05  FILLER            PIC X(1) VALUE SPACES.
               05  FECHA-ED.
                   07  DIA-ED        PIC 99/.
                   07  MES-ED        PIC 99BBB.
               05  NOMBRE-ED         PIC X(30).
               05  FILLER            PIC X(2) VALUE SPACES.
               05  DOMI-ED           PIC X(20) .
               05  FILLER            PIC X(2) VALUE SPACES.
               05  CP-ED             PIC ZZZZ.
               05  FILLER            PIC XX VALUE SPACES.
               05  IDENT-ED          PIC X(15)B(2).
               05  DOC-ED            PIC X(5)B(5).
               05  MONTO-ED          PIC ZZZ,ZZ9.99BB.
               05  PORC-ED           PIC Z9.99BB.
               05  PERCEP-ED         PIC ZZZ,ZZ9.99BB.
           03  RENG2.
               05  FILLER            PIC X(70) VALUE SPACES.

               05  QUEDICE           PIC X(22) VALUE
                  "*******  Totales :".
               05  TREG-ED           PIC ZZZZZZBBB.
               05  TMON-ED           PIC ZZZ,ZZ9.99BB.
               05  TPER-ED           PIC ZZZ,ZZ9.99.
       01  CALCULOS.
           03  TOT-MON               PIC 9(6)V99.
           03  TOT-PER               PIC 9(6)V99.
           03  TOT-REGISTROS         PIC 9(6) VALUE 0.
           03  TOT-RETENIDO          PIC 9(12)V99.
       PROCEDURE DIVISION.

       ABRO-ARCHIVOS.
           PERFORM ARMA-PANTALLA.
           OPEN INPUT RES127 CUENTAS
                OUTPUT IMP-132.
       COMIENZO.
           READ RES127 NEXT AT END STOP RUN.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE RES127 IMP-132.
       FF1.
           EXIT PROGRAM.
       FF2.
           STOP RUN.
      *...............................................................* 
      *                  CARGA DE PARAMETROS                          * 
      *...............................................................* 
      
       TOMA-PARAMETROS.
       C1.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 8.
           ACCEPT DF LINE 10 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO FF.
           MOVE D-D TO DIAX-ED.
           MOVE D-M TO MESX-ED.
           MOVE D-A TO ANOX-ED.
           DISPLAY FEC-ED LINE 10 POSITION 40.
       C2.
           DISPLAY SPACES LINE 11 POSITION 40 SIZE 8.
           ACCEPT HF LINE 11 POSITION 40 NO BEEP TAB 
                  CONVERT ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C1.
           MOVE H-D TO DIAX-ED.
           MOVE H-M TO MESX-ED.
           MOVE H-A TO ANOX-ED.
           DISPLAY FEC-ED LINE 11 POSITION 40.
           
           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
       C3.       
           DISPLAY SPACES LINE 16 POSITION 17 SIZE 8.
           ACCEPT COMP1 LINE 16 POSITION 17  NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
           MOVE COMP1 TO COMP1-ED.
       C4.
           DISPLAY SPACES LINE 17 POSITION 17 SIZE 13.
           ACCEPT IMP1 LINE 17 POSITION 17 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3.
           MOVE IMP1 TO IMP1-ED.
           DISPLAY IMP1-ED LINE 17 POSITION 17.
       C5.
           DISPLAY SPACES LINE 18 POSITION 17 SIZE 8.
           ACCEPT FEC1 LINE 18 POSITION 17 NO BEEP TAB ON EXCEPTION 
                  CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.
           MOVE DIA1 TO DIA1-ED.
           MOVE MES1 TO MES1-ED.
           MOVE ANO1 TO ANO1-ED.
           DISPLAY FECHA1-ED LINE 18 POSITION 17.

       C6.
           DISPLAY SPACES LINE 19 POSITION 17 SIZE 20.
           ACCEPT BANCO1 LINE 19 POSITION 17 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C5.
           MOVE BANCO1 TO BANCO1-ED.
       C7.
           DISPLAY SPACES LINE 20 POSITION 17 SIZE 13.
           ACCEPT MULTA1 LINE 20 POSITION 17 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C6.
           MOVE MULTA1 TO MULTA1-ED.
           DISPLAY MULTA1-ED LINE 20 POSITION 17.
       
       C8.
           DISPLAY SPACES LINE 21 POSITION 17 SIZE 10.
           ACCEPT INTER1 LINE 21 POSITION 17 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C7.
           MOVE INTER1 TO INTER1-ED. 
           DISPLAY INTER1-ED LINE 21 POSITION 17.

       C3X.       
           DISPLAY SPACES LINE 16 POSITION 55 SIZE 8.
           ACCEPT COMP2 LINE 16 POSITION 55  NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C8.
           MOVE COMP2 TO COMP2-ED.
       C4X.
           DISPLAY SPACES LINE 17 POSITION 55 SIZE 13.
           ACCEPT IMP2 LINE 17 POSITION 55 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3X.
           MOVE IMP2 TO IMP2-ED.
           DISPLAY IMP2-ED LINE 17 POSITION 55.
       
       C5X.
           DISPLAY SPACES LINE 18 POSITION 55 SIZE 8.
           ACCEPT FEC2 LINE 18 POSITION 55 NO BEEP TAB ON EXCEPTION 
                  CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4X.
           MOVE DIA2 TO DIA2-ED.
           MOVE MES2 TO MES2-ED.
           MOVE ANO2 TO ANO2-ED.
           DISPLAY FECHA2-ED LINE 18 POSITION 55.
       
       C6X.
           DISPLAY SPACES LINE 19 POSITION 55 SIZE 20.
           ACCEPT BANCO2 LINE 19 POSITION 55 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C5X.
           MOVE BANCO2 TO BANCO2-ED.
       C7X.
           DISPLAY SPACES LINE 20 POSITION 55 SIZE 13.
           ACCEPT MULTA2 LINE 20 POSITION 55 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C6X.
           MOVE MULTA2 TO MULTA2-ED.
           DISPLAY MULTA2-ED LINE 20 POSITION 55.
       
       C8X.
           DISPLAY SPACES LINE 21 POSITION 55 SIZE 10.
           ACCEPT INTER2 LINE 21 POSITION 55 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C7X.
           MOVE INTER2 TO INTER2-ED. 
           DISPLAY INTER2-ED LINE 21 POSITION 55.


       TOMA-PARAMETROS-F.
   
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
           
       LEA.
           
           READ RES127 NEXT RECORD AT END GO TO PROCESO-F.
       SALTE.    
           MOVE 0 TO VALIDO.
           PERFORM VALIDA THRU VALIDA-F.
           IF VALIDO NOT  = 0 GO TO LEA.
           PERFORM ARMA-SUME THRU ARMA-SUME-F
      ***     PERFORM AVANCE-L THRU AVANCE-L-F
      ***     PERFORM IMPRIMA THRU IMPRIMA-F.
           GO TO LEA.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           
           COMPUTE ESTA = (ANO-RES * 372) + (MES-RES * 31) + DIA-RES.
           IF ESTA < DESDE OR ESTA > HASTA MOVE 9 TO VALIDO
                                           GO TO VALIDA-F.
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE I-BRU-RES        TO IDENT-ED.
           MOVE DESC-RES         TO NOMBRE-ED.
           MOVE " "              TO DOMI-ED.
           MOVE 0                TO CP-ED.
           MOVE MES-RES          TO MES-ED.
           MOVE DIA-RES          TO DIA-ED.
           MOVE IMP-RES          TO MONTO-ED.
           MOVE PER-RES          TO PERCEP-ED.
           MOVE POR-RES          TO PORC-ED.
                  
           COMPUTE TOT-MON ROUNDED = TOT-MON + IMP-RES.
           COMPUTE TOT-PER ROUNDED = TOT-PER + PER-RES.
           ADD 1 TO TOT-REGISTROS

           MOVE EMP-RES          TO EMP-CTA
           MOVE SUC-RES          TO SUC-CTA
           MOVE RUBRO-RES        TO RUBRO-CTA
           MOVE CUE-RES          TO CUENTA
           MOVE SUC-C-RES        TO SUB-CUENTA
           READ CUENTAS INVALID KEY GO TO ARMA-SUME-F.
           MOVE COD-LOCA         TO CP-ED
           MOVE DIRE             TO DOMI-ED.


       ARMA-SUME-F.
           EXIT.    
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           WRITE LINEA FROM TIT1 AFTER 1.           
           WRITE LINEA FROM TIT1-1 AFTER 1.           
           WRITE LINEA FROM TIT1-2 AFTER 1.           
           WRITE LINEA FROM TIT1-3 AFTER 1.           
           WRITE LINEA FROM TIT1-4 AFTER 1.           
           WRITE LINEA FROM TIT1-5 AFTER 1.           

           MOVE ALL "-" TO LINEA.
           WRITE LINEA AFTER 2.
           WRITE LINEA FROM TIT2 AFTER 1.
           MOVE ALL "-" TO LINEA.
           WRITE LINEA  AFTER 1.
           MOVE SPACES TO LINEA.
           WRITE LINEA AFTER 1.
           MOVE 11 TO CONT-RENG.

       TITULOS-F.
           EXIT.

       IMPRIMA.
           WRITE LINEA FROM RENG1 AFTER 1.
            
       IMPRIMA-F.
           EXIT.

      *...........................................................* 
      *              POSICIONAMIENTO DE CLAVES SEGUN OPCION       *
      *...........................................................*
      *     COPY "ORDENA.RUT".

       PROCESO-F.
           EXIT.

       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Listado de Rentas" 
                    LINE 4 POSITION 2.
           DISPLAY "------- -- ------" 
                    LINE 5 POSITION 2.
           
           DISPLAY "-------- Periodo a Informar ---------"
               LINE 8 POSITION 5 REVERSE.
           DISPLAY "Desde Fecha :" LINE 10 POSITION 10.      
           DISPLAY "Hasta Fecha :" LINE 11 POSITION 10.
      
           DISPLAY "------- Depositos Efectuados --------"
               LINE 13 POSITION 5 REVERSE.
           DISPLAY "Deposito Nro. 1 " LINE 14 POSITION 2.      
           DISPLAY "Deposito Nro. 2 " LINE 14 POSITION 40.
           DISPLAY "Comprobante  :" LINE 16 POSITION 2.
           DISPLAY "Importe Dep. :" LINE 17 POSITION 2.
           DISPLAY "Fecha Dep.   :" LINE 18 POSITION 2.
           DISPLAY "Banco Recaud.:" LINE 19 POSITION 2.
           DISPLAY "Multa        :" LINE 20 POSITION 2.
           DISPLAY "Intereses    :" LINE 21 POSITION 2.
           DISPLAY "Comprobante  :" LINE 16 POSITION 40.
           DISPLAY "Importe Dep. :" LINE 17 POSITION 40.
           DISPLAY "Fecha Dep.   :" LINE 18 POSITION 40.
           DISPLAY "Banco Recaud.:" LINE 19 POSITION 40.
           DISPLAY "Multa        :" LINE 20 POSITION 40.
           DISPLAY "Intereses    :" LINE 21 POSITION 40.
      
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*
       PROCESO-FINAL.           
           MOVE TOT-MON TO TMON-ED.
           MOVE TOT-PER TO TPER-ED. 
           MOVE TOT-REGISTROS TO TREG-ED
           MOVE ALL "-" TO LINEA.
           WRITE LINEA AFTER 2.
           MOVE "*******  Totales :" TO QUEDICE
           WRITE LINEA FROM RENG2 AFTER 1.
           MOVE ALL "-" TO LINEA.
           WRITE LINEA AFTER 2.
       PROCESO-FINAL-F.
            EXIT.



