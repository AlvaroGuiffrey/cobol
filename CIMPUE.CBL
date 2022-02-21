       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CIMPUE.
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
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "EMPREBM.REG".
           COPY "IMPU.REG".
           COPY "ACTI.REG".
           COPY "IMP-132.REG".
           
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
           03  RR                          PIC X(80) VALUE ALL "Ä".
           03  DATOS-PARAMETROS.
               05  ACT                     PIC 9(8).
               05  EMP                     PIC 9(4).
               05  SUC                     PIC 99.
               05  IMP                     PIC 9.
       01  TITULOS.
           03  TIT1.
               05  FILLER                 PIC X(50) VALUE 
                "Sistema Integral Administrativo Contable".
           03  TIT2.
               05  FILLER                 PIC X(10) VALUE 
                           "Empresa.:".
               05  EMP-ED                 PIC X(40).
           03  TIT2-0.               
               05  FILLER                 PIC X(30) VALUE 
                "Consulta de Impuesto".
               05  FILLER                 PIC X(9) VALUE "Desde .:".
               05  DESFEC.
                   07  DD-ED              PIC 99/.
                   07  DM-ED              PIC 99/.
                   07  DA-ED              PIC 99.
               05  FILLER                 PIC X(3) VALUE " a".
               05  HASFEC.
                   07  HD-ED              PIC 99/.
                   07  HM-ED              PIC 99/.
                   07  HA-ED              PIC 99.
           03  TIT3.               
               05  FILLER                 PIC X(13) VALUE "Impuesto :".
               05  IMPUE-ED               PIC 9.
               05  FILLER                 PIC X(5) VALUE SPACES.
               05  FILLER                 PIC X(13) VALUE "Actividad :".
               05  ACTIV-ED               PIC X(25).
           03  TIT4.
               05  FILLER                 PIC X(8) VALUE " Fecha ".
               05  FILLER                 PIC X(4) VALUE SPACES.
               05  FILLER                 PIC X(11) VALUE "Comprobante".
               05  FILLER                 PIC X(3) VALUE SPACES.
               05  FILLER                 PIC X(10) VALUE " N e t o ".
               05  FILLER                 PIC X(3) VALUE SPACES. 
               05  FILLER                 PIC X(10) VALUE "Calculo".
               05  FILLER                 PIC X(18) VALUE "Porcentaje".
               05  FILLER                 PIC X(20) 
                          VALUE " M i n i m o ".
       
       01  RENGLONES.
           03  RENG1.
               05  FECHA-ED.
                   07  DIA-ED             PIC 99/.
                   07  MES-ED             PIC 99/.
                   07  ANO-ED             PIC 99.
               05  FILLER                     PIC X(6) VALUE SPACES.
               05  COMPRO-ED                  PIC ZZZZZZ.
               05  FILLER                     PIC X(6) VALUE SPACES.           
               05  NET-ED                     PIC ---,--9.99.
               05  FILLER                     PIC X(8) VALUE SPACES.
               05  CAL-ED                     PIC Z.
               05  FILLER                     PIC X(8) VALUE SPACES.
               05  POR-ED                     PIC Z9.999.
               05  FILLER                     PIC X(8) VALUE SPACES.
               05  MIN-ED                     PIC ZZZ,ZZ9.99.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
           
           COPY "EMPRE.ERR".
           COPY "IMPU.ERR".
           COPY "ACTI.ERR".
           COPY "IMP-132.ERR".
       
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.

           OPEN INPUT IMPU EMPRESAS ACTI
                OUTPUT IMP-132.
       COMIENZO.
           PERFORM ARMA-PANTALLA.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE IMPU ACTI EMPRESAS IMP-132.
       FF1.
           EXIT PROGRAM.
       FF2.
           STOP RUN.
      *...............................................................* 
      *                  CARGA DE PARAMETROS                          * 
      *...............................................................* 
      
       TOMA-PARAMETROS.

       C1.    
           ACCEPT EMP LINE 10 POSITION 30 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO FF.
       C2.
           ACCEPT SUC LINE 10 POSITION 35 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C1.
           MOVE EMP TO NUM-EMP
           MOVE SUC TO SUC-EMP
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 10 POSITION 40 SIZE 30
                GO TO C2.
           DISPLAY RAZ-SOC-EMP LINE 10 POSITION 40 SIZE 30.
           MOVE RAZ-SOC-EMP TO EMP-ED.     
       C3.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 1.
           ACCEPT IMP LINE 12 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
           MOVE IMP TO IMPUE-ED.           
           
       C4.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 3.
           ACCEPT ACT LINE 14 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3.
           MOVE IMP TO IMP-ACT
           MOVE ACT TO ACT-ACT.
           READ ACTI INVALID KEY 
            DISPLAY "No hay actividad creada para este impuesto"
             LINE 24 POSITION 15 SIZE 64
              ACCEPT QQ
               GO TO C3.
           MOVE DET-ACT TO ACTIV-ED.
           DISPLAY DET-ACT LINE 14 POSITION 45 SIZE 30.
       C5.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           ACCEPT DF LINE 16 POSITION 40 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.
       C6.
           ACCEPT HF LINE 18 POSITION 40 NO BEEP TAB ON EXCEPTION
                 CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.       
           
           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
           MOVE D-D  TO DD-ED
           MOVE D-M  TO DM-ED
           MOVE D-A  TO DA-ED
           MOVE H-D  TO HD-ED
           MOVE H-M  TO HM-ED 
           MOVE H-A  TO HA-ED.
       
       TOMA-PARAMETROS-F.
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
       
       LEA.
           READ IMPU NEXT RECORD AT END GO TO PROCESO-F.

       SALTE.    
           MOVE 0 TO VALIDO.
           PERFORM VALIDA THRU VALIDA-F.
           IF VALIDO NOT  = 0 GO TO LEA.
           PERFORM ARMA-SUME THRU ARMA-SUME-F
           MOVE RENG1 TO LINEA-PANTALLA.
           PERFORM AVANCE-P THRU AVANCE-P-F
      *     PERFORM IMPRIMA THRU IMPRIMA-F.
           IF CORTA = 9 GO TO PROCESO-F.
           GO TO LEA.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           IF EMP-IMP NOT = EMP     MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.

           IF SUC-IMP NOT = SUC     MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.
           
           IF IMP-IMP NOT = IMP  MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.
           
           IF ACT-IMP NOT = ACT  MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.
        
           COMPUTE ESTA = (AA-IMP * 372) + (MM-IMP * 31) + DD-IMP.
           IF ESTA < DESDE OR ESTA > HASTA  MOVE 9 TO VALIDO
                                            GO TO VALIDA-F.      
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE DD-IMP     TO DIA-ED.
           MOVE MM-IMP     TO MES-ED.
           MOVE AA-IMP     TO ANO-ED.
           MOVE COMPRO-IMP TO COMPRO-ED.
           MOVE NET-IMP    TO NET-ED.
           MOVE CAL-IMP    TO CAL-ED.
           MOVE POR-IMP    TO POR-ED.
           MOVE MIN-IMP    TO MIN-ED.
       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY TIT1 LINE 1 POSITION 1.
           DISPLAY TIT2 LINE 3 POSITION 1.
           DISPLAY TIT3 LINE 4 POSITION 1.
           DISPLAY TIT4 LINE 6 POSITION 1.
           DISPLAY RR LINE 7 POSITION 1.
           MOVE 6 TO CONT-RENG.


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
           DISPLAY "Sistema Integral Aministrativo Contable"
                     LINE 2 POSITION 3.
           DISPLAY "Consulta de Impuesto" LINE 5 POSITION 30.
           DISPLAY "Empresa .:" LINE 10 POSITION 15.      
           DISPLAY "Impuesto  ..............:" LINE 12 POSITION 10.
           DISPLAY "Actividad ..............:" LINE 14 POSITION 10.
           DISPLAY "Desde Fecha ............:" LINE 16 POSITION 10.
           DISPLAY "Hasta Fecha ............:" LINE 18 POSITION 10.
      
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
           DISPLAY "              Llegue al final"
                 LINE 24 POSITION 15.
           ACCEPT QQ LINE 24 POSITION 15.
       PROCESO-FINAL-F.
           EXIT.
