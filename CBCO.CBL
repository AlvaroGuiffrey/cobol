       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CBCO.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "BCO.SEL".
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "BCO.REG".
           COPY "IMP-132.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
           03  RR                        PIC X(80) VALUE ALL "-".
           03  DB                        PIC 9(4).
           03  HB                        PIC 9(4).
       01  TITULOS.
           03  TIT0.
               05  FILLER                PIC X(30) VALUE 
                   "Sistema de Cheques".           
           03  TIT1.    
               05  FILLER                PIC X(30)  VALUE 
                   "Consulta de Bancos".
           03  TIT2.
               05  FILLER                PIC X(10) VALUE SPACES.
               05  FILLER                PIC X(50) VALUE 
                   "Codigo                        Nombre Bco.".
       01  RENGLONES.   
           02  RENG1.
             03  FILLER                    PIC X(12) VALUE SPACES.
             03  NUMERO-ED                 PIC ZZZZ.       
             03  FILLER                    PIC X(22) VALUE SPACES.
             03  NOMBRE-ED                 PIC X(30).
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "BCO.ERR".
           COPY "IMP-132.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.
           PERFORM ARMA-PANTALLA.
           OPEN INPUT  BCO
                OUTPUT IMP-132.
       COMIENZO.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE BCO IMP-132.
       FF1.
           EXIT PROGRAM.
       FF2.
           STOP RUN.
      *...............................................................* 
      *                  CARGA DE PARAMETROS                          * 
      *...............................................................* 
      
       TOMA-PARAMETROS.
       C1.
           ACCEPT DB LINE 10 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO FF.
       C3.         
           ACCEPT HB LINE 12 POSITION 40 NO BEEP TAB  
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C1.
       TOMA-PARAMETROS-F.
   
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
           
       LEA.
           
           READ BCO NEXT RECORD AT END GO TO PROCESO-F.
       SALTE.    
           MOVE 0 TO VALIDO.
           PERFORM VALIDA THRU VALIDA-F.
           IF VALIDO NOT  = 0 GO TO LEA.
           PERFORM ARMA-SUME THRU ARMA-SUME-F
           MOVE RENG1 TO LINEA-PANTALLA.
           PERFORM AVANCE-P THRU AVANCE-P-F
           IF CORTA = 9 
              GO TO PROCESO-F.
           GO TO PROCESO.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           
           IF BCO-NUMERO < DB OR BCO-NUMERO > HB MOVE 9 TO VALIDO
                                                 GO TO VALIDA-F.
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE BCO-NUMERO        TO NUMERO-ED.
           MOVE BCO-NOMBRE        TO NOMBRE-ED.
       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY TIT0 LINE 1 POSITION 1.
           DISPLAY TIT1 LINE 2 POSITION 1.
           DISPLAY SPACES LINE 3 POSITION 1.
           DISPLAY TIT2 LINE 4 POSITION 1.
           DISPLAY RR LINE 5 POSITION 1.
           
           
           MOVE 5 TO CONT-RENG.

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
           
           DISPLAY "Sistema de Cheques" LINE 2 POSITION 1.
           DISPLAY "------- -- -------" LINE 3 POSITION 1.
           DISPLAY "Consulta de Bancos" LINE 5 POSITION 1.
           DISPLAY "-------- -- ------" LINE 6 POSITION 1.
           DISPLAY "Desde Numero.:" LINE 10 POSITION 20.
           DISPLAY "Hasta Numero.:" LINE 12 POSITION 20.
      
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
           DISPLAY "          Enter para Continuar" 
                   LINE 23 POSITION 15 SIZE 64.
           ACCEPT QQ.
       PROCESO-FINAL-F.
           EXIT.



