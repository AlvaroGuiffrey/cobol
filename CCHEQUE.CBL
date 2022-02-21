       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CCHEQUE.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "EMPRE.SEL".
           COPY "CHEQUES.SEL".
           COPY "FACT.SEL".
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "EMPREBM.REG".
           COPY "CHEQUES.REG".
           COPY "FACT.REG".
           COPY "IMP-132.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
           03  EMP                  PIC 9(4).
           03  SUC                  PIC 99.
           03  NUMERO               PIC 9(8).
       01  TITULOS.
           03  TIT1.
               05  FILLER           PIC X(12) VALUE "Empresa .:".         
               05  EMP-ED           PIC X(40).
           03  TIT2.               
               05  FILLER           PIC X(15) VALUE 
                   "N§ Factura .:".
               05  NUMERO-ED        PIC ZZZZZZZZ.
               05  FILLER           PIC X(10) VALUE SPACES.
               05  FILLER           PIC X(26) VALUE 
                   "Importe Total Factura .:".
               05  TOT-ED           PIC ZZ,ZZZ,ZZ9.99.  
           03  TIT3.               
               05  FILLER           PIC X(50) VALUE 
                   "Consulta de Cheques x N§ Factura".
           03  TIT4.
               05  FILLER           PIC X(10) VALUE                
                   "N§ Cheque".
               05  FILLER           PIC X(5) VALUE SPACES.
               05  FILLER           PIC X(10) VALUE 
                   "N§ Postal".
               05  FILLER           PIC X(5) VALUE SPACES.
               05  FILLER           PIC X(5) VALUE "Banco".
               05  FILLER           PIC X(5) VALUE SPACES.
               05  FILLER           PIC X(10) VALUE " Importe".
               05  FILLER           PIC X(10) VALUE "Fecha Venc".
       01  RENGLONES.
           03  RENG1.
               05  NUMERO-CH-ED     PIC ZZZZZZZZZZBBBBBBB.
               05  CPOSTAL-ED       PIC ZZZZBBBBBB.
               05  BCO-ED           PIC ZZBBB.
               05  IMP-FACTURA-ED   PIC ZZ,ZZZ,ZZ9.99BBB.
               05  FECHA-VEN-ED.
                   07  DIA-VEN-ED   PIC 99/.
                   07  MES-VEN-ED   PIC 99/.
                   07  ANO-VEN-ED   PIC 99BBBBBB.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "EMPRE.ERR".
           COPY "CHEQUES.ERR".
           COPY "FACT.ERR".
           COPY "IMP-132.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.
           PERFORM ARMA-PANTALLA.
           OPEN INPUT  CHEQUES EMPRESAS FACTURAS
                OUTPUT IMP-132.
       COMIENZO.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE CHEQUES EMPRESAS FACTURAS IMP-132.
       FF1.
           EXIT PROGRAM.
       FF2.
           STOP RUN.
      *...............................................................* 
      *                  CARGA DE PARAMETROS                          * 
      *...............................................................* 
      
       TOMA-PARAMETROS.
       C1.
           ACCEPT EMP LINE 10 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO FF.
       C2.
           ACCEPT SUC LINE 10 POSITION 45 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C1.
           MOVE EMP TO NUM-EMP.
           MOVE SUC TO SUC-EMP.
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 10 POSITION 50 SIZE 29
                ACCEPT QQ
                GO TO C2.
           MOVE RAZ-SOC-EMP TO EMP-ED.
           DISPLAY EMP-ED LINE 10 POSITION 50 SIZE 29.
       C3.
           ACCEPT NUMERO LINE 12 POSITION 30 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
           MOVE NUMERO TO NUMERO-ED.
                      
           MOVE EMP    TO FA-EMP.
           MOVE SUC    TO FA-SUC.
           MOVE 0      TO FA-CUENTA. 
           MOVE NUMERO TO FA-NRO-FACTURA.
           START FACTURAS KEY NOT LESS CLAVE-FACTURAS
                 INVALID KEY DISPLAY "No Facturas"
                 LINE 23 POSITION 15 SIZE 64
                 ACCEPT QQ
                 GO TO C3.
       LEE-EN-FACTURAS.           
           READ FACTURAS NEXT RECORD AT END GO TO FF.
           IF FA-NRO-FACTURA < NUMERO GO TO LEE-EN-FACTURAS.
           IF FA-NRO-FACTURA > NUMERO GO TO FF.
           MOVE FA-TOTAL-FAC TO TOT-ED.
           DISPLAY TOT-ED LINE 12 POSITION 40 SIZE 13.

       TOMA-PARAMETROS-F.
   
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
           
       SE-POSICIONA.
           MOVE 0      TO CH-NRO-CHEQUE.
           MOVE NUMERO TO CH-NRO-FACTURA.
           
           START CHEQUES KEY NOT LESS CH-CLAVE-FACTURA INVALID KEY
                 DISPLAY "No Factura" LINE 23 POSITION 15 SIZE 64
                 ACCEPT QQ
                 GO TO C3.

       LEA.
           READ CHEQUES NEXT RECORD AT END GO TO PROCESO-F.
       SALTE.    
           MOVE 0 TO VALIDO.
           PERFORM VALIDA THRU VALIDA-F.
           IF VALIDO NOT  = 0 GO TO LEA.
           PERFORM ARMA-SUME THRU ARMA-SUME-F
           MOVE RENG1 TO LINEA-PANTALLA.
           PERFORM AVANCE-P THRU AVANCE-P-F
           IF CORTA = 9 
              GO TO PROCESO-F.
           GO TO LEA.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           IF CH-EMP NOT = EMP MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           IF CH-SUC NOT = SUC MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           IF CH-NRO-FACTURA < NUMERO OR CH-NRO-FACTURA > NUMERO
                             MOVE 9 TO VALIDO
                             GO TO VALIDA-F.
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE CH-NUMERO-CH      TO NUMERO-CH-ED.
           MOVE CH-CPOSTAL        TO CPOSTAL-ED.
           MOVE CH-BCO            TO BCO-ED.
           MOVE CH-IMP-FACTURA    TO IMP-FACTURA-ED.
           MOVE CH-DIA-VENC       TO DIA-VEN-ED.
           MOVE CH-MES-VENC       TO MES-VEN-ED.
           MOVE CH-ANO-VENC       TO ANO-VEN-ED.
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
           DISPLAY TIT4 LINE 5 POSITION 1.
           
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
           DISPLAY "Consulta de Cheques" LINE 4 POSITION 1.
           DISPLAY "-------- -- -------" LINE 5 POSITION 1.
           DISPLAY "Empresa .....:" LINE 10 POSITION 4.
           DISPLAY "N§ Factura ..:" LINE 12 POSITION 4.
      
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
           DISPLAY "              No Hay Mas" 
                   LINE 23 POSITION 15 SIZE 64.
           ACCEPT QQ.
       PROCESO-FINAL-F.
           EXIT.



