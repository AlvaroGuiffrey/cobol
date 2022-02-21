       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CCARTERA.
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
           03  NUMERO               PIC 9(10).
           03  TOT                  PIC 9(8)V99.
       01  TITULOS.
           03  TIT0.
               05  FILLER           PIC X(50) VALUE
               "Consulta Cheques en Cartera.".
           03  TIT1.
               05  FILLER           PIC X(12) VALUE "Empresa .:".         
               05  EMP-ED           PIC X(30).
               05  FILLER           PIC X(5) VALUE SPACES.
           03  TIT2.               
               05  FILLER           PIC X(10) VALUE "N§ Cheque".
               05  FLLER            PIC X(2) VALUE SPACES.
               05  FILLER           PIC X(5) VALUE "Bco.".
               05  FILLER           PIC X(2) VALUE SPACES. 
               05  FILLER           PIC X(9) VALUE "N§ Postal".
               05  FILLER           PIC X(2) VALUE SPACES.
               05  FILLER           PIC X(10) VALUE "N§ Factura".
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(6) VALUE "N§ CTA".
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(12) VALUE 
                   "Imp.Cheques".
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(16) VALUE 
                   "Fecha Venc.".
       01  RENGLONES.       
           03  RENG1.
               05  NUMERO-CH-ED     PIC ZZZZZZZZZZBBB.
               05  FILLER           PIC X VALUE SPACES.
               05  BCO-ED           PIC ZZZBBBBBB.
               05  CPOSTAL-ED       PIC ZZZZBB.
               05  NRO-FACTURA-ED   PIC ZZZZZZZZBBBB.
               05  NRO-CTA-ED       PIC ZZZZZZBBBB.
               05  IMP-FACTURA-ED   PIC ZZZ,ZZ9.99BBBB.
               05  FECHA-VENC-ED. 
                   07  DIA-VEN-ED   PIC 99/.
                   07  MES-VEN-ED   PIC 99/.
                   07  ANO-VEN-ED   PIC 9999.
           03  RENGTOT.
               05  FILLER           PIC X(51) VALUE 
                  "***********   Total  :".
               05  TOT-ED           PIC ZZZ,ZZ9.99.
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
           ACCEPT DF LINE 12 POSITION 30 NO BEEP TAB  
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
       C4.
           ACCEPT HF LINE 14 POSITION 30 NO BEEP 
                  TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3.
           
           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
       
       TOMA-PARAMETROS-F.
   
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
           
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
           GO TO PROCESO.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           IF CH-EMP NOT = EMP MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           IF CH-SUC NOT = SUC MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           COMPUTE ESTA = (CH-ANO-VENC * 372) + (CH-MES-VENC * 31) + 
                          CH-DIA-VENC.

           IF ESTA < DESDE OR ESTA > HASTA MOVE 9 TO VALIDO
                                           GO TO VALIDA-F.
           IF CH-ANO-ENT NOT = 0
              AND CH-MES-ENT NOT = 0 
                  AND CH-DIA-ENT NOT = 0
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
           MOVE CH-NRO-CTA        TO NRO-CTA-ED.
           MOVE CH-DIA-VENC       TO DIA-VEN-ED.
           MOVE CH-MES-VENC       TO MES-VEN-ED.
           MOVE CH-ANO-VENC       TO ANO-VEN-ED.
           MOVE CH-IMP-FACTURA    TO IMP-FACTURA-ED.
           MOVE CH-NRO-FACTURA    TO NRO-FACTURA-ED.
           COMPUTE TOT = TOT + CH-IMP-FACTURA.

       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY TIT0 LINE 1 POSITION 1.
           DISPLAY TIT1 LINE 2 POSITION 1.
           DISPLAY TIT2 LINE 3 POSITION 1.
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
           DISPLAY "Consulta de Cheques en Cartera" LINE 4 POSITION 1.
           DISPLAY "-------- -- ------- -- -------" LINE 5 POSITION 1.
           DISPLAY "Empresa .....:" LINE 10 POSITION 4.
           DISPLAY "Desde Fecha .:" LINE 12 POSITION 4.
           DISPLAY "Hasta Fecha .:" LINE 14 POSITION 4.
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
           DISPLAY SPACES 
           MOVE TOT TO TOT-ED.
           DISPLAY RENGTOT.
           
           DISPLAY "          Enter para Continuar" 
                   LINE 23 POSITION 15 SIZE 64.
           ACCEPT QQ.
       PROCESO-FINAL-F.
           EXIT.



