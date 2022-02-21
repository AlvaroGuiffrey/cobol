       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LENT-A.
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
           03  EMP                  PIC 9(4) VALUE 0.
           03  SUC                  PIC 99 VALUE 0.
           03  NUMERO               PIC 9(10) VALUE 0.
       01  TITULOS.
           03  TIT0.
               05  FILLER           PIC X(50) VALUE
               "Listado Cheques Entregados.".
           03  TIT1.
               05  FILLER           PIC X(12) VALUE "Empresa .:".         
               05  EMP-ED           PIC X(30).
               05  FILLER           PIC X(5) VALUE SPACES.
           03  TIT2.               
               05  FILLER           PIC X(8) VALUE "Nro. CH.".
               05  FILLER           PIC X(4) VALUE SPACES.
               05  FILLER           PIC X(5) VALUE "Bco.".
               05  FILLER           PIC X(2) VALUE SPACES. 
               05  FILLER           PIC X(9) VALUE "N§ Postal".
               05  FILLER           PIC X(2) VALUE SPACES.
               05  FILLER           PIC X(20) VALUE 
                   "     Entregado a.".
               05  FILLER           PIC X(2) VALUE SPACES.
               05  FILLER           PIC X(13) VALUE 
                   "Fecha Entrega".
               05  FILLER           PIC X(2) VALUE SPACES.
               05  FILLER           PIC X(20) VALUE 
                   "Concepto de Entrega".
       01  RENGLONES.       
           03  RENG1.
               05  NRO-CH-ED        PIC ZZZZZZZZZZ.
               05  FILLER           PIC X VALUE SPACES.
               05  BCO-ED           PIC ZZZBBBBBB.
               05  CPOSTAL-ED       PIC ZZZZBBBBBBB.
               05  ENTREGADO-A-ED   PIC X(20).
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FECHA-ENTREGA-ED.
                   07  DIA-ENT-ED    PIC 99/.
                   07  MES-ENT-ED    PIC 99/.
                   07  ANO-ENT-ED    PIC 9999BBBB.
               05  CONCEPTO-ENT-ED   PIC X(20).  
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
       C4.
           ACCEPT DF LINE 14 POSITION 40 NO BEEP TAB  
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
       C6.
           ACCEPT HF LINE 16 POSITION 40 NO BEEP 
                  TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.
           
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
           PERFORM AVANCE-L THRU AVANCE-L-F
           PERFORM IMPRIMA THRU IMPRIMA-F.
           GO TO PROCESO.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           IF CH-EMP NOT = EMP MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           IF CH-SUC NOT = SUC MOVE 9 TO VALIDO
                               GO TO VALIDA-F.
           
           
           IF CH-ANO-ENT = 0 MOVE 9 TO VALIDO
                             GO TO VALIDA-F.
       
           COMPUTE ESTA = (CH-ANO-VENC * 372) + (CH-MES-VENC * 31) + 
                          CH-DIA-VENC.
           IF ESTA < DESDE OR ESTA > HASTA MOVE 9 TO VALIDO
                                           GO TO VALIDA-F.
           
       
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE CH-NUMERO-CH      TO NRO-CH-ED.
           MOVE CH-CPOSTAL        TO CPOSTAL-ED.
           MOVE CH-BCO            TO BCO-ED.
           MOVE CH-DIA-ENT        TO DIA-ENT-ED.
           MOVE CH-MES-ENT        TO MES-ENT-ED.
           MOVE CH-ANO-ENT        TO ANO-ENT-ED.
           MOVE CH-ENTREGADO-A    TO ENTREGADO-A-ED.
           MOVE CH-CONCEPTO-ENT   TO CONCEPTO-ENT-ED.
       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           WRITE LINEA FROM TIT0 AFTER 1.
           WRITE LINEA FROM TIT1 AFTER 1.
           WRITE LINEA FROM TIT2 AFTER 1.
           MOVE SPACES TO LINEA.
           WRITE LINEA AFTER 1.
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
           DISPLAY "Listado de Cheques Entregados" LINE 4 POSITION 1.
           DISPLAY "------- -- ------- ----------" LINE 5 POSITION 1.
           DISPLAY "Empresa .....:" LINE 10 POSITION 4.
           DISPLAY "Desde Fecha ........:" LINE 14 POSITION 4.
           DISPLAY "Hasta Fecha ........:" LINE 16 POSITION 4.
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
       PROCESO-FINAL-F.
           EXIT.



