      * ..................................................................*
      *   SISTEMA  : CHAMA                                                *
      *   PROGRAMA : LIS-CAJA                                             *
      *   TAREA : LISTADO DE LA CAJA                                      *
      * ..................................................................*

       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LIS-CAJA.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "MANEJO.SEL".
           COPY "EMPRE.SEL".
           COPY "CUENTA.SEL".
           COPY "LACAJA.SEL".
           COPY "TARJ.SEL".
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "MANEJO.REG".
           COPY "LACAJA.REG".
           COPY "CUENTABM.REG".
           COPY "EMPREBM.REG".
           COPY "TARJ.REG".
           COPY "IMP-132.REG".
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
               03  ESTA-CONDI           PIC 99 VALUE 0.
               03  EMP                    PIC 9(4).
               03  SUC                    PIC 99.
               03  TURNO                  PIC 9.
               
               03  FECHA-P.
                   05  ANO-P              PIC 99.
                   05  MES-P              PIC 99.
                   05  DIA-P              PIC 99.
       01  TITULOS.
           02  TIT1.
               03  FILLER                     PIC X(60) VALUE 
                   "       LISTADO DE LA CAJA".
               03  FILLER                 PIC X(20).
               03  FILLER                 PIC X(17) VALUE 
                         "Fecha Listado.:".

               03  FECHA-L.
                   05  DIA-L              PIC 99/.
                   05  MES-L              PIC 99/.
                   05  ANO-L              PIC 99.
                  
           02  TIT2.
               03  FILLER                 PIC X(10) VALUE
                    "Empresa.:".
               03  EMP-ED                 PIC X(25).
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(10) VALUE
                       "Turno..:".
               03  TURNO-ED               PIC 9.
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(14) VALUE
                       "Desde Fecha.:".
               03  DESFEC-ED.
                   05  D-DIA-ED         PIC 99/.
                   05  D-MES-ED         PIC 99/.
                   05  D-ANO-ED         PIC 99.
               03  FILLER               PIC X.
               03  FILLER               PIC X(3) VALUE "al".
               03  HASFEC-ED.
                   05  H-DIA-ED         PIC 99/.
                   05  H-MES-ED         PIC 99/.
                   05  H-ANO-ED         PIC 99.                   
               03  FILLER                 PIC X(5).
           02  TIT3.
               03  FILLER                 PIC X(8) VALUE "  Fecha".
               03  FILLER                 PIC X(3).
               03  FILLER                 PIC X(13) VALUE
                         "Condicion".
               03  FILLER                 PIC X(8) VALUE
                         "Compr.".               
               03  FILLER                 PIC X(7) VALUE "Rubro ".
               03  FILLER                 PIC X(1).
               03  FILLER                 PIC X(10) VALUE "   Cuenta".
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(30) VALUE "    Nombre".
               03  FILLER                 PIC X(2).
               03  FILLER                 PIC X(10) VALUE "  Tipo".
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(10) VALUE " Importe".
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(12) VALUE "Imp. Cheque".
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(15) VALUE
                          "Tarjeta uti.".
           02  TIT4.
               03  FILLER                 PIC X(132) VALUE ALL "-".
       01  RENGLONES.
           02  RENG1.      
               03  LAC-FECHA-ED.
                   05  LAC-DIA-ED         PIC 99/.
                   05  LAC-MES-ED         PIC 99/.
                   05  LAC-ANO-ED         PIC 99.
               03  FILLER                 PIC X(3).
               03  CONDI-ED               PIC X(10).
               03  FILLER                 PIC X(3).
               03  COMPROB-ED             PIC ZZZZZZBB.
               03  RUBRO-ED               PIC Z(7).
               03  FILLER                 PIC X(1).
               03  CUENTA-ED              PIC ZZZZZZ.
               03  FILLER                 PIC X(1).
               03  S-CUENTA-ED            PIC ZZZZ.
               03  FILLER                 PIC X(3).
               03  NOM-ED                 PIC X(30).
               03  FILLER                 PIC XX.
               03  TIPO-ED                PIC X(10).
               03  FILLER                 PIC X.
               03  IMPORTE-ED             PIC ---,---,--9.99.
               03  FILLER                 PIC X(5).
               03  IMP-CHEQ-ED            PIC ---,---,--9.99.
               03  FILLER                 PIC X(8).
               03  TARJ-UTI-ED            PIC 99. 
               03  TARJ-ED                PIC X(12).
           02  RENG-SUB.
               03  FILLER                 PIC X(30) VALUE SPACES.
               03  FILLER                 PIC X(20) VALUE ALL "*".
               03  FILLER                 PIC XX.
               03  FILLER                 PIC X(10) VALUE "Subtotal".
               03  FILLER                 PIC XX.
               03  FILLER                 PIC X(11) VALUE "Importe.:".
               03  TOT-IMPORTE-S-ED       PIC --,---,--9.99.
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(11) VALUE "Cheque..:".
               03  TOT-CHEQUE-S-ED        PIC --,---,--9.99.
           02  RENG-TOTAL.
               03  FILLER                 PIC X(30) VALUE SPACES.
               03  FILLER                 PIC X(20) VALUE ALL "*-".
               03  FILLER                 PIC XX.
               03  FILLER                 PIC X(10) VALUE "TOTAL".
               03  FILLER                 PIC XX.
               03  FILLER                 PIC X(11) VALUE "Importe.:".
               03  TOT-IMPORTE-ED         PIC --,---,--9.99.
               03  FILLER                 PIC X(5).
               03  FILLER                 PIC X(11) VALUE "Cheque..:".
               03  TOT-CHEQUE-ED          PIC --,---,--9.99.         
       01  CAMPOS-DE-CALCULO.
           03  TOT-IMPORTE                PIC S9(8)V99.
           03  TOT-CHEQUE                 PIC S9(8)V99.
           03  TOT-IMPORTE-S              PIC S9(8)V99.           
           03  TOT-CHEQUE-S               PIC S9(8)V99.
       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "MANEJO.ERR".
           COPY "LACAJA.ERR".
           COPY "TARJ.ERR".
           COPY "IMP-132.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.
           PERFORM ARMA-PANTALLA.
           OPEN INPUT  LACAJA EMPRESAS CUENTAS TARJ MANEJO
                OUTPUT IMP-132.
       COMIENZO.
           ACCEPT FECHA-P FROM DATE
           MOVE FECHA-P TO FECHA-L
           MOVE DIA-P TO DIA-L
           MOVE MES-P TO MES-L
           MOVE ANO-P TO ANO-L
           DISPLAY FECHA-L LINE 2 POSITION 70.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE LACAJA IMP-132 EMPRESAS CUENTAS.
       FF1.
           EXIT PROGRAM.
       FF2.
           STOP RUN.
      *...............................................................* 
      *                  CARGA DE PARAMETROS                          * 
      *...............................................................* 
      
       TOMA-PARAMETROS.
       C1.
           PERFORM MENS.
           ACCEPT EMP LINE 10 POSITION 30 NO BEEP TAB ON EXCEPTION
               CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO FF.
       C2.                
           PERFORM MENS1.
           ACCEPT SUC LINE 12 POSITION 30 NO BEEP TAB ON EXCEPTION
               CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C1.           
           MOVE EMP TO NUM-EMP MA-EMP
           MOVE SUC TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY DISPLAY "No Exsiste empresa"
                LINE 12 POSITION 35 SIZE 20
                GO TO C2.
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                LINE 24 POSITION 15 SIZE 64
                GO TO C2.
           MOVE RAZ-SOC-EMP TO EMP-ED.
           DISPLAY RAZ-SOC-EMP LINE 12 POSITION 35 SIZE 30.
       C3.
           PERFORM MENS1.
           ACCEPT TURNO LINE 16 POSITION 30 NO BEEP TAB ON EXCEPTION 
               CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
           MOVE TURNO TO TURNO-ED.
       C4.
           PERFORM MENS1.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 6.
           ACCEPT DF LINE 18 POSITION 30 NO BEEP TAB ON EXCEPTION 
               CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3.
       C5.
           PERFORM MENS1.
           DISPLAY SPACES LINE 20 POSITION 30 SIZE 6.
           ACCEPT HF LINE 20 POSITION 30 NO BEEP TAB ON EXCEPTION 
               CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.
           MOVE D-D TO D-DIA-ED
           MOVE D-M TO D-MES-ED
           MOVE D-A TO D-ANO-ED
           MOVE H-D TO H-DIA-ED
           MOVE H-M TO H-MES-ED
           MOVE H-A TO H-ANO-ED.
           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D.
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
        

       TOMA-PARAMETROS-F.
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
           
       LEA.
           READ LACAJA NEXT RECORD AT END GO TO PROCESO-F.           
                                              
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
           IF LAC-EMP    NOT = EMP MOVE 9 TO VALIDO 
                                   GO TO VALIDA-F.
           IF LAC-SUC    NOT = SUC MOVE 9 TO VALIDO
                                   GO TO VALIDA-F.
           IF LAC-TURNO  NOT = TURNO MOVE 9 TO VALIDO
                                   GO TO VALIDA-F.
           COMPUTE ESTA = (LAC-ANO * 372) + (LAC-MES * 31) + LAC-DIA.

           IF ESTA < DESDE OR ESTA > HASTA MOVE 9 TO VALIDO
                                   GO TO VALIDA-F.
           

       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           IF LAC-CONDI = 0 MOVE 1 TO LAC-CONDI.
           IF LAC-RUBRO = 100001 MOVE 99 TO LAC-CONDI.
           IF ESTA-CONDI = 0
              MOVE LAC-CONDI TO ESTA-CONDI.
           
           IF ESTA-CONDI NOT = LAC-CONDI
              PERFORM CORTA-CONDI THRU CORTA-CONDI-F.                

           MOVE TURNO          TO TURNO-ED
           MOVE LAC-ANO        TO LAC-ANO-ED
           MOVE LAC-MES        TO LAC-MES-ED
           MOVE LAC-DIA        TO LAC-DIA-ED

           IF LAC-CONDI = 99 MOVE "Saldo Ant" TO CONDI-ED.
           IF LAC-CONDI = 1 MOVE "Contado" TO CONDI-ED.
           IF LAC-CONDI = 9 MOVE "Cobros" TO CONDI-ED.
           IF LAC-CONDI = 6 MOVE "Credito" TO CONDI-ED.
           IF LAC-CONDI = 5 MOVE "Pago" TO CONDI-ED.
           
           MOVE LAC-COMPROB    TO COMPROB-ED
           MOVE LAC-CUENTA     TO CUENTA-ED
           MOVE LAC-SUB-CTA    TO S-CUENTA-ED
           MOVE LAC-EMP       TO EMP-CTA
           MOVE LAC-SUC       TO SUC-CTA
           MOVE LAC-RUBRO     TO RUBRO-CTA
           MOVE LAC-CUENTA    TO CUENTA
           MOVE LAC-SUB-CTA   TO SUB-CUENTA
           READ CUENTAS INVALID KEY MOVE "No Cuenta" TO NOMBRE-C.
           MOVE NOMBRE-C TO NOM-ED
           IF LAC-TIPO =       "E" MOVE "Efectivo" TO TIPO-ED.
           IF LAC-TIPO =       "C" MOVE "Cheque" TO TIPO-ED.
           IF LAC-TIPO =       "T" MOVE "Tarjeta" TO TIPO-ED.
           IF LAC-TIPO =       "O" MOVE "Otros" TO TIPO-ED.
           IF LAC-CONDI = 6        MOVE "Ctas.Ctes "     TO TIPO-ED.
           
           MOVE LAC-RUBRO TO RUBRO-ED.
           IF LAC-SUMA-RESTA = 0 
              MOVE LAC-IMPORTE    TO IMPORTE-ED
              MOVE LAC-IMP-CHEQ   TO IMP-CHEQ-ED
              MOVE LAC-TARJ-UTI   TO TARJ-UTI-ED 
              COMPUTE  TOT-IMPORTE-S = TOT-IMPORTE-S + LAC-IMPORTE
              COMPUTE  TOT-CHEQUE-S  = TOT-CHEQUE-S  + LAC-IMP-CHEQ
              COMPUTE TOT-IMPORTE    = TOT-IMPORTE + LAC-IMPORTE
              COMPUTE TOT-CHEQUE     = TOT-CHEQUE  + LAC-IMP-CHEQ
            ELSE
              COMPUTE IMPORTE-ED     = 0 - LAC-IMPORTE
              COMPUTE IMP-CHEQ-ED    = 0 - LAC-IMP-CHEQ
              MOVE LAC-TARJ-UTI TO TARJ-UTI-ED TAR-NUM
              COMPUTE  TOT-IMPORTE-S = TOT-IMPORTE-S - LAC-IMPORTE
              COMPUTE  TOT-CHEQUE-S  = TOT-CHEQUE-S  - LAC-IMP-CHEQ
              COMPUTE TOT-IMPORTE    = TOT-IMPORTE - LAC-IMPORTE
              COMPUTE TOT-CHEQUE     = TOT-CHEQUE  - LAC-IMP-CHEQ.
      
       READ TARJ INVALID KEY MOVE " " TO TAR-NOM.
           MOVE TAR-NOM TO TARJ-ED.

       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           WRITE LINEA FROM TIT1 AFTER 1.
           WRITE LINEA FROM TIT2 AFTER 1.
           WRITE LINEA FROM TIT3 AFTER 1.
           WRITE LINEA FROM TIT4 AFTER 1
           MOVE 5 TO CONT-RENG.

       TITULOS-F.
           EXIT.

       IMPRIMA.
           WRITE LINEA FROM RENG1 AFTER 1.
            
       IMPRIMA-F.
           EXIT.

           
       CORTA-CONDI.
           MOVE TOT-IMPORTE-S TO TOT-IMPORTE-S-ED.
           MOVE TOT-CHEQUE-S  TO TOT-CHEQUE-S-ED.
           WRITE LINEA FROM RENG-SUB AFTER 1.
           MOVE ALL "-" TO LINEA.           
           WRITE LINEA AFTER 2.
           MOVE 0 TO TOT-IMPORTE-S TOT-CHEQUE-S.
           MOVE LAC-CONDI TO ESTA-CONDI
           ADD 3 TO CONT-RENG.          
       CORTA-CONDI-F.
      *...........................................................* 
      *              POSICIONAMIENTO DE CLAVES SEGUN OPCION       *
      *...........................................................*
      *     COPY "ORDENA.RUT".

       PROCESO-F.
           EXIT.

       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Listado de Caja" LINE 2 POSITION 35.
           DISPLAY "------- -- ----" LINE 3 POSITION 35.
           DISPLAY "Empresa.......:" LINE 10 POSITION 10.
           DISPLAY "Sucursal......:" LINE 12 POSITION 10.
           DISPLAY "Turno.........:" LINE 16 POSITION 10.
           DISPLAY "Desde Fecha...:" LINE 18 POSITION 10.
           DISPLAY "Hasta Fecha...:" LINE 20 POSITION 10.
         
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       

       PROCESO-FINAL.           
           PERFORM CORTA-CONDI THRU CORTA-CONDI-F.
           MOVE TOT-IMPORTE TO TOT-IMPORTE-ED.
           MOVE TOT-CHEQUE  TO TOT-CHEQUE-ED.
           WRITE LINEA FROM RENG-TOTAL AFTER 1.
           ADD 1 TO CONT-RENG.
       PROCESO-FINAL-F.
           EXIT.

       MENS.
           DISPLAY "Con Escape o Flecha ^ Sale al Menu" LINE 24 
              POSITION 15 SIZE 64.
       MENS1.
           DISPLAY "Con las Flecha Avanza o Retrocede"
             LINE 24 POSITION 15 SIZE 64. 



           