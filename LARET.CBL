       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LARET.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "EMPRE.SEL".
           COPY "ARET.SEL".
           COPY "CUENTA.SEL".
           COPY "NUMERO.SEL".
           COPY "IMP-132.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "EMPREBM.REG".
           COPY "CUENTABM.REG".
           COPY "NUMERO.REG".
           COPY "ARET.REG".
           COPY "IMP-132.REG".
           
       WORKING-STORAGE SECTION.
           COPY "UTILES.WOR".
       01  VARIOS.
           03  DATOS-PARAMETROS.
               05  RR                     PIC X(80) VALUE ALL "Ä".
               05  EMP                    PIC 9(4) VALUE 0.
               05  SUC                    PIC 99 VALUE 0.
               05  RUB                    PIC 9(6) VALUE 0.
               05  CTA                    PIC 9(6) VALUE 0.
               05  S-CTA                  PIC 99 VALUE 0.
       01  TITULOS.
           03  TIT1.
               05  FILLER                 PIC X(60) VALUE 
               "Sistema Administrativo Contable y de Gestion Comercial".
           03  TIT2.
               05  FILLER                 PIC X(10) VALUE 
                           "Empresa.:".
               05  EMP-ED                 PIC X(30)BBBBB.
               05  FILLER                 PIC X(30) VALUE 
                   "Listado de Articulos a Retirar".
           03  TIT3.
               05  FILLER                 PIC X(10) VALUE  "Cuenta.:".
               05  CTA-ED                 PIC ZZZZZZBB.
               05  S-CTA-ED               PIC ZZBBBBBBBB.
               05  NOMBRE-ED              PIC X(30).
                  
           03  TIT4.
               05  FILLER                 PIC X(7) VALUE "Reng.".
               05  FILLER                 PIC X(8) VALUE " Fecha ".
               05  FILLER                 PIC X(3) VALUE SPACES.
               05  FILLER                 PIC X(7) VALUE 
                   "Comp.".
               05  FILLER                 PIC X(25) VALUE 
                   "   A r t i c u l o".
               05  FILLER                 PIC X VALUE SPACES.
               05  FILLER                 PIC X(7) VALUE "Cant.".
               05  FILLER                 PIC X(10) VALUE " Importe".
               05  FILLER                 PIC X(13) VALUE 
                  "Importe. Ret.".
       01  RENGLONES.                   
           03  RENG1.
               05  RENG-ED                PIC ZZZBB. 
               05  FECHA-ED.
                   07  DIA-ED             PIC 99/.
                   07  MES-ED             PIC 99/.
                   07  ANO-ED             PIC 99B.
               05  COMPROB-ED             PIC ZZZZZZZZB.
               05  ART-ED                 PIC X(25)BB.
               05  CANT-ED                PIC ZZZZZZB.
               05  IMPORTE-ED             PIC ZZZ,ZZ9.99BB.
               05  IMP-ARE-ED             PIC ZZZ,ZZ9.99.            
           03  RENG-TOT.                   
               05  FILLER                 PIC X(56) VALUE 
                  "            **********  Totales .:".
               05  IMP-ED                 PIC ZZZ,ZZ9.99BBB.
               05  IMPRET-ED              PIC ZZZ,ZZ9.99.
       01  CAMPOS-DE-CALCULO.
           03  IMP                        PIC 9(6)V99.
           03  IMPRET                     PIC 9(6)V99.
       PROCEDURE DIVISION.
       DECLARATIVES.
           
           COPY "EMPRE.ERR".
           COPY "CS.ERR".
           COPY "ARET.ERR".
           COPY "NUMERO.ERR".
           COPY "IMP-132.ERR".
       
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.

           OPEN INPUT ARETIRAR EMPRESAS NUMERO CUENTAS
                OUTPUT IMP-132.
       COMIENZO.
           PERFORM ARMA-PANTALLA.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
           PERFORM TITULOS THRU TITULOS-F.
           PERFORM PROCESO THRU PROCESO-F.
           PERFORM PROCESO-FINAL THRU PROCESO-FINAL-F.
       FF.    
           CLOSE ARETIRAR NUMERO EMPRESAS CUENTAS.
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
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 6.
           ACCEPT RUB LINE 12 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C2.
       
       C4.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 6.
           ACCEPT CTA LINE 14 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C3.
           IF CTA = 0
              MOVE " T o d a s " TO NOMBRE-ED
              MOVE 0 TO CTA-ED
                        S-CTA-ED
              GO TO TOMA-PARAMETROS-F.
       C5.
           DISPLAY SPACES LINE 14 POSITION 47 SIZE 2.
           ACCEPT S-CTA LINE 14 POSITION 47 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1 GO TO C4.
           MOVE EMP      TO EMP-CTA       
           MOVE SUC      TO SUC-CTA
           MOVE RUB      TO RUBRO-CTA
           MOVE CTA      TO CUENTA
           MOVE S-CTA    TO SUB-CUENTA
           READ CUENTAS INVALID KEY 
             DISPLAY "No existe cuenta" 
              LINE 14 POSITION 47 SIZE 30
               GO TO C4.
           MOVE NOMBRE-C TO NOMBRE-ED.               

           
           MOVE CTA   TO CTA-ED.
           MOVE S-CTA TO S-CTA-ED.
       TOMA-PARAMETROS-F.
           EXIT.

      *.................................................................* 
      *                   PROCEDIMIENTO GENERAL
      *.................................................................* 
       
       PROCESO.
         
           MOVE EMP   TO AR-EMP
           MOVE SUC   TO AR-SUC
           MOVE RUB   TO AR-RUB
           MOVE CTA   TO AR-CTA
           MOVE S-CTA TO AR-S-CTA
           MOVE 0     TO AR-COMPROB
           MOVE 0     TO AR-RENG.
           
           START ARETIRAR KEY NOT LESS CLAVE-ARETIRAR INVALID KEY
                  DISPLAY "No Pudo Hacer el Estart"
                    LINE 23 POSITION 15 SIZE 64
                     ACCEPT QQ 
                      STOP RUN.

       LEA.
           READ ARETIRAR NEXT RECORD AT END GO TO PROCESO-F.

       SALTE.    
           MOVE 0 TO VALIDO.
           PERFORM VALIDA THRU VALIDA-F.
           IF VALIDO NOT  = 0 GO TO LEA.
           PERFORM ARMA-SUME THRU ARMA-SUME-F
           PERFORM AVANCE-L THRU AVANCE-L-F
           PERFORM IMPRIMA THRU IMPRIMA-F.
           GO TO LEA.
      *..........................................................*    
      *                    VALIDACION REGISTROS                  *
      *..........................................................*

       VALIDA.
           
           IF AR-EMP NOT = EMP     MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.

           
           IF AR-SUC NOT = SUC     MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.
           
           
           IF CTA NOT = 0 
              IF AR-CTA NOT = CTA     MOVE 9 TO VALIDO
                                      GO TO VALIDA-F.
         
           
           IF S-CTA NOT = 0 
              IF AR-S-CTA NOT = S-CTA
                                  MOVE 9 TO VALIDO
                                  GO TO VALIDA-F.
       
       VALIDA-F.
           EXIT.
      *..........................................................* 
      *               ARMA LINEAS Y ACUMULA TOTALIZADORES        *
      *..........................................................*

       ARMA-SUME.
           MOVE AR-ANO      TO ANO-ED
           MOVE AR-MES      TO MES-ED
           MOVE AR-DIA      TO DIA-ED
           MOVE AR-RENG     TO RENG-ED
           MOVE AR-ARTICULO TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "**** No Articulo ****"
                TO DET-NUM.
           MOVE DET-NUM     TO ART-ED.
           MOVE AR-IMPORTE  TO IMPORTE-ED.
           MOVE AR-IMP-ARE  TO IMP-ARE-ED.
           MOVE AR-COMPROB  TO COMPROB-ED.
           MOVE AR-CANTIDAD TO CANT-ED.
           COMPUTE IMP = IMP + AR-IMPORTE.
           COMPUTE IMPRET = IMPRET + AR-IMP-ARE.
       ARMA-SUME-F.
           
           COPY "AVANCES.RUT".

      * ................................................................*
      *    IMPRESION DE TITULOS Y RENGLONES                             *
      * ................................................................*

       TITULOS.

           WRITE LINEA FROM TIT1 AFTER 1.
           WRITE LINEA FROM TIT2 AFTER 1.
           WRITE LINEA FROM TIT3 AFTER 2.
           WRITE LINEA FROM TIT4 AFTER 1
           MOVE RR TO LINEA.                                        
           WRITE LINEA AFTER 2.
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
           DISPLAY 
            "Sistema Administrativo Contable y de Gestion Comercial"
                     LINE 2 POSITION 3.
           DISPLAY 
            "------------------------------------------------------"
                     LINE 3 POSITION 3.
           DISPLAY "Listado de Articulos a Retirar"
                    LINE 5 POSITION 20.
           DISPLAY "Empresa .:" LINE 10 POSITION 10.      
           DISPLAY "Rubro ...:" LINE 12 POSITION 10.
           DISPLAY "Cuenta (0.- Todas) .:" LINE 14 POSITION 10.
      * ................................................................*
      *     RUTINA DE IMPRESION FINAL (TOTALES Y OTROS )                *
      * ................................................................*

       PROCESO-FINAL.           
           MOVE RR TO LINEA.
           WRITE LINEA AFTER 2.
           WRITE LINEA FROM RENG-TOT AFTER 1.
       PROCESO-FINAL-F.
           EXIT.
