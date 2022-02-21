       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REMITO.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "NUMERO.SEL".
              COPY "ART02.SEL".
              COPY "CUENTA.SEL".
              COPY "EMPRE.SEL".
              COPY "COMPRO.SEL".
              COPY "IMP-132.SEL".
              COPY "CPOSTAL.SEL".
              COPY "P-CTAS.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "NUMERO.REG".
              COPY "ART02.REG".
              COPY "EMPREBM.REG".
              COPY "COMPRO1.REG".
              COPY "CUENTABM.REG".
              COPY "P-CTAS.REG".
              COPY "CPOSTAL.REG".
              COPY "IMP-132.REG".
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "ENCABE.WOR".
       01  VARIOS.
           03  EMP                PIC 9(4) VALUE 0.
           03  SUC                PIC 99 VALUE 0.
           03  RUBRO              PIC 9(6) VALUE 0.
       01  A                      PIC X.
       77  EXISTE                 PIC X(2) VALUE "SI".       
      * 01  NUMERO                 PIC 9(6).
       01  CONT                   PIC 9(6) VALUE 0.
       01  YAGRABO                PIC 9 VALUE 0.
       01  VECTORES.
           03  VEC-FACTURA OCCURS 20.
               05  V-ART          PIC 9(6).
               05  V-CAN          PIC S9(6)V99.
               05  V-PRE          PIC S9(6)V99.
               05  V-IMP          PIC S9(6)V99.
               05  V-IVA          PIC S9(6)V99.
               05  V-IV1          PIC S9(6)V99.
               05  V-OTR          PIC S9(6)V99.

       01  CAMPOS-DE-CAPTURA.
           
           03  PORC               PIC 99V99 VALUE 0.
           03  FECHA              PIC 9(6).
           03  R-FECHA REDEFINES FECHA.
               05  DIA            PIC 99.
               05  MES            PIC 99.
               05  ANO            PIC 99.
           03  SUMA-RESTA         PIC 9    VALUE 0.
           03  CAP-ART            PIC 9(6) VALUE 0.
           03  CAP-CAM            PIC 9(2) VALUE 0.
           03  CAP-REP            PIC 9(6) VALUE 0.

           03  CAP-CAN            PIC S9(6)V99 VALUE 0.
           03  CAP-PRE            PIC S9(6)V99 VALUE 0.
           03  CAP-IMP            PIC S9(6)V99 VALUE 0.
           03  CAP-IVA            PIC S9(6)V99 VALUE 0.
           03  CAP-IV1            PIC S9(6)V99 VALUE 0.
           03  CAP-OTR            PIC S9(6)V99 VALUE 0.
           
           03  CAP-CTA            PIC 9(6) VALUE 0.
           03  CAP-S-C            PIC 9(2) VALUE 0.
       01  CAMPOS-TOTALIZADORES.
           03  TOT-IMP            PIC S9(6)V99 VALUE 0.
           03  TOT-IVA            PIC S9(6)V99 VALUE 0.
           03  TOT-IV1            PIC S9(6)V99 VALUE 0.
           03  TOT-OTR            PIC S9(6)V99 VALUE 0.
           03  TOTAL              PIC S9(6)V99 VALUE 0.
       01  CAMPOS-EDITADOS.
           03  ART-ED             PIC ZZZZZZ.
           03  CAN-ED             PIC ---,--9.99.
           03  PRE-ED             PIC ---,--9.99.
           03  IMP-ED             PIC ---,--9.99.
           03  PESOS-ED           PIC --,---,--9.99.


       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "NUMERO.ERR".
           COPY "ART02.ERR".
           COPY "EMPRE.ERR".
           COPY "CUENTA.ERR".
           COPY "COMPRO1.ERR".
           COPY "P-CTA.ERR".
           COPY "CPOSTAL.ERR".
           COPY "IMP-132.ERR".
       
       END DECLARATIVES.
       PROGRAMA SECTION.

       ABRO-ARCHIVOS.
           OPEN I-O    COMPRO
                INPUT  ART02  NUMERO CUENTAS EMPRESAS P-DE-CTAS CPOSTAL
                OUTPUT IMP-132.

           PERFORM ARMA-PANTALLA1 THRU ARMA-PANTALLA1-F.
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F.
       ACA.
           PERFORM ARMA-PANTALLA2 THRU ARMA-PANTALLA2-F
           DISPLAY RAZ-SOC-EMP LINE 2 POSITION 14 SIZE 30.
           DISPLAY P-NOMBRE LINE 2 POSITION 59 SIZE 20.
           PERFORM PROCESO THRU PROCESO-F
           GO TO ACA.

       FF.
           CLOSE ART02  NUMERO CUENTAS EMPRESAS COMPRO 
                 P-DE-CTAS CPOSTAL IMP-132.
       FFF.
           EXIT PROGRAM.
       FFFF.
           STOP RUN.
      *  ----------------------------------------------------------


       ARMA-PANTALLA1.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE
           DISPLAY " SISTEMA DE FACTURACION " LINE 2 POSITION 25
           DISPLAY " Emision de Remitos " LINE 4 POSITION 26
           DISPLAY " Empresa que Remite "  LINE 12 POSITION 10
      *     DISPLAY " Porcentaje de Ajuste..." LINE 10 POSITION 10
      *       " 1 - Suma  2 - Resta "   LINE 12 POSITION 10 REVERSE
                   " Fecha de Remito.:" LINE 16 POSITION 10
                   " Rubro ..........:" LINE 14 POSITION 10
                   "Con Flecha o Escape  sale" LINE 23 POSITION 10.
       ARMA-PANTALLA1-F.
           EXIT.
              
      *  ----------------------------------------------------------

       TOMA-PARAMETROS.  
       C1.
           ACCEPT EMP LINE 12 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1     GO TO FF.
       C2.
           ACCEPT SUC LINE 12 POSITION 45 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FF.
           IF FLE1     GO TO C1.
           MOVE EMP TO NUM-EMP
           MOVE SUC TO SUC-EMP
           READ EMPRESAS INVALID KEY DISPLAY " No Existe Empresa "
                LINE 12 POSITION 50 SIZE 29
                GO TO C1.
           DISPLAY RAZ-SOC-EMP LINE 12 POSITION 50 SIZE 29.


       C3.
           ACCEPT RUBRO LINE 14 POSITION 40 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO FF.
           IF FLE1       GO TO C2.
           MOVE 1       TO P-VERSION.
           MOVE RUBRO   TO P-RUBRO
           MOVE 0       TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY 
              DISPLAY " Rubro Inexistente " LINE 14 POSITION 50 SIZE 29
              ACCEPT QQ
              GO TO C3.
           DISPLAY P-NOMBRE LINE 14 POSITION 50 SIZE 29.
       C4.
           ACCEPT FECHA LINE 16 POSITION 40 NO BEEP TAB
                ON EXCEPTION CONDI MOVE 1 TO SW1.
      
           IF ESCAPE-X   GO TO FF.
           IF FLE1       GO TO C3.
       C5.
      *     ACCEPT CAP-REP LINE 18 POSITION 40 NO BEEP TAB CONVERT
      *          ON EXCEPTION CONDI MOVE 1 TO SW1.
      *
      *     IF ESCAPE-X   GO TO FF.
      *     IF FLE1       GO TO C4.
      *     
      *     MOVE CAP-CAM    TO REP-CAMION
      *     MOVE CAP-REP    TO REP-NUMERO
      *     READ REPARTO INVALID KEY GO TO C5.


       TOMA-PARAMETROS-F.
           EXIT.

      *  ----------------------------------------------------------

       ARMA-PANTALLA2.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE
           DISPLAY " SISTEMA DE FACTURACION " LINE 1 POSITION 25 REVERSE
           DISPLAY "Empresa .:" LINE 2 POSITION 2.
           DISPLAY "Rubro :" LINE 2 POSITION 50.
           DISPLAY "Cuenta Nro. .:"          LINE 3 POSITION 2
                   "Direccion ...:"          LINE 4 POSITION 2 
                   "Sit. I.V.A. .:"          LINE 3 POSITION 55
                   "CUIT.:"                  LINE 4 POSITION 55
                   "Articulo "               LINE 6 POSITION 2
                   "Descripcion "            LINE 6 POSITION 13
                   "Cantidad "               LINE 6 POSITION 40
                   "Precio"                  LINE 6 POSITION 50
                   "Importe"                 LINE 6 POSITION 60
           DISPLAY ALL "-"                   LINE 7 POSITION 2 SIZE 77
           DISPLAY ALL "_"                   LINE 19 POSITION 2 SIZE 77
           DISPLAY "Art."                    LINE 21 POSITION 1
                   "Nombre"                  LINE 21 POSITION 8
                   "Cantidad"                LINE 21 POSITION 25
                   "Precio "                 LINE 21 POSITION 42
                  "Subtotal ...:"            LINE 20 POSITION 53
                  "I.V.A.18%...:"            LINE 21 POSITION 53
                  "I.V.A.9%....:"            LINE 22 POSITION 53
                  "Otros Imp. .:"            LINE 23 POSITION 53
                  "TOTAL.......:"            LINE 24 POSITION 53.

       ARMA-PANTALLA2-F.
           EXIT.

      *  ----------------------------------------------------------

       PROCESO.
           PERFORM CAPTURA-CUENTA THRU CAPTURA-CUENTA-F.
           MOVE 0 TO LIN I TOT-IMP TOT-IVA TOT-IV1 TOT-OTR.
       GIRA.
           PERFORM CAPTURA-RENGLON THRU CAPTURA-RENGLON-F.
           PERFORM GUARDA-MUESTRA THRU GUARDA-MUESTRA-F.
           IF I = 12 GO TO CIERRA-FACTURA.
           DISPLAY "Otro articulo ?(S) " LINE 24 POSITION 3 SIZE 30
           MOVE "S"   TO DED
           ACCEPT DED LINE 24 POSITION 35 NO BEEP TAB ECHO UPDATE
              REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X  GO TO CIERRA-FACTURA.
           IF DED NOT = "S" AND DED NOT = "s" GO TO CIERRA-FACTURA.
           GO TO GIRA.

       CIERRA-FACTURA.
           DISPLAY "Confirma Remito" LINE 24 POSITION 3 SIZE 30
           MOVE "S"   TO DED
           ACCEPT DED LINE 24 POSITION 35 NO BEEP TAB ECHO UPDATE
              REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X  GO TO PROCESO-F.
           IF DED NOT = "S" AND DED NOT = "s" GO TO PROCESO-F.
           
           MOVE EMP     TO EMP-COMPRO.
           MOVE SUC     TO SUC-COMPRO.
           MOVE 1       TO CAJA-COMPRO.
           MOVE 7       TO NUM-COMP
           MOVE SPACES  TO SUB-T-C-COMPRO
           MOVE SUC     TO SUC-C-COMPRO.
           
           READ COMPRO INVALID KEY 
                DISPLAY "** ** **"
                ACCEPT QQ
                MOVE 0 TO NUM-COMPRO.
           ADD 1 TO NUM-COMPRO.
           REWRITE REG-COMPRO INVALID KEY DISPLAY "No Regrabo Comp"
                   ACCEPT QQ
                   STOP RUN.
           
           MOVE 0 TO YAGRABO
           PERFORM ARMA-GRABA THRU ARMA-GRABA-F.
           MOVE 9 TO YAGRABO.
           PERFORM ARMA-GRABA THRU ARMA-GRABA-F.
      *     PERFORM IMPRIMA THRU IMPRIMA-F  2 TIMES
           MOVE 0 TO TOT-IMP TOT-OTR TOT-IVA TOT-IV1 TOTAL
                     S-TOTAL-ED DGR-ED IVA-INS-ED IVA-N-INS-ED.

           PERFORM ACTUALIZA THRU ACTUALIZA-F
           
           GO TO PROCESO-F.

      *  ----------------------------------------------------------

       CAPTURA-CUENTA.
       C1X.
           ACCEPT CAP-CTA LINE 3 POSITION 18 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO FF.
           IF FLE1       GO TO FF.
       C2X.
           ACCEPT CAP-S-C LINE 3 POSITION 25 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO PROCESO-F.
           IF FLE1       GO TO C1X.

           MOVE EMP      TO EMP-CTA
           MOVE SUC      TO SUC-CTA
           MOVE RUBRO    TO RUBRO-CTA
           MOVE CAP-CTA  TO CUENTA
           MOVE CAP-S-C  TO SUB-CUENTA
           READ CUENTAS INVALID KEY DISPLAY "No Existe Cuenta"
                LINE 3 POSITION 28 SIZE 25 
                GO TO C2X.
           DISPLAY NOMBRE-C LINE 3 POSITION 28 SIZE 25.
           DISPLAY DIRE     LINE 4 POSITION 20
           IF INSCRIPTO = "R" DISPLAY "INSCRITO"  LINE 3 POSITION 69.
           IF INSCRIPTO = "N" DISPLAY "NO INSCR." LINE 3 POSITION 69.
           IF INSCRIPTO = "E" DISPLAY "EXENTO"    LINE 3 POSITION 69.
           IF INSCRIPTO = "C" DISPLAY "C.FINAL"   LINE 3 POSITION 69.
           DISPLAY IGAN     LINE 4 POSITION 65 SIZE 14.
       CAPTURA-CUENTA-F.
           EXIT.

      *  ----------------------------------------------------------

       CAPTURA-RENGLON.
       C3X.
           DISPLAY SPACES LINE 22 POSITION 1 SIZE 6
           ACCEPT CAP-ART LINE 22 POSITION 1 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO PROCESO-F.
           IF FLE1       GO TO C2X.
           MOVE CAP-ART TO COD-NUM
           READ NUMERO INVALID KEY GO TO C3X.
                      
           MOVE EMP     TO EMP-ART02
           MOVE SUC     TO SUC-ART02
      *     MOVE CAP-ART TO NUM-ART02
      *     MOVE 1       TO MAR-ART02.
           
           
           START ART02 KEY NOT LESS CLAVE-ART02 INVALID KEY 
                 DISPLAY "imposible start art02"
                 ACCEPT QQ
                 GO TO C3X.

       LEE-ART02.    
           READ ART02 NEXT RECORD AT END DISPLAY "no art02"
                LINE 22 POSITION 9 SIZE 15
                GO TO C3X.
           DISPLAY DET-NUM LINE 22 POSITION 9 SIZE 15.
           IF NUM-ART02 > CAP-ART GO TO C3X.
           IF NUM-ART02 NOT = CAP-ART GO TO LEE-ART02.

       C4X.
           ACCEPT CAP-CAN LINE 22 POSITION 26 NO BEEP TAB CONVERT
                ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO PROCESO-F.
           IF FLE1       GO TO C3X.
           IF CAP-CAN = 0 GO TO C4X.
           MOVE PRE-V-ART02  TO CAP-PRE PRE-ED
           DISPLAY PRE-ED LINE 22 POSITION 40.

       C5X.
           DISPLAY "Confirma ? (S) ..:" LINE 24 POSITION 3 SIZE 35
           MOVE "S"  TO DED
           ACCEPT DED LINE 24 POSITION 25 NO BEEP TAB ECHO UPDATE 
                REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X   GO TO PROCESO-F.
           IF FLE1       GO TO C4X.
           IF DED NOT = "S" AND DED = "s" GO TO C4X.
           COMPUTE CAP-IMP ROUNDED = CAP-CAN * CAP-PRE
           COMPUTE CAP-IVA ROUNDED = CAP-IMP * .18
           COMPUTE TOT-IMP  = TOT-IMP + CAP-IMP
           COMPUTE TOT-IVA  = TOT-IVA + CAP-IVA
           COMPUTE TOT-OTR  = TOT-OTR + CAP-OTR.

           MOVE 0 TO CAP-IMP CAP-IVA TOT-IMP TOT-IVA TOT-OTR

           IF INSCRIPTO = "R" OR
              INSCRIPTO = "E" MOVE 0 TO CAP-IV1 CAP-OTR  
                              GO TO CAPTURA-RENGLON-F.
           COMPUTE CAP-IV1 ROUNDED = CAP-IMP * .09.
           COMPUTE TOT-IV1  = TOT-IV1 + CAP-IV1.
           MOVE 0 TO CAP-IV1 TOT-IV1.

       CAPTURA-RENGLON-F.
           EXIT.

      *  ----------------------------------------------------------

       GUARDA-MUESTRA.
           ADD 1 TO I
           MOVE CAP-ART     TO V-ART(I) 
           MOVE CAP-CAN     TO V-CAN(I) CAN-ED
           MOVE CAP-PRE     TO V-PRE(I) PRE-ED
           MOVE CAP-IMP     TO V-IMP(I) IMP-ED
           MOVE CAP-IVA     TO V-IVA(I)
           MOVE CAP-IV1     TO V-IV1(I)
           MOVE CAP-OTR     TO V-OTR(I).
           COMPUTE LIN =  7 + I
           DISPLAY ART-ED       LINE LIN POSITION 3
           DISPLAY DET-NUM      LINE LIN POSITION 13 SIZE 25
           DISPLAY CAN-ED       LINE LIN POSITION 40
           DISPLAY PRE-ED       LINE LIN POSITION 50
           DISPLAY IMP-ED       LINE LIN POSITION 60.
           MOVE TOT-IMP   TO PESOS-ED
           DISPLAY PESOS-ED LINE 20 POSITION 66. 
           MOVE TOT-IVA   TO PESOS-ED
           DISPLAY PESOS-ED LINE 21 POSITION 66. 
           MOVE TOT-IV1   TO PESOS-ED
           DISPLAY PESOS-ED LINE 22 POSITION 66. 
           MOVE TOT-OTR   TO PESOS-ED
           DISPLAY PESOS-ED LINE 23 POSITION 66. 
           COMPUTE PESOS-ED = TOT-IMP + TOT-IVA + TOT-IV1 + TOT-OTR
           DISPLAY PESOS-ED LINE 24 POSITION 66.
       
       GUARDA-MUESTRA-F.
           EXIT.

      *  ----------------------------------------------------------
       
       ARMA-GRABA.
           MOVE NUM-COMPRO    TO NUMERO-ED.
           MOVE DIA           TO DIA-F.           
           MOVE MES           TO MES-F.
           MOVE ANO           TO ANO-F.
           MOVE CUENTA        TO NRO-CTA-ED.
           MOVE SUB-CUENTA    TO S-NRO-CTA-ED.
           MOVE NOMBRE-C      TO NOMBRE-ED.
           MOVE DIRE          TO DIRE-ED.
           MOVE COD-LOCA      TO CPOS-ED C-POS
           MOVE COD-LOCA-1    TO C-POS-1.
           READ CPOSTAL INVALID KEY MOVE "*-*-*-*-*-*-*-*" TO
                NOM-POSTAL.
           MOVE NOM-POSTAL    TO LOCA-ED.
           IF INSCRIPTO = "R" 
               MOVE " RESPONSABLE INSCRIPTO" TO IVA-ED.
           IF INSCRIPTO = "N" 
               MOVE "RESPONSABLE NO INSCRIPTO."  TO IVA-ED.
           IF INSCRIPTO = "E" MOVE "E X E N T O"    TO IVA-ED.
           IF INSCRIPTO = "C" MOVE "C.  F I N A L"   TO IVA-ED.
           MOVE IGAN          TO CUIT-ED.
           MOVE "  "     TO COND-ED.
           PERFORM IMPRIMA THRU IMPRIMA-F.
       PONE-0-A-I.
           MOVE 0 TO I.
       
       IMP-RENG.
           ADD 1 TO I.
           IF I = 13 GO TO SIGUE.
           IF V-ART(I) = 0 MOVE SPACES TO LINEA
                           WRITE LINEA AFTER 1
                           GO TO IMP-RENG.
           
           MOVE V-ART(I) TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "No Existe"
                TO DET-NUM.
           
           MOVE V-ART(I)    TO ARTI-ED.
           MOVE V-CAN(I)    TO CANT-ED.
           MOVE DET-NUM     TO DESC-ED
           MOVE V-PRE(I)    TO PRECIO-ED.
           MOVE V-IMP(I)    TO PRECIO-TOT-ED.
           WRITE LINEA FROM RENG7 AFTER 1.
           
       GRABA-VENTAS.
      *     MOVE CO-NUMERO    TO VEN-COMPROB           
      *     MOVE 1            TO VEN-TIPO-COMP
      *     MOVE I            TO VEN-RENGLON
      *     MOVE V-ART(I)     TO VEN-ARTICULO VEN-ARTICULO1
      *     MOVE EMP-CTA      TO VEN-EMPRE
      *     MOVE SUC-CTA      TO VEN-SUC
      *     MOVE RUBRO-CTA    TO VEN-RUBRO
      *     MOVE CUENTA       TO VEN-CUENTA
      *     MOVE SUB-CUENTA   TO VEN-S-CUENTA 
      *     MOVE ANO          TO VEN-ANO
      *     MOVE MES          TO VEN-MES
      *     MOVE DIA          TO VEN-DIA
      *     MOVE V-CAN(I)     TO VEN-CANTIDAD
      *     MOVE V-PRE(I)     TO VEN-PRECIO
      *     MOVE 0            TO VEN-DESCUENTO
      *     MOVE V-IMP(I)     TO VEN-IMPORTE
      *     MOVE V-IVA(I)     TO VEN-IVA18
      *     MOVE V-IV1(I)     TO VEN-IVA9
      *     MOVE 0            TO VEN-INTERNO
      *     MOVE V-OTR(I)     TO VEN-OTRO.
      *     IF YAGRABO = 9 GO TO IMP-RENG.
      *     WRITE REG-VENTAS INVALID KEY DISPLAY "no grabo en ventas"
      *           ACCEPT QQ.
      * 
       ACTUALIZA-STOCK.
      *     COMPUTE ART-STOCK = ART-STOCK - V-CAN(I).
      *     REWRITE REG-ARTIC INVALID KEY DISPLAY "NO REGRABO ARTIC"
      *             ACCEPT QQ.
           GO TO IMP-RENG.
      
       SIGUE.
           MOVE SPACES TO LINEA.
           MOVE TOT-IMP     TO S-TOTAL-ED.
           MOVE TOT-OTR     TO DGR-ED.
           MOVE TOT-IVA     TO IVA-INS-ED.
           MOVE TOT-IV1     TO IVA-N-INS-ED.
           COMPUTE TOTAL = TOT-IMP + TOT-OTR + TOT-IVA + TOT-IV1.
           MOVE TOTAL       TO TOT-ED.
           PERFORM IMPRIMA-FINAL THRU IMPRIMA-FINAL-F.
       
       GRABA-IVA.
      *     MOVE 1            TO IVA-EMPRE.
      *     MOVE 0            TO IVA-SUC.
      *     MOVE 1            TO IVA-LIBRO
      *     MOVE CO-NUMERO    TO IVA-NUMERO IVA-NUMERO-C
      *     MOVE ANO          TO IVA-ANO IVA-ANO-C
      *     MOVE MES          TO IVA-MES IVA-MES-C
      *     MOVE DIA          TO IVA-DIA IVA-DIA-C
      *     MOVE 100          TO IVA-RUBRO
      *     MOVE CUENTA       TO IVA-CUENTA       
      *     MOVE SUB-CUENTA   TO IVA-S-CUENTA
      *     MOVE 1            TO IVA-TIPO-COMPROB
      *     MOVE A            TO IVA-LETRA
      *     MOVE 0            TO IVA-SERIE
      *     IF INSCRIPTO = "R" MOVE 0 TO IVA-TIPO-OPERA.
      *     IF INSCRIPTO = "N" MOVE 1 TO IVA-TIPO-OPERA.
      *     IF INSCRIPTO = "E" MOVE 2 TO IVA-TIPO-OPERA.
      *     IF INSCRIPTO = "C" MOVE 3 TO IVA-TIPO-OPERA.
      *     MOVE TOTAL        TO IVA-M-TOTAL                                                       
      *     MOVE TOT-IMP      TO IVA-M-GRAVADO
      *     MOVE 0            TO IVA-M-EXENTO
      *     MOVE 0            TO IVA-IMP-INTERNO
      *                          IVA-RET-DGI
      *                          IVA-RET-DGR
      *     MOVE TOT-IVA      TO IVA-18
      *     MOVE TOT-IV1      TO IVA-9
      *     MOVE NOMBRE-C     TO IVA-DESCR
      *     MOVE IGAN         TO IVA-CUIT
      *     MOVE 0            TO IVA-IMP1
      *                          IVA-NETO1 
      *                          IVA-IMP2
      *                          IVA-NETO2
      *                          IVA-IMP3
      *                          IVA-NETO3.
      *     IF YAGRABO = 9 GO TO GRABA-MOVI.
      *     WRITE REG-IVA INVALID KEY DISPLAY "No Grabo iva"       
      *            ACCEPT QQ.
      *
      *
       GRABA-MOVI.
      *     MOVE 1            TO EMP-MOV.
      *     MOVE 0            TO SUC-EMP-MOV.
      *     MOVE 100          TO RUBRO-MOV
      *     MOVE CUENTA       TO CUENTA-MOV       
      *     MOVE SUB-CUENTA   TO SUB-CUENTA-MOV
      *     MOVE ANO          TO ANO-MOV
      *     MOVE MES          TO MES-MOV
      *     MOVE DIA          TO DIA-MOV
      *     MOVE CO-NUMERO    TO MOVIMIENTO-MOV           
      *     MOVE 1            TO TIPO-MOV    
      *     MOVE 1            TO TIPO-C-MOV
      *     MOVE "A"          TO SUB-T-C-MOV
      *     MOVE 0            TO SUC-C-MOV
      *     MOVE IVA-NUMERO-C TO COMPRO-MOV
      *     MOVE 0            TO REMA-MOV
      *     MOVE IVA-M-TOTAL  TO IMPORTE-MOV
      *     MOVE 0            TO IMP-EXT-MOV
      *     MOVE 0            TO SUCURSAL-MOV
      *     MOVE 0            TO ASIENTO-MOV.
      *
      *     IF YAGRABO = 9 GO TO ARMA-GRABA-F.
      *     WRITE REG-MOV INVALID KEY DISPLAY "No Regrabo Movimientos".
      * 
       ARMA-GRABA-F.
           EXIT.
       
       IMPRIMA.
      *     WRITE LINEA FROM RENG1 AFTER 1.
           WRITE LINEA FROM RENG2 AFTER 5.
           WRITE LINEA FROM RENG3 AFTER 1.
           WRITE LINEA FROM RENG4 AFTER 1.
           WRITE LINEA FROM RENG4-1 AFTER 1.
           WRITE LINEA FROM RENG5 AFTER 3.
           WRITE LINEA FROM RENG6 AFTER 1.
           WRITE LINEA FROM RENG6-1 AFTER 3.
           MOVE SPACES TO LINEA.
           WRITE LINEA AFTER 1.
       IMPRIMA-F.
           EXIT.
       IMPRIMA-FINAL.
      *     WRITE LINEA FROM RENG9 AFTER 1.
      *     WRITE LINEA FROM RENG10 AFTER 1.
      *     WRITE LINEA FROM RENG11 AFTER 1.
      *     WRITE LINEA FROM RENG12 AFTER 1.
           MOVE SPACES TO LINEA.
           WRITE LINEA AFTER 8.

       IMPRIMA-FINAL-F.
           EXIT.
      *  ----------------------------------------------------------
       
       ACTUALIZA.
      *     MOVE 1            TO EMP-CTA.
      *     MOVE 0            TO SUC-CTA.
      *     MOVE 100          TO RUBRO-CTA
      *     MOVE CUENTA       TO CUENTA       
      *     MOVE SUB-CUENTA   TO SUB-CUENTA
      *     COMPUTE SALDO = SALDO  + IVA-M-TOTAL.
      *     REWRITE REG-CTAS INVALID KEY DISPLAY 
      *            "No pudo regrabar ctasctes"
      *             ACCEPT QQ.


       ACTUALIZA-F.
           EXIT.


       PROCESO-F.
           EXIT.
           
      *  ==========================================================
       
       PROCESO-FINAL.
       PROCESO-FINAL-F.
           EXIT.


       MENS.
           DISPLAY SPACES LINE 13 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 14 POSITION 15 SIZE 64.           
           DISPLAY "Con Escape o Flecha Retrocede va al Menu" 
              LINE 13 POSITION 15.
       MENS1.
           DISPLAY SPACES LINE 13 POSITION 15 SIZE 64.
           DISPLAY SPACES LINE 14 POSITION 15 SIZE 64.           
           DISPLAY "Con Las Flechas Retrocede o Avanza"
                   LINE 13 POSITION 15.
           

