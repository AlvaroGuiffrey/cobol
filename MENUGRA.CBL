       IDENTIFICATION DIVISION.
       PROGRAM-ID.       MENU.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "CLAVES.SEL".
              COPY "EMPRE.SEL".
               COPY "MANEJO.SEL".
       DATA DIVISION.
       FILE SECTION.
              COPY "CLAVES.REG".
              COPY "EMPREBM.REG".       
              COPY "MANEJO.REG".
       WORKING-STORAGE SECTION. 
       01  DATOS-PARAMETROS.
           03  CLA1         PIC 9(10).
           03  CLA2         PIC X(10).
           03  CLA3         PIC X(10).
           03  EMP          PIC 9999.
           03  SUC          PIC 99.
           03  F-A          PIC X(4).
           03  F-B          PIC X(4).
           03  PAG          PIC X(4).
           03  C1           PIC X.           
           03  C2           PIC X.
           03  C3           PIC X.
       01  LAS-FECHAS.
           03  FECHA.
               05  ANO       PIC 99.
               05  MES       PIC 99.
               05  DIA       PIC 99.
           03  FECHA-ED.
               05  DIA-ED     PIC 99/.
               05  MES-ED     PIC 99/.
               05  ANO-ED     PIC 99.

       01  TECLA          PIC 9(3).
       01  OPCION         PIC 9(3).

              COPY "UTILES.WOR".

       01  ESTRACTURA-DE-MENU-GENERAL.
           03  LINEAS.
              05  RAFON          PIC X(80) VALUE ALL "╟".
              05  RA             PIC X(80) VALUE ALL "д".
              05  RA25           PIC X(25) VALUE ALL "д".
              05  RAA            PIC x(27) VALUE ALL "ш".
       
       PROCEDURE DIVISION.
       ABRO-ARCHIVOS.
           OPEN INPUT CLAVES EMPRESAS
                I-O MANEJO.
               
           PERFORM ARMA-PANTALLA.
       C1.       
           GO TO MENU-GENERAL.
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 40 SIZE 40.
           ACCEPT EMP LINE 10 POSITION 40 NO BEEP TAB CONVERT
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO FINAL-X.
       C2.
           PERFORM MENS.
           DISPLAY SPACES LINE 10 POSITION 46 SIZE 34.
           ACCEPT SUC LINE 10 POSITION 46 NO BEEP TAB CONVERT 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1.
           MOVE EMP TO NUM-EMP CLA-EMP MA-EMP
           MOVE SUC TO SUC-EMP CLA-SUC MA-SUC
           READ EMPRESAS INVALID KEY DISPLAY "No Existe Empresa"
                LINE 24 POSITION 15 SIZE 64 BLINK
                ACCEPT QQ
                GO TO C1.
           DISPLAY RAZ-SOC-EMP LINE 10 POSITION 50 SIZE 30.       
           READ CLAVES INVALID KEY DISPLAY 
             "No Tiene Estipuldas las Claves,Debe Cargarlas"
                 LINE 24 POSITION 15 SIZE 64
                  ACCEPT QQ
                   GO TO C1.
       C3.
           PERFORM MENS.
           DISPLAY SPACES LINE 12 POSITION 40 SIZE 10.
           ACCEPT CLA1 LINE 12 POSITION 40 REVERSE OFF
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.                 
           IF FLE1 GO TO C2.
       C4.
           PERFORM MENS.
           DISPLAY SPACES LINE 14 POSITION 40 SIZE 10.
           ACCEPT CLA2 LINE 14 POSITION 40 REVERSE OFF
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.                 
           IF FLE1 GO TO C3.
       C5.
           PERFORM MENS.
           DISPLAY SPACES LINE 16 POSITION 40 SIZE 10.
           ACCEPT CLA3 LINE 16 POSITION 40 REVERSE OFF
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.                 
           IF FLE1 GO TO C4.
       
       CHEQUE-CLAVE.           
           IF CLA1 NOT = CLAVE1      
              MOVE "N" TO C1
            ELSE
              MOVE "S" TO C1.
       
           IF CLA2 NOT = CLAVE2      
              MOVE "N" TO C2
            ELSE
              MOVE "S" TO C2.
       
           IF CLA3 NOT = CLAVE3      
              MOVE "N" TO C3
            ELSE
              MOVE "S" TO C3.

           IF C1 = "N" AND
               C2 = "N" AND
                C3 = "N" 
                 DISPLAY "Digitar Claves Correctas"
                  LINE 24 POSITION 15 SIZE 64 BLINK
                   ACCEPT QQ
                    GO TO C3.
       
       CHEQUEA-MANEJO.           
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "PARAMETROS DE MANEJO DEL SISTEMA"
                 LINE 4 POSITION 25.
           READ MANEJO INVALID KEY 
           DISPLAY "La Empresa no tiene cargado parametros de manejo"
                LINE 24 POSITION 15 SIZE 64
                 ACCEPT QQ 
                  GO TO C1.
           PERFORM ARMA-PANTALLA1.
           DISPLAY MA-CCT        LINE 10 POSITION 40.
           DISPLAY MA-PEND       LINE 11 POSITION 40.
           DISPLAY MA-IVA        LINE 12 POSITION 40.
           DISPLAY MA-CAJA       LINE 13 POSITION 40.
           DISPLAY MA-REM        LINE 14 POSITION 40.
           DISPLAY MA-PAGARE     LINE 15 POSITION 40.
           DISPLAY MA-ESTADIS    LINE 16 POSITION 40.
           DISPLAY MA-IMPUE      LINE 17 POSITION 40.
           DISPLAY MA-STOCK      LINE 18 POSITION 40.
           DISPLAY MA-FACTURA-A  LINE 19 POSITION 40.
           DISPLAY MA-FACTURA-B  LINE 20 POSITION 40.
           DISPLAY MA-IMP-PAGARE LINE 21 POSITION 40.
       
       PREGUNTA.           
           DISPLAY "Modifica salidas a Impresora (S/N)"
                   LINE 24 POSITION 15 SIZE 64.
           ACCEPT DED LINE 24 POSITION 70 NO BEEP TAB  REVERSE
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1.
           IF DED = "S" OR DED = "s" 
              GO TO SI-MODIFICA.
           GO TO MENU-GENERAL.
       SI-MODIFICA.
       C1X.           
           PERFORM MENS.
           DISPLAY SPACES LINE 19 POSITION 40 SIZE 40.
           ACCEPT F-A LINE 19 POSITION 40 NO BEEP TAB 
                  ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO PREGUNTA.
       C2X.           
           PERFORM MENS.
           DISPLAY SPACES LINE 20 POSITION 40 SIZE 40.
           ACCEPT F-B LINE 20 POSITION 40 NO BEEP TAB 
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C1X.
       C3X.           
           PERFORM MENS.
           DISPLAY SPACES LINE 21 POSITION 40 SIZE 40.
           ACCEPT PAG LINE 21 POSITION 40 NO BEEP TAB 
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF FLE1 GO TO C2X.

       REGRABA-MANEJO.
           MOVE F-A TO MA-FACTURA-A
           MOVE F-B TO MA-FACTURA-B
           MOVE PAG TO MA-IMP-PAGARE.
           
           REWRITE REG-MANEJO INVALID KEY 
              DISPLAY "NO REGRABO MANEJO"
              LINE 24 POSITION 15 SIZE 64
                ACCEPT QQ
                 STOP RUN.
       
       MENU-GENERAL.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.       
                 
           
           PERFORM DIBUJA THRU DIBUJA-F.
              
           ACCEPT  TECLA LINE 24 POSITION  46 NO BEEP TAB CONVERT
                    ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL-X.
           IF TECLA = 0 GO TO FINAL-X.
           IF TECLA = 1 GO TO CONTA.
           IF TECLA = 2 GO TO CTACTE.   
           IF TECLA = 3 GO TO IVA.
           IF TECLA = 4 GO TO GESTION.
           GO TO MENU-GENERAL.
       
       CONTA.
           MOVE 0 TO TECLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I  FROM 1 BY 1  UNTIL  I > 24.
           DISPLAY RA25 LINE 2 POSITION 27.           
           DISPLAY RA25 LINE 4 POSITION 27.
           DISPLAY RAA  LINE 5 POSITION 27.
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 7 POSITION 1.
           DISPLAY RA LINE 18 POSITION 1.
           DISPLAY RA LINE 20 POSITION 1.
           DISPLAY RA LINE 24 POSITION 1.

           DISPLAY "з" LINE 1 POSITION 1.
           DISPLAY "ц" LINE 7 POSITION 1.
           DISPLAY "ц" LINE 18 POSITION 1.
           DISPLAY "ц" LINE 20 POSITION 1.
           DISPLAY "ю" LINE 24 POSITION 1.
           
           DISPLAY "©" LINE 1 POSITION 80.
           DISPLAY "╢" LINE 7 POSITION 80.
           DISPLAY "╢" LINE 18 POSITION 80.
           DISPLAY "╢" LINE 20 POSITION 80.
           DISPLAY "ы" LINE 24 POSITION 80.
           
           
           DISPLAY "з" LINE 2 POSITION 27.
           DISPLAY "Ё" LINE 3 POSITION 27.
           DISPLAY "ю" LINE 4 POSITION 27.
           
           DISPLAY "©" LINE 2 POSITION 52.
           DISPLAY "Ёш" LINE 3 POSITION 52..
           DISPLAY "ыш" LINE 4 POSITION 52.
           
           DISPLAY "Contabilidad General" LINE 3 POSITION 29.
           DISPLAY " 1.- Mantenimiento de Matrizero"
                     LINE 10  POSITION 5.
           DISPLAY " 2.- Mantenimiento de Plan de Ctas"
                     LINE 11 POSITION 5.
           DISPLAY " 3.- Mantenimiento de Empresas"                                                      
                     LINE 12 POSITION 5.
           DISPLAY " 4.- Mantenimiento de Monedas"  
                     LINE 13 POSITION 5.
           DISPLAY " 5.- Mantenimiento de Cotizaciones"
                     LINE 14 POSITION 5.
           DISPLAY " 6.- Mantenimiento de Presupuesto"
                     LINE 15 POSITION 5.
           DISPLAY " 7.- Captura de Movimientos"
                     LINE 16 POSITION 5.
           DISPLAY " 8.- Actualizacion de Movimientos"
                     LINE 17 POSITION 5.
           DISPLAY " 9.- Consulta de Mayor"
                     LINE 10 POSITION 45.
           DISPLAY "10.- Listado  Diario"
                     LINE 11  POSITION 45.
           DISPLAY "11.- Listado Libro Mayor"
                     LINE 12 POSITION 45.
           DISPLAY "12.- Listado de Balance"
                     LINE 13 POSITION 45.
           DISPLAY "13.- Correcion de  Asientos"
                     LINE 14 POSITION 45.
           DISPLAY "14.- Borrado de Movimientos"     
                     LINE 15 POSITION 45.
           DISPLAY "15.- Cuadro  de Resultado"
                     LINE 16 POSITION 45.
           DISPLAY "16.- Control Mov. x Pantalla"
                     LINE 17  POSITION 45.
                  
           DISPLAY "0.- Vuelve al Menu  General"
                   LINE 19 POSITION 24 REVERSE.
           DISPLAY "Su Opcion ..?   ___"           
                 LINE 22 POSITION 15. 
           
           ACCEPT TECLA LINE 22 POSITION 32 NO BEEP TAB CONVERT
                   ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X GO TO MENU-GENERAL.
           
           IF TECLA = 0 GO TO MENU-GENERAL.
           
           IF TECLA = 1 CALL "MATRIZ.COB "
                       CANCEL "MATRIZ.COB ".
           
           IF TECLA = 2 CALL "P-CTA.COB "
                       CANCEL "P-CTA.COB ".
           
           IF TECLA = 3 CALL "EMPRESA.COB "
                       CANCEL "EMPRESA.COB ".
           
           IF TECLA = 4 CALL "MONEDA.COB "
                       CANCEL "MONEDA.COB ".
           
           IF TECLA = 5 CALL "COTIZA.COB "
                       CANCEL "COTIZA.COB ".
           
           IF TECLA = 6 CALL "PRESUPUE.COB "
                       CANCEL "PRESUPUE.COB ".
           
           IF TECLA = 7 CALL "92CAPTU.COB "
                       CANCEL "92CAPTU.COB ".
           
           IF TECLA = 8 CALL "92ACTUCA.COB "
                       CANCEL "92ACTUCA.COB ".
           
           IF TECLA = 9 CALL "CONSULTA.COB "
                       CANCEL "CONSULTA.COB ".
           
           IF TECLA = 10 CALL "92LISDIA.COB"
                       CANCEL "92LISDIA.COB ".
           
           IF TECLA = 11 CALL "92MAYOR.COB "
                       CANCEL "92MAYO.COB ".
           
           IF TECLA = 12 CALL "OTROBFEC.COB "
                       CANCEL "OTROBFEC.COB ".
                         CALL "REBALAN.COB"
                       CANCEL "REBALAN.COB ".
           
           IF TECLA = 13 CALL "CORASIBM.COB "
                       CANCEL "CORASIBM.COB ".
           
           IF TECLA = 14 CALL "92ABRECA.COB "
                       CANCEL "92ABRECA.COB ".
           
           IF TECLA = 15 CALL "BALPRESU.COB "
                       CANCEL "BALPRESU.COB ".
                         CALL "PRESU-BA.COB"           
                       CANCEL "PRESU-BA.COB".

           IF TECLA = 16 CALL "LISPANBM.COB "
                       CANCEL "LISPANBM.COB ".
           
           GO TO CONTA.

       CTACTE.
           MOVE 0 TO TECLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I  FROM 1 BY 1  UNTIL  I > 24.
           DISPLAY RA25 LINE 2 POSITION 27.           
           DISPLAY RA25 LINE 4 POSITION 27.
           DISPLAY RAA  LINE 5 POSITION 27.
           
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 7 POSITION 1.
           DISPLAY RA LINE 18 POSITION 1.
           DISPLAY RA LINE 20 POSITION 1.
           DISPLAY RA LINE 24 POSITION 1.

           DISPLAY "з" LINE 1 POSITION 1.
           DISPLAY "ц" LINE 7 POSITION 1.
           DISPLAY "ц" LINE 18 POSITION 1.
           DISPLAY "ц" LINE 20 POSITION 1.
           DISPLAY "ю" LINE 24 POSITION 1.
           
           DISPLAY "©" LINE 1 POSITION 80.
           DISPLAY "╢" LINE 7 POSITION 80.
           DISPLAY "╢" LINE 18 POSITION 80.
           DISPLAY "╢" LINE 20 POSITION 80.
           DISPLAY "ы" LINE 24 POSITION 80.
           
           
           
           DISPLAY "з" LINE 2 POSITION 27.
           DISPLAY "Ё" LINE 3 POSITION 27.
           DISPLAY "ю" LINE 4 POSITION 27.
           
           DISPLAY "©" LINE 2 POSITION 52.
           DISPLAY "Ёш" LINE 3 POSITION 52.
           DISPLAY "ыш" LINE 4 POSITION 52.
           
           DISPLAY "Cuentas   Corrientes" LINE 3 POSITION 29.
           DISPLAY " 1.- Mantenimiento de C. Postal"
                     LINE 10  POSITION 5.
           DISPLAY " 2.- Mantenimiento de Condiciones"
                     LINE 11 POSITION 5.
           DISPLAY " 3.- Mantenimiento de Cuentas"                                                      
                     LINE 12 POSITION 5.
           DISPLAY " 4.- Captura de  Movimiento"  
                     LINE 13 POSITION 5.
           DISPLAY " 5.- Consulta de Movimientos"
                     LINE 14 POSITION 5.
           DISPLAY " 6.- Consulta de Pendientes"
                     LINE 15 POSITION 5.
           DISPLAY " 7.- Listado de Saldos"
                     LINE 10 POSITION 45.
           DISPLAY " 8.- Resumen de Cuentas"
                     LINE 11 POSITION 45.
           DISPLAY " 9.- Cierre Periodico"
                     LINE 12 POSITION 45.
           DISPLAY "10.- Calculo de Intereses"
                     LINE 13  POSITION 45.
           DISPLAY "11.- Emision  de Recibos"
                     LINE 14  POSITION 45.
           DISPLAY "12.- Captura Mercaderia Pend. " 
                      LINE 15  POSITION 45.           
           DISPLAY "13.- Consulta Mercaderia Pend. " 
                      LINE 16  POSITION 45.           
           DISPLAY "14.- Listado Mercaderia Pend. " 
                      LINE 17  POSITION 45.           
           
           DISPLAY "0.- Vuelve al Menu  General"
                   LINE 19 POSITION 24 REVERSE.
           DISPLAY "Su Opcion ..?   ___"           
                 LINE 22 POSITION 15. 
           
           ACCEPT TECLA LINE 22 POSITION 32 NO BEEP TAB CONVERT
                   ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X GO TO MENU-GENERAL.
           
           IF TECLA = 0 GO TO MENU-GENERAL.
           
           IF TECLA = 1 CALL "CPOSTAL.COB "
                       CANCEL "CPOSTAL.COB ".
           
           IF TECLA = 2 CALL "CONDI.COB "
                       CANCEL "CONDI.COB ".
           
           IF TECLA = 3 CALL "CUENTA.COB "
                       CANCEL "CUENTA.COB ".
           
           IF TECLA = 4 CALL "CAPMOV.COB "
                       CANCEL "CAPMOV.COB ".
           
           IF TECLA = 5 CALL "CONMOV.COB "
                       CANCEL "CONMOV.COB ".
           
           IF TECLA = 6 CALL "CON-PEN.COB "
                       CANCEL "CON-PEN.COB ".
           
           IF TECLA = 7 CALL "LIS-SALD.COB "
                       CANCEL "LIS-SALD.COB ".
           
           IF TECLA = 8 CALL "RESUMEN.COB "
                       CANCEL "RESUMEN.COB ".
           
           IF TECLA = 9 CALL "CIE-CTAS.COB "
                       CANCEL "CIE-CTAS.COB ".
           
           IF TECLA = 10 CALL "CAL-INTE.COB"
                       CANCEL "CAL-INTE.COB ".
           
           IF TECLA = 11 CALL "RECIBO.COB"
                       CANCEL "RECIBO.COB ".

           IF TECLA = 12 CALL "ARET.COB"
                       CANCEL "ARET.COB ".

           IF TECLA = 13 CALL "CARET.COB"
                       CANCEL "CARET.COB ".

           IF TECLA = 14 CALL "LARET.COB"
                       CANCEL "LARET.COB ".


           GO TO CTACTE.



       IVA.
           
           MOVE 0 TO TECLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I  FROM 1 BY 1  UNTIL  I > 24.
           DISPLAY RA25 LINE 2 POSITION 27.           
           DISPLAY RA25 LINE 4 POSITION 27.
           DISPLAY RAA  LINE 5 POSITION 27.
           
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 7 POSITION 1.
           DISPLAY RA LINE 18 POSITION 1.
           DISPLAY RA LINE 20 POSITION 1.
           DISPLAY RA LINE 24 POSITION 1.

           DISPLAY "з" LINE 1 POSITION 1.
           DISPLAY "ц" LINE 7 POSITION 1.
           DISPLAY "ц" LINE 18 POSITION 1.
           DISPLAY "ц" LINE 20 POSITION 1.
           DISPLAY "ю" LINE 24 POSITION 1.
           
           DISPLAY "©" LINE 1 POSITION 80.
           DISPLAY "╢" LINE 7 POSITION 80.
           DISPLAY "╢" LINE 18 POSITION 80.
           DISPLAY "╢" LINE 20 POSITION 80.
           DISPLAY "ы" LINE 24 POSITION 80.
           
           
           
           DISPLAY "з" LINE 2 POSITION 27.
           DISPLAY "Ё" LINE 3 POSITION 27.
           DISPLAY "ю" LINE 4 POSITION 27.
           
           DISPLAY "©" LINE 2 POSITION 52.
           DISPLAY "Ёш" LINE 3 POSITION 52..
           DISPLAY "ыш" LINE 4 POSITION 52.
           
           DISPLAY "    I . V . A " LINE 3 POSITION 29.
           DISPLAY " 1.- Mantenimiento de Actividad"
                     LINE 10  POSITION 2.
           DISPLAY " 2.- Mantenimiento de Alicuota"
                     LINE 11 POSITION 2.
           DISPLAY " 3.- Captura de IVA"                                                      
                     LINE 12 POSITION 2.
           DISPLAY " 4.- Captura de Impuesto"  
                     LINE 13 POSITION 2.
           DISPLAY " 5.- Libros de  IVA"
                     LINE 14 POSITION 2.
           DISPLAY " 6.- Listado de  Impuestos  / Tasas"
                     LINE 15 POSITION 2.
           DISPLAY " 7.- Control, Listado  de Impuesto"
                     LINE 10 POSITION 42.
           DISPLAY " 8.- Control, Consulta de Impuesto"
                     LINE 11 POSITION 42.
           DISPLAY " 9.- Listado Formulario  468b"
                     LINE 12 POSITION 42.
           DISPLAY "10.- Listado Resolucion 127/93"
                     LINE 13  POSITION 42.
           DISPLAY "11.- Declaracion D.G.R"
                     LINE 14  POSITION 42.
           DISPLAY "12.- ..." LINE 15  POSITION 42.           
           DISPLAY "0.- Vuelve al Menu  General"
                   LINE 19 POSITION 24 REVERSE.
           DISPLAY "Su Opcion ..?   ___"           
                 LINE 22 POSITION 15. 
           
           ACCEPT TECLA LINE 22 POSITION 32 NO BEEP TAB CONVERT
                   ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X GO TO MENU-GENERAL.
           
           IF TECLA = 0 GO TO MENU-GENERAL.
           
           IF TECLA = 1 CALL "ACTI.COB "
                       CANCEL "ACTI.COB ".
           
           IF TECLA = 2 CALL "ALICUO.COB "
                       CANCEL "ALICUO.COB ".
           
           IF TECLA = 3 CALL "CAPIVA.COB "
                       CANCEL "CAPIVA.COB ".
           
           IF TECLA = 4 CALL "IMPUE.COB "
                       CANCEL "IMPUE.COB ".
           
           IF TECLA = 5 CALL "LIBIVA.COB "
                       CANCEL "LIBIVA.COB ".
           
           IF TECLA = 6 CALL "LISIMP.COB "
                       CANCEL "LISIMP.COB ".
           
           IF TECLA = 7 CALL "LIMPUE.COB "
                       CANCEL "LIMPUE.COB ".
           
           IF TECLA = 8 CALL "CIMPUE.COB "
                       CANCEL "CIMPUE.COB ".
           
           IF TECLA = 9 CALL "F468B.COB"
                       CANCEL "F468B.COB".
           
           IF TECLA = 10 CALL "LISRES.COB"
                       CANCEL "LISRES.COB ".
           
           IF TECLA = 11 CALL "DDJJ127.COB"
                       CANCEL "DDJJ127.COB ".


           GO TO IVA.


       GESTION.
           
           MOVE 0 TO TECLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM R1 VARYING I  FROM 1 BY 1  UNTIL  I > 24.
           DISPLAY RA25 LINE 2 POSITION 27.           
           DISPLAY RA25 LINE 4 POSITION 27.
           DISPLAY RAA  LINE 5 POSITION 27.
           
           DISPLAY RA LINE 1 POSITION 1.
           DISPLAY RA LINE 7 POSITION 1.
           DISPLAY RA LINE 19 POSITION 1.
           DISPLAY RA LINE 21 POSITION 1.
           DISPLAY RA LINE 24 POSITION 1.

           DISPLAY "з" LINE 1 POSITION 1.
           DISPLAY "ц" LINE 7 POSITION 1.
           DISPLAY "ц" LINE 19 POSITION 1.
           DISPLAY "ц" LINE 21 POSITION 1.
           DISPLAY "ю" LINE 24 POSITION 1.
           
           DISPLAY "©" LINE 1 POSITION 80.
           DISPLAY "╢" LINE 7 POSITION 80.
           DISPLAY "╢" LINE 19 POSITION 80.
           DISPLAY "╢" LINE 21 POSITION 80.
           DISPLAY "ы" LINE 24 POSITION 80.
           
           
           
           DISPLAY "з" LINE 2 POSITION 27.
           DISPLAY "Ё" LINE 3 POSITION 27.
           DISPLAY "ю" LINE 4 POSITION 27.
           
           DISPLAY "©" LINE 2 POSITION 52.
           DISPLAY "Ёш" LINE 3 POSITION 52.
           DISPLAY "ыш" LINE 4 POSITION 52.
           
           DISPLAY "  Gestion Comercial " LINE 3 POSITION 29.
           DISPLAY " 1.- Mantenimiento de Marcas "
                     LINE 8  POSITION 5.
           DISPLAY " 2.- Mantenimiento de Articulos "
                     LINE 9 POSITION 5.
           DISPLAY " 3.- Mantenimiento de Lineas"                                                      
                     LINE 10 POSITION 5.
           DISPLAY " 4.- Mantenimiento de Envases"  
                     LINE 11 POSITION 5.
           DISPLAY " 5.- Mantenimiento de Compras"
                     LINE 12 POSITION 5.
           DISPLAY " 6.- Mantenimiento Art. Ventas"  
                     LINE 13 POSITION 5.
           DISPLAY " 7.- Mantenimiento Tarj. Creditos"
                     LINE 14 POSITION 5.
           DISPLAY " 8.- Mantenimiento Condi. de Venta"
                     LINE 15 POSITION 5.
           DISPLAY " 9.- Mantenimiento Reg. de Caja"
                     LINE 16 POSITION 5.
           DISPLAY "10.- Mantenimiento y Modif. de Stock"
                     LINE 17 POSITION 5.
           DISPLAY "11.- Modif. Precios P/Lineas"
                     LINE 8  POSITION 45.
           DISPLAY "12.- Modif.Precios S/Costos"
                     LINE 9  POSITION 45.           
           DISPLAY "13.- Listado de Precios C/Posicion" 
                     LINE 10  POSITION 45.           
           DISPLAY "14.- Listado de Precios P/Marcas" 
                     LINE 11  POSITION 45.           
           DISPLAY "15.- Listado de Precios C/Pos" 
                     LINE 12  POSITION 45.           
           DISPLAY "16.- List. de Stock Valorizado" 
                     LINE 13  POSITION 45.           
           DISPLAY "17.- Listado Caja  P/Turno" 
                     LINE 14  POSITION 45.           
           DISPLAY "18.- Listado Venta P/Turno" 
                     LINE 15  POSITION 45.           
           DISPLAY "19.- Consulta p/Pantalla de Precios" 
                     LINE 16  POSITION 45.           
           DISPLAY "20.- Cotizacion de monedas" 
                     LINE 17  POSITION 45.           
           DISPLAY "21.- FACTURACION" 
                     LINE 18  POSITION 45.           

           DISPLAY "0.- Vuelve al Menu  General"
                   LINE 20 POSITION 24 REVERSE.
           DISPLAY "Su Opcion ..?   ___"           
                 LINE 22 POSITION 15. 
           
           ACCEPT TECLA LINE 22 POSITION 32 NO BEEP TAB CONVERT
                   ON EXCEPTION CONDI MOVE 1 TO SW1.

           IF ESCAPE-X GO TO MENU-GENERAL.
           IF TECLA = 0 GO TO MENU-GENERAL.
           
           IF TECLA = 1 CALL "MARCAS.COB"
                       CANCEL "MARCAS.COB".

           IF TECLA = 2 CALL "NUMERO.COB"
                       CANCEL "NUMERO.COB".
           
           IF TECLA = 3 CALL "LINEAS.COB"
                       CANCEL "LINEAS.COB".
           
           IF TECLA = 4 CALL "ENVASE.COB"
                       CANCEL "ENVASE.COB".
           
           IF TECLA = 5 CALL "ART01.COB"
                       CANCEL "ART01.COB".
           
           IF TECLA = 6 CALL "ART02.COB"
                       CANCEL "ART02.COB".
           
           IF TECLA = 7 CALL "TARJ.COB"
                       CANCEL "TARJ.COB".
           
           IF TECLA = 8 CALL "CONDI.COB"
                       CANCEL "CONDI.COB".
           
           IF TECLA = 9 CALL "CAP-CAJA.COB"
                       CANCEL "CAP-CAJA.COB".
           
           IF TECLA = 10 CALL "MSTOCK.COB"
                       CANCEL "MSTOCK.COB".
           
           IF TECLA = 11 CALL "ACTPRE.COB"
                       CANCEL "ACTPRE.COB".
           
           IF TECLA = 12 CALL "CAPPRE.COB"
                       CANCEL "CAPPRE.COB".
           
           IF TECLA = 13 CALL "LISPRE2.COB"
                       CANCEL "LISPRE2.COB".
           
           IF TECLA = 14 CALL "LISPRE8.COB"
                       CANCEL "LISPRE8.COB".
           
           IF TECLA = 15 CALL "LISPRE2.COB"
                       CANCEL "LISPRE2.COB".
           
           IF TECLA = 16 CALL "VSTOCK.COB"
                       CANCEL "VSTOCK.COB".
           
           IF TECLA = 17 CALL "LIS-CAJA.COB"
                       CANCEL "LIS-CAJA.COB".
           
           IF TECLA = 18 CALL "VENTURNO.COB"
                       CANCEL "VENTURNO.COB".
           
           IF TECLA = 19 CALL "ARTITOTA.COB"
                       CANCEL "ARTITOTA.COB".
           
           IF TECLA = 20 CALL "COTIZA.COB "
                       CANCEL "COTIZA.COB".
                     
           IF TECLA = 21 CALL "FAC.COB "
                       CANCEL "FAC.COB".

           GO TO GESTION.



       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Empresa que va a trabajar ? .:"
                LINE 10 POSITION 5.
           DISPLAY "Clave Nro. 1 .:" LINE 12 POSITION 10.
           DISPLAY "Clave Nro. 2 .:" LINE 14 POSITION 10.
           DISPLAY "Clave Nro. 3 .:" LINE 16 POSITION 10.
       ARMA-PANTALLA1.           
           DISPLAY "Ctas. Ctes      ...:" LINE 10 POSITION 10.
           DISPLAY "Pendientes      ...:" LINE 11 POSITION 10.
           DISPLAY "I.V.A           ...:" LINE 12 POSITION 10.
           DISPLAY "Caja            ...:" LINE 13 POSITION 10.
           DISPLAY "Remitos         ...:" LINE 14 POSITION 10.
           DISPLAY "Pagares         ...:" LINE 15 POSITION 10.
           DISPLAY "Estadisticas    ...:" LINE 16 POSITION 10.
           DISPLAY "Impuestos       ...:" LINE 17 POSITION 10.
           DISPLAY "Stock Pendiente ...:" LINE 18 POSITION 10.
           DISPLAY "Imprime Factura A .:" LINE 19 POSITION 10.
           DISPLAY "Imprime Factura B .:" LINE 20 POSITION 10.
           DISPLAY "Imprime Pagare  ...:" LINE 21 POSITION 10.
       
       
       MENS.
           DISPLAY "Con ESC sale del menu,Con Flecha ^ retrocede"
                   LINE 23 POSITION 15 SIZE 64.
       
       DIBUJA.       
           PERFORM R2 VARYING I FROM 1 BY 1 UNTIL I > 25.
           DISPLAY "шшзддддддддддддддддддддддддддддддддддддддддддд©шш"
                  LINE 1 POSITION 18.
           DISPLAY "шшЁ  Sistema Integral Administrativo Contable Ёшш"
                    LINE 2 POSITION 18.
           DISPLAY "шшЁ                      Y                    Ёшш" 
                    LINE 3 POSITION 18.
           DISPLAY "шшЁ               Gestion Comercial           Ёшш"
                       LINE 4 POSITION 18.
       
           
           DISPLAY "шшюдддддддддддддддддддддддддддддддддддддддддддышш"
                  LINE 5 POSITION 18.
           DISPLAY "шшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшшш"                       
                  LINE 6 POSITION 18.
       
       
       
       CUADRO1.    
           DISPLAY "дддддддддддддддддддддддддддд" LINE 8 POSITION 27.
           DISPLAY "ддддддддддддддбддддддддддддд" LINE 10 POSITION 27.
           DISPLAY "Ё" LINE 9 POSITION 27.
           DISPLAY "Ё" LINE 9 POSITION 54.

           DISPLAY "з" LINE 8 POSITION 27.          
           DISPLAY "©" LINE 8 POSITION 54.
           DISPLAY "ю" LINE 10 POSITION 27.
           DISPLAY "ы" LINE 10 POSITION 54.
           DISPLAY "        Menu General      " LINE 9 POSITION 28.
       CUADRO2.    
           DISPLAY "дддддддддддаддддддддддд" LINE 14 POSITION 11.
           DISPLAY "ддддддддддддддддддддддд" LINE 16 POSITION 11.
           DISPLAY "  1.- Contabilidad   " LINE 15 POSITION 12.
           DISPLAY "Ё" LINE 15 POSITION 11.
           DISPLAY "Ё" LINE 15 POSITION 33.
           DISPLAY "з" LINE 14 POSITION 11.          
           DISPLAY "©" LINE 14 POSITION 33.
           DISPLAY "ю" LINE 16 POSITION 11.
           DISPLAY "ы" LINE 16 POSITION 33.
           DISPLAY SPACES LINE 14 POSITION 34 SIZE 7.
           DISPLAY SPACES LINE 15 POSITION 34 SIZE 7.
           DISPLAY SPACES LINE 16 POSITION 34 SIZE 7.
       CUADRO3.    
           
           DISPLAY "дддддддддддаддддддддддд" LINE 19 POSITION 10.
           DISPLAY "ддддддддддддддддддддддд" LINE 21 POSITION 10.
           DISPLAY "2.- Cuentas Corrientes" LINE 20 POSITION 11.
           DISPLAY "Ё" LINE 20 POSITION 10.
           DISPLAY "Ё" LINE 20 POSITION 32.
           DISPLAY "з" LINE 19 POSITION 10.          
           DISPLAY "©" LINE 19 POSITION 32.
           DISPLAY "ю" LINE 21 POSITION 10.
           DISPLAY "ы" LINE 21 POSITION 32.
       CUADRO4.    
           
           DISPLAY "дддддддддддаддддддддддд" LINE 14 POSITION 48.
           DISPLAY "ддддддддддддддддддддддд" LINE 16 POSITION 48.
           DISPLAY "    3.-  I . V . A    " LINE 15 POSITION 49.
           DISPLAY SPACES LINE 14 POSITION 42 SIZE 6.
           DISPLAY SPACES LINE 15 POSITION 42 SIZE 6.
           DISPLAY SPACES LINE 16 POSITION 42 SIZE 6.
           
           DISPLAY "Ё" LINE 15 POSITION 48.
           DISPLAY "Ё" LINE 15 POSITION 71.
           DISPLAY "з" LINE 14 POSITION 48.          
           DISPLAY "©" LINE 14 POSITION 71.
           DISPLAY "ю" LINE 16 POSITION 48.
           DISPLAY "ы" LINE 16 POSITION 71.
       
       CUADRO5.    
           
           DISPLAY "дддддддддддаддддддддддд" LINE 19 POSITION 49.
           DISPLAY "ддддддддддддддддддддддд" LINE 21 POSITION 49.
           DISPLAY "4.- Gestion Comercial " LINE 20 POSITION 50.
           DISPLAY "Ё" LINE 20 POSITION 49.                    
           DISPLAY "Ё" LINE 20 POSITION 72.
           DISPLAY "з" LINE 19 POSITION 49.          
           DISPLAY "©" LINE 19 POSITION 72.
           DISPLAY "ю" LINE 21 POSITION 49.
           DISPLAY "ы" LINE 21 POSITION 72.
       
       
       RAYAS.
           DISPLAY "  Ё  " LINE 11 POSITION 39.
           DISPLAY "  Ё  " LINE 12 POSITION 39.
           DISPLAY "зддддддддддддддддддеддддддддддддддддд©" 
                      LINE 13 POSITION 22.
           DISPLAY "Ё" LINE 14 POSITION 41.
           DISPLAY "Ё" LINE 15 POSITION 41.
           DISPLAY "Ё" LINE 16 POSITION 41.
           DISPLAY "здддддддддддддддддддадддддддддддддддддд©" 
                      LINE 17 POSITION 21.
           DISPLAY "Ё                                      Ё"       
                      LINE 18 POSITION 21.
           DISPLAY "  Su opcion.. ?  ___   " 
                        LINE 24 POSITION 28.
           DISPLAY "                       " LINE 23 POSITION 28.
       DIBUJA-F.
           EXIT.

       
       R1.
           DISPLAY "Ё" LINE I POSITION 1.
           DISPLAY "Ё" LINE I POSITION 80.
       R2.       
           DISPLAY RAFON LINE I POSITION 1.
       
       FINAL-X.
           CLOSE EMPRESAS MANEJO CLAVES.
       SAL2.    
           STOP RUN.       
