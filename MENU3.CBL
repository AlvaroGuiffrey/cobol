       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU3.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. SET'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
      *...............................(LINEAS DE IMPRESION)
       01 CAPTURA.
          02 CLAVE-1-CAP       PIC 999.
          02 CLAVE-2-CAP       PIC 999.
          02 CLAVE-3-CAP       PIC 999.
      *...............................(CAMPOS DE EDICION)
       01 EDICION.
          02 NUM-E-EDI         PIC 9999.
          02 SUC-EDI         PIC 99.
          02 COD-LOC-EDI     PIC 9999.
          02 MONEDA-EDI      PIC 99.
          02 VERSION-EDI     PIC 99.
          02 CLAVE-1-EDI     PIC 999.
          02 CLAVE-2-EDI     PIC 999.
          02 CLAVE-3-EDI     PIC 999.
      *...............................(CAMPOS AUXILIARES)
       01 AUXILIARES.
          02 CODI            PIC 9.
       COPY "VARIOS.WOR".
      *    02 @@@@@@@@@@@@
       PROCEDURE DIVISION.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
      *...............................(ABRE PANTALLA)
           MOVE "M3MODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 = 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-INI.
           PERFORM TIT-PAN.Š           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
      *................................(PROGRAMA PRINCIPAL)
       ACTU.
           MOVE " - MENU DE TAREAS - " TO D20-CAP.
           MOVE 0 TO CODI.
       PANTA-1.
           PERFORM BORRA-PANTA.
           DISPLAY " <<< CODIGOS  DE  TAREAS >>> " LINE 6 POSITION 25
                           REVERSE.
           DISPLAY " 301 - Mant.de Marcas " LINE 8 POSITION 5.
           DISPLAY " 302 - Mant.de Articulos" LINE 9 POSITION 5.
           DISPLAY " 303 - Mant.de Lineas" LINE 10 POSITION 5.
           DISPLAY " 304 - Mant.de Envases" LINE 11 POSITION 5.
           DISPLAY " 305 - Mant.de Art. Compra" LINE 12 POSITION 5.
           DISPLAY " 306 - Mant.de Art. Venta" LINE 13 POSITION 5.
           DISPLAY " 307 - Mant.de Condiciones" LINE 14 POSITION 5.
           DISPLAY " 308 - Mant.de Cotizaciones" LINE 15 POSITION 5.
           DISPLAY " 309 - Mant.de Nro.Comprob." LINE 16 POSITION 5.
           DISPLAY " 310 - Mant.de Stock" LINE 17 POSITION 5.
           DISPLAY " 311 - Mant.de Partidas" LINE 18 POSITION 5.
           DISPLAY " 312 - Cambio de Partidas" LINE 19 POSITION 5.
      *     DISPLAY " 313 - Mant.de Empresas" LINE 20 POSITION 5.
      *     DISPLAY " 314 - C lculo  de intereses" LINE 21 POSITION 5.
      *     DISPLAY " 315 - " LINE 22 POSITION 5. 
      *=-=-=-=-=-= SEGUNDA PANTALLA =-=-=-=-=-=-=-=-
           DISPLAY " 316 - FACTURACION" LINE 8 POSITION 40.
           DISPLAY " 317 - Estadistica Ventas" LINE 9 POSITION 40.
           DISPLAY " 318 - Alta de Precios y Stock" LINE 10 POSITION 40.
           DISPLAY " 319 - Baja de Stock" LINE 11 POSITION 40.
           DISPLAY " 320 - Informe de Ventas" LINE 12 POSITION 40.
           DISPLAY " 321 - Informe de Ventas p/Compr." LINE 13
                                POSITION 40.
           DISPLAY " 322 - Informe de Ventas por dia" LINE 14 
                                POSITION 40.
           DISPLAY " 323 - Recepci¢n de Leche" LINE 15 POSITION 40.
      *     DISPLAY " 324 - " LINE 16 POSITION 40.
      *     DISPLAY " 325 - " LINE 17 POSITION 40.
      *     DISPLAY " 326 - " LINE 18 POSITION 40.
           DISPLAY " 327 - Lista Precios" LINE 19 POSITION 40.
           DISPLAY " 328 - Lista Stock Valorizado" LINE 20 POSITION 40.
           DISPLAY " 329 - Lista Caja" LINE 21 POSITION 40. 
      *=-=-=-=-=-=- MENSAJE DE ESCAPE =-=-=-=-=-=-=-
           DISPLAY " Esc - Retorna al MENU PRINCIPAL" LINE 22
                     POSITION 40.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           MOVE " INGRESE CODIGO" TO MEN-1.
           MOVE " DE TAREA :    " TO MEN-2.
           MOVE "   ? " TO MEN-3.
           PERFORM MENS-PAN.
       CARGUE-NUM-1.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.Š           IF ESCAP STOP RUN.
           IF NUM-CAP < 301 GO TO CARGUE-NUM-1.
           IF NUM-CAP > 329 GO TO CARGUE-NUM-1.
           IF NUM-CAP = 301 
      *          PERFORM NO-HABI        
                CALL "MARCAS.COB"
                CANCEL "MARCAS.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 302 
      *          PERFORM NO-HABI
                CALL "NUMERO.COB"
                CANCEL "NUMERO.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 303 
      *         PERFORM NO-HABI
               CALL "LINEAS.COB"
               CANCEL "LINEAS.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 304 
      *         PERFORM NO-HABI
               CALL "ENVASE.COB"
               CANCEL "ENVASE.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 305 
      *         PERFORM NO-HABI
               CALL "ART01.COB"
               CANCEL "ART01.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 306 
      *         PERFORM NO-HABI
               CALL "ART02.COB"
               CANCEL "ART02.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 307 
      *         PERFORM NO-HABI
               CALL "CONDI.COB"
               CANCEL "CONDI.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 308 
      *         PERFORM NO-HABI
               CALL "COTIZA.COB"
               CANCEL "COTIZA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 309 
      *         PERFORM NO-HABI
               CALL "COMPRO.COB"
               CANCEL "COMPRO.COB"
               GO TO INI-MOD.           
           IF NUM-CAP = 310 
      *          PERFORM NO-HABI
               CALL "MSTOCK.COB"
               CANCEL "MSTOCK.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 311 
      *               PERFORM NO-HABIŠ                CALL "PARTI.COB"
                CANCEL "PARTI.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 312 
      *         PERFORM NO-HABI
                CALL "CAMPAR.COB"
                CANCEL "CAMPAR.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 313 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 314 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 315 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 316 
      *         PERFORM NO-HABI
               CALL "FACINDU.COB"
               CANCEL "FACINDU.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 317 
      *         PERFORM NO-HABI
               CALL "ESTQUE.COB"
               CANCEL "ESTQUE.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 318 
      *         PERFORM NO-HABI
               CALL "CAPPRE.COB"
               CANCEL "CAPPRE.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 319 
      *         PERFORM NO-HABI
               CALL "BSTOCK.COB"
               CANCEL "BSTOCK.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 320 
      *         PERFORM NO-HABI
               CALL "INFOPE1.COB"
               CANCEL "INFOPE1.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 321 
      *         PERFORM NO-HABI
               CALL "INFOPE2.COB"
               CANCEL "INFOPE2.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 322 
      *               PERFORM NO-HABIŠ               CALL "INFOPE3.COB"
               CANCEL "INFOPE3.COB "
               GO TO INI-MOD.
           IF NUM-CAP = 323 
      *         PERFORM NO-HABI
               CALL "LECHE.COB"
               CANCEL "LECHE.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 324 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 325 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 326 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 327 
      *         PERFORM NO-HABI
               CALL "LISPRE82.COB"
               CANCEL "LISPRE82.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 328 
      *         PERFORM NO-HABI
               CALL "VSTOCK"
               CANCEL "VSTOCK"
               GO TO INI-MOD.
           IF NUM-CAP = 329 
      *               PERFORM NO-HABI
               CALL "LIS-CAJA.COB"
               CANCEL "LIS-CAJA.COB"
               GO TO INI-MOD.
           GO TO CARGUE-NUM-1.
       NO-HABI.
           DISPLAY SPACES LINE 25 POSITION 1 SIZE 50.
           MOVE " *** CODIGO NO " TO MEN-1
           MOVE "HABILITADO *** " TO MEN-2
           MOVE SPACES TO MEN-3.
           PERFORM MENS-PAN 70 TIMES.
       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 253003 TO TAB-POS(4).
           MOVE 092102 TO TAB-POS(5).
           MOVE 102130 TO TAB-POS(6).
           MOVE 112130 TO TAB-POS(7).
           MOVE 117104 TO TAB-POS(8).
           MOVE 132103 TO TAB-POS(9).
           MOVE 142103 TO TAB-POS(10).
           MOVE 152103 TO TAB-POS(11).
           MOVE 162130 TO TAB-POS(12).
           MOVE 182115 TO TAB-POS(13).
           MOVE 185615 TO TAB-POS(14).
           MOVE 192115 TO TAB-POS(15).
           MOVE 195615 TO TAB-POS(16).
           MOVE 202115 TO TAB-POS(17).
           MOVE 205615 TO TAB-POS(18).
           MOVE 222102 TO TAB-POS(19).
           MOVE 225602 TO TAB-POS(20).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE "       GES" TO TIT-1-PAN.
           MOVE "TION COMER" TO TIT-2-PAN.
           MOVE "CIAL      " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ALTA,BAJA Y MOD." LINE 14 POSITION 18.
           DISPLAY "      DE CLAVES " LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 16 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-P-CAP.
           MOVE 4 TO SW3.
           PERFORM BORRA.
      * PANTA-1.
      *     ...... DETALLAR PANTALLAS.........
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
  