       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU4.
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
           MOVE "M4MODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
      *................................(PROGRAMA PRINCIPAL)
       ACTU.
           MOVE " - MENU DE TAREAS - " TO D20-CAP.
           MOVE 0 TO CODI.
       PANTA-1.
           PERFORM BORRA-PANTA.
           DISPLAY " <<< CODIGOS  DE  TAREAS >>> " LINE 6 POSITION 25
                           REVERSE.
           DISPLAY " 401 - Mant. C¢digo Postal " LINE 8 POSITION 5.
           DISPLAY " 402 - Mant.de Actividad" LINE 9 POSITION 5.
           DISPLAY " 403 - Mant.de Empresa" LINE 10 POSITION 5.
           DISPLAY " 404 - Mant.de Al¡cuota" LINE 11 POSITION 5.
           DISPLAY " 405 - Captura de Cuentas" LINE 12 POSITION 5.
           DISPLAY " 406 - Captura de I.V.A." LINE 13 POSITION 5.
           DISPLAY " 407 - Listados Libros" LINE 14 POSITION 5.
           DISPLAY " 408 - Listados Impuestos-Tasas" LINE 15 POSITION 5.
           DISPLAY " 409 - Listado Form.468/b." LINE 16 POSITION 5.
           DISPLAY " 410 - List.Resoluci¢n 127/93." LINE 17 POSITION 5.
           DISPLAY " 411 - List.de Persepcion DGR" LINE 18 POSITION 5.
           DISPLAY " 412 - List.DDJJ Persepcion DGR" LINE 19 POSITION 5.
      *     DISPLAY " 413 - Mant.de Empresas" LINE 20 POSITION 5.
      *     DISPLAY " 414 - C lculo  de intereses" LINE 21 POSITION 5.
      *     DISPLAY " 415 - " LINE 22 POSITION 5. 
      *=-=-=-=-=-= SEGUNDA PANTALLA =-=-=-=-=-=-=-=-
      *     DISPLAY " 416 - " LINE 8 POSITION 40.
      *     DISPLAY " 417 - " LINE 9 POSITION 40.
      *     DISPLAY " 418 - " LINE 10 POSITION 40.
      *     DISPLAY " 419 - " LINE 11 POSITION 40.
      *     DISPLAY " 420 - " LINE 12 POSITION 40.
      *     DISPLAY " 421 - " LINE 13 POSITION 40.
      *     DISPLAY " 422 - " LINE 14 POSITION 40.
      *     DISPLAY " 423 - " LINE 15 POSITION 40.
      *     DISPLAY " 424 - " LINE 16 POSITION 40.
      *     DISPLAY " 425 - " LINE 17 POSITION 40.
      *     DISPLAY " 426 - " LINE 18 POSITION 40.
      *     DISPLAY " 427 - " LINE 19 POSITION 40.
      *     DISPLAY " 428 - " LINE 20 POSITION 40.
      *     DISPLAY " 429 - " LINE 21 POSITION 40. 
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
           PERFORM CAP-NUM.
           IF ESCAP STOP RUN.
           IF NUM-CAP < 401 GO TO CARGUE-NUM-1.
           IF NUM-CAP > 429 GO TO CARGUE-NUM-1.
           IF NUM-CAP = 401 
      *          PERFORM NO-HABI        
                CALL "CPOSTAL.COB"
                CANCEL "CPOSTAL.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 402 
      *          PERFORM NO-HABI
                CALL "ACTI.COB"
                CANCEL "ACTI.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 403 
      *         PERFORM NO-HABI
               CALL "EMPRESA.COB"
               CANCEL "EMPRESA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 404 
      *         PERFORM NO-HABI
               CALL "ALICUO.COB"
               CANCEL "ALICUO.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 405 
      *         PERFORM NO-HABI
               CALL "CUENTA.COB"
               CANCEL "CUENTA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 406 
      *         PERFORM NO-HABI
               CALL "CAPIVA.COB"
               CANCEL "CAPIVA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 407 
      *         PERFORM NO-HABI
               CALL "LIBIVA.COB"
               CANCEL "LIBIVA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 408 
      *         PERFORM NO-HABI
               CALL "LISIMP.COB"
               CANCEL "LISIMP.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 409 
      *         PERFORM NO-HABI
               CALL "ALIIVA.COB"    
               CANCEL "ALIIVA.COB"
               CALL "f468b.cob"
               CANCEL "f468b.cob"
               GO TO INI-MOD.           
           IF NUM-CAP = 410 
      *         PERFORM NO-HABI
               CALL "LISRES.COB"
               CANCEL "LISRES.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 411 
      *         PERFORM NO-HABI
                CALL "LISRES.COB"
                CANCEL "LISRES.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 412 
      *         PERFORM NO-HABI
                CALL "DDJJ127.COB"
                CANCEL "DDJJ127.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 413 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 414 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 415 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 416 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 417 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 418 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 419 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 420 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 421 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 422 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 423 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 424 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 425 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 426 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 427 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 428 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 429 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
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
           MOVE "LIQUIDACIO" TO TIT-1-PAN.
           MOVE "N DE IMPUE" TO TIT-2-PAN.
           MOVE "TOS/TASAS " TO TIT-3-PAN.
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
  