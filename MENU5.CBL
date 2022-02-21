       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU5.
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
           MOVE "M5MODA" TO NOM-PAN.
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
           DISPLAY " 501 - Mant. de Bancos " LINE 8 POSITION 5.
           DISPLAY " 502 - Consulta de Bancos" LINE 9 POSITION 5.
           DISPLAY " 503 - Mant.de Cartera" LINE 10 POSITION 5.
           DISPLAY " 504 - Consulta de Cartera" LINE 11 POSITION 5.
           DISPLAY " 505 - Listado de Cartera" LINE 12 POSITION 5.
           DISPLAY " 506 - Entrega de Valores" LINE 13 POSITION 5.
           DISPLAY " 507 - Consulta de Entregas" LINE 14 POSITION 5.
           DISPLAY " 508 - Listado de Entregas" LINE 15 POSITION 5.
      *     DISPLAY " 509 - Listado Form.468/b." LINE 16 POSITION 5.
      *     DISPLAY " 510 - List.Resoluci¢n 127/93." LINE 17 POSITION 5.
      *     DISPLAY " 511 - List.de Persepcion DGR" LINE 18 POSITION 5.
      *     DISPLAY " 512 - List.DDJJ Persepcion DGR" LINE 19 POSITION 5.
      *     DISPLAY " 513 - Mant.de Empresas" LINE 20 POSITION 5.
      *     DISPLAY " 514 - C lculo  de intereses" LINE 21 POSITION 5.
      *     DISPLAY " 515 - " LINE 22 POSITION 5. 
      *=-=-=-=-=-= SEGUNDA PANTALLA =-=-=-=-=-=-=-=-
      *     DISPLAY " 516 - " LINE 8 POSITION 40.
      *     DISPLAY " 517 - " LINE 9 POSITION 40.
      *     DISPLAY " 518 - " LINE 10 POSITION 40.
      *     DISPLAY " 519 - " LINE 11 POSITION 40.
      *     DISPLAY " 520 - " LINE 12 POSITION 40.
      *     DISPLAY " 521 - " LINE 13 POSITION 40.
      *     DISPLAY " 522 - " LINE 14 POSITION 40.
      *     DISPLAY " 523 - " LINE 15 POSITION 40.
      *     DISPLAY " 524 - " LINE 16 POSITION 40.
      *     DISPLAY " 525 - " LINE 17 POSITION 40.
      *     DISPLAY " 526 - " LINE 18 POSITION 40.
      *     DISPLAY " 527 - " LINE 19 POSITION 40.
      *     DISPLAY " 528 - " LINE 20 POSITION 40.
      *     DISPLAY " 529 - " LINE 21 POSITION 40. 
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
           IF NUM-CAP < 501 GO TO CARGUE-NUM-1.
           IF NUM-CAP > 529 GO TO CARGUE-NUM-1.
           IF NUM-CAP = 501 
      *          PERFORM NO-HABI        
                CALL "BCO.COB"
                CANCEL "BCO.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 502 
      *          PERFORM NO-HABI
                CALL "CBCO.COB"
                CANCEL "CBCO.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 503 
      *         PERFORM NO-HABI
               CALL "CHEQUES.COB"
               CANCEL "CHEQUES.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 504 
      *         PERFORM NO-HABI
               CALL "CCARTERA.COB"
               CANCEL "CCARTERA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 505 
      *         PERFORM NO-HABI
               CALL "LCARTERA.COB"
               CANCEL "LCARTERA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 506 
      *         PERFORM NO-HABI
               CALL "ENTREGA.COB"
               CANCEL "ENTREGA.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 507 
      *         PERFORM NO-HABI
               CALL "CENT-A.COB"
               CANCEL "CENT-A.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 508 
      *         PERFORM NO-HABI
               CALL "LENT-A.COB"
               CANCEL "LENT-A.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 509 
               PERFORM NO-HABI
      *         CALL "ALIIVA.COB"    
      *         CANCEL "ALIIVA.COB"
      *         CALL "f468b.cob"
      *         CANCEL "f468b.cob"
               GO TO INI-MOD.           
           IF NUM-CAP = 510 
               PERFORM NO-HABI
      *         CALL "LISRES.COB"
      *         CANCEL "LISRES.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 511 
               PERFORM NO-HABI
      *          CALL "LISRES.COB"
      *          CANCEL "LISRES.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 512 
               PERFORM NO-HABI
      *          CALL "DDJJ127.COB"
      *          CANCEL "DDJJ127.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 513 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 514 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 515 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 516 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 517 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 518 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 519 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 520 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 521 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 522 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 523 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 524 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 525 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 526 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 527 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 528 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 529 
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
           MOVE "    - CART" TO TIT-1-PAN.
           MOVE "ERA DE CHE" TO TIT-2-PAN.
           MOVE "QUES -    " TO TIT-3-PAN.
       OPC-MODA.
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
  