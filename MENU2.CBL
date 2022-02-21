       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU2.
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
           MOVE "M2MODA" TO NOM-PAN.
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
           DISPLAY " 201 - Mant.de Cuentas " LINE 8 POSITION 5.
           DISPLAY " 202 - Mant.de Cod.Postal" LINE 9 POSITION 5.
           DISPLAY " 203 - Mant.de Condiciones" LINE 10 POSITION 5.
           DISPLAY " 204 - Captura de Movimientos" LINE 11 POSITION 5.
           DISPLAY " 205 - Consulta de Movimientos" LINE 12 POSITION 5.
           DISPLAY " 206 - Consulta de Pendientes" LINE 13 POSITION 5.
           DISPLAY " 207 - Emisi¢n Recibos Cobros" LINE 14 POSITION 5.
           DISPLAY " 208 - Emisi¢n Recidos Pago" LINE 15 POSITION 5.
           DISPLAY " 209 - Listado de Saldos" LINE 16 POSITION 5.
           DISPLAY " 210 - Listado Resumen de Cta." LINE 17 POSITION 5.
           DISPLAY " 211 - List.Resumen de Tamberos" LINE 18 POSITION 5.
           DISPLAY " 212 - List.Resumen Anticipos" LINE 19 POSITION 5.
      *     DISPLAY " 213 - Mant.de Empresas" LINE 20 POSITION 5.
      *     DISPLAY " 214 - C lculo  de intereses" LINE 21 POSITION 5.
      *     DISPLAY " 215 - " LINE 22 POSITION 5. 
      *=-=-=-=-=-= SEGUNDA PANTALLA =-=-=-=-=-=-=-=-
           DISPLAY " 216 - Cierre Peri¢dico" LINE 8 POSITION 40.
      *     DISPLAY " 217 - " LINE 9 POSITION 40.
      *     DISPLAY " 218 - " LINE 10 POSITION 40.
      *     DISPLAY " 219 - " LINE 11 POSITION 40.
      *     DISPLAY " 220 - " LINE 12 POSITION 40.
      *     DISPLAY " 221 - " LINE 13 POSITION 40.
      *     DISPLAY " 222 - " LINE 14 POSITION 40.
      *     DISPLAY " 223 - " LINE 15 POSITION 40.
      *     DISPLAY " 224 - " LINE 16 POSITION 40.
      *     DISPLAY " 225 - " LINE 17 POSITION 40.
      *     DISPLAY " 226 - " LINE 18 POSITION 40.
      *     DISPLAY " 227 - " LINE 19 POSITION 40.
      *     DISPLAY " 228 - " LINE 20 POSITION 40.
      *     DISPLAY " 229 - " LINE 21 POSITION 40. 
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
           IF NUM-CAP < 201 GO TO CARGUE-NUM-1.
           IF NUM-CAP > 229 GO TO CARGUE-NUM-1.
           IF NUM-CAP = 201 
      *          PERFORM NO-HABI        
                CALL "CUENTA.COB"
                CANCEL "CUENTA.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 202 
      *          PERFORM NO-HABI
                CALL "CPOSTAL.COB"
                CANCEL "CPOSTAL.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 203 
      *         PERFORM NO-HABI
               CALL "CONDI.COB"
               CANCEL "CONDI.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 204 
      *         PERFORM NO-HABI
               CALL "CAPMOV.COB"
               CANCEL "CAPMOV.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 205 
      *         PERFORM NO-HABI
               CALL "CONMOV.COB"
               CANCEL "CONMOV.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 206 
      *         PERFORM NO-HABI
               CALL "CON-PEN.COB"
               CANCEL "CON-PEN.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 207 
      *         PERFORM NO-HABI
               CALL "RECIBO.COB"
               CANCEL "RECIBO.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 208 
      *         PERFORM NO-HABI
               CALL "RECPAG1.COB"
               CANCEL "RECPAG1.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 209 
      *         PERFORM NO-HABI
               CALL "SALDOS.COB"
               CANCEL "SALDOS.COB"
               GO TO INI-MOD.           
           IF NUM-CAP = 210 
      *          PERFORM NO-HABI
               CALL "RESDEBIT.COB"
               CANCEL "RESDEBIT.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 211 
      *         PERFORM NO-HABI
                CALL "RESLIQ.COB"
                CANCEL "RESLIQ.COB"
                GO TO INI-MOD.
           IF NUM-CAP = 212 
      *         PERFORM NO-HABI
               CALL "RESANT.COB"
               CANCEL "RESANT.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 213 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 214 
               PERFORM NO-HABI
      *          CALL "..."
      *          CANCEL "..."
               GO TO INI-MOD.
           IF NUM-CAP = 215 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 216 
      *         PERFORM NO-HABI
               CALL "CIERRE.COB"
               CANCEL "CIERRE.COB"
               GO TO INI-MOD.
           IF NUM-CAP = 217 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 218 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 219 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 220 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 221 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 222 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 223 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 224 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 225 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 226 
               PERFORM NO-HABI
      *         CALL " ... "
      *         CANCEL " ... "
               GO TO INI-MOD.
           IF NUM-CAP = 227 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 228 
               PERFORM NO-HABI
      *         CALL ""
      *         CANCEL ""
               GO TO INI-MOD.
           IF NUM-CAP = 229 
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
           MOVE "       CUE" TO TIT-1-PAN.
           MOVE "NTAS CORRI" TO TIT-2-PAN.
           MOVE "ENTES     " TO TIT-3-PAN.
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
  