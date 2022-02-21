       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECPAG1.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. JUL'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CUENTA.SEL".
      * COPY "CS.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "MOVI.SEL".
       COPY "P-CTAS.SEL".
       COPY "COMPRO.SEL".
       COPY "CPOSTAL.SEL".
       COPY "BCO.SEL".
       COPY "CHEQUES.SEL".
       COPY "LACAJA.SEL".
       COPY "PENDI.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "CUENTABM.REG".
      * COPY "CS.REG".
       COPY "EMPREBM.REG".
       COPY "CPOSTAL.REG".
       COPY "IMP02.REG".
       COPY "P-CTAS.REG".
       COPY "MOVI.REG".
       COPY "COMPRO1.REG".
       COPY "PENDI.REG".
       COPY "BCO.REG".
       COPY "CHEQUES.REG".
       COPY "LACAJA.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA         PIC 99 COMP-4 VALUE 15.
          02 LIN-1.
             03 FILLER     PIC X(70) VALUE SPACES.
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X(12) VALUE "RECIBO Nro:".
             03 COMPRO-ED  PIC 99999999.
             03 FILLER     PIC 99 COMP-4 VALUE 20.
          02 LIN-01.
             03 FILLER     PIC X(50) VALUE SPACES.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(70) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-2.
             03 FILLER     PIC X(50) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
             03 FILLER     PIC X(19) VALUE "    RECIBI/MOS DE:".
             03 NOMBRE-ED  PIC X(30).
             03 FILLER     PIC XX VALUE " (".
             03 CUENTA-ED  PIC ZZZZZZ.
             03 FILLER     PIC X VALUE ")".
             03 FILLER     PIC X(12) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
          02 LIN-3.
             03 FILLER     PIC X(50) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "|".
             03 DIRE-ED    PIC X(30).
             03 FILLER     PIC X(21) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
          02 LIN-4.
             03 FILLER     PIC X(50) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "|".
             03 COD-L-ED   PIC 9999.
             03 FILLER     PIC XXX VALUE " -".
             03 NOM-L-ED   PIC X(30).
             03 FILLER     PIC X(14) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
          02 LIN-5.
             03 FILLER     PIC X(50) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "|".
             03 FILLER     PIC X(10) VALUE "TELEFONO: ".
             03 TEL-ED     PIC X(20).
             03 FILLER     PIC X(21) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
          02 LIN-51.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(11) VALUE ALL "-".
             03 FILLER     PIC X(16) VALUE "ESTADO DE CUENTA".
             03 FILLER     PIC X(11) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(10) VALUE SPACES.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(70) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-6.
             03 FILLER     PIC X VALUE "|".
             03 FILLER     PIC X(19) VALUE "   SALDO ANT.   :$".
             03 ANT-SAL-ED   PIC ----.---.--9,99.
             03 FILLER     PIC X(4) VALUE SPACES.
             03 FILLER     PIC X VALUE "|".
             03 FILLER     PIC X(10) VALUE SPACES.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(70) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*". 
          02 LIN-7.
             03 FILLER     PIC X VALUE "|".
             03 FILLER     PIC X(19) VALUE "   MENOS PAGO   :$".
             03 PAGO-ED     PIC ZZZZ.ZZZ.ZZ9,99.
             03 FILLER     PIC X(5) VALUE "    |".
             03 FILLER     PIC X(10) VALUE SPACES.
             03 FILLER     PIC X(5) VALUE "|".
             03 FILLER     PIC X(15) VALUE "LA CANTIDAD DE".
             03 SALIDA-1-ED   PIC X(50).
             03 FILLER     PIC XX VALUE " |".
          02 LIN-8.
             03 FILLER     PIC X(20) VALUE "|".
             03 FILLER     PIC X(15) VALUE ALL "-".
             03 FILLER     PIC X(5) VALUE "    |".
             03 FILLER     PIC X(10) VALUE SPACES.
             03 FILLER     PIC X(5) VALUE "|".
             03 FILLER     PIC X(15) VALUE SPACES. 
             03 SALIDA-2-ED   PIC X(50).
             03 FILLER     PIC X(2) VALUE " |".
          02 LIN-9.
             03 FILLER     PIC X(20) VALUE "|   SALDO ACTUAL :$".
             03 ACT-SAL-ED PIC ----.---.--9,99.
             03 FILLER     PIC X(5) VALUE "    |".
             03 FILLER     PIC X(10) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "|".     
             03 SALIDA-3-ED   PIC X(50).
             03 FILLER     PIC X(2) VALUE " |".
          02 LIN-91.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(38) VALUE ALL "-".
             03 FILLER     PIC X(11) VALUE "*".
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(70) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-92.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(120) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-10.
             03 FILLER     PIC X(3) VALUE "|".
             03 FILLER     PIC X(16) VALUE "EN CONCEPTO DE:".
             03 COM-1-ED   PIC X(50).
             03 COM-2-ED   PIC X(50).
             03 FILLER     PIC X(3) VALUE "  |".
          02 LIN-11.
             03 FILLER     PIC X(3) VALUE "|".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 COM-3-ED   PIC X(50).
             03 COM-4-ED   PIC X(50).
             03 FILLER     PIC X(3) VALUE "  |".
          02 LIN-111.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(120) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-112.
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(24) VALUE ALL "-".
             03 FILLER     PIC X VALUE "*".
          02 LIN-12.
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X VALUE "|".
             03 FILLER     PIC X(6) VALUE "SON $ ".
             03 TOT-GRAL-ED  PIC ***.***.***.**9,99.
             03 FILLER     PIC X VALUE "|". 
             03 FILLER     PIC 99 COMP-4 VALUE 20.
             03 FILLER     PIC X(20) VALUE SPACES.
             03 LOC-EMP-ED PIC X(15).
             03 FILLER     PIC X VALUE ",".
             03 MES-ED     PIC X(10).
             03 FILLER     PIC X VALUE SPACES.
             03 DIA-ED     PIC 99B.
             03 FILLER     PIC X(3) VALUE "de ".
             03 ANO-ED     PIC 9999.
             03 FILLER     PIC XX VALUE ".-". 
          02 LIN-121.
             03 FILLER     PIC 99 COMP-4 VALUE 14.
             03 FILLER     PIC X VALUE "*".
             03 FILLER     PIC X(24) VALUE ALL "-".
             03 FILLER     PIC X VALUE "-".
          02 LIN-122.
             03 FILLER     PIC X(11) VALUE "CTA. Nro : ".
             03 CTA-NRO-ED  PIC ZZZZZ9.
             03 FILLER      PIC X(3) VALUE " - ".
             03 CTA-NOM-ED  PIC X(20).
          02 LIN-130.
             03 FILLER     PIC 99 COMP-4 VALUE 20.
             03 FILLER     PIC X(20) VALUE "Dejo constancia que ".
             03 FILLER     PIC X(20) VALUE "la acreditacion es -".
             03 FILLER     PIC X(20) VALUE "PROVISORIA- hasta ".
          02 LIN-131.
             03 FILLER     PIC X(20) VALUE "que se haga efectivo".
             03 FILLER     PIC X(20) VALUE " el valor detallado,".
             03 FILLER     PIC X(20) VALUE " siendo responsable ".
          02 LIN-132.
             03 FILLER     PIC X(20) VALUE "de su devolucion en ".
             03 FILLER     PIC X(20) VALUE "EFECTIVO si el posee".
             03 FILLER     PIC X(20) VALUE "dor tuviera problema".
          02 LIN-133.
             03 FILLER     PIC X(20) VALUE "para su cobro total.".
             03 FILLER     PIC X(20) VALUE "Reembolsando los gas".
             03 FILLER     PIC X(20) VALUE "tos ocasionados.-   ".
          02 LIN-134.
             03 FILLER     PIC X(40) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE ALL ".".
          02 LIN-135.
             03 FILLER     PIC X(30) VALUE SPACES.
             03 FILLER     PIC X(11) VALUE "Aclaracion:".         
       01 CAPTURA.
          02 CUENTA-CAP        PIC 9(6) VALUE 0.
          02 SUB-CUENTA-CAP    PIC 9(4) VALUE 0.
          02 RUBRO-CAP         PIC 9(6) VALUE 0.
          02 NOMBRE-CAP        PIC X(45).
          02 ESTADO-CAP        PIC XX.
          02 CONDICION-CAP     PIC 999 VALUE 0.
          02 DIRE-CAP          PIC X(30).
          02 COD-LOCA-CAP      PIC 9(4) VALUE 0.
          02 INSCRIPTO-CAP     PIC X.
          02 ZONA-CAP          PIC 99 VALUE 0.
          02 IGAN-CAP          PIC X(15).
          02 SALDO-ANT-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-CAN-CAP     PIC S9(13)V99 VALUE 0.
          02 SALDO-TOP-CAP     PIC S9(13)V99 VALUE 0. 
          02 SALDO-CAP         PIC S9(13)V99 VALUE 0.
          02 TEL-CAP           PIC X(10).
          02 PMOV-CAP          PIC 9(6) VALUE 0.
          02 PPEND-CAP         PIC 9(6) VALUE 0.
          02 ING-BRUTOS-CAP    PIC 9(8) VALUE 0.
          02 COM-1-CAP         PIC X(50).
          02 COM-2-CAP         PIC X(50).
          02 COM-3-CAP         PIC X(50).
          02 COM-4-CAP         PIC X(50).
          02 IMP-CAP           PIC 9(13)V99 VALUE 0.
          02 SALDO-CON         PIC 9(13)V99 VALUE 0.
          02 IMPORTE-I-PEND    PIC 9(13)V99 VALUE 0.
          02 IMPORTE-I-MOV     PIC 9(13)V99 VALUE 0.
          02 ACUENTA-I-PEND    PIC 9(13)V99 VALUE 0.
          02 AYUDA             PIC 9(13)V99 VALUE 0.
          02 C-VAL             PIC 9 VALUE 0.
      *-=-=-=-=-=- AGREGADO POR CAJA =-=-=-=-=-=-=-=-=
          02 IMP-P-CAP         PIC 9(13)V99 VALUE 0.
          02 BCO-CAP           PIC 999 VALUE 0.
          02 CH-CPOSTAL-CAP    PIC 9(4) VALUE 0.
          02 CH-NUM-CAP        PIC 9(10) VALUE 0.
          02 CH-FEC-VENC-CAP.
             03 CH-DIA-VENC-CAP   PIC 99 VALUE 0.
             03 CH-MES-VENC-CAP   PIC 99 VALUE 0.
             03 CH-ANO-VENC-CAP   PIC 9999 VALUE 0.
          02 TIPO-CAP        PIC X VALUE SPACES.
          02 CONT-CH-CAP         PIC 9 VALUE 0.
          02 IMP-P-ED            PIC ZZZZ.ZZZ.ZZZ.ZZ9,99.
          02 CH-FEC-VENC-ED.
             03 CH-DIA-VENC-ED   PIC 99/.
             03 CH-MES-VENC-ED   PIC 99/.
             03 CH-ANO-VENC-ED   PIC 9999.
          02 DATOS-CHEQUE.
             03 BCO-D-ED         PIC 9(4).
             03 FILLER           PIC X VALUE " ".
             03 BCO-NOM-ED       PIC X(15).
             03 FILLER           PIC XXX VALUE " -".
             03 CH-CPOSTAL-D-ED     PIC 9(4).
             03 FILLER           PIC X(7) VALUE " - N':".
             03 CH-NUM-D-ED      PIC 9(10).
          02 DATOS-CHEQUE1.
             03 FILLER           PIC X(9) VALUE "FECHA :".
             03 CH-FEC-VENC-D-ED.
                05 CH-DIA-VENC-D-ED  PIC 99.
                05 FILLER            PIC X VALUE "/".
                05 CH-MES-VENC-D-ED  PIC 99.
                05 FILLER            PIC X VALUE "/".
                05 CH-ANO-VENC-D-ED  PIC 9999.
          02 DATOS-CHEQUE2.
             03 BCO-1-ED       PIC 9999.
             03 FILLER         PIC X VALUE "-".
             03 CH-CPOSTAL-1-ED  PIC 9(4).
             03 FILLER         PIC X VALUE "-".
             03 CH-NUM-1-ED    PIC 9(10).
             03 FILLER         PIC X VALUE "-".
             03 FEC-1-ED.
                05 DIA-1-ED    PIC 99.
                05 FILLER      PIC X VALUE "/".
                05 MES-1-ED    PIC 99.
                05 FILLER      PIC X VALUE "/".
                05 ANO-1-ED    PIC 9999.
          02 TURNO-CAP          PIC 99 VALUE 0.
          02 FEC-C-1.
             03 ANO-C-1         PIC 9999.
             03 MES-C-1         PIC 99.
             03 DIA-C-1         PIC 99.
          02 FEC-C-2.
             03 ANO-C-2         PIC 9999.
             03 MES-C-2         PIC 99.
             03 DIA-C-2         PIC 99.
          02 NRO-FACTURA        PIC 9(8).
          02 ANO-REC            PIC 9999.
          02 MES-REC            PIC 99.
          02 DIA-REC            PIC 99.
          02 NRO-CTA            PIC 9(8).
          02 IMP-FACTURA        PIC 9(8)V99.
          02 RUBRO              PIC 9(7).          
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       01 EDICION.
          02 CUENTA-EDI        PIC Z(6) BLANK WHEN ZERO.
          02 SUB-CUENTA-EDI    PIC ZZ BLANK WHEN ZERO.
          02 NOMBRE-EDI      PIC X(45).
          02 ESTADO-EDI        PIC XX.
          02 CONDICION-EDI     PIC ZZZ BLANK WHEN ZERO.
          02 DIRE-EDI          PIC X(30).
          02 LOCA-IDE          PIC X(29).
          02 INSCRIPTO-EDI     PIC X.
          02 ZONA-EDI          PIC ZZ BLANK WHEN ZERO.
          02 IGAN-EDI          PIC X(15).
          02 SAL-C-EDI         PIC ----.---.---.--9,99.
          02 SAL-EDI           PIC ----.---.---.--9,99.
          02 SAL-T-EDI         PIC ----.---.---.--9,99.
          02 TEL-EDI           PIC X(10).
          02 IMP-EDI           PIC ZZZZ.ZZZ.ZZZ.ZZ9,99.
       01 AUXILIARES.
          02 LISTO-N-D         PIC 999999 VALUE 0.
          02 LISTO-N-H         PIC 999999 VALUE 0.
          02 LISTO-Z           PIC 99.
          02 LISTO-A-D         PIC X(30).
          02 LISTO-A-H         PIC X(30).
          02 CONT-L            PIC 9(6) VALUE 0.
          02 ZONA-L            PIC 99 VALUE 0.
          02 RUBRO-L           PIC 9(7) VALUE 0.
          02 CUENTA-H          PIC 9(6) VALUE 0.
          02 SUB-CUENTA-H      PIC 9(4) VALUE 0.
          02 NOMBRE-H          PIC X(30).
       COPY "AUSTRAL.WOR".
       COPY "CLAVES.WOR".
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CUENTA.ERR".
       COPY "P-CTA.ERR".
       COPY "CPOSTAL.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       COPY "MOVI.ERR".
       COPY "PENDI.ERR".
       COPY "COMPRO1.ERR".
       COPY "LACAJA.ERR".
       COPY "CHEQUES.ERR".
       COPY "BCO.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "CCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE. 
       PANTA-INI. 
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-INI.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-CLAVE.
           IF CLAVE-1-CAP NOT = 0 GO TO SIGA-INI.
           PERFORM CLAVE-SYS.
           PERFORM BORRA-CLAVE.
       SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
       CARGO-MOD.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) 
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO TAREA1.
           GO TO CARGO-MOD.
       TAREA1.
           OPEN INPUT COMPRO CUENTAS P-DE-CTAS EMPRESAS MOVI CPOSTAL.
           OPEN OUTPUT IMP02.
           MOVE " EMISION REC.PAGO " TO D20-CAP.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           MOVE COD-LOC-EMP TO C-POS.
           MOVE REP-LOC-EMP TO C-POS-1
           READ CPOSTAL INVALID KEY MOVE "********" TO NOM-POSTAL.
           MOVE NOM-POSTAL TO LOC-EMP-ED.
           PERFORM CARGA-CAJA.
           PERFORM CARGA-RUB.
       TAREA1-1.
           PERFORM BORRA-PANTA-C.
           MOVE "E" TO TIPO-CAP.
      *-=-=-=-=-= AGREGADO POR CAJA -=-=-=-=--=-=-=-
           DISPLAY "-E- Efectivo -C- Cheque -T- Tarjeta"
                  LINE 15 POSITION 20 REVERSE.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           PERFORM CARGA-CUE THRU CARGA-CUE-F.
           IF CUENTA = 0  MOVE 0 TO SUB-CUENTA GO TO TAREA2-1.
           PERFORM CARGA-SUB-CUE.
           READ CUENTAS INVALID KEY GO TO TAREA1-1.
           GO TO ALTA.
       TAREA2-1.
           PERFORM CARGA-NOM.
           START CUENTAS KEY IS NOT < NOMBRE-C
                     INVALID KEY GO TO TAREA2-1.
       TAREA2-2.
           READ CUENTAS NEXT AT END GO TO TAREA2-1.
           PERFORM DESP-RUB.
           PERFORM DESP-CUE.
           PERFORM DESP-NOM THRU DESP-SAL-CAN.
           PERFORM MEN-CONF.
           MOVE 28 TO SW3.
           PERFORM CAP-COD1.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 GO TO ALTA.
           PERFORM TAREA2-2.
       ALTA.
           PERFORM DESP-NOM THRU DESP-SAL-CAN.
      *-=-=-=-=-=- AGREGADO POR CAJA =-=-=-=-=--=-=-=
           PERFORM CARGA-TIPO.
           IF TIPO-CAP = "e" OR TIPO-CAP = "E"
             DISPLAY "-E- Efectivo" LINE 15 POSITION 20 
                                    BLINK REVERSE
             GO TO ALTA1.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c"
             DISPLAY "-C- Cheque" LINE 15 POSITION 33 
                                  BLINK REVERSE
             GO TO CARGA-CHEQUE.
           IF TIPO-CAP = "t" OR TIPO-CAP = "T"
             DISPLAY "-T- Tarjeta" LINE 15 POSITION 44 
                                   BLINK REVERSE
             GO TO CARGA-CHEQUE.
           GO TO ALTA.
       ALTA1.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           PERFORM CARGA-IMP.
           PERFORM DESP-IMP.
           PERFORM CARGA-COM1 THRU CARGA-COM4.
      *-=-=-=-=-= AGREGADO POR CAJA --=-=-=-=--=-
       ALTA2.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           MOVE SALDO TO ANT-SAL-ED.
           MOVE IMP-CAP TO TOT-GRAL PAGO-ED TOT-GRAL-ED.
           COMPUTE SALDO = SALDO + IMP-CAP.
           IF SALDO-CAN < IMP-CAP
               MOVE 0 TO SALDO-CAN
               MOVE "N" TO ESTADO
           ELSE COMPUTE SALDO-CAN = SALDO-CAN + IMP-CAP.
      *=-=-=-=-=-= pasa a normal con saldo a cancelar -=-=-=-=-=-
           COMPUTE SALDO-CON ROUNDED = SALDO-TOP * 5 / 100.
           IF SALDO-CAN > 0
               IF SALDO-CAN < SALDO-CON
                  MOVE "N" TO ESTADO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           IF SALDO-CAN = 0
                 MOVE "N" TO ESTADO.           
           MOVE SALDO TO ACT-SAL-ED.
           COMPUTE PMOV = PMOV + 1
           PERFORM GRABA-MOVI.
      *-=-=-=-=-=-= AGREGADO POR CAJA =-=-=-=-=-=-===-=-
           IF TIPO-CAP = "C" OR TIPO-CAP = "c"
               PERFORM GRABA-CHEQUE.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
               PERFORM GRABA-CHEQUE.
      *=-=-=-=-=-=-=-= no graba -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

           IF CU-SALDO(4) NOT NUMERIC MOVE 0 TO CU-SALDO(4).
           MOVE IMP-CAP TO AYUDA

           IF CU-SALDO(4) > 0 
              IF AYUDA    > CU-SALDO(4)
                 COMPUTE  AYUDA = AYUDA - CU-SALDO(4)
                 MOVE 0 TO CU-SALDO(4)
                 GO TO VEA-OTRO-SALDO
               ELSE
                 COMPUTE CU-SALDO(4) = CU-SALDO(4) - IMP-CAP
                 GO TO SALE-DE-SALDOS.
       VEA-OTRO-SALDO.
           IF CU-SALDO(3) > 0 
              IF AYUDA    > CU-SALDO(3)
                 COMPUTE  AYUDA = AYUDA - CU-SALDO(3)
                 MOVE 0 TO CU-SALDO(3)
                 GO TO VEA-OTRO-SALDO1
               ELSE
                 COMPUTE CU-SALDO(3) = CU-SALDO(3) - AYUDA
                 GO TO SALE-DE-SALDOS.
       VEA-OTRO-SALDO1.
           IF CU-SALDO(2) > 0 
              IF AYUDA    > CU-SALDO(2)
                 COMPUTE  AYUDA = AYUDA - CU-SALDO(2)
                 MOVE 0 TO CU-SALDO(2)
                 GO TO VEA-OTRO-SALDO2
               ELSE
                 COMPUTE CU-SALDO(2) = CU-SALDO(2) - AYUDA
                 GO TO SALE-DE-SALDOS.
       VEA-OTRO-SALDO2.
           IF CU-SALDO(1) > 0 
              IF AYUDA    > CU-SALDO(1)
                 COMPUTE  AYUDA = AYUDA - CU-SALDO(1)
                 MOVE 0 TO CU-SALDO(1)
                 GO TO SALE-DE-SALDOS
               ELSE
                 COMPUTE CU-SALDO(1) = CU-SALDO(1) - AYUDA
                 GO TO SALE-DE-SALDOS.

       SALE-DE-SALDOS.
           PERFORM REGRABA-CUENTAS.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           PERFORM VEO-PENDI THRU VEO-PENDI-F.      
           PERFORM CONVERSOR THRU FIN-CONVERSOR.
           GO TO IMPRE.
      *-=-=-=-=-=-= AGREGADO POR CAJA =-=-=-=-=-=--=-
       CARGA-CHEQUE.
           PERFORM CARGA-IMP.
           PERFORM DESP-IMP.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
               PERFORM PANTA-3
             ELSE PERFORM PANTA-2.
           PERFORM CARGA-BCO THRU CARGA-FEC-VENC.
           PERFORM BORRA-PANTA-2.
           PERFORM CARGA-COM1 THRU CARGA-COM2.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE DATOS-CHEQUE TO COM-3-CAP.
           DISPLAY COM-3-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) BLINK REVERSE.
           MOVE DATOS-CHEQUE1 TO COM-4-CAP.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           DISPLAY COM-4-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                        SIZE TAB-SIZ(SW3) BLINK REVERSE.
           PERFORM MUEVO-CHEQUE.
       CARGA-CHE-1.
           MOVE 28 TO SW3.
           PERFORM MEN-CONF.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) 
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F01 GO TO ALTA2.
           IF F02 GO TO CARGA-CHEQUE.
           GO TO CARGA-CHE-1.
       MUEVO-CHEQUE.
           MOVE NUM-EMP TO CH-EMP.
           MOVE SUC-EMP TO CH-SUC.
           MOVE BCO-CAP TO CH-BCO CH-BCO1 CH-BCO2.
           MOVE CH-CPOSTAL-CAP TO CH-CPOSTAL CH-CPOSTAL1 CH-CPOSTAL2.
           MOVE CH-NUM-CAP TO CH-NUMERO-CH CH-NUMERO-CH1 
                CH-NRO-CHEQUE CH-NUM-CHEQUE.
           OPEN INPUT CHEQUES.
           READ CHEQUES INVALID KEY DISPLAY "NO LEI CHEQUE".
           CLOSE CHEQUES.
           MOVE DIA TO CH-DIA-ENT.
           MOVE MES TO CH-MES-ENT.
           MOVE ANO TO CH-ANO-ENT
             IF ANO = 99 ADD 1900 TO CH-ANO-ENT
                 ELSE ADD 2000 TO CH-ANO-ENT.
           MOVE NOMBRE-C TO CH-ENTREGADO-A 
           MOVE SPACES TO CH-CONCEPTO-ENT
                 CH-CON-GARANTIA CH-COMENTA.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c"
             MOVE DATOS-CHEQUE2 TO CH-CONCEPTO-ENT.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
             MOVE DATOS-CHEQUE2 TO CH-CONCEPTO-ENT.
           MOVE 0 TO CH-IMP-INTERES
                  CH-TASA-INTERES CH-MONEDA CH-COD-VENC.
       GRABA-CHEQUE.
           OPEN I-O CHEQUES.
           REWRITE REG-CHEQUES INVALID KEY DISPLAY "NO REGRABE CHEQUE".
           CLOSE CHEQUES.
       CARGA-BCO.
           MOVE 26 TO SW3.
           PERFORM BORRA.
           MOVE 46 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF F02 GO TO CARGA-CHEQUE.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-BCO.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
             IF NUM-CAP < 900 GO TO CARGA-BCO.
           MOVE NUM-CAP TO BCO-CAP BCO-NUMERO BCO-D-ED BCO-1-ED.
           DISPLAY BCO-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE 4.
           OPEN INPUT BCO.
           READ BCO INVALID KEY MOVE "BANCO INEXISTENTE" TO BCO-NOMBRE.
           CLOSE BCO.
           MOVE 26 TO SW3.
           DISPLAY BCO-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE BCO-NOMBRE TO BCO-NOM-ED.
       CARGA-CPOSTAL-BCO.
           MOVE 27 TO SW3.
           PERFORM BORRA.
           MOVE 47 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF F02 GO TO CARGA-BCO.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-CPOSTAL-BCO.
           MOVE NUM-CAP TO CH-CPOSTAL-CAP CH-CPOSTAL-D-ED 
                        CH-CPOSTAL-1-ED.
       CARGA-NUM-CH.
           MOVE 48 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF F02 GO TO CARGA-CPOSTAL-BCO.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           MOVE NUM-CAP TO CH-NUM-CAP CH-NUM-D-ED CH-NUM-1-ED.
       CARGA-FEC-VENC.
           MOVE 49 TO SW3.
           PERFORM BORRA.
           ACCEPT CH-FEC-VENC-CAP LINE TAB-LIN(SW3) 
               POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3) NO BEEP TAB
               ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F02 GO TO CARGA-NUM-CH.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF CH-FEC-VENC-CAP = 0
             MOVE DIA TO CH-DIA-VENC-CAP
             MOVE MES TO CH-MES-VENC-CAP
             MOVE ANO TO CH-ANO-VENC-CAP
                 IF ANO = 99 ADD 1900 TO CH-ANO-VENC-CAP
                    ELSE ADD 2000 TO CH-ANO-VENC-CAP.
           MOVE CH-DIA-VENC-CAP TO CH-DIA-VENC-D-ED CH-DIA-VENC-ED
                  DIA-1-ED.
           MOVE CH-MES-VENC-CAP TO CH-MES-VENC-D-ED CH-MES-VENC-ED
                  MES-1-ED.
           MOVE CH-ANO-VENC-CAP TO CH-ANO-VENC-D-ED CH-ANO-VENC-ED
                  ANO-1-ED.
           DISPLAY CH-FEC-VENC-ED LINE TAB-LIN(SW3) 
              POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
          
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       REGRABA-CUENTAS.
           CLOSE CUENTAS.
           OPEN I-O CUENTAS.
           REWRITE REG-CTAS INVALID KEY WRITE REG-CTAS.
           CLOSE CUENTAS.
           OPEN INPUT CUENTAS.
       GRABA-MOVI.
      *-=-=-=-=-=-=--= AGREGUE CAMPOS CAJA =-=-=-=--=-=-=-=-
           MOVE EMP-CTA TO EMP-MOV EMP-COMPRO EMP-PEND LAC-EMP.
           MOVE SUC-CTA TO SUC-EMP-MOV SUC-COMPRO SUC-EMP-PEND LAC-SUC.
           MOVE RUBRO-CTA TO RUBRO-MOV RUBRO-PEND LAC-RUBRO.
           MOVE CUENTA TO CUENTA-MOV CUENTA-ED CUENTA-PEND LAC-CUENTA.
           MOVE SUB-CUENTA TO SUB-CUENTA-MOV SUB-CUENTA-PEND 
                 LAC-SUB-CTA.
           MOVE TIPO-CAP TO LAC-TIPO.
           MOVE DIA TO DIA-MOV LAC-DIA.
           MOVE MES TO MES-MOV LAC-MES.
           MOVE ANO TO ANO-MOV
             IF ANO = 99 ADD 1900 TO ANO-MOV
               ELSE ADD 2000 TO ANO-MOV.
           MOVE ANO-MOV TO LAC-ANO.
           MOVE 1 TO TIPO-MOV.
           MOVE 3 TO TIPO-C-MOV NUM-COMP.
           MOVE SPACES TO SUB-T-C-MOV SUB-T-C-COMPRO.
           MOVE SUC-EMP TO SUC-C-MOV SUC-C-COMPRO.
      *=-=-=-= SOLO CASA CENTRAL =-=-=-=-=-====
           MOVE 0 TO SUC-C-MOV SUC-C-COMPRO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           READ COMPRO INVALID KEY EXIT PROGRAM
                                   STOP RUN.
           COMPUTE NUM-COMPRO = NUM-COMPRO + 1.
           CLOSE COMPRO.
           OPEN I-O COMPRO.
           REWRITE REG-COMPRO INVALID KEY WRITE REG-COMPRO.
           CLOSE COMPRO.
           OPEN INPUT COMPRO.
           MOVE NUM-COMPRO TO COMPRO-MOV COMPRO-ED LAC-COMPROB.
           MOVE 0 TO REMA-MOV.
           MOVE IMP-CAP TO IMPORTE-MOV.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
              MOVE BCO-CAP TO LAC-TARJ-UTI
              MOVE 0 TO LAC-IMP-CHEQ
              MOVE IMP-CAP TO LAC-IMPORTE
              MOVE 5 TO LAC-CONDI.
           IF TIPO-CAP = "e" OR TIPO-CAP = "E" 
             MOVE IMP-CAP TO LAC-IMPORTE
             MOVE 0 TO LAC-IMP-CHEQ LAC-TARJ-UTI
             MOVE 5 TO LAC-CONDI.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c"
             MOVE IMP-CAP TO LAC-IMP-CHEQ
             MOVE 0 TO LAC-IMPORTE LAC-TARJ-UTI
             MOVE 5 TO LAC-CONDI.
           MOVE TURNO-CAP TO LAC-TURNO.
           MOVE 0 TO IMP-EXT-MOV
           MOVE 2 TO LAC-SUMA-RESTA.
      *-=-=-=-=-=-=-AGREGADO POR CAJA -=-=-=--=-=-=-=-=-
      *     MOVE "SEGUN DETALLE" TO COMENTARIO-MOV.
           IF TIPO-CAP = "E" OR TIPO-CAP = "e"
             MOVE COM-1-CAP TO COMENTARIO-MOV.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c"
             MOVE DATOS-CHEQUE2 TO COMENTARIO-MOV.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t"
             MOVE DATOS-CHEQUE2 TO COMENTARIO-MOV.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- 
           MOVE SPACES TO SUCURSAL-MOV.
           MOVE 0 TO ASIENTO-MOV.
           MOVE PMOV TO MOVIMIENTO-MOV.
           CLOSE MOVI.
           OPEN I-O MOVI.
      *=-=-=-=-=-=-=- no graba =-=-=-=-=-=-=-=-=-=-=-=-
           WRITE REG-MOV INVALID KEY REWRITE REG-MOV.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
           CLOSE MOVI.
           OPEN INPUT MOVI.
      *-=-=-=--=-= AGREGADO POR CAJA -=-=-=-=-=-=-=
           OPEN I-O LACAJA.
           WRITE REG-LACAJA INVALID KEY DISPLAY "NO GRABA CAJA".
           CLOSE LACAJA.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       VEO-PENDI.
           OPEN I-O PENDI.
           MOVE 0 TO PENDIENTE-PEND DIA-PEND MES-PEND ANO-PEND.
           MOVE 0 TO IMPORTE-I-PEND ACUENTA-I-PEND.
           MOVE IMPORTE-MOV TO IMPORTE-I-MOV.
           MOVE CH-DIA-VENC-CAP TO DIA-C-1.
           MOVE CH-MES-VENC-CAP TO MES-C-1.
           MOVE CH-ANO-VENC-CAP TO ANO-C-1.
           MOVE DIA-MOV TO DIA-C-2.
           MOVE MES-MOV TO MES-C-2.
           MOVE ANO-MOV TO ANO-C-2.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c" 
              IF FEC-C-1 > FEC-C-2 GO TO NUEVO-PENDI.
           IF TIPO-CAP = "T" OR TIPO-CAP = "t" 
              IF FEC-C-1 > FEC-C-2 GO TO NUEVO-PENDI.
           START PENDI KEY IS NOT < CLAVE-PEND
                 INVALID KEY GO TO NUEVO-PENDI.
       ANALISO-PENDI.
           READ PENDI NEXT AT END GO TO  NUEVO-PENDI.
           MOVE 0 TO IMPORTE-I-PEND.
           IF EMP-PEND NOT = EMP-MOV GO TO  NUEVO-PENDI.
           IF SUC-EMP-MOV NOT = SUC-EMP-PEND GO TO NUEVO-PENDI.
           IF RUBRO-MOV NOT = RUBRO-PEND GO TO NUEVO-PENDI.
           IF CUENTA-MOV NOT = CUENTA-PEND GO TO NUEVO-PENDI.
      *     IF SUB-CUENTA-MOV NOT = SUB-CUENTA-PEND GO TO NUEVO-PENDI.
           IF TIPO-MOV = D-H-PEND GO TO ANALISO-PENDI.
           MOVE 0 TO IMPORTE-I-PEND.
           COMPUTE IMPORTE-I-PEND = IMPORTE-PEND - ACUENTA-PEND.
           IF IMPORTE-I-MOV > IMPORTE-I-PEND GO TO MENOR-PENDI.
           IF IMPORTE-I-MOV < IMPORTE-I-PEND GO TO MAYOR-PENDI.
           DELETE PENDI INVALID KEY DISPLAY "NO BORRE PENDI-111".
           CLOSE PENDI.
           GO TO VEO-PENDI-F.
       MAYOR-PENDI.
           COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-I-MOV. 
           REWRITE REG-PEND INVALID KEY 
                     DISPLAY "NO PUDE REGRABAR PENDI".
           CLOSE PENDI.
           GO TO VEO-PENDI-F.
       MENOR-PENDI.
           COMPUTE IMPORTE-I-MOV = IMPORTE-I-MOV - IMPORTE-I-PEND.
           DELETE PENDI INVALID KEY 
                    DISPLAY " NO PUDE BORRAR PENDI-22".
           GO TO ANALISO-PENDI.
       NUEVO-PENDI.
           MOVE EMP-MOV TO EMP-PEND.
           MOVE SUC-EMP-MOV TO SUC-EMP-PEND.
           MOVE RUBRO-MOV TO RUBRO-PEND.
           MOVE CUENTA-MOV TO CUENTA-PEND.
           MOVE SUB-CUENTA-MOV TO SUB-CUENTA-PEND.
           MOVE ANO-MOV TO ANO-PEND ANO-EMI-PEND.
           MOVE MES-MOV TO MES-PEND MES-EMI-PEND.
           MOVE DIA-MOV TO DIA-PEND DIA-EMI-PEND.
           IF FEC-C-1 > FEC-C-2
             MOVE ANO-C-1 TO ANO-PEND
             MOVE MES-C-1 TO MES-PEND
             MOVE DIA-C-1 TO DIA-PEND.
           MOVE MOVIMIENTO-MOV TO REMATE-PEND.
           MOVE TIPO-MOV TO D-H-PEND.
           MOVE TIPO-C-MOV TO TIPO-PEND.
           MOVE SUB-T-C-MOV TO SUB-T-PEND.
           MOVE SUC-C-MOV TO SUC-T-PEND.
           MOVE COMPRO-MOV TO COMPRO-PEND.
           MOVE IMPORTE-I-MOV TO IMPORTE-PEND.
           MOVE "SEGUN DETALLE" TO DETA-PEND.
           IF TIPO-CAP = "C" OR TIPO-CAP = "c" OR TIPO-CAP = "T"
                 OR TIPO-CAP = "t"
                 MOVE DATOS-CHEQUE2 TO DETA-PEND. 
           ADD 1 TO PPEND.
           MOVE PPEND TO PENDIENTE-PEND.
           MOVE 0 TO ACUENTA-PEND.
           WRITE REG-PEND INVALID KEY 
                      DISPLAY "NO PUDE GRABAR PENDI". 
           CLOSE PENDI.
           CLOSE CUENTAS.
           OPEN I-O CUENTAS.
           REWRITE REG-CTAS.
           CLOSE CUENTAS.
           OPEN INPUT CUENTAS.
           GO TO VEO-PENDI-F.
       VEO-PENDI-F.
           EXIT.             
       GRABA-CUENTAS.
           CLOSE CUENTAS.
           OPEN I-O CUENTAS.
           WRITE REG-CTAS INVALID KEY REWRITE REG-CTAS.
           CLOSE CUENTAS.
           OPEN INPUT CUENTAS.
           GO TO TAREA1-1.
       IMPRE.
           MOVE 0 TO CONT-R.
           MOVE SALIDA-1 TO SALIDA-1-ED.
           MOVE SALIDA-2 TO SALIDA-2-ED.
           MOVE SALIDA-3 TO SALIDA-3-ED.
           MOVE COM-1-CAP TO COM-1-ED.
           MOVE COM-2-CAP TO COM-2-ED.
           MOVE COM-3-CAP TO COM-3-ED.
           MOVE COM-4-CAP TO COM-4-ED.
           MOVE DIA TO DIA-ED.
           MOVE ANO-MOV TO ANO-ED.
           MOVE SPACES TO MES-ED.
           IF MES = 1 MOVE "ENERO" TO MES-ED.
           IF MES = 2 MOVE "FEBRERO" TO MES-ED.
           IF MES = 3 MOVE "MARZO" TO MES-ED.
           IF MES = 4 MOVE "ABRIL" TO MES-ED.
           IF MES = 5 MOVE "MAYO" TO MES-ED.
           IF MES = 6 MOVE "JUNIO" TO MES-ED.
           IF MES = 7 MOVE "JULIO" TO MES-ED.
           IF MES = 8 MOVE "AGOSTO" TO MES-ED.
           IF MES = 9 MOVE "SETIEMBRE" TO MES-ED.
           IF MES = 10 MOVE "OCTUBRE" TO MES-ED.
           IF MES = 11 MOVE "NOVIMBRE" TO MES-ED.
           IF MES = 12 MOVE "DICIEMBRE" TO MES-ED.
           MOVE NOMBRE-C TO CTA-NOM-ED.
           MOVE CUENTA TO CTA-NRO-ED.

           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM LIN-1 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-01 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-2 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-3 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-4 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-5 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-51 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-6 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-7 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-9 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-91 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-92 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-10 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-11 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-111 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM LIN-112 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-12 AFTER 1.
           WRITE LIN-IMP02 FROM LIN-121 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM LIN-122 AFTER 1.
           ADD 20 TO CONT-R.
           PERFORM ESPA 14 TIMES.
           MOVE 0 TO CONT-R.
      *-=-=-=-=-=-= INPRIME DOS RECIBOS -=-=-=-=-=-=-=-=
      *     WRITE LIN-IMP02 FROM COMPRI AFTER 0.
      *     WRITE LIN-IMP02 FROM LIN-1 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-01 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-2 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-3 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-4 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-5 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-51 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-6 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-7 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-8 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-9 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-91 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-92 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-10 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-11 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-111 AFTER 1.
      *     PERFORM ESPA.
      *     WRITE LIN-IMP02 FROM LIN-112 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-12 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-121 AFTER 1.
      *     PERFORM ESPA.
      *     WRITE LIN-IMP02 FROM LIN-122 AFTER 1.
      *     ADD 20 TO CONT-R.
      *     IF TIPO-CAP = "E" OR TIPO-CAP = "e"
      *        PERFORM ESPA 14 TIMES
      *        GO TO IMPRE-1.
      *     PERFORM ESPA.
      *     WRITE LIN-IMP02 FROM LIN-130 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-131 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-132 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-133 AFTER 1.
      *     PERFORM ESPA 3 TIMES.
      *     WRITE LIN-IMP02 FROM LIN-134 AFTER 1.
      *     WRITE LIN-IMP02 FROM LIN-135 AFTER 1.
      *     PERFORM ESPA 6 TIMES.
      *-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0-0--
       IMPRE-1.
           MOVE 0 TO CONT-R.
           GO TO TAREA1-1.
       IMPRE-F.
           EXIT.
       ESPA.
           MOVE SPACES TO LIN-IMP02.
           WRITE LIN-IMP02 AFTER 1.
           ADD 1 TO CONT-R.
       CARGA-EMP.
           PERFORM MEN-MODA-S.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-CTA.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                              SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
           MOVE RAZ-SOC-EMP TO NOMBRE-ED.
           MOVE CALLE-EMP TO DIRE-ED.
           MOVE COD-LOC-EMP TO COD-L-ED C-POS.
           MOVE 0 TO C-POS-1.
           READ CPOSTAL INVALID KEY MOVE "*******" TO NOM-POSTAL.
           MOVE NOM-POSTAL TO NOM-L-ED.
           MOVE 0 TO TEL-ED.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP SUC-COMPRO SUC-CTA NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.       
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           MOVE VERSION-EMP TO P-VERSION.
       CARGA-EMP-F.
           EXIT.
       CARGA-CAJA.
           PERFORM MEN-ANTE.
           MOVE 45 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUC.
           IF NUM-CAP = 0 GO TO CARGA-CAJA.
           MOVE NUM-CAP TO CAJA-COMPRO.
           MOVE 50 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CAJA.
           IF NUM-CAP = 0 GO TO CARGA-CAJA.
           MOVE NUM-CAP TO TURNO-CAP.
       CARGA-RUB.
           PERFORM MEN-ANTE.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CAJA.
           IF NUM-CAP = 0 GO TO CARGA-RUB.
           MOVE NUM-CAP TO P-RUBRO RUBRO-CTA.
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       CARGA-CUE.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-RUB.
           IF NUM-CAP = 0 GO TO CARGA-CUE-F.
           MOVE NUM-CAP TO CUENTA.
           MOVE 0 TO SUB-CUENTA.
           READ CUENTAS INVALID KEY GO TO CARGA-SUB-CUE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       CARGA-CUE-F.
           EXIT.
       CARGA-SUB-CUE.
           PERFORM MEN-ANTE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE.
      *-=-=-=-=-=- INVALIDA SUB CUENTAS =-=-=-=-=-=-=-=-=-=
           IF NUM-CAP NOT = 0 GO TO CARGA-SUB-CUE.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
           MOVE NUM-CAP TO SUB-CUENTA.
       CARGA-NOM.
           PERFORM MEN-ANTE-DES.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SUB-CUE.
           IF F03 PERFORM DESP-NOM GO TO CARGA-IMP.
           MOVE ALF-CAP TO NOMBRE-C.
       CARGA-TIPO.
           MOVE 25 TO SW3.
           PERFORM BORRA.
           ACCEPT TIPO-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F02 GO TO TAREA1-1.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           
       CARGA-SAL.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           ACCEPT SALDO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-CUE.
           GO TO CARGA-IMP.           
       CARGA-IMP.
           PERFORM MEN-ANTE.
           MOVE 0 TO IMP-CAP.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           ACCEPT IMP-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-SAL.
           IF F10 GO TO CARGA-VAL.
           MOVE IMP-CAP TO TOT-GRAL.
           IF IMP-CAP = 0 GO TO CARGA-IMP.
           PERFORM DESP-IMP.
       CARGA-VAL.
           PERFORM PANTA-2.
           MOVE 1 TO C-VAL.
       CARGA-VAL-1.
           PERFORM MEN-ANTE-DES.
      *----------- VOY POR ACA -----------
       CARGA-COM1.
           PERFORM MEN-ANTE-DES.
           MOVE SPACES TO COM-1-CAP.
           MOVE 40 TO SW3.
           PERFORM BORRA.
           ACCEPT COM-1-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-IMP.
           IF F03 MOVE SPACES TO COM-1-CAP GO TO CARGA-COM2.
       CARGA-COM2.
           PERFORM MEN-ANTE-DES.
           MOVE SPACES TO COM-2-CAP.
           MOVE 41 TO SW3.
           PERFORM BORRA.
           ACCEPT COM-2-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COM1.
           IF F03 MOVE SPACES TO COM-2-CAP GO TO CARGA-COM3.
       CARGA-COM3.
           PERFORM MEN-ANTE-DES.
           MOVE SPACES TO COM-3-CAP.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           ACCEPT COM-3-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COM2.
           IF F03 MOVE SPACES TO COM-3-CAP GO TO CARGA-COM4.
       CARGA-COM4.
           PERFORM MEN-ANTE-DES.
           MOVE SPACES TO COM-4-CAP.
           MOVE 43 TO SW3.
           PERFORM BORRA.
           ACCEPT COM-4-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COM3.
           IF F03 MOVE SPACES TO COM-4-CAP.
       DESP-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 7 TO SW3.
           PERFORM BORRA.
           MOVE RUBRO-CTA TO P-RUBRO.
           DISPLAY P-RUBRO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE 0 TO P-RUBRO-A.
           READ P-DE-CTAS INVALID KEY GO TO CARGA-RUB.
           IF P-CODIGO NOT = 9 PERFORM MEN-NO-CTA 50 TIMES
                         MOVE SPACES TO MEN-1 MEN-2 MEN-3
                               PERFORM MENS-PAN
                               GO TO CARGA-RUB.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           DISPLAY P-NOMBRE LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3).
       DESP-CUE.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
           MOVE 10 TO SW3.
           PERFORM BORRA.
           DISPLAY SUB-CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-NOM.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       DESP-EST.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY ESTADO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) BLINK REVERSE.
       DESP-SAL.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE SALDO TO SAL-EDI.
           DISPLAY SAL-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-SAL-CAN.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE SALDO-CAN TO SAL-C-EDI.
           DISPLAY SAL-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       DESP-IMP.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE IMP-CAP TO IMP-EDI.
           DISPLAY IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) BLINK.
       MEN-NO-CTA.
           MOVE "-- RUBRO NO HAB" TO MEN-1.
           MOVE "ILITADO PARA CU" TO MEN-2.
           MOVE "ENTAS CTES. -- " TO MEN-3.
           PERFORM MENS-PAN.    
       PANTA-1.
           DISPLAY "EMPRESA  :          SUC:      - " LINE 7 POSITION 5.
           DISPLAY "CAJA     : " LINE 8 POSITION 5.
           DISPLAY "TURNO : " LINE 8 POSITION 25.
           DISPLAY "RUBRO    :          - " LINE 9 POSITION 5.
           DISPLAY "CUENTA   :          SUB:  " LINE 10 POSITION 5.
           DISPLAY "NOMBRE   : " LINE 11 POSITION 5.
           DISPLAY "ESTADO   :" LINE 12 POSITION 5.
           DISPLAY "SALDO ACTUAL       :" LINE 13 POSITION 5.
           DISPLAY "SALDO CIERRE ANTER.:" LINE 14 POSITION 5.
           DISPLAY "TIPO PAGO:" LINE 15 POSITION 5.
           DISPLAY "-E- Efectivo -C- Cheque -T- Tarjeta"
                  LINE 15 POSITION 20 REVERSE. 
           DISPLAY "IMPORTE PAGADO     :" LINE 16 POSITION 5 REVERSE.
           DISPLAY "EN CONCEPTO DE:" LINE 17 POSITION 5.
       PANTA-2.
           DISPLAY "BANCO         :" LINE 18 POSITION 5.
           DISPLAY "CODIGO POSTAL :" LINE 19 POSITION 5.
           DISPLAY "N' CHEQUE     :" LINE 20 POSITION 5.
           DISPLAY "FECHA VENC.   :" LINE 20 POSITION 40.
       PANTA-3.
           DISPLAY "TARJETA       :" LINE 18 POSITION 5.
           DISPLAY "CODIGO POSTAL :" LINE 19 POSITION 5.
           DISPLAY "N' CUPON      :" LINE 20 POSITION 5.
           DISPLAY "FECHA VENC.   :" LINE 20 POSITION 40.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 091607 TO TAB-POS(7).
           MOVE 093030 TO TAB-POS(8).
           MOVE 101606 TO TAB-POS(9).
           MOVE 103004 TO TAB-POS(10).
           MOVE 111630 TO TAB-POS(11).
           MOVE 121630 TO TAB-POS(12).
           MOVE 131604 TO TAB-POS(13).
           MOVE 133030 TO TAB-POS(14).
           MOVE 141610 TO TAB-POS(15).
           MOVE 151602 TO TAB-POS(16).
           MOVE 121601 TO TAB-POS(17).
           MOVE 163703 TO TAB-POS(18).
           MOVE 164525 TO TAB-POS(19).
           MOVE 171601 TO TAB-POS(20).
           MOVE 173715 TO TAB-POS(21).
           MOVE 162719 TO TAB-POS(22).
           MOVE 132719 TO TAB-POS(23).
           MOVE 142719 TO TAB-POS(24).
           MOVE 121606 TO TAB-POS(30).
           MOVE 123004 TO TAB-POS(31).
           MOVE 151606 TO TAB-POS(32).
           MOVE 153004 TO TAB-POS(33).
           MOVE 181602 TO TAB-POS(34).
           MOVE 121630 TO TAB-POS(35).
           MOVE 151630 TO TAB-POS(36).
           MOVE 211906 TO TAB-POS(37).
           MOVE 215006 TO TAB-POS(38).
           MOVE 181050 TO TAB-POS(40).
           MOVE 191050 TO TAB-POS(41).
           MOVE 201050 TO TAB-POS(42).
           MOVE 211050 TO TAB-POS(43).
           MOVE 081606 TO TAB-POS(45).
      *-=-=-=-=- AGREGADO POR CAJA -=-=-=-=-=-=-=-
           MOVE 151601 TO TAB-POS(25).
           MOVE 182730 TO TAB-POS(26).
           MOVE 192730 TO TAB-POS(27).
           MOVE 182103 TO TAB-POS(46).
           MOVE 192104 TO TAB-POS(47).
           MOVE 202110 TO TAB-POS(48).
           MOVE 205608 TO TAB-POS(49).
           MOVE 083402 TO TAB-POS(50).
       TIT-PAN.
           MOVE "   - ARCHI" TO TIT-1-PAN.
           MOVE "VO DE CTAS" TO TIT-2-PAN.
           MOVE ". CTES.-  " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  EMISION RECIBOS" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1 
                     UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 40 TO SW3.
           PERFORM BORRA.
           MOVE 41 TO SW3.
           PERFORM BORRA.
           MOVE 42 TO SW3.
           PERFORM BORRA.
           MOVE 43 TO SW3.
           PERFORM BORRA.
       BORRA-PANTA-2.
           DISPLAY SPACES LINE 18 POSITION 5 SIZE 70.
           DISPLAY SPACES LINE 19 POSITION 5 SIZE 70.
           DISPLAY SPACES LINE 20 POSITION 5 SIZE 70.
           DISPLAY SPACES LINE 21 POSITION 5 SIZE 70.
       CIERRE-A.
           CLOSE IMP02 CUENTAS COMPRO P-DE-CTAS MOVI EMPRESAS CPOSTAL.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "CONVER.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".