       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAPPRE.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. ENE'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "ENVASE.SEL".
       COPY "PANTA.SEL".
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "ALICUO.SEL".
       COPY "INTER.SEL".
       COPY "PARTI.SEL".
       COPY "EMPRE.SEL".
       COPY "BSTOCK.SEL".
       COPY "LINEAS.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "ENVASE.REG".
       COPY "PANTA.REG".
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "ALICUO.REG".
       COPY "INTER.REG".
       COPY "PARTI.REG".
       COPY "BSTOCK.REG".
       COPY "LINEAS.REG".
       COPY "EMPREBM.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA   PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 TIP-COM-ED   PIC X(10).
             03 FILLER       PIC X(6) VALUE " Nro: ".
             03 NUM-COM-ED   PIC X(15)BB.
             03 FILLER       PIC X(12) VALUE "PROVEEDOR:".
             03 BB           PIC 99 COMP-4 VALUE 18.
             03 CC           PIC 99 COMP-4 VALUE 14.
             03 PRO-COM-ED   PIC X(30).
             03 DD           PIC 99 COMP-4 VALUE 20.
             03 EE           PIC 99 COMP-4 VALUE 15.
          02 IMP-2.
             03 NUM-ED       PIC Z(6)B.
             03 MAR-ED       PIC 9(4)B(4).
             03 DET-NUM-ED   PIC X(30)B(4).
             03 DET-MAR-ED   PIC X(20)B(4).
             03 CAN-ED       PIC ZZZ.ZZ9,999B(4).
             03 COS-ED       PIC Z.ZZZ.ZZZ.ZZ9,99.
             03 PRE-I-ED     PIC Z.ZZZ.ZZZ.ZZ9,99.
      *         03 PRE-N-ED     PIC Z.ZZZ.ZZZ.ZZ9,99.
             03 FILLER       PIC XX VALUE SPACES.
             03 PARTI-ED     PIC 9(8).
         02 IMP-3.
             03 FILLER       PIC X(5) VALUE "SON: ".
             03 ART-ED       PIC ZZZZZ9BB.
             03 FILLER       PIC X(21) VALUE "ARTICULOS que suman: ".
             03 UNI-ED       PIC ZZZZZ9,999BB.
             03 FILLER       PIC X(21) VALUE "UNIDADES por un valor".
             03 FILLER       PIC X(21) VALUE " NETO de ----------->".
             03 TOTAL-ED     PIC Z.ZZZ.ZZZ.ZZ9,99.
         02 IMP-5.
             03 FILLER       PIC X(12) VALUE "- RUBRO --".
             03 FILLER       PIC X(19) VALUE "-- DESCRIPCION --".
             03 FILLER       PIC X(15) VALUE "- DEBITO -".
             03 FILLER       PIC X(15) VALUE "- CREDITO-".
         02 IMP-51.
             03 RUB-D-BS-ED  PIC Z(7).
             03 FILLER       PIC X(5) VALUE SPACES.
             03 DES-D-BS-ED  PIC X(17)BB.
             03 DEB-D-BS-ED  PIC ---.--9,99.
         02 IMP-52.
             03 FILLER       PIC X(3) VALUE " a ".
             03 RUB-C-BS-ED  PIC Z(7)BB.
             03 DES-C-BS-ED  PIC X(17)BB.
             03 FILLER       PIC X(15) VALUE SPACES.
             03 CRE-C-BS-ED  PIC ---.--9,99.
         02 IMP-53.
             03 FILLER       PIC X(14) VALUE "  TOTALES --> ".
             03 FILLER       PIC X(17) VALUE SPACES.
             03 TOT-DEB-ED   PIC ---.--9,99B(5).
             03 TOT-CRE-ED   PIC ---.--9,99.
         02 TIT-1.
             03 FILLER       PIC X(15) VALUE "ARTIC./MARCA".
             03 FILLER       PIC X(15) VALUE "------- DETALLE".
             03 FILLER       PIC X(19) VALUE " ARTIC.--------".
             03 FILLER       PIC X(10) VALUE "------ MAR".
             03 FILLER       PIC X(14) VALUE "CA -------".
             03 FILLER       PIC X(15) VALUE "- CANTIDAD-".
      *=-=-=-=-=-=-= imprime solo costo =-=-=-=-=-=-=-=-=-==-=-=-
             03 FILLER       PIC X(16) VALUE "---- IMPORTE ---".
      *=-=-=-=-=-=-= imprime precio final y no inscripto =-=-=-=-
             03 FILLER       PIC X(18) VALUE " - PRECIO NETO -".
      *       03 FILLER       PIC X(16) VALUE "-PRECIO NO INSC-".
             03 FILLER       PIC X(10) VALUE "  PARTIDA ".
       01 TIP-COM-CAP        PIC X(10).
       01 NUM-COM-CAP        PIC X(15).
       01 PRO-COM-CAP        PIC X(30).
       01 ART-CAP            PIC 9(6).
       01 UNI-CAP            PIC 9(6)V999.
       01 IMP-COS-CAP        PIC 9(12)V99 VALUE 0.
       01 PROM-ANT-CAP       PIC 9(15)V99 VALUE 0.
       01 PROM-ACT-CAP       PIC 9(15)V99 VALUE 0.
       01 RUB-CAP    PIC 9(8) VALUE 0.
       01 PRE-CAP    PIC 9(12)V99 VALUE 0.
       01 II-CAP     PIC 9(12)V99 VALUE 0.
       01 NETO-CAP   PIC 9(12)V99 VALUE 0.
       01 RECA-CAP   PIC 9(12)V99 VALUE 0.
       01 PRE-F-CAP  PIC 9(12)V99 VALUE 0.
       01 PRE-N-CAP  PIC 9(12)V99 VALUE 0.
       01 IVA-CAP    PIC 9(12)V99 VALUE 0.
       01 TOTAL-CAP  PIC 9(12)V99 VALUE 0.
       01 COS-CAP    PIC 9(12)V99 VALUE 0.
       01 PRO-II-CAP PIC 9(12)V99 VALUE 0.
       01 ELPOR      PIC 9(2)V99 VALUE 0.
       01 TOT-CRE-CAP PIC S9(12)V99 VALUE 0.
       01 TOT-DEB-CAP PIC S9(12)V99 VALUE 0.
       01 PAR-P.
          02 AA-P    PIC 99 VALUE 0.
          02 MM-P    PIC 99 VALUE 0.
          02 DD-P    PIC 99 VALUE 0.
          02 NUM-P   PIC 99 VALUE 0.
       01 ANO1       PIC 9999 VALUE 0.
       01 COS-EDI    PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 PRE-EDI    PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 PRE-F-EDI  PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 PRE-V-EDI  PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 II-EDI     PIC ZZZ.ZZZ.ZZZ.ZZ9,99.
       01 MONE-EDI   PIC XXX.
       01 II-COD     PIC 9 VALUE 0.
       01 COD-MONE   PIC 9 VALUE 0.
       01 COD-IVA    PIC 9 VALUE 0.
       01 CAN-CAP    PIC 9(6)V999 VALUE 0.
       01 NUM-OT     PIC 9(6) VALUE 0.
       01 ENV-OT     PIC 9(3) VALUE 0.
       01 CONT-LEO   PIC 99 VALUE 0.
       01 IVA-1-CAP  PIC 99V99.
       01 IVA-2-CAP  PIC 99V99.
       01 FECHA-CAP.
          03 DD-CAP  PIC 99.
          03 MM-CAP  PIC 99.
          03 AA-CAP  PIC 99.
       01 FECHA-F-CAP.
          03 DD-F-CAP PIC 99 VALUE 0.
          03 MM-F-CAP PIC 99 VALUE 0.
          03 AA-F-CAP PIC 99 VALUE 0.
       01 FECHA-F-EDI.
          03 DD-F-EDI  PIC 99/.
          03 MM-F-EDI  PIC 99/.
          03 AA-F-EDI  PIC 99.
       01 CAN-II-CAP     PIC 9(6)V999 VALUE 0.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "ENVASE.ERR".
       COPY "ART01.ERR".
       COPY "ART02.ERR".
       COPY "NUMERO.ERR".
       COPY "MARCAS.ERR".
       COPY "EMPRE.ERR".
       COPY "PARTI.ERR".
       COPY "ALICUO.ERR".
       COPY "INTER.ERR".
       COPY "LINEAS.ERR".
       COPY "BSTOCK.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 = 25 GO TO SIGA-CLAVE.
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
           MOVE 28 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F02 GO TO ACTU1.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " PRECIO FINAL S/IVA " TO D20-CAP.
           MOVE 0 TO COD-IVA.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
           GO TO PANTA-ACTU.
       ACTU1.
           MOVE " PRECIO FINAL C/IVA " TO D20-CAP.
           MOVE 1 TO COD-IVA.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
       PANTA-ACTU.
           DISPLAY "EMPRESA    :" LINE 8 POSITION 3.
           DISPLAY " - SUC.: " LINE 8 POSITION 18.
       PANTA-ACTU1.
           DISPLAY "TIPO COMPR.:" LINE 10 POSITION 3.
           DISPLAY "NRO COMPR. :" LINE 12 POSITION 3.
           DISPLAY "PROVEEDOR  :" LINE 14 POSITION 3.
           DISPLAY "FECHA FACT.:" LINE 16 POSITION 3.
           DISPLAY " PRECAUCION:                  " LINE 18 POSITION 25
                                       BLINK REVERSE.
           DISPLAY " 1) Ingrese PRECIOS de COSTO  " LINE 19 POSITION 25
                                       REVERSE.
           DISPLAY "    sin IVA ni IMP.INTERNOS   " LINE 20 POSITION 25
                                       REVERSE.
           DISPLAY " 2) Conecte la IMPRESORA      " LINE 21 POSITION 25
                                       REVERSE.  
           GO TO PANTA-3.
       PANTA-ACTU2.
           DISPLAY "NRO ARTIC. :" LINE 10 POSITION 3.
           DISPLAY "DETALLE    :" LINE 12 POSITION 3.
           DISPLAY "PRECIO ANT.:" LINE 14 POSITION 3.
           DISPLAY "COSTO ACT. :" LINE 16 POSITION 3.
           DISPLAY "PRECIO SUG.:" LINE 20 POSITION 3.
           DISPLAY "PRECIO ACT.:" LINE 22 POSITION 3.
           DISPLAY "CANT.INGR. :" LINE 18 POSITION 3.
       PANTA-3.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
           PERFORM CARGO-EMP THRU CARGO-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
      *     PERFORM CARGO-ALI THRU CARGO-ALI-F.
           OPEN INPUT ART01
                INPUT ART02
                INPUT ENVASE
                INPUT NUMERO
                INPUT MARCAS.
           OPEN OUTPUT IMP02.
       CARGA-TIP-COM.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           MOVE ALF-CAP TO TIP-COM-ED.
       CARGA-NUM-COM.
           PERFORM MEN-ANTE.
           MOVE 31 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-TIP-COM.
           MOVE ALF-CAP TO NUM-COM-ED.
       CARGA-PRO.
           MOVE 32 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-NUM-COM.
           MOVE ALF-CAP TO PRO-COM-ED.
       CARGA-FEC-F.
           MOVE 34 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-F-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-PRO.
           IF DD-F-CAP > 31 GO TO CARGA-FEC-F.
           IF DD-F-CAP = 0 GO TO CARGA-FEC-F.
           IF MM-F-CAP > 12 GO TO CARGA-FEC-F.
           IF MM-F-CAP = 0 GO TO CARGA-FEC-F.
           MOVE DD-F-CAP TO DD-F-EDI.
           MOVE MM-F-CAP TO MM-F-EDI.
           MOVE AA-F-CAP TO AA-F-EDI.
           DISPLAY FECHA-F-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
       CARGA-SIGA.
           PERFORM BORRA-PANTA-1.
           PERFORM PANTA-ACTU2.
           MOVE 0 TO CONT-R CONT-P CONT-I TOTAL-CAP ART-CAP UNI-CAP.
       CARGA-NUM.
           MOVE 0 TO II-CAP.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3,
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA-C.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM PIE-P THRU PIE-P-F 
                    PERFORM CIERRO-A
                    GO TO INI-MOD.
           MOVE 0 TO ENV-OT.
           IF NUM-CAP = 0 GO TO CARGA-NOM.
           MOVE NUM-CAP TO COD-ART02 COD-ART01.
           GO TO SIGA-NUM1.
       SIGA-NUM.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           MOVE NUM-CAP TO NUM-ENVA ENV-OT.
           READ ENVASE WITH NO LOCK INVALID KEY
                                   MOVE "******" TO NOM-ENVA
                                   MOVE 0 TO ENV-OT.
           MOVE 24 TO SW3.
           DISPLAY NOM-ENVA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3).
       SIGA-NUM1.
           MOVE NUM-EMP TO EMP-ART02 EMP-ART01 EMP-PARTI.
           MOVE SUC-EMP TO SUC-ART02 SUC-PARTI.

           IF NUM-ART02 = 0 MOVE MAR-ART02 TO NUM-ART02
                            MOVE 0         TO MAR-ART02.

           MOVE NUM-ART02 TO NUM-OT.
           IF SUC-EMP = 0
                MOVE 1 TO SUC-ART02.
      *     MOVE 0 TO SUC-ART02.
           MOVE 0 TO CONT-LEO.
           START ART02 KEY IS NOT < CLAVE-ART02 
                         INVALID KEY GO TO CARGA-NUM.
       L-ART02.
           READ ART02 NEXT WITH NO LOCK AT END GO TO CARGA-NUM.
           MOVE COD-ART02 TO COD-ART01 COD-PARTI.
           READ ART01 INVALID KEY GO TO CARGA-NUM.
           IF CONT-LEO > 100 GO TO CARGA-NUM.
           IF IMP-IVA-ART01 = 0 MOVE 1 TO ALI-ALI
              ELSE MOVE IMP-IVA-ART01 TO ALI-ALI.
           PERFORM CARGO-ALI THRU CARGO-ALI-F.
           IF ENV-OT = 0 GO TO S-L-ART02.
           IF ENV-OT NOT = ENV-ART01 ADD 1 TO CONT-LEO
                                     UNLOCK ART01 RECORD
                                     GO TO L-ART02.
       S-L-ART02.
           MOVE NUM-ART02 TO COD-NUM.
           MOVE MAR-ART02 TO COD-MAR.
           READ NUMERO WITH NO LOCK INVALID KEY 
                            MOVE "******" TO DET-NUM.
           READ MARCAS WITH NO LOCK INVALID KEY 
                            MOVE "******" TO DET-MAR.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-ART02 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE DET-NUM TO ALF-CAP.
           PERFORM DES-ALF.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE DET-MAR TO ALF-CAP.
           PERFORM DES-ALF.
           MOVE ENV-ART01 TO NUM-ENVA.
           READ ENVASE WITH NO LOCK INVALID KEY 
                         MOVE "*****" TO NOM-ENVA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE NOM-ENVA TO ALF-CAP.
           PERFORM DES-ALF.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 0 TO COD-MONE.
           MOVE 0 TO PRE-V-EDI.
           IF PRE-V-ART02 NOT = 0
                MOVE " $ " TO MONE-EDI
                MOVE 1 TO COD-MONE
                MOVE PRE-V-ART02 TO PRE-V-EDI.
      *=-=-=-=-- OJO CAMBIAR CUANDO MODIFIQUE IMP.INTERNO =-=-=-
           IF IMP-II-ART01 = 0
              IF PRE-E-ART02 NOT = 0
                MOVE 2 TO COD-MONE
                MOVE "U$S" TO MONE-EDI
                MOVE PRE-E-ART02 TO PRE-V-EDI.
           DISPLAY PRE-V-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3).
           DISPLAY MONE-EDI LINE 14 POSITION 35 SIZE 3 BLINK REVERSE.
           PERFORM MEN-CONS.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F03 GO TO L-ART02.
           MOVE 0 TO II-COD.
       CARGA-PARTI.
           MOVE 0 TO PAR-P.
           IF REGI-ART01 = "p" OR REGI-ART01 = "P"
                DISPLAY " REGISTRO POR PARTIDA " LINE 11 POSITION 15
                 BLINK REVERSE
              ELSE GO TO CARGA-COS.
           MOVE AA-F-CAP TO AA-P.
           MOVE MM-F-CAP TO MM-P.
           MOVE DD-F-CAP TO DD-P.
           MOVE 0 TO NUM-P.
       CARGA-PARTI1.
           ADD 1 TO NUM-P.
           MOVE PAR-P TO PAR-PARTI.
           OPEN INPUT PARTI.
           READ PARTI INVALID KEY CLOSE PARTI GO TO CARGA-COS.
           CLOSE PARTI.
           GO TO CARGA-PARTI1.    
       CARGA-COS.
           IF COD-MONE = 0 PERFORM CARGA-COD-M.
           PERFORM MEN-ANTE.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           ACCEPT COS-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-NUM.
           DISPLAY "Porc.Ajuste Espec..:" LINE 16 POSITION 40
           ACCEPT ELPOR LINE 16 POSITION 65 NO BEEP TAB CONVERT
           COMPUTE COS-CAP ROUNDED = COS-CAP - ((COS-CAP * ELPOR) 
                                        / 100)
           MOVE COS-CAP TO COS-EDI.
           DISPLAY COS-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           IF IMP-II-ART01 = 1 MOVE 1 TO II-COD
                                GO TO CARGA-II.
           GO TO CARGA-CAN.
       CARGA-II.
           DISPLAY "IMP.INTER. :" LINE 17 POSITION 3.
           MOVE 33 TO SW3.
           PERFORM BORRA.
           ACCEPT II-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                         SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                         ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COS.
           MOVE II-CAP TO II-EDI.
           DISPLAY II-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3).
       CARGA-CAN.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           ACCEPT CAN-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-PRE.
           COMPUTE SAL-S-ART02 = SAL-S-ART02 + CAN-CAP.
           COMPUTE PROM-ANT-CAP ROUNDED = UNI-COMP-ART01 *
                                  VAL-COMP-ART01.
           COMPUTE UNI-COMP-ART01 ROUNDED = UNI-COMP-ART01 + CAN-CAP.
           MOVE COS-CAP TO COSTO-STOCK-ART01.
       CARGA-PRE-S.
           COMPUTE PRE-CAP ROUNDED = ( COS-CAP + II-CAP )
                       * PORC-ART01 / 100.
           COMPUTE PRE-CAP ROUNDED = PRE-CAP + COS-CAP.
           IF COD-IVA = 0 GO TO CARGA-PRE-II.
           PERFORM CARGO-ALI THRU CARGO-ALI-F.
           IF NUM-ART01 < 900000
                COMPUTE IVA-CAP ROUNDED = PRE-CAP * IVA-1-CAP / 100
                COMPUTE PRE-CAP = PRE-CAP + IVA-CAP.
       CARGA-PRE-II.
           IF IMP-II-ART01 = 0
                      GO TO DESP-PRE-S.
      *=-=-=-=-=- SACAR CUANDO MODIFIQUE IMP-INTERNO -=-=-=-=-=-
           COMPUTE PRE-CAP = PRE-CAP + II-CAP.
           MOVE II-CAP TO PRE-E-ART02.
           GO TO DESP-PRE-S.
      *=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-===
           OPEN I-O INTER.
           MOVE NUM-EMP TO EMP-INT.
           MOVE SUC-EMP TO SUC-INT.
           MOVE COD-ART02 TO ART-INT.
           MOVE 0 TO AA-INT MM-INT DD-INT PRO-II-CAP CAN-II-CAP.
           START INTER KEY IS NOT < CLAVE-INTER
                INVALID KEY MOVE 0 TO PRO-II-CAP
                            GO TO CARGA-PRE-II2.
       CARGA-PRE-II1.  
           READ INTER NEXT AT END GO TO CARGA-PRE-II2.
           IF EMP-INT NOT = NUM-EMP GO TO CARGA-PRE-II2.
           IF SUC-EMP NOT = SUC-INT GO TO CARGA-PRE-II2.
           IF ART-INT NOT = COD-ART02 GO TO CARGA-PRE-II2.
           IF CAN-INT = 0 GO TO CARGA-PRE-II1.
           COMPUTE PRO-II-CAP ROUNDED = PRO-II-CAP + ( IMP-INT *
                    CAN-INT ).
           COMPUTE CAN-II-CAP = CAN-II-CAP + CAN-INT.
           GO TO CARGA-PRE-II1.
       CARGA-PRE-II2.
           COMPUTE PRO-II-CAP ROUNDED = PRO-II-CAP + ( II-CAP *
                      CAN-CAP ).
           COMPUTE CAN-II-CAP = CAN-II-CAP + CAN-CAP.
           MOVE NUM-EMP TO EMP-INT.
           MOVE SUC-EMP TO SUC-INT.
           MOVE COD-ART02 TO ART-INT.
           MOVE DD-F-CAP TO DD-INT.
           MOVE MM-F-CAP TO MM-INT.
           MOVE AA-F-CAP TO AA-INT.
           READ INTER INVALID KEY MOVE CAN-CAP TO CAN-INT
                                  MOVE II-CAP TO IMP-INT
                                  GO TO GRAVO-PRE-II.
           COMPUTE CAN-INT = CAN-INT + CAN-CAP.
           COMPUTE IMP-INT = IMP-INT + II-CAP.
       GRAVO-PRE-II.
           WRITE REG-INTER INVALID KEY REWRITE REG-INTER.
           CLOSE INTER.
       DESP-PRE-S.
      *=-=-=-== VER CUANDO MODIFIQUE IMP.INTERNO =-=-=-===
      *     COMPUTE PRE-CAP ROUNDED = PRE-CAP + ( PRO-II-CAP /
      *                CAN-II-CAP ).
      *=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=
           MOVE PRE-CAP TO PRE-EDI.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           DISPLAY PRE-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) REVERSE.
           IF COD-IVA = 0 DISPLAY " - SIN IVA - " LINE 20 POSITION 35
                            REVERSE.
           IF COD-IVA = 1 DISPLAY " - CON IVA - " LINE 20 POSITION 35
                            REVERSE.
       CARGA-PRE.
           PERFORM MEN-ANTE.
           DISPLAY " <F1> PRECIO SUG. <F3> ANTERIOR " LINE 22
                  POSITION 40 SIZE 31 LOW REVERSE.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           ACCEPT PRE-F-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            SIZE TAB-SIZ(SW3) TAB CONVERT
                            ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-COS.
           IF F01 MOVE PRE-CAP TO PRE-F-CAP.
           IF F03 PERFORM PRE-ANT THRU PRE-ANT-F.
           DISPLAY SPACES LINE 22 POSITION 40 SIZE 32.
           DISPLAY SPACES LINE 20 POSITION 35 SIZE 20.
           MOVE PRE-F-CAP TO PRE-F-EDI.
           COMPUTE NETO-CAP ROUNDED = ( PRE-F-CAP - II-CAP ) /
                ( ( IVA-1-CAP / 100 ) + 1 ).
           COMPUTE RECA-CAP ROUNDED = NETO-CAP * IVA-2-CAP / 100.
           IF IMP-IVA-ART01 NOT = 999
                COMPUTE PRE-N-CAP ROUNDED  = PRE-F-CAP + RECA-CAP
                ELSE MOVE PRE-F-CAP TO PRE-N-CAP.
           PERFORM BORRA.
           DISPLAY PRE-F-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                             SIZE TAB-SIZ(SW3) BLINK REVERSE.
           PERFORM MEN-CONF.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           UNLOCK ART01 RECORD.
           IF F01 GO TO GRAVO.
           GO TO CARGA-NUM.      
       PRE-ANT.
           IF COD-MONE = 0 GO TO PRE-ANT-F.
           IF COD-MONE = 1 MOVE PRE-V-ART02 TO PRE-F-CAP
                      GO TO PRE-ANT-F.
           IF COD-MONE = 2 MOVE  PRE-E-ART02 TO PRE-F-CAP
                      GO TO PRE-ANT-F.
       PRE-ANT-F.
           EXIT.
       CARGA-COD-M.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           DISPLAY "INGRESE <1> EN -$- o <2> EN U$S" LINE 16
                  POSITION 35 REVERSE.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF NUM-CAP > 2 OR NUM-CAP < 1 GO TO CARGA-COD-M.
           MOVE NUM-CAP TO COD-MONE.
           DISPLAY "                               " LINE 16
                  POSITION 35.
           IF COD-MONE = 1 MOVE " $ " TO MONE-EDI.
           IF COD-MONE = 2 MOVE "U$S" TO MONE-EDI.
           DISPLAY MONE-EDI LINE 14 POSITION 35 SIZE 3 BLINK
                     REVERSE.
       GRAVO.
           CLOSE ART01 ART02.
           OPEN I-O ART01 ART02.
           MOVE COS-CAP TO COSTO-STOCK-ART01.
           COMPUTE PROM-ACT-CAP ROUNDED = PROM-ANT-CAP +
                      ( COS-CAP * CAN-CAP).
           COMPUTE VAL-COMP-ART01 ROUNDED = PROM-ACT-CAP /
                          UNI-COMP-ART01.
           IF COD-MONE = 0 MOVE 0 TO PRE-V-ART02 PRE-E-ART02.
           IF COD-MONE = 1
                 MOVE PRE-F-CAP TO PRE-V-ART02.
           IF COD-MONE = 2
                 MOVE PRE-F-CAP TO PRE-E-ART02.
           MOVE DD-F-CAP TO DD-CAP.
           MOVE MM-F-CAP TO MM-CAP.
           MOVE AA-F-CAP TO AA-CAP.
           IF COD-MONE = 0 MOVE 0 TO FEC-V-ART02 FEC-E-ART02.
           IF COD-MONE =  1
                 MOVE FECHA-CAP TO FEC-V-ART02.
           IF COD-MONE = 2
                 MOVE FECHA-CAP TO FEC-V-ART02.
           COMPUTE ART-CAP = ART-CAP + 1.
           COMPUTE UNI-CAP = UNI-CAP + CAN-CAP.
           MOVE FECHA-CAP TO FEC-U-COMP-ART01.
           WRITE REG-ART01 INVALID KEY REWRITE REG-ART01.
           WRITE REG-ART02 INVALID KEY REWRITE REG-ART02.
           CLOSE ART01.
           MOVE 0 TO PARTI-ED.
           IF REGI-ART01 = "P" OR REGI-ART01 = "p"
              PERFORM ACT-PARTI THRU ACT-PARTI-F.
           OPEN INPUT ART01.
           IF SUC-EMP = 0 GO TO SIGA-GRAVO.
           CLOSE ART02.
           OPEN INPUT ART02.
           PERFORM GRAVO-B THRU GRAVO-B-F.
           GO TO IMPRE.
       GRAVO-B.
           MOVE EMP-ART02 TO EMP-BS.
           MOVE SUC-ART02 TO SUC-BS.
           MOVE AA-CAP TO AA-BS.
           MOVE MM-CAP TO MM-BS.
           MOVE DD-CAP TO DD-BS.
           MOVE LIN-1-ART01 TO COD-1-LIN.
           MOVE LIN-2-ART01 TO COD-2-LIN.
           MOVE LIN-3-ART01 TO COD-3-LIN.
           MOVE LIN-4-ART01 TO COD-4-LIN.
           OPEN INPUT LINEAS.
           READ LINEAS INVALID KEY MOVE 0 TO RUB-1-LIN.
           CLOSE LINEAS.
           MOVE RUB-1-LIN TO RUB-BS.
           MOVE 1 TO CONDI-BS.
           OPEN I-O BSTOCK.
           READ BSTOCK INVALID KEY MOVE 0 TO IMP-BS.
           COMPUTE IMP-COS-CAP ROUNDED = COS-CAP * CAN-CAP.
           COMPUTE IMP-BS = IMP-BS + IMP-COS-CAP.
           REWRITE REG-BSTOCK INVALID KEY WRITE REG-BSTOCK.
           MOVE RUB-CAP TO RUB-BS.
           MOVE 2 TO CONDI-BS.
           READ BSTOCK INVALID KEY MOVE 0 TO IMP-BS.
           COMPUTE IMP-BS = IMP-BS + IMP-COS-CAP.
           REWRITE REG-BSTOCK INVALID KEY WRITE REG-BSTOCK.
           CLOSE BSTOCK.
       GRAVO-B-F.
           EXIT.
       ACT-PARTI.
           MOVE PAR-P TO PARTI-ED.
           MOVE COS-CAP TO PRE-C-PARTI.
           MOVE PRE-F-CAP TO PRE-V-PARTI.
           MOVE FECHA-CAP TO FEC-PARTI.
           MOVE CAN-CAP TO SAL-S-PARTI.
           MOVE 0 TO II-PARTI BAJ-S-PARTI ENT-S-PARTI KILO-S-PARTI
                     KILO-B-PARTI.
           MOVE SPACES TO SEC-1-PARTI SEC-2-PARTI SEC-3-PARTI.
           OPEN I-O PARTI.
           WRITE REG-PARTI INVALID KEY DISPLAY "NO GRAVO PARTI"
                         DISPLAY "YA EXISTE PARTIDA"
                         CLOSE PARTI
                         STOP RUN.
           CLOSE PARTI.
       ACT-PARTI-F.
           EXIT.

       SIGA-GRAVO.
           COMPUTE SUC-ART02 = SUC-ART02 + 1.
           READ ART02 INVALID KEY CLOSE ART02
                              OPEN INPUT ART02
                              GO TO IMPRE.
           REWRITE REG-ART02 INVALID KEY WRITE REG-ART02.
           GO TO SIGA-GRAVO.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           PERFORM FECHA-ROT132.
           PERFORM HORA-ROT132.
           MOVE CONT-P TO HOJ-E132.
           MOVE "<<< LISTAD" TO TIT-1-EDI.
           MOVE "O  DE  ING" TO TIT-2-EDI.
           MOVE "RESO  DE  " TO TIT-3-EDI.
           MOVE "STOCK  Y  " TO TIT-4-EDI.
           MOVE "PRECIOS  >" TO TIT-5-EDI.
           MOVE ">>        " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-1 AFTER 1.
           ADD 9 TO CONT-R.
       IMPRE.
           IF CONT-P = 0 GO TO TITU.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           MOVE NUM-ART02 TO NUM-ED.
           MOVE MAR-ART02 TO MAR-ED.
           MOVE DET-NUM TO DET-NUM-ED.
           MOVE DET-MAR TO DET-MAR-ED.
           MOVE CAN-CAP TO CAN-ED.
           COMPUTE IMP-COS-CAP ROUNDED = COS-CAP * CAN-CAP.
      *=-=-=-=-= PARA LISTAR VALORES DE COSTOS =-=-=-=-=-=-=-
           MOVE IMP-COS-CAP TO COS-ED.
      *=-=-=-=-=- PARA LISTAR PRECIOS FINALES =-=-=-===-=-==-
           MOVE PRE-F-CAP TO PRE-I-ED.
      *     MOVE PRE-N-CAP TO PRE-N-ED.
           COMPUTE TOTAL-CAP = TOTAL-CAP + IMP-COS-CAP.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO CARGA-NUM.
       PIE-P.
           MOVE ART-CAP TO ART-ED.
           MOVE UNI-CAP TO UNI-ED.
           MOVE TOTAL-CAP TO TOTAL-ED.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           ADD 2 TO CONT-R.
       PIE-11.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM IMP-5 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO RUB-BS CONDI-BS FEC-BS TOT-DEB-CAP TOT-CRE-CAP.
           OPEN INPUT BSTOCK.
           START BSTOCK KEY IS NOT < CLAVE-BSTOCK
                     INVALID KEY CLOSE BSTOCK
                     PERFORM CIERRO-A
                     GO TO INI-MOD.
       PIE-12.
           IF CONT-R > 66 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          WRITE LIN-IMP02 FROM IMP-5 AFTER 1
                          ADD 1 TO CONT-R.
           READ BSTOCK NEXT AT END GO TO PIE-15.
           IF CONDI-BS = 1
                   GO TO PIE-13.
           GO TO PIE-15.
       PIE-13.
           MOVE RUB-BS TO RUB-D-BS-ED.
           MOVE IMP-BS TO DEB-D-BS-ED.
           MOVE "POR COMPRAS VS" TO DES-D-BS-ED.
           COMPUTE TOT-DEB-CAP = TOT-DEB-CAP + IMP-BS.
           WRITE LIN-IMP02 FROM IMP-51 AFTER 1.
           ADD 1 TO CONT-R
           GO TO PIE-12.
       PIE-15.
           MOVE TOT-DEB-CAP TO TOT-DEB-ED.
           MOVE 0 TO TOT-CRE-ED.
           WRITE LIN-IMP02 FROM IMP-53 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO TOT-DEB-CAP TOT-CRE-CAP.
           CLOSE BSTOCK.
           OPEN OUTPUT BSTOCK.
           CLOSE BSTOCK.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       PIE-P-F.
           EXIT. 
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       CARGA-NOM.
           PERFORM MEN-ANTE.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F02 GO TO CARGA-NUM.
           IF ALF-CAP = SPACES GO TO CARGA-NUM.
           MOVE ALF-CAP TO DET-NUM.
           START NUMERO KEY IS NOT < DET-NUM
                    INVALID KEY GO TO CARGA-NUM.
       SIGA-NOM.
           READ NUMERO NEXT AT END GO TO CARGA-NUM.
           MOVE COD-NUM TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE DET-NUM TO ALF-CAP.
           PERFORM DES-ALF.
           PERFORM MEN-CONS.
           MOVE 3 TO SW3.
           PERFORM CAP-COD1.
           IF ESCAP PERFORM CIERRO-A GO TO INI-MOD.
           IF F03 GO TO SIGA-NOM.
           GO TO SIGA-NUM.
       CARGO-EMP.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGO-EMP.
           MOVE NUM-CAP TO NUM-EMP MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           OPEN INPUT EMPRESAS.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                     GO TO CARGO-EMP.
           OPEN INPUT MANEJO
           READ MANEJO INVALID KEY DISPLAY "No Manejo"
                CLOSE MANEJO
                GO TO CARGO-EMP.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS MANEJO GO TO INI-MOD.
           IF F02 CLOSE EMPRESAS MANEJO GO TO CARGO-EMP.
           IF NUM-CAP = 0 
               MOVE 15 TO SW3 
               DISPLAY "ACT.TODAS LAS SUC." LINE 9 POSITION 30
                              SIZE 2.
           MOVE NUM-CAP TO SUC-EMP.
           IF SUC-EMP = 0 MOVE 0 TO NUM-SUC-E132
                          MOVE SPACES TO SUC-E132
                          CLOSE EMPRESAS GO TO CARGO-EMP-F.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                     GO TO INI-MOD.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE TAB-SIZ(SW3).
           MOVE SUC-EMP TO NUM-SUC-E132.
           MOVE RAZ-SOC-EMP TO SUC-E132.
           CLOSE EMPRESAS.
       CARGO-EMP-F.
           EXIT. 
       CARGO-ALI.
           MOVE NUM-EMP TO EMP-ALI.
           MOVE SUC-EMP TO SUC-ALI.
           IF IMP-IVA-ART01 = 0
                  MOVE 1 TO ALI-ALI
             ELSE MOVE IMP-IVA-ART01 TO ALI-ALI.
           MOVE 0 TO MM-ALI DD-ALI AA-ALI.
           OPEN INPUT ALICUO.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
                     INVALID KEY CLOSE ALICUO GO TO INI-MOD.
       CARGO-ALI1.
           READ ALICUO NEXT AT END CLOSE ALICUO GO TO CARGO-ALI-F.
           IF EMP-ALI NOT = NUM-EMP CLOSE ALICUO GO TO CARGO-ALI-F.
           IF SUC-EMP NOT = SUC-ALI CLOSE ALICUO GO TO CARGO-ALI-F.
           IF IMP-IVA-ART01 NOT = 0
              IF ALI-ALI NOT = IMP-IVA-ART01 CLOSE ALICUO 
                            GO TO CARGO-ALI-F.
           IF IMP-IVA-ART01 = 0
              IF ALI-ALI NOT = 1 CLOSE ALICUO GO TO CARGO-ALI-F.
           MOVE ANO TO ANO1.
           IF ANO1 < 99 ADD 2000 TO ANO1
               ELSE ADD 1000 TO ANO1.
           IF AA-ALI > ANO1 CLOSE ALICUO GO TO CARGO-ALI-F.
           IF AA-ALI = ANO1
               IF MM-ALI > MES CLOSE ALICUO GO TO CARGO-ALI-F.
           IF AA-ALI = ANO1
               IF MM-ALI = MES
                 IF DD-ALI > DIA CLOSE ALICUO GO TO CARGO-ALI-F.
           MOVE IVA-1-ALI TO IVA-1-CAP.
           MOVE IVA-2-ALI TO IVA-2-CAP.
           GO TO CARGO-ALI1.
       CARGO-ALI-F.
           EXIT.    
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 074720 TO TAB-POS(4).
           MOVE 102503 TO TAB-POS(5).
           MOVE 112503 TO TAB-POS(6).
           MOVE 122503 TO TAB-POS(7).
           MOVE 132503 TO TAB-POS(8).
           MOVE 142503 TO TAB-POS(9).
           MOVE 152503 TO TAB-POS(10).
           MOVE 162503 TO TAB-POS(11).
           MOVE 081502 TO TAB-POS(12)
           MOVE 083002 TO TAB-POS(13).
           MOVE 083540 TO TAB-POS(14).
           MOVE 093540 TO TAB-POS(15)
           MOVE 101510 TO TAB-POS(16).
           MOVE 121530 TO TAB-POS(17).
           MOVE 131530 TO TAB-POS(18).
           MOVE 141518 TO TAB-POS(19).
           MOVE 161518 TO TAB-POS(20).
           MOVE 201518 TO TAB-POS(21).
           MOVE 221518 TO TAB-POS(22).
           MOVE 181518 TO TAB-POS(23).
           MOVE 125520 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 125003 TO TAB-POS(29).
           MOVE 101510 TO TAB-POS(30).
           MOVE 121515 TO TAB-POS(31).
           MOVE 141530 TO TAB-POS(32).
           MOVE 171518 TO TAB-POS(33).
           MOVE 161508 TO TAB-POS(34).
           MOVE 111525 TO TAB-POS(35).
           MOVE 131508 TO TAB-POS(36). 
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO DE PRE" TO TIT-2-PAN.
           MOVE "CIOS-     " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  PRECIOS SIN IVA (Agregar iva)" 
           LINE 14 POSITION 18.
           DISPLAY "<F2>  PRECIOS CON IVA (Prog.agrega iva)" 
           LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-1.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 35 TO SW3.
           PERFORM BORRA.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           MOVE 21 TO SW3.
           PERFORM BORRA.
           MOVE 22 TO SW3.
           PERFORM BORRA.
           MOVE 23 TO SW3.
           PERFORM BORRA.
           MOVE 24 TO SW3.
           PERFORM BORRA.
           MOVE 29 TO SW3.
           PERFORM BORRA.
           DISPLAY SPACES LINE 17 POSITION 3 SIZE 60.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       CIERRO-A.
           CLOSE NUMERO MARCAS ART01 ART02 ENVASE IMP02.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
       COPY "ROT132.PRO".
