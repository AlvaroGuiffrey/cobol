       IDENTIFICATION DIVISION.
       PROGRAM-ID. DDJJ127.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. ABR'94.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *...............................(*.SEL)
       COPY "RES127.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "CPOSTAL.SEL".
       COPY "IMP02.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
      *...............................(*.REG)
       COPY "RES127.REG".
       COPY "EMPREBM.REG".
       COPY "CUENTABM.REG".
       COPY "CPOSTAL.REG".
       COPY "IMP02.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
      *...............................(ROTULO DE IMPRESION)
       COPY "ROT132.IMP".
      *...............................(LINEAS DE IMPRESION)
       01 IMPRESION.
          02 COMPRI.
             03 A01        PIC 99 COMP-4 VALUE 15.
          02 TIT-0.
             03 FILLER PIC X(30) VALUE SPACES.
             03 FILLER PIC X(24) VALUE "IMPUESTO A LOS INGRESOS ".
             03 FILLER PIC X(24) VALUE "BRUTOS        -         ".
             03 FILLER PIC X(24) VALUE "AGENTE DE PERCEPCION ".
          02 TIT-01.
             03 FILLER        PIC X(30) VALUE SPACES.
             03 FILLER        PIC X(72) VALUE ALL "~".
          02 TIT-02.
             03 FILLER        PIC X(132) VALUE ALL "-".      
          02 TIT-1.
             03 FILLER        PIC X(45) VALUE SPACES.
             03 FILLER        PIC X(20) VALUE "DECLARACION JURADA -".
             03 FILLER        PIC X(22) VALUE " Reemplaza form.DGR 87".
          02 TIT-2.
             03 RAZ-SOC-ED    PIC X(30).
             03 FILLER        PIC X(27) VALUE SPACES.
             03 FILLER        PIC X(17) VALUE "Nro.Ing.Brutos : ".
             03 ING-BRUTOS-E-ED PIC X(15).
             03 FILLER        PIC X(27) VALUE SPACES.
             03 SEC-PRO-ED    PIC X(15).   
          02 TIT-3.
             03 FILLER        PIC X(8) VALUE "Calle : ".
             03 CALLE-ED      PIC X(30).
             03 FILLER        PIC X(16) VALUE " - Cod.Postal : ".
             03 COD-LOC-ED    PIC 9(4).
             03 FILLER        PIC X(15) VALUE " - Localidad : ".
             03 NOM-POSTAL-ED PIC X(35)BB.
             03 PRO-POSTAL-ED PIC X(20).        
          02 TIT-4.
             03 FILLER PIC X(24) VALUE "PERIODO INFORMADO : del ".
             03 MM-D-ED       PIC 99/.
             03 AA-D-ED       PIC 99.
             03 FILLER        PIC X(4) VALUE " al ".
             03 MM-H-ED       PIC 99/.
             03 AA-H-ED       PIC 99.
             03 FILLER        PIC X(75) VALUE SPACES.
             03 FILLER        PIC X(12) VALUE "Hoja Nro. : ".
             03 HOJA-ED       PIC Z.ZZ9.
          02 TIT-5.
             03 FILLER        PIC X(20) VALUE "DEPOSITOS EFECTUADOS".
          02 TIT-6.
             03 FILLER        PIC X(8) VALUE "COMPR.: ".
             03 COMPRO-ED     PIC X(15).
             03 FILLER        PIC X(11) VALUE " IMPORTE $ ".
             03 IMPORTE-ED    PIC ZZZ.ZZ9,99.
             03 FILLER        PIC X(9) VALUE " FECHA : ".
             03 FECHA-ED.
                05 DIA-ED     PIC 99/.
                05 MES-ED     PIC 99/.
                05 ANO-ED     PIC 99.
             03 FILLER        PIC X(9) VALUE " BANCO : ". 
             03 BANCO-ED      PIC X(30).
          02 TIT-7.
             03 FILLER        PIC X(19) VALUE "DETALLE DE SUJETOS ".
             03 FILLER        PIC X(10) VALUE "PERCIBIDOS".      
          02 TIT-8.
             03 I-BRU-RES-ED  PIC X(15)BBB.
             03 DESC-RES-ED   PIC X(30)BBB.
             03 DIRE-ED       PIC X(30)BBB.
             03 FILLER        PIC X VALUE "(".
             03 COD-LOCA-ED   PIC 9999.
             03 FILLER        PIC X(2) VALUE ")".
             03 LOCA-ED       PIC X(30).
          02 TIT-9.
             03 FILLER        PIC X(10) VALUE SPACES.
             03 FILLER        PIC X(9) VALUE "PERIODO: ".
             03 MES-RES-ED    PIC 99/.
             03 ANO-RES-ED    PIC 99BBB.
             03 FILLER        PIC X(13) VALUE "MONTO OP.: $".
             03 IMP-RES-ED    PIC ZZZ.ZZ9,99BBB.
             03 FILLER        PIC X(16) VALUE "PERCEPCIONES: $".
             03 PER-RES-ED    PIC ZZZ.ZZ9,99.
          02 TIT-10.
             03 FILLER        PIC X(10) VALUE SPACES.
             03 DET-TOT-ED    PIC X(17).
             03 FILLER        PIC X(13) VALUE "MONTO OP.: $".
             03 IMP-TOT-ED    PIC ZZZ.ZZ9,99BBB.
             03 FILLER        PIC X(16) VALUE "PERCEPCIONES: $".
             03 PER-TOT-ED    PIC ZZZ.ZZ9,99.
          02 TIT-11.
             03 FILLER   PIC X(10) VALUE SPACES.
             03 FILLER   PIC X(20) VALUE "La informacion propo".
             03 FILLER   PIC X(20) VALUE "rcionada reviste el ".
             03 FILLER   PIC X(20) VALUE " caracter de DECLARA".
             03 FILLER   PIC X(20) VALUE "CION JURADA.- ".
          02 TIT-12.
             03 FILLER   PIC X(40) VALUE SPACES.
             03 FILLER   PIC X(20) VALUE "Firma responsable : ".
          02 TIT-13.
             03 FILLER   PIC X(40) VALUE SPACES.
             03 FILLER   PIC X(20) VALUE "Aclaracion firma  : ".
       01 CAMPOS-AUXILIARES.
          02 COD-IMP      PIC X VALUE SPACES.
          02 CTA-IMP      PIC 9(6) VALUE 0.
          02 CONT-P-AUX   PIC 999 VALUE 0.
          02 LIS          PIC X VALUE SPACES.
          02 IMP-T        PIC S9(9)V99 VALUE 0.
          02 PER-T        PIC S9(9)V99 VALUE 0.
          02 IMP-S        PIC S9(9)V99 VALUE 0.
          02 PER-S        PIC S9(9)V99 VALUE 0.
          02 LIB-CAP      PIC 9 VALUE 0.
          02 ANO-INT      PIC 99 VALUE 0.
          02 MES-INT      PIC 99 VALUE 0.
          02 DIA-INT      PIC 99 VALUE 0.
          02 FECHA-H-C.
             03 DIA-H-C   PIC 99 VALUE 0.
             03 MES-H-C   PIC 99 VALUE 0.
             03 ANO-H-C   PIC 99 VALUE 0.
          02 FECHA-H.
             03 ANO-H     PIC 99 VALUE 0.
             03 MES-H     PIC 99 VALUE 0.
             03 DIA-H     PIC 99 VALUE 0.
          02 FECHA-D.
             03 ANO-D     PIC 99 VALUE 0.
             03 MES-D     PIC 99 VALUE 0.
             03 DIA-D     PIC 99 VALUE 0.
          02 FECHA-CAP.
             03 DIA-CAP   PIC 99 VALUE 0.
             03 MES-CAP   PIC 99 VALUE 0.
             03 ANO-CAP   PIC 99 VALUE 0.
          02 FECHA-PER-CAP.
             03 MES-PER-CAP PIC 99 VALUE 0.
             03 ANO-PER-CAP PIC 99 VALUE 0.
          02 FECHA-PER-EDI.
             03 MES-PER-EDI  PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 ANO-PER-EDI  PIC 99. 
          02 D-IVA-EDI    PIC X(10).  
          02 IVA-H        PIC 9 VALUE 0.
          02 COMPRO-CAP   PIC X(15).
          02 IMPORTE-CAP  PIC 9(6)V99 VALUE 0.
          02 BANCO-CAP    PIC X(30).
          02 MES-SUM      PIC 99 VALUE 0.
          02 ANO-SUM      PIC 99 VALUE 0.
          02 IMP-SUM      PIC 9(6)V99 VALUE 0.
          02 PER-SUM      PIC 9(6)V99 VALUE 0.
          02 I-BRU-IMP    PIC X(15) VALUE SPACES.
          02 DESC-IMP     PIC X(30) VALUE SPACES.
          02 IMPORTE-EDI  PIC ZZZ.ZZ9,99.
          02 FECHA-C-EDI.
             03 DIA-C-EDI PIC 99/.
             03 MES-C-EDI PIC 99/.
             03 ANO-C-EDI PIC 99.
          02 XX           PIC X.
       COPY "VARIOS.WOR".    
       COPY "CLAVES.WOR".
       COPY "ABREV.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
      *...............................(*.ERR)
       COPY "RES127.ERR".
       COPY "EMPRE.ERR".
       COPY "CUENTA.ERR".
       COPY "CPOSTAL.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
      *...............................(ABRE PANTALLA)
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-CLAVE.
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
      *...............................(SELECCIONA TAREAS)
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "S" TO COD-IMP.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F05 MOVE "S" TO COD-IMP
                  MOVE " LISTADO DD.JJ. " TO D20-CAP
                  GO TO LISTO.
           GO TO CARGO-MOD.
       
      *................................(PROGRAMA PRINCIPAL)
       LISTO.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           OPEN INPUT EMPRESAS RES127.
           OPEN OUTPUT IMP02.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD. 
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       CARGA-LIB.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           MOVE SPACES TO LIS.
           MOVE 1 TO IVA-RES IVA-H.
       CARGA-F-D.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           MOVE 9 TO SW3. 
           PERFORM BORRA.
           ACCEPT FECHA-PER-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 PERFORM CIERRE-A 
                  GO TO LISTO.
           IF MES-PER-CAP = 0 OR MES-PER-CAP > 12 GO TO CARGA-F-D.
           MOVE MES-PER-CAP TO MES-RES MES-D MES-INT MES-PER-EDI
                       MES-SUM.
           MOVE ANO-PER-CAP TO ANO-RES ANO-D ANO-INT ANO-PER-EDI
                       ANO-SUM.
           MOVE 0 TO DIA-INT DIA-RES.
           DISPLAY FECHA-PER-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
       CARGA-F-H.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-PER-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-F-D.
           MOVE MES-PER-CAP TO MES-H MES-PER-EDI.
           MOVE ANO-PER-CAP TO ANO-H ANO-PER-EDI.
           IF MES-H = 0 OR MES-H > 12 GO TO CARGA-F-H.
           IF ANO-H < ANO-RES GO TO CARGA-F-H.
           IF ANO-H = ANO-RES
               IF MES-H < MES-RES GO TO CARGA-F-H.
           DISPLAY FECHA-PER-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
       LISTO-ENCA01.
           MOVE 0 TO CONT-R.
           MOVE 1 TO CONT-P.
           MOVE CONT-P TO HOJA-ED.
           MOVE MES-D TO MM-D-ED.
           MOVE ANO-D TO AA-D-ED.
           MOVE MES-H TO MM-H-ED.
           MOVE ANO-H TO AA-H-ED.
           MOVE ING-BRUTOS-EMP TO ING-BRUTOS-E-ED.
           IF SEC-PRO-EMP = 1 OR SEC-PRO-EMP = 2
              MOVE "-MAYORISTA-" TO SEC-PRO-ED.
           IF SEC-PRO-EMP = 3 MOVE "-MANUFACTURERO-" TO SEC-PRO-ED.
           IF SEC-PRO-EMP = 4 MOVE "-INDUSTRIAL-" TO SEC-PRO-ED.
           IF SEC-PRO-EMP > 4 OR SEC-PRO-EMP = 0
            MOVE SPACES TO SEC-PRO-ED.
           MOVE CALLE-EMP TO CALLE-ED.
           MOVE COD-LOC-EMP TO C-POS COD-LOC-ED.
           OPEN INPUT CPOSTAL.
           READ CPOSTAL INVALID KEY 
               MOVE "**********" TO NOM-POSTAL PRO-POSTAL.
           CLOSE CPOSTAL.
           MOVE NOM-POSTAL TO NOM-POSTAL-ED.
           MOVE PRO-POSTAL TO PRO-POSTAL-ED.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-01 AFTER 1.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-1 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-1 AFTER 0.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-2 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-3 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-4 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-5 AFTER 1.
           ADD 10 TO CONT-R.          
       CARGA-DEP.
           MOVE SPACES TO COD-IMP.
           DISPLAY "------ DEPOSITOS EFECTUADOS -------" LINE 18
                 POSITION 5.
           DISPLAY "COMPROBANTE  : " LINE 19 POSITION 5.
           DISPLAY "IMPORTE DEP. : " LINE 20 POSITION 5.
           DISPLAY "FECHA DEP.   : " LINE 21 POSITION 5.
           DISPLAY "BANCO RECAUD.: " LINE 22 POSITION 5.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
       CARGA-DEP1.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM MEN-ANTE.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 COMPUTE CONT-R = 72 - CONT-R
                  PERFORM ESPA CONT-R TIMES
                  GO TO CARGA-F-H.       
           MOVE ALF-CAP TO COMPRO-CAP COMPRO-ED.
       CARGA-DEP2.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           ACCEPT IMPORTE-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-DEP1.
           MOVE IMPORTE-CAP TO IMPORTE-EDI IMPORTE-ED.
           DISPLAY IMPORTE-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
       CARGA-DEP3.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           ACCEPT FECHA-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-DEP2.
           IF DIA-CAP > 31 OR DIA-CAP = 0 GO TO CARGA-DEP3.
           IF MES-CAP > 12 OR MES-CAP = 0 GO TO CARGA-DEP3.
           MOVE DIA-CAP TO DIA-C-EDI DIA-ED.
           MOVE MES-CAP TO MES-C-EDI MES-ED.
           MOVE ANO-CAP TO ANO-C-EDI ANO-ED.
           DISPLAY FECHA-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
       CARGA-DEP4.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-ALF.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-DEP3.
           MOVE ALF-CAP TO BANCO-ED.
       CARGA-DEP5.
           MOVE "DIGITE <F1> SIG" TO MEN-1.
           MOVE "UE <F3> OTRO DE" TO MEN-2.
           MOVE "POSITO         " TO MEN-3.
           PERFORM MENS-PAN.
           MOVE 28 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) TAB CONVERT
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F03 MOVE "O" TO COD-IMP
                  GO TO CARGA-DEP6.
           IF F01 MOVE "S" TO COD-IMP
                  GO TO CARGA-DEP6.
           GO TO CARGA-DEP5.
       CARGA-DEP6.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
           WRITE LIN-IMP02 FROM TIT-6 AFTER 1.
           ADD 1 TO CONT-R.
           IF COD-IMP = "O" GO TO CARGA-DEP.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-7 AFTER 1.
           PERFORM ESPA 1 TIMES.
           ADD 2 TO CONT-R.
       SIGA-L.
           MOVE 0 TO DIA-INT CONT-P-AUX CTA-IMP.
           MOVE 0 TO MOV-RES IMP-T PER-T IMP-S PER-S IMP-SUM PER-SUM.
           MOVE SPACES TO I-BRU-RES DESC-RES I-BRU-IMP DESC-IMP.
           MOVE NUM-EMP TO EMP-RES.
           MOVE SUC-EMP TO SUC-RES.
           START RES127 KEY IS NOT < CLAVE-RES
                 INVALID KEY PERFORM CIERRE-A
                             GO TO INI-MOD.
           MOVE "N" TO XX.
       LISTO-1.
           IF CONT-R > 63 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1 
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU
                          PERFORM TRANS.
           READ RES127 NEXT AT END GO TO CIERRO.
           IF EMP-RES NOT = NUM-EMP GO TO CIERRO.
           IF SUC-RES NOT = SUC-EMP GO TO CIERRO.
           IF IVA-RES NOT = IVA-H
              GO TO CIERRO.
           IF ANO-RES < ANO-D GO TO LISTO-1.
           IF ANO-RES = ANO-D
             IF MES-RES < MES-D GO TO LISTO-1.
           IF ANO-RES > ANO-H GO TO LISTO-1.
           IF ANO-RES = ANO-H
              IF MES-RES > MES-H GO TO LISTO-1.
           IF I-BRU-RES NOT = I-BRU-IMP
               PERFORM IMPRE-8 THRU IMPRE-8-F.
           IF I-BRU-RES = I-BRU-IMP
               IF DESC-RES NOT = DESC-IMP
                PERFORM IMPRE-8 THRU IMPRE-8-F.         
           IF ANO-RES NOT = ANO-SUM
              PERFORM IMPRE-9 THRU IMPRE-9-F.
           IF ANO-RES = ANO-SUM
              IF MES-RES NOT = MES-SUM
                 PERFORM IMPRE-9 THRU IMPRE-9-F.
           COMPUTE IMP-T = IMP-T + IMP-RES.
           COMPUTE IMP-S = IMP-S + IMP-RES.
           COMPUTE IMP-SUM = IMP-SUM + IMP-RES.
           COMPUTE PER-T = PER-T + PER-RES.
           COMPUTE PER-S = PER-S + PER-RES.
           COMPUTE PER-SUM = PER-SUM + PER-RES.
           GO TO LISTO-1.
       IMPRE-8.
           IF I-BRU-IMP = SPACES AND DESC-IMP = SPACES
              MOVE I-BRU-RES TO I-BRU-IMP I-BRU-RES-ED
              MOVE DESC-RES TO DESC-IMP DESC-RES-ED
              MOVE ANO-RES TO ANO-SUM
              MOVE MES-RES TO MES-SUM
              GO TO IMPRE-8-S.
           PERFORM IMPRE-LIN.
           MOVE 0 TO MES-SUM ANO-SUM PER-SUM IMP-SUM.
           MOVE I-BRU-RES TO I-BRU-IMP I-BRU-RES-ED.
           MOVE DESC-RES TO DESC-IMP DESC-RES-ED.
           IF CONT-R > 63 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1 
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU
                          PERFORM TRANS.
           MOVE ANO-RES TO ANO-SUM
           MOVE MES-RES TO MES-SUM.
       IMPRE-8-S.
           MOVE NUM-EMP TO EMP-CTA.
           MOVE SUC-EMP TO SUC-CTA.
           MOVE RUBRO-RES TO RUBRO-CTA.
           MOVE CUE-RES TO CUENTA.
           MOVE 0 TO SUB-CUENTA.
           OPEN INPUT CUENTAS.
           READ CUENTAS INVALID KEY MOVE SPACES TO DIRE
                                    MOVE 0 TO COD-LOCA.
           CLOSE CUENTAS.    
           OPEN INPUT CPOSTAL.
           MOVE COD-LOCA TO C-POS.
           READ CPOSTAL INVALID KEY MOVE SPACES TO NOM-POSTAL.
           CLOSE CPOSTAL.
           MOVE DIRE TO DIRE-ED.
           MOVE COD-LOCA TO COD-LOCA-ED.
           MOVE NOM-POSTAL TO LOCA-ED.
           IF CAT-RES = 2 MOVE "NO INSCRIPTO" TO I-BRU-RES-ED.
           WRITE LIN-IMP02 FROM TIT-8 AFTER 1.
           ADD 1 TO CONT-R.
       IMPRE-8-F.
           EXIT.
       IMPRE-9.
           PERFORM IMPRE-LIN.
           MOVE 0 TO MES-SUM ANO-SUM PER-SUM IMP-SUM.
           MOVE ANO-RES TO ANO-SUM
           MOVE MES-RES TO MES-SUM.
           IF CONT-R > 63 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1 
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU
                          PERFORM TRANS.
       IMPRE-9-F.
           EXIT.
       IMPRE-LIN.
           MOVE ANO-SUM TO ANO-RES-ED.
           MOVE MES-SUM TO MES-RES-ED.
           MOVE IMP-SUM TO IMP-RES-ED.
           MOVE PER-SUM TO PER-RES-ED.
           WRITE LIN-IMP02 FROM TIT-9 AFTER 1.
           ADD 1 TO CONT-R.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJA-ED.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-01 AFTER 1.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-1 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-1 AFTER 0.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-2 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-3 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-4 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-7 AFTER 1.
           PERFORM ESPA 1 TIMES.
           ADD 10 TO CONT-R.
       TRANS.
           MOVE "SUBTOTAL --->" TO DET-TOT-ED.
           MOVE PER-S TO PER-TOT-ED.
           MOVE IMP-S TO IMP-TOT-ED.
           WRITE LIN-IMP02 FROM TIT-10 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-10 AFTER 0.
           PERFORM ESPA 1 TIMES.
           ADD 1 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       CIERRO.
           PERFORM IMPRE-9 THRU IMPRE-9-F.
           IF CONT-R > 55 WRITE LIN-IMP02 FROM ROT132-1 AFTER 1 
                          ADD 1 TO CONT-R
                          PERFORM TRANS
                          COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU
                          MOVE 0 TO PER-S IMP-S.
           MOVE IMP-T TO IMP-TOT-ED.
           MOVE PER-T TO PER-TOT-ED.
           MOVE "TOTALES --->" TO DET-TOT-ED.
           MOVE 0 TO PER-T PER-S IMP-T IMP-S.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-10 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-10 AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           PERFORM ESPA 2 TIMES.
           WRITE LIN-IMP02 FROM TIT-11 AFTER 1.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-12 AFTER 1.
           PERFORM ESPA 1 TIMES.
           WRITE LIN-IMP02 FROM TIT-13 AFTER 1.
           ADD 6 TO CONT-R.
              COMPUTE CONT-R = 72 - CONT-R
              PERFORM ESPA CONT-R TIMES.
       CIERRO-F.
           GO TO CARGA-LIB.
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
           MOVE NUM-CAP TO NUM-EMP EMP-CAP EMP-RES.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132 RAZ-SOC-ED.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP SUC-RES NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
       CARGA-EMP-F.
           EXIT.
       CIERRE-A.
           CLOSE IMP02 RES127 EMPRESAS.
      *................................(CARGA DATOS)

      *................................(DESPLEGA DATOS)

       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 101601 TO TAB-POS(7).
           MOVE 102010 TO TAB-POS(8).
           MOVE 142806 TO TAB-POS(9).
           MOVE 162806 TO TAB-POS(10).
           MOVE 182801 TO TAB-POS(11).
           MOVE 202806 TO TAB-POS(12).
           MOVE 192015 TO TAB-POS(13).
           MOVE 202015 TO TAB-POS(14).
           MOVE 212015 TO TAB-POS(15).
           MOVE 222015 TO TAB-POS(16). 
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE " - ARCHIVO" TO TIT-1-PAN.
           MOVE " DE  RES. " TO TIT-2-PAN.
           MOVE " 127/83 - " TO TIT-3-PAN.
       PANTA-1.
           DISPLAY "EMPRESA  :           SUC:       -" LINE 7 POSITION 5.
           DISPLAY "------------ PERIODO A INFORMAR ------------"
                      LINE 13 POSITION 5.
           DISPLAY "FECHA DESDE <MMAA>   : " LINE 14 POSITION 5.
           DISPLAY "FECHA HASTA <MMAA>   : " LINE 16 POSITION 5.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO DD.JJ.  " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "ROT132.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
           COPY "CLAVES.PRO".
