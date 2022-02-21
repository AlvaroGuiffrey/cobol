       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISIMP.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. OCT'92.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "MANEJO.SEL".
       COPY "EMPRE.SEL".
       COPY "IMPU.SEL".
       COPY "ACTI.SEL".
       COPY "CPOSTAL.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "MANEJO.REG".
       COPY "EMPREBM.REG".
       COPY "IMPU.REG".
       COPY "ACTI.REG".
       COPY "CPOSTAL.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(11) VALUE "EMPRESA  :".
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 NOM-EMP-ED   PIC X(30).
             03 FILLER       PIC X(2) VALUE " (".
             03 NUM-EMP-ED   PIC ZZZ9.
             03 FILLER       PIC XX VALUE ")".
             03 FILLER       PIC 99 COMP-4 VALUE 15.
             03 FILLER       PIC X(30) VALUE SPACES.
             03 FILLER       PIC X(13) VALUE "PROCESO DEL :".
             03 DD-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 MM-ED        PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 AA-ED        PIC 99.
          02 IMP-2.
             03 FILLER       PIC X(11) VALUE "SUCURSAL :".
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 NOM-SUC-ED   PIC X(30).
             03 FILLER       PIC X(2) VALUE " (".
             03 NUM-SUC-ED   PIC 99.
             03 FILLER       PIC X(2) VALUE ")".
             03 FILLER       PIC 99 COMP-4 VALUE 15.
          02 IMP-21.
             03 FILLER       PIC X(11) VALUE "DOMICILIO:".
             03 CALLE-ED     PIC X(30)BB.
             03 FILLER       PIC X VALUE "(".
             03 COD-LOC-ED   PIC 9999.
             03 FILLER       PIC X(3) VALUE ")".
             03 NOM-LOC-ED   PIC X(35)BB.
             03 FILLER       PIC X(3) VALUE "-".
             03 PRO-LOC-ED   PIC X(20).
          02 IMP-3.
             03 FILLER       PIC X(130) VALUE ALL "=".
          02 IMP-4.
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 NOM-IMP-ED   PIC X(30).
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "CONTRIBUYENTE: ".
             03 NUM-IMP-ED   PIC X(15).
             03 FILLER       PIC 99 COMP-4 VALUE 15.
          02 IMP-5.
             03 FILLER       PIC X(48) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "PERIODO DEL :".
             03 MM-D-ED      PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 AA-D-ED      PIC 99BBBBB.
             03 FILLER       PIC X(5) VALUE "AL :".
             03 MM-H-ED      PIC 99.
             03 FILLER       PIC X VALUE "/".
             03 AA-H-ED      PIC 99.
          02 IMP-6.
             03 FILLER       PIC X(130) VALUE ALL "=".
          02 IMP-7.
             03 FILLER       PIC X(17) VALUE "------------ ACTI".
             03 FILLER       PIC X(23) VALUE "VIDAD -----------".
             03 FILLER       PIC X(13) VALUE "ALICUOTA".
             03 FILLER       PIC X(18) VALUE "MONTO IMPONIBLE".
             03 FILLER       PIC X(18) VALUE "VALOR CALCULADO".
             03 FILLER       PIC X(10) VALUE "CATEG.".
             03 FILLER       PIC X(15) VALUE "--------- COMEN".
             03 FILLER       PIC X(15) VALUE "TARIO ---------".
          02 IMP-8.
             03 NUM-ACT-ED   PIC ZZ9B.
             03 FILLER       PIC XX VALUE "-".
             03 DET-ACT-ED   PIC X(30)BBBBBB.
             03 POR-IMP-ED   PIC Z9,999B(5).
             03 NET-IMP-ED   PIC --.---.--9,99B(5).
             03 VAL-IMP-ED   PIC --.---.--9,99B(5).
             03 CAT-IMP-ED   PIC X(5)B(5).
             03 COM-IMP-ED   PIC ZZZZZZZBBB.
             03 COM-ACT-ED   PIC X(20).
          02 IMP-9.
             03 FILLER       PIC X(130) VALUE ALL "-".
          02 IMP-10.
             03 FILLER       PIC X(15) VALUE SPACES.
             03 FILLER       PIC X(11) VALUE "SUBTOTALES ".
             03 LACT-ED      PIC X(17).
             03 FILLER       PIC X(10) VALUE " ".
             03 TOT-NET-ED   PIC --.---.--9,99B(5).
             03 TOT-VAL-ED   PIC --.---.--9,99.
          02 IMP-11.
             03 FILLER       PIC X(50) VALUE SPACES.
             03 FILLER       PIC X(34) VALUE ALL "-".
          02 IMP-12.
             03 FILLER       PIC X(15) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "MINIMO---------".
             03 FILLER       PIC X(23) VALUE "-------------->".
             03 FILLER       PIC X(18) VALUE SPACES.
             03 MIN-ED       PIC --.---.--9,99.
          02 IMP-13.
             03 FILLER       PIC X(15) VALUE SPACES.
             03 FILLER       PIC X(15) VALUE "TOTAL CALCULADO".
             03 FILLER       PIC X(23) VALUE "-------------->".
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC 99 COMP-4 VALUE 18.
             03 TOT-CAL-ED   PIC --.---.--9,99.
             03 FILLER       PIC 99 COMP-4 VALUE 15.
          02 IMP-99.
             03 FILLER       PIC X(10) VALUE SPACES.
             03 FILLER       PIC X(17) VALUE "<<< SIN ACTIVIDAD".
             03 FILLER       PIC X(17) VALUE " >>>  Toma ultimo".
             03 FILLER       PIC X(17) VALUE " MINIMO (VERIFICA".
             03 FILLER       PIC X(10) VALUE "R)".
       01 COD-OPE            PIC X.
       01 LISTO-I-D          PIC 9.
       01 LISTO-D            PIC 999.
       01 LISTO-H            PIC 999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 L-EMP-D            PIC 9(4) VALUE 0.
       01 L-EMP-H            PIC 9(4) VALUE 0.
       01 L-SUC-D            PIC 99 VALUE 0.
       01 L-SUC-H            PIC 99 VALUE 0.
       01 ESTA-ACT           PIC 999 VALUE 0.
       01 DET                PIC X VALUE "S".
       01 ELCORTE            PIC 9 VALUE 0.
       01 FEC-D.
          02 MM-D            PIC 99.
          02 AA-D            PIC 99.
       01 FEC-H.
          02 MM-H            PIC 99.
          02 AA-H            PIC 99.
       01 FEC-EDI.
          02 MM-EDI          PIC ZZ.
          02 FILLER          PIC X VALUE "/".
          02 AA-EDI          PIC ZZ.
       01 VAL-INT            PIC S9(12)V99 VALUE 0.
       01 MIN-INT            PIC S9(12)V99 VALUE 0.
       01 MIN-USO            PIC 9 VALUE 0.
       01 TOT-NET            PIC S9(12)V99 VALUE 0.
       01 TOT-VAL            PIC S9(12)V99 VALUE 0.
       01 TOT-CAL            PIC S9(12)V99 VALUE 0.
       01 DEP-INT            PIC S9(12)V99 VALUE 0.
       01 IMP-INT            PIC 999.
       01 POR-INT            PIC 99V999.
       01 POR-EDI            PIC Z9,999.
       01 VAL-1-EDI          PIC ZZZZZ.ZZ9,99.
       01 VAL-2-EDI          PIC ZZZZZ.ZZ9,99.
       01 MEN-C-EDI          PIC X(30).
       01 COD-ACT            PIC X VALUE SPACES.
       01 IND-ORD            PIC 9.
       01 A                  PIC 99 VALUE 0.
       01 DONDE              PIC X.
       01 ANTE.
          03 ANT             PIC X OCCURS 30 TIMES.
       01 CAPTU.
          03 CAP             PIC X OCCURS 30 TIMES.
       01 CONTA-AUX.
          02 C-A-1          PIC 99 VALUE 0.
          02 C-A-2          PIC 99 VALUE 0.
          02 C-A-3          PIC 99 VALUE 0.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "MANEJO.ERR".
       COPY "EMPRE.ERR".
       COPY "IMPU.ERR".
       COPY "ACTI.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
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
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       CARGA-COD.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           DISPLAY " 2 - ING.BRUTOS-DGR " LINE 8 POSITION 40 REVERSE.
           DISPLAY " 3 - TASA MUNICIPAL " LINE 9 POSITION 40 REVERSE.
           PERFORM MENS-PAN.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE ACTI GO TO INI-MOD.
           IF NUM-CAP < 2 OR NUM-CAP > 3 GO TO CARGA-COD.
           MOVE NUM-CAP TO IMP-ACT.
           IF IMP-ACT = 2 
             DISPLAY " 2 - ING.BRUTOS-DGR " LINE 8 POSITION 40 
                      BLINK REVERSE.
           IF IMP-ACT = 3
             DISPLAY " 3 - TASA MUNICIPAL " LINE 9 POSITION 40
                      BLINK REVERSE.
       LIST-NUM.
           MOVE 0 TO CONT-P CONT-R CONT-I.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "IMPUESTO\TASA :" LINE 7 POSITION 3.
           DISPLAY "DESDE EMPRESA : " LINE 9 POSITION 3.
           DISPLAY "- SUC.:" LINE 9 POSITION 28.
           DISPLAY "HASTA EMPRESA : " LINE 11 POSITION 3.
           DISPLAY "- SUC.:" LINE 11 POSITION 28.
           DISPLAY "--------- PERIODO ---------" LINE 14 POSITION 3.
           DISPLAY "DESDE (MMAA)  : " LINE 16 POSITION 3.
           DISPLAY "HASTA (MMAA)  : " LINE 18 POSITION 3.
           DISPLAY "USO MINIMO    : " LINE 20 POSITION 3.
           DISPLAY " 1 - MINIMO GRABADO   " LINE 19 POSITION 25 REVERSE.
           DISPLAY " 2 - ULTIMO MINIMO    " LINE 20 POSITION 25 REVERSE.
           DISPLAY " 3 - ACT.MINIMO GRAB. " LINE 21 POSITION 25 REVERSE.
       L-N-COD.
           PERFORM MEN-ANTE.
           PERFORM BORRA-PANTA-C.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO INI-MOD.
           IF NUM-CAP = 2 GO TO L-N-CODS.
           IF NUM-CAP = 3 GO TO L-N-CODS.
           IF NUM-CAP = 8 GO TO L-N-CODS.
           IF NUM-CAP = 9 GO TO L-N-CODS.
           GO TO L-N-COD.
       L-N-CODS.
           MOVE NUM-CAP TO IMP-ACT IMP-INT IMP-IMP.
           MOVE 9 TO SW3.
           IF IMP-ACT = 2
              MOVE "2 : ING.BRUTOS-DGR" TO NOM-IMP-ED 
              DISPLAY " ING.BRUTOS-DGR " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.
           IF IMP-ACT = 3
              MOVE "3 : TASA MUNICIPAL" TO NOM-IMP-ED
              DISPLAY " TASA MUNICIPAL " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.
           IF IMP-ACT = 8
              MOVE "8 : COMPRAS       " TO NOM-IMP-ED
              DISPLAY " COMPRAS        " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.   
           IF IMP-ACT = 9
              MOVE "9 : OT.GASTOS     " TO NOM-IMP-ED
              DISPLAY " OT.GASTOS      " LINE TAB-LIN(SW3)
                     POSITION TAB-COL(SW3) BLINK REVERSE.
       L-N-DESDE.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-COD.
           MOVE NUM-CAP TO L-EMP-D NUM-EMP MA-EMP.
           OPEN INPUT EMPRESAS MANEJO.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID CLOSE EMPRESAS GO TO L-N-DESDE.
           READ MANEJO INVALID KEY DISPLAY " ".
           CLOSE EMPRESAS MANEJO.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO NOM-EMP-ED.
       L-N-SUC-D.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO SUC-EMP L-SUC-D.
           OPEN INPUT EMPRESAS.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS GO TO L-N-SUC-D.
           CLOSE EMPRESAS.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-SUC-D.
           MOVE NUM-CAP TO L-EMP-H NUM-EMP.
           MOVE 0 TO SUC-EMP.
           OPEN INPUT EMPRESAS.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS GO TO L-N-HASTA.
           CLOSE EMPRESAS.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
       L-N-SUC-H.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-HASTA.
           MOVE NUM-CAP TO L-SUC-H SUC-EMP.
           OPEN INPUT EMPRESAS.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS GO TO INI-MOD.
           CLOSE EMPRESAS.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3).
       L-N-F-D.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-D LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F02 GO TO L-N-SUC-H.
           IF ESCAP GO TO INI-MOD.
           MOVE AA-D TO AA-EDI
           MOVE MM-D TO MM-EDI
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
       L-N-F-H.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           ACCEPT FEC-H LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF F02 GO TO L-N-F-D.
           IF ESCAP GO TO INI-MOD.
           MOVE AA-H TO AA-EDI.
           MOVE MM-H TO MM-EDI.
           DISPLAY FEC-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3).
       L-N-MIN.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           ACCEPT MIN-USO LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-F-H.
           IF MIN-USO < 1 OR MIN-USO > 3
              GO TO L-N-MIN.

           DISPLAY "DETALLADO ? (S)..:" LINE 23 POSITION 3 SIZE 20
           ACCEPT DET LINE 23 POSITION 23.

       L-N-POSI.
           OPEN OUTPUT IMP02.
           OPEN INPUT ACTI IMPU EMPRESAS.
           MOVE L-EMP-D TO NUM-EMP EMP-IMP.
           MOVE L-SUC-D TO SUC-EMP SUC-IMP.
       L-N-POSI1.
           START EMPRESAS KEY IS NOT < CLAVE-EMP
                   INVALID KEY PERFORM CIERRE-A GO TO L-N-DESDE.
       L-N-EMPRE.
           READ EMPRESAS NEXT AT END PERFORM CIERRE-A GO TO LIST-NUM.
           IF NUM-EMP > L-EMP-H PERFORM CIERRE-A GO TO LIST-NUM.
           IF NUM-EMP = L-EMP-H
             IF SUC-EMP > L-SUC-H PERFORM CIERRE-A GO TO LIST-NUM.
           MOVE AA-D TO AA-IMP AA-D-ED.
           MOVE MM-D TO MM-IMP MM-D-ED.
           MOVE AA-H TO AA-H-ED.
           MOVE MM-H TO MM-H-ED.
           MOVE NUM-EMP TO EMP-IMP NUM-EMP-ED.
           MOVE SUC-EMP TO SUC-IMP NUM-SUC-ED.
           MOVE CALLE-EMP TO CALLE-ED.
           MOVE COD-LOC-EMP TO COD-LOC-ED C-POS.
           OPEN INPUT CPOSTAL.
           READ CPOSTAL INVALID KEY MOVE "******" TO NOM-POSTAL
                           MOVE "******" TO PRO-POSTAL.
           CLOSE CPOSTAL.
           MOVE NOM-POSTAL TO NOM-LOC-ED.
           MOVE PRO-POSTAL TO PRO-LOC-ED.
           IF SUC-EMP = 0 MOVE RAZ-SOC-EMP TO NOM-EMP-ED
               ELSE MOVE RAZ-SOC-EMP TO NOM-SUC-ED.
           MOVE 0 TO NUM-IMP-ED ACT-ACT.
           IF IMP-IMP = 8 GO TO POSI-LEO.
           IF IMP-IMP = 9 GO TO POSI-LEO.
           IF IMP-IMP = 2 MOVE ING-BRUTOS-EMP TO NUM-IMP-ED
               MOVE ACT-PRO-EMP TO ACT-ACT.
           IF IMP-IMP = 3 MOVE MUNI-EMP TO NUM-IMP-ED
               MOVE ACT-MUN-EMP TO ACT-ACT.
           MOVE IMP-IMP TO IMP-ACT.
           READ ACTI INVALID KEY MOVE 0 TO VAL-2-ACT.
           MOVE VAL-2-ACT TO MIN-INT.
       POSI-LEO.
           MOVE 0 TO CONT-I CONT-R TOT-NET TOT-VAL VAL-INT MIN-INT.
           MOVE 0 TO ACT-IMP.
           MOVE "N" TO COD-ACT.
           START IMPU KEY IS NOT < CLAVE-IMPU
                        INVALID KEY PERFORM CIERRE-A
                        GO TO LIST-NUM.
       L-LEO.
           READ IMPU NEXT AT END 
                        PERFORM VEO-TITU THRU VEO-TITU-F
                        PERFORM PIE-P THRU PIE-P-F
                        GO TO L-N-EMPRE.
           IF EMP-IMP NOT = NUM-EMP
                           PERFORM VEO-TITU THRU VEO-TITU-F
                           PERFORM PIE-P THRU PIE-P-F
                           GO TO L-N-EMPRE.
           IF EMP-IMP = NUM-EMP
              IF SUC-EMP NOT = SUC-IMP
                           PERFORM VEO-TITU THRU VEO-TITU-F
                           PERFORM PIE-P THRU PIE-P-F
                           GO TO L-N-EMPRE.
           IF IMP-IMP NOT = IMP-INT
                           PERFORM VEO-TITU THRU VEO-TITU-F 
                           PERFORM PIE-P THRU PIE-P-F
                           GO TO L-N-EMPRE.
           IF AA-IMP < AA-D GO TO L-LEO.
           IF AA-IMP = AA-D
             IF MM-IMP < MM-D GO TO L-LEO.
           IF AA-IMP > AA-H GO TO L-LEO.
           IF AA-IMP = AA-H
             IF MM-IMP > MM-H GO TO L-LEO.

           IF ESTA-ACT = 0 MOVE ACT-IMP TO ESTA-ACT.
           MOVE 0 TO ELCORTE
           IF ESTA-ACT NOT = ACT-IMP 
                           PERFORM VEO-TITU THRU VEO-TITU-F 
                           MOVE 9 TO ELCORTE
                           PERFORM PIE-P THRU PIE-P-F
                           MOVE ACT-IMP TO ESTA-ACT
                           MOVE 0 TO VAL-INT TOT-NET TOT-VAL.

           IF CONT-R = 0 PERFORM TITU.
           IF CONT-R = 30 PERFORM ESPA 6 TIMES
                     MOVE 0 TO CONT-R.
           MOVE ACT-IMP TO ACT-ACT.
           MOVE IMP-IMP TO IMP-ACT. 
           READ ACTI INVALID KEY MOVE "*******" TO DET-ACT.
           IF IMP-IMP = 2
               IF ACT-ACT = ACT-PRO-EMP MOVE "S" TO COD-ACT.
           IF IMP-IMP = 3
               IF ACT-ACT = ACT-MUN-EMP MOVE "S" TO COD-ACT.
           IF COD-ACT = "S"
             IF MIN-USO = 1
                MOVE MIN-IMP TO MIN-INT.
           IF COD-ACT = "S"
             IF MIN-USO = 2
                MOVE VAL-2-ACT TO MIN-INT.
           IF COD-ACT = "S"
             IF MIN-USO = 3 MOVE VAL-2-ACT TO MIN-INT.
           IF IMP-IMP = 8 OR IMP-IMP = 9
              IF MIN-USO = 3
                 MOVE 0 TO POR-IMP.
           IF MIN-USO NOT = 3 GO TO LEO-SI1.
           IF MIN-USO = 3
             MOVE VAL-2-ACT TO MIN-IMP
             CLOSE IMPU
             OPEN I-O IMPU
             REWRITE REG-IMPU 
             CLOSE IMPU
             OPEN INPUT IMPU
             START IMPU KEY IS = CLAVE-IMPU
               INVALID KEY GO TO LEO-SI.
      *                     PERFORM VEO-TITU THRU VEO-TITU-F 
      *                     PERFORM PIE-P THRU PIE-P-F
      *                     GO TO L-N-EMPRE.
       LEO-SI.
           READ IMPU NEXT AT END PERFORM VEO-TITU THRU VEO-TITU-F
                                 PERFORM PIE-P THRU PIE-P-F
                                 GO TO L-N-EMPRE.
       LEO-SI1.
           MOVE ACT-ACT TO NUM-ACT-ED. 
           MOVE DET-ACT TO DET-ACT-ED LACT-ED.
           MOVE POR-IMP TO POR-IMP-ED.
           MOVE NET-IMP TO NET-IMP-ED.
           MOVE COM-ACT TO COM-ACT-ED.
           MOVE SPACES TO CAT-IMP-ED.
           IF COD-ACT = "S" MOVE "PRINC" TO CAT-IMP-ED
                  MOVE "N" TO COD-ACT.
           IF CAL-IMP < 2
             COMPUTE VAL-INT ROUNDED = NET-IMP * POR-IMP / 100.
           IF CAL-IMP = 2 MOVE VAL-1-ACT TO VAL-INT.
           IF CAL-IMP > 2 MOVE NET-IMP TO VAL-INT
                          MOVE 0 TO NET-IMP-ED.
           MOVE VAL-INT TO VAL-IMP-ED.
           IF CAL-IMP < 3
                 COMPUTE TOT-NET = TOT-NET + NET-IMP.
           COMPUTE TOT-VAL = TOT-VAL + VAL-INT
           MOVE COMPRO-IMP   TO COM-IMP-ED
           IF DET = "S" OR DET = "s" 
              WRITE LIN-IMP02 FROM IMP-8 AFTER ADVANCING 1
              ADD 1 TO CONT-R
              ADD 1 TO CONT-I.
           GO TO L-LEO.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           MOVE ANO TO AA-E132 AA-ED.
           MOVE MES TO MM-E132 MM-ED.
           MOVE DIA TO DD-E132 DD-ED.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E132.
           MOVE MINU TO MI-E132.
           WRITE LIN-IMP02 FROM NORMAL AFTER 0.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-21 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-5 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-6 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM IMP-7 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-9 AFTER 1.
           ADD 9 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           IF CONT-R > 23 COMPUTE CONT-R = 36 - CONT-R
                    PERFORM ESPA CONT-R TIMES
                    MOVE 0 TO CONT-R.
           IF CONT-R = 0 GO TO PIE-P-F.
           WRITE LIN-IMP02 FROM IMP-9 AFTER 1.
           MOVE TOT-NET TO TOT-NET-ED.
           MOVE TOT-VAL TO TOT-VAL-ED.
           WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
           PERFORM ESPA.
           MOVE MIN-INT TO MIN-ED.
           WRITE LIN-IMP02 FROM IMP-12 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-11 AFTER 1.
           IF TOT-VAL > MIN-INT MOVE TOT-VAL TO TOT-CAL-ED
               ELSE MOVE MIN-INT TO TOT-CAL-ED.
           WRITE LIN-IMP02 FROM IMP-13 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-11 AFTER 1.
           ADD 6 TO CONT-R.
           IF ELCORTE NOT = 9            
               COMPUTE CONT-R = 36 - CONT-R
               PERFORM ESPA CONT-R TIMES
               MOVE 0 TO CONT-R.
           MOVE 0 TO ELCORTE.
       PIE-P-F.
           EXIT.
       VEO-TITU.
           IF CONT-R > 0 GO TO VEO-TITU-F.
           PERFORM TITU.
           MOVE 0 TO TOT-NET TOT-VAL.
           WRITE LIN-IMP02 FROM IMP-99 AFTER 1.
       VEO-TITU-F.
           EXIT.
       CIERRE-A.
           CLOSE EMPRESAS ACTI IMPU IMP02.
       LEO-MAR.
           READ ACTI NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 082301 TO TAB-POS(3).
           MOVE 122302 TO TAB-POS(4).
           MOVE 142330 TO TAB-POS(5).
           MOVE 172306 TO TAB-POS(6).
           MOVE 162301 TO TAB-POS(7).
           MOVE 071901 TO TAB-POS(8).
           MOVE 072230 TO TAB-POS(9).
           MOVE 091904 TO TAB-POS(10).
           MOVE 093602 TO TAB-POS(11).
           MOVE 094030 TO TAB-POS(12)
           MOVE 111904 TO TAB-POS(13).
           MOVE 113602 TO TAB-POS(14).
           MOVE 114030 TO TAB-POS(15)
           MOVE 161905 TO TAB-POS(16).
           MOVE 181905 TO TAB-POS(17).
           MOVE 201901 TO TAB-POS(18).
           MOVE 182312 TO TAB-POS(19).
           MOVE 192312 TO TAB-POS(20).
           MOVE 133330 TO TAB-POS(21).
           MOVE 143330 TO TAB-POS(22).
           MOVE 153330 TO TAB-POS(23).
           MOVE 163330 TO TAB-POS(24).
           MOVE 173330 TO TAB-POS(25).
           MOVE 183330 TO TAB-POS(26).
           MOVE 193330 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071404 TO TAB-POS(29).
           MOVE 072602 TO TAB-POS(30).
           MOVE 073230 TO TAB-POS(31).
           MOVE 081006 TO TAB-POS(34).
           MOVE 084706 TO TAB-POS(35).
           MOVE 191430 TO TAB-POS(36).
       TIT-PAN.
           MOVE "-ARCHIVO D" TO TIT-1-PAN.
           MOVE "E IMPUESTO" TO TIT-2-PAN.
           MOVE "S Y OTROS-" TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO NUMERICO" LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
