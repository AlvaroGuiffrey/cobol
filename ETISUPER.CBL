       IDENTIFICATION DIVISION.
       PROGRAM-ID. ETISUP.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. JUN'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ART02.SEL".
       COPY "NUMERO.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "NUMERO.REG".
       COPY "ART02.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 A1           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X VALUE "É".
             03 FILLER       PIC X(62) VALUE ALL "Í".
             03 FILLER       PIC X VALUE "»".
             03 FILLER       PIC X(3) VALUE " . ".
             03 FILLER       PIC X VALUE "É".
             03 FILLER       PIC X(62) VALUE ALL "Í".
             03 FILLER       PIC X VALUE "»".
          02 IMP-2.
             03 FILLER       PIC X VALUE "º".
             03 FILLER       PIC X(6) VALUE " Cod.:".
             03 NUM-ED1      PIC ZZZZZ9.
             03 FILLER       PIC X(50) VALUE SPACES.
             03 FILLER       PIC X VALUE "º".
             03 FILLER       PIC X(4) VALUE " . º".
             03 FILLER       PIC X(6) VALUE " Cod.:".
             03 NUM-ED2      PIC ZZZZZ9.
             03 FILLER       PIC X(50) VALUE SPACES.
             03 FILLER       PIC X VALUE "º".
          02 IMP-3.
             03 FILLER       PIC X(2) VALUE "º ".
             03 A2           PIC 99 COMP-4 VALUE 14.
             03 DET-ED1      PIC X(30).
             03 A3           PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(7) VALUE " º . º ".
             03 A4           PIC 99 COMP-4 VALUE 14.
             03 DET-ED2      PIC X(30).
             03 A5           PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(2) VALUE " º".
          02 IMP-4.
             03 FILLER       PIC X VALUE "º".
             03 FILLER       PIC X(62) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "º . º".
             03 FILLER       PIC X(62) VALUE SPACES.
             03 FILLER       PIC X VALUE "º".
          02 IMP-5.
             03 FILLER       PIC X(15) VALUE "º     ".
             03 A6           PIC 99 COMP-4 VALUE 14.
             03 MON-ED1      PIC X(4).
             03 A7           PIC 99 COMP-4 VALUE 18.
             03 PRE-ED1      PIC ZZZ9,99.
             03 A8           PIC 99 COMP-4 VALUE 15.
             03 A9           PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(16) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "º . º".
             03 FILLER       PIC X(14) VALUE SPACES.
             03 B1           PIC 99 COMP-4 VALUE 14.
             03 MON-ED2      PIC X(4).
             03 B2           PIC 99 COMP-4 VALUE 18.
             03 PRE-ED2      PIC ZZZ9,99.
             03 B3           PIC 99 COMP-4 VALUE 15.
             03 B4           PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(16) VALUE SPACES.
             03 FILLER       PIC X VALUE "º".
          02 IMP-51.
             03 FILLER       PIC X(23) VALUE SPACES.
             03 C1           PIC 99 COMP-4 VALUE 14.
             03 C2           PIC 99 COMP-4 VALUE 18.
             03 PRE-ED11     PIC ZZZ9,99.
             03 C3           PIC 99 COMP-4 VALUE 15.
             03 C4           PIC 99 COMP-4 VALUE 20.
             03 FILLER       PIC X(43) VALUE SPACES.
             03 C5           PIC 99 COMP-4 VALUE 14.
             03 C6           PIC 99 COMP-4 VALUE 18.
             03 PRE-ED21     PIC ZZZ9,99.
             03 C7           PIC 99 COMP-4 VALUE 15.
             03 C8           PIC 99 COMP-4 VALUE 20.
          02 IMP-6.
             03 FILLER       PIC X VALUE "È".
             03 FILLER       PIC X(62) VALUE ALL "Í".
             03 FILLER       PIC X(5) VALUE "¼ . È".
             03 FILLER       PIC X(62) VALUE ALL "Í".
             03 FILLER       PIC X VALUE "¼".
          02 IMP-7.
             03 FILLER       PIC X(132) VALUE SPACES.
          02 IMP-8.
             03 FILLER       PIC X(132) VALUE ALL ".".
       01 LISTO-D            PIC 999999.
       01 LISTO-H            PIC 999999.
       01 MARCA-D            PIC 9999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 IND-LIS            PIC 9 VALUE 0.
       01 COD-1-LIS          PIC 9 VALUE 0.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       01 CONT-L            PIC 9 VALUE 0.
       COPY "VARIOS.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART02.ERR".
       COPY "NUMERO.ERR".
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
           MOVE 0 TO COD-1-LIS.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP STOP RUN.
           IF F05 GO TO LIST-COD.
           IF F06 GO TO LIST-UNO.
           GO TO CARGO-MOD.
       LIST-UNO.
           MOVE 9 TO COD-1-LIS.
       LIST-COD.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTA VARIOS ART. " TO D20-CAP.
           IF COD-1-LIS = 9 
              MOVE " LISTA UNO POR VEZ " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "EMPRESA : " LINE 20 POSITION 3.
           DISPLAY "SUCURSAL: " LINE 21 POSITION 3.
           DISPLAY "DESDE: " LINE 7 POSITION 3.
           DISPLAY "HASTA: " LINE 9 POSITION 3.
           DISPLAY "MARCA: " LINE 12 POSITION 3.
           DISPLAY " ATENCION:                " LINE 11 POSITION 20
                       REVERSE.
           DISPLAY "     Ingrese 0 en marca y " LINE 12 POSITION 20
                       REVERSE.
           DISPLAY " lista todos los Artic.   " LINE 13 POSITION 20
                       REVERSE.
           DISPLAY "LISTO: " LINE 15 POSITION 3.
           OPEN INPUT ART02 NUMERO.
           OPEN OUTPUT IMP02.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           MOVE 0 TO IND-LIS CONT-R CONT-L.
           ACCEPT EMP-ART02 LINE 20 POSITION 15 SIZE 4 TAB CONVERT.
           ACCEPT SUC-ART02 LINE 21 POSITION 15 SIZE 2 TAB CONVERT.
       L-N-DESDE.
           MOVE 3 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO CIERRE.
           MOVE NUM-CAP TO LISTO-D NUM-ART02.
           IF COD-1-LIS = 9 MOVE 0 TO MAR-ART02 GO TO L-N-POSI1.
       L-N-HASTA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO CIERRE.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
       L-N-MARCA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO CIERRE.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO MARCA-D MAR-ART02.
       L-N-POSI.
           MOVE 0 TO IND-LIS.
       L-N-POSI1.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY GO TO L-N-DESDE.
       LEO.
      *-.-.-.-.-. LISTA UN ROTULO -.-.-.-.-.-.-.-.-.-.-   
           ADD 1 TO IND-LIS.
      *-.-.-.-.-.-LISTA DOS ROTULOS.-.-.-.-.-.-.-.-.-.-
      *     MOVE 0 TO IND-LIS.
      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
           IF IND-LIS > 2 MOVE 1 TO IND-LIS.
           READ ART02 NEXT AT END GO TO CIERRE.
           IF COD-1-LIS = 9 GO TO LEO1.
           IF NUM-ART02 > LISTO-H GO TO CIERRE.
           IF MARCA-D NOT = 0
               IF MARCA-D NOT = MAR-ART02 
                                 COMPUTE IND-LIS = IND-LIS - 1
                                 GO TO LEO.
       LEO1.
           MOVE NUM-ART02 TO COD-NUM.
           READ NUMERO INVALID KEY MOVE "******" TO DET-NUM.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
           MOVE 16 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       L-N-MUEVO.
           IF IND-LIS = 0
              MOVE NUM-ART02 TO NUM-ED1 NUM-ED2
              MOVE DET-NUM TO DET-ED1 DET-ED2
              IF PRE-V-ART02 NOT = 0
                    MOVE PRE-V-ART02 TO PRE-ED1 PRE-ED11
                    MOVE PRE-V-ART02 TO PRE-ED2 PRE-ED21
                    MOVE " $ " TO MON-ED1 MON-ED2
              ELSE MOVE PRE-E-ART02 TO PRE-ED1 PRE-ED11
                   MOVE PRE-E-ART02 TO PRE-ED2 PRE-ED21
                   MOVE "U$S" TO MON-ED1 MON-ED2.
           IF IND-LIS = 0
                GO TO L-N-IMPRE.
           IF IND-LIS = 1
              MOVE NUM-ART02 TO NUM-ED1
              MOVE DET-NUM TO DET-ED1
              IF PRE-V-ART02 NOT = 0
                    MOVE PRE-V-ART02 TO PRE-ED1 PRE-ED11
                    MOVE " $ " TO MON-ED1
              ELSE MOVE PRE-E-ART02 TO PRE-ED1 PRE-ED11
                   MOVE "U$S" TO MON-ED1.
           IF IND-LIS = 2
              MOVE NUM-ART02 TO NUM-ED2
              MOVE DET-NUM TO DET-ED2
              IF PRE-V-ART02 NOT = 0
                   MOVE PRE-V-ART02 TO PRE-ED2 PRE-ED21
                   MOVE " $ " TO MON-ED2
              ELSE MOVE PRE-E-ART02 TO PRE-ED2 PRE-ED21
                   MOVE "U$S" TO MON-ED2.
           IF IND-LIS = 1
              IF COD-1-LIS = 9 GO TO L-N-DESDE. 
           IF IND-LIS = 2
              GO TO L-N-IMPRE.
           IF IND-LIS = 1 
              GO TO LEO.
       L-N-IMPRE.
           ADD 1 TO CONT-L.
           IF CONT-L > 9 MOVE 1 TO CONT-L
                         COMPUTE CONT-R = 72 - CONT-R
                         PERFORM ESPA CONT-R TIMES
                         MOVE 0 TO CONT-R.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-5 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-51 AFTER 0.
           WRITE LIN-IMP02 FROM IMP-6 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-8 AFTER 1.
           ADD 7 TO CONT-R.
       IMPRE-F.
           IF COD-1-LIS = 9 GO TO L-N-DESDE.
           GO TO LEO.
       CIERRE.
           IF IND-LIS = 0 PERFORM CIERRE-A 
                          GO TO INI-MOD.
           COMPUTE IND-LIS = IND-LIS - 1
           IF IND-LIS = 1
             MOVE 0 TO NUM-ED2 PRE-ED2 PRE-ED21
             MOVE SPACES TO DET-ED2 MON-ED2
             PERFORM L-N-IMPRE.
           PERFORM CIERRE-A.
           GO TO INI-MOD.
       ESPA.
           WRITE LIN-IMP02 FROM IMP-7 AFTER 1.
           ADD 1 TO CONT-R.
       CIERRE-A.
           CLOSE NUMERO ART02 IMP02.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 071006 TO TAB-POS(3).
           MOVE 091006 TO TAB-POS(4).
           MOVE 121004 TO TAB-POS(5).
           MOVE 112503 TO TAB-POS(6).
           MOVE 122503 TO TAB-POS(7).
           MOVE 132503 TO TAB-POS(8).
           MOVE 142503 TO TAB-POS(9).
           MOVE 152503 TO TAB-POS(10).
           MOVE 162503 TO TAB-POS(11).
           MOVE 172503 TO TAB-POS(12)
           MOVE 182503 TO TAB-POS(13).
           MOVE 192503 TO TAB-POS(14).
           MOVE 151006 TO TAB-POS(15).
           MOVE 152030 TO TAB-POS(16).
           MOVE 172120 TO TAB-POS(17).
           MOVE 103320 TO TAB-POS(18).
           MOVE 113320 TO TAB-POS(19).
           MOVE 123320 TO TAB-POS(20).
           MOVE 133320 TO TAB-POS(21).
           MOVE 143320 TO TAB-POS(22).
           MOVE 153320 TO TAB-POS(23).
           MOVE 163320 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
           MOVE "   -IMPRES" TO TIT-1-PAN.
           MOVE "ION DE ROT" TO TIT-2-PAN.
           MOVE "ULOS-     " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LISTA VARIOS  " LINE 14 POSITION 18.
           DISPLAY "<F6>  LISTA UNO POR VEZ " LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
 