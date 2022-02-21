       IMPRE.
           DISPLAY " - PASO II - IMPRESION DE BALANCE " LINE 21
                 POSITION 10 SIZE 50.
           DISPLAY "                    " LINE 15 POSITION 50
                       LOW REVERSE.
           DISPLAY " INGRESE   CANTIDAD " LINE 16 POSITION 50
                       LOW REVERSE.
           DISPLAY " DE COPIAS A LISTAR " LINE 17 POSITION 50
                       LOW REVERSE.
           DISPLAY "       -     -      " LINE 18 POSITION 50
                       LOW REVERSE.
           DISPLAY "                    " LINE 19 POSITION 50
                       LOW REVERSE.
           ACCEPT CONT-I LINE 18 POSITION 60 SIZE 2.
       VEA-IMP.
           MOVE 0 TO CONT-P  CONT-R.
           IF CONT-I = 0 GO TO FF.
           COMPUTE CONT-I =  CONT-I - 1.
           OPEN INPUT P-DE-CTAS MAYOR-F.
           OPEN OUTPUT LIS-BAL.
           WRITE LINEA FROM COMPRI AFTER 0.
       S-FECHA.
           MOVE DIA-H TO DD-B-ED.
           MOVE ANO-H TO AA-B-ED.
           IF MES-H = 1 MOVE "ENERO*****" TO MM-B-ED.
           IF MES-H = 2 MOVE "FEBRERO***" TO MM-B-ED.
           IF MES-H = 3 MOVE "MARZO*****" TO MM-B-ED.
           IF MES-H = 4 MOVE "ABRIL*****" TO MM-B-ED.
           IF MES-H = 5 MOVE "MAYO******" TO MM-B-ED.
           IF MES-H = 6 MOVE "JUNIO*****" TO MM-B-ED.
           IF MES-H = 7 MOVE "JULIO*****" TO MM-B-ED.
           IF MES-H = 8 MOVE "AGOSTO****" TO MM-B-ED.
           IF MES-H = 9 MOVE "SEPTIEMBRE" TO MM-B-ED.
           IF MES-H = 10 MOVE "OCTUBRE***" TO MM-B-ED.
           IF MES-H = 11 MOVE "NOVIEMBRE " TO MM-B-ED.
           IF MES-H = 12 MOVE "DICIEMBRE " TO MM-B-ED.
           MOVE 0 TO RUBRO-MAY NUM-EMP-MAY SUC-EMP-MAY VERSION-MAY.
           MOVE 0 TO RUBRO-A-MAY M1-MAY M2-MAY M3-MAY M4-MAY
           MOVE 0 TO M5-MAY M6-MAY MAT1.
           START MAYOR-F KEY NOT < CLAVE-MAY
                 INVALID KEY DISPLAY "NO ENTRO MAY"
                       LINE 23 POSITION 5   STOP RUN.
000000 LEA-MAYOR.
           READ MAYOR-F NEXT AT END GO TO FINAL-I.
           IF VERSION-MAY NOT = VERSION-EMP GO TO LEA-MAYOR.
           MOVE VERSION-MAY TO P-VERSION
           MOVE RUBRO-MAY TO P-RUBRO
           MOVE RUBRO-A-MAY TO P-RUBRO-A
           READ P-DE-CTAS INVALID KEY MOVE "***" TO P-NOMBRE.
000000 SOLO-CORTE.
      *-.-.-.-.-.-.INVALIDA CORTE DE MATRIZ-.-.-.-.-.-.-
           GO TO YA-CORTO.
           IF MAT1 = 0 
              MOVE M1 TO MAT1
              MOVE M2 TO MAT2
              MOVE M3 TO MAT3
              MOVE M4 TO MAT4
              MOVE M5 TO MAT5
              MOVE M6 TO MAT6
              MOVE 0 TO CONT-R
              PERFORM TITULOS THRU TITULOS-F.
           IF MAT1 NOT = M1
                 PERFORM CORTE-1 THRU CORTE-6
                 PERFORM ACA1 THRU TITULOS-F
                 GO TO YA-CORTO.
           IF MAT2 NOT = M2
                 PERFORM CORTE-1 THRU CORTE-5
                 PERFORM ACA2 THRU TITULOS-F
                 GO TO YA-CORTO.
           IF MAT3 NOT = M3
                  PERFORM CORTE-1 THRU CORTE-4
                  PERFORM ACA3 THRU TITULOS-F
                  GO TO YA-CORTO.
           IF MAT4 NOT = M4
                 PERFORM CORTE-1 THRU CORTE-3
                 PERFORM ACA4 THRU TITULOS-F.
           IF MAT5 NOT = M5
                 PERFORM CORTE-1 THRU CORTE-2
                 PERFORM ACA5 THRU TITULOS-F.
           IF MAT6 NOT = M6
                 PERFORM CORTE-1
                 PERFORM ACA6 THRU TITULOS-F.
000000 YA-CORTO.
           IF CONT-P = 0 PERFORM TITULOS THRU TITULOS-F.
           IF SALDO-MAY NOT = 0 OR
                SALDO-EXT-MAY NOT = 0 OR
                  SALDO-MES-MAY NOT = 0
              PERFORM ARMA-SUME THRU ARME-SUME-F
              PERFORM AVANCE THRU AVANCE-F
              PERFORM IMPRIMA.
           GO TO LEA-MAYOR.
       ARMA-SUME.
           MOVE P-RUBRO TO CTA-ED.
           MOVE P-RUBRO-A TO AUX-ED.
           MOVE P-NOMBRE TO NOM-CTA-ED.
       AS.
           IF SALDO-MAY > 0
              MOVE SALDO-MAY TO SALDO-D-ED
              MOVE 0 TO SALDO-C-ED.
           IF SALDO-MAY = 0
              MOVE 0 TO SALDO-D-ED SALDO-C-ED.
           IF SALDO-MAY < 0
              MOVE SALDO-MAY TO SALDO-C-ED
              MOVE 0 TO SALDO-D-ED.
           MOVE SALDO-EXT-MAY TO SUMA-D-ED.
           MOVE SALDO-MES-MAY TO SUMA-C-ED.
      *     IF P-RUBRO-A NOT = 0 GO TO ARME-SUME-F.
          
           ADD  SALDO-EXT-MAY TO ACUM1
           ADD  SALDO-MES-MAY TO ACUM2.
           IF SALDO-MAY > 0
              ADD  SALDO-MAY TO ACUM3.
           IF SALDO-MAY < 0
              ADD  SALDO-MAY TO ACUM4.
      *     ADD  SALDO-MAY TO ACUM5.
      *     ADD  SALDO-MAY TO ACUM6.
       ARME-SUME-F.
           EXIT.
000000 AVANCE.
           ADD 1 TO CONT-R
           IF CONT-R > 65 COMPUTE CONT-R = 72 - CONT-R
                             PERFORM ESPA CONT-R TIMES
                             MOVE 0 TO CONT-R
                             PERFORM TITULOS.
       AVANCE-F.
           EXIT.
       AVANCE-C.
           ADD 1 TO CONT-R
           IF CONT-R > 65 COMPUTE CONT-R = 72 - CONT-R
                             PERFORM ESPA CONT-R TIMES
                             MOVE 0 TO CONT-R
                             PERFORM TITULOS.
       AVANCE-C-F.
           EXIT.
       TITULOS.
           IF AJUSTE = 0 MOVE " AUSTRALES " TO TITU-2.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO CONT-P-ED.
           WRITE LINEA FROM TIT-1 AFTER 2.
           IF SUC-EMP = 0
              WRITE LINEA FROM TIT-21 AFTER 1
              ELSE WRITE LINEA FROM TIT-2 AFTER 1.
           WRITE LINEA FROM TIT-3 AFTER 1.
           WRITE LINEA FROM RAY-1 AFTER 1.
           WRITE LINEA FROM TIT-40 AFTER 1.
           WRITE LINEA FROM TIT-4 AFTER 1.
           WRITE LINEA FROM RAY-1 AFTER 1.
           WRITE LINEA FROM TIT-5 AFTER 1.
           WRITE LINEA FROM RAY-3 AFTER 1.
           ADD 10 TO CONT-R.
      *-.-.-.-.-.-.INVALIDA CORTE DE MATRICES -.-.-.-.-.-.-
           GO TO TITULOS-F.
       ACA1.
           MOVE M1 TO NUMERO-MATRIZ MAT1
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1
           PERFORM AVANCE 1 TIMES.
000000 ACA2.
           MOVE M2 TO NUMERO-MATRIZ MAT2.
           IF M2 = 0 GO TO TITULOS-F.
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1.
           PERFORM AVANCE 1 TIMES.
000000 ACA3.
           MOVE M3 TO NUMERO-MATRIZ MAT3.
           IF M3 = 0 GO TO TITULOS-F.
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1.
           PERFORM AVANCE 1 TIMES.
000000 ACA4.
           MOVE M4 TO NUMERO-MATRIZ MAT4.
           IF M4 = 0 GO TO TITULOS-F.
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1.
           PERFORM AVANCE 1 TIMES.
       ACA5.
           MOVE M5 TO NUMERO-MATRIZ MAT5.
           IF M5 = 0 GO TO TITULOS-F.
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1.
           PERFORM AVANCE 1 TIMES.
       ACA6.
           MOVE M6 TO NUMERO-MATRIZ MAT6.
           IF M6 = 0 GO TO TITULOS-F.
           PERFORM LEO-MAT.
           WRITE LINEA FROM REN-1 AFTER 1.
           PERFORM AVANCE 1 TIMES.
000000 TITULOS-F.
           EXIT.
       LEO-MAT.
           READ MATRIZ INVALID KEY
                MOVE "*************" TO DESCRIPCION-MATRIZ.
           MOVE DESCRIPCION-MATRIZ TO NOM-MAT-ED MAT-TOTAL-ED.
       IMPRIMA.
           WRITE LINEA FROM REN-2 AFTER 1.
       CORTE-1.
           IF MAT6 = 0 GO TO CORTE-2.
           MOVE MAT6 TO NUMERO-MATRIZ.
           PERFORM LEO-MAT.
           MOVE ACUM6 TO TOTAL-ED.
           WRITE LINEA FROM RAY-4 AFTER 1.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-5 AFTER 1.
           MOVE 0 TO ACUM6.
           ADD 2 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
000000 CORTE-2.
           IF MAT5 = 0 GO TO CORTE-3.
           MOVE MAT5 TO NUMERO-MATRIZ.
           PERFORM LEO-MAT.
           MOVE ACUM5 TO TOTAL-ED.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-5 AFTER 1.
           MOVE 0 TO ACUM5 ACUM6.
           ADD 1 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
000000 CORTE-3.
           IF MAT4 = 0 GO TO CORTE-4.
           MOVE MAT4 TO NUMERO-MATRIZ.
           PERFORM LEO-MAT.
           MOVE ACUM4 TO TOTAL-ED.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-5 AFTER 1.
           MOVE 0 TO ACUM4 ACUM5 ACUM6.
           ADD 1 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
000000 CORTE-4.
           IF MAT3 = 0 GO TO CORTE-5.
           MOVE MAT3 TO NUMERO-MATRIZ.
           PERFORM  LEO-MAT.
           MOVE ACUM3 TO TOTAL-ED.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-5 AFTER 1.
           MOVE 0 TO ACUM3 ACUM4 ACUM5 ACUM6.
           ADD 1 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
       CORTE-5.
           IF MAT2 = 0 GO TO CORTE-6.
           MOVE MAT2 TO NUMERO-MATRIZ.
           PERFORM LEO-MAT.
           MOVE ACUM2 TO TOTAL-ED.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-5 AFTER 1.
           MOVE 0 TO ACUM2 ACUM3 ACUM4 ACUM5 ACUM6.
           ADD 1 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
       CORTE-6.
           MOVE MAT1 TO NUMERO-MATRIZ.
           PERFORM LEO-MAT.
           MOVE ACUM1 TO TOTAL-ED.
           WRITE LINEA FROM TOT-1 AFTER 1.
           WRITE LINEA FROM RAY-6 AFTER 1.
           WRITE LINEA FROM RAY-2 AFTER 1.
           WRITE LINEA FROM RAY-3 AFTER 1.
           MOVE 0 TO ACUM1 ACUM2 ACUM3 ACUM4 ACUM5 ACUM6.
           ADD 3 TO CONT-R.
           PERFORM AVANCE 1 TIMES.
      * =========================================
       ESPA.
           MOVE SPACES TO LINEA.
           WRITE LINEA.
           ADD 1 TO CONT-R.
000000 FINAL-I.
           MOVE 0 TO CTA-ED.
           MOVE 0 TO AUX-ED.
           MOVE " T O T A L E S : " TO NOM-CTA-ED.
           MOVE ACUM1 TO SUMA-D-ED.
           MOVE ACUM2 TO SUMA-C-ED.
           MOVE ACUM3 TO SALDO-D-ED.
           MOVE ACUM4 TO SALDO-C-ED.
           WRITE LINEA FROM RAY-1.
           PERFORM AVANCE.
           WRITE LINEA FROM REN-2 AFTER 1.
           PERFORM AVANCE.
           WRITE LINEA FROM RAY-1 AFTER 1.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
      *     IF MAT6 NOT = 0 PERFORM CORTE-1.
      *     IF MAT5 NOT = 0 PERFORM CORTE-2.
      *     IF MAT4 NOT = 0 PERFORM CORTE-3.
      *     IF MAT3 NOT = 0 PERFORM CORTE-4.
      *     IF MAT2 NOT = 0 PERFORM CORTE-5.
      *     IF MAT1 NOT = 0 PERFORM CORTE-6.
      *     CLOSE MATRIZ
           CLOSE MAYOR-F
           CLOSE P-DE-CTAS
           CLOSE LIS-BAL.
       FINAL-IMP.
           GO TO VEA-IMP.    
