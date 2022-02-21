      *************************************************************
      *  RUTINA DE CONVERSION NUMERO A LETRAS                     * 
      *************************************************************
      
       PROGRAMA-PRINCIPAL.
           MOVE 40  TO HS-LARGO-LIT
           MOVE SPACES TO HS-LITERAL HS-PA-MOVER
           MOVE 0 TO HS-LARGO-LIT HS-W HS-X
           MOVE 1 TO HS-I
           MOVE HS-IMPORTE TO HS-IMPORTE-W
      *    DISPLAY HS-IMPORTE-W
           PERFORM PROCESOX THRU SAL-P
                   UNTIL HS-I GREATER THAN 5.
           MOVE HS-LITERAL-W TO HS-LITERAL.
      *    DISPLAY HS-LARGO-LIT "*" HS-LITERAL.
      *    DISPLAY HS-R-L(1)
      *    DISPLAY HS-R-L(2)
      *    DISPLAY HS-R-L(3).
       FINALES.
           GO TO SALIDA.     
      *     EXIT PROGRAM.
      * **************************************
       PROCESOX.
      *    DISPLAY "PROCESO"
           IF HS-I = 5 MOVE HS-IMPOR5 TO HS-AUX
            ELSE    MOVE HS-IMPOR(HS-I) TO HS-AUX.
       PASO2.
      *      DISPLAY "PASO2 "  HS-I  "AUX=  "  HS-AUX
           IF HS-I GREATER THAN 5 GO TO SAL-P.
           IF HS-AUX = 0 GO TO PASO5.
           IF HS-AUX LESS THAN 16 GO TO PASO3.
           IF HS-AUX LESS THAN 100 GO TO PASO4.
           IF HS-AUX = 100 MOVE HS-TABLA3(3) TO HS-ELEMENTO
                        MOVE HS-LARGO3(3) TO HS-LARGO
                        PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1
                                UNTIL HS-X GREATER THAN 15 OR
                                      HS-X GREATER THAN HS-LARGO
                        MOVE 0 TO HS-AUX
                        GO TO PASO2.
           IF HS-CIFRA1 = 1 OR HS-CIFRA1 = 5 OR HS-CIFRA1 = 7 OR
              HS-CIFRA1 = 9
                        MOVE HS-TABLA3(HS-CIFRA1) TO HS-ELEMENTO
                        MOVE HS-LARGO3(HS-CIFRA1) TO HS-LARGO
                        PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1
                                UNTIL HS-X GREATER THAN 15 OR
                                      HS-X GREATER THAN HS-LARGO
                        MOVE 0 TO HS-CIFRA1
                        GO TO PASO2.
           MOVE HS-TABLA1(HS-CIFRA1) TO HS-ELEMENTO
           MOVE HS-LARGO1(HS-CIFRA1) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1
                         UNTIL HS-X GREATER THAN 15 OR
                                HS-X GREATER THAN HS-LARGO
           MOVE "CIENTOS"      TO HS-ELEMENTO
           MOVE 07             TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1
                              UNTIL HS-X GREATER THAN 7
           MOVE 0 TO HS-CIFRA1
           GO TO PASO2.
       PASO3.
           IF HS-I = 3 AND HS-AUX = 1 AND HS-IMPOR(3) = 1
                        MOVE 0 TO HS-AUX
                        GO TO PASO2.
      *    DISPLAY "PASO3 AUX = "  HS-AUX
           MOVE HS-TABLA1 (HS-AUX) TO HS-ELEMENTO
           MOVE HS-LARGO1 (HS-AUX) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                              UNTIL HS-X GREATER THAN 15 OR
                                    HS-X GREATER THAN HS-LARGO
           IF HS-AUX NOT = 1 MOVE 0 TO HS-AUX GO TO PASO2.
           IF HS-I   NOT = 4 MOVE 0 TO HS-AUX GO TO PASO2.
           MOVE HS-TABLA3 (4) TO HS-ELEMENTO
           MOVE HS-LARGO3 (4) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                              UNTIL HS-X GREATER THAN 15 OR
                                    HS-X GREATER THAN HS-LARGO
           MOVE 0 TO HS-AUX
           GO TO PASO2.
       PASO4.
      *    DISPLAY "PASO4 AUX= " HS-AUX
           IF HS-AUX = 20
                MOVE HS-TABLA3 (2) TO HS-ELEMENTO
                MOVE HS-LARGO3 (2) TO HS-LARGO
                PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                                 UNTIL HS-X GREATER THAN 15 OR
                                       HS-X GREATER THAN HS-LARGO
                MOVE 0 TO HS-AUX
                GO TO PASO2.
           MOVE HS-TABLA2 (HS-CIFRA2) TO HS-ELEMENTO
           MOVE HS-LARGO2 (HS-CIFRA2) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                              UNTIL HS-X GREATER THAN 15 OR
                                    HS-X GREATER THAN HS-LARGO
           IF HS-CIFRA3 = 0 OR HS-AUX LESS THAN 30
                            MOVE 0 TO HS-CIFRA2
                            GO TO PASO2.
           MOVE HS-TABLA3 (8) TO HS-ELEMENTO
           MOVE HS-LARGO3 (8) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                              UNTIL HS-X GREATER THAN 15 OR
                                    HS-X GREATER THAN HS-LARGO
           MOVE 0 TO HS-CIFRA2
           GO TO PASO2.
       PASO5.
      *    DISPLAY "PASO5 AUX= " HS-AUX "IMPOR5= " HS-IMPOR5
           IF HS-I = 4 
                  IF HS-IMPOR5 = 0 GO TO PASO5B
                   ELSE         GO TO PASO5A.
           IF HS-IMPOR(HS-I) = 0 GO TO PASO5B.
       PASO5A.
      *    DISPLAY "PASO5A I= " HS-I
           MOVE HS-TABLA4 (HS-I) TO HS-ELEMENTO
           MOVE HS-LARGO4 (HS-I) TO HS-LARGO
           PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                              UNTIL HS-X GREATER THAN 15 OR
                                    HS-X GREATER THAN HS-LARGO
           IF HS-I LESS THAN 3
              IF HS-IMPOR (HS-I) GREATER THAN 1
                 MOVE HS-TABLA3 (10) TO HS-ELEMENTO
                 MOVE HS-LARGO3 (10) TO HS-LARGO
                 PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1 
                                   UNTIL HS-X GREATER THAN 15 OR
                                         HS-X GREATER THAN HS-LARGO
                 GO TO PASO5B
                ELSE 
                     MOVE SPACES TO HS-ELEMENTO
                     MOVE 1  TO HS-LARGO
                     PERFORM MOVIMIENTO VARYING HS-X FROM 1 BY 1
                            UNTIL HS-X GREATER THAN HS-LARGO.
       PASO5B.
      *    DISPLAY "PASO5B I= " HS-I
           ADD 1 TO HS-I.
       SAL-P.
           EXIT.
       MOVIMIENTO.
      *    DISPLAY "MOVIMIENTO"
           ADD 1 TO HS-LARGO-LIT
           MOVE HS-ITEM(HS-X) TO HS-ITEM-LIT(HS-LARGO-LIT).

