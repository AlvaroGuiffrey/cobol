

      *    RUT1.CBL          
      *=-=-=- INVALIDAR CUANDO NO TIENE VENCIMIENTO =-=-=-=-=-==
       VENCI.
           IF NUM-CONDI > 50 AND NUM-CONDI < 90
              GO TO VENCI-1.
           GO TO SIGO.
       VENCI-1.
           DISPLAY "VENCIMIENTO   : " LINE 23 POSITION 3.
           ACCEPT FEC-V-CAP LINE 23 POSITION 19 SIZE 6
               TAB CONVERT 
              ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
           IF FEC-V-CAP = 0 MOVE DD-CAP TO DD-V-CAP
                            MOVE MM-CAP TO MM-V-CAP
                            MOVE AA-CAP TO AA-V-CAP.
           IF AA-V-CAP < AA-CAP GO TO VENCI-1.
           IF AA-V-CAP = AA-CAP 
             IF MM-V-CAP < MM-CAP GO TO VENCI-1.
           IF AA-V-CAP = AA-CAP
             IF MM-V-CAP = MM-CAP
               IF DD-V-CAP < DD-CAP GO TO VENCI-1.
           DISPLAY DD-V-CAP LINE 23 POSITION 19 BLINK REVERSE.
           DISPLAY "/" LINE 23 POSITION 21 REVERSE.
           DISPLAY MM-V-CAP LINE 23 POSITION 22 BLINK REVERSE.
           DISPLAY "/" LINE 23 POSITION 24 REVERSE.
           DISPLAY AA-V-CAP LINE 23 POSITION 25 BLINK REVERSE.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
