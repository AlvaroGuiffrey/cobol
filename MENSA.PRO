      *=====================================================
      * MENSAJES VARIOS
      *===================================================
       MEN-NO-BAJA.
           MOVE "*** NO REALIZA " TO MEN-1.
           MOVE "BAJA ***       " TO MEN-2.
           MOVE SPACES TO MEN-3.
           PERFORM MENS-PAN.
           PERFORM TIEMPO 100 TIMES.
       MEN-NO-MODI.
           MOVE "*** NO REALIZA " TO MEN-1.
           MOVE "MODIFICACION **" TO MEN-2.
           MOVE SPACES TO MEN-3.
           PERFORM MENS-PAN.
           PERFORM TIEMPO 100 TIMES.
       MEN-CONS.
           MOVE "DIGITE <F3> PRO" TO MEN-1.
           MOVE "X.PAG.         " TO MEN-2.
           MOVE "     - ¨[ ]? - " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-F-CONS.
           MOVE "FIN CONSULTA, <" TO MEN-1.
           MOVE "F1> SALE.      " TO MEN-2.
           MOVE "     - ¨[ ]? - " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-CONF.
           MOVE "CONFIRME CON <F" TO MEN-1.
           MOVE "1> LA TAREA.   " TO MEN-2.
           MOVE "     - ¨[ ]? - " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-MODA-A.
           MOVE "INGRESE MODALID" TO MEN-1.
           MOVE "AD A ACTIVAR.  " TO MEN-2.
           MOVE "     - ¨[ ]? - " TO MEN-3.
           MOVE SPACES TO MEN-4 MEN-5.
           PERFORM MENS-PAN.
       MEN-MODA-S.
           MOVE "DIGITE <ESC> CA" TO MEN-4.
           MOVE "MBIA MODALIDAD " TO MEN-5.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
       MEN-ANTE.
           MOVE "DIGITE <F2> VUE" TO MEN-1.
           MOVE "LVE CAMPO ANTER" TO MEN-2.
           MOVE "IOR.           " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-ANTE-DES.
           MOVE "DIGITE <F2> VUE" TO MEN-1.
           MOVE "LVE ANTERIOR, <" TO MEN-2.
           MOVE "F3> PROXIMO    " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-B-M.
           MOVE "DIGITE <F9> MOD" TO MEN-1.
           MOVE "IFICA Y <F10> B" TO MEN-2.
           MOVE "AJA  - ¨[ ]? - " TO MEN-3.
           PERFORM MENS-PAN.
       MEN-BLA-123.
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MENS-PAN.
       TIEMPO.
           MOVE SW3 TO SW3.
       MEN-LIS.
           DISPLAY " ESTOY LISTANDO ... " LINE 15 POSITION 32 BLINK.
      *----------------------------------------------------
      * SISTEMA: -GESTION COMERCIAL- MENSAJES VARIOS
      * NOMBRE RUTINA: MENSA.PRO
      * VERSION: 001    FECHA: JUL'90
      *====================================================
