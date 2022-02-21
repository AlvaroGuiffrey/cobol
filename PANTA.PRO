      *=====================================================
      * DATOS FIJOS DE PANTALLA
      *=====================================================
       FECHA-PAN.
           ACCEPT FECHA FROM DATE.
           DISPLAY DIA LINE 2 POSITION 69.
           DISPLAY "/" LINE 2 POSITION 71.
           DISPLAY MES LINE 2 POSITION 72.
           DISPLAY "/" LINE 2 POSITION 74.
           DISPLAY ANO LINE 2 POSITION 75.
       TITU-PAN.
           DISPLAY TITULO-PAN LINE 4 POSITION 2.
       MODA-PAN.
           MOVE 1 TO SW3.
           DISPLAY D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3) REVERSE.
       MENS-PAN.
           DISPLAY MENSAJE LINE 25 POSITION 2 SIZE 79.
      *----------------------------------------------------
      * SISTEMA: -GESTION COMERCIAL- DATOS PANTALLAS
      * NOMBRE RUTINA: PANTA.PRO
      * VERSION: 001    FECHA: JUL'90
      *====================================================
