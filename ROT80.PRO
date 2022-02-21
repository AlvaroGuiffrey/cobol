      *===================================================
      * DATOS FIJO DE LISTADOS
      *---------------------------------------------------
       FECHA-ROT80.
           ACCEPT FECHA FROM DATE.
           MOVE DIA TO DD-E80.
           MOVE MES TO MM-E80.
           MOVE ANO TO AA-E80.
       HORA-ROT80.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E80.
           MOVE MINU TO MI-E80.
      *---------------------------------------------------
      * SISTEMA: -GESTION COMERCIAL- DATOS ROTULO 80 
      * NOMBRE RUTINA: ROT80.PRO
      * VERSION: 001    FECHA: JUL'90
      *===================================================
