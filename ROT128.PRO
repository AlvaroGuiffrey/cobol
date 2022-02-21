      *=================================================
      * DATOS FIJOS DE ROTULO DE 128
      *-------------------------------------------------
       FECHA-ROT128.
           ACCEPT FECHA FROM DATE.
           MOVE DIA TO DD-E128.
           MOVE MES TO MM-E128.
           MOVE ANO TO AA-E128.
       HORA-ROT128.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E128.
           MOVE MINU TO MI-E128.
      *-------------------------------------------------
      * SISTEMA: -GESTION COMERCIAL- DATOS ROTULO 128
      * NOMBRE RUTINA: ROT128.PRO
      * VERSION: 001     FECHA: JUL'90
      *=================================================
