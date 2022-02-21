      *=====================================================
      * CAPTURA DE CAMPOS
      *=====================================================
       CAP-ALF.
           PERFORM BORRA.
           ACCEPT ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-D20.
           PERFORM BORRA.
           ACCEPT D20-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-NUM.
           PERFORM BORRA.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-DEC10.
           PERFORM BORRA.
           ACCEPT DEC10-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          NO BEEP TAB CONVERT
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-DEC15.
           PERFORM BORRA.
           ACCEPT DEC15-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                            NO BEEP TAB CONVERT
                            ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-DEC6.
           PERFORM BORRA.
           ACCEPT DEC6-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           NO BEEP TAB CONVERT
                           ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-COD1.
           PERFORM BORRA.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) 
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-OPCION.
           PERFORM BORRA.
           ACCEPT OPCION LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       TAB ON EXCEPTION EXCEP MOVE SW3 TO SW3.
       CAP-SW1.
           PERFORM BORRA.
           ACCEPT SW1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3)
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
      *----------------------------------------------------
      * SISTEMA: -GESTION COMERCIAL- CAPTURA CAMPOS
      * NOMBRE RUTINA: CAPTU.PRO
      * VERSION: 001    FECHA: JUL'90
      *====================================================
