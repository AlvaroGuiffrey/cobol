      *----------- TABLA POSICIONES -----------------------
      * PRECAUCION ... !!!
      * HABILITAR:  DET-EST     PIC X(20)
      *----------------------------------------------------
       DESP-EST.
           MOVE "...???" TO DET-EST.
           IF POS-MCU = 1 MOVE "DEL.IZQ" TO  DET-EST.
           IF POS-MCU = 2 MOVE "DEL.IZQ.AD." TO DET-EST.
           IF POS-MCU = 3 MOVE "DEL.DER.AD." TO DET-EST.
           IF POS-MCU = 4 MOVE "DEL.DER." TO DET-EST.
           IF POS-MCU = 5 MOVE "TRA.IZQ.-1-" TO DET-EST.
           IF POS-MCU = 6 MOVE "TRA.IZQ.AD.-1-" TO DET-EST.
           IF POS-MCU = 7 MOVE "TRA.DER.AD.-1-" TO DET-EST.
           IF POS-MCU = 8 MOVE "TRA.DER.-1-" TO DET-EST.
           IF POS-MCU = 9 MOVE "TRA.IZQ.-2-" TO DET-EST.
           IF POS-MCU = 10 MOVE "TRA.IZQ.AD.-2-" TO DET-EST.
           IF POS-MCU = 11 MOVE "TRA.DER.AD.-2-" TO DET-EST.
           IF POS-MCU = 12 MOVE "TRA.DER.-2-" TO DET-EST.
           IF POS-MCU = 13 MOVE "TRA.IZQ.-3-" TO DET-EST.
           IF POS-MCU = 14 MOVE "TRA.IZQ.AD.-3-" TO DET-EST.
           IF POS-MCU = 15 MOVE "TRA.DER.AD.-3-" TO DET-EST.
           IF POS-MCU = 16 MOVE "TRA.DER.-3-" TO DET-EST.
           IF POS-MCU = 17 MOVE "AUX.-1-" TO DET-EST.
           IF POS-MCU = 18 MOVE "AUX.-2-" TO DET-EST.
           IF POS-MCU = 19 MOVE "AUX.-3-" TO DET-EST.
           IF POS-MCU = 20 MOVE "AUX.-4-" TO DET-EST.
      *--------------- FIN TABLA POSICIONES ------------------
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      *----------- TABLA DE OPERACIONES  ---------------------
      * PRECAUCION ... !!!
      * HABILITAR :  DET-OPE     PIC X(20)
      *------------------------------------------------------
           MOVE "... ???" TO DET-OPE.
           IF OPE-MCU = 1 MOVE " ALTA-DEPOS." TO DET-OPE.
           IF OPE-MCU = 2 MOVE " ALTA-C/VEHIC." TO DET-OPE.
           IF OPE-MCU = 5 MOVE " BAJA-DEPOS." TO DET-OPE.
           IF OPE-MCU = 6 MOVE " BAJA-C/VEHIC." TO DET-OPE.
           IF OPE-MCU = 7 MOVE " BAJA-DESUSO " TO DET-OPE.
           IF OPE-MCU = 11 MOVE " ARREGLO " TO DET-OPE.
           IF OPE-MCU = 12 MOVE " CONTROL " TO DET-OPE.
           IF OPE-MCU = 13 MOVE " ALINEACION " TO DET-OPE.
           IF OPE-MCU = 14 MOVE " BALANCEO " TO DET-OPE.
           IF OPE-MCU = 15 MOVE " ALIN./BALANC." TO DET-OPE.
      *---------------- FIN TABLA OPERACIONES -------------------
