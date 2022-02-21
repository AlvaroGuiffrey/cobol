       IDENTIFICATION DIVISION.
        PROGRAM-ID. COB1203.

       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
          SOURCE-COMPUTER. IBM-PC.
          OBJECT-COMPUTER. IBM-PC.
        SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      *********************************
      * COLOCAR EL ".SEL" DEL ARCHIVO *
      *********************************
           COPY "CPOSTAL.SEL".

       DATA DIVISION.
        FILE SECTION.
      *********************************
      * COLOCAR EL ".REG" DEL ARCHIVO * 
      *********************************
            COPY "CPOSTAL.REG".

        WORKING-STORAGE SECTION.
         77 RESPUESTA    PIC X.
         77 CODIGO-TECLA PIC 99.
         01 C-ILU        PIC 99 VALUE 0.
         01 C-VEC        PIC 99 VALUE 0.
      ****************************************************************
      * HABILITAR LOS DATOS A CONSULTAR PON EL "PIC" CORRESPONDIENTE *
      ****************************************************************
         01 TABLAS.
            02 REG-VEC OCCURS 10 TIMES.
               03 D-1-VEC        PIC 9(4)B.
               03 D-2-VEC        PIC 9(2)BB.
               03 D-3-VEC        PIC X(30).
      *         03 D-4-VEC        PIC ---- .
      *         03 D-5-VEC        PIC ---- .
      *         03 D-6-VEC        PIC ---- .      
      ***************************************************************

         01 ESTRUCTURA-VENTANA.
            02 EV-HANDLE           PIC 999 BINARY VALUE 0.
            02 EV-NUM-FILAS        PIC 999 BINARY.
            02 EV-NUM-COLS         PIC 999 BINARY.
            02 EV-POSICION         PIC X.
               88 EV-POSICION-RELATIVA-PANTALLA VALUE "S".
               88 EV-POSICION-RELATIVA-VENTANA  VALUE "W".
            02 EV-BORDE            PIC X.
               88 EV-BORDE-ON         VALUE "Y"
                                       WHEN FALSE "N".
            02 EV-TIPO-BORDE       PIC 9.
            02 EV-CARACTER-BORDE   PIC X.
            02 EV-RECUBRIR         PIC X.
               88 EV-RECUBRIR-ON      VALUE "Y"
                                       WHEN FALSE "N".
            02 EV-CARACTER-RECUBRIR  PIC X.
            02 EV-POSICION-TITULO    PIC X.
               88 EV-ARRIBA           VALUE "T".
               88 EV-ABAJO            VALUE "B".
            02 EV-SITUACION-TITULO   PIC X.
               88 EV-CENTRADO         VALUE "C".
               88 EV-IZQUIERDA        VALUE "L".
               88 EV-DERECHA          VALUE "R".
            02 EV-LONGITUD-TITULO    PIC 999 BINARY.
            02 EV-TITULO             PIC X(64). 
        LINKAGE SECTION.
      ***************************************************************
      * HABILITAR LOS CAMPOS DE TRANSFERENCIA AL PROGRAMA PRINCIPAL *
      ***************************************************************
          01 C-POS-T        PIC 9(4).
          01 C-POS-1-T      PIC 99.         
          01 NOM-POSTAL-T   PIC X(30).

      **************************************************************
      * DETALLAR LOS CAMPOS DE LA LINKAGE EN EL ORDEN DEL CALL DEL *
      * PROGRAMA PRINCIPAL                                         *
      ************************************************************** 
       PROCEDURE DIVISION USING C-POS-T C-POS-1-T NOM-POSTAL-T.
      
       INICIO.
      ********************
      * ABRIR EL ARCHIVO *
      ********************
           OPEN INPUT CPOSTAL.

           COMPUTE EV-NUM-FILAS = 10.
           COMPUTE EV-NUM-COLS = 40.
           MOVE "S" TO EV-POSICION.
           MOVE "Y" TO EV-BORDE.
           MOVE 0 TO EV-TIPO-BORDE.
           MOVE "°" TO EV-CARACTER-BORDE.
           MOVE "T" TO EV-POSICION-TITULO.
           MOVE "C" TO EV-SITUACION-TITULO.
      ************************
      * TITULO DE LA VENTANA *
      ************************
           MOVE " - CODIGOS POSTALES - " TO EV-TITULO.
           MOVE 22 TO EV-LONGITUD-TITULO.
      ***********************
           DISPLAY ESTRUCTURA-VENTANA HIGH ERASE
                  LINE 10 POSITION 20 CONTROL "WINDOW-CREATE".
           MOVE 0 TO C-VEC C-ILU.
           PERFORM CARGO-VEC THRU CARGO-V-F.
       SIGO.
           ACCEPT RESPUESTA LINE C-ILU POSITION 40 NO BEEP 
               ON EXCEPTION CODIGO-TECLA NEXT SENTENCE.
           IF CODIGO-TECLA = 52 GO TO ARRIBA.
           IF CODIGO-TECLA = 53 GO TO ABAJO.
           IF CODIGO-TECLA = 13 GO TO SALGO.
           GO TO ABAJO.

       ARRIBA.
           IF C-ILU = 1 GO TO MAS-ARRIBA.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU, POSITION 1
                   CONTROL "BCOLOR = BLUE, FCOLOR = WHITE".  
           COMPUTE C-ILU = C-ILU - 1.
           IF C-ILU = 0 GO TO MAS-ARRIBA.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU POSITION 1 REVERSE.
           GO TO SIGO.

       MAS-ARRIBA. 
           MOVE REG-VEC(9) TO REG-VEC(10).
           MOVE REG-VEC(8) TO REG-VEC(9).
           MOVE REG-VEC(7) TO REG-VEC(8).
           MOVE REG-VEC(6) TO REG-VEC(7).
           MOVE REG-VEC(5) TO REG-VEC(6).
           MOVE REG-VEC(4) TO REG-VEC(5).
           MOVE REG-VEC(3) TO REG-VEC(4).
           MOVE REG-VEC(2) TO REG-VEC(3).
           MOVE REG-VEC(1) TO REG-VEC(2).
           MOVE 1 TO C-VEC C-ILU.
           PERFORM CARGO-ATRAS THRU CARGO-A-F.
           GO TO SIGO.

       ABAJO.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU, POSITION 1
                   CONTROL "FCOLOR = WHITE, BCOLOR = BLUE".
           COMPUTE C-ILU = C-ILU + 1.
           IF C-ILU > 10 GO TO MAS-ABAJO.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU POSITION 1 REVERSE.
           GO TO SIGO.

       MAS-ABAJO. 
           MOVE REG-VEC(2) TO REG-VEC(1).
           MOVE REG-VEC(3) TO REG-VEC(2).
           MOVE REG-VEC(4) TO REG-VEC(3).
           MOVE REG-VEC(5) TO REG-VEC(4).
           MOVE REG-VEC(6) TO REG-VEC(5).
           MOVE REG-VEC(7) TO REG-VEC(6).
           MOVE REG-VEC(8) TO REG-VEC(7).
           MOVE REG-VEC(9) TO REG-VEC(8).
           MOVE REG-VEC(10) TO REG-VEC(9).
           MOVE 10 TO C-VEC C-ILU.
      ********************************************************
      * MOVER CLAVE ALFABETICA DEL VECTOR A CLAVE ALFABETICA *
      * DEL ARCHIVO CORRESPONDIENTE Y ME POSICIONO           *
      ********************************************************
           MOVE D-3-VEC(C-VEC) TO NOM-POSTAL.
           START CPOSTAL KEY IS > NOM-POSTAL
                  INVALID KEY GO TO SIGO.

           PERFORM CARGO-V-1 THRU CARGO-V-F.
           GO TO SIGO.

       SALGO.
      ************************************************************
      * MUEVO LOS DATOS DEL VECTOR A LOS CAMPOS DE TRANSFERENCIA *
      ************************************************************    
           MOVE D-1-VEC(C-ILU) TO C-POS-T.
           MOVE D-2-VEC(C-ILU) TO C-POS-1-T.
           MOVE D-3-VEC(C-ILU) TO NOM-POSTAL-T.
      *     MOVE D-4-VEC(C-ILU) TO --- .
      *     MOVE D-5-VEC(C-ILU) TO --- .
      *     MOVE D-6-VEC(C-ILU) TO --- .
      ************************************************************
      * CIERRO EL ARCHIVO Y LA VENTANA *
      **********************************
           CLOSE CPOSTAL.
           DISPLAY ESTRUCTURA-VENTANA CONTROL "WINDOW-REMOVE".
           EXIT PROGRAM. 

       CARGO-VEC.
           MOVE 1 TO C-ILU C-VEC.
      ************************************************************
      * MUEVO CAMPO TRANSFERENCIA CLAVE A LA CLAVE ALFABETICO Y  *
      * ME POSICIONO EN ARCHIVO                                  *
      ************************************************************
           MOVE NOM-POSTAL-T TO NOM-POSTAL.
           START CPOSTAL KEY IS NOT < NOM-POSTAL 
                          INVALID KEY GO TO SALGO.
       CARGO-V-1.
      ******************************
      * LEO ARCHIVO HACIA ADELANTE *
      ******************************
           READ CPOSTAL NEXT AT END GO TO CARGO-V-B.
           PERFORM MUEVO-D.
           ADD 1 TO C-VEC.
           IF C-VEC > 10 GO TO CARGO-V-D.
           GO TO CARGO-V-1. 
       CARGO-V-B.
           PERFORM MUEVO-0.
           ADD 1 TO C-VEC.
           IF C-VEC > 10 GO TO CARGO-V-D.
           GO TO CARGO-V-B.
       CARGO-V-D.
           MOVE 1 TO C-VEC.
       CARGO-V-D1.
           DISPLAY REG-VEC(C-VEC) LINE C-VEC, POSITION 1
                   CONTROL "FCOLOR = WHITE, BCOLOR = BLUE".
           ADD 1 TO C-VEC.
           IF C-VEC > 10 GO TO CARGO-V-I.
           GO TO CARGO-V-D1.
       CARGO-V-I.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU, POSITION 1 REVERSE.
       CARGO-V-F.
           EXIT.

       CARGO-ATRAS.
      **********************************************************
      * MUEVO CLAVE DEL VECTOR A LA DEL ARCHIVO Y ME POSICIONO *
      **********************************************************
           MOVE D-3-VEC(2) TO NOM-POSTAL.
           START CPOSTAL KEY IS < NOM-POSTAL
                     INVALID KEY GO TO CARGO-A-F.
       CARGO-A-1.
           MOVE 1 TO C-VEC.
      ***************************
      * LEO ARCHIVO HACIA ATRAS *
      ***************************
           READ CPOSTAL PREVIOUS AT END GO TO CARGO-A-B.
           PERFORM MUEVO-D.
           COMPUTE C-VEC = C-VEC - 1.
           IF C-VEC = 0 GO TO CARGO-A-D.
           GO TO CARGO-A-D. 
       CARGO-A-B.
           PERFORM MUEVO-0.
           COMPUTE C-VEC = C-VEC - 1.
           IF C-VEC = 0 GO TO CARGO-A-D.
           GO TO CARGO-A-B.
       CARGO-A-D.
           MOVE 1 TO C-VEC.
       CARGO-A-D1.
           DISPLAY REG-VEC(C-VEC) LINE C-VEC, POSITION 1
                    CONTROL "BCOLOR = BLUE, FCOLOR = WHITE".
           ADD 1 TO C-VEC.
           IF C-VEC > 10 GO TO CARGO-A-I.
           GO TO CARGO-A-D1.
       CARGO-A-I.
           DISPLAY REG-VEC(C-ILU) LINE C-ILU, POSITION 1 REVERSE.
       CARGO-A-F.
           EXIT.
       MUEVO-0.
      *******************************************************
      * MUEVO 0 o SPACES A LOS CAMPOS DEL VECTOR NECESARIOS *
      *******************************************************
           MOVE 0 TO D-1-VEC(C-VEC) D-2-VEC(C-VEC).
           MOVE SPACES TO D-3-VEC(C-VEC).
      *******************************************************
       MUEVO-D.
      *************************************
      * HABILITAR DATOS A MOVER AL VECTOR *
      *************************************
           MOVE C-POS TO D-1-VEC(C-VEC).
           MOVE C-POS-1 TO D-2-VEC(C-VEC).
           MOVE NOM-POSTAL TO D-3-VEC(C-VEC).
      *     MOVE --- TO D-4-VEC(C-VEC).
      *     MOVE --- TO D-5-VEC(C-VEC).
      *     MOVE --- TO D-6-VEC(C-VEC).
      **************************************