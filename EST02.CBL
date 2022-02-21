       IDENTIFICATION DIVISION.
       PROGRAM-ID. EST02.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. JUL'90.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ART02.SEL".
       COPY "ART01.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "EST02.SEL".
       COPY "ENVASE.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART02.REG".
       COPY "ART01.REG".
       COPY "EST02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "ENVASE.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 COMPRI.
             03 AA           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(6) VALUE "MARCA ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(17) VALUE " ----ENVASE----- ".
             03 FILLER       PIC X(3) VALUE "MES".
             03 FILLER       PIC X(12) VALUE " -CANTIDAD- ".
             03 FILLER       PIC X(4) VALUE "MON ".
             03 FILLER       PIC X(15) VALUE "PRECIO PROMED. ".
             03 FILLER       PIC X(15) VALUE "-COSTO PROM.-  ".
             03 FILLER       PIC X(13) VALUE "-UTIL. BRUTA-". 
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM-ED       PIC ZZZZZ9B.
             03 MAR-ED       PIC 9999BB.
             03 DET-ED       PIC X(30)B.
             03 ENV-ED       PIC X(15)B.
             03 MES-V-ED       PIC Z9BB.
             03 UNI-V-ED     PIC ZZZZZ9,999B.
             03 MON-V-ED     PIC XXXB.
             03 VAL-V-ED     PIC ZZ.ZZZ.ZZ9,99BB.
             03 COS-V-ED     PIC ZZ.ZZZ.ZZ9,99BB.
             03 UTI-V-ED     PIC --.---.--9,99.  
          02 TOT-1.
             03 FILLER       PIC X(47) VALUE SPACES.
             03 DET-T-ED     PIC X(13).
             03 MES-T-ED     PIC Z9BB BLANK WHEN ZERO.
             03 FILLER       PIC X(11) VALUE SPACES. 
             03 MON-T-ED     PIC XXXB.
             03 VAL-T-ED     PIC ZZ.ZZZ.ZZ9,99BB.
             03 COS-T-ED     PIC ZZ.ZZZ.ZZ9,99BB.
             03 UTI-T-ED     PIC --.---.--9,99.
          02 LIN-1.
             03 FILLER       PIC X(47) VALUE SPACES.
             03 FILLER       PIC X(84) VALUE ALL "-".
       01 TOTAL-1.
          02 VAL-T-1         PIC 9(15)V99 OCCURS 13.
          02 COS-T-1         PIC 9(15)V99 OCCURS 13.
          02 UTI-T-1         PIC S9(14)V99 OCCURS 13.
       01 TOTAL-2.
          02 VAL-T-2         PIC 9(15)V99 OCCURS 13.
          02 COS-T-2         PIC 9(15)V99 OCCURS 13.
          02 UTI-T-2         PIC S9(14)V99 OCCURS 13.        
       01 MES-CAP            PIC 99 VALUE 0.
       01 MES-H              PIC 99 VALUE 0.
       01 MES-D              PIC 99 VALUE 0.
       01 UTI-V-CAP          PIC S9(13)V99 VALUE 0.
       01 UTI-V-EDI          PIC ----.---.--9,99.
       01 T                  PIC 99 VALUE 0.
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 SAL-S              PIC S9(6)V999.
       01 SAL-E              PIC S9(6)V999.
       01 UNI-V              PIC 9(6)V999.
       01 IND-ORD            PIC 9.
       01 COD-CERO           PIC X VALUE SPACES.
       01 MARCA-L            PIC 9(3).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 SAL-S-EDI          PIC -----9,999.
       01 SAL-M-EDI          PIC ZZZZZZ.
       01 UNI-V-EDI          PIC ZZZZZ9,999.
       01 SAL-E-EDI          PIC -----9,999. 
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 DATOS.
          02 DATO     PIC X(71) OCCURS 10 TIMES.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART02.ERR".
       COPY "ART01.ERR".
       COPY "EST02.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
       COPY "ENVASE.ERR".
       COPY "EMPRE.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 > 25 GO TO SIGA-CLAVE.
           DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
           GO TO PANTA-INI.
       SIGA-CLAVE.
           IF CLAVE-1-CAP NOT = 0 GO TO SIGA-INI.
           PERFORM CLAVE-SYS.
           PERFORM BORRA-CLAVE.
       SIGA-INI.
           PERFORM TIT-PAN.
           PERFORM TITU-PAN.
           PERFORM FECHA-PAN.
           PERFORM OPC-MODA.
           PERFORM MEN-MODA-A.
       CARGO-MOD.
            MOVE 3 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F01 GO TO ACTU.
           IF F03 GO TO CONS-NUM.
           IF F05 GO TO LIST-NUM.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " ACT. ESTADISTICA " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM BORRA-PANTA.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "CORRESPONDE MES :" LINE 12 POSITION 3.
           DISPLAY "BORRA ACUMULADO :" LINE 14 POSITION 3.
           DISPLAY " <S> Si <N> No " LINE 14 POSITION 25 REVERSE.
           DISPLAY " PRECAUCION :                     " LINE 16 
               POSITION 24 BLINK REVERSE.
           DISPLAY "             Modifica los valores " LINE 17
               POSITION 24 REVERSE.
           DISPLAY " de las estadisticas para el mes  " LINE 18
               POSITION 24 REVERSE.
           DISPLAY " solicitado por Ud.               " LINE 19
               POSITION 24 REVERSE. 
           MOVE SPACES TO MEN-1 MEN-2 MEN-3.
           PERFORM MEN-MODA-S.
       SIGA-ACTU.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       SIGA-ACTU-1.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           ACCEPT MES-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                  SIZE TAB-SIZ(SW3) NO BEEP TAB
                  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF MES-CAP = 0 GO TO SIGA-ACTU-1.
           IF MES-CAP > 12 GO TO SIGA-ACTU-1.
           MOVE NUM-EMP TO EMP-EST02 EMP-ART01 EMP-ART02.
           MOVE SUC-EMP TO SUC-EST02 SUC-ART02.
       SIGA-ACTU-2.
           MOVE 22 TO SW3.
           MOVE "N" TO COD-CERO.
           MOVE SPACES TO ALF-CAP.
           PERFORM CAP-ALF.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO SIGA-ACTU-1.
           IF ALF-CAP = SPACES MOVE "N" TO ALF-CAP.
           MOVE ALF-CAP TO COD-CERO.
           DISPLAY "ESTOY TRABAJANDO !!!" LINE 21 POSITION 21 BLINK.
           OPEN I-O EST02
                I-O ART02
                INPUT ART01
                INPUT MARCAS
                INPUT ENVASE
                INPUT NUMERO.
           MOVE 0 TO NUM-ART02.
           MOVE 0 TO MAR-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02
                       INVALID KEY PERFORM C-ACTU
                                   EXIT PROGRAM
                                   STOP RUN.
       SIGA-A1.
           READ ART02 NEXT AT END PERFORM C-ACTU
                                  GO TO INI-MOD.
           IF EMP-ART02 NOT = NUM-EMP GO TO SIGA-A1.
           IF SUC-ART02 NOT = SUC-EMP GO TO SIGA-A1.
           MOVE NUM-ART02 TO NUM-EST02.
           MOVE MAR-ART02 TO MAR-EST02.
           MOVE UNI-V-ART02 TO UNI-V-EST02.
           MOVE COS-V-ART02 TO COS-V-EST02.
           MOVE VAL-V-ART02 TO VAL-V-EST02.
           MOVE MES-CAP TO MES-EST02.
           IF UNI-V-ART02 = 0 GO TO SIGA-A1.
           IF PRE-V-ART02 NOT = 0 MOVE " $ " TO MON-V-EST02.
           IF PRE-E-ART02 NOT = 0 MOVE "U$S" TO MON-V-EST02.
           IF PRE-V-ART02 = 0 AND PRE-E-ART02 = 0 
                 MOVE " " TO MON-V-EST02.
           REWRITE REG-EST02 INVALID KEY WRITE REG-EST02.
           IF COD-CERO = "S" OR COD-CERO = "s"
              MOVE 0 TO UNI-V-ART02 COS-V-ART02 VAL-V-ART02
              REWRITE REG-ART02 INVALID KEY WRITE REG-ART02.
           GO TO SIGA-A1.
       CONS-NUM.
           PERFORM MEN-MODA-S.
           PERFORM BORRA-PANTA.
           MOVE 1 TO SW3.
           MOVE "CONSULTA NUMERICA" TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANTA-CONS.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02 EMP-EST02.
           MOVE SUC-EMP TO SUC-ART02 SUC-EST02.
       C-N-DESDE.
           MOVE "X" TO COD3.
           PERFORM MEN-F-CONS.
           PERFORM BORRA-C.
           MOVE 11 TO SW3.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3) 
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           IF F02 GO TO CONS-NUM.
           MOVE NUM-CAP TO MES-EST02.
       C-N-DESDE1.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                          SIZE TAB-SIZ(SW3) NO BEEP TAB
                          ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO C-N-DESDE.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO COD-ART02 COD-EST02.
       C-N-POSI.
           OPEN INPUT ART02
                INPUT NUMERO
                INPUT MARCAS
                INPUT EST02.
           MOVE SPACES TO MON-V-EST02.
           START EST02 KEY IS NOT < CLAVE-EST02
                         INVALID KEY CLOSE NUMERO EST02 MARCAS ART02
                                     GO TO C-N-DESDE.
           MOVE "N" TO COD2.
           GO TO C-SIGO-N.
       C-LEO-N.
           MOVE 3 TO SW3.
           PERFORM MEN-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO EST02 ART02 MARCAS GO TO INI-MOD.
           IF F03 GO TO C-SIGO-N.
           PERFORM MEN-F-CONS.
           PERFORM CAP-COD1.
           IF ESCAP CLOSE NUMERO EST02 ART02 MARCAS GO TO INI-MOD.
           IF F01 GO TO F-CONS.
       C-SIGO-N.
           IF COD3 = "F" GO TO F-CONS.
           PERFORM LEO-EST02.
      *     PERFORM LEO-ART02.
           IF COD3 = "F" GO TO F-CONS.
           IF EMP-EST02 > NUM-EMP GO TO C-SIGO-N.
           IF SUC-EST02 > SUC-EMP GO TO C-SIGO-N.
           IF UNI-V-EST02 = 0 GO TO C-SIGO-N.
           MOVE NUM-EST02 TO COD-NUM.
           PERFORM LEO-NUM-R.
      *     IF ACT-PRI-EMP = 99999
      *            MOVE NUM-ART02 TO N-ART02
      *            COMPUTE COD-MAR = N-1-ART02 * 1
      *      ELSE MOVE MAR-ART02 TO COD-MAR.
           MOVE MAR-EST02 TO COD-MAR.
           PERFORM LEO-MAR-R.
           IF COD3 = "F" GO TO F-CONS.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           DISPLAY MES-EST02 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           PERFORM DES-COD.
           PERFORM DES-DET THRU DES-MAR.
           PERFORM DES-UNI THRU DES-UTI.
           GO TO C-LEO-N.
       F-CONS.
           CLOSE NUMERO EST02 ART02 MARCAS.
           IF COD2 = "N" GO TO C-N-DESDE.
           GO TO C-N-DESDE.     
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "HASTA: " LINE 9 POSITION 40.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "MES DESDE      :" LINE 12 POSITION 3.
           DISPLAY "MES HASTA      :" LINE 14 POSITION 3.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       L-N-DESDE.
           MOVE NUM-EMP TO EMP-ART02 EMP-EST02 EMP-ART01.
           MOVE SUC-EMP TO SUC-ART02 SUC-EST02.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D COD-EST02.
       L-N-HASTA.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-DESDE.
           MOVE NUM-CAP TO LISTO-H.
           MOVE "N" TO COD2.
       L-N-M-DESDE.
           PERFORM MEN-ANTE.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-HASTA.
           MOVE NUM-CAP TO MES-D MES-EST02.
       L-N-M-HASTA.
           PERFORM MEN-ANTE.
           MOVE 20 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP GO TO INI-MOD.
           IF F02 GO TO L-N-M-DESDE.
           MOVE NUM-CAP TO MES-H.
       L-N-POSI.
           OPEN OUTPUT IMP02.
           OPEN INPUT ART02.
           OPEN INPUT ART01.
           OPEN INPUT ENVASE.
           OPEN INPUT EST02.
           OPEN INPUT NUMERO.
           OPEN INPUT MARCAS.
           MOVE "X" TO COD3.
           MOVE SPACES TO MON-V-EST02.
           START EST02 KEY IS NOT < CLAVE-EST02
                        INVALID KEY CLOSE ART02 MARCAS NUMERO IMP02
                              CLOSE EST02 ART01 ENVASE
                              GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE 0 TO CONT-P CONT-I CONT-R.
           MOVE "      <<< " TO TIT-1-EDI.
           MOVE "LISTADO NU" TO TIT-2-EDI.
           MOVE "MERICO DE " TO TIT-3-EDI.
           MOVE "ESTADISTIC" TO TIT-4-EDI.
           MOVE "A -VENTA- " TO TIT-5-EDI.
           MOVE ">>>       " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           MOVE 0 TO T.
           PERFORM ACERO 13 TIMES.
           MOVE 0 TO T.
           GO TO L-LEO.
       ACERO.
           ADD 1 TO T.
           MOVE 0 TO VAL-T-1(T) VAL-T-2(T) COS-T-1(T) COS-T-2(T).
           MOVE 0 TO UTI-T-1(T) UTI-T-2(T).
       L-LEO.
           IF CONT-R > 67 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          MOVE 0 TO CONT-R
                          PERFORM TITU.
           PERFORM LEO-EST02.
           IF COD-EST02 > LISTO-H GO TO L-LEO.
           IF MES-EST02 < MES-D GO TO L-LEO.
           IF MES-EST02 > MES-H GO TO L-LEO.
           IF UNI-V-EST02 = 0 GO TO L-LEO.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP02 NUMERO MARCAS ART02 EST02
                         CLOSE ART01 ENVASE
                         GO TO L-N-DESDE.
           MOVE COD-EST02 TO COD-ART01.
           READ ART01 INVALID KEY MOVE 0 TO ENV-ART01.
           MOVE ENV-ART01 TO NUM-ENVA.
           READ ENVASE INVALID KEY MOVE "******" TO NOM-ENVA.
           MOVE NUM-EST02 TO COD-NUM.
           PERFORM LEO-NUM-R.
           IF COD3 = "F" PERFORM PIE-P
                         CLOSE IMP02 NUMERO MARCAS ART02 EST02
                         CLOSE ART01 ENVASE
                         GO TO L-N-DESDE.
           MOVE NUM-EST02 TO N-ART02.
      *     IF MARCA-L = 0 PERFORM CARGA-MARCA.
           IF CONT-R = 0 PERFORM TITU.     
      *     IF ACT-PRI-EMP = 99999
      *           IF N-1-ART02 NOT = MARCA-L
      *                 GO TO C-MARCA
      *           ELSE GO TO SIGO-L.
      *     IF MAR-ART02 = MARCA-L GO TO SIGO-L.
           GO TO SIGO-L.
       SIGO-L.               
           PERFORM MUEVO-IMP.
           IF COD-EST02 = SPACES GO TO L-LEO.
           IF UNI-V-EST02 = 0 GO TO L-LEO.
           WRITE LIN-IMP02 FROM IMP-3 AFTER ADVANCING 1.
           ADD 1 TO CONT-R.
           ADD 1 TO CONT-I.
           GO TO L-LEO.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           MOVE ANO TO AA-E132.
           MOVE MES TO MM-E132.
           MOVE DIA TO DD-E132.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E132.
           MOVE MINU TO MI-E132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
      *     WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
      *     WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
      *     ADD 10 TO CONT-R.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE 0 TO T.
           PERFORM IMPRE-TOT 12 TIMES.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           MOVE "TOTALES EN" TO DET-T-ED.
           MOVE 0 TO MES-T-ED.
           MOVE " $ " TO MON-T-ED.
           MOVE VAL-T-1(13) TO VAL-T-ED.
           MOVE COS-T-1(13) TO COS-T-ED.
           MOVE UTI-T-1(13) TO UTI-T-ED.
           WRITE LIN-IMP02 FROM TOT-1 AFTER 1.
           ADD 1 TO CONT-R.
           MOVE "U$S" TO MON-T-ED.
           MOVE VAL-T-2(13) TO VAL-T-ED.
           MOVE COS-T-2(13) TO COS-T-ED.
           MOVE UTI-T-2(13) TO UTI-T-ED.
           WRITE LIN-IMP02 FROM TOT-1 AFTER 1.
           ADD 1 TO CONT-R.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
       IMPRE-TOT.
           ADD 1 TO T.
           IF CONT-R > 68 COMPUTE CONT-R = 72 - CONT-R
                          PERFORM ESPA CONT-R TIMES
                          PERFORM TITU.
           MOVE T TO MES-T-ED.
           MOVE "SUBTOTAL MES" TO DET-T-ED.
           MOVE " $ " TO MON-T-ED.
           MOVE VAL-T-1(T) TO VAL-T-ED.
           MOVE COS-T-1(T) TO COS-T-ED.
           MOVE UTI-T-1(T) TO UTI-T-ED.
           IF VAL-T-1(T) NOT = 0
             WRITE LIN-IMP02 FROM TOT-1 AFTER 1
             ADD 1 TO CONT-R.
           MOVE "U$S" TO MON-T-ED.
           MOVE VAL-T-2(T) TO VAL-T-ED.
           MOVE COS-T-2(T) TO COS-T-ED.
           MOVE UTI-T-2(T) TO UTI-T-ED.
           IF VAL-T-2(T) NOT = 0
              WRITE LIN-IMP02 FROM TOT-1 AFTER 1
              ADD 1 TO CONT-R.
       LEO-EST02.
           READ EST02 NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-ART02.
           READ ART02 NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT WITH NO LOCK AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO WITH NO LOCK INVALID KEY MOVE "F" TO COD3.
       LEO-MAR-R.
           READ MARCAS WITH NO LOCK INVALID KEY MOVE "F" TO COD3.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 254101 TO TAB-POS(3).
           MOVE 091010 TO TAB-POS(4).
           MOVE 094710 TO TAB-POS(5).
           MOVE 091020 TO TAB-POS(6).
           MOVE 074720 TO TAB-POS(7).
           MOVE 082104 TO TAB-POS(8).
           MOVE 083402 TO TAB-POS(9).
           MOVE 083920 TO TAB-POS(10).
           MOVE 122102 TO TAB-POS(11).
           MOVE 142110 TO TAB-POS(12)
           MOVE 162112 TO TAB-POS(13).
           MOVE 172115 TO TAB-POS(14).
           MOVE 182115 TO TAB-POS(15)
           MOVE 192115 TO TAB-POS(16).
           MOVE 143530 TO TAB-POS(17).
           MOVE 153530 TO TAB-POS(18).
           MOVE 202115 TO TAB-POS(19).
           MOVE 142102 TO TAB-POS(20).
           MOVE 232503 TO TAB-POS(21).
           MOVE 142101 TO TAB-POS(22).
           MOVE 222103 TO TAB-POS(23).
           MOVE 222503 TO TAB-POS(24).
           MOVE 222903 TO TAB-POS(25).
           MOVE 223303 TO TAB-POS(26).
           MOVE 223703 TO TAB-POS(27).
           MOVE 103530 TO TAB-POS(28).
           MOVE 132108 TO TAB-POS(29).
           MOVE 113520 TO TAB-POS(30).
           MOVE 212110 TO TAB-POS(31).
       TIT-PAN.
           MOVE "  ARCHIVO " TO TIT-1-PAN.
           MOVE "ESTADISTIC" TO TIT-2-PAN.
           MOVE "A -VENTA-" TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  ACT. ESTADISTICA" LINE 14 POSITION 18.
           DISPLAY "<F3>  CONS. NUMERICA " LINE 15 POSITION 18.
           DISPLAY "<F5>  LIST. NUMERICO" LINE 16 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 17 POSITION 18.
       PANTA-CONS.
           DISPLAY "EMPRESA         :" LINE  8 POSITION  3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           DISPLAY "CORRESPONDE MES :" LINE 12 POSITION 3.
           DISPLAY "CODIGO ARTICULO :" LINE 14 POSITION 3.
           DISPLAY "UNID. VENDIDAS  :" LINE 16 POSITION 3.
           DISPLAY "PRECIO PROMEDIO :" LINE 17 POSITION 3.
           DISPLAY "COSTO PROMEDIO  :" LINE 18 POSITION 3.
           DISPLAY "UTIL. BRUTA.    :" LINE 19 POSITION 3.           
       DES-DET.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-MAR.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       DES-COD.
           MOVE 12 TO SW3.
           PERFORM BORRA.
           DISPLAY COD-EST02 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).
       DES-UNI.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE UNI-V-EST02 TO UNI-V-EDI.
           DISPLAY UNI-V-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                               SIZE 10.
       DES-MON.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           IF MON-V-EST02 = " $ " 
             DISPLAY " EN PESOS " LINE TAB-LIN(SW3) 
                     POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) BLINK REVERSE.
           IF MON-V-EST02 = "U$S"
             DISPLAY " EN DOLAR " LINE TAB-LIN(SW3) 
                     POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) BLINK REVERSE. 
       DES-VAL.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE VAL-V-EST02 TO DEC10-EDI.
           PERFORM DES-DEC10.
       DES-COS.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE COS-V-EST02 TO DEC10-EDI.
           PERFORM DES-DEC10. 
       DES-UTI.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           COMPUTE UTI-V-CAP ROUNDED = ( VAL-V-EST02 - COS-V-EST02 ) 
                        * UNI-V-EST02.
           MOVE UTI-V-CAP TO UTI-V-EDI.
           DISPLAY UTI-V-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
       E-ART01.
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "CARGUE ARCH.ART.-COMPRA-" TO ALF-CAP.
           PERFORM DES-ALF.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
       BORRA-C.
           MOVE 11 TO SW3.
           PERFORM BORRA.
           MOVE 12 TO SW3. 
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 19 TO SW3.
           PERFORM BORRA.
       LEA-EMPRE.
           OPEN INPUT EMPRESAS.
           MOVE 8 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           MOVE NUM-CAP TO NUM-EMP.
           MOVE 0 TO SUC-EMP.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                     GO TO LEA-EMPRE.
           MOVE 10 TO SW3.
           PERFORM BORRA.
           MOVE RAZ-SOC-EMP TO ALF-CAP EMP-E132.
           PERFORM DES-ALF.
           MOVE 9 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP CLOSE EMPRESAS GO TO INI-MOD.
           IF F02 CLOSE EMPRESAS GO TO LEA-EMPRE.
           MOVE NUM-CAP TO SUC-EMP NUM-SUC-E132.
           IF NUM-CAP = 0 GO TO F-LEA-EMPRE.
           READ EMPRESAS INVALID KEY CLOSE EMPRESAS
                                 GO TO LEA-EMPRE.
           MOVE 10 TO SW3.
           MOVE RAZ-SOC-EMP TO ALF-CAP SUC-E132.
           PERFORM DES-ALF.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       MUEVO-IMP.
           MOVE NUM-EST02 TO NUM-ED.
           MOVE MAR-EST02 TO MAR-ED.
           MOVE DET-NUM TO DET-ED.
           MOVE UNI-V-EST02 TO UNI-V-ED.
           MOVE VAL-V-EST02 TO VAL-V-ED.
           MOVE COS-V-EST02 TO COS-V-ED.
           COMPUTE UTI-V-CAP = ( VAL-V-EST02 - COS-V-EST02 ) *
                 UNI-V-EST02.
           MOVE UTI-V-CAP TO UTI-V-ED.
           MOVE MES-EST02 TO MES-V-ED.
           MOVE MON-V-EST02 TO MON-V-ED.
           MOVE NOM-ENVA TO ENV-ED.
           IF MON-V-EST02 = " $ "
             COMPUTE VAL-T-1(MES-EST02) = VAL-T-1(MES-EST02) +
                   VAL-V-EST02
             COMPUTE VAL-T-1(13) = VAL-T-1(13) + VAL-V-EST02
             COMPUTE COS-T-1(MES-EST02) = COS-T-1(MES-EST02) +
                   COS-V-EST02
             COMPUTE COS-T-1(13) = COS-T-1(13) + COS-V-EST02
             COMPUTE UTI-T-1(MES-EST02) = UTI-T-1(MES-EST02) +
                   UTI-V-CAP
             COMPUTE UTI-T-1(13) = UTI-T-1(13) + UTI-V-CAP.
           IF MON-V-EST02 = "U$S"
             COMPUTE VAL-T-2(MES-EST02) = VAL-T-2(MES-EST02) +
                   VAL-V-EST02
             COMPUTE VAL-T-2(13) = VAL-T-2(13) + VAL-V-EST02
             COMPUTE COS-T-2(MES-EST02) = COS-T-2(MES-EST02) +
                   COS-V-EST02
             COMPUTE COS-T-2(13) = COS-T-2(13) + COS-V-EST02
             COMPUTE UTI-T-2(MES-EST02) = UTI-T-2(MES-EST02) +
                   UTI-V-CAP
             COMPUTE UTI-T-2(13) = UTI-T-2(13) + UTI-V-CAP.
       C-ACTU.
           CLOSE ART02 EST02 ART01 NUMERO MARCAS ENVASE.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".