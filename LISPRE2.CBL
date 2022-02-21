       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISPRE2.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. MAR'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "EMPRE.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "PANTA.SEL".
       COPY "IMP02.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "EMPREBM.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "PANTA.REG".
       COPY "IMP02.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
          02 IMP-0.
             03 Z1       PIC 99 COMP-4 VALUE 15.
          02 IMP-10.
             03 FILLER       PIC X(20) VALUE SPACES.
             03 A1            PIC 99 COMP-4 VALUE 18.
             03 FILLER       PIC X(8) VALUE "MARCA : ".
             03 A2           PIC 99 COMP-4 VALUE 14.
             03 COD-MAR-ED   PIC 9(4).
             03 FILLER       PIC XXX VALUE " - ".
             03 MARCA-ED     PIC X(20).
             03 A3           PIC 99 COMP-4 VALUE 20.
             03 A4           PIC 99 COMP-4 VALUE 15.
          02 IMP-1.
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(5) VALUE " MON ".
             03 FILLER       PIC X(11) VALUE "PRECIO VTA.".
             03 FILLER       PIC X(12) VALUE " -POSICION--".
             03 FILLER       PIC XX VALUE "||".
             03 FILLER       PIC X(7) VALUE "NUMERO ".
             03 FILLER       PIC X(15) VALUE "---------- DETA".
             03 FILLER       PIC X(15) VALUE "LLE -----------".
             03 FILLER       PIC X(5) VALUE " MON ".
             03 FILLER       PIC X(11) VALUE "PRECIO VTA.".
             03 FILLER       PIC X(12) VALUE " -POSICION--".
          02 IMP-2.
             03 FILLER       PIC X(5) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(17) VALUE " Articulos.-".
          02 IMP-3.
             03 NUM1-ED       PIC ZZZZZ9B BLANK WHEN ZERO.
             03 DET1-ED       PIC X(30)B.
             03 MON1-ED       PIC XXXB.
             03 PRE1-V-ED     PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER        PIC X VALUE SPACES.
             03 SEC1-1-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC1-2-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC1-3-ED     PIC XXX.
             03 FILLER        PIC XX VALUE "||".
             03 NUM2-ED       PIC ZZZZZ9B BLANK WHEN ZERO.
             03 DET2-ED       PIC X(30)B.
             03 MON2-ED       PIC XXXB.
             03 PRE2-V-ED     PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER        PIC X VALUE SPACES.
             03 SEC2-1-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC2-2-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC2-3-ED     PIC XXX.
          02 IMP-4.
             03 FILLER        PIC 99 COMP-4 VALUE 14.
             03 SUB3-ED        PIC X(30) VALUE SPACES.
             03 FILLER        PIC 99 COMP-4 VALUE 20.
             03 FILLER        PIC X(5) VALUE SPACES.
             03 FILLER        PIC XX VALUE "||".
             03 NUM3-ED       PIC ZZZZZ9B BLANK WHEN ZERO.
             03 DET3-ED       PIC X(30)B.
             03 MON3-ED       PIC XXXB.
             03 PRE3-V-ED     PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER        PIC X VALUE SPACES.
             03 SEC3-1-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC3-2-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC3-3-ED     PIC XXX.
          02 IMP-5.
             03 NUM4-ED       PIC ZZZZZ9B BLANK WHEN ZERO.
             03 DET4-ED       PIC X(30)B.
             03 MON4-ED       PIC XXXB.
             03 PRE4-V-ED     PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
             03 FILLER        PIC X VALUE SPACES.
             03 SEC4-1-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC4-2-ED     PIC XXX.
             03 FILLER        PIC X VALUE "-".
             03 SEC4-3-ED     PIC XXX.
             03 FILLER        PIC XX VALUE "||".
             03 FILLER        PIC X VALUE SPACES.
             03 FILLER        PIC 99 COMP-4 VALUE 14.
             03 SUB4-ED       PIC X(30).
             03 FILLER        PIC 99 COMP-4 VALUE 20.
          02 IMP-6.
             03 FILLER        PIC 99 COMP-4 VALUE 14.
             03 SUB50-ED      PIC X(30) VALUE SPACES.
             03 FILLER        PIC 99 COMP-4 VALUE 20.
             03 FILLER        PIC X(5) VALUE SPACES.
             03 FILLER        PIC XX VALUE "||".
             03 FILLER        PIC X VALUE SPACES.
             03 FILLER        PIC 99 COMP-4 VALUE 14.
             03 SUB51-ED      PIC X(30) VALUE SPACES.
             03 FILLER        PIC 99 COMP-4 VALUE 20.      
       01 LISTO-D            PIC 9(10).
       01 LISTO-H            PIC 9(10).
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 MARCA-L            PIC 9(4).
       01 FEC-EDI            PIC ZZ/ZZ/ZZ.
       01 NUM-MAR.
          02 N-ART02            PIC 9(6).
          02 NU-ART02 REDEFINES N-ART02.
             03 N-1-ART02       PIC 9(3).
             03 N-2-ART02       PIC 9(3).
       01 QQ                 PIC X.
       01 CONT-V             PIC 999 VALUE ZERO.
       01 CONT-I1            PIC 999 VALUE  ZERO.
       01 CONT-I2            PIC 999 VALUE ZERO.
       01 DATOS.
          02 DATO OCCURS 120 TIMES.
             03 NUM          PIC 9(6).
             03 DET          PIC X(30).
             03 MON          PIC XXX.
             03 PRE          PIC 9(7)V99.
             03 SEC1         PIC XXX.
             03 SEC2         PIC XXX.
             03 SEC3         PIC XXX.
       01 FEC-I.
          03 DD-I            PIC 99.
          03 FEC-V-I         PIC 9999.
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "ART01.ERR".
       COPY "ART02.ERR".
       COPY "MARCAS.ERR".
       COPY "NUMERO.ERR".
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
           MOVE 3 TO SW3.
       CARGO-MOD.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP GO TO FFF.
           EXIT PROGRAM.
           IF F05 GO TO LIST-NUM.
           IF F06 GO TO LIST-ALF.
           GO TO CARGO-MOD.
       LIST-ALF.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO ALFABETICO " TO D20-CAP.
           PERFORM MODA-PAN.
           PERFORM PANT-LIS.
           MOVE 0 TO CONT-R CONT-I.
       L-A-DESDE.
           PERFORM LIMPIA-VEC.
           PERFORM BORRA-PANTA-C.
           DISPLAY "INGRESE MARCA A LISTAR" LINE 23 POSITION 3 BLINK
                                            REVERSE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           DISPLAY SPACES LINE 23 POSITION 3 SIZE 30.
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D MAR-ART02.
           MOVE MAR-ART02 TO LISTO-H.
           MOVE MAR-ART02 TO COD-MAR.
           PERFORM CARGA-MARCA.
           PERFORM DES-MAR.
           DISPLAY "INGRESE NOMBRE DESDE:" LINE 11 POSITION 3.
           ACCEPT DET-NUM LINE 11 POSITION 25 SIZE 30 TAB.
       L-A-POSI.
           OPEN OUTPUT IMP02.
           WRITE LIN-IMP02 FROM IMP-0 AFTER 0.
           OPEN INPUT ART02 ART01.
           OPEN INPUT NUMERO.
           MOVE "X" TO COD3.
           START NUMERO KEY IS NOT < DET-NUM
                        INVALID KEY PERFORM CIERRE-A
                        GO TO L-A-DESDE.
           MOVE "A" TO COD2.
           MOVE 0 TO CONT-P CONT-R CONT-I.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<< LISTA " TO TIT-2-EDI.
           MOVE "ALFABETICA" TO TIT-3-EDI.
           MOVE " DE -PRECI" TO TIT-4-EDI.
           MOVE "OS- >>>   " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           GO TO L-LEO-A.
       L-LEO-A.
           IF CONT-V = 120 PERFORM IMPRE. 
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
           IF COD3 = "F" PERFORM IMPRE
                         PERFORM CIERRE-A
                         GO TO L-A-DESDE.
           IF COD-NUM = SPACES GO TO L-LEO-A.
      *     IF ACT-PRI-EMP = 99999 
      *          MOVE COD-NUM TO N-ART02 NUM-ART02
      *          MOVE 0 TO MAR-ART02
      *          IF N-1-ART02 NOT = MARCA-L GO TO L-LEO-A.
      *     IF ACT-PRI-EMP NOT = 99999
           MOVE COD-NUM TO NUM-ART02
           MOVE COD-MAR TO MAR-ART02.
           READ ART02 INVALID KEY GO TO L-LEO-A.
           PERFORM MUEVO-IMP THRU MUEVO-IMP-F.
           GO TO L-LEO-A.
       LIST-NUM.
           PERFORM BORRA-PANTA.
           PERFORM MEN-MODA-S.
           MOVE 1 TO SW3.
           MOVE " LISTADO NUMERICO " TO D20-CAP.
           PERFORM MODA-PAN.
           MOVE 0 TO CONT-R CONT-I.
       PANT-LIS.
           DISPLAY "DESDE: " LINE 9 POSITION 3.
           DISPLAY "EMPRESA        :" LINE 8 POSITION 3.
           DISPLAY "- SUC :" LINE 8 POSITION 26.
           PERFORM LEA-EMPRE THRU F-LEA-EMP.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           MOVE NUM-EMP TO EMP-ART02.
           MOVE SUC-EMP TO SUC-ART02.
       L-N-DESDE.
           PERFORM BORRA-PANTA-C.
           DISPLAY "INGRESE MARCA A LISTAR" LINE 23 POSITION 3 BLINK
                                            REVERSE.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           DISPLAY SPACES LINE 23 POSITION 3 SIZE 30
           IF ESCAP GO TO INI-MOD.
           IF F01 GO TO INI-MOD.
           MOVE NUM-CAP TO LISTO-D LISTO-H COD-MAR.
           PERFORM CARGA-MARCA.
           PERFORM DES-MAR. 
           MOVE COD-MAR TO MAR-ART02.
       L-N-POSI.
           PERFORM LIMPIA-VEC.
           OPEN OUTPUT IMP02.
           WRITE LIN-IMP02 FROM IMP-0  AFTER 0.
           OPEN INPUT ART02 ART01.
           OPEN INPUT NUMERO.
           MOVE 0 TO NUM-ART02.
           START ART02 KEY IS NOT < CLAVE-ART02
                        INVALID KEY PERFORM CIERRE-A
                        GO TO L-N-DESDE.
           MOVE "N" TO COD2.
           MOVE "X" TO COD3.
           MOVE 0 TO CONT-P CONT-I CONT-R CONT-V.
           MOVE "          " TO TIT-1-EDI.
           MOVE "<<< LISTA " TO TIT-2-EDI.
           MOVE "NUMERICA D" TO TIT-3-EDI.
           MOVE "E -PRECIOS" TO TIT-4-EDI.
           MOVE "- >>>     " TO TIT-5-EDI.
           MOVE "          " TO TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
           PERFORM MEN-LIS.
           GO TO L-LEO.
       L-LEO.
           IF CONT-V = 120 PERFORM IMPRE.
           PERFORM LEO-ART02.
           MOVE NUM-ART02 TO COD-NUM.
           PERFORM LEO-NUM-R.
           IF COD3 = "F" DISPLAY "FINAL ART02" LINE 19 POSITION 10
                         PERFORM IMPRE
                         PERFORM CIERRE-A
                         GO TO L-N-DESDE.
           DISPLAY CONT-V LINE 17 POSITION 10.
           DISPLAY NUM-ART02 LINE 18 POSITION 10.
           DISPLAY MAR-ART02 LINE 18 POSITION 20.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 20.
           IF MAR-ART02 NOT = LISTO-H 
                DISPLAY "NO GUARDO" LINE 18 POSITION 30
                                    GO TO L-LEO.
           MOVE NUM-ART02 TO N-ART02.
      *    IF ACT-PRI-EMP = 99999
      *          IF N-1-ART02 NOT = MARCA-L
      *                PERFORM IMPRE
      *                PERFORM CIERRE-A
      *                 GO TO L-N-DESDE
      *           ELSE GO TO SIGO-L.
            GO TO SIGO-L.
       SIGO-L.               
           DISPLAY "GUARDO !!!" LINE 18 POSITION 30 BLINK REVERSE.
           PERFORM MUEVO-IMP THRU MUEVO-IMP-F.
           GO TO L-LEO.
       CARGA-MARCA.
           OPEN INPUT MARCAS.
           MOVE SPACES TO DET-MAR.
      *     IF ACT-PRI-EMP = 99999 MOVE N-1-ART02 TO COD-MAR.
           READ MARCAS INVALID KEY MOVE "********" TO MARCA-ED.
           IF DET-MAR NOT = SPACES MOVE DET-MAR TO MARCA-ED.
           MOVE COD-MAR TO MARCA-L COD-MAR-ED.
           CLOSE MARCAS.
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
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-1 AFTER 1.
           ADD 8 TO CONT-R.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       PIE-P.
           PERFORM ESPA.
           MOVE CONT-I TO CONT-L-ED.
           WRITE LIN-IMP02 FROM IMP-2 AFTER 1.
           MOVE 0 TO CONT-I.
           ADD 1 TO CONT-R.
       LEO-ART02.
           READ ART02 NEXT AT END MOVE "F" TO COD3.
       LEO-NUM.
           READ NUMERO NEXT AT END MOVE "F" TO COD3.
       LEO-NUM-R.
           READ NUMERO INVALID KEY 
                   DISPLAY "ERROR EN :" LINE 20 POSITION 10
                   DISPLAY COD-NUM LINE 20 POSITION 20 REVERSE
                   ACCEPT QQ LINE 20 POSITION 60.
      *                      MOVE "F" TO COD3.
       LEO-MAR-R.
           READ MARCAS INVALID KEY MOVE "F" TO COD3.
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
           MOVE 102110 TO TAB-POS(11).
           MOVE 122115 TO TAB-POS(12)
           MOVE 142106 TO TAB-POS(13).
           MOVE 152115 TO TAB-POS(14).
           MOVE 162115 TO TAB-POS(15)
           MOVE 172115 TO TAB-POS(16).
           MOVE 182108 TO TAB-POS(17).
           MOVE 192106 TO TAB-POS(18).
           MOVE 202106 TO TAB-POS(19).
           MOVE 222103 TO TAB-POS(20).
           MOVE 222503 TO TAB-POS(21).
           MOVE 222903 TO TAB-POS(22).
           MOVE 212103 TO TAB-POS(23).
           MOVE 212503 TO TAB-POS(24).
           MOVE 212903 TO TAB-POS(25).
           MOVE 213303 TO TAB-POS(26).
           MOVE 213703 TO TAB-POS(27).
           MOVE 103530 TO TAB-POS(28).
           MOVE 132108 TO TAB-POS(29).
           MOVE 092020 TO TAB-POS(30).
       TIT-PAN.
           MOVE "     LISTA" TO TIT-1-PAN.
           MOVE "DO DE PREC" TO TIT-2-PAN.
           MOVE "IOS       " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F5>  LIST.NUMERICO P/MARCA " LINE 14 POSITION 18.
           DISPLAY "<F6>  LIST.ALFABET. P/MARCA " LINE 15 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 16 POSITION 18.
       DES-MAR.
           MOVE 30 TO SW3.
           PERFORM BORRA.
           DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                           SIZE TAB-SIZ(SW3).           
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 10 BY 1
                             UNTIL SW1 > 19.
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
       F-LEA-EMPRE.
           CLOSE EMPRESAS.
       F-LEA-EMP.
           EXIT.     
       MUEVO-IMP.
           ADD 1 TO CONT-V.
           IF DET-NUM = "FR.CUB.BO.PRI. 0TITULO        "
                 MOVE "- CUBETA PRIMARIA BO.MAESTRA -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.BO.SEC. 0TITULO        "
                 MOVE "- CUBETA SECUND. BO. MAESTRA -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.EM.AUX. 0TITULO        "
                 MOVE "- CUBETA EMBRAGUE AUXILIAR - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.EM.PRI. 0TITULO        "
                 MOVE "- CUBETA PRIM. BO. EMBRAGUE -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.EM.SEC. 0TITULO        "
                 MOVE "- CUBETA SEC. BO. EMBRAGUE - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.PAST. 0TITULO              "
                 MOVE "- PASTILLAS DE FRENO -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO.DC. 0TITULO             "
                 MOVE "- BOMBAS DOBLE CIRCUITO -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO.EM.DC. 0TITULO          "
                 MOVE "- BOMBAS EMBRAGUE D.C.-      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CIN. 0TITULO               "
                 MOVE "- CINTAS DE FRENO -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CAMP. 0TITULO              "
                 MOVE "- CAMPANAS DE FRENO -        " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.DIS. 0TITULO               "
                 MOVE "- DISCOS DE FRENO -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.RU.DEL. 0TITULO        "
                 MOVE "- CUBETA RUEDA DELANTERA -   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CUB.RU.TRA. 0TITULO        "
                 MOVE "- CUBETA RUEDA TRASERA -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.DEP.LIQ. 0TITULO           "
                 MOVE "- DEPOSITO DE LIQUIDO -      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.FLEX EM. 0TITULO           "
                 MOVE "- FLEXIBLES DE EMBRAGUE      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.FLEX. 0TITULO              "
                 MOVE "- FLEXIBLES DE FRENO-        " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.GPOL. 0TITULO              "
                 MOVE "- GUARDAPOLVOS -             " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.JUNTA TAPA DEP. 0TITULO    "
                 MOVE "- JUNTAS TAPA DEPOSITOS -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.PIS.F.DIS. 0TITULO         "
                 MOVE "- PISTONES DE MORDAZA F.D. - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE BO.DC. 0TITULO          "
                 MOVE "- REPARACION BOMBA D.C. -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE BO.EM. 0TITULO          "
                 MOVE "- REPARACION BOMBA EMBRAGUE -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE.BO.AUX. 0TITULO         "
                 MOVE "- REPARACION EMBRAGUE AUXIL.-" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE.BO.AZZZ 0TITULO         "
                 MOVE "- REPARACION BOMBA MAESTRA - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE.CI. 0TITULO             "
                 MOVE "- REPARACION CILINDRO RUEDA -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE.FD. 0TITULO             "
                 MOVE "- REPARACION FRENO A DISCO - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RE.SER.TEN 0TITULO         "
                 MOVE "- REPARACION SERVO TENSA -   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RETEN SER. 0TITULO         "
                 MOVE "- DESPIECE SERVO BENDIX -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.SER.TEN 0TITULO            "
                 MOVE "- DESPIECE SERVO TENSA -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.VAL.BO. 0TITULO            "
                 MOVE "- VALVULAS BOMBA MAESTRA -   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.ASIENT.VAL. 0TITULO        "
                 MOVE "- ASIENTOS DE VALVULAS -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BULBO 0TITULO              "
                 MOVE "- BULBOS -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CONEX.PLAST.BO. 0TITULO    "
                 MOVE "- CONEXIONES PLASTICAS -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.TAPA DEP. 0TITULO          "
                 MOVE "- TAPAS DE DEPOSITOS -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO DC. 0TITULO             "
                 MOVE "- BOMBAS DOBLE CIRCUITO -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO EM. 0TITULO             "
                 MOVE "- BOMBAS DE EMBRAGUE -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO.AUX. 0TITULO            "
                 MOVE "- CILINDROS AUX. EMBRAGUE -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BO.B 0TITULO               "
                 MOVE "- BOMBAS MAESTRAS -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.ANC. 0TITULO               "
                 MOVE "- ANCLAJES -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.BUL.CONEC. 0TITULO         "
                 MOVE "- BULONES CONECTORES -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CONO 0TITULO               "
                 MOVE "- CONOS -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.HORQ. 0TITULO              "
                 MOVE "- HORQUILLAS -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.PERN.EMP. 0TITULO          "
                 MOVE "- PERNO DE EMPUJE -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.PURG. 0TITULO              "
                 MOVE "- PURGADORES -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RACC. 0TITULO              "
                 MOVE "- RACCORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.REDUC. 0TITULO             "
                 MOVE "- REDUCCIONES -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.REGIST. 0TITULO            "
                 MOVE "- REGISTROS -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.RES. 0TITULO               "
                 MOVE "- RESORTES -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.SEGU. 0TITULO              "
                 MOVE "- SEGUROS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.TUER.CANO 0TITULO          "
                 MOVE "- TUERCAS PARA CANOS -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.UNION 0TITULO              "
                 MOVE "- UNIONES -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM.PLAC. 0TITULO              "
                 MOVE "- PLACAS DE EMBRAGUE -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM PORTALAMINAS 0TITULO       "
                 MOVE "- PORTALAMINAS -             " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM MAZAS  0TITULO             "
                 MOVE "- MAZAS -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM TAPAS HASTA  0TITULO       "
                 MOVE "- TAPAS -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM CHAPAS HASTA  0TITULO      "
                 MOVE "- CHAPAS -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM.FUND. 0TITULO              "
                 MOVE "- FUNDICIONES DE EMBRAGUE -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM DIS.TEL. 0TITULO           "
                 MOVE "- DISCOS TELESCOP.-          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM.DIS. 0TITULO               "
                 MOVE "- DISCOS DE AUTOMOTORES -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EM DIS.RIG. 0TITULO           "
                 MOVE "- DISCOS DE TRACTORES -      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "FR.CI.RU. 0TITULO             "
                 MOVE "- CILINDROS DE RUEDAS -      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS  SEGMENTOS 0TITULO       "
                 MOVE "- SEGMENTOS -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS BMW 0TITULO              "
                 MOVE "- B.M.W.-                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS CATERP. 0TITULO          "
                 MOVE "- CATERPILLAR -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS CH- 0TITULO              "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS CIT- 0TITULO             "
                 MOVE "- CITROEN -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS COMP. 0TITULO            "
                 MOVE "- COMPRESORES -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS D- 0TITULO               "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS DI TELLA 0TITULO         "
                 MOVE "- SIAM-DI TELLA -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS F- 0TITULO               "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS FI- 0TITULO              "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS I- 0TITULO               "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS INDENOR 0TITULO          "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS J- 0TITULO               "
                 MOVE "- AUTOMOTORES JAPONESES      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS JEEP 0TITULO             "
                 MOVE "- JEEP WILLYS -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS LISTER 0TITULO           "
                 MOVE "- LISTER -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS PERKINS 0TITULO          "
                 MOVE "- PERKINS  -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS PEU- 0TITULO             "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS R- 0TITULO               "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS RAST- 0TITULO            "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS VILLA- 0TITULO           "
                 MOVE "- MOTORES VILLA -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS VOLVO 0TITULO            "
                 MOVE "- VOLVO -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS VW- 0TITULO              "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.BEDF- 0TITULO            "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.BMW- 0TITULO             "
                 MOVE "- B.M.W.-                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.CASE- 0TITULO            "
                 MOVE "- CASE -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.VOLVO 0TITULO            "
                 MOVE "- VOVLO -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.ZANELLO 0TITULO          "
                 MOVE "- ZANELLO -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BORGW- 0TITULO                "
                 MOVE "- BORGWARD -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.VILLA 0TITULO            "
                 MOVE "- MOTORES VILLA -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.CATERP. 0TITULO          "
                 MOVE "- CATERPILLAR -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.CH- 0TITULO              "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.CIT- 0TITULO             "
                 MOVE "- CITROEN -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.D- 0TITULO               "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.DEUTZ- 0TITULO           "
                 MOVE "- DEUTZ -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.DI TELLA- 0TITULO        "
                 MOVE "- SIAM-DI TELLA -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.F- 0TITULO               "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.FI- 0TITULO              "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.HANOMAG 0TITULO          "
                 MOVE "- HANOMAG -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.I- 0TITULO               "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.INDENOR 0TITULO          "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.J- 0TITULO               "
                 MOVE "- AUTOMOTORES JAPONESES      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.JOHN DEERE 0TITULO       "
                 MOVE "- JOHN DEERE -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.M.BEN- 0TITULO           "
                 MOVE "- MERCEDES-BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.PERKINS 0TITULO          "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.PEU- 0TITULO             " 
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.R- 0TITULO               "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.RAST- 0TITULO            "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.SCANIA 0TITULO           "
                 MOVE "- SCANIA -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.SOMECA 0TITULO           "
                 MOVE "- SOMECA -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.VW- 0TITULO              "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS.ZETOR 0TITULO            "
                 MOVE "- ZETOR -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS   0TITULO                "
                 MOVE "- AROS DE RECAMBIO -         " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "AROS. 0TITULO                 "
                 MOVE "- AROS DE RECTIFICACION -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT. 0TITULO                  "
                 MOVE "- EXTREMOS -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.FI- 0TITULO               "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.F- 0TITULO                "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.I- 0TITULO                "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.R- 0TITULO                "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.                      
           IF DET-NUM = "EXT.CH- 0TITULO               "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.RAST- 0TITULO             "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.VW- 0TITULO               "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.D- 0TITULO                "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.M.BEN- 0TITULO            "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.PEU- 0TITULO              "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT. 0TITULO                  "
                 MOVE "- EXTREMOS DE DIRECCION -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.DKW 0TITULO               "
                 MOVE "- D.K.W.-                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.SIAM 0TITULO              "
                 MOVE "- SIAM -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.BORGWARD 0TITULO          "
                 MOVE "- BORGWARD -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "EXT.J- 0TITULO                "
                 MOVE "- AUTOMOTORES JAPONESES -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.FI- 0TITULO               "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.F- 0TITULO                "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.I- 0TITULO                "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.R- 0TITULO                "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.CH- 0TITULO               "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.VW- 0TITULO               "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.D- 0TITULO                "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.PEU- 0TITULO              "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT.TOYO- 0TITULO             "
                 MOVE "- TOYOTA -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ROT. 0TITULO                  "
                 MOVE "- ROTULAS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.LIB. 0TITULO              "
                 MOVE "- BRAZO LIBRE O FALSO BRAZO  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.LIB.CH- 0TITULO           "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.LIB.D- 0TITULO            "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.LIB.F- 0TITULO            "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.LIB.I- 0TITULO            "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT. 0TITULO              "
                 MOVE "- BRAZO PITMAN -             " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT.CH- 0TITULO           "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT.D- 0TITULO            "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT.F- 0TITULO            "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT.I- 0TITULO            "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.PIT.TOYO- 0TITULO         "
                 MOVE "- TOYOTA -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.ROT. 0TITULO              "
                 MOVE "- BRAZO ROTULA -             " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.ROT.D- 0TITULO            "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.ROT.F- 0TITULO            "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.ROT.FI- 0TITULO           "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.ROT.VW- 0TITULO           "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.SUS. 0TITULO              "
                 MOVE "- BRAZOS DE SUSPENCION -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BRA.SUS.I- 0TITULO            "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "MANG.ACOP. 0TITULO            "
                 MOVE "- MANGUITOS DE ACOPLE -      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ACOP.CREM. 0TITULO            "
                 MOVE "- ACOPLE DE CREMALLERA -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "BARRA 0TITULO                 "
                 MOVE "- BARRAS -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "SALI.CREMALL. 0TITULO         "
                 MOVE "- SALIDA DE CREMALLERA -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "SALI.CAJA 0TITULO             "
                 MOVE "- SALIDA DE CAJA -           " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ART.ESF.SELLADA 0TITULO       "
                 MOVE "- ARTICULACION ESFERICA -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ART.CREMALL. 0TITULO          "
                 MOVE "- ARTICULACION CREMALLERA -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "ART.SALI.CAJA 0TITULO         "
                 MOVE "- ARTICULACION SALIDA CAJA - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "PARR.INF. 0TITULO             "
                 MOVE "- PARRILLA INFERIOR -        " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "PARR.IZQ. 0TITULO             "
                 MOVE "- PARRILLA IZQUIERDA -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "PARR.DER. 0TITULO             "
                 MOVE "- PARRILLA DERECHA -         " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "JUEGO REP.EXT. 0TITULO        "
                 MOVE "- REPARACION DE EXTREMOS -   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "JUEGO REP.ROT. 0TITULO        "
                 MOVE "- REPARACION DE ROTULAS -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "TENSOR 0TITULO                "
                 MOVE "- TENSORES -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "PERNO 0TITULO                 "
                 MOVE "- PERNOS -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "CHAVETAS 0TITULO              "
                 MOVE "- CHAVETAS -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP. 0TITULO           "
                 MOVE "- JUEGO COMPLETO DE JUNTAS - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.DI TELLA 0TITULO   "
                 MOVE "- DI TELLA -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.BORGW- 0TITULO     "
                 MOVE "- BORGWARD -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.COMP.DEUTZ- 0TITULO     "
                 MOVE "- DEUTZ -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL. 0TITULO           "
                 MOVE "- JUNTA TAPA DE VALVULAS -   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.DI TELLA 0TITULO   "
                 MOVE "- DI TELLA -                 " TO DET-NUM 
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.PERK- 0TITULO      " 
                 MOVE "- PERKINS -                  " TO DET-NUM 
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART. 0TITULO           "
                 MOVE "- JUEGO COMPLETO DE CARTER - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.DI TELLA 0TITULO   "
                 MOVE "- DI TELLA -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CART.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL 0TITULO            "
                 MOVE "- TAPA DE VALVULAS LATERALES " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.VAL RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM. 0TITULO           "
                 MOVE "- MULTIPLE DE ADMISION -     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.FI- 0TITULO        " 
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.BORGW- 0TITULO     "
                 MOVE "- BORGWARD -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ADM.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.ENTRE MULT. 0TITULO         "
                 MOVE "- JUNTA ENTRE MULTIPLES -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.DIST. 0TITULO               "
                 MOVE "- JUNTA DISTRIBUCION A BLOCK " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.DIST.PERK- 0TITULO          "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.DIST.M.F- 0TITULO           "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.BUJ. 0TITULO            "
                 MOVE "- JUEGO PARA BUJIAS -        " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.CAB. 0TITULO                "
                 MOVE "- JUNTA DE CABALLETE -       " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.PREC. 0TITULO               "
                 MOVE "- PRECAMARA DE COMBUSTION -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF. 0TITULO           "
                 MOVE "- JUNTA TAPA DE DIFERENCIAL -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.DI TELLA 0TITULO   "
                 MOVE "- DI TELLA -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.DIF.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA 0TITULO            "
                 MOVE "- JUEGO CAJA DE VELOCIDAD -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.JUE.CAJA I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.NUC.DIF. 0TITULO            "
                 MOVE "- NUCLEO DE DIFERENCIAL -    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.PAL.DIF. 0TITULO            "
                 MOVE "- JUNTA PALIER Y DIFERENCIAL " TO DET-NUM
                 MOVE 0 TO NUM-ART02.    
           IF DET-NUM = "J.TAPA CIL. 0TITULO           "
                 MOVE "- JUNTA TAPA DE CILINDROS -  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.CH- 0TITULO        "
                 MOVE "- CHERVROLET -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.DI TELLA 0TITULO   "
                 MOVE "- DI TELLA -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.JEEP- 0TITULO      "
                 MOVE "- JEEP WILLYS -              " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.HANOMAG- 0TITULO   "
                 MOVE "- HANOMAG -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA CIL.J.DEERE- 0TITULO   "
                 MOVE "-JOHN DEERE -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL. 0TITULO           "
                 MOVE "-TAPA CILINDRO MAT.ESPECIAL -" TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.CH- 0TITULO        "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.D- 0TITULO         "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.INDEN- 0TITULO     "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.BORGW- 0TITULO     "
                 MOVE "- BORGWARD -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.I- 0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.R- 0TITULO         "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.TAPA.CIL.HANOMAG- 0TITULO   "
                 MOVE "- HANOMAG -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E. 0TITULO         "
                 MOVE "- MULTIPLE ADMISION Y ESCAPE " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.CH- 0TITULO      "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.DI TELLA 0TITULO "
                 MOVE "- DI TELLA -                 " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.D- 0TITULO       "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.FI- 0TITULO      "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.F- 0TITULO       "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.INDEN- 0TITULO   "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.I- 0TITULO       "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.A Y E.R- 0TITULO       "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC. 0TITULO             "
                 MOVE "- SALIDA DE ESCAPE -         " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.CH- 0TITULO          "
                 MOVE "- CHEVROLET -                " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.BEDF- 0TITULO        "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.D- 0TITULO           "
                 MOVE "- DODGE -                    " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.VW- 0TITULO          "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.FI- 0TITULO          "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.F- 0TITULO           "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.INDEN- 0TITULO       "
                 MOVE "- INDENOR -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.M.BEN- 0TITULO       "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.PERK- 0TITULO        "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.M.F- 0TITULO         "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.PEU- 0TITULO         "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.RAST- 0TITULO        "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.I- 0TITULO           "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.SA.ESC.R- 0TITULO           "
                 MOVE "- RENAULT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC. 0TITULO           "
                 MOVE "- JUNTA DE MULTIPLE ESCAPE - " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.BEDF- 0TITULO      "
                 MOVE "- BEDFORD -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.VW- 0TITULO        "
                 MOVE "- VOLKSWAGEN -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.FI- 0TITULO        "
                 MOVE "- FIAT -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.F- 0TITULO         "
                 MOVE "- FORD -                     " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.M.BEN- 0TITULO     "
                 MOVE "- MERCEDES BENZ -            " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.PERK- 0TITULO      "
                 MOVE "- PERKINS -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.M.F- 0TITULO       "
                 MOVE "- MASSEY FERGUSON -          " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.PEU- 0TITULO       "
                 MOVE "- PEUGEOT -                  " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.RAST- 0TITULO      "
                 MOVE "- RASTROJERO -               " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.I -0TITULO         "
                 MOVE "- IKA -                      " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
           IF DET-NUM = "J.MULT.ESC.R- 0TITULO         "
                 MOVE "-RENAULT -                   " TO DET-NUM
                 MOVE 0 TO NUM-ART02.
                 
           MOVE NUM-ART02 TO NUM(CONT-V).
           MOVE DET-NUM TO DET(CONT-V).
           MOVE SEC-1-ART02 TO SEC1(CONT-V).
           MOVE SEC-2-ART02 TO SEC2(CONT-V).
           MOVE SEC-3-ART02 TO SEC3(CONT-V).
           IF PRE-V-ART02 NOT = 0
              MOVE "$" TO MON(CONT-V)
              MOVE PRE-V-ART02 TO PRE(CONT-V).
      **************OSV*********************
      *     IF PRE-E-ART02 NOT = 0
      *        MOVE "U$S" TO MON(CONT-V)
      *        MOVE PRE-E-ART02 TO PRE(CONT-V).
           IF PRE(CONT-V) = 0
              MOVE "S/PRE" TO MON(CONT-V)
              MOVE EMP-ART02 TO EMP-ART01
              MOVE NUM-ART02 TO NUM-ART01
              MOVE MAR-ART02 TO MAR-ART01
              READ ART01 INVALID KEY MOVE 0 TO PRE(CONT-V)
              MOVE COSTO-STOCK-ART01 TO PRE(CONT-V).
       MUEVO-IMP-F.
           EXIT.
       IMPRE.
           IF CONT-R = 0 PERFORM TITU.
           MOVE 0 TO CONT-I1.
           MOVE 60 TO CONT-I2.
           PERFORM IMPRE1 THRU IMPRE1-F 60 TIMES.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.
           MOVE 0 TO CONT-R.
           PERFORM LIMPIA-VEC.
       IMPRE1.
           ADD 1 TO CONT-I1.
           ADD 1 TO CONT-I2.
           IF NUM(CONT-I1) = 0 GO TO IMPRE-S-4.
           IF NUM(CONT-I2) = 0 GO TO IMPRE-S-5.
           IF NUM(CONT-I1) = 0 AND NUM(CONT-I2) = 0 GO TO IMPRE-S-6.
           MOVE NUM(CONT-I1) TO NUM1-ED.
           MOVE DET(CONT-I1) TO DET1-ED.
           MOVE MON(CONT-I1) TO MON1-ED.
           MOVE PRE(CONT-I1) TO PRE1-V-ED.
           MOVE SEC1(CONT-I1) TO SEC1-1-ED.
           MOVE SEC2(CONT-I1) TO SEC1-2-ED.
           MOVE SEC3(CONT-I1) TO SEC1-3-ED.
           MOVE NUM(CONT-I2) TO NUM2-ED.
           MOVE DET(CONT-I2) TO DET2-ED.
           MOVE MON(CONT-I2) TO MON2-ED.
           MOVE PRE(CONT-I2) TO PRE2-V-ED.
           MOVE SEC1(CONT-I2) TO SEC2-1-ED.
           MOVE SEC2(CONT-I2) TO SEC2-2-ED.
           MOVE SEC3(CONT-I2) TO SEC2-3-ED.
           WRITE LIN-IMP02 FROM IMP-3 AFTER 1.
           ADD 1 TO CONT-R.
       IMPRE1-F.
           EXIT.
       IMPRE-S-4.
           MOVE DET(CONT-I1) TO SUB3-ED.
           MOVE NUM(CONT-I2) TO NUM3-ED.
           MOVE DET(CONT-I2) TO DET3-ED.
           MOVE MON(CONT-I2) TO MON3-ED.
           MOVE PRE(CONT-I2) TO PRE3-V-ED.
           MOVE SEC1(CONT-I2) TO SEC3-1-ED.
           MOVE SEC2(CONT-I2) TO SEC3-2-ED.
           MOVE SEC3(CONT-I2) TO SEC3-3-ED.
           WRITE LIN-IMP02 FROM IMP-4 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO IMPRE1-F.
        IMPRE-S-5.
           MOVE DET(CONT-I2) TO SUB4-ED.
           MOVE NUM(CONT-I1) TO NUM4-ED.
           MOVE DET(CONT-I1) TO DET4-ED.
           MOVE MON(CONT-I1) TO MON4-ED.
           MOVE PRE(CONT-I1) TO PRE4-V-ED.
           MOVE SEC1(CONT-I1) TO SEC4-1-ED.
           MOVE SEC2(CONT-I1) TO SEC4-2-ED.
           MOVE SEC3(CONT-I1) TO SEC4-3-ED.
           WRITE LIN-IMP02 FROM IMP-5 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO IMPRE1-F.
       IMPRE-S-6.
           MOVE DET(CONT-I1) TO SUB50-ED.
           MOVE DET(CONT-I2) TO SUB51-ED.
           WRITE LIN-IMP02 FROM IMP-6 AFTER 1.
           ADD 1 TO CONT-R.
           GO TO IMPRE1-F.    
       LIMPIA-VEC.
           MOVE 0 TO CONT-V.
           PERFORM LIMPIA-VEC1 120 TIMES.
           MOVE 0 TO CONT-V CONT-I1 CONT-I2.
       LIMPIA-VEC1.
           ADD 1 TO CONT-V.
           MOVE 0 TO NUM(CONT-V) PRE(CONT-V).
           MOVE SPACES TO SEC1(CONT-V) SEC2(CONT-V) SEC3(CONT-V).
           MOVE SPACES TO DET(CONT-V) MON(CONT-V).
               
       CIERRE-A.
           CLOSE ART02 NUMERO IMP02 ART01.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "ROT132.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".

       FFF.
           EXIT PROGRAM.
       FFFF.
           STOP RUN.
