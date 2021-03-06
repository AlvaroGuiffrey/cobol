       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVER.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. DIC'99.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CHEQUES.SEL".
       COPY "CHEQUES9.SEL".
       COPY "ALICUO.SEL".
       COPY "ALICUO9.SEL".
       COPY "MOVI.SEL".
       COPY "MOVI9.SEL".
       COPY "PENDI.SEL".
       COPY "PENDI9.SEL".
       COPY "COTIZA.SEL".
       COPY "COTIZA9.SEL".
       COPY "PANTA.SEL".
       COPY "IMP01.SEL".
       DATA DIVISION.
       FILE SECTION.
       COPY "ALICUO.REG".
       COPY "ALICUO9.REG".
       COPY "CHEQUES.REG".
       COPY "CHEQUES9.REG".
       COPY "MOVI.REG".
       COPY "MOVI9.REG".
       COPY "PENDI.REG".
       COPY "PENDI9.REG".
       COPY "COTIZA.REG".
       COPY "COTIZA9.REG".
       COPY "PANTA.REG".
       COPY "IMP01.REG".
       WORKING-STORAGE SECTION.
       COPY "ROT80.IMP".
       01 IMPRESION.
          02 NORMAL.
             03 AA          PIC 99 COMP-4 VALUE 18.
          02 IMP-1.
             03 FILLER       PIC X(18) VALUE SPACES.
             03 FILLER       PIC X(3) VALUE "NRO".
             03 FILLER       PIC X(4) VALUE SPACES.
             03 FILLER       PIC X(12) VALUE "--FECHA---".
             03 FILLER       PIC X(9) VALUE "IVA %".
             03 FILLER       PIC X(9) VALUE "SOB.%".
             03 FILLER       PIC X(5) VALUE "PER.%".
          02 IMP-2.
             03 FILLER       PIC X(18) VALUE SPACES.
             03 FILLER       PIC X(5) VALUE "Son: ".
             03 CONT-L-ED    PIC ZZZZZZ.
             03 FILLER       PIC X(10) VALUE " TASAS IVA".
          02 IMP-3.
             03 FILLER      PIC X(18) VALUE SPACES.
             03 ALI-ED      PIC ZZZ.
             03 FILLER      PIC X(4) VALUE SPACES.
             03 DD-ED       PIC 99/.
             03 MM-ED       PIC 99/.
             03 AA-ED       PIC 9999BB.
             03 IVA-1-ED    PIC Z9,99B(4).
             03 IVA-2-ED    PIC Z9,99B(4).
             03 IVA-3-ED    PIC Z9,99.
       01 LISTO-D            PIC 999.
       01 LISTO-H            PIC 999.
       01 LISTO-D-A          PIC X(10).
       01 LISTO-H-A          PIC X(10).
       01 IND-ORD            PIC 9.
       01 ALI-CAP            PIC 9(3) VALUE 0.
       01 FEC-CAP            PIC 9(8) VALUE 0.
       01 FECHA-CAP.
          02 DD-CAP          PIC 99 VALUE 0.
          02 MM-CAP          PIC 99 VALUE 0.
          02 ANO-CAP.
             03 AAAA-CAP     PIC 99 VALUE 0.  
             03 AA-CAP       PIC 99 VALUE 0.
       01 IVA-1-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-2-CAP          PIC 9(2)V99 VALUE 0.
       01 IVA-3-CAP          PIC 9(2)V99 VALUE 0.
       01 PESO-CAP           PIC 9(4)V999.
       01 ALI-EDI            PIC ZZ9.
       01 FECHA-EDI.
          02 DD-EDI          PIC 99/.
          02 MM-EDI          PIC 99/.
          02 AA-EDI          PIC 9999.
       01 IVA-1-EDI          PIC Z9,99.
       01 IVA-2-EDI          PIC Z9,99.
       01 IVA-3-EDI          PIC Z9,99.
       01 QQ                 PIC X.
       01 CONT-CONVER        PIC 999999.
       01 DATOS.
          02 DATO OCCURS 10 TIMES.
             03 COD-DAT     PIC 9(4).
             03 DET-DAT     PIC X(20).
       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "CHEQUES.ERR".
       COPY "CHEQUES9.ERR".
       COPY "MOVI.ERR".
       COPY "MOVI9.ERR".
       COPY "PENDI.ERR".
       COPY "PENDI9.ERR".
       COPY "ALICUO.ERR".
       COPY "ALICUO9.ERR".
       COPY "COTIZA.ERR".
       COPY "COTIZA9.ERR".
       COPY "IMP01.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA WITH NO LOCK.
           CLOSE PANTA.
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
           MOVE 28 TO SW3.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM 
                   STOP RUN.
           IF F01 GO TO ACTU.
           GO TO CARGO-MOD.
       ACTU.
           MOVE " CONVERSION " TO D20-CAP.
           PERFORM MODA-PAN
           PERFORM BORRA-PANTA.
       CONVER-ALI.
           OPEN INPUT ALICUO9.
           OPEN OUTPUT ALICUO.
           CLOSE ALICUO.
           OPEN I-O ALICUO.
           DISPLAY "CONVIERTO :" LINE 15 POSITION 5.
           DISPLAY "ALICUO" LINE 15 POSITION 20 BLINK REVERSE.
           DISPLAY "REGISTRO  :" LINE 17 POSITION 5.
           MOVE 0 TO EMP-ALI9 SUC-ALI9 ALI-ALI9 FEC-ALI9 CONT-CONVER.
           START ALICUO9 KEY IS NOT < CLAVE-ALICUO9
                      INVALID KEY GO TO CONVER-ALI-F.
       CONVER-ALI-1.
           READ ALICUO9 NEXT WITH NO LOCK AT END GO TO CONVER-ALI-F.
           MOVE EMP-ALI9 TO EMP-ALI.
           MOVE SUC-ALI9 TO SUC-ALI.
           MOVE ALI-ALI9 TO ALI-ALI.
           MOVE AA-ALI9 TO AA-ALI.
           ADD 1900 TO AA-ALI.
           MOVE MM-ALI9 TO MM-ALI.
           MOVE DD-ALI9 TO DD-ALI.
           MOVE IVA-1-ALI9 TO IVA-1-ALI.
           MOVE IVA-2-ALI9 TO IVA-2-ALI.
           MOVE IVA-3-ALI9 TO IVA-3-ALI.
           WRITE REG-ALICUO INVALID KEY REWRITE REG-ALICUO.
           ADD 1 TO CONT-CONVER.
           DISPLAY CONT-CONVER LINE 17 POSITION 20.
           GO TO CONVER-ALI-1.
       CONVER-ALI-F.
           CLOSE ALICUO ALICUO9.
           ACCEPT QQ LINE 18 POSITION 20.
       CONVER-COTI.
           OPEN INPUT COTIZA9.
           OPEN OUTPUT COTIZA.
           CLOSE COTIZA.
           OPEN I-O COTIZA.
           DISPLAY "CONVIERTO :" LINE 15 POSITION 5.
           DISPLAY "COTIZA" LINE 15 POSITION 20 BLINK REVERSE.
           DISPLAY "REGISTRO  :" LINE 17 POSITION 5.
           MOVE 0 TO EMP-COTIZA9 MON-COTIZA9 FEC-COTIZA9 CONT-CONVER.
           START COTIZA9 KEY IS NOT < CLAVE-COTIZA9
                      INVALID KEY GO TO CONVER-COTI-F.
       CONVER-COTI-1.
           READ COTIZA9 NEXT WITH NO LOCK AT END GO TO CONVER-COTI-F.
           MOVE EMP-COTIZA9 TO EMP-COTIZA.
           MOVE MON-COTIZA9 TO MON-COTIZA.
           MOVE AA-COTIZA9 TO AA-COTIZA.
           ADD 1900 TO AA-COTIZA.
           MOVE MM-COTIZA9 TO MM-COTIZA.
           MOVE DD-COTIZA9 TO DD-COTIZA.
           MOVE VAL-COTIZA9 TO VAL-COTIZA.
           WRITE REG-COTIZA INVALID KEY REWRITE REG-COTIZA.
           ADD 1 TO CONT-CONVER.
           DISPLAY CONT-CONVER LINE 17 POSITION 20.
           GO TO CONVER-COTI-1.
       CONVER-COTI-F.
           CLOSE COTIZA9 COTIZA.
           ACCEPT QQ LINE 18 POSITION 20.
       CONVER-MOVI.
           OPEN INPUT MOVI9.
           OPEN OUTPUT MOVI.
           CLOSE MOVI.
           OPEN I-O MOVI.
           DISPLAY "CONVIERTO :" LINE 15 POSITION 5.
           DISPLAY "MOVI    " LINE 15 POSITION 20 BLINK REVERSE.
           DISPLAY "REGISTRO  :" LINE 17 POSITION 5.
           MOVE 0 TO EMP-MOV9 SUC-EMP-MOV9 RUBRO-MOV9 CUENTA-MOV9
              SUB-CUENTA-MOV9 ANO-MOV9 MES-MOV9 DIA-MOV9 
              MOVIMIENTO-MOV9 CONT-CONVER.
           START MOVI9 KEY IS NOT < CLAVE-MOV9
                      INVALID KEY GO TO CONVER-MOVI-F
                      DISPLAY "SALI POR INVALID".
           DISPLAY "PASE POSICIONO".
       CONVER-MOVI-1.
           READ MOVI9 NEXT WITH NO LOCK AT END GO TO CONVER-MOVI-F.
           MOVE EMP-MOV9 TO EMP-MOV.
           MOVE SUC-EMP-MOV9 TO SUC-EMP-MOV.
           MOVE RUBRO-MOV9 TO RUBRO-MOV.
           MOVE CUENTA-MOV9 TO CUENTA-MOV.
           MOVE SUB-CUENTA-MOV9 TO SUB-CUENTA-MOV.
           MOVE ANO-MOV9 TO ANO-MOV.
           ADD 1900 TO ANO-MOV.
           MOVE MES-MOV9 TO MES-MOV.
           MOVE DIA-MOV9 TO DIA-MOV.
           MOVE MOVIMIENTO-MOV9 TO MOVIMIENTO-MOV.
           MOVE TIPO-MOV9 TO TIPO-MOV.
           MOVE TIPO-C-MOV9 TO TIPO-C-MOV.
           MOVE SUB-T-C-MOV9 TO SUB-T-C-MOV.
           MOVE SUC-C-MOV9 TO SUC-C-MOV.
           MOVE COMPRO-MOV9 TO COMPRO-MOV.
           MOVE REMA-MOV9 TO REMA-MOV.
           MOVE IMPORTE-MOV9 TO IMPORTE-MOV.
           MOVE IMP-EXT-MOV9 TO IMP-EXT-MOV.
           MOVE COMENTARIO-MOV9 TO COMENTARIO-MOV.
           MOVE SUCURSAL-MOV9 TO SUCURSAL-MOV.
           MOVE ASIENTO-MOV9 TO ASIENTO-MOV.
           WRITE REG-MOV INVALID KEY REWRITE REG-MOV.
           ADD 1 TO CONT-CONVER.
           DISPLAY CONT-CONVER LINE 17 POSITION 20.
           GO TO CONVER-MOVI-1.
       CONVER-MOVI-F.
           CLOSE MOVI MOVI9.
           ACCEPT QQ LINE 18 POSITION 20.
       CONVER-PENDI.
           OPEN INPUT PENDI9.
           OPEN OUTPUT PENDI.
           CLOSE PENDI.
           OPEN I-O PENDI.
           DISPLAY "CONVIERTO :" LINE 15 POSITION 5.
           DISPLAY "PENDI   " LINE 15 POSITION 20 BLINK REVERSE.
           DISPLAY "REGISTRO  :" LINE 17 POSITION 5.
           MOVE 0 TO EMP-PEND9 SUC-EMP-PEND9 RUBRO-PEND9 
             CUENTA-PEND9 SUB-CUENTA-PEND9 VENCIMIENTO-PEND9
             PENDIENTE-PEND9 CONT-CONVER.
           START PENDI9 KEY IS NOT < CLAVE-PEND9
                      INVALID KEY GO TO CONVER-PENDI-F.
       CONVER-PENDI-1.
           READ PENDI9 NEXT WITH NO LOCK AT END GO TO CONVER-PENDI-F.
           MOVE EMP-PEND9 TO EMP-PEND.
           MOVE SUC-EMP-PEND9 TO SUC-EMP-PEND.
           MOVE RUBRO-PEND9 TO RUBRO-PEND.
           MOVE CUENTA-PEND9 TO CUENTA-PEND.
           MOVE SUB-CUENTA-PEND9 TO SUB-CUENTA-PEND.
           MOVE ANO-PEND9 TO ANO-PEND.
           ADD 1900 TO ANO-PEND.
           MOVE MES-PEND9 TO MES-PEND.
           MOVE DIA-PEND9 TO DIA-PEND.
           MOVE PENDIENTE-PEND9 TO PENDIENTE-PEND.
           MOVE TIPO-PEND9 TO TIPO-PEND.
           MOVE SUB-T-PEND9 TO SUB-T-PEND.
           MOVE SUC-T-PEND9 TO SUC-T-PEND.
           MOVE COMPRO-PEND9 TO COMPRO-PEND.
           MOVE REMATE-PEND9 TO REMATE-PEND.
           MOVE D-H-PEND9 TO D-H-PEND.
           MOVE IMPORTE-PEND9 TO IMPORTE-PEND.
           MOVE ACUENTA-PEND9 TO ACUENTA-PEND.
           MOVE DETA-PEND9 TO DETA-PEND.
           MOVE DIA-EMI-PEND9 TO DIA-EMI-PEND.
           MOVE MES-EMI-PEND9 TO MES-EMI-PEND.
           MOVE ANO-EMI-PEND9 TO ANO-EMI-PEND.
           ADD 1900 TO ANO-EMI-PEND.
           WRITE REG-PEND INVALID KEY REWRITE REG-PEND.
           ADD 1 TO CONT-CONVER.
           DISPLAY CONT-CONVER LINE 17 POSITION 20.
           GO TO CONVER-PENDI-1.
       CONVER-PENDI-F.
           CLOSE PENDI PENDI9.
           ACCEPT QQ LINE 18 POSITION 20.
       CONVER-CHE.
           OPEN INPUT CHEQUES9.
           OPEN OUTPUT CHEQUES.
           CLOSE CHEQUES.
           OPEN I-O CHEQUES.
           DISPLAY "CONVIERTO :" LINE 15 POSITION 5.
           DISPLAY "CHEQUES " LINE 15 POSITION 20 BLINK REVERSE.
           DISPLAY "REGISTRO  :" LINE 17 POSITION 5.
           MOVE 0 TO CH-EMP9 CH-SUC9 CH-BCO9 CH-CPOSTAL9
              CH-NUMERO-CH9 CONT-CONVER.
           START CHEQUES9 KEY IS NOT < CLAVE-CHEQUES9
                      INVALID KEY GO TO CONVER-CHE-F.
       CONVER-CHE-1.
           READ CHEQUES9 NEXT WITH NO LOCK AT END GO TO CONVER-CHE-F.
           MOVE CLAVE-CHEQUES9 TO CLAVE-CHEQUES.
           MOVE CH-CLAVE-BCO9 TO CH-CLAVE-BCO.
           MOVE CH-CLAVE-FACTURA9 TO CH-CLAVE-FACTURA.
           MOVE CH-ANO-VENC9 TO CH-ANO-VENC.
           IF CH-ANO-VENC < 50
               ADD 2000 TO CH-ANO-VENC
                 ELSE ADD 1900 TO CH-ANO-VENC.
           DISPLAY CH-ANO-VENC9 LINE 18 POSITION 10
           DISPLAY CH-ANO-VENC LINE 18 POSITION 30
   
           MOVE CH-MES-VENC9 TO CH-MES-VENC.
           MOVE CH-DIA-VENC9 TO CH-DIA-VENC.
           MOVE CH-NUM-CHEQUE9 TO CH-NUM-CHEQUE.
           MOVE CH-ANO-REC9 TO CH-ANO-REC.
           IF CH-ANO-REC < 50 
              ADD 2000 TO CH-ANO-REC
                 ELSE ADD 1900 TO CH-ANO-REC.
           MOVE CH-MES-REC9 TO CH-MES-REC.
           MOVE CH-DIA-REC9 TO CH-DIA-REC.
           MOVE CH-NRO-CTA9 TO CH-NRO-CTA.
           MOVE CH-IMP-FACTURA9 TO CH-IMP-FACTURA.
           MOVE CH-ENTREGADO-A9 TO CH-ENTREGADO-A.
           MOVE CH-ANO-ENT9 TO CH-ANO-ENT.
           IF CH-ANO-ENT < 50
             ADD 2000 TO CH-ANO-ENT
             ELSE ADD 1900 TO CH-ANO-ENT.
           IF CH-ANO-ENT9 = 0 AND CH-MES-ENT9 = 0
               MOVE 0 TO CH-ANO-ENT.
           MOVE CH-MES-ENT9 TO CH-MES-ENT.
           MOVE CH-DIA-ENT9 TO CH-DIA-ENT.
           MOVE CH-CONCEPTO-ENT9 TO CH-CONCEPTO-ENT.
           MOVE CH-TIPO-DOCUM9 TO CH-TIPO-DOCUM.
           MOVE CH-IMP-INTERES9 TO CH-IMP-INTERES.
           MOVE CH-TASA-INTERES9 TO CH-TASA-INTERES.
           MOVE CH-MONEDA9 TO CH-MONEDA.
           MOVE CH-CON-GARANTIA9 TO CH-CON-GARANTIA.
           MOVE CH-COD-VENC9 TO CH-COD-VENC.
           MOVE CH-COMENTA9 TO CH-COMENTA.
           MOVE CH-RUBRO9 TO CH-RUBRO.
           WRITE REG-CHEQUES INVALID KEY REWRITE REG-CHEQUES.
           ADD 1 TO CONT-CONVER.
           DISPLAY CONT-CONVER LINE 17 POSITION 20.
           GO TO CONVER-CHE-1.
       CONVER-CHE-F.
           CLOSE CHEQUES CHEQUES9.
           ACCEPT QQ LINE 18 POSITION 20.
           STOP RUN.
       CONF-CAMPOS.
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 091020 TO TAB-POS(3).
           MOVE 094720 TO TAB-POS(4).
           MOVE 102503 TO TAB-POS(5).
           MOVE 112503 TO TAB-POS(6).
           MOVE 122503 TO TAB-POS(7).
           MOVE 132503 TO TAB-POS(8).
           MOVE 142503 TO TAB-POS(9).
           MOVE 152503 TO TAB-POS(10).
           MOVE 162503 TO TAB-POS(11).
           MOVE 172503 TO TAB-POS(12)
           MOVE 182503 TO TAB-POS(13).
           MOVE 192503 TO TAB-POS(14).
           MOVE 255801 TO TAB-POS(15)
           MOVE 101503 TO TAB-POS(16).
           MOVE 121510 TO TAB-POS(17).
           MOVE 141505 TO TAB-POS(18).
           MOVE 161505 TO TAB-POS(19).
           MOVE 181505 TO TAB-POS(20).
           MOVE 201509 TO TAB-POS(21).
           MOVE 143320 TO TAB-POS(22).
           MOVE 153320 TO TAB-POS(23).
           MOVE 163320 TO TAB-POS(24).
           MOVE 173320 TO TAB-POS(25).
           MOVE 183320 TO TAB-POS(26).
           MOVE 193320 TO TAB-POS(27).
           MOVE 254101 TO TAB-POS(28).
           MOVE 071504 TO TAB-POS(29).
           MOVE 073002 TO TAB-POS(30).
           MOVE 073830 TO TAB-POS(31).
       TIT-PAN.
           MOVE "     -ARCH" TO TIT-1-PAN.
           MOVE "IVO VARIOS" TO TIT-2-PAN.
           MOVE " -        " TO TIT-3-PAN.
       OPC-MODA.
           DISPLAY "<F1>  CONVERSION 2000." LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU   " LINE 17 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-PANTA-C.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           MOVE 19 TO SW3.
           PERFORM BORRA.
           MOVE 20 TO SW3.
           PERFORM BORRA.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "CLAVES.PRO".
