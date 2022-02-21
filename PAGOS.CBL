       IDENTIFICATION DIVISION.
       PROGRAM-ID.       RECIBOS.
       AUTHOR.           H-A-S.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        PC.
       OBJECT-COMPUTER.        PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              COPY "P-CTAS.SEL".
              COPY "EMPRE.SEL".
              COPY "CUENTA.SEL".
              COPY "MOVI.SEL".
              COPY "CPOSTAL.SEL".
              COPY "COMPRO.SEL".
              COPY "IMP-132.SEL".
              COPY "LACAJA.SEL".
       
       DATA DIVISION.
       FILE SECTION.
              COPY "P-CTAS.REG".
              COPY "EMPREBM.REG".
              COPY "CUENTABM.REG".
              COPY "MOVI.REG".              
              COPY "CPOSTAL.REG".
              COPY "COMPRO1.REG".
              COPY "IMP-132.REG".
              COPY "LACAJA.REG".
       
       WORKING-STORAGE SECTION.
                  COPY "UTILES.WOR".
                  COPY "PESOS.WOR".
       01  DATOS-PARAMETROS.
           03  EMP                     PIC 9999 VALUE 0.
           03  SUC                     PIC 99 VALUE 0.
           03  RUBRO                   PIC 9(6) VALUE 0.
           03  CTA                     PIC 9(6) VALUE 0.
           03  S-CTA                   PIC 9999 VALUE 0.
           03  COM                     PIC 9 VALUE 0.
       01  DATOS-PARAMETROS-ED.
           03  EMP-ED                  PIC ZZZZ.
           03  SUC-ED                  PIC ZZ.
           03  RUBRO-ED                PIC ZZZZZZ.
           03  CTA-ED                  PIC ZZZZZZ.
           03  S-CTA-ED                PIC ZZZZ.
       
       01  RR                          PIC X(62) VALUE ALL "*".
       01  TIPO-COMP                   PIC X VALUE SPACES.
       01  ITEM                        PIC 99 VALUE 0.
       01  NOM-CTA                     PIC X(30).
       01  CUIT-CTA                    PIC X(13).
       01  ULTIMO                      PIC 9(6) VALUE 0.
       01  ULTIMO1                     PIC 9(6) VALUE 0.       
       01  IVAX                        PIC 9(8)V99.
       01  GRA                         PIC 9(8)V99.
       01  NUMERO                      PIC 9(6).
       01  ESTE-NETO                   PIC S9(10)V99 VALUE 0.
       01  TOTAL                       PIC S9(10)V99 VALUE 0.       
       01  ESTE-IMPORTE                PIC S9(10)V99 VALUE 0.       
       01  MULTA                       PIC S9(10)V99 VALUE 0.       
       01  MORA                        PIC S9(10)V99 VALUE 0.       
       01  VALOR                       PIC S9(10)V99 VALUE 0.       
       01  RENGLON                     PIC 999 VALUE 0 .
       01  RAYITA                      PIC X(33) VALUE ALL "Ä".
       01  NOMBRE                      PIC X(30).
       
       01  RENGLONES.
           03  RENG-PANT.
               05  R-ED             PIC B99BB.
               05  DIA-ED           PIC B99/.
               05  MES-ED           PIC 99/.
               05  ANO-ED           PIC 99.                              
               05  FILLER           PIC X(5) VALUE SPACES.
               05  COMPRO-ED        PIC ZZZZZZZZZZ.
               05  FILLER           PIC X(5) VALUE SPACES.
               
               05  DETA-ED          PIC X(23)B.
               05  PESOS-ED         PIC ZZZ,ZZ9.99B(3).
           
           03  RENG2.       
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(16) VALUE 
                   "Recibimos de .:".
               05  LACTA-ED         PIC X(35).
               05  FILLER           PIC X(7) VALUE SPACES.
               05  FILLER           PIC 99 COMP-4 VALUE 14.
               05  FILLER           PIC 99 COMP-4 VALUE 16.
               05  DIA-REC-ED       PIC 99/.
               05  MES-REC-ED       PIC 99/.
               05  ANO-REC-ED       PIC 99.
               05  FILLER           PIC 99 COMP-4 VALUE 18.
               05  FILLER           PIC 99 COMP-4 VALUE 20.
           03  RENG3.
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(16) VALUE 
                   "Direccion ....:".
               05  LADIRE-ED        PIC X(30).
           03  RENG4.
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(15) VALUE 
                   "Localidad ....:".
               05  FILLER           PIC XX VALUE "( ".
               05  NRO-LOCA-ED      PIC ZZZZ.
               05  FILLER           PIC XX VALUE " )".
               05  LALOCA-ED        PIC X(30).
           03  RENG5.
               05  FILLER           PIC X(20) VALUE SPACES.
               05  FILLER           PIC X(22) VALUE 
                   "LA SUMA DE PESOS...:".
               05  FILLER           PIC 99 COMP-4 VALUE 14.
               05  FILLER           PIC 99 COMP-4 VALUE 16.

               05  IMPORTE-ED       PIC ****,**9.99.
               05  FILLER           PIC 99 COMP-4 VALUE 18.
               05  FILLER           PIC 99 COMP-4 VALUE 20.
           
           03  RENG6.       
               05  FILLER           PIC X(5) VALUE SPACES.
               05  FILLER           PIC X(05) VALUE 
                   "SON ".
               05  LACANT-ED        PIC X(60).       
           03  RENG7.       
               05  FILLER           PIC X(10) VALUE SPACES.
               05  LACANT1-ED       PIC X(60).
           03  RENG7-1.
               05  FILLER           PIC X(3) VALUE SPACES.
               05  FILLER           PIC X(60) VALUE 
               "Pago factura(s) segun detalle".

           03  RENG8.
               05  FILLER           PIC X(10) VALUE SPACES.
               05  FILLER           PIC X(15) VALUE 
                   "Nro. Factura".
               05  FILLER           PIC X(15) VALUE 
                   "F e c h a".
               05  FILLER           PIC X(10) VALUE 
                   " Importe".
           03  RENG9.
               05  FILLER           PIC X(12) VALUE SPACES.
               05  COMP-ED          PIC ZZZZZZZZB(6).
               05  FECHA-FAC-ED.
                   07  DIA-F-ED     PIC 99/.
                   07  MES-F-ED     PIC 99/.
                   07  ANO-F-ED     PIC 99BBBBB.
               05  IMP-ED           PIC ZZZ,ZZ9.99.
           03  RENG10.
               05  FILLER           PIC X(45) VALUE SPACES.
               05  FILLER           PIC X(30) VALUE ALL ".".
           03  RENG11.
               05  FILLER           PIC X(55) VALUE SPACES.
               05  FILLER           PIC X(10) VALUE "FIRMA".
               05  FILLER           PIC 99 COMP-4 VALUE 14.



       
       01  CAMPO-EDITA.
           03  WCLAVE-IVA-ED.
               05  WIVA-EMP-ED         PIC ZZZZ.
               05  WIVA-SUC-ED         PIC ZZ.
               05  WIVA-FECHA-ED.
                   07  WIVA-DIA-ED     PIC 99/.
                   07  WIVA-MES-ED     PIC 99/.
                   07  WIVA-ANO-ED     PIC 99.
               05  WIVA-NUMERO-ED      PIC ZZZZZZ.
               05  WIVA-FECHA-C-ED.
                   07  WIVA-DIA-C-ED   PIC 99/.
                   07  WIVA-MES-C-ED   PIC 99/.
                   07  WIVA-ANO-C-ED   PIC 99.
           03  WIVA-RUBRO-ED           PIC ZZZZZZ.
           03  WIVA-CUENTA-ED          PIC ZZZZZZ.
           03  WIVA-S-CUENTA-ED          PIC ZZ.
           03  WIVA-SERIE-ED           PIC ZZZZ.
           03  WIVA-NUMERO-C-ED        PIC ZZZZZZZZ.
           03  WIVA-M-TOTAL-ED         PIC ZZZ,ZZ9.99.
           03  WIVA-M-GRAVADO-ED       PIC ZZZ,ZZ9.99.
           03  WIVA-M-EXENTO-ED        PIC ZZZ,ZZ9.99.
           03  WIVA-IMP-INTERNO-ED     PIC ZZZ,ZZ9.99.
           03  WIVA-RET-DGI-ED         PIC ZZZ,ZZ9.99.
           03  WIVA-RET-DGR-ED         PIC ZZZ,ZZ9.99.
           03  WIVA-18-ED              PIC ZZZ,ZZ9.99.
           03  WIVA-9-ED               PIC ZZZ,ZZ9.99.
           03  WIVA-TASA-ED            PIC ZZZ,ZZ9.99.
           03  WIVA-IMP1-ED            PIC ZZZZ.
           03  WIVA-IMP2-ED            PIC ZZZZ.
           03  WIVA-IMP3-ED            PIC ZZZZ.
           03  WIVA-NETO1-ED           PIC ZZZ,ZZ9.99.
           03  WIVA-NETO2-ED           PIC ZZZ,ZZ9.99.
           03  WIVA-NETO3-ED           PIC ZZZ,ZZ9.99.          
       01  VECTORES.
           03  VECTOR-LEIDOS OCCURS 200.
               05  V-EMP          PIC 9(4).    
               05  V-SUC          PIC 99.
               05  V-IMP          PIC 9(6).
               05  V-MOV          PIC 9(6).
               05  V-DIA          PIC 99.
               05  V-MES          PIC 99.
               05  V-ANO          PIC 99.
               05  V-COMP         PIC 9(8).
           03  VECTOR-A-PAGAR OCCURS 20.
               05  V-IMPORTE      PIC 9(10)V99.
               05  V-MULTA        PIC 9(10)V99.
               05  V-MORA         PIC 9(10)V99.
               05  V-COM          PIC X(20).
               05  V-IMPUESTO     PIC 999.
               05  V-COMPRO       PIC 9(8).
               05  V-FECHA-COMP.
                   07  V-DIA-COMP     PIC 99.
                   07  V-MES-COMP     PIC 99.
                   07  V-ANO-COMP     PIC 99.
              
           03  VECTOR-DE-REGISTROS.
               05  V-REGISTRO     PIC X(250) OCCURS 20.
           03  MESES-LETRA.
               05  M1             PIC X(12) VALUE "  ENERO".
               05  M2             PIC X(12) VALUE " FEBRERO".
               05  M3             PIC X(12) VALUE "  MARZO".
               05  M4             PIC X(12) VALUE "  ABRIL".
               05  M5             PIC X(12) VALUE "  MAYO".
               05  M6             PIC X(12) VALUE "  JUNIO".
               05  M7             PIC X(12) VALUE "  JULIO".
               05  M8             PIC X(12) VALUE "  AGOSTO".
               05  M9             PIC X(12) VALUE " SETIEMBRE".
               05  M10            PIC X(12) VALUE "  OCTUBRE".
               05  M11            PIC X(12) VALUE " NOVIMEBRE".
               05  M12            PIC X(12) VALUE " DICIEMBRE".
           03  MES-L REDEFINES MESES-LETRA PIC X(12) OCCURS 12.
               
       01  A                      PIC X.
       01  I1                     PIC 99 VALUE 0.
       01  CONTADOR-IVA           PIC 9(6) VALUE 0.
       01  ESTE-RENG              PIC 999.
       01  TOPE-IVA               PIC 9(6) VALUE 0.
       77  EXISTE                 PIC X(2) VALUE "SI".
       77  TASA                   PIC 99 VALUE 0.
       77  SALTO                  PIC 99 VALUE 0.

       77  DIF                    PIC 9(4)V99 VALUE 0.
       01  ES-INS                 PIC X.
       01  SOLO-MES               PIC X.
       01  RECIBE-LETRAS.          
           03  RECIBE-L1          PIC X(60).
           03  RECIBE-L2          PIC X(60).

       PROCEDURE DIVISION.
       DECLARATIVES.
           COPY "P-CTA.ERR".
           COPY "EMPRE.ERR".
           COPY "CUENTA.ERR".
           COPY "MOVI.ERR".
           COPY "CPOSTAL.ERR".
           COPY "COMPRO1.ERR".
           COPY "IMP-132.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO-ARCHIVOS.
           OPEN I-O    MOVI CUENTAS COMPRO        
                INPUT  CPOSTAL EMPRESAS P-DE-CTAS
                OUTPUT IMP-132.
       RETOMA.
           ACCEPT FECHA-SIS FROM DATE
           MOVE DIA-SIS   TO  DIA-SIS-ED
           MOVE MES-SIS   TO  MES-SIS-ED
           MOVE ANO-SIS   TO  ANO-SIS-ED
           
           MOVE 0 TO VALIDO
           PERFORM ARMA-PANTALLA.
           DISPLAY FECHA-SIS-ED LINE 2 POSITION 70
           PERFORM TOMA-PARAMETROS THRU TOMA-PARAMETROS-F
           IF VALIDO  = 9 GO TO FINAL1.           
           PERFORM LEA-MOV THRU LEA-MOV-F.
           GO TO RETOMA.

       TOMA-PARAMETROS.
       C1.    
           DISPLAY SPACES LINE 1 POSITION 12 SIZE 4 REVERSE.
           ACCEPT EMP LINE 1 POSITION 12 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 
                 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1     MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.
           DISPLAY SPACES LINE 1 POSITION 12 SIZE 4.
           MOVE EMP TO EMP-ED.
           DISPLAY EMP-ED LINE 1 POSITION 12.
       C2.

           DISPLAY SPACES LINE 1 POSITION 17 SIZE 2 REVERSE.      
           ACCEPT SUC LINE 1 POSITION 17 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 
                 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.

           IF FLE1 MOVE 9 TO VALIDO 
                       GO TO C1.
           DISPLAY SPACES LINE 1 POSITION 17 SIZE 2.
           MOVE SUC TO SUC-ED.
           DISPLAY SUC-ED LINE 1 POSITION 17.                      
           MOVE EMP TO NUM-EMP 
           MOVE SUC TO SUC-EMP 
           READ EMPRESAS INVALID KEY MOVE "NO EMPRESAS" TO 
                RAZ-SOC-EMP
                DISPLAY RAZ-SOC-EMP LINE 1 POSITION 30 SIZE 40
                GO TO C1.
           DISPLAY RAZ-SOC-EMP LINE 1 POSITION 22 SIZE 30.
       
       
       C3.
           DISPLAY SPACES LINE 2 POSITION 10 SIZE 6 REVERSE.
           ACCEPT RUBRO LINE 2 POSITION 10 NO BEEP TAB CONVERT REVERSE
             ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1 GO TO C2.
           MOVE RUBRO TO RUBRO-ED.
           DISPLAY RUBRO-ED LINE 2 POSITION 10.
           MOVE RUBRO TO P-RUBRO.
           MOVE 0 TO P-RUBRO-A.
           MOVE VERSION-EMP TO P-VERSION
           READ P-DE-CTAS INVALID KEY DISPLAY "No Existe Rubro"
               LINE 2 POSITION 25 SIZE 30
               GO TO C3.
           DISPLAY P-NOMBRE LINE 2 POSITION 25 SIZE 30.
           
       
       C4.
           DISPLAY SPACES LINE 3 POSITION 10 SIZE 6 REVERSE.
           ACCEPT CTA LINE 3 POSITION 10 NO BEEP TAB CONVERT REVERSE
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1 GO TO C3.
           MOVE CTA TO CTA-ED.
           DISPLAY CTA-ED LINE 3 POSITION 10 SIZE 6.
       
       
       C5.
           DISPLAY SPACES LINE 3 POSITION 17 SIZE 2 REVERSE.
           ACCEPT S-CTA LINE 3 POSITION 17 NO BEEP TAB CONVERT 
               REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1 GO TO C4.
           MOVE S-CTA TO S-CTA-ED.
           DISPLAY S-CTA-ED LINE 3 POSITION 17 SIZE 2.
       
           MOVE EMP    TO EMP-CTA
           MOVE SUC    TO SUC-CTA
           MOVE RUBRO  TO RUBRO-CTA
           MOVE CTA    TO CUENTA
           MOVE S-CTA  TO SUB-CUENTA
           READ CUENTAS INVALID KEY DISPLAY "No Existe Cuenta"
                 LINE 3 POSITION 20 SIZE 30
                 GO TO C4.
           DISPLAY NOMBRE-C LINE 3 POSITION 20 SIZE 30.
       

           MOVE 0 TO  ANO-MOV MES-MOV DIA-MOV 
                      MOVIMIENTO-MOV RENGLON TOTAL I I1
           
           MOVE EMP      TO EMP-MOV.
           MOVE SUC      TO SUC-EMP-MOV
           MOVE RUBRO    TO RUBRO-MOV
           MOVE CTA      TO CUENTA-MOV
           MOVE S-CTA    TO SUB-CUENTA-MOV
           
           START MOVI KEY NOT LESS CLAVE-MOV INVALID KEY 
                 DISPLAY "NO HAY MOVIMIENTOS" LINE 24 POSITION 10
                          SIZE 69
                 ACCEPT QQ
                 GO TO C1.
       
       
           MOVE "S" TO DED.
           ACCEPT DED LINE 3 POSITION 75 ECHO UPDATE 
             NO BEEP TAB ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1 GO TO C5.


       TOMA-PARAMETROS-F.
           EXIT.          
       LEA-MOV.
           READ MOVI NEXT AT END  GO TO APAGAR.
           IF EMP-MOV        NOT = EMP    GO TO APAGAR.
           IF SUC-EMP-MOV    NOT = SUC    GO TO APAGAR.
           IF RUBRO-MOV      NOT = RUBRO  GO TO APAGAR.
           IF CUENTA-MOV     NOT = CTA    GO TO APAGAR.
           IF SUB-CUENTA-MOV NOT = S-CTA  GO TO APAGAR.
           IF TIPO-C-MOV     NOT = 2      GO TO LEA-MOV. 
           IF DED = "S"
              IF IMPORTE-MOV    NOT > IMP-EXT-MOV GO TO LEA-MOV.
           
           PERFORM MOSTRAR-IMPUESTO THRU MOSTRAR-IMPUESTO-F
           GO TO LEA-MOV.
       MOSTRAR-IMPUESTO.
           MOVE DIA-MOV        TO DIA-ED
           MOVE MES-MOV        TO MES-ED
           MOVE ANO-MOV        TO ANO-ED 
           COMPUTE VALOR = IMPORTE-MOV - IMP-EXT-MOV
           MOVE VALOR          TO PESOS-ED
           MOVE COMENTARIO-MOV TO DETA-ED
           MOVE COMPRO-MOV     TO COMPRO-ED
           PERFORM MOS THRU MOS-F 
           GO TO MOSTRAR-IMPUESTO-F.
       MOS.
           ADD 1        TO I RENGLON
           MOVE RENGLON TO R-ED
           IF I > 12  DISPLAY "LLENO PANTALLA" LINE 18 POSITION 30
                                    SIZE 49
                      ACCEPT QQ LINE 18 POSITION 70
                      PERFORM BORRAR-PANTALLA VARYING I FROM 1 BY 1
                              UNTIL I > 13
                      MOVE 0 TO I.
           COMPUTE LIN = 5 + I                    
           DISPLAY  RENG-PANT LINE LIN POSITION 3 SIZE 75.

           MOVE EMP-MOV           TO V-EMP(RENGLON)
           MOVE SUC-EMP-MOV       TO V-SUC(RENGLON)
           MOVE ANO-MOV           TO V-ANO(RENGLON)
           MOVE MES-MOV           TO V-MES(RENGLON)
           MOVE DIA-MOV           TO V-DIA(RENGLON)
           MOVE MOVIMIENTO-MOV    TO V-MOV(RENGLON).
           MOVE COMPRO-MOV        TO V-COMP(RENGLON).
           GO TO MOS-F.
       BORRAR-PANTALLA.
           COMPUTE LIN = 5 + I
           DISPLAY SPACES LINE LIN POSITION 1 SIZE 79.    
       MOS-F.               
           EXIT.                         
       MOSTRAR-IMPUESTO-F.
           EXIT.    
       APAGAR.
           ACCEPT ESTE-RENG LINE 20 POSITION 22 NO BEEP TAB CONVERT
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF I1  = 10 GO TO EMITE-REC.
           IF ESCAPE-X GO TO EMITE-REC.
      *     IF FLE1 GO TO C4.
           IF ESTE-RENG = 0 GO TO APAGAR.
           IF ESTE-RENG > RENGLON GO TO APAGAR.
           MOVE V-EMP(ESTE-RENG) TO EMP-MOV
           MOVE V-SUC(ESTE-RENG) TO SUC-EMP-MOV
           MOVE V-ANO(ESTE-RENG) TO ANO-MOV
           MOVE V-MES(ESTE-RENG) TO MES-MOV
           MOVE V-DIA(ESTE-RENG) TO DIA-MOV
           MOVE V-MOV(ESTE-RENG) TO MOVIMIENTO-MOV
           MOVE RUBRO            TO RUBRO-MOV
           MOVE CTA              TO CUENTA-MOV
           MOVE S-CTA            TO SUB-CUENTA-MOV
           READ MOVI INVALID KEY DISPLAY "IMP"
                                   GO TO APAGAR.

           COMPUTE VALOR = IMPORTE-MOV - IMP-EXT-MOV
           MOVE VALOR      TO ESTE-IMPORTE PESOS-ED.
           
           DISPLAY PESOS-ED    LINE 21 POSITION 22 REVERSE
           DISPLAY " " LINE 24 POSITION 78 REVERSE.
       CAP-IMP.
           ACCEPT ESTE-IMPORTE LINE 21 POSITION 22 NO BEEP TAB
                ECHO UPDATE REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1     GO TO C4.
           GO TO CONFIRMA. 
       CAP-MUL.
           ACCEPT MULTA LINE 22 POSITION 22 NO BEEP TAB ECHO UPDATE
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1     GO TO CAP-IMP.
           ACCEPT MORA LINE 23 POSITION 22 NO BEEP TAB ECHO UPDATE
                  REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X GO TO FINAL1.
           IF FLE1     GO TO CAP-MUL.
       CONFIRMA.           
           ACCEPT DED LINE 24 POSITION 78 TAB REVERSE
           IF DED NOT = "S" AND DED NOT = "s" GO TO APAGAR.
           PERFORM GUARDA-SUME THRU GUARDA-SUME-F
           GO TO APAGAR.
       GUARDA-SUME.
           ADD 1    TO I1
           MOVE ESTE-IMPORTE    TO V-IMPORTE(I1)
           MOVE MULTA           TO V-MULTA(I1)
           MOVE MORA            TO V-MORA(I1)
           MOVE COMENTARIO-MOV  TO V-COM(I1)
           MOVE COMPRO-MOV      TO V-COMPRO(I1)
           MOVE ANO-MOV         TO V-ANO(I1)
           MOVE MES-MOV         TO V-MES(I1)
           MOVE DIA-MOV         TO V-DIA(I1)
           MOVE REG-MOV         TO V-REGISTRO(I1)
           COMPUTE TOTAL   = TOTAL + ESTE-IMPORTE + MULTA + MORA.
           MOVE TOTAL           TO PESOS-ED
           DISPLAY PESOS-ED LINE 24 POSITION 30 REVERSE.
       GUARDA-SUME-F.
           EXIT.   
       EMITE-REC.
           IF TOTAL = 0 GO TO LEA-MOV-F.
           DISPLAY "EMITE RECIBO" LINE 24 POSITION 50 SIZE 29
                    BLINK
           MOVE "S" TO DED
           ACCEPT DED LINE 24 POSITION 75 TAB ECHO UPDATE
           IF DED NOT = "S" AND DED NOT = "s" GO TO LEA-MOV-F.
           MOVE 0 TO COM
           PERFORM REGRABA-COMP THRU REGRABA-COMP-F.
           IF COM = 9 GO TO APAGAR.
           PERFORM LISTA-RECIBO THRU LISTA-RECIBO-F.
      *     PERFORM LISTA-RECIBO THRU LISTA-RECIBO-F.
       
           PERFORM REGRABA-REGISTRO THRU REGRABA-REGISTRO-F
                VARYING I FROM 1 BY 1 UNTIL I > I1
           
           PERFORM GRABA-MOV    THRU GRABA-MOV-F.
           PERFORM REGRABA-CTA  THRU REGRABA-CTA-F.
           
           GO TO LEA-MOV-F.
       LISTA-RECIBO.
           MOVE NOMBRE-C     TO LACTA-ED
           MOVE DIRE         TO LADIRE-ED.
           MOVE COD-LOCA     TO NRO-LOCA-ED C-POS
           MOVE 0            TO C-POS-1.    
           READ CPOSTAL INVALID KEY MOVE "** ** ** ** ** ** "
                 TO NOM-POSTAL.
           MOVE NOM-POSTAL TO LALOCA-ED.
           
           
           MOVE TOTAL        TO IMPORTE-ED
                                PTOT-GRAL.
           PERFORM CONVERSOR THRU FIN-CONVERSOR.
           MOVE PSALIDA-1     TO RECIBE-LETRAS
           MOVE RECIBE-L1     TO LACANT-ED
           MOVE RECIBE-L2     TO LACANT1-ED

           MOVE DIA-SIS      TO DIA-REC-ED  
           MOVE MES-SIS      TO MES-REC-ED
           MOVE ANO-SIS      TO ANO-REC-ED  
           
           WRITE LINEA FROM RENG2  AFTER 10
           WRITE LINEA FROM RENG3  AFTER 1
           WRITE LINEA FROM RENG4  AFTER 1
           WRITE LINEA FROM RENG5  AFTER 7
           WRITE LINEA FROM RENG6  AFTER 2
           WRITE LINEA FROM RENG7  AFTER 1           
           WRITE LINEA FROM RENG7-1 AFTER 1
           WRITE LINEA FROM RENG8  AFTER 1
           
           PERFORM LISTA-FAC THRU LISTA-FAC-F
                VARYING I FROM 1 BY 1 UNTIL I > I1.
           COMPUTE SALTO = 11 - I1
           WRITE LINEA FROM RENG10 AFTER SALTO.
           WRITE LINEA FROM RENG11 AFTER 1.
           MOVE SPACES TO LINEA
           WRITE LINEA AFTER 10.

       LISTA-RECIBO-F.
           EXIT.
      
       LISTA-FAC.
           MOVE V-COMPRO(I)    TO COMP-ED
           MOVE V-IMPORTE(I) TO IMP-ED.
           MOVE V-ANO(I)     TO ANO-F-ED
           MOVE V-MES(I)     TO MES-F-ED
           MOVE V-DIA(I)     TO DIA-F-ED
           WRITE LINEA FROM RENG9  AFTER 1.
       LISTA-FAC-F.
           EXIT.
       
       REGRABA-REGISTRO.
           MOVE V-REGISTRO(I) TO REG-MOV
           
           COMPUTE IMP-EXT-MOV = IMP-EXT-MOV + V-IMPORTE(I).           
           REWRITE REG-MOV INVALID KEY DISPLAY "IMP".

       REGRABA-REGISTRO-F.
           EXIT.
       LEA-MOV-F.
           EXIT.    
       FINAL1.

           CLOSE   EMPRESAS CUENTAS P-DE-CTAS CPOSTAL.
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.

           DISPLAY "Empresa.:" LINE 1 POSITION 1
                   "Rubro ..:" LINE 2 POSITION 1
                   "Cuenta .:" LINE 3 POSITION 1
                   "Impagas (S) .:" LINE 3 POSITION 55.
           DISPLAY "Reng. Fecha Venc."    LINE 4 POSITION 3
                   "Comprobante"      LINE 4 POSITION 24
                   "D e t a l l e"        LINE 4 POSITION 41
                   " Importe "            LINE 4 POSITION 65
           DISPLAY ALL "-" LINE 5 POSITION 3 SIZE 76
           DISPLAY ALL "-" LINE 19 POSITION 3 SIZE 76
                   "Paga Renglon ....:"   LINE 20 POSITION 3
                   "Importe .........:"   LINE 21 POSITION 3
                   "Importe Multa....:"   LINE 22 POSITION 3
                   "Importe Mora.....:"   LINE 23 POSITION 3
                   "TOTAL RECIBO.....:"   LINE 24 POSITION 3

                   "Confirma (S).:"       LINE 24 POSITION 63.


       REGRABA-COMP.
           MOVE EMP    TO EMP-COMPRO
           MOVE SUC    TO SUC-COMPRO  SUC-C-COMPRO
           MOVE 1      TO CAJA-COMPRO
           MOVE 3      TO NUM-COMP
           MOVE SPACES TO SUB-T-C-COMPRO
          
           READ COMPRO INVALID KEY DISPLAY "No hay Comp. Grabado"
                 LINE 25 POSITION 15 SIZE 64
                 ACCEPT QQ
                 DISPLAY SPACES LINE 25 POSITION 15 SIZE 64
                 MOVE 9 TO COM
                 GO TO REGRABA-COMP-F.
           ADD 1 TO NUM-COMPRO.
           REWRITE REG-COMPRO INVALID KEY DISPLAY
               "No Regrabo Comprobante"
               LINE 25 POSITION 15 SIZE 64
               ACCEPT QQ
               DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.
       REGRABA-COMP-F.
           EXIT.

       GRABA-MOV.         
           MOVE EMP        TO EMP-MOV LAC-EMP
           MOVE SUC        TO SUC-EMP-MOV SUC-C-MOV SUCURSAL-MOV
                              LAC-SUC
           MOVE 1          TO LAC-TURNO
           MOVE 9          TO LAC-CONDI
           MOVE RUBRO      TO RUBRO-MOV LAC-RUBRO
           MOVE CTA        TO CUENTA-MOV LAC-CUENTA
           MOVE S-CTA      TO SUB-CUENTA-MOV LAC-SUB-CTA
           MOVE ANO-SIS    TO ANO-MOV LAC-ANO
           MOVE MES-SIS    TO MES-MOV LAC-MES
           MOVE DIA-SIS    TO DIA-MOV LAC-DIA
           MOVE "E"        TO LAC-TIPO
           MOVE NUM-COMPRO TO COMPRO-MOV MOVIMIENTO-MOV LAC-COMPROB
           MOVE 2          TO TIPO-MOV 
           MOVE 3          TO TIPO-C-MOV
           MOVE SPACES     TO SUB-T-C-MOV
           MOVE 0          TO REMA-MOV           
           MOVE TOTAL      TO IMPORTE-MOV LAC-IMPORTE
           MOVE 0          TO IMP-EXT-MOV LAC-IMP-CHEQ LAC-TARJ-UTI
                              LAC-SUMA-RESTA
           MOVE "Pago a Cuenta"
                           TO COMENTARIO-MOV
           MOVE 0          TO ASIENTO-MOV

           WRITE REG-MOV INVALID KEY DISPLAY 
                 "No Grabo Movimiento"
                 LINE 25 POSITION 15 SIZE 64
                 ACCEPT QQ
                 DISPLAY SPACES LINE 25 POSITION 15 SIZE 64.
           OPEN I-O LACAJA.
           WRITE REG-LACAJA INVALID KEY DISPLAY
                 "NO GRABO LA CAJA".
           CLOSE LACAJA.
       GRABA-MOV-F.
           EXIT.

       REGRABA-CTA.
           COMPUTE SALDO = SALDO - IMPORTE-MOV
           REWRITE REG-CTAS INVALID KEY DISPLAY "No Regrabo Cuentas"
                     LINE 25 POSITION 15 SIZE 64
                     ACCEPT QQ
                     DISPLAY SPACES LINE 25 POSITION 15 SIZE 64
                     GO TO REGRABA-CTA-F.
       REGRABA-CTA-F.
           EXIT.
       
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
     
      **     COPY "CONVER.PRO".
             COPY "PESOS.PRO".
