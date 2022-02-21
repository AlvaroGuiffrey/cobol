       IDENTIFICATION DIVISION.
       PROGRAM-ID.       LISFACT.
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
       
       DATA DIVISION.
       FILE SECTION.
              COPY "P-CTAS.REG".
              COPY "EMPREBM.REG".
              COPY "CUENTABM.REG".
              COPY "MOVI.REG".              
              COPY "CPOSTAL.REG".
              COPY "COMPRO1.REG".
              COPY "IMP-132.REG".
       
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
       01  RENGLON                     PIC 99 VALUE 0 .
       01  RAYITA                      PIC X(33) VALUE ALL "Ä".
       01  NOMBRE                      PIC X(30).
       
       01  RENGLONES.
           03  TIT1.
               05  FILLER             PIC X(5) VALUE SPACES.
               05  FILLER             PIC X(28) VALUE
               "LISTADO CREDITOS EMITIDOS".
               05  FILLER             PIC X(14) VALUE 
               "DESDE FECHA :".
               05  FEC-ED.
                   07  D-ED           PIC 99/.
                   07  M-ED           PIC 99/.
                   07  A-ED           PIC 99BB.
               05  FILLER             PIC X(14) VALUE 
               "HASTA FECHA :".
               05  FEC1-ED.
                   07  D1-ED           PIC 99/.
                   07  M1-ED           PIC 99/.
                   07  A1-ED           PIC 99.

           03  TIT2.
               05  FILLER             PIC X(5) VALUE SPACES.
               05  FILLER             PIC X(12) VALUE
               "F E C H A".
               05  FILLER             PIC X(8) VALUE
               "CUENTA ".
               05  FILLER             PIC X(12) VALUE
               "COMPROB.".
               05  FILLER             PIC X(14) VALUE
               "I M P O R T E".
           03  RENG1.
               05  FILLER             PIC X(5) VALUE SPACES.
               05  FECHA-ED.
                   07  DIA-ED         PIC 99/.
                   07  MES-ED         PIC 99/.
                   07  ANO-ED         PIC 99BBB.
               05  CTA-ED             PIC ZZZZZZB(4).
               05  COMP-ED            PIC ZZZZZZB(4).
               05  IMP-ED             PIC ZZZ,ZZZ,ZZ9.99.

       
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
           03  VECTOR-LEIDOS OCCURS 40.
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
       01  ESTE-RENG              PIC 99.
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
           PERFORM TITULOS THRU TITULOS-F
           PERFORM PROCESO THRU PROCESO-F
           GO TO RETOMA.

       TOMA-PARAMETROS.
       C1.    
           DISPLAY SPACES LINE 5 POSITION 20 SIZE 4 REVERSE.
           ACCEPT EMP LINE 5 POSITION 20 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 
                 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO FINAL1.
           IF FLE1     MOVE 9 TO VALIDO 
                       GO TO FINAL1.
           DISPLAY SPACES LINE 5 POSITION 20 SIZE 4.
           MOVE EMP TO EMP-ED.
           DISPLAY EMP-ED LINE 5 POSITION 20.
       C2.

           DISPLAY SPACES LINE 5 POSITION 27 SIZE 2 REVERSE.      
           ACCEPT SUC LINE 5 POSITION 27 ECHO UPDATE
              NO BEEP TAB REVERSE ON EXCEPTION CONDI MOVE 1 
                 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO FINAL1.

           IF FLE1 MOVE 9 TO VALIDO 
                       GO TO C1.
           DISPLAY SPACES LINE 5 POSITION 27 SIZE 2.
           MOVE SUC TO SUC-ED.
           DISPLAY SUC-ED LINE 5 POSITION 27.                      
           MOVE EMP TO NUM-EMP 
           MOVE SUC TO SUC-EMP 
           READ EMPRESAS INVALID KEY MOVE "NO EMPRESAS" TO 
                RAZ-SOC-EMP
                DISPLAY RAZ-SOC-EMP LINE 5 POSITION 30 SIZE 40
                GO TO C1.
           DISPLAY RAZ-SOC-EMP LINE 5 POSITION 30 SIZE 30.
       
       
       C3.
           DISPLAY SPACES LINE 8 POSITION 20 SIZE 6 REVERSE.
           ACCEPT RUBRO LINE 8 POSITION 20 NO BEEP TAB CONVERT REVERSE
             ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO
                       GO TO FINAL1.
           IF FLE1 GO TO C2.
           MOVE RUBRO TO RUBRO-ED.
           DISPLAY RUBRO-ED LINE 8 POSITION 20.
           MOVE RUBRO TO P-RUBRO.
           MOVE 0 TO P-RUBRO-A.
           MOVE VERSION-EMP TO P-VERSION
           READ P-DE-CTAS INVALID KEY DISPLAY "No Existe Rubro"
               LINE 8 POSITION 25 SIZE 30
               GO TO C3.
           DISPLAY P-NOMBRE LINE 8 POSITION 27 SIZE 30.
           
       
       C4.
           DISPLAY SPACES LINE 11 POSITION 20 SIZE 6 REVERSE.
           ACCEPT DF LINE 11 POSITION 20 NO BEEP TAB CONVERT REVERSE
                   ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO FINAL1.
           IF FLE1 GO TO C3.
       
       C5.
           DISPLAY SPACES LINE 14 POSITION 20 SIZE 2 REVERSE.
           ACCEPT HF LINE 14 POSITION 20 NO BEEP TAB CONVERT 
               REVERSE ON EXCEPTION CONDI MOVE 1 TO SW1.
           IF ESCAPE-X MOVE 9 TO VALIDO 
                       GO TO TOMA-PARAMETROS-F.
           IF FLE1 GO TO C4.
           COMPUTE DESDE = (D-A * 372) + (D-M * 31) + D-D
           COMPUTE HASTA = (H-A * 372) + (H-M * 31) + H-D.
           MOVE D-D TO D-ED
           MOVE D-M TO M-ED
           MOVE D-A TO A-ED
           MOVE H-D TO D1-ED
           MOVE H-M TO M1-ED
           MOVE H-A TO A1-ED.

       TOMA-PARAMETROS-F.
           EXIT.          
       PROCESO.
       LEA-MOV.
           READ MOVI NEXT AT END  GO TO PROCESO-F.
           IF RUBRO-MOV NOT = RUBRO GO TO LEA-MOV.
           COMPUTE ESTA = (ANO-MOV * 372) + (MES-MOV * 31) + DIA-MOV.
           IF ESTA < DESDE OR ESTA > HASTA GO TO LEA-MOV.
           IF IMPORTE-MOV = IMP-EXT-MOV GO TO LEA-MOV.
           IF TIPO-C-MOV NOT = 4 GO TO LEA-MOV.
           PERFORM ARMA THRU ARMA-F
           PERFORM AVANCE-L THRU AVANCE-L-F
           PERFORM IMPRIMA THRU IMPRIMA-F
           GO TO LEA-MOV.
       ARMA.
           MOVE DIA-MOV      TO DIA-ED
           MOVE MES-MOV      TO MES-ED
           MOVE ANO-MOV      TO ANO-ED
           MOVE CUENTA-MOV   TO CTA-ED
           MOVE COMPRO-MOV   TO COMP-ED
           MOVE IMPORTE-MOV  TO IMP-ED.
           COMPUTE TOTAL = TOTAL + IMPORTE-MOV.
       ARMA-F.
           EXIT.

           COPY "AVANCES.RUT".

       IMPRIMA.
           WRITE LINEA FROM RENG1 AFTER 1.
       IMPRIMA-F.
           EXIT.
       TITULOS.
           WRITE LINEA FROM TIT1 AFTER 2
           WRITE LINEA FROM TIT2 AFTER 2
           MOVE 4 TO CONT-RENG.
       TITULOS-F.
           EXIT.
       PROCESO-F.
           EXIT.
       FINAL1.
           MOVE TOTAL TO IMP-ED
           MOVE 0     TO DIA-ED MES-ED ANO-ED CTA-ED COMP-ED
           WRITE LINEA FROM RENG1 AFTER 2.
           CLOSE   EMPRESAS CUENTAS P-DE-CTAS CPOSTAL.
           GO TO FFFF.
       ARMA-PANTALLA.
           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           DISPLAY "Listado de Creditos Emitidos" LINE 2 POSITION 20
           DISPLAY "Empresa......:" LINE 5 POSITION 1
                   "Rubro .......:" LINE 8 POSITION 1
                   "Desde Fecha..:" LINE 11 POSITION 1
                   "Hasta Fecha..:" LINE 14 POSITION 1.
       FFFF.    
           EXIT PROGRAM.
       FFF1.
           STOP RUN.
     
