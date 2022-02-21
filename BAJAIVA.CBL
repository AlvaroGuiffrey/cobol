000000 IDENTIFICATION DIVISION.
       PROGRAM-ID.  COART02.
       AUTHOR.  ALVARO.
       ENVIRONMENT  DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  AMI.
       OBJECT-COMPUTER.  AMI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "IVA.SEL".
           COPY "IVA1.SEL".
000000 DATA DIVISION.
       FILE SECTION.
       COPY "IVA.REG".
       COPY "IVA1.REG".
       WORKING-STORAGE SECTION.
       77 LLAVEFIN                PIC X.
       01 CONT                    PIC 9(6) VALUE 0.
       01 ERRORES                 PIC XX.
       01 DED                     PIC X.
       01 LAFECHA.
          03  HF                  PIC 9(6).
          03  R-HF REDEFINES HF.
              05  H-D             PIC 99.
              05  H-M             PIC 99.
              05  H-A             PIC 99.
          03  ESTA                PIC 9(6) VALUE 0.
          03  DESDE               PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       ABRO-ARCHIVOS.
            OPEN INPUT IVA.
            OPEN OUTPUT IVA1.
            MOVE 0 TO CONT. 
            DISPLAY SPACES LINE 1 POSITION 1 ERASE.
            DISPLAY " A T E N C I O N" LINE 3 POSITION 35 BLINK
            DISPLAY "BORRADO DE PASES DE I.V.A." LINE 5 POSITION 20.
            DISPLAY "Se genera archivo IVA1 con" LINE 7 POSITION 20
            DISPLAY "los pases que continuaran " LINE 9 POSITION 20
                    "en el equipo por lo que se" LINE 11 POSITION 20
                    "debe COPIAR dicho archivo " LINE 13 POSITION 20
                    "a IVA una vez respaldo IVA" LINE 15 POSITION 20
            DISPLAY "DIGITE HASTA FECHA A BORRAR" LINE 18 POSITION 10
            ACCEPT HF LINE 18 POSITION 50 NO BEEP TAB CONVERT
            DISPLAY "FECHA CORRECTA ?(S)" LINE 24 POSITION 10
            ACCEPT DED LINE 24 POSITION 50 NO BEEP TAB
            IF DED NOT = "S" AND DED NOT = "S" STOP RUN.
            COMPUTE DESDE = (H-A * 372) + (H-M * 31) + H-D.

       LEA.
            READ IVA NEXT AT END CLOSE IVA IVA1
                            STOP RUN.
            ADD 1 TO CONT
            DISPLAY CONT LINE 20 POSITION 35
            COMPUTE ESTA = (ANO-IVA * 372) + (MES-IVA * 31) + DIA-IVA
            IF ESTA  NOT > DESDE GO TO LEA.
            WRITE REG-IVA1 FROM REG-IVA INVALID KEY
                        DISPLAY "IMP".
            GO TO LEA.
