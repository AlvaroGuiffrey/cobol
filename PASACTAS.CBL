000000 IDENTIFICATION DIVISION.
       PROGRAM-ID.  COART02.
       AUTHOR.  ALVARO.
       ENVIRONMENT  DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  AMI.
       OBJECT-COMPUTER.  AMI.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "CUENTA.SEL".
           COPY "CSX.SEL".
000000 DATA DIVISION.
       FILE SECTION.
       COPY "CUENT.REG".
       COPY "CSX.REG".
       WORKING-STORAGE SECTION.
       77 LLAVEFIN                PIC X.
       01 CONT                    PIC 9(6) VALUE 0.
       01 ERRORES                 PIC XX.
       PROCEDURE DIVISION.
       ABRO-ARCHIVOS.
            OPEN INPUT CUENTAS.
            OPEN OUTPUT CUENTASX.
            MOVE 0 TO CONT. 
            DISPLAY SPACES LINE 1 POSITION 1 ERASE.
            DISPLAY "PASAJE DE CUENTAS CORRIENTES" LINE 20 POSITION 10.
       LEA.
            READ CUENTAS NEXT AT END CLOSE CUENTAS CUENTASX
                            STOP RUN.
            ADD 1 TO CONT
            DISPLAY CONT LINE 15 POSITION 35
            MOVE CLAVE-CTA TO CLAVE-CTAX.
            IF RUBRO-CTA = 1201 MOVE 1201 TO RUBRO-CTAX
                                MOVE 51 TO CONDICIONX.
            IF RUBRO-CTA = 2001 MOVE 2001 TO RUBRO-CTAX
                                MOVE 86 TO CONDICIONX.
            MOVE NOMBRE-C TO NOMBRE-CX.
            MOVE DIRE TO DIREX
            MOVE COD-LOCA TO COD-LOCAX
            MOVE ESTADO TO ESTADOX
      *      MOVE CONDICION TO CONDICIONX
            MOVE INSCRIPTO TO INSCRIPTOX
            MOVE ZONA TO ZONAX.
            MOVE IGAN TO IGANX.
            MOVE ING-BRUTOS TO ING-BRUTOSX.
            MOVE C-ING-BRUTOS TO C-ING-BRUTOSX.
            MOVE JNC TO JNCX.
            MOVE TEL TO TELX.
            MOVE 0 TO SALDOX.
            MOVE 0 TO SALDO-ANTX.
            MOVE 0 TO SALDO-CANX.
            MOVE 5000 TO SALDO-TOPX.
            MOVE 0 TO PMOVX.
            MOVE 0 TO PPENDX.
            MOVE 0 TO FECHA-CANX.
            MOVE 0 TO COD-LOCA-1.
            MOVE SPACES TO CTACTE-CTA.
            MOVE 0 TO CU-ANO-INTX CU-MES-INTX CU-DIA-INTX
            MOVE 0 TO CU-DIAX(1) CU-MESX(1) CU-ANOX(1) CU-SALDOX(1)
            MOVE 0 TO CU-DIAX(2) CU-MESX(2) CU-ANOX(2) CU-SALDOX(2)
            MOVE 0 TO CU-DIAX(3) CU-MESX(3) CU-ANOX(3) CU-SALDOX(3)
            MOVE 0 TO CU-DIAX(4) CU-MESX(4) CU-ANOX(4) CU-SALDOX(4)
            
            WRITE REG-CTASX INVALID KEY
                        DISPLAY "IMP".
            GO TO LEA.
