       IDENTIFICATION DIVISION.
       PROGRAM-ID. F468B.
       AUTHOR. GUIFFREY ALVARO.
       DATE-WRITTEN. MAR'93.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *...............................(*.SEL)
       COPY "MANEJO.SEL".
       COPY "IVA.SEL".
       COPY "ALICUO.SEL".
       COPY "CUENTA.SEL".
       COPY "EMPRE.SEL".
       COPY "IMP02.SEL".
       COPY "PANTA.SEL".
       DATA DIVISION.
       FILE SECTION.
      *...............................(*.REG)
       COPY "MANEJO.REG".
       COPY "IVA.REG".
       COPY "ALICUO.REG".
       COPY "EMPREBM.REG".
       COPY "CUENTA.REG".
       COPY "IMP02.REG".
       COPY "PANTA.REG".
       WORKING-STORAGE SECTION.
      *...............................(ROTULO DE IMPRESION)
       COPY "ROT132.IMP".
      *...............................(LINEAS DE IMPRESION)
       01 IMPRESION.
          02 COMPRI.
             03 A01        PIC 99 COMP-4 VALUE 15.
          02 TIT.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(19) VALUE "- PERIODO FISCAL : ".
             03 MES-ED     PIC 99.
             03 FILLER     PIC X VALUE "/".
             03 ANO-ED     PIC 99.
             03 FILLER     PIC XX VALUE " -".
          02 TIT-0.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "*-------------------".
             03 FILLER     PIC X(20) VALUE "--------------------".
             03 FILLER     PIC X(20) VALUE "-------------------*".
             03 FILLER     PIC X(4) VALUE "----".
             03 FILLER     PIC X(20) VALUE "*------------------*".
             03 FILLER     PIC X(9) VALUE "----*----".
             03 FILLER     PIC X(20) VALUE "*------------------*".
             03 FILLER     PIC X(5) VALUE "----*".             
          02 TIT-00.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "|-------------------".
             03 FILLER     PIC X(20) VALUE "--------------------".
             03 FILLER     PIC X(20) VALUE "-------------------|".
             03 FILLER     PIC X(4) VALUE "----".
             03 FILLER     PIC X(20) VALUE "|------------------|".
             03 FILLER     PIC X(9) VALUE "----|----".
             03 FILLER     PIC X(20) VALUE "|------------------|".
             03 FILLER     PIC X(5) VALUE "----|".
          02 TIT-01.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RUBRO 1 - INFORMAC".
             03 FILLER     PIC X(20) VALUE "ION SOBRE VENTAS Y/O".
             03 FILLER     PIC X(20) VALUE " INGRESOS          |".
             03 FILLER     PIC X(4) VALUE " C".
             03 FILLER     PIC X(20) VALUE "|    COLUMNA I     |".
             03 FILLER     PIC X(9) VALUE " D  | C  ".
             03 FILLER     PIC X(20) VALUE "|    COLUMNA II    |".
             03 FILLER     PIC X(5) VALUE " D  |".
          02 TIT-02.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RUBRO 2 - INFORMAC".
             03 FILLER     PIC X(20) VALUE "ION SOBRE COMPRAS   ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " C".
             03 FILLER     PIC X(20) VALUE "|    COLUMNA I     |".
             03 FILLER     PIC X(9) VALUE " D  | C  ".
             03 FILLER     PIC X(20) VALUE "|    COLUMNA II    |".
             03 FILLER     PIC X(5) VALUE " D  |".
          02 TIT-03.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RUBRO 3 - DETERMIN".
             03 FILLER     PIC X(20) VALUE "ACION DEL IMPUESTO D".
             03 FILLER     PIC X(20) VALUE "EL PERIODO         |".
             03 FILLER     PIC X(4) VALUE " C".
             03 FILLER     PIC X(20) VALUE "| (a favor resp.)  |".
             03 FILLER     PIC X(9) VALUE " D  | C  ".
             03 FILLER     PIC X(20) VALUE "| (a favor D.G.I.) |".
             03 FILLER     PIC X(5) VALUE " D  |".
          02 TIT-04.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RUBRO 4 - CONCEPTO".
             03 FILLER     PIC X(20) VALUE "S INGRESADOS A CUENT".
             03 FILLER     PIC X(20) VALUE "A                  |".
             03 FILLER     PIC X(4) VALUE " C".
             03 FILLER     PIC X(20) VALUE "| (a favor resp.)  |".
             03 FILLER     PIC X(9) VALUE " D  | C  ".
             03 FILLER     PIC X(20) VALUE "| (a favor D.G.I.) |".
             03 FILLER     PIC X(5) VALUE " D  |".
          02 TIT-05.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RUBRO 5 - DETERMIN".
             03 FILLER     PIC X(20) VALUE "ACION DEL SALDO DE I".
             03 FILLER     PIC X(20) VALUE "MPUESTO A INGRESAR |".
             03 FILLER     PIC X(4) VALUE " C".
             03 FILLER     PIC X(20) VALUE "| (a favor resp.)  |".
             03 FILLER     PIC X(9) VALUE " D  | C  ".
             03 FILLER     PIC X(20) VALUE "| (a favor D.G.I.) |".
             03 FILLER     PIC X(5) VALUE " D  |".
          02 IMP-01.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| VENTAS,LOCAC.,PRES".
             03 FILLER     PIC X(20) VALUE "T.EXENTAS Y NO GRAVA".
             03 FILLER     PIC X(20) VALUE "DAS                |".
             03 FILLER     PIC X(4) VALUE " 13".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-1-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 2  | 48 | ".
             03 VAL-2-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 5  |".
          02 IMP-02.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| EXPORTACIONES     ".
             03 FILLER     PIC X(20) VALUE "                    ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 14".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-3-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 0  | 21 | ".
             03 VAL-4-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 3  |".
          02 IMP-03.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SUBTOTALES COL.I y".
             03 FILLER     PIC X(20) VALUE " COL.II             ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 16".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-5-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 7  | 41 | ".
             03 VAL-6-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 8  |".
          02 IMP-04.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DIFERENCIA (Cod.16".
             03 FILLER     PIC X(20) VALUE "-Cod.41)            ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 20".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-7-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 5  | 42 | ".
             03 VAL-8-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 6  |".
          02 IMP-05.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| COMPRAS EXENTAS Y ".
             03 FILLER     PIC X(20) VALUE "NO GRAVADAS         ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 22".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-9-E    PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 1  | 43 | ".
             03 VAL-10-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 4  |".
          02 IMP-06.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| COMPRAS DE BIENES ".
             03 FILLER     PIC X(20) VALUE "DE USO              ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 24".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-11-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 8  | 97 | ".
             03 VAL-12-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 3  |".
          02 IMP-07.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| IMPORTACIONES GRAV".
             03 FILLER     PIC X(20) VALUE "ADAS                ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 28".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-13-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 0  | 50 | ".
             03 VAL-14-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 7  |".
          02 IMP-08.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SUBTOTALES COL.I y".
             03 FILLER     PIC X(20) VALUE "COL.II              ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 25".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-15-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 6  | 51 | ".
             03 VAL-16-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 5  |".
          02 IMP-09.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DIFERENCIA (Cod.25".
             03 FILLER     PIC X(20) VALUE "-Cod.51)            ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 47".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-17-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 7  | 52 | ".
             03 VAL-18-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 3  |".         
          02 IMP-10.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DEBITO FISCAL-TASA".
             03 FILLER     PIC X(9) VALUE " GENERAL ".
             03 TASA-1-E   PIC ZZ9,99.
             03 FILLER     PIC X(5) VALUE " %   ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 70 | ".
             03 VAL-19-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 1  |".
          02 IMP-11.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DEBITO FISCAL-RESP".
             03 FILLER     PIC X(20) VALUE ".INSCRIPTOS-Tasa Dif".
             03 FILLER     PIC X(9) VALUE "erencial ".
             03 TASA-2-E   PIC ZZ9,99.
             03 FILLER     PIC X(5) VALUE " %  |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 30 | ".
             03 VAL-20-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 2  |".
          02 IMP-12.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DEBITO FISCAL CONS".
             03 FILLER     PIC X(20) VALUE "UMIDORES FINALES - T".
             03 FILLER     PIC X(9) VALUE "asa      ".
             03 TASA-3-E   PIC ZZ9,99.
             03 FILLER     PIC X(5) VALUE " %  |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 31 | ".
             03 VAL-21-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 0  |".
          02 IMP-13.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DEBITO FISCAL RESP".
             03 FILLER     PIC X(20) VALUE ".NO INSCRIPTOS - Tas".
             03 FILLER     PIC X(9) VALUE "a ".
             03 TASA-4-E   PIC ZZ9,99.
             03 FILLER     PIC X(5) VALUE " %  |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 44 | ".
             03 VAL-22-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 2  |".
          02 IMP-14.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DEBITO FISCAL RESP".
             03 FILLER     PIC X(20) VALUE ".NO INSCRIPTOS - Tas".
             03 FILLER     PIC X(9) VALUE "a".
             03 TASA-5-E   PIC ZZ9,99.
             03 FILLER     PIC X(5) VALUE " %  |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 11 | ".
             03 VAL-23-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 6  |".
          02 IMP-15.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| OPERAC.CON TRAT.PR".
             03 FILLER     PIC X(20) VALUE "EFERENCIAL EXCEPTO D".
             03 FILLER     PIC X(20) VALUE "IFERIMIENTOS       |".  
             03 FILLER     PIC X(4) VALUE " 61 ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-24-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 2  | 72 | ".
             03 VAL-25-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 8  |".
          02 IMP-16.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| CREDITO FISCAL - T".
             03 FILLER     PIC X(14) VALUE "asa General ".
             03 TASA-6-E   PIC ZZ9,99.
             03 FILLER     PIC X(20) VALUE " %                 |".
             03 FILLER     PIC X(4) VALUE " 60 ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-26-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 4  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-17.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| CREDITO FISCAL - T".
             03 FILLER     PIC X(14) VALUE "asa Diferen.".
             03 TASA-7-E   PIC ZZ9,99.
             03 FILLER     PIC X(20) VALUE " %                 |".
             03 FILLER     PIC X(4) VALUE " 26 ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-27-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 4  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-18.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| AJUSTE ANUAL Art.1".
             03 FILLER     PIC X(20) VALUE "2-2do parrafo.Ley   ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 83".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-28-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 3  | 84 | ".
             03 VAL-29-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 1  |".
          02 IMP-19.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| CREDITO FISCAL VIN".
             03 FILLER     PIC X(20) VALUE "C.A EXPORT.-AFECT.A ".
             03 FILLER     PIC X(20) VALUE "ACRED.DEVOL.O TRANS|".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 45 | ".
             03 VAL-30-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 0  |".          
          02 IMP-20.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SALDO A FAVOR PERI".
             03 FILLER     PIC X(20) VALUE "ODO ANTERIOR Art.20-".
             03 FILLER     PIC X(20) VALUE "1er.parrafo.Ley    |".
             03 FILLER     PIC X(4) VALUE " 65".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-31-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 5  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-21.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SUBTOTAL   COL.I y".
             03 FILLER     PIC X(20) VALUE "COL.II              ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 71".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-32-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 8  | 77 | ".
             03 VAL-33-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 9  |".
          02 IMP-22.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DIFERENCIA entre l".
             03 FILLER     PIC X(20) VALUE "a suma de la Col.I y".
             03 FILLER     PIC X(20) VALUE " la suma de Col.II |".
             03 FILLER     PIC X(4) VALUE " 81".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-34-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 7  | 82 | ".
             03 VAL-35-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 5  |".
          02 IMP-23.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SALDO A FAVOR DEL ".
             03 FILLER     PIC X(20) VALUE "PERIODO ANTERIOR Art".
             03 FILLER     PIC X(20) VALUE ".20-2do parrafo    |".
             03 FILLER     PIC X(4) VALUE " 01".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-35B-E  PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 9  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-24.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| MONTO UTILIZADO   ".
             03 FILLER     PIC X(20) VALUE "                    ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE "   ".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(13) VALUE " |    | 02 | ".
             03 VAL-36-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 7  |".
          02 IMP-25.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SALDO A FAVOR REST".
             03 FILLER     PIC X(20) VALUE "ANTE                ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 03".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-37-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 5  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-26.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| RETENCIONES Y PERC".
             03 FILLER     PIC X(20) VALUE "EPCIONES            ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 80".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-38-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 9  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-27.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| PAGOS A CUENTA S/R".
             03 FILLER     PIC X(20) VALUE "G Nro.              ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 12".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-39-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 4  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-28.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| TOTAL(Cod.03+80+12".
             03 FILLER     PIC X(20) VALUE ")                   ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 17".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-40-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 5  |    | ".
             03 FILLER     PIC X(16) VALUE SPACES.
             03 FILLER     PIC X(7) VALUE " |    |".
          02 IMP-29.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| TRANSPORTE R.3 Cod".
             03 FILLER     PIC X(20) VALUE ". 82                ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " **".
             03 FILLER     PIC X(2) VALUE "| ".
             03 FILLER     PIC X(16) VALUE ALL "*".
             03 FILLER     PIC X(13) VALUE " | ** | 15 | ".
             03 VAL-41-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 9  |".
          02 IMP-30.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| TRANSPORTE R.4 Cod".
             03 FILLER     PIC X(20) VALUE ". 17                ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 73".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-42-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 6  | ** | ".
             03 FILLER     PIC X(16) VALUE ALL "*".
             03 FILLER     PIC X(7) VALUE " | ** |".
          02 IMP-31.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DIFERENCIA ENTRE C".
             03 FILLER     PIC X(20) VALUE "od.15 y Cod.73      ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 67".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-43-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 1  | 66 | ".
             03 VAL-44-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 3  |".
          02 IMP-32.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| IMPUESTO RESPONSAB".
             03 FILLER     PIC X(20) VALUE "LES NO INSCRIPTOS(Ar".
             03 FILLER     PIC X(20) VALUE "t.II Titulo V.Ley  |".
             03 FILLER     PIC X(4) VALUE " 07".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-45-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 8  | 04 | ".
             03 VAL-46-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 3  |".
          02 IMP-33.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| SUBTOTAL COLUMNA I".
             03 FILLER     PIC X(20) VALUE " COLUMNA II         ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 53".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-47-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 4  | 75 | ".
             03 VAL-48-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 9  |".
          02 IMP-34.
             03 FILLER     PIC X(5) VALUE SPACES.
             03 FILLER     PIC X(20) VALUE "| DIFERENCIA entre C".
             03 FILLER     PIC X(20) VALUE "odigo 75 y 53       ".
             03 FILLER     PIC X(20) VALUE "                   |".
             03 FILLER     PIC X(4) VALUE " 86".
             03 FILLER     PIC X(2) VALUE "| ".
             03 VAL-49-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(13) VALUE " | 8  | 87 | ".
             03 VAL-50-E   PIC -----.---.--9,99.
             03 FILLER     PIC X(7) VALUE " | 6  |".
       01 CAMPOS-AUXILIARES.
          02 IVA-1-INT    PIC 999V99 VALUE 0.
          02 IVA-2-INT    PIC 999V99 VALUE 0.
          02 IVA-3-INT    PIC 999V99 VALUE 0.
          02 ALI-INT      PIC 999 VALUE 0.
          02 CONT-P-AUX   PIC 999 VALUE 0.
          02 LIS          PIC X VALUE SPACES.
          02 NETO-T       PIC S9(6)V99 VALUE 0.
          02 NO-GRA-T     PIC S9(6)V99 VALUE 0.
          02 EXE-T        PIC S9(6)V99 VALUE 0.
          02 RET-T        PIC S9(6)V99 VALUE 0.
          02 IMP-1-T      PIC S9(6)V99 VALUE 0.
          02 IMP-2-T      PIC S9(6)V99 VALUE 0.
          02 TOT-T        PIC S9(6)V99 VALUE 0.
          02 PER-T        PIC S9(6)V99 VALUE 0.
          02 CRE-DEB-T    PIC S9(6)V99 VALUE 0.
          02 TOT-CAP      PIC S9(6)V99 VALUE 0.
          02 LIB-CAP      PIC 9 VALUE 0.
          02 COD-IMP      PIC X VALUE SPACES.
          02 ANO-INT      PIC 99 VALUE 0.
          02 MES-INT      PIC 99 VALUE 0.
          02 DIA-INT      PIC 99 VALUE 0.
          02 FECHA-H-C.
             03 DIA-H-C   PIC 99 VALUE 0.
             03 MES-H-C   PIC 99 VALUE 0.
             03 ANO-H-C   PIC 99 VALUE 0.
          02 FECHA-H.
             03 ANO-H     PIC 99 VALUE 0.
             03 MES-H     PIC 99 VALUE 0.
             03 DIA-H     PIC 99 VALUE 0.
          02 FECHA-CAP.
             03 MES-CAP   PIC 99 VALUE 0.
             03 ANO-CAP   PIC 99 VALUE 0. 
          02 D-IVA-EDI    PIC X(10).  
          02 IVA-H        PIC 9 VALUE 0.
          02 XX           PIC X.
          02 IVA-TOT-CAL  PIC 999V99 VALUE 0.
          02 IVA-1-CAL    PIC S9(10)V99 VALUE 0.
          02 IVA-2-CAL    PIC S9(10)V99 VALUE 0.
          02 VAL-1        PIC S9(10)V99 VALUE 0.
          02 VAL-2        PIC S9(10)V99 VALUE 0.
          02 VAL-3        PIC S9(10)V99 VALUE 0.
          02 VAL-4        PIC S9(10)V99 VALUE 0.
          02 VAL-5        PIC S9(10)V99 VALUE 0.
          02 VAL-6        PIC S9(10)V99 VALUE 0.
          02 VAL-7        PIC S9(10)V99 VALUE 0.
          02 VAL-8        PIC S9(10)V99 VALUE 0.
          02 VAL-9        PIC S9(10)V99 VALUE 0.
          02 VAL-10       PIC S9(10)V99 VALUE 0.
          02 VAL-11       PIC S9(10)V99 VALUE 0.
          02 VAL-12       PIC S9(10)V99 VALUE 0.
          02 VAL-13       PIC S9(10)V99 VALUE 0.
          02 VAL-14       PIC S9(10)V99 VALUE 0.
          02 VAL-15       PIC S9(10)V99 VALUE 0.
          02 VAL-16       PIC S9(10)V99 VALUE 0.
          02 VAL-17       PIC S9(10)V99 VALUE 0.
          02 VAL-18       PIC S9(10)V99 VALUE 0.
          02 VAL-19       PIC S9(10)V99 VALUE 0.
          02 VAL-20       PIC S9(10)V99 VALUE 0.
          02 VAL-21       PIC S9(10)V99 VALUE 0.
          02 VAL-22       PIC S9(10)V99 VALUE 0.
          02 VAL-23       PIC S9(10)V99 VALUE 0.
          02 VAL-24       PIC S9(10)V99 VALUE 0.
          02 VAL-25       PIC S9(10)V99 VALUE 0.
          02 VAL-26       PIC S9(10)V99 VALUE 0.
          02 VAL-27       PIC S9(10)V99 VALUE 0.
          02 VAL-28       PIC S9(10)V99 VALUE 0.
          02 VAL-29       PIC S9(10)V99 VALUE 0.
          02 VAL-30       PIC S9(10)V99 VALUE 0.
          02 VAL-31       PIC S9(10)V99 VALUE 0.
          02 VAL-32       PIC S9(10)V99 VALUE 0.
          02 VAL-33       PIC S9(10)V99 VALUE 0.
          02 VAL-34       PIC S9(10)V99 VALUE 0.
          02 VAL-35       PIC S9(10)V99 VALUE 0.
          02 VAL-35B      PIC S9(10)V99 VALUE 0.
          02 VAL-36       PIC S9(10)V99 VALUE 0.
          02 VAL-37       PIC S9(10)V99 VALUE 0.
          02 VAL-38       PIC S9(10)V99 VALUE 0.
          02 VAL-39       PIC S9(10)V99 VALUE 0.
          02 VAL-40       PIC S9(10)V99 VALUE 0.
          02 VAL-41       PIC S9(10)V99 VALUE 0.
          02 VAL-42       PIC S9(10)V99 VALUE 0.
          02 VAL-43       PIC S9(10)V99 VALUE 0.
          02 VAL-44       PIC S9(10)V99 VALUE 0.
          02 VAL-45       PIC S9(10)V99 VALUE 0.
          02 VAL-46       PIC S9(10)V99 VALUE 0.
          02 VAL-47       PIC S9(10)V99 VALUE 0.
          02 VAL-48       PIC S9(10)V99 VALUE 0.
          02 VAL-49       PIC S9(10)V99 VALUE 0.
          02 VAL-50       PIC S9(10)V99 VALUE 0.
          02 TASA-1       PIC 999V99 VALUE 0.
          02 TASA-2       PIC 999V99 VALUE 0.
          02 TASA-3       PIC 999V99 VALUE 0.
          02 TASA-4       PIC 999V99 VALUE 0.
          02 TASA-5       PIC 999V99 VALUE 0.
          02 TASA-6       PIC 999V99 VALUE 0.
          02 TASA-7       PIC 999V99 VALUE 0. 
          02 IVA-1-GEN    PIC 99V99 VALUE 0.
          02 IVA-2-GEN    PIC 99V99 VALUE 0.
          02 IVA-3-GEN    PIC 99V99 VALUE 0.
          02 CC           PIC X.
          02 QQ           PIC X.
       01 CAMPOS-DE-EDICION.
          02 VAL-EDI      PIC -----.---.--9,99.
       COPY "VARIOS.WOR".    
       COPY "CLAVES.WOR".
       COPY "ABREV.WOR".
       PROCEDURE DIVISION.
       DECLARATIVES.
      *...............................(*.ERR)
       COPY "MANEJO.ERR".
       COPY "IVA.ERR".
       COPY "ALICUO.ERR".
       COPY "EMPRE.ERR".
       COPY "CUENTA.ERR".
       COPY "IMP02.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
           PERFORM CONF-CAMPOS.
           MOVE 0 TO CLAVE-1-CAP.
           OPEN INPUT MANEJO.
       INI-MOD.
      *...............................(ABRE PANTALLA)
           MOVE "GCMODA" TO NOM-PAN.
           OPEN INPUT PANTA.
           READ PANTA.
           CLOSE PANTA.
           MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
           ADD 1 TO SW2.
           IF SW2 = 25 GO TO SIGA-CLAVE.
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
      *...............................(SELECCIONA TAREAS)
           MOVE 28 TO SW3.
           PERFORM BORRA.
           MOVE "S" TO COD-IMP.
           ACCEPT COD1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       ON EXCEPTION EXCEP GO TO TAREAS.
           GO TO CARGO-MOD.
       TAREAS.
           IF ESCAP EXIT PROGRAM
                    STOP RUN.
           IF F05 MOVE "S" TO COD-IMP
                  MOVE " LIST. F.468/B " TO D20-CAP
                  GO TO LISTO.
           GO TO CARGO-MOD.
       
      *................................(PROGRAMA PRINCIPAL)
       LISTO.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-1.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           PERFORM MEN-MODA-S.
           PERFORM MODA-PAN.
           OPEN INPUT EMPRESAS IVA ALICUO.
           OPEN OUTPUT IMP02.
           PERFORM CARGA-EMP THRU CARGA-EMP-F.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 GO TO INI-MOD.
           PERFORM CHEQUEA-CLAVE2.
           IF COD-CLAVE = 9 GO TO INI-MOD. 
           PERFORM CHEQUEA-CLAVE3.
           IF COD-CLAVE = 9 GO TO INI-MOD.
       CARGA-LIB.
           MOVE 1 TO IVA-IVA.
           MOVE "F.468/B - " TO TIT-2-EDI
           MOVE "DECLARACIO" TO TIT-3-EDI
           MOVE "N JURADA M" TO TIT-4-EDI
           MOVE "ENSUAL    " TO TIT-5-EDI
           MOVE SPACES TO TIT-1-EDI TIT-6-EDI.
           MOVE TITULO-EDI TO TIT-E132.
       CARGA-F-D.
           DISPLAY SPACES LINE 18 POSITION 30 SIZE 40.
           MOVE 9 TO SW3. 
           PERFORM BORRA.
           ACCEPT FECHA-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-LIB.
           IF MES-CAP = 0 OR MES-CAP > 12 GO TO CARGA-F-D.
           MOVE 0 TO DIA-IVA.
           MOVE MES-CAP TO MES-IVA MES-INT MES-ED.
           MOVE ANO-CAP TO ANO-IVA ANO-INT ANO-ED.
           MOVE 0 TO DIA-INT.
       SIGA-L.
           MOVE 0 TO VAL-1 VAL-2 VAL-3 VAL-4 VAL-5 VAL-6 VAL-7 VAL-8.
           MOVE 0 TO VAL-9 VAL-10 VAL-11 VAL-12 VAL-13 VAL-14 VAL-15.
           MOVE 0 TO VAL-16 VAL-17 VAL-18 VAL-19 VAL-20 VAL-21 VAL-22.
           MOVE 0 TO VAL-23 VAL-24 VAL-25 VAL-26 VAL-27 VAL-28 VAL-29.
           MOVE 0 TO VAL-30 VAL-31 VAL-32 VAL-33 VAL-34 VAL-35 VAL-35B.
           MOVE 0 TO VAL-36 VAL-37 VAL-38 VAL-39 VAL-40 VAL-41 VAL-42.
           MOVE 0 TO VAL-43 VAL-44 VAL-45 VAL-46 VAL-47 VAL-48 VAL-49.
           MOVE 0 TO VAL-50.
           MOVE 0 TO TASA-1 TASA-2 TASA-3 TASA-4 TASA-5 TASA-6 TASA-7.
           MOVE 0 TO DIA-INT CONT-P-AUX DIA-IVA.
           MOVE 0 TO MOV-IVA.
           MOVE 1 TO IVA-IVA.
           MOVE NUM-EMP TO EMP-IVA EMP-ALI.
           MOVE SUC-EMP TO SUC-IVA SUC-ALI.
           MOVE 1 TO ALI-ALI ALI-INT ALI-IVA.
           MOVE 31 TO DD-ALI DIA-VAL-IVA.
           MOVE MES-CAP TO MM-ALI MES-IVA MES-VAL-IVA.
           MOVE ANO-CAP TO AA-ALI ANO-IVA ANO-VAL-IVA.
           PERFORM VEO-ALI THRU VEO-ALI-F.
           MOVE IVA-1-ALI TO IVA-1-GEN.
           MOVE IVA-2-ALI TO IVA-2-GEN.
           MOVE IVA-3-ALI TO IVA-3-GEN.
           START IVA KEY IS NOT < CLAVE-IVA
                 INVALID KEY GO TO SIGA2.
       LISTO-1.
           READ IVA NEXT AT END GO TO SIGA2.
           IF EMP-IVA NOT = NUM-EMP GO TO SIGA2.
           IF SUC-IVA NOT = SUC-EMP GO TO SIGA2.
           IF ANO-IVA > ANO-CAP GO TO SIGA2.
           IF ANO-IVA = ANO-CAP
              IF MES-IVA > MES-CAP GO TO SIGA2.
           IF IVA-IVA NOT = 1 GO TO SIGA2.
           GO TO VENTAS.
       VENTAS.
      *=-=-=-=-= VENTAS EXENTAS Y NO GRAVADAS - RUBRO 1 =-=-=-=-=
           IF TIP-COM-IVA NOT = 4 
               IF ALI-IVA < 60 OR ALI-IVA > 69 
               COMPUTE VAL-1 = VAL-1 + NO-GRA-IVA + EXE-IVA.
           IF TIP-COM-IVA = 4 
             IF ALI-IVA < 60 OR ALI-IVA > 69
               COMPUTE VAL-2 = VAL-2 - NO-GRA-IVA - EXE-IVA.
           IF TIP-COM-IVA NOT = 4 
              IF ALI-IVA > 59 AND ALI-IVA < 70
               COMPUTE VAL-3 = VAL-3 + NO-GRA-IVA + EXE-IVA.
           IF TIP-COM-IVA = 4 
              IF ALI-IVA > 59 AND ALI-IVA < 70
               COMPUTE VAL-4 = VAL-4 - NO-GRA-IVA - EXE-IVA.
      *=-=-=-=-=-= DEBITOS FISCALES - RUBRO 3=-=-=-=-=-=-=-=-
           IF INS-IVA = "R"
                 MOVE ALI-IVA TO ALI-ALI ALI-INT
                 MOVE DIA-VAL-IVA TO DD-ALI
                 MOVE MES-VAL-IVA TO MM-ALI
                 MOVE ANO-VAL-IVA TO AA-ALI
                 PERFORM VEO-ALI THRU VEO-ALI-F.
           IF INS-IVA = "R"
                 IF IVA-1-ALI NOT = IVA-1-GEN
                    MOVE IVA-1-ALI TO TASA-2
                    COMPUTE VAL-20 = VAL-20 + CRE-DEB-IVA.
           IF INS-IVA = "R"
                 IF IVA-1-ALI = IVA-1-GEN      
                      MOVE IVA-1-GEN TO TASA-1
                      COMPUTE VAL-19 = VAL-19 + CRE-DEB-IVA.
           IF INS-IVA = "C"
              MOVE IVA-1-GEN TO TASA-3
              COMPUTE VAL-21 = VAL-21 + CRE-DEB-IVA.
           IF INS-IVA = "N" 
              MOVE ALI-IVA TO ALI-ALI ALI-INT
              MOVE MES-VAL-IVA TO MM-ALI
              MOVE ANO-VAL-IVA TO AA-ALI
              MOVE DIA-VAL-IVA TO DD-ALI
              PERFORM VEO-ALI THRU VEO-ALI-F.
           IF INS-IVA = "N" AND ALI-IVA = 1
              MOVE IVA-1-GEN TO TASA-4
              COMPUTE IVA-TOT-CAL = IVA-1-ALI + IVA-2-ALI
              COMPUTE IVA-1-CAL ROUNDED = CRE-DEB-IVA / IVA-TOT-CAL
                            * IVA-1-ALI
              COMPUTE IVA-2-CAL ROUNDED = CRE-DEB-IVA / IVA-TOT-CAL
                            * IVA-2-ALI

      ****  MODIFICACIONES POR EL CASO DE COMBUSTIBLES         
      
                    MOVE IMP-1-IVA TO IVA-1-CAL
                    MOVE IMP-2-IVA TO IVA-2-CAL
      ****   --------------------------------------------
              COMPUTE VAL-22 = VAL-22 + IVA-1-CAL
              COMPUTE VAL-46 = VAL-46 + IVA-2-CAL.
           IF INS-IVA = "N" AND ALI-IVA NOT = 1
              COMPUTE IVA-TOT-CAL = IVA-1-ALI + IVA-2-ALI
              COMPUTE IVA-1-CAL ROUNDED = CRE-DEB-IVA / IVA-TOT-CAL
                              * IVA-1-ALI
              COMPUTE IVA-2-CAL ROUNDED = CRE-DEB-IVA / IVA-TOT-CAL
                              * IVA-2-ALI
              IF IVA-1-ALI NOT = IVA-1-GEN
                MOVE IVA-1-ALI TO TASA-5
                COMPUTE VAL-23 = VAL-23 + IVA-1-CAL
                COMPUTE VAL-46 = VAL-46 + IVA-2-CAL
              ELSE MOVE IVA-1-GEN TO TASA-4
                COMPUTE VAL-22 = VAL-22 + IVA-1-CAL
                COMPUTE VAL-46 = VAL-46 + IVA-2-CAL.
      *=-=-=-=-=-=-= OPER.TRAT.PREFERENCIAL - RUBRO 3 =-=-=-=
           MOVE 0 TO VAL-24 VAL-25.
      *=-=-=-=-=-=-= CRED.FISCAL VINC.A EXPORTAC - RUBRO 3 =-=-=-=
           MOVE 0 TO VAL-30.
      *=-=-=-=-= RETENCIONES DGI =-=-=-=-=-=-=-=-=-=-=-=
           IF ALI-IVA = 100 
              COMPUTE VAL-38 = VAL-38 - RET-IVA.
      *=-=-=-=-= CORTA ANALISIS VENTAS =-=-=-=-=-=-=-=-=-=-=
           GO TO LISTO-1.
       SIGA2.
           MOVE 0 TO DIA-INT CONT-P-AUX DIA-IVA.
           MOVE 0 TO MOV-IVA.
           MOVE 2 TO IVA-IVA.
           MOVE NUM-EMP TO EMP-IVA.
           MOVE SUC-EMP TO SUC-IVA.
           MOVE 1 TO ALI-ALI ALI-INT.
           MOVE 0 TO DD-ALI.
           MOVE MES-CAP TO MM-ALI MES-IVA.
           MOVE ANO-CAP TO AA-ALI ANO-IVA.
           START IVA KEY IS NOT < CLAVE-IVA
                 INVALID KEY GO TO SIGA3.
       LISTO-2.
           READ IVA NEXT AT END GO TO SIGA3.
           IF EMP-IVA NOT = NUM-EMP GO TO SIGA3.
           IF SUC-IVA NOT = SUC-EMP GO TO SIGA3.
           IF ANO-IVA > ANO-CAP GO TO SIGA3.
           IF ANO-IVA = ANO-CAP
              IF MES-IVA > MES-CAP GO TO SIGA3.
           IF IVA-IVA NOT = 2 GO TO SIGA3.
           GO TO COMPRAS.
       SIGA3.       
           MOVE 0 TO DIA-INT CONT-P-AUX DIA-IVA.
           MOVE 0 TO MOV-IVA.
           MOVE 3 TO IVA-IVA.
           MOVE NUM-EMP TO EMP-IVA EMP-ALI.
           MOVE SUC-EMP TO SUC-IVA SUC-ALI.
           MOVE 1 TO ALI-ALI ALI-INT.
           MOVE 0 TO DD-ALI.
           MOVE MES-CAP TO MM-ALI MES-IVA.
           MOVE ANO-CAP TO AA-ALI ANO-IVA.
           START IVA KEY IS NOT < CLAVE-IVA
                 INVALID KEY GO TO CIERRO.
       LISTO-3.
           READ IVA NEXT AT END GO TO CIERRO.
           IF EMP-IVA NOT = NUM-EMP GO TO CIERRO.
           IF SUC-IVA NOT = SUC-EMP GO TO CIERRO.
           IF ANO-IVA > ANO-CAP GO TO CIERRO.
           IF ANO-IVA = ANO-CAP
              IF MES-IVA > MES-CAP GO TO CIERRO.
           IF IVA-IVA NOT = 3 GO TO CIERRO.
           GO TO COMPRAS.        
       COMPRAS.
      *=-=-=-=-= COMPRAS EXENTAS Y NO GRAVADAS - RUBRO 2 =-=-=-
           IF ALI-IVA < 50 OR ALI-IVA > 69 
              IF TIP-COM-IVA NOT = 4 
               IF TIP-COM-IVA NOT = 9 AND INS-IVA NOT = "N" 
                COMPUTE VAL-9 = VAL-9 + EXE-IVA + NO-GRA-IVA.
           IF ALI-IVA < 50 OR ALI-IVA > 69 
             IF TIP-COM-IVA = 4 
              COMPUTE VAL-10 = VAL-10 - EXE-IVA - NO-GRA-IVA.
      *=-=-=-=== BIENES DE USO =-=-=-=-=-=-=-=-=-=-=-=-=
           IF ALI-IVA > 49 AND ALI-IVA < 60
             IF TIP-COM-IVA NOT = 4
               COMPUTE VAL-11 = VAL-11 + NETO-IVA + EXE-IVA 
                   + NO-GRA-IVA
             ELSE COMPUTE VAL-12 = VAL-12 - NETO-IVA - EXE-IVA
                   - NO-GRA-IVA.
      *=-=-=-=-=-=-= IMPORTACIONES =-=-=-=-=-=-=-=--=-=--=
           IF ALI-IVA > 59 AND ALI-IVA < 70
             IF TIP-COM-IVA NOT = 4
               COMPUTE VAL-13 = VAL-13 + NETO-IVA + EXE-IVA
                   + NO-GRA-IVA
             ELSE COMPUTE VAL-14 = VAL-14 - NETO-IVA - EXE-IVA
                   - NO-GRA-IVA.
      *=-=-=-=-=-=-= CREDITO FISCAL - RUBRO 3 =-=-=-=-=-=-=-=
           MOVE ALI-IVA TO ALI-ALI ALI-INT
           MOVE DIA-VAL-IVA TO DD-ALI
           MOVE MES-VAL-IVA TO MM-ALI
           MOVE ANO-VAL-IVA TO AA-ALI
           PERFORM VEO-ALI THRU VEO-ALI-F.
           IF IVA-1-ALI NOT = IVA-1-GEN
              MOVE IVA-1-ALI TO TASA-7
              COMPUTE VAL-27 = VAL-27 + CRE-DEB-IVA.
           IF IVA-1-ALI = IVA-1-GEN
              MOVE IVA-1-ALI TO TASA-6
              COMPUTE VAL-26 = VAL-26 + CRE-DEB-IVA.
      *=-=-=-=-=--=-= PERCEPCIONES - RUBRO 4 =-=-=-=-=-=-=-==
           COMPUTE VAL-38 = VAL-38 + PER-IVA.                   
      *=-=-=--= CORTA ANALISIS COMPRAS =-=-=-=-=
           IF IVA-IVA = 2 GO TO LISTO-2
               ELSE GO TO LISTO-3.
       TITU.
           ADD 1 TO CONT-P.
           MOVE 0 TO CONT-R.
           MOVE CONT-P TO HOJ-E132.
           ACCEPT FECHA FROM DATE.
           MOVE ANO TO AA-E132.
           MOVE MES TO MM-E132.
           MOVE DIA TO DD-E132.
           ACCEPT TIEMPO FROM TIME.
           MOVE HORA TO HH-E132.
           MOVE MINU TO MI-E132.
           WRITE LIN-IMP02 FROM COMPRI AFTER 0.
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
           WRITE LIN-IMP02 FROM ROT132-3 AFTER 1
           WRITE LIN-IMP02 FROM ROT132-4 AFTER 1
           WRITE LIN-IMP02 FROM ROT132-5 AFTER 1
           WRITE LIN-IMP02 FROM ROT132-1 AFTER 1
           ADD 5 TO CONT-R.
           PERFORM ESPA.
       ESPA.
           WRITE LIN-IMP02 FROM ROT132-2 AFTER 1.
           ADD 1 TO CONT-R.
       CIERRO.
           PERFORM PANTA-2.
           PERFORM CAP-AJU THRU CAP-PAG.
      *=-=-=-=-=-=-= RUBRO 1 =-=-=-=-=-=-=-=-
           COMPUTE VAL-5 = VAL-1 + VAL-3.
           COMPUTE VAL-6 = VAL-2 + VAL-4.
           IF VAL-5 > VAL-6
             COMPUTE VAL-7 = VAL-5 - VAL-6
             MOVE 0 TO VAL-8
             ELSE COMPUTE VAL-8 = VAL-6 - VAL-5
                  MOVE 0 TO VAL-7.
      *=-=-=-=-=-=- RUBRO 2 =-=-=-=-=-=-=-=-=-=
           COMPUTE VAL-15 = VAL-9 + VAL-11 + VAL-13.
           COMPUTE VAL-16 = VAL-10 + VAL-12 + VAL-14.
           IF VAL-15 > VAL-16
            COMPUTE VAL-17 = VAL-15 - VAL-16
                    MOVE 0 TO VAL-18
            ELSE COMPUTE VAL-18 = VAL-16 - VAL-15
                    MOVE 0 TO VAL-17.
      *=-=-=-=-=-=- RUBRO 3 =-=-=-=-=-=-=-=-=-=-=-
           COMPUTE VAL-32 = VAL-24 + VAL-26 + VAL-27 + VAL-28 + VAL-31.
           COMPUTE VAL-33 = VAL-19 + VAL-20 + VAL-21 + VAL-22 +
                        VAL-23 + VAL-25 + VAL-29 + VAL-30.
           IF VAL-32 > VAL-33
             COMPUTE VAL-34 = VAL-32 - VAL-33
             MOVE 0 TO VAL-35
             ELSE COMPUTE VAL-35 = VAL-33 - VAL-32
                  MOVE 0 TO VAL-34.
      *=-=-=-=-=-== RUBRO 4 =-=-=-=-=-=-=-=-=-=-
           COMPUTE VAL-37 = VAL-35B - VAL-36.
           COMPUTE VAL-40 = VAL-37 + VAL-38 + VAL-39.
      *=-=-=-=-=-=- RUBRO 5 =-=-=-=-=-=-=-=-=-=-=
           MOVE VAL-35 TO VAL-41.
           MOVE VAL-40 TO VAL-42.
           IF VAL-41 > VAL-42
             COMPUTE VAL-44 = VAL-41 - VAL-42
             MOVE 0 TO VAL-43
             ELSE COMPUTE VAL-43 = VAL-42 - VAL-41
                  MOVE 0 TO VAL-44.
           IF VAL-46 < 0
                 MOVE VAL-46 TO VAL-45
                 MOVE 0 TO VAL-46
             ELSE MOVE 0 TO VAL-45.
           COMPUTE VAL-47 = VAL-45 + VAL-43.
           COMPUTE VAL-48 = VAL-44 + VAL-46.
           IF VAL-47 > VAL-48
             COMPUTE VAL-49 = VAL-47 - VAL-48
             MOVE 0 TO VAL-50
             ELSE COMPUTE VAL-50 = VAL-48 - VAL-47
                  MOVE 0 TO VAL-49.
           PERFORM TITU.
           PERFORM IMPRE-1.
           PERFORM TITU.
           PERFORM IMPRE-2.
           CLOSE EMPRESAS ALICUO IVA IMP02.
           GO TO LISTO.
       IMPRE-1.
           MOVE VAL-1 TO VAL-1-E.
           MOVE VAL-2 TO VAL-2-E.
           MOVE VAL-3 TO VAL-3-E.
           MOVE VAL-4 TO VAL-4-E.
           MOVE VAL-5 TO VAL-5-E.
           MOVE VAL-6 TO VAL-6-E.
           MOVE VAL-7 TO VAL-7-E.
           MOVE VAL-8 TO VAL-8-E.
           MOVE VAL-9 TO VAL-9-E.
           MOVE VAL-10 TO VAL-10-E.
           MOVE VAL-11 TO VAL-11-E.
           MOVE VAL-12 TO VAL-12-E.
           MOVE VAL-13 TO VAL-13-E.
           MOVE VAL-14 TO VAL-14-E.
           MOVE VAL-15 TO VAL-15-E.
           MOVE VAL-16 TO VAL-16-E.
           MOVE VAL-17 TO VAL-17-E.
           MOVE VAL-18 TO VAL-18-E.
           MOVE VAL-19 TO VAL-19-E.
           MOVE VAL-20 TO VAL-20-E.
           MOVE VAL-21 TO VAL-21-E.
           MOVE VAL-22 TO VAL-22-E.
           MOVE VAL-23 TO VAL-23-E.
           MOVE VAL-24 TO VAL-24-E.
           MOVE VAL-25 TO VAL-25-E.
           MOVE VAL-26 TO VAL-26-E.
           MOVE VAL-27 TO VAL-27-E.
           MOVE VAL-28 TO VAL-28-E.
           MOVE VAL-29 TO VAL-29-E.
           MOVE VAL-30 TO VAL-30-E.
           MOVE VAL-31 TO VAL-31-E.
           MOVE VAL-32 TO VAL-32-E.
           MOVE VAL-33 TO VAL-33-E.
           MOVE VAL-34 TO VAL-34-E.
           MOVE VAL-35 TO VAL-35-E.
           MOVE VAL-36 TO VAL-36-E.
           MOVE VAL-37 TO VAL-37-E.
           MOVE VAL-38 TO VAL-38-E.
           MOVE VAL-39 TO VAL-39-E.
           MOVE VAL-35B TO VAL-35B-E.
           MOVE VAL-40 TO VAL-40-E.
           MOVE VAL-41 TO VAL-41-E.
           MOVE VAL-42 TO VAL-42-E.
           MOVE VAL-43 TO VAL-43-E.
           MOVE VAL-44 TO VAL-44-E.
           MOVE VAL-45 TO VAL-45-E.
           MOVE VAL-46 TO VAL-46-E.
           MOVE VAL-47 TO VAL-47-E.
           MOVE VAL-48 TO VAL-48-E.
           MOVE VAL-49 TO VAL-49-E.
           MOVE VAL-50 TO VAL-50-E.
           MOVE TASA-1 TO TASA-1-E.
           MOVE TASA-2 TO TASA-2-E.
           MOVE TASA-3 TO TASA-3-E.
           MOVE TASA-4 TO TASA-4-E.
           MOVE TASA-5 TO TASA-5-E.
           MOVE TASA-6 TO TASA-6-E.
           MOVE TASA-7 TO TASA-7-E.
           WRITE LIN-IMP02 FROM TIT AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-01 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-01 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-03 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-04 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-02 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-05 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.           
           WRITE LIN-IMP02 FROM IMP-06 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.      
           WRITE LIN-IMP02 FROM IMP-07 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-08 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-09 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-03 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.                      
           WRITE LIN-IMP02 FROM IMP-10 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-11 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-12 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-13 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-14 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-15 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-16 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-17 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-18 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-19 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-20 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.      
           WRITE LIN-IMP02 FROM IMP-21 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-22 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           ADD 54 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R
           PERFORM ESPA CONT-R TIMES.
       IMPRE-2.
           WRITE LIN-IMP02 FROM TIT.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-04 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.           
           WRITE LIN-IMP02 FROM IMP-23 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-24 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-25 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-26 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-27 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-28 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           PERFORM ESPA.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-05 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.        
           WRITE LIN-IMP02 FROM IMP-29 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.     
           WRITE LIN-IMP02 FROM IMP-30 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-31 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-32 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-33 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-00 AFTER 1.
           WRITE LIN-IMP02 FROM IMP-34 AFTER 1.
           WRITE LIN-IMP02 FROM TIT-0 AFTER 1.
           ADD 31 TO CONT-R.
           COMPUTE CONT-R = 72 - CONT-R.
           PERFORM ESPA CONT-R TIMES.                                                 
       CARGA-EMP.
           PERFORM MEN-MODA-S.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           MOVE 4 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF NUM-CAP = 0 GO TO CARGA-EMP.
           MOVE NUM-CAP TO NUM-EMP EMP-CAP EMP-IVA MA-EMP.
           MOVE 0 TO SUC-EMP MA-SUC.
           READ EMPRESAS INVALID KEY GO TO CARGA-EMP.
           READ MANEJO INVALID KEY DISPLAY " ".
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO EMP-E132.
       CARGA-SUC.
           PERFORM MEN-ANTE.
           MOVE 5 TO SW3.
           PERFORM BORRA.
           PERFORM CAP-NUM.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-EMP.
           MOVE NUM-CAP TO SUC-EMP SUC-IVA NUM-SUC-E132.
           READ EMPRESAS INVALID KEY GO TO CARGA-SUC.
           MOVE 6 TO SW3.
           PERFORM BORRA.
           DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3).
           MOVE RAZ-SOC-EMP TO SUC-E132.
           PERFORM CHEQUEA-CLAVE1.
           IF COD-CLAVE = 9 PERFORM CIERRE-A GO TO INI-MOD.
       CARGA-EMP-F.
           EXIT.
       CIERRE-A.
           CLOSE IMP02 ALICUO IVA EMPRESAS.
       VEO-ALI.
           MOVE EMP-IVA TO EMP-ALI.
           MOVE SUC-IVA TO SUC-ALI.
           MOVE ALI-IVA TO ALI-ALI.
           MOVE 0 TO AA-ALI MM-ALI DD-ALI.
           MOVE 0 TO IVA-1-INT IVA-2-INT IVA-3-INT.
           START ALICUO KEY IS NOT < CLAVE-ALICUO
              INVALID KEY GO TO VEO-ALI-F.
       VEO-ALI1.
           READ ALICUO NEXT AT END GO TO VEO-ALI2.
           IF EMP-ALI > EMP-IVA GO TO VEO-ALI2.
           IF EMP-ALI < EMP-IVA GO TO VEO-ALI1.
           IF EMP-ALI = EMP-IVA
              IF SUC-ALI > SUC-IVA GO TO VEO-ALI2.
           IF ALI-ALI NOT = ALI-IVA GO TO VEO-ALI2.
           IF AA-ALI > ANO-VAL-IVA GO TO VEO-ALI2.
           IF AA-ALI = ANO-VAL-IVA
              IF MM-ALI > MES-VAL-IVA GO TO VEO-ALI2.
           IF AA-ALI = ANO-VAL-IVA
              IF MM-ALI = MES-VAL-IVA
                 IF DD-ALI > DIA-VAL-IVA GO TO VEO-ALI2.
           MOVE IVA-1-ALI TO IVA-1-INT.
           MOVE IVA-2-ALI TO IVA-2-INT.
           MOVE IVA-3-ALI TO IVA-3-INT.
           GO TO VEO-ALI1.
       VEO-ALI2.
           MOVE IVA-1-INT TO IVA-1-ALI.
           MOVE IVA-2-INT TO IVA-2-ALI.
           MOVE IVA-3-INT TO IVA-3-ALI.
           MOVE ALI-INT TO ALI-ALI.
       VEO-ALI-F.
           EXIT.               

      *................................(CARGA DATOS)
       CAP-AJU.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           MOVE 13 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-28 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CARGA-F-D.
           MOVE VAL-28 TO VAL-EDI.
           PERFORM DESP-VAL.
           IF VAL-28 NOT = 0 GO TO CAP-SAL-1.
           MOVE 14 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-29 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                      SIZE TAB-SIZ(SW3) NO BEEP TAB
                      ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CAP-AJU.
           MOVE VAL-29 TO VAL-EDI.
           PERFORM DESP-VAL.
       CAP-SAL-1.
           PERFORM MEN-ANTE.
           MOVE 15 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-31 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                   SIZE TAB-SIZ(SW3) NO BEEP TAB
                   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CAP-AJU.
           MOVE VAL-31 TO VAL-EDI.
           PERFORM DESP-VAL.
       CAP-SAL-2.
           MOVE 16 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-35B LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3) NO BEEP TAB
                    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CAP-SAL-1.
           MOVE VAL-35B TO VAL-EDI.
           PERFORM DESP-VAL.
       CAP-MON.
           MOVE 17 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-36 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                       SIZE TAB-SIZ(SW3) NO BEEP TAB
                       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CAP-SAL-2.
           MOVE VAL-36 TO VAL-EDI.
           PERFORM DESP-VAL.
       CAP-PAG.
           MOVE 18 TO SW3.
           PERFORM BORRA.
           ACCEPT VAL-39 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                     SIZE TAB-SIZ(SW3) NO BEEP TAB
                     ON EXCEPTION EXCEP MOVE SW3 TO SW3.
           IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
           IF F02 GO TO CAP-MON.
           MOVE VAL-39 TO VAL-EDI.
           PERFORM DESP-VAL.
      *................................(DESPLEGA DATOS)
       DESP-VAL.
           DISPLAY VAL-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
                    SIZE TAB-SIZ(SW3).
       CONF-CAMPOS.
      *................................(CONFIGURO CAMPOS)
           MOVE 045125 TO TAB-POS(1).
           MOVE 250275 TO TAB-POS(2).
           MOVE 102004 TO TAB-POS(3).
           MOVE 071604 TO TAB-POS(4).
           MOVE 073002 TO TAB-POS(5).
           MOVE 073730 TO TAB-POS(6).
           MOVE 101601 TO TAB-POS(7).
           MOVE 102010 TO TAB-POS(8).
           MOVE 102804 TO TAB-POS(9).
           MOVE 162806 TO TAB-POS(10).
           MOVE 182801 TO TAB-POS(11).
           MOVE 202806 TO TAB-POS(12).
           MOVE 163516 TO TAB-POS(13).
           MOVE 165516 TO TAB-POS(14).
           MOVE 173516 TO TAB-POS(15).
           MOVE 193516 TO TAB-POS(16).
           MOVE 205516 TO TAB-POS(17).
           MOVE 213516 TO TAB-POS(18).
           MOVE 254101 TO TAB-POS(28).
       TIT-PAN.
      *................................(PANTALLAS)
           MOVE " - ARCHIVO" TO TIT-1-PAN.
           MOVE " DE  IVA -" TO TIT-2-PAN.
           MOVE "          " TO TIT-3-PAN.
       PANTA-1.
           DISPLAY "EMPRESA  :           SUC:       -" LINE 7 POSITION 5.
           DISPLAY "PERIODO FISCAL<MMAA> : " LINE 10 POSITION 5.
       PANTA-2.
           DISPLAY " - INGRESAR VALORES - " LINE 12 POSITION 28 REVERSE.
           DISPLAY "-------- DETALLE ------------" LINE 14 POSITION 5.
           DISPLAY "-(a favor respons.)-" LINE 14 POSITION 35.
           DISPLAY "-(a favor D.G.I.)-" LINE 14 POSITION 55.
           DISPLAY "   ***  RUBRO 3 ***          " LINE 15 POSITION 5.
           DISPLAY "AJUSTE ANUAL Art.12 2do par. " LINE 16 POSITION 5.
           DISPLAY "SALDO A FAV.P/ANT.Art.20 1er " LINE 17 POSITION 5.
           DISPLAY "   ***  RUBRO 4 ***          " LINE 18 POSITION 5.
           DISPLAY "SALDO A FAV.P/ANT.Art.20 2do " LINE 19 POSITION 5.
           DISPLAY "MONTO UTILIZADO              " LINE 20 POSITION 5.
           DISPLAY "PAGOS A CUENTA S/R.G.        " LINE 21 POSITION 5.
       OPC-MODA.
           DISPLAY "<F5>  LISTADO F.468/B  " LINE 14 POSITION 18.
           DISPLAY "<ESC> VUELVE AL MENU" LINE 15 POSITION 18.
       BORRA-PANTA.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 6 BY 1
                            UNTIL SW1 > 23.
       BORRA-P2.
           MOVE 0 TO SW1.
           PERFORM BORRA-P1 VARYING SW1 FROM 12 BY 1
                            UNTIL SW1 > 21.
       BORRA-P1.
           DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
      *................................(*.PRO)
           COPY "PANTA.PRO".
           COPY "ROT132.PRO".
           COPY "BORRA.PRO".
           COPY "MENSA.PRO".
           COPY "CAPTU.PRO".
           COPY "DESPLI.PRO".
           COPY "CLAVES.PRO".
