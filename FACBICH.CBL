       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTUCO.
       AUTHOR. ALVARO GUIFFREY.
       DATE-WRITTEN. NOV'91.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC-XT-AT.
       OBJECT-COMPUTER. PC-XT-AT.
       SPECIAL-NAMES.
	      DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "CUENTA.SEL".
       COPY "ALICUO.SEL".
       COPY "EMPRE.SEL".
       COPY "ART01.SEL".
       COPY "ART02.SEL".
       COPY "NUMERO.SEL".
       COPY "MARCAS.SEL".
       COPY "ENVASE.SEL".
       COPY "OPERA.SEL".
       COPY "COMPRO.SEL".
       COPY "PANTA.SEL".
       COPY "CONDI.SEL".
       COPY "MONE.SEL".
       COPY "COTIZA.SEL". 
       COPY "CPOSTAL.SEL".
       COPY "IVA.SEL".
       COPY "PENDI.SEL".
       COPY "MOVI.SEL".
       COPY "IMPU.SEL".
       COPY "ACTI.SEL".
       COPY "INSDIR.SEL".
       COPY "RES127.SEL".
      * COPY "IMP02.SEL".
	  SELECT IMP02 ASSIGN TO PRINT MIRTA
		FILE STATUS IS ERRORES.

       COPY "VENTURNO.SEL".
       COPY "LACAJA.SEL".
       COPY "TARJ.SEL".
       COPY "MANEJO.SEL".
       COPY "ARET.SEL".

       DATA DIVISION.
       FILE SECTION.
       COPY "CUENTABM.REG".
       COPY "ALICUO.REG".
       COPY "EMPREBM.REG".
       COPY "ART01.REG".
       COPY "ART02.REG".
       COPY "NUMERO.REG".
       COPY "MARCAS.REG".
       COPY "CPOSTAL.REG".
       COPY "ENVASE.REG".
       COPY "OPERA.REG".
       COPY "IVA.REG".
       COPY "MOVI.REG".
       COPY "PENDI.REG".
       COPY "COMPRO1.REG".
       COPY "MONE.REG".
       COPY "COTIZA.REG".
       COPY "PANTA.REG".
       COPY "CONDI.REG".
       COPY "IMPU.REG".
       COPY "ACTI.REG".
       COPY "INSDIR.REG".
       COPY "RES127.REG".
       COPY "IMP02.REG".

       COPY "VENTURNO.REG".
       COPY "LACAJA.REG".
       COPY "TARJ.REG".
       COPY "MANEJO.REG".
       COPY "ARET.REG".


       WORKING-STORAGE SECTION.
       COPY "ROT132.IMP".
       01 IMPRESION.
	  02 TIT-1.
	     03 T1           PIC 99 COMP-4 VALUE 18.
	     03 T2           PIC 99 COMP-4 VALUE 14.
	     03 EMP-ED       PIC X(18).
	     03 T3           PIC 99 COMP-4 VALUE 20.
	     03 T4           PIC 99 COMP-4 VALUE 15.
	     03 FILLER       PIC X(2) VALUE SPACES.
	     03 T5           PIC 99 COMP-4 VALUE 14.
	     03 CTA-ED       PIC ZZZ9B BLANK WHEN ZERO.
	     03 NOM-ED       PIC X(23).
	     03 T6           PIC 99 COMP-4 VALUE 20.
	  02 TIT-2.
	     03 COM-1-EMP-ED PIC X(30).
	     03 COM-2-EMP-ED PIC X(30).
	     03 FILLER       PIC X(14) VALUE " ".
	     03 DOM-ED       PIC X(47).
	  02 TIT-3.
	     03 FILLER       PIC X(74) VALUE SPACES.
	     03 CPOS-ED      PIC 9999B.
	     03 FILLER       PIC XX VALUE "- ".
	     03 LOC-ED       PIC X(30)BB.
	     03 PRO-ED       PIC X(15).
	  02 TIT-4.
	     03 CALLE-EMP-ED PIC X(30).
	     03 FILLER       PIC X(44) VALUE " ".
	     03 FILLER       PIC X(5) VALUE "TEL: ".
	     03 TEL-ED       PIC X(10).
	  02 TIT-5.
	     03 CPOS-EMP-ED  PIC 9999B.
	     03 FILLER       PIC XX VALUE "- ".
	     03 LOC-EMP-ED   PIC X(30)BBB.
	     03 PRO-EMP-ED   PIC X(20).
	  02 TIT-6.
	     03 COM-3-EMP-ED PIC X(30).
	     03 COM-4-EMP-ED PIC X(30).
	     03 FILLER       PIC X(23) VALUE SPACES.
	     03 IGAN-ED      PIC X(15).
	     03 FILLER       PIC XX VALUE SPACES.
	     03 RES-ED       PIC X(15).
	  02 TIT-7.
	     03 LOC-FAC-ED   PIC X(28).
	     03 FILLER       PIC X(3) VALUE " -".
	     03 PRO-FAC-ED   PIC X(18).
	     03 FILLER       PIC X(9) VALUE SPACES.
	     03 DD-ED        PIC 99/.
	     03 MM-ED        PIC 99/.
	     03 FILLER       PIC XX VALUE "19".
	     03 AA-ED        PIC 99.       
	     03 FILLER       PIC X(10) VALUE SPACES.
	     03 A1           PIC 99 COMP-4 VALUE 14.
	     03 T-FAC-ED     PIC X(12).
	     03 FILLER       PIC X VALUE "-".
	     03 SUB-T-C-ED   PIC X.
	     03 FILLER       PIC X VALUE "-".
	     03 A2           PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(4) VALUE SPACES.
	     03 SUC-C-ED     PIC 9999.
	     03 MOV-ED       PIC 99999999.
	  02 TIT-8.
	     03 FILLER       PIC X(83) VALUE SPACES.
	     03 CONDI-ED     PIC X(20).
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * IMPRESION SIPEC - SERGIO GRANTON MOTOS 
      * IMPRESION IMPRENTA V.ELISA PARA SCARAZZINI
      * IMPRESION IMPRENTA V.ELISA PARA VIOLLAZ
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *      COPY "W1-BICHO.WOR".             
      *      COPY "W1-FARMA.WOR".             
	     

	   02 TIT0-0.
	      03 FILLER       PIC 99 COMP-4 VALUE 15.
	      03 FILLER       PIC X(74) VALUE SPACES.
	      03 FILLER       PIC 99 COMP-4 VALUE 15.
	      03 FILLER       PIC 99 COMP-4 VALUE 14.
	      03 T-FAC1-ED    PIC X(12).
	      03 FILLER       PIC 99 COMP-4 VALUE 20.
	   02 TIT1-1.
	     03 Z10          PIC 99 COMP-4 VALUE 15.
	     03 Z11          PIC 99 COMP-4 VALUE 14.
	     03 FILLER       PIC X VALUE SPACES.
	     03 NOM1-ED      PIC X(30).
	     03 FILLER       PIC X(2) VALUE " (".
	     03 CTA1-ED      PIC ZZZZZ9 BLANK WHEN ZERO.
	     03 FILLER       PIC X(13) VALUE ")".
	     03 DD1-ED       PIC 99/.
	     03 MM1-ED       PIC 99/.
	     03 FILLER       PIC XX VALUE "19".
	     03 AA1-ED       PIC 99.
	     03 Z12          PIC 99 COMP-4 VALUE 20.
	  02 TIT1-2.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 DOM1-ED      PIC X(40)BB.
	     03 CPOS1-ED     PIC 9999B.
	     03 FILLER        PIC XX VALUE "- ".
	     03 LOC1-ED      PIC X(30).
	  02 TIT1-3.
	     03 FILLER       PIC X(8) VALUE "  CUIT:".
	     03 IGAN1-ED     PIC X(15).
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 RES1-ED      PIC X(15).
	     03 FILLER       PIC X(8) VALUE "  TEL:".
	     03 TEL1-ED      PIC X(10).
	     03 FILLER       PIC X(41) VALUE SPACES.
	     03 CONDI1-ED    PIC X(20).
	  02 TIT1-4.
	     03 FILLER       PIC X(14) VALUE "  ING.BRUTOS: ".
	     03 ING-BRUTOS1-ED  PIC X(15).
	     03 FILLER       PIC X(5) VALUE "  -  ".
	     03 C-ING-BRUTOS1-ED PIC X(15).
	     
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-
      * IMPRESION YUSTY
      * PARA TANO
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *    COPY "W2-BICHO.WOR".       
      *    COPY "W2-SOTE.WOR".       

	  02 TIT2-1.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 FILLER       PIC X(34) VALUE SPACES.
	     03 T-FAC2-ED    PIC X(20).
	     03 FILLER       PIC X(3) VALUE SPACES.
	     03 DD2-ED       PIC 99.
	     03 FILLER       PIC X VALUE "/".
	     03 MM2-ED       PIC 99.
	     03 FILLER       PIC X VALUE "/".
	     03 AA2-ED       PIC 99.
	     03 FILLER       PIC X(7) VALUE SPACES.
	     03 DD2-V-ED     PIC 99.
	     03 FILLER       PIC X VALUE "/".
	     03 MM2-V-ED     PIC 99.
	     03 FILLER       PIC X VALUE "/".
	     03 AA2-V-ED     PIC 99.
	  02 TIT2-2.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 NOM2-ED      PIC X(30).
	     03 FILLER       PIC X(30).
	     03 CTA2-ED      PIC ZZZZZ9 BLANK WHEN ZERO.
	  02 TIT2-3.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 DOM2-ED      PIC X(40).
	  02 TIT2-4.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 CPOS2-ED     PIC 9999.
	     03 FILLER       PIC X(3) VALUE " -".
	     03 LOC2-ED      PIC X(30).
	     03 FILLER       PIC X(10) VALUE "    TEL.: ".
	     03 TEL2-ED      PIC X(15).
	  02 TIT2-5.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 FILLER       PIC X(12) VALUE "ING.BRUTOS: ".
	     03 ING-BRUTOS2-ED  PIC X(15).
	     03 FILLER       PIC X(5) VALUE "  -  ".
	     03 C-ING-BRUTOS2-ED  PIC X(15).
	  02 TIT2-6.
	     03 FILLER       PIC X(7) VALUE SPACES.
	     03 RES2-ED      PIC X(15).
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 IGAN2-ED     PIC X(15).
	  02 TIT2-7.
	     03 FILLER       PIC X(20) VALUE SPACES.
	     03 CONDI2-ED    PIC X(20).
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * RENGLONES PARA SOTELO "A" no anular nunca
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	  02 IMP1-15.
	     03 FILLER       PIC 99 COMP-4 VALUE 15.
	     03 FILLER       PIC X(12) VALUE "  CANTIDAD".
	     03 FILLER       PIC X(53) VALUE "DESCRIPCION ARTICULO".
	     03 FILLER       PIC X(8) VALUE "I.V.A.".
	     03 FILLER       PIC X(14) VALUE " IMP.INTERNO".
	     03 FILLER       PIC X(14) VALUE "PRECIO UNIT.".
	     03 FILLER       PIC X(14) VALUE "  TOTAL NETO".
	     03 FILLER       PIC X(12) VALUE " FINAL UNIT.".
	  02 IMP1-16.
	     03 UNI11-ED     PIC ZZZZZ9 BLANK WHEN ZERO.
	     03 COMA1-ED     PIC X.
	     03 UNI21-ED     PIC 999 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 N-ART1-ED    PIC X(30)BB.
	     03 DET1-ED      PIC X(20)B.
	     03 ALI-IVA-ED   PIC Z9,99B.
	     03 FILLER       PIC XX VALUE "%".
	     03 I-INT1-ED    PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 PRE1-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 IMP1-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 FIN1-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	  02 IMP1-17.
	     03 FILLER       PIC X(5) VALUE "SON:".
	     03 CANT-ART1-ED PIC Z9B.
	     03 FILLER       PIC X(9) VALUE "ARTICULOS".
	     03 FILLER       PIC X(50) VALUE SPACES.
	     03 FILLER       PIC X(30) VALUE "NETO GRAVADO + EXENTO".
	     03 FILLER       PIC X(20) VALUE SPACES.
	     03 NET11-ED     PIC ZZZZZ.ZZ9,99.
	  02 IMP1-18.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 FILLER       PIC X(13) VALUE "NETO GRAVADO".
	     03 FILLER       PIC X(13) VALUE SPACES.
	     03 FILLER       PIC X(13) VALUE "  EXENTO".
	     03 FILLER       PIC X(4) VALUE SPACES.
	     03 IVA-11-ED    PIC Z9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC X(9) VALUE SPACES.
	     03 IVA-21-ED    PIC Z9,99 BLANK WHEN ZERO.
	  02 IMP1-19.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 NET1-ED      PIC ZZZZ.ZZ9,99BB.
	     03 II1-ED       PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 EXE1-ED      PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.  
	     03 IVA-I1-ED    PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 IVA-N1-ED    PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 TOT-11-ED    PIC ZZZZ.ZZ9,99.
	  02 IMP1-20.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 FILLER       PIC X(13) VALUE SPACES.
	     03 FILLER       PIC X(13) VALUE "RES.127/93-->".
	     03 PER1-ED      PIC ZZZZ.ZZ9,99.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * RENGLONES PARA SOTELO "B" no anular nunca
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	  02 IMP2-15.
	     03 FILLER       PIC 99 COMP-4 VALUE 15.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 FILLER       PIC X(12) VALUE "  CANTIDAD".
	     03 FILLER       PIC X(53) VALUE "DESCRIPCION ARTICULO".
	     03 FILLER       PIC X(8) VALUE "I.V.A.".
	     03 FILLER       PIC X(14) VALUE " IMP.INTERNO".
	     03 FILLER       PIC X(14) VALUE "PRECIO UNIT.".
	     03 FILLER       PIC X(14) VALUE "     TOTAL".
	     03 FILLER       PIC X(12) VALUE " FINAL UNIT.".
	   02 IMP2-16.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 UNI12-ED     PIC ZZZZZ9 BLANK WHEN ZERO.
	     03 COMA2-ED     PIC X.
	     03 UNI22-ED     PIC 999 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 N-ART2-ED    PIC X(30)BB.
	     03 DET2-ED      PIC X(20)B.
	     03 ALI-IVA2-ED  PIC Z9,99B.
	     03 FILLER       PIC XX VALUE "%".
	     03 I-INT2-ED    PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 PRE2-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 IMP2-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 FIN2-ED      PIC ZZZZZ.ZZ9,99 BLANK WHEN ZERO.
	  02 IMP2-17.
	     03 FILLER       PIC X(5) VALUE SPACES.
	     03 FILLER       PIC X(5) VALUE "SON:".
	     03 CANT-ART2-ED PIC Z9B.
	     03 FILLER       PIC X(9) VALUE "ARTICULOS".
	     03 FILLER       PIC X(50) VALUE SPACES.
	     03 FILLER       PIC X(30) VALUE "     GRAVADO + EXENTO".
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 NET12-ED     PIC ZZZZZ.ZZ9,99.
	  02 IMP2-18.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 FILLER       PIC X(3) VALUE SPACES.
	     03 FILLER       PIC X(13) VALUE "   GRAVADO".
	     03 FILLER       PIC X(13) VALUE SPACES.
	     03 FILLER       PIC X(13) VALUE "  EXENTO".
	     03 FILLER       PIC X(13) VALUE " RES.127/93 ".
	  02 IMP2-19.
	     03 FILLER       PIC 99 COMP-4 VALUE 18.
	     03 FILLER       PIC X(3) VALUE SPACES.
	     03 NET2-ED      PIC ZZZZ.ZZ9,99BB.
	     03 II2-ED       PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	    03 EXE2-ED      PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.  
	     03 PER2-ED      PIC ZZZZ.ZZ9,99.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 IVA-N2-ED    PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 TOT-12-ED    PIC ZZZZ.ZZ9,99.
	  02 IMP2-20.
	     03 IVA-I2-ED    PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
      *=-=-=-==-====-====-=-=-=-=-=-=-=-=-=-=-===-=-=-=-===-===
      * RENGLONES PARA FARMACIA VILLA ELISA FACTURAS <B>
      * no anular nunca
      *=-=-===-=-=====-=-=-=-=-=-=-=-==-==-=-==-==-=====-===-=-
	  02 IMP3-15.
	     03 FILLER       PIC X(12) VALUE " CANTIDAD ".
	     03 FILLER       PIC X(53) VALUE "DESCRIPCION ARTIC.".
	     03 FILLER       PIC X(15) VALUE "  CODIGO ART.".
	     03 FILLER       PIC X(13) VALUE "PRECIO UNIT.".
	     03 FILLER       PIC X(8) VALUE " TOTALES".
	  02 IMP3-16.
	     03 UNI1-ED3     PIC ZZZZZ9 BLANK WHEN ZERO.
	     03 COMA-ED3     PIC X.
	     03 UNI2-ED3     PIC 999 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 N-ART-ED3    PIC X(30)BB.
	     03 DET-ED3      PIC X(20)B.
	     03 REP-ED3      PIC X.
	     03 FILLER       PIC X VALUE "(".
	     03 ART-ED3      PIC 9999999999 BLANK WHEN ZERO.
	     03 FILLER       PIC X(4) VALUE ")".
	     03 PRE-ED3       PIC Z.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC XXX VALUE SPACES.
	     03 IMP-ED3       PIC Z.ZZ9,99 BLANK WHEN ZERO.
	  02 IMP3-17.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 COM1-ED3     PIC X(40).
	     03 COM2-ED3     PIC X(40).
	  02 IMP3-18. 
	     03 FILLER       PIC XX VALUE SPACES.
	     03 COM3-ED3     PIC X(40).
	     03 FILLER       PIC X(43) VALUE SPACES.
	     03 FILLER       PIC 99 COMP-4 VALUE 14.
	     03 TOT-ED3       PIC ZZ.ZZ9,99.
	     03 FILLER       PIC 99 COMP-4 VALUE 20.
	  02 IMP3-19.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 FILLER       PIC X(7) VALUE "REF.:".
	     03 REF-ED3      PIC X(20).
      *=-=-=-=--=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * RENGLONES STANDAR no anular nunca
      *=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
	  02 IMP-15.
	     03 FILLER       PIC X(12) VALUE " CANTIDAD ".
	     03 FILLER       PIC X(53) VALUE "DESCRIPCION ARTIC.".
	     03 FILLER       PIC X(15) VALUE "  CODIGO ART.".
	     03 FILLER       PIC X(19) VALUE "ENVASE".
	     03 FILLER       PIC X(16) VALUE "PRECIO UNIDAD".
	     03 FILLER       PIC X(11) VALUE "   TOTALES".
	  02 IMP-151.
	     03 FILLER       PIC X(65) VALUE ALL "-".
	     03 FILLER       PIC X(50) VALUE ALL "-".
	     03 FILLER       PIC X(12) VALUE ALL "-".
	  02 IMP-16.
	     03 UNI1-ED      PIC ZZZZZ9 BLANK WHEN ZERO.
	     03 COMA-ED      PIC X.
	     03 UNI2-ED      PIC 999 BLANK WHEN ZERO.
	     03 FILLER       PIC XX VALUE SPACES.
	     03 N-ART-ED     PIC X(30)BB.
	     03 DET-ED       PIC X(20)B.
	     03 REP-ED       PIC X.
	     03 FILLER       PIC X VALUE "(".
	     03 ART-ED       PIC 9999999999 BLANK WHEN ZERO.
	     03 FILLER       PIC X(2) VALUE ")".
	     03 ENV-ED       PIC X(20)BB.
	     03 PRE-ED       PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	     03 FILLER       PIC X VALUE SPACES.
	     03 IMP-ED       PIC ZZZZ.ZZ9,99 BLANK WHEN ZERO.
	  02 IMP-17.
	     03 FILLER       PIC X(45) VALUE SPACES.
	     03 FILLER       PIC X(10) VALUE "    REF: ".
	     03 REF-ED       PIC X(20).
	     03 FILLER        PIC X(21) VALUE SPACES.
      *=-=-=-=-=-=-=-=-=-=SIN TOTAL=(BICHO)
	     03 FILLER       PIC X(7) VALUE SPACES.
      *=-=-=-=-=-=-=-=-=-=CON TOTAL=(KAPPES-MIRTA)-=-=-
      *        03 FILLER       PIC X(7) VALUE "TOTAL $".
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	     03 FILLER       PIC 99 COMP-4 VALUE 14.
	     03 TOT-ED       PIC ZZZZ.ZZ9,99.
	     03 FILLER       PIC 99 COMP-4 VALUE 20.
	  02 IMP-171.
	     03 FILLER       PIC X(13) VALUE " EQUIVALE A: ".
	     03 EXT-ED       PIC ZZZZ.ZZ9,99B.
	     03 MON-ED       PIC X(20).
	     03 FILLER       PIC X(10) VALUE "  -  REF: ".
	     03 REF-ED1       PIC X(20).
	     03 FILLER       PIC X(24) VALUE SPACES.
      *-------------------SIN TOTAL-(BICHO)----------------
	     03 FILLER       PIC X(7) VALUE SPACES.
      *-------------------CON TOTAL-(KAPPES-MIRTA)---------
      *       03 FILLER       PIC X(7) VALUE "TOTAL $".
      *------------------------------------------------------
	     03 FILLER       PIC 99 COMP-4 VALUE 14.
	     03 TOT-1-ED     PIC ZZZZ.ZZ9,99.
	     03 FILLER       PIC 99 COMP-4 VALUE 20.              
	  02 IMP-18.
	     03 FILLER       PIC X VALUE SPACES.
      *=-=-=-=-=PARA BICHO SIN COMENTARIO=-=-=
	     03 LF-ED        PIC X(16) VALUE " Factura Nro.:".
	     03 LA-FACTURA-ED PIC ZZZZZZZ BLANK WHEN ZERO.
      *=-=-=-=-=PARA KAPPES - MIRTA -=-=-=-=-=
      *       03 FILLER       PIC X(24) VALUE "C O M E N T A R I O S :".
	     03 FILLER       PIC X(65) VALUE SPACES.
	     03 FILLER       PIC X(24) VALUE "SUBTOTAL $    :".
	     03 NET-ED       PIC ZZZZ.ZZ9,99.
	  02 IMP-19.
	     03 FILLER       PIC X VALUE SPACES.
	     03 COM1-ED      PIC X(40).
	     03 FILLER       PIC X(49) VALUE SPACES.
	     03 FILLER       PIC X(7) VALUE "I.V.A.".
	     03 IVA-1-ED     PIC Z9,99.
	     03 FILLER       PIC X(3) VALUE "% :".
	     03 FILLER       PIC X(9) VALUE SPACES.
	     03 IVA-I-ED     PIC ZZZZ.ZZ9,99.
	  02 IMP-191.
	     03 FILLER       PIC X(90) VALUE SPACES.
	     03 FILLER       PIC X(24) VALUE "IMP.INTERNO   :".
	     03 II-ED        PIC ZZZZ.ZZ9,99.
	  02 IMP-193.
	     03 FILLER       PIC X VALUE SPACES.
	  02 IMP-192.
	     03 FILLER       PIC X VALUE SPACES.
	     03 COM1-ED1     PIC X(40) VALUE SPACES.
	  02 IMP-20.
	     03 FILLER       PIC X(1) VALUE SPACES.
	     03 COM2-ED      PIC X(40).
	     03 FILLER       PIC X(49) VALUE SPACES.
	     03 FILLER       PIC X(7) VALUE "I.V.A.".
	     03 IVA-2-ED     PIC Z9,99.
	     03 FILLER       PIC X(12) VALUE "% :".
	     03 IVA-N-ED     PIC ZZZZ.ZZ9,99.
	  02 IMP-201.
	     03 FILLER       PIC X VALUE SPACES.
	     03 COM2-ED1     PIC X(40).
	  02 IMP-202.
	     03 FILLER       PIC X VALUE SPACES.
	     03 COM3-ED      PIC X(40).
	     03 FILLER       PIC X(49) VALUE SPACES.
	     03 FILLER       PIC X(10) VALUE "RES127/93 ".
	     03 POR-RES-ED   PIC 9,9.
	     03 FILLER       PIC X(11) VALUE "%:".
	     03 RES-IMP-ED   PIC ZZZZ.ZZ9,99.
	  02 IMP-203.
	     03 FILLER       PIC X VALUE SPACES.
	     03 COM3-ED1     PIC X(40).
	  02 IMP-21.
	     05 FILLER       PIC X VALUE SPACES.
	     05 FILLER       PIC X(5) VALUE "CUIT:".
	     05 CUIT-EMP-ED  PIC X(15).
	     05 FILLER       PIC X(12) VALUE "  I.BRUTOS:".
	     05 I-BRUTOS-EMP-ED  PIC X(15).
	     05 FILLER       PIC X(10) VALUE "  MUNIC.:".
	     05 MUNI-EMP-ED  PIC X(15).
	     05 FILLER       PIC X(10) VALUE "  T.Aut.:".
	     05 JUB-EMP-ED   PIC X(15).
	     05 FILLER       PIC X(11) VALUE "  Ap.Pers: ".
	     05 AP-JUB-EMP-ED PIC X(15).
	  02 IMP-22.
	     05 FILLER       PIC X(7) VALUE "  IVA:".
	     05 IVA-EMP-ED   PIC X(15).           
	     05 FILLER       PIC X(30) VALUE "  Imp.Int.:No Resp.".
	     05 AA2          PIC 99 COMP-4 VALUE 14.
	     05 CANT-I-ED    PIC X(13).
	     05 AA3          PIC 99 COMP-4 VALUE 20.
	  02 IMP-30.
	     03 FILLER       PIC X VALUE SPACES.
	     03 FILLER       PIC X(24) VALUE "C O M E N T A R I O S :".
	  02 IMP-40.
	     03 FILLER       PIC X VALUE SPACES.
	     03 FILLER       PIC X(5) VALUE "SON: ".
	     03 CANT-ART-ED  PIC Z9B.
	     03 FILLER       PIC X(20) VALUE "ARTICULOS".
	     03 JJ01         PIC 99 COMP-4 VALUE 14.
	     03 COM-REP-ED   PIC X(22).
	     03 JJ02         PIC 99 COMP-4 VALUE 20.
	  02 IMP-50.
	     03 FILLER       PIC X VALUE SPACES.
	     03 KK01         PIC 99 COMP-4 VALUE 14.
	     03 FILLER       PIC X(20) VALUE "REMITO P/REPARTO ".
	     03 KK02         PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 FILLER       PIC X(11) VALUE " CANTIDAD ".
	     03 FILLER       PIC X(31) VALUE "DESCRIPCION ARTIC.".
	     03 FILLER       PIC X(13) VALUE "COMENTARIO".
	     03 FILLER       PIC X(10) VALUE "ENVASE".
	  02 IMP-500.
	     03 FILLER       PIC X VALUE SPACES.
	     03 KK010         PIC 99 COMP-4 VALUE 14.
	     03 FILLER       PIC X(20) VALUE "PENDIENTE DE ENTREGA".
	     03 KK020         PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 FILLER       PIC X(11) VALUE " CANTIDAD ".
	     03 FILLER       PIC X(31) VALUE "DESCRIPCION ARTIC.".
	     03 FILLER       PIC X(13) VALUE "COMENTARIO".
	     03 FILLER       PIC X(10) VALUE "ENVASE".
	  02 IMP-51.
	     03 FILLER       PIC X VALUE SPACES.
	     03 KK03         PIC 99 COMP-4 VALUE 14.
	     03 COM-REP-ED1  PIC X(20).
	     03 KK04         PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 FILLER       PIC X(65) VALUE ALL "-".
	  02 IMP-52.
	     03 FILLER       PIC X VALUE SPACES.
	     03 KK05         PIC 99 COMP-4 VALUE 14.
	     03 FILLER       PIC X(20) VALUE ALL "-".
	     03 KK06         PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 IMP-R1-ED    PIC X(65).
	  02 IMP-53.
	     03 FILLER       PIC X VALUE SPACES.
	     03 KK07         PIC 99 COMP-4 VALUE 14.
	     03 NOM1-ED1     PIC X(20).
	     03 KK08         PIC 99 COMP-4 VALUE 20.
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 IMP-R2-ED    PIC X(65).
	  02 IMP-54.
	     03 FILLER       PIC X VALUE SPACES.
	     03 DOM1-ED1     PIC X(40).
	     03 FILLER       PIC X(18) VALUE SPACES.
	     03 IMP-R3-ED    PIC X(65).
	  02 IMP-55.
	     03 FILLER       PIC X VALUE SPACES.
	     03 CPOS1-ED1    PIC 9999B.
	     03 FILLER       PIC XX VALUE "- ".
	     03 LOC1-ED1     PIC X(30).
	     03 FILLER       PIC X(21) VALUE SPACES.
	     03 IMP-R4-ED    PIC X(65).
	  02 IMP-56.
	     03 FILLER       PIC X(59) VALUE SPACES.
	     03 IMP-R5-ED    PIC X(65). 
	  02 LIN-10.
	     03 FILLER       PIC X(132) VALUE SPACES.
	  02 LIN-12.
	     03 FILLER       PIC X(96) VALUE SPACES.
	     03 FILLER       PIC X(43) VALUE ALL "=".
	  02 LIN-13.
	     03 FILLER       PIC X(65) VALUE ALL "*".
	  02 LIN-30.
	     03 F1        PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "- SIN VALOR CONTABLE -". 
	     03 F2        PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE "La mercaderia se FACTURA".
	     03 FILLER    PIC X(25) VALUE "a la entrega segun la exi".
	     03 FILLER    PIC X(25) VALUE "stencia ".
	  02 LIN-32.
	     03 H1        PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "- SIN VALOR CONTABLE -".
	     03 H2        PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE " Incluye IVA e Imp.Int. -".
	     03 FILLER    PIC X(25) VALUE "Los precios pueden variar".
	     03 FILLER    PIC X(25) VALUE " sin previo aviso".
	  02 LIN-321.
	     03 H11       PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "- SIN VALOR CONTABLE -".
	     03 H21       PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE " No  incluye  IVA    -".
	     03 FILLER    PIC X(25) VALUE "Los precios pueden variar".
	     03 FILLER    PIC X(25) VALUE " sin previo aviso".
	  02 LIN-33.
	     03 J1        PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "- A FACTURAR -".
	     03 J2        PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE "                    Los p".
	     03 FILLER    PIC X(25) VALUE "recios seran los vigentes".
	     03 FILLER    PIC X(25) VALUE " al dia de facturar.-".
	  02 LIN-331.
	     03 J11       PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "- A FACTURAR -".
	     03 J21       PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE "Los precios pueden variar".
	     03 FILLER    PIC X(25) VALUE " al dia de facturar.-".
	  02 LIN-34.
	     03 Y11       PIC 99 COMP-4 VALUE 14.
	     03 FILLER    PIC X(25) VALUE "*** FACTURADO ***".
	     03 Y12       PIC 99 COMP-4 VALUE 20.
	     03 FILLER    PIC X(25) VALUE "Mercaderia en reparto a s".
	     03 FILLER    PIC X(25) VALUE "olicitud del comprador".
	  02 LIN-35.
	     03 FILLER    PIC X(28) VALUE "- Mercaderia a retirar -".

       01  RENGLONES-DE-PAGARE.
	   03  R1-PAGARE.
	       05  FILLER           PIC 99 COMP-4 VALUE 18.
	       05  FILLER           PIC X(50).
	       05  FEC-PAGARE.
		   07  DIAV-PAG      PIC 99BB.
		   07  MESV-PAG      PIC 99BB.
		   07  ANOV-PAG      PIC 99B(4).
	       05  IMP-PAG           PIC ZZZ.ZZZ.ZZ9,99.
	   03  R2-PAGARE.
	       05  FILLER           PIC X(25).
	       05  LUG-PAG          PIC X(15)B(9).
	       05  DIA-PAG          PIC 99BBBBB.
	       05  ELMES-PAG        PIC X(15)B(07) VALUE SPACES.
	       05  ANO-PAG          PIC 99.
	   03  R3-PAGARE.
	       05  FILLER           PIC X(23) VALUE SPACES.
	       05  DIAV1-PAG        PIC 99/.
	       05  MESV1-PAG        PIC 99/.
	       05  ANOV1-PAG        PIC 99B(3).
	   03  R4-PAGARE.
	       05  FILLER           PIC X(21) VALUE SPACES.
	       05  NOM-PAG          PIC X(30).
	   03  R5-PAGARE.
	       05  FILLER           PIC X(48) VALUE SPACES.
	       05  PES-PAG          PIC X(30).
	   03  R6-PAGARE.
	       05  FILLER           PIC X(15) VALUE SPACES.
	       05  PES1-PAG         PIC X(55).
	   03  R7-PAGARE.
	       05  FILLER           PIC X(24) VALUE SPACES.
	       05  NOMC-PAG         PIC X(55).
	   03  R8-PAGARE.
	       05  FILLER           PIC X(23) VALUE SPACES.
	       05  DOC-PAG          PIC X(55).
	   03  R9-PAGARE.
	       05  FILLER           PIC X(20) VALUE SPACES.
	       05  DIR-PAG          PIC X(55).
	   03  R10-PAGARE.
	       05  FILLER           PIC X(20) VALUE SPACES.
	       05  LOC-PAG          PIC X(55).
       01  LOSMESES.
	   03  MMESES.
	       05  FILLER      PIC X(14) VALUE "  ENERO".
	       05  FILLER      PIC X(14) VALUE "  FEBRERO".
	       05  FILLER      PIC X(14) VALUE "  MARZO".
	       05  FILLER      PIC X(14) VALUE "  ABRIL".
	       05  FILLER      PIC X(14) VALUE "  MAYO".
	       05  FILLER      PIC X(14) VALUE "  JUNIO".
	       05  FILLER      PIC X(14) VALUE "  JULIO".
	       05  FILLER      PIC X(14) VALUE "  AGOSTO".
	       05  FILLER      PIC X(14) VALUE "  SETIEMBRE".
	       05  FILLER      PIC X(14) VALUE "  OCTUBRE".
	       05  FILLER      PIC X(14) VALUE "  NOVIEMBRE".
	       05  FILLER      PIC X(14) VALUE "  DICIEMBRE".
	   03  R-MMESES REDEFINES MMESES PIC X(14) OCCURS 12.

       01 VV                 PIC 9 VALUE 0.
       01 TIPO-PAGO          PIC X VALUE " ".
       01 PAR-IMPRESORA-A    PIC X(4) VALUE "LPT1".
       01 PAR-IMPRESORA-B    PIC X(4) VALUE "LPT2".
       01 MIRTA              PIC X(4).
       01 C-RES123           PIC X VALUE SPACES.
       01 CTA-CAP            PIC 9(6) VALUE 0.
       01 NOM-CAP            PIC X(30).
       01 PRE-CAP            PIC 9(10)V99 VALUE 0.
       01 FIN-CAP            PIC 9(10)V99 VALUE 0. 
       01 PRE-N-CAP          PIC 9(10)V99 VALUE 0.
       01 IVA-P-CAP          PIC 9(10)V99 VALUE 0.
       01 IMP-N-CAP          PIC 9(10)V99 VALUE 0.
       01 PRE-P-CAP          PIC 9(10)V99 VALUE 0.
       01 UNI-N-CAP          PIC 9(10)V99 VALUE 0.
       01 PRE1-CAP           PIC 9(10)V99 VALUE 0.
       01 PRE-U-CAP          PIC 9(10)V99 VALUE 0.
       01 CAP-II             PIC 9(10)V99 VALUE 0.
       01 II-CAP             PIC 9(10)V99 VALUE 0.
       01 IMP-INT            PIC 9(10)V99 VALUE 0.
       01 IMP-CAP            PIC 9(10)V99 VALUE 0.
       01 IMP-T-CAP          PIC 9(10)V99 VALUE 0.
       01 EXE-T-CAP          PIC 9(10)V99 VALUE 0.
       01 EXT-CAP            PIC 9(10)V99 VALUE 0.
       01 LA-FACTURA         PIC 9(8) VALUE 0.
       01 MON-CAP            PIC 9 VALUE 0.
       01 MES-AYUDA          PIC 99 VALUE 0.
       01 VALOR-PAGARE       PIC 9(12)V99 VALUE 0.
       01 ELPAG              PIC X VALUE " ".
       01 REF-CAP            PIC X(20).
       01 CONT-REP           PIC 99 VALUE 0.
       01 D-R-CAP            PIC S9(9)V99 VALUE 0.
       01 P-D-R-CAP          PIC S99V99 VALUE 0.
       01 TOT-CAP            PIC 9(10)V99 VALUE 0.
       01 SIN-9              PIC S9(10)V99 VALUE 0.
       01 SUB-T-C-CAP        PIC X VALUE SPACES.
       01 SUC-C-CAP          PIC 9(4) VALUE 0.
       01 MOV-CAP            PIC 9(8) VALUE 0.
       01 MOV-AUX            PIC 9(8) VALUE 0.
       01 RENG-CAP            PIC 99 VALUE 0.
       01 UNI-CAP             PIC 999999V999.
       01 FRA-CAP            PIC X.
       01 VEN-CAP            PIC 9(6) VALUE 0.
       01 NOM-M-CAP          PIC X(30) VALUE SPACES.
       01 DOM-M-CAP          PIC X(30) VALUE SPACES.
       01 CPOS-M-CAP         PIC 9(4) VALUE 0.
       01 LOC-M-CAP          PIC X(30) VALUE SPACES.
       01 TEL-M-CAP          PIC X(10) VALUE SPACES.
       01 RES-M-CAP          PIC X VALUE SPACES.
       01 CUIT-M-CAP         PIC X(15) VALUE SPACES.
       01 SIT-M-CAP          PIC 99 VALUE 0.
       01 I-BRU-M-CAP        PIC X(15) VALUE SPACES.
       01 DET-CAP            PIC X(20).
       01 DET-CAP0           PIC X(20).
       01 IVA-I-SUB          PIC 9(10)V99 VALUE 0.
       01 IVA-N-SUB          PIC 9(10)V99 VALUE 0.
       01 OT-IMP-SUB         PIC 9(10)V99 VALUE 0.
       01 TOTAL-SUB          PIC 9(10)V99 VALUE 0.
       01 TOTAL-CREDITO      PIC S9(12)V99 VALUE 0.
       01 V                  PIC 99.
       01 Y                  PIC 9 VALUE 0.
       01 X                  PIC 9 VALUE 0.
       01 BASE-PRO           PIC 9(10)V99 VALUE 0.
       01 BASE-MUN           PIC 9(10)V99 VALUE 0. 
       01 IVA-CAP            PIC 9(10)V99 VALUE 0.
       01 IVA-N-CAP          PIC 9(10)V99 VALUE 0.
       01 NETO-CAP           PIC 9(10)V99 VALUE 0.
       01 EXE-CAP            PIC 9(10)V99 VALUE 0.
       01 PER-CAP            PIC 9(10)V99 VALUE 0.
       01 OT-IMP-CAP         PIC 9(10)V99 VALUE 0.
       01 CANT-ART           PIC 99 VALUE 0.
       01 FIN-INT            PIC 9(10)V99 VALUE 0.
       01 COTI-VAL           PIC 9(10)V999.
       01 COM-CAP            PIC X(14).
       01 COD-II             PIC 9 VALUE 0.
       01 REP-CAP            PIC 9 VALUE 0.
       01 IMP-REP            PIC 99 VALUE 0.
       01 ACT-MUN-PRI        PIC 999 VALUE 0.
       01 ACT-PRO-PRI        PIC 999 VALUE 0.
       01 POR-ACT-MUN        PIC 99V99 VALUE 0.
       01 POR-ACT-PRO        PIC 99V99 VALUE 0.
       01 CAL-ACT-MUN        PIC 9 VALUE 0.
       01 CAL-ACT-PRO        PIC 9 VALUE 0.
       01 VAL-1-ACT-MUN      PIC 9(10)V99 VALUE 0.
       01 VAL-1-ACT-PRO      PIC 9(10)V99 VALUE 0.
       01 VAL-2-ACT-MUN      PIC 9(10)V99 VALUE 0.
       01 VAL-2-ACT-PRO      PIC 9(10)V99 VALUE 0.
       01 SALDO-COM          PIC S9(13)V99 VALUE 0.
       01 TOTAL-P-CAP        PIC S9(13)V99 VALUE 0.
       01 IMPORTE-INT        PIC S9(13)V99 VALUE 0.
       01 RUBRO-INT          PIC 9(7) VALUE 0.
       01 IMP-R.
	  02 UNI1-R          PIC ZZZZZ9.
	  02 COMA-R          PIC X.
	  02 UNI2-R          PIC 999 BLANK WHEN ZERO.
	  02 FILLER          PIC X VALUE SPACES.
	  02 N-ART-R         PIC X(30)B.
	  02 COM-R           PIC X(12)B.
	  02 ENV-R           PIC X(10).
       01 COM-REP.
	  02 COM-1-REP       PIC X(7) VALUE SPACES.
	  02 COM-2-REP       PIC X VALUE SPACES.
	  02 COM-3-REP       PIC 9999 VALUE 0.
	  02 COM-4-REP       PIC 9(8) VALUE 0.
       01 IVA-1-CAP          PIC 99V99 VALUE 0.
       01 IVA-2-CAP          PIC 99V99 VALUE 0.
       01 IVA-I              PIC 99V99.
       01 I-IVA REDEFINES IVA-I.
	  03 IVA-I1          PIC 99.
	  03 IVA-I2          PIC 99.
       01 IVA-N              PIC 99V99.
       01 N-IVA REDEFINES IVA-N.
	  03 IVA-N1          PIC 99.
	  03 IVA-N2          PIC 99.
       01 FEC-CAP.
	  03 DD-CAP          PIC 99 VALUE 0.
	  03 MM-CAP          PIC 99 VALUE 0.
	  03 AA-CAP          PIC 99 VALUE 0.
       01 FEC-V-CAP.
	  03 DD-V-CAP        PIC 99 VALUE 0.
	  03 MM-V-CAP        PIC 99 VALUE 0.
	  03 AA-V-CAP        PIC 99 VALUE 0.
       01 COM1               PIC X(40).
       01 COM2               PIC X(40).
       01 COM3               PIC X(40).
       01 C-MEN-ST           PIC X VALUE SPACES.
       01 CONT-F             PIC 9 VALUE 0.
       01 PRE-F-EDI          PIC ZZZZ.ZZZ.ZZ9,99.
       01 PRE-EDI            PIC ZZZZ.ZZZ.ZZ9,99.
       01 PRE1-EDI           PIC ZZZZ.ZZZ.ZZ9,99.
       01 PRE-U-EDI          PIC ZZZZ.ZZZ.ZZ9,99.
       01 SUB-T-C-EDI        PIC X.
       01 SUC-C-EDI          PIC 9999.
       01 MOV-EDI            PIC 99999999.
       01 IMP-EDI            PIC ZZZZ.ZZZ.ZZ9,99.
       01 D-R-EDI            PIC ----.---.--9,99.
       01 TOT-EDI            PIC ZZZZ.ZZZ.ZZ9,99.
       01 IMP-T-EDI          PIC ZZZZ.ZZZ.ZZ9,99.
       01 IVA-EDI            PIC ZZZZ.ZZZ.ZZ9,99.
       01 IVA-N-EDI          PIC ZZZZ.ZZZ.ZZ9,99.
       01 II-EDI             PIC ZZZZ.ZZZ.ZZ9,99.
       01 EXE-SUB-ED         PIC ZZZZ.ZZZ.ZZ9,99.
       01 II-SUB-ED          PIC ZZZZ.ZZZ.ZZ9,99.
       01 IVA-I-SUB-ED       PIC ZZZZ.ZZZ.ZZ9,99.
       01 IVA-N-SUB-ED       PIC ZZZZ.ZZZ.ZZ9,99.
       01 OT-IMP-SUB-ED      PIC ZZZZ.ZZZ.ZZ9,99.
       01 TOTAL-SUB-ED       PIC ZZZZ.ZZZ.ZZ9,99.
       01 CAP-LIS1          PIC 9 VALUE 0.
       01 IVA1-IND          PIC 9V9999.
       01 IND-IVA1 REDEFINES IVA1-IND.
	  03 IVA11-IND      PIC 9.
	  03 IVA12-IND      PIC 9999.
       01 RESP-EDI          PIC X(15).
       01 NETO-EDI          PIC ZZZZ.ZZZ.ZZ9,99.
       01 FEC-EDI           PIC 99/99/99.
       01 CONDI-A           PIC 999.
       01 QQ                PIC X.
       01 PRE-BOL           PIC 9(10)V99 VALUE 0.
       01 ENV-EDI.
	  03 NOM-ENV-EDI    PIC X(20).
       01 NUM-MAR.
	  02 N-ART02         PIC 9(6).
	  02 NU-ART02 REDEFINES N-ART02.
	     03 N-1-ART02    PIC 9(3).
	     03 N-2-ART02    PIC 9(3).
       01 CANTIDAD.
	  02 CANT-UNI        PIC 9(6)V999.
	  02 UNI-CANT  REDEFINES CANT-UNI.
	     03 UNI-1        PIC 999999.
	     03 UNI-2        PIC 999.
       01 STOCK.
	  02 STOCK-CAP       PIC S9(6)V999.
	  02 CAP-STOCK REDEFINES STOCK-CAP.
	     03 STO-1        PIC S9(6).
	     03 STO-2        PIC 999.
       01 STOCK-EDI          PIC ------9,999.
       01 IVA-1-EDI          PIC Z9,99.
       01 IVA-2-EDI          PIC Z9,99.
       01 UNI-STOCK          PIC 9(6)V999.
       01 TOT-STOCK          PIC 9(6)V999.
       01 NUM-ART02-EDI      PIC ZZZZZ9.
       01 CONT-S             PIC 9 VALUE 0.
       01 COD-DET0           PIC 9 VALUE 0.
       01 CONTROLO           PIC X.
       01 COD-REP            PIC 9.
       01 YY                 PIC 9 VALUE 0.
       01 INS-DIR-COD        PIC 9 VALUE 0.
       01 ACOMODA-ART.
	  03   ART-ACOMODA   PIC 9(6).
	  03   ART-PRECIO    PIC 9(4).
       01 VECTOR-ALI-PER.
	  02 ALI-PER         PIC 99V999 OCCURS 10.
       01 ALI-INSDIR         PIC 99V999 VALUE 0.
      *=-=-=-=-=-=-=-= VECTOR =-=-=-=-=-=-=-=-=-=
      * PARA TODOS
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=    
      *    COPY "W3-TODOS.WOR".       

       01 VECTOR.
	  02 ART-VEC         PIC 9(10) OCCURS 20.
	  02 N-ART-VEC       PIC X(30) OCCURS 20.
	  02 ENV-VEC         PIC X(20) OCCURS 20.
	  02 N-MAR-VEC       PIC X(20) OCCURS 20.
	  02 UNI-S-VEC       PIC 9(6)V999 OCCURS 20.
	  02 UNI-VEC         PIC 9(6)V999 OCCURS 20.
	  02 UNI-V-VEC       PIC 9(6)V999 OCCURS 20.
	  02 CANT1-VEC       PIC 9(6)V999 OCCURS 20.
	  02 FRA-VEC         PIC X OCCURS 20.
	  02 T-UNI-VEC       PIC 9 OCCURS 20.
	  02 PRE-VEC         PIC 9(10)V99 OCCURS 20.
	  02 D-R-VEC         PIC 9(10)V99 OCCURS 20.
	  02 IMP-VEC         PIC 9(10)V99 OCCURS 20.
	  02 COS-VEC         PIC 9(10)V99 OCCURS 20.
	  02 II-VEC          PIC 9(10)V99 OCCURS 20.
	  02 DET-VEC         PIC X(20) OCCURS 20.
	  02 REP-VEC         PIC 9 OCCURS 20.
	  02 LIN-1-VEC       PIC 999 OCCURS 20.
	  02 ALI-VEC         PIC 99V99 OCCURS 20.
	  02 ACT-MUN-VEC     PIC 999 OCCURS 20.
	  02 ACT-PRO-VEC     PIC 999 OCCURS 20.
      **=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==--=-=-=-=-=-=-=
      * PARA VIOLLAZ - SOTELO
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       01  LASLETRAS.          
	   03  LASL        PIC X(133).
	   03  R-LASL REDEFINES LASL.
	       05  LASL1   PIC X(30).
	       05  LASL2   PIC X(103).

	  

       COPY "VARIOS.WOR".
       COPY "CLAVES.WOR".
       COPY "PESOS.WOR".

       LINKAGE SECTION.
       01  LINK-EMP        PIC 9(4).
       01  LINK-SUC        PIC 9(2).
       01  LINK-RUB        PIC 9(6).

       PROCEDURE DIVISION.
       DECLARATIVES.
       COPY "EMPRE.ERR".
       COPY "ALICUO.ERR".
       COPY "ART01.ERR".
       COPY "ART02.ERR".
       COPY "NUMERO.ERR".
       COPY "MARCAS.ERR".
       COPY "ENVASE.ERR".
       COPY "OPERA.ERR".
       COPY "COMPRO1.ERR".
       COPY "IMP02.ERR".
       COPY "CONDI.ERR".
       COPY "CPOSTAL.ERR".
       COPY "MONE.ERR".
       COPY "COTIZA.ERR".
       COPY "IVA.ERR".
       COPY "CUENTA.ERR".
       COPY "IMPU.ERR".
       COPY "ACTI.ERR".
       COPY "INSDIR.ERR".
       COPY "RES127.ERR".
       COPY "ARET.ERR".
       END DECLARATIVES.
       PROGRAMA SECTION.
       ABRO.
	   MOVE "LPT1" TO MIRTA
	   
	   PERFORM CONF-CAMPOS.
	   MOVE 0 TO CLAVE-1-CAP.
       INI-MOD.
	   MOVE "GCMODA" TO NOM-PAN.
	   OPEN INPUT PANTA.
	   READ PANTA.
	   CLOSE PANTA.
       INI-MOD1.
	   MOVE 0 TO SW2 COD-CLAVE.
       PANTA-INI.
	   ADD 1 TO SW2.
	   IF SW2 = 25  GO TO SIGA-CLAVE.
	   DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
	   GO TO PANTA-INI.
       ARMA-P.
	   MOVE 0 TO SW2 COD-CLAVE.
       ARMA-P1.
	   ADD 1 TO SW2.
	   IF SW2 > 25  GO TO ARMA-P-F.
	   DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
	   GO TO ARMA-P1.
       ARMA-P-F.
	   EXIT. 
       SIGA-CLAVE.
	   IF CLAVE-1-CAP NOT = 0 GO TO SIGA-INI.
	   PERFORM CLAVE-SYS.
	   PERFORM BORRA-CLAVE.
       SIGA-INI.
	   PERFORM TIT-PAN.
	   PERFORM TITU-PAN.
	   PERFORM FECHA-PAN.
	   PERFORM BORRA-PANTA.
	   PERFORM PANTA-MOD.
	    OPEN INPUT EMPRESAS MANEJO.
	   PERFORM CARGO-EMP THRU CARGO-EMP-F.
	   CLOSE EMPRESAS MANEJO.
      *-=-=-= FACTURA CON UNA SOLA CLAVE =-=-=-=--=-=-=-=-==-=
	   PERFORM CHEQUEA-CLAVE1.
	   IF COD-CLAVE = 9 GO TO INI-MOD1.
      *=-=-=- FACTURA CON DOS CLAVES -=-=-=-=-==-=-=-=-=-=-=-=
      *     PERFORM CHEQUEA-CLAVE2.
      *     IF COD-CLAVE = 9 GO TO INI-MOD1.
      *=-=-=- FACTURA CON TRES CLAVES =-=-=-=-=-=-=-=-=-=-=-=
      *     PERFORM CHEQUEA-CLAVE3.
      *     IF COD-CLAVE = 9 GO TO INI-MOD1.
      ****** CARGA ALICUOTAS ING.BRUTOS,MUNICIPALIDAD,RES123,INS.DIR**
	   OPEN INPUT ACTI.
	   MOVE 2 TO IMP-ACT.
	   MOVE ACT-PRO-PRI TO ACT-ACT.
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT CAL-ACT VAL-1-ACT
					   VAL-2-ACT.
	   MOVE POR-ACT TO POR-ACT-PRO.
	   MOVE CAL-ACT TO CAL-ACT-PRO.
	   MOVE VAL-1-ACT TO VAL-1-ACT-PRO.
	   MOVE VAL-2-ACT TO VAL-2-ACT-PRO.
	   MOVE 2 TO IMP-ACT.
	   MOVE 300 TO ACT-ACT.
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT.
	   MOVE POR-ACT TO ALI-INSDIR.
	   MOVE 2 TO IMP-ACT
	   MOVE 981 TO ACT-ACT
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT
	   END-READ
	   COMPUTE  ALI-PER(1) = POR-ACT * 1.
	   MOVE 2 TO IMP-ACT
	   MOVE 982 TO ACT-ACT
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT
	   END-READ
	   MOVE POR-ACT TO ALI-PER(2).
	   MOVE 2 TO IMP-ACT
	   MOVE 983 TO ACT-ACT
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT
	   END-READ
	   MOVE POR-ACT TO ALI-PER(3).
	   MOVE 2 TO IMP-ACT
	   MOVE 984 TO ACT-ACT
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT
	   END-READ
	   MOVE POR-ACT TO ALI-PER(4).
	   MOVE 3 TO IMP-ACT.
	   MOVE ACT-MUN-PRI TO ACT-ACT.
	   READ ACTI INVALID KEY MOVE 0 TO POR-ACT CAL-ACT VAL-1-ACT
					   VAL-2-ACT.
	   MOVE POR-ACT TO POR-ACT-MUN.
	   MOVE CAL-ACT TO CAL-ACT-MUN.
	   MOVE VAL-1-ACT TO VAL-1-ACT-MUN.
	   MOVE VAL-2-ACT TO VAL-2-ACT-MUN.
	   CLOSE ACTI.
      *****************************************************
	   PERFORM CARGO-COM THRU CARGO-COM-F.
	   PERFORM CARGO-FEC THRU CARGO-FEC-F.
	   PERFORM CARGO-ALI THRU CARGO-ALI-F.
       CARGO-MOD.
	   MOVE 0 TO SW2.
       CARGO-MOD1.
	   ADD 1 TO SW2.
	   IF SW2 = 25 GO TO CARGO-MOD2.
	   DISPLAY LIN-PAN(SW2) LINE SW2 POSITION 1.
	   GO TO CARGO-MOD1.
       CARGO-MOD2.
	   PERFORM TIT-PAN.
	   PERFORM TITU-PAN.
	   PERFORM FECHA-PAN.
	   PERFORM OPC-MODA.
	   PERFORM MEN-MODA-A.
	   MOVE 3 TO SW3.
	   ACCEPT COD1 LINE 25 POSITION TAB-COL(SW3)
		   ON EXCEPTION EXCEP GO TO TAREAS.
	   GO TO CARGO-MOD.
       TAREAS.
	   IF ESCAP GO TO INI-MOD1.
	   IF F01 GO TO FACTU.
	   IF F02 GO TO REMI.
	   IF F03 GO TO DEBI.
	   IF F04 GO TO CREDI.
	   IF F05 GO TO PRESU.
	   IF F06 GO TO PEDI. 
	   GO TO INI-MOD1.
       FACTU.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE "p/Fac.:" TO COM-1-REP.
	   MOVE "- FACTURA -" TO T-FAC-ED T-FAC1-ED.
	   MOVE 2 TO Y COD-REP.
	   MOVE " EMISION FACTURAS " TO D20-CAP.
	   MOVE "S" TO CONTROLO.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       REMI.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE "- REMITO -" TO T-FAC-ED T-FAC1-ED.
	   MOVE 7 TO Y COD-REP.
	   MOVE " EMISION REMITOS " TO D20-CAP.
	   MOVE "S" TO CONTROLO.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       DEBI.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE "c/N.D.:" TO COM-1-REP.
	   MOVE "-N.DEBITO- " TO T-FAC-ED T-FAC1-ED.
	   MOVE 5 TO Y COD-REP.
	   MOVE " EMISION N.DEBITO " TO D20-CAP.
	   MOVE "S" TO CONTROLO.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       CREDI.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE "-N.CREDITO-" TO T-FAC-ED T-FAC1-ED.
	   MOVE "S" TO CONTROLO.
	   MOVE 4 TO Y COD-REP.
	   MOVE " EMISION N.CREDITO "  TO D20-CAP.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       PRESU.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE "PRESUPUESTO" TO T-FAC-ED T-FAC1-ED.
	   MOVE "S" TO CONTROLO.
	   MOVE 1 TO Y COD-REP.
	   MOVE " EMISION PRESUPUESTO "  TO D20-CAP.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       PEDI.
	   OPEN INPUT ART02
		INPUT ART01
		INPUT MARCAS
		INPUT NUMERO
		INPUT ENVASE
		OUTPUT IMP02.
	   MOVE " PEDIDO " TO T-FAC-ED T-FAC1-ED.
	   MOVE "S" TO CONTROLO.
	   MOVE 8 TO Y COD-REP.
	   MOVE " EMISION PEDIDOS " TO D20-CAP.
	   PERFORM MODA-PAN.
	   GO TO SIGA-FACTU.
       SIGA-FACTU.
	   MOVE 4 TO SW3.
	   PERFORM BORRA.
	   PERFORM BORRA-PANTA.
	   PERFORM PANTA-FACTU.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3.
	   PERFORM MEN-MODA-S.
       COT-ABRO.
	   MOVE 1 TO NUM-MON MON-COTIZA.
	   MOVE NUM-EMP TO EMP-COTIZA.
	   MOVE DIA TO DD-COTIZA.
	   MOVE MES TO MM-COTIZA.
	   MOVE ANO TO AA-COTIZA.
	   OPEN INPUT MONEDAS.
	   READ MONEDAS INVALID KEY PERFORM ERROR-MON 500 TIMES
			DISPLAY SPACES LINE 22 POSITION 4 SIZE 25
			CLOSE MONEDAS
			PERFORM CIERRO-A
			GO TO INI-MOD1.
	   CLOSE MONEDAS.
	   OPEN INPUT COTIZA.
	   READ COTIZA INVALID KEY PERFORM ERROR-COT 500 TIMES
			DISPLAY SPACES LINE 22 POSITION 4 SIZE 25
			CLOSE COTIZA
			PERFORM CIERRO-A
			CALL "COTIZA.COB"
			CANCEL "COTIZA.COB"
			GO TO INI-MOD1.
	   CLOSE COTIZA.
	   COMPUTE COTI-VAL = VAL-COTIZA * 1.
	   GO TO ENTRE-FACTU.
       ERROR-MON.
	   DISPLAY "NO EXISTE MONEDA !!!" LINE 22 POSITION 4.
	   DISPLAY " "   LINE 22 POSITION 3.                
       ERROR-COT.
	   DISPLAY "NO EXISTE COTIZACION !!!" LINE 22 POSITION 4.
	   DISPLAY " " LINE 22 POSITION 3.
       ENTRE-FACTU.
	   GO TO CAP-CONDI.

       CAP-CONDI.
	   MOVE "<F1>RECIBO <F3>" TO MEN-1.
	   MOVE "MOV.<F4>P.ENT.  " TO MEN-2.
	   MOVE SPACES TO MEN-3.
	   PERFORM MENS-PAN.
	   PERFORM BORRA-CAP1.
	   MOVE 32 TO SW3.
	   PERFORM BORRA.
	   MOVE 31 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 CLOSE IMP02 
	     CALL "PAGOS.COB"
	     CANCEL "PAGOS.COB"
	     PERFORM ARMA-P THRU ARMA-P-F
	     PERFORM MODA-PAN
	     PERFORM BORRA-PANTA
	     PERFORM PANTA-FACTU
	     OPEN OUTPUT IMP02
	     GO TO CAP-CONDI.
	   IF F02 GO TO ENTRE-FACTU.
	   IF F03 CLOSE IMP02 
	     CALL "CONMOV.COB" 
	     CANCEL "CONMOV.COB"
	     PERFORM MODA-PAN
	     PERFORM BORRA-PANTA
	     PERFORM PANTA-FACTU
	     OPEN OUTPUT IMP02
	     GO TO CAP-CONDI.
	   IF F04 
      *       CALL "CARET.COB"
      *       CANCEL "CARET.COB"
	     PERFORM MODA-PAN
	     PERFORM BORRA-PANTA
	     PERFORM PANTA-FACTU
	     GO TO CAP-CONDI.

	   IF NUM-CAP > 50
	      IF MA-CCT NOT = "S" AND 
		 MA-CCT NOT = "s" GO TO CAP-CONDI.


	   IF NUM-CAP = 0 MOVE 32 TO SW3
		   MOVE 0 TO NUM-CONDI CONDI-A
		   MOVE "*** CONTADO ***" TO NOM-CONDI
		   DISPLAY NOM-CONDI LINE TAB-LIN(SW3) 
			       POSITION TAB-COL(SW3)
			       SIZE TAB-SIZ(SW3) REVERSE
		   MOVE NOM-CONDI TO CONDI-ED CONDI1-ED
		   GO TO CAP-CTA.
	   MOVE NUM-CAP TO NUM-CONDI CONDI-A.
	   OPEN INPUT CONDI.
	   READ CONDI INVALID KEY CLOSE CONDI GO TO ENTRE-FACTU. 
	   CLOSE CONDI. 
	   MOVE NOM-CONDI TO CONDI-ED CONDI1-ED.
	   MOVE 32 TO SW3.
	   DISPLAY NOM-CONDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			     SIZE TAB-SIZ(SW3) REVERSE.
       CAP-CTA.
	   MOVE "<F1>CAMBIA SALT" TO MEN-1.
	   MOVE "O <F3>SIN CTA. " TO MEN-2.
	   MOVE "<F10>BUSCA ALF." TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 0 TO COD-DET0.
	   IF CONTROLO = "S" DISPLAY "CON SALTO" LINE 20 POSITION 10
					REVERSE.
	   IF CONTROLO = "N" DISPLAY "SIN SALTO" LINE 20 POSITION 10 
					REVERSE.
	   MOVE SPACES TO DET-CAP0.
	   MOVE 26 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO ENTRE-FACTU.
	   IF F03 MOVE 0 TO CUENTA NUM-CAP.
	   IF F01 GO TO CAMBIO.
	   IF F09 CALL "CUENTA.COB"
		  CANCEL "CUENTA.COB"
		  GO TO CAP-CTA. 
	   MOVE NUM-EMP TO EMP-CTA.
	   MOVE SUC-EMP TO SUC-CTA.
	   MOVE 0 TO DD-V-CAP MM-V-CAP AA-V-CAP.
      ******* SE POSICIONA EN CUENTAS SEGUN CONDICION ******
      * PARA BICHO KAPPES MIRTA CTA 2001 DEUDORES
      * PARA TANO CTA 1200 DEUDORES
      * PARA BACCI CTA 2001 DEUDORES
      * PARA SOTELO CTA 1 DEUDORES 3 SUCURSALES
      * PARA VIOLLAZ CTA 1 DEUDORES 3 SUCURSALES
      * PARA SERGIO GRANTON CTA 1 DEUDORES
      * PARA FARMACIA VILLA ELISA CTA 1 DEUDORES
      *=-=-=-=-=-=- CLIENTES CONTADO =-=-=-=-=-=-=-=-==-=-=
	   IF NUM-CONDI < 31    
		MOVE 2001 TO RUBRO-CTA RUBRO-INT.
      *=-=-=-=-=-=- CLIENTES A RETIRAR =-=-=-=-=-=-=-=-=-=-
	   IF NUM-CONDI > 30 AND NUM-CONDI < 50
	       MOVE 2001 TO RUBRO-CTA RUBRO-INT.
      *=-=-=-=--=-=- BIENES DE USO =-=-=-=-=-=-=-=-=-=-=-=-
	   IF NUM-CONDI = 50 
	       MOVE 2001 TO RUBRO-CTA RUBRO-INT.
      *=-=-=-=-=-=-= CLIENTES CUENTAS CTES =-=-=-=-=-=-=-=-
	   IF NUM-CONDI > 50 MOVE 2001 TO RUBRO-CTA RUBRO-INT.
      *=-=-=-=-=-=-= REMITOS INTERNOS =-=-=-=-=-=-=-=-=-=-=
	   IF NUM-CONDI > 89 MOVE 3 TO RUBRO-CTA RUBRO-INT.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-= 
	   IF F10 MOVE NUM-EMP TO EMP-CTA
		  MOVE SUC-EMP TO SUC-CTA
		  GO TO BUSCA-CTA.
       CTA-SIGO.
	   MOVE NUM-CAP TO CTA-CAP.
	   MOVE 0 TO SUB-CUENTA.
      *=-=-VENTAS EN DEPOSITOS  A RETIRAR SOLO A =-=-=-=-=-=
      *=-=-=-=- CLIENTES CON CTAS.CTES.=-=-=-=-=-=-=-=-=-=-
      *     IF NUM-CONDI > 30 AND NUM-CONDI < 50
      *          IF CTA-CAP = 0 GO TO CAP-CTA.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   IF NUM-CONDI > 50 AND NUM-CONDI < 90
		IF CTA-CAP = 0 GO TO CAP-CTA.


       CAP-CTA0. 
	   IF CTA-CAP = 0 MOVE 0 TO CTA-ED CTA1-ED CUENTA RUBRO-CTA
			  GO TO CAP-CTAM.
	   MOVE CTA-CAP TO CUENTA CTA-ED CTA1-ED.
	   GO TO SIGA-CTA1.
       BUSCA-CTA.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-CTA.
	   MOVE ALF-CAP TO NOMBRE-C.
	   OPEN INPUT CUENTAS
	   START CUENTAS KEY IS NOT < NOMBRE-C
		 INVALID KEY CLOSE CUENTAS
			     GO TO CAP-CTA.
       BUSCA-CTA1.
	   READ CUENTAS NEXT AT END CLOSE CUENTAS GO TO CAP-CTA.
	   IF EMP-CTA NOT = NUM-EMP GO TO BUSCA-CTA1.
	   IF SUC-CTA NOT = SUC-EMP GO TO BUSCA-CTA1.
	   IF RUBRO-CTA NOT = RUBRO-INT GO TO BUSCA-CTA1.
	   MOVE 28 TO SW3.
	   DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		  SIZE TAB-SIZ(SW3).
	   MOVE 26 TO SW3.
	   DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		    SIZE TAB-SIZ(SW3). 
	   MOVE " <F2>CTA.ANTER." TO MEN-1.
	   MOVE " <F3>PROX.CTA. " TO MEN-2.
	   MOVE SPACES TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP CLOSE CUENTAS
		    PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO BUSCA-CTA2.
	   IF F03 GO TO BUSCA-CTA1.
	   GO TO BUSCA-CTA3.
       BUSCA-CTA2.
	   READ CUENTAS PREVIOUS AT END CLOSE CUENTAS GO TO CAP-CTA.
	   IF EMP-CTA NOT = NUM-EMP GO TO BUSCA-CTA2.
	   IF SUC-CTA NOT = SUC-EMP GO TO BUSCA-CTA2.
	   IF RUBRO-CTA NOT = RUBRO-INT GO TO BUSCA-CTA2.
	   MOVE 28 TO SW3.
	   DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		    SIZE TAB-SIZ(SW3).
	   MOVE 26 TO SW3.
	   DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		    SIZE TAB-SIZ(SW3).
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP CLOSE CUENTAS
		    PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO BUSCA-CTA2.
	   IF F03 GO TO BUSCA-CTA1.
	   GO TO BUSCA-CTA3.
       BUSCA-CTA3.
	   MOVE CUENTA TO NUM-CAP.
	   CLOSE CUENTAS.
	   GO TO CTA-SIGO.
       ATRA.
	   MOVE "-- EN ATRASO --" TO MEN-1.
	   MOVE " <F1> FACTURA  " TO MEN-2.
	   MOVE " <F2> VUELVE   " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO SIGA-CTA.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-CTA.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO ATRA.
       MORO.
	   MOVE "--< MOROSO >--" TO MEN-1.
	   MOVE " <F1> FACTURA " TO MEN-2.
	   MOVE " <F2> VUELVE  " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO SIGA-CTA.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-CTA.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO MORO.            
       CAMBIO.
	   IF CONTROLO = "S" MOVE "N" TO CONTROLO
	      GO TO CAP-CTA.
	   IF CONTROLO = "N" MOVE "S" TO CONTROLO
	       GO TO CAP-CTA.
       CAP-CTAM.
	   PERFORM BORRA-PANTA1.
	   PERFORM PANTA-CTA.
       CAP-CTAM1.
	   MOVE 40 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 PERFORM BORRA-PANTA1
		  PERFORM PANTA-FACTU1
		  GO TO CAP-CTA.
	   IF F03 MOVE NOM-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO NOM-M-CAP.
	   DISPLAY NOM-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		     SIZE TAB-SIZ(SW3).
       CAP-DOMM.
	   MOVE 41 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-CTAM.
	   IF F03 MOVE DOM-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO DOM-M-CAP.
	   DISPLAY DOM-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		     SIZE TAB-SIZ(SW3).
       CAP-CPOSM.
	   MOVE 42 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-DOMM.
	   IF F03 MOVE CPOS-M-CAP TO NUM-CAP.
	   MOVE NUM-CAP TO C-POS CPOS-M-CAP COD-LOCA.
	   OPEN INPUT CPOSTAL.
	   READ CPOSTAL WITH NO LOCK INVALID KEY CLOSE CPOSTAL 
			  GO TO CAP-LOCM.
	   CLOSE CPOSTAL.
	   MOVE NOM-POSTAL TO LOC-M-CAP.
	   MOVE 43 TO SW3.
	   PERFORM BORRA.
	   DISPLAY LOC-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
	   GO TO CAP-TELM.
       CAP-LOCM.
	   MOVE 43 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM  CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-CPOSM.
	   IF F03 MOVE LOC-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO LOC-M-CAP.
	   DISPLAY LOC-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		     SIZE TAB-SIZ(SW3).
       CAP-TELM.
	   MOVE 44 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-CPOSM.
	   IF F03 MOVE TEL-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO TEL-M-CAP.
	   DISPLAY TEL-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE TAB-SIZ(SW3).
       CAP-RESM.
	   MOVE "<R>RESP.INS.<N>" TO MEN-1.
	   MOVE "R.NO.INSC<C>C.F" TO MEN-2.
	   MOVE "INAL<E>EXTO/NO " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 45 TO SW3.
	   MOVE SPACES TO ALF-CAP.
	   DISPLAY "C" LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		       SIZE TAB-SIZ(SW3) BLINK REVERSE.
	   PERFORM CAP-ALF.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3.
	   PERFORM MENS-PAN.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-TELM.
	   IF F03 MOVE RES-M-CAP TO ALF-CAP.
	   IF ALF-CAP = SPACES MOVE "C" TO ALF-CAP.
	   MOVE ALF-CAP TO RES-M-CAP.
	   DISPLAY RES-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE TAB-SIZ(SW3).
	   IF RES-M-CAP = "r" MOVE "R" TO RES-M-CAP.
	   IF RES-M-CAP = "R" MOVE "RESP.INSCR." TO RESP-EDI
			      MOVE "R" TO INSCRIPTO
			      GO TO CAP-RESM1.
	   IF RES-M-CAP = "c" MOVE "C" TO RES-M-CAP.
	   IF RES-M-CAP = "C" MOVE "CONS.FINAL" TO RESP-EDI
			      MOVE "C" TO INSCRIPTO
			      MOVE SPACES TO CUIT-M-CAP
      *-=-=-=-=-=- PARA GRANTON SERGIO-SCARAZZINI -=-=-=-=-=-=-=-
      *---> CUIT TOMA DOCUMENTO <-----
      *                        GO TO CAP-RESM1.
      *=-=-=-=-=-=-=-=-=-=-= PARA EL RESTO -=-=-=-=-=-=-=-=-=-=-=
			      GO TO CAP-CTA-F.
      *=-=-=-=-=-=-=-=-=-===-=-=-=-=======-===-=======-=-=-===-=
	   IF RES-M-CAP = "n" MOVE "N" TO RES-M-CAP.
	   IF RES-M-CAP = "N" MOVE "R.no INSCR." TO RESP-EDI
			      MOVE "N" TO INSCRIPTO
			      GO TO CAP-RESM1.
	   IF RES-M-CAP = "e" MOVE "E" TO RES-M-CAP.
	   IF RES-M-CAP = "E" MOVE "EXTO/NO RES" TO RESP-EDI
			     MOVE "E" TO INSCRIPTO
			     GO TO CAP-RESM1.
	   GO TO CAP-RESM.
       CAP-RESM1.
	   MOVE 46 TO SW3.
	   PERFORM BORRA.
	   DISPLAY RESP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
       CAP-CUITM.
	   MOVE 47 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD1.
	   IF F02 GO TO CAP-RESM.
	   IF F03 MOVE CUIT-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO CUIT-M-CAP.
	   DISPLAY CUIT-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE TAB-SIZ(SW3).
       CAP-SITM.
	   IF SEC-PRO-EMP = 1 OR SEC-PRO-EMP = 5 OR
	      SEC-PRO-EMP = 6
			      MOVE 0 TO SIT-M-CAP
			      MOVE SPACES TO I-BRU-M-CAP
			      GO TO CAP-CTA-F. 
	   DISPLAY SPACES LINE 12 POSITION 23 SIZE 15
	   MOVE 48 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD.
	   IF F02 GO TO CAP-RESM.
	   IF F03 MOVE SIT-M-CAP TO NUM-CAP.
	   MOVE NUM-CAP TO SIT-M-CAP.
	   DISPLAY SIT-M-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			 SIZE TAB-SIZ(SW3).
	   DISPLAY SPACES LINE 12 POSITION 23 SIZE 15.
	   IF SIT-M-CAP = 1
	      DISPLAY " INSCRIPTO " LINE 12 POSITION 23 SIZE 15.
	   IF SIT-M-CAP = 2
	      DISPLAY " NO INSCR. " LINE 12 POSITION 23 SIZE 15.
	   IF SIT-M-CAP = 3
	      DISPLAY " EXENTO " LINE 12 POSITION 23 SIZE 15.
	   IF SIT-M-CAP = 4
	      DISPLAY " OT.JURISD. " LINE 12 POSITION 23 SIZE 15.
       CAP-I-BRUM.
	   DISPLAY SPACES LINE 12 POSITION 56 SIZE 15.
	   ACCEPT ALF-CAP LINE 12 POSITION 56 SIZE 15
		    NO BEEP TAB
		    ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRE-A GO TO INI-MOD. 
	   IF F02 GO TO CAP-SITM.
	   IF F03 MOVE I-BRU-M-CAP TO ALF-CAP.
	   MOVE ALF-CAP TO I-BRU-M-CAP.
	   DISPLAY I-BRU-M-CAP LINE 12 POSITION 56 SIZE 15.
       CAP-CTA-F.
	   MOVE 0 TO CUENTA.
	   MOVE NOM-M-CAP TO NOMBRE-C.
	   MOVE DOM-M-CAP TO DIRE.
	   MOVE CUIT-M-CAP TO IGAN.
	   MOVE TEL-M-CAP TO TEL.
	   MOVE SIT-M-CAP TO C-ING-BRUTOS.
	   MOVE I-BRU-M-CAP TO ING-BRUTOS.
	   PERFORM BORRA-PANTA1.
	   PERFORM PANTA-FACTU1.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			SIZE TAB-SIZ(SW3).
	   MOVE SPACES TO DET-CAP0.
	   GO TO SIGA-CTA.
       SIGA-CTA1.
	   OPEN INPUT CUENTAS.
	   READ CUENTAS INVALID KEY CLOSE CUENTAS GO TO CAP-CTA.
	   CLOSE CUENTAS.
	   MOVE 1 TO RUB-OPE.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   MOVE SPACES TO DET-CAP0.

      * SENA
	   IF NUM-CONDI > 50
	      IF CONDICION NOT = 51
		 DISPLAY "NO HABILITADO" LINE 25 POSITION 1 SIZE 78
		 ACCEPT QQ LINE 25 POSITION 60
		 GO TO CAP-CTA.


	   IF NUM-CONDI > 50 AND NUM-CONDI < 90
	     IF ESTADO = "A" GO TO ATRA.
	   IF NUM-CONDI > 50 AND NUM-CONDI < 90
	     IF ESTADO = "M" GO TO MORO.
	   IF NUM-CONDI > 50 AND NUM-CONDI < 90
		MOVE SALDO TO SALDO-COM.
       SIGA-CTA.
	   MOVE "CONS.FINAL" TO RESP-EDI. 
	   IF INSCRIPTO = "R" MOVE "RESP.INSCR." TO RESP-EDI.
	   IF INSCRIPTO = "N" MOVE "RESP.NO INSCR." TO RESP-EDI.
	   IF INSCRIPTO = "E" MOVE "EXTO/NO RESP." TO RESP-EDI.
	   IF INSCRIPTO = "C" MOVE "CONS.FINAL" TO RESP-EDI.
	   MOVE 29 TO SW3.
	   PERFORM BORRA.
	   DISPLAY RESP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   GO TO CAP-COMPRO.
       CAP-COMPRO.
	   MOVE 0 TO CANT-ART IMP-REP RENG-CAP REP-CAP.
	   MOVE Y TO YY.
	   IF Y = 4 MOVE 2 TO YY.
	   IF Y = 5 MOVE 2 TO YY.
	   MOVE YY TO NUM-COMP.
	   MOVE SUC-EMP TO SUC-C-CAP SUC-C-COMPRO.
      *-=-=-=-=-=-=-=-=- EMPRESA SIN SUCURSAL =(KAPPES MIRTA)-=-=
      *     MOVE 0 TO SUC-C-CAP SUC-C-COMPRO.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	  MOVE SPACES TO SUB-T-C-COMPRO SUB-T-C-CAP.
      *-=-=-=-=-=-=-=- BUSCA TIPO A-B =-=-=-=-=-=-=-=-=-=-=-=-=-==-=
	   IF YY = 2 PERFORM VEO-TIPO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=     
	   OPEN INPUT COMPRO.
	   READ COMPRO.
	   CLOSE COMPRO.
	   MOVE NUM-COMPRO TO MOV-CAP.
	   COMPUTE MOV-CAP = MOV-CAP + 1.
	   MOVE SUC-C-CAP TO SUC-C-EDI.
	   MOVE SUB-T-C-CAP TO SUB-T-C-EDI.
	   MOVE 49 TO SW3.
	   PERFORM BORRA.
	   DISPLAY SUB-T-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
	   MOVE 50 TO SW3.
	   PERFORM BORRA.
	   DISPLAY SUC-C-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
	   MOVE 8 TO SW3.
	   PERFORM BORRA. 
	   MOVE MOV-CAP TO MOV-EDI.
	   DISPLAY MOV-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3) REVERSE.
	   MOVE 1 TO RENG-CAP.
	   MOVE 7 TO SW3.
	   DISPLAY RENG-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   PERFORM LIMP-VEC.
	   MOVE 0 TO RUB-OPE CTA-OPE SUB-CTA-OPE.
	   MOVE 0 TO IMP-T-CAP TOT-CAP D-R-CAP P-D-R-CAP IVA-CAP.
	   MOVE 0 TO IMP-CAP PRE-CAP UNI-CAP PRE-U-CAP EXE-CAP.
	   MOVE 0 TO COD-IMPU-OPE(1) COD-IMPU-OPE(2) COD-IMPU-OPE(3).
	   MOVE 0 TO COD-IMPU-OPE(4) COD-IMPU-OPE(5) IMP-IMPU-OPE(1).
	   MOVE 0 TO IMP-IMPU-OPE(2) IMP-IMPU-OPE(3) IMP-IMPU-OPE(4).
	   MOVE 0 TO IMP-IMPU-OPE(5) NETO-CAP PRE-P-CAP UNI-N-CAP.
	   MOVE 0 TO IMP-N-CAP IVA-P-CAP PRE-N-CAP II-CAP CAP-II.
	   MOVE 0 TO COD-II IMP-INT EXE-T-CAP AA-V-CAP DD-V-CAP.
	   MOVE 0 TO MM-V-CAP IVA-I-SUB IVA-N-SUB TOTAL-SUB.
	   MOVE 0 TO SIN-9
	   DISPLAY SPACES LINE 23 POSITION 3 SIZE 25.
	   MOVE SPACES TO MON-ED REF-CAP TEL-ED.
	   GO TO CAP-NUME.
       VEO-TIPO.

	      CLOSE IMP02
	   MOVE SPACES TO SUB-T-C-COMPRO.
	   IF INSCRIPTO = "R" OR INSCRIPTO = "r"
		 MOVE "A" TO SUB-T-C-COMPRO SUB-T-C-CAP
		 MOVE MA-FACTURA-A TO MIRTA
		 OPEN OUTPUT IMP02.
	   IF INSCRIPTO = "N" OR INSCRIPTO = "n"
		 MOVE "A" TO SUB-T-C-COMPRO SUB-T-C-CAP
		 MOVE MA-FACTURA-A TO MIRTA
		 OPEN OUTPUT IMP02.
	   IF INSCRIPTO = "C" OR INSCRIPTO = "c"
		 MOVE "B" TO SUB-T-C-COMPRO SUB-T-C-CAP
		 MOVE MA-FACTURA-B TO MIRTA
		 OPEN OUTPUT IMP02.
	   IF INSCRIPTO = "E" OR INSCRIPTO = "e"
		 MOVE "B" TO SUB-T-C-COMPRO SUB-T-C-CAP
		 MOVE MA-FACTURA-B TO MIRTA
		 OPEN OUTPUT IMP02.
       CAP-NUME.
	   MOVE 0 TO II-CAP IMP-IVA-ART01 IMP-CAP.
	   MOVE 5 TO TIPO-ENVA.
	   DISPLAY SPACES LINE 11 POSITION 39 SIZE 15. 
      *****************************************************
      * CORTA CANTIDAD DE RENGLONES
      *****************************************************
      * PARA VIOLLAZ -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==
      *     IF Y = 7
      *       IF RENG-CAP > 40 GO TO CIERRE-FACTU.
      *     IF Y NOT = 7 
      *=-=-=-=-=-=-=PARA SOTELO-=-=-=-=-=-=-=-
      *      IF Y = 2 OR Y = 5 OR Y = 4
      *        IF SUB-T-C-CAP = "A"
      *           IF RENG-CAP > 27 GO TO CIERRE-FACTU.
      *      IF Y = 2 OR Y = 5 OR Y = 4
      *         IF SUB-T-C-CAP = "B"
      *           IF RENG-CAP > 10 GO TO CIERRE-FACTU.
      *     IF Y = 1 OR Y = 7 OR Y = 8 OR Y = 9
      *         IF RENG-CAP > 20 GO TO CIERRE-FACTU.
      *=-=-=-=-=-=-= PARA FARMACIA VILLA ELISA =-=-=-=-=
      *     IF Y = 2 OR Y = 5 OR Y = 4
      *       IF SUB-T-C-CAP = "B"
      *          IF RENG-CAP > 15 GO TO CIERRE-FACTU.
      *=-=-=-=-=-= PARA OTROS (EL RESTO) =-=-=-=-=-==-==-=-==
	      IF RENG-CAP > 20 GO TO CIERRE-FACTU.
      *=-=-=-=-=-=--=-=-=-=-=-=-=-=-==-=-=-=-==--=-=
       CAP-NUM0.
	   MOVE "<F1>CIERRA COMP" TO MEN-1.
	   MOVE "ROB.<F10>BUSCA " TO MEN-2.
	   MOVE "ALF.<F5>RES.123" TO MEN-3.
	   DISPLAY SPACES LINE 22 POSITION 19 SIZE 50. 
	   PERFORM MENS-PAN. 
	   PERFORM BORRA-CAP.
	   MOVE 7 TO SW3.
	   DISPLAY RENG-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   MOVE 0 TO NUM-CAP COD-ART02 COD-ART01.
	   MOVE 9 TO SW3.
	   MOVE SPACES TO DET-CAP C-RES123.
	   PERFORM BORRA.
	   ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3) TAB CONVERT
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 GO TO CIERRE-FACTU.
	   IF F02 GO TO CAP-CTA.
	   IF F05 MOVE "S" TO C-RES123
		  GO TO CIERRE-FACTU.
	   IF F03 MOVE 0 TO NUM-CAP GO TO CAP-DESC.
      ************* BUSQUEDA ALFABETICA ********************
	   IF F10 GO TO BUSCA-A.
	   MOVE 9 TO COD-CAP.
      ******* CARGA NOMBRE DE ARTICULO INEXISTENTE EN ARCH. ****
	   IF NUM-CAP = 0 GO TO CAP-DESC.
       ARTI-SIGA.
	   MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
	   MOVE SUC-EMP TO SUC-ART02.
      *=-=-=-=-=-=- ARTICULOS SIN MARCAS =-=-=-=-=-=--
	   MOVE 0 TO MAR-ART02.
	   MOVE NUM-CAP TO NUM-ART02 NUM-ART02-EDI.
	   START ART02 KEY IS NOT < CLAVE-ART02 INVALID KEY
		     GO TO CAP-NUME.
	   READ ART02 NEXT WITH NO LOCK AT END GO TO CAP-NUME.
	   DISPLAY NUM-ART02-EDI LINE TAB-LIN(SW3)
		 POSITION TAB-COL(SW3) SIZE TAB-SIZ(SW3).
       CAP-NUM000.
	   MOVE COD-ART02 TO COD-ART01.
      *-=-=-=-=-=- ARTICULO CON MARCAS =-=-=-=-=-=-=-
      *     MOVE NUM-CAP TO COD-ART02 COD-ART01.
      *     READ ART02 INVALID KEY GO TO CAP-NUME.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   READ ART01 WITH NO LOCK INVALID KEY GO TO CAP-NUME.
	   MOVE MAR-ART02 TO COD-MAR.
	   MOVE NUM-ART02 TO COD-NUM.
	   READ NUMERO WITH NO LOCK INVALID KEY 
		      MOVE "*******" TO DET-NUM.
	   READ MARCAS WITH NO LOCK INVALID KEY 
			MOVE "*******" TO DET-MAR.
	   MOVE ENV-ART01 TO NUM-ENVA.
	   READ ENVASE WITH NO LOCK INVALID KEY 
				   MOVE "UNIDAD" TO NOM-ENVA
				   MOVE 1 TO CANT1-ENVA CANT2-ENVA
				   MOVE 5 TO TIPO-ENVA
				   MOVE 0 TO PESO-ENVA.
	   IF PRE-V-ART02 = 0
	     IF PRE-E-ART02 NOT = 0 
	       COMPUTE PRE-V-ART02 ROUNDED = PRE-E-ART02 * COTI-VAL.
       SIGO-0.
	   MOVE 0 TO II-CAP.
      ************* COCHINADA POR IMPUESTO INTERNO ***************
	   IF IMP-II-ART01 = 1
	       MOVE PRE-E-ART02 TO II-CAP.
      ************************************************************
	   IF II-CAP NOT = 0
	     DISPLAY "IMP.INTERNO   :" LINE 13 POSITION 3
	     MOVE II-CAP TO II-EDI
	     DISPLAY II-EDI LINE 13 POSITION 19.
	   IF Y = 2 OR Y = 7 OR Y = 4 OR Y = 5 OR Y = 1
	     IF INSCRIPTO = "R" OR INSCRIPTO = "N"
	       IF IMP-IVA-ART01 NOT = 999
		COMPUTE FIN-CAP ROUNDED = PRE-V-ART02 * 1
		COMPUTE PRE-CAP ROUNDED = ( PRE-V-ART02 - II-CAP ) 
		    / ( ( IVA-1-CAP / 100 ) + 1 )
	      ELSE COMPUTE FIN-CAP ROUNDED = PRE-V-ART02 * 1
		   COMPUTE PRE-CAP ROUNDED = PRE-V-ART02 - II-CAP.
	   IF Y = 2 OR Y = 7 OR Y = 4 OR Y = 5 OR Y = 1
	     IF INSCRIPTO = "C" OR INSCRIPTO = "E"
		COMPUTE FIN-CAP ROUNDED = PRE-V-ART02 * 1
		COMPUTE PRE-CAP ROUNDED = PRE-V-ART02 - II-CAP.
	   IF Y = 8 
		COMPUTE FIN-CAP ROUNDED = PRE-V-ART02 * 1
		COMPUTE PRE-CAP ROUNDED = PRE-V-ART02 - II-CAP.
	   GO TO SIGO-NUME.
       SIGO-NUME.
	   PERFORM DESP-DESC THRU DESP-ENV.
	   PERFORM CARGO-DET.
	   MOVE SAL-S-ART02 TO STOCK-CAP STOCK-EDI.
	   DISPLAY STOCK-EDI LINE 22 POSITION 19.
	   IF STO-2 = 0
	      DISPLAY SPACES LINE 22 POSITION 26 SIZE 5.
       SIGO-NUME-1.
	   IF Y = 4 GO TO SIGO-NUME-2.
	   IF SAL-S-ART02 > 0 GO TO SIGO-NUME-2.
	   IF SAL-S-ART02 < 0 
	       DISPLAY "STOCK NEGATIVO - Verifique" LINE 22 
		  POSITION 35 BLINK REVERSE.
	   IF SAL-S-ART02 = 0
	      DISPLAY "SIN STOCK - Verifique" LINE 22 POSITION 35
			BLINK REVERSE.
	   IF C-MEN-ST = "N" GO TO SIGO-NUME-2.
	   MOVE " CONFIRME :   " TO MEN-1.
	   MOVE " <F1> FACTURA " TO MEN-2.
	   MOVE " <F2> VUELVE  " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO SIGO-NUME-2.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-NUME.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO SIGO-NUME-1.
       SIGO-NUME-2.    
	   GO TO CAP-UNI.
       BUSCA-A.
	   MOVE 9 TO SW3.
	   PERFORM BORRA.
	   MOVE 11 TO SW3.
	   PERFORM BORRA.
	   MOVE 10 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-NUME.
	   MOVE ALF-CAP TO DET-NUM.
	   START NUMERO KEY IS NOT < DET-NUM
		     INVALID KEY GO TO CAP-NUME.
       BUSCA-A1.
	   READ NUMERO NEXT AT END GO TO CAP-NUME.
	   MOVE COD-NUM TO NUM-ART02.
	   MOVE 0 TO MAR-ART02.
	   MOVE NUM-EMP TO EMP-ART02 EMP-ART01.
	   MOVE SUC-EMP TO SUC-ART02.
	   START ART02 KEY IS NOT < CLAVE-ART02
		INVALID KEY GO TO BUSCA-A1.
       BUSCA-A11.
	   READ ART02 NEXT AT END GO TO BUSCA-A1.
	   IF EMP-ART02 NOT = NUM-EMP GO TO BUSCA-A1.
	   IF SUC-ART02 NOT = SUC-EMP GO TO BUSCA-A1.
	   IF NUM-ART02 NOT = COD-NUM GO TO BUSCA-A1.
	   MOVE 9 TO SW3.
	   MOVE NUM-ART02 TO NUM-ART02-EDI.
	   DISPLAY NUM-ART02-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		  SIZE TAB-SIZ(SW3).
	   MOVE 10 TO SW3.
	   DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		  SIZE TAB-SIZ(SW3).
	   MOVE " <F2>ART.ANTER." TO MEN-1.
	   MOVE " <F3>PROX.ART. " TO MEN-2.
	   MOVE SPACES TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF F02 GO TO BUSCA-A2.
	   IF F03 GO TO BUSCA-A1.
	   GO TO CAP-NUM000.
       BUSCA-A2.
	   READ NUMERO PREVIOUS AT END GO TO CAP-NUME.
	   MOVE COD-NUM TO NUM-ART02.
	   MOVE 0 TO MAR-ART02.
	   MOVE NUM-EMP TO EMP-ART02.
	   MOVE SUC-EMP TO SUC-ART02.
	   START ART02 KEY IS NOT < CLAVE-ART02
		INVALID KEY GO TO BUSCA-A1.
       BUSCA-A21.
	   READ ART02 NEXT AT END GO TO BUSCA-A1.
	   IF EMP-ART02 NOT = NUM-EMP GO TO BUSCA-A1.
	   IF SUC-ART02 NOT = SUC-EMP GO TO BUSCA-A1.
	   IF NUM-ART02 NOT = COD-NUM GO TO BUSCA-A1.
	   MOVE 9 TO SW3.
	   MOVE NUM-ART02 TO NUM-ART02-EDI
	   DISPLAY NUM-ART02-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		  SIZE TAB-SIZ(SW3).
	   MOVE 10 TO SW3.
	   DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		  SIZE TAB-SIZ(SW3).
	   MOVE " <F2>ART.ANTER." TO MEN-1.
	   MOVE " <F3>PROX.ART. " TO MEN-2.
	   MOVE SPACES TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF F02 GO TO BUSCA-A2.
	   IF F03 GO TO BUSCA-A1.
	   GO TO CAP-NUM000.
       CAP-DESC.
	   MOVE 11 TO SW3.
	   PERFORM BORRA.
	   MOVE 10 TO SW3.
	   PERFORM BORRA.
	   MOVE 0 TO COD-ART02.
	   MOVE SPACES TO ALF-CAP.
	   PERFORM CAP-ALF.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-NUME.
	   MOVE ALF-CAP TO DET-NUM.
	   MOVE SPACES TO DET-MAR.
	   MOVE 0 TO COD-CAP LIN-1-ART01
	   MOVE 5 TO TIPO-ENVA
	   PERFORM CARGO-DET
      *=-=-=-==- INVALIDAR PARA SOTELO y FARMACIA -=-=-=-=-====
	   PERFORM CAP-ENV
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   GO TO CAP-PRE.
       CAP-PRE.
	   IF IMP-IVA-ART01 = 999
		  DISPLAY " EXENTO IVA " LINE 11 POSITION 39 
			     BLINK REVERSE.
	   MOVE "DIGITE <F10> CA" TO MEN-1.
	   MOVE "RGA IMP.INTERNO" TO MEN-2.
	   MOVE " <F9> EXENTO   " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 12 TO SW3.
	   PERFORM BORRA.
	   MOVE 0 TO COD-II.
	   ACCEPT PRE-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
			   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-DESC.
	   IF F09 MOVE 999 TO IMP-IVA-ART01
		  GO TO CAP-PRE.
	   IF F10 PERFORM CARGA-II
		  GO TO CAP-PRE.
	   COMPUTE FIN-CAP ROUNDED = PRE-CAP * 1.
	   IF Y = 2 OR Y = 7 OR Y = 4 OR Y = 5 OR Y = 1
	     IF INSCRIPTO = "R" OR INSCRIPTO = "N"
	       IF IMP-IVA-ART01 NOT = 999
		 COMPUTE PRE-CAP ROUNDED = ( PRE-CAP - II-CAP ) /
		    ( ( IVA-1-CAP / 100 ) + 1 )
		ELSE COMPUTE PRE-CAP ROUNDED = PRE-CAP - II-CAP.
	   PERFORM DESP-PRE THRU DESP-PRE-F.
       CAP-UNI.
      ********CALCULA PRECIO UNITARIO DESDE TOTAL VENTA *********
	   IF LIN-1-ART01 > 900 GO TO CAP-PRE-C.
      ***********************************************************
	   MOVE " DIGITE: <F10> " TO MEN-1.
	   MOVE " -INS.DIRECTO- " TO MEN-2.     
	   PERFORM MENS-PAN.
	   MOVE 0 TO IMP-CAP INS-DIR-COD.
       CAP-UNI1. 
	   MOVE 13 TO SW3.
	   PERFORM BORRA.
	   ACCEPT UNI-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3) NO BEEP TAB CONVERT
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 MOVE 0 TO II-CAP
		  GO TO CAP-PRE.
	   IF F10 MOVE 1 TO INS-DIR-COD
		   MOVE " **FACTURA**   " TO MEN-1
		   PERFORM MENS-PAN
		   GO TO CAP-UNI1.
	   IF UNI-CAP = 0 GO TO CAP-NUME.
	   IF NUM-CAP = 0 GO TO CAP-UNI3.
	   IF UNI-CAP < SAL-S-ART02 GO TO CAP-UNI3.
	   IF UNI-CAP = SAL-S-ART02 GO TO CAP-UNI3.
       CAP-UNI2.
	   IF C-MEN-ST = "N" GO TO CAP-UNI3.
	   DISPLAY "- SUPERA STOCK -" LINE 22 POSITION 35
			BLINK REVERSE.
	   MOVE " CONFIRME :   " TO MEN-1.
	   MOVE " <F1> FACTURA " TO MEN-2.
	   MOVE " <F2> VUELVE  " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-UNI3.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-UNI.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO CAP-UNI2.
       CAP-UNI3.          
	   MOVE UNI-CAP TO UNI-STOCK.
	   PERFORM MEN-ANTE.
       CAP-FRA.
	   IF TIPO-ENVA = 5 MOVE 5 TO T-UNI-OPE 
			    GO TO SIGA-CONDI.
	   MOVE 27 TO SW3.
	   DISPLAY "No" LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			LOW REVERSE.
	   MOVE SPACES TO FRA-CAP.
	   ACCEPT FRA-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3) TAB LOW REVERSE
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-UNI.
	   IF FRA-CAP = SPACES MOVE "N" TO FRA-CAP.
	   MOVE TIPO-ENVA TO T-UNI-OPE.
	   IF FRA-CAP = "N" OR FRA-CAP = "n"
			MOVE UNI-CAP TO UNI-STOCK 
			GO TO SIGA-CONDI.
	   DISPLAY "Si" LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			LOW REVERSE BLINK.
	   MOVE "p/FRACCION" TO NOM-ENVA.
	   IF UNI-CAP > CANT1-ENVA GO TO CAP-UNI.
	   COMPUTE UNI-STOCK ROUNDED = ( 1 / CANT1-ENVA ) * UNI-CAP. 
	   MOVE 5 TO T-UNI-OPE.
      ********** NO ME GUSTA COMO OPERA ESTA CONDICION ***********
	   MOVE 99 TO NUM-CONDI.
	   GO TO SIGA-CONDI.
       CAP-PRE-C.
      ************** CALCULA UNIDAD DESDE TOTAL VENTA ************
	   MOVE 14 TO SW3.
	   PERFORM BORRA.
	   PERFORM CARGO-IMP-C.
	   PERFORM DESP-IMP.
	   COMPUTE UNI-CAP ROUNDED = IMP-CAP / ( FIN-CAP / 10 ).
	   MOVE UNI-CAP TO UNI-STOCK.
      ************************************************************
       SIGA-CONDI.
	   OPEN INPUT CONDI.
	   READ CONDI INVALID KEY MOVE 0 TO TIP-CONDI POR-CONDI.
	   CLOSE CONDI.
      *     IF POR-CONDI = 0 GO TO FINAL-CONDI.
	   IF TIP-CONDI = 0
	     IF COND-ART02(1) NOT = 98
	       COMPUTE PRE-CAP ROUNDED = PRE-CAP - ( PRE-CAP 
			* POR-CONDI / 100 ).
	   IF TIP-CONDI = 1 
	     COMPUTE PRE-CAP ROUNDED = PRE-CAP + ( PRE-CAP 
			* POR-CONDI / 100 ).
	   IF INSCRIPTO = "R" OR INSCRIPTO = "N"
	    IF IMP-IVA-ART01 NOT = 999
	     IF Y NOT = 8
	      COMPUTE FIN-CAP ROUNDED =  PRE-CAP + II-CAP +
		  (PRE-CAP * IVA-1-CAP / 100 )
	    ELSE COMPUTE FIN-CAP ROUNDED = PRE-CAP + II-CAP.
	   IF INSCRIPTO = "C" OR INSCRIPTO = "E"
	     COMPUTE FIN-CAP ROUNDED = PRE-CAP + II-CAP.
	   COMPUTE PRE-U-CAP ROUNDED = PRE-CAP / CANT1-ENVA.
	   IF NUM-CONDI = 99 
	      COMPUTE FIN-CAP ROUNDED = FIN-CAP / CANT1-ENVA
	      COMPUTE PRE-CAP ROUNDED = PRE-CAP / CANT1-ENVA
	      COMPUTE II-CAP ROUNDED = II-CAP / CANT1-ENVA.
	FINAL-CONDI.
	   MOVE CONDI-A TO NUM-CONDI.
	   PERFORM DESP-PRE THRU DESP-PRE-F.
	   IF II-CAP NOT = 0
	     MOVE II-CAP TO II-EDI
	     DISPLAY II-EDI LINE 13 POSITION 19.
	SIGA-FRA.
	   IF LIN-1-ART01 > 900
	       COMPUTE IMP-CAP ROUNDED  = (PRE-CAP / 10) * UNI-CAP
	       COMPUTE II-CAP ROUNDED = (II-CAP / 10) * UNI-CAP
	       COMPUTE SIN-9 = SIN-9 + IMP-CAP
	    ELSE
	       COMPUTE IMP-CAP ROUNDED  = PRE-CAP * UNI-CAP
	       COMPUTE II-CAP ROUNDED = II-CAP * UNI-CAP.
	   PERFORM DESP-IMP.
	   COMPUTE TOT-STOCK ROUNDED = STOCK-CAP - UNI-STOCK.
       SIGA-FRA-1.
	   IF TOT-STOCK = 0 OR TOT-STOCK > 0
		     GO TO SIGA-FRA-2.
	   IF Y = 4 GO TO SIGA-FRA-2.
	   IF Y NOT = 4
	     IF TOT-STOCK < 0 
	       DISPLAY "** FALTA STOCK ** Verifique" LINE 22
		    POSITION 35 SIZE 30 BLINK REVERSE
		    MOVE TOT-STOCK TO STOCK-EDI
		    DISPLAY STOCK-EDI LINE 22 POSITION 19
		    BLINK REVERSE.
	   IF C-MEN-ST = "N" GO TO SIGA-FRA-2.
	   MOVE " CONFIRME :   " TO MEN-1.
	   MOVE " <F1> FACTURA " TO MEN-2.
	   MOVE " <F2> VUELVE  " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO SIGA-FRA-2.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO CAP-NUME.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO SIGA-FRA-1.
       SIGA-FRA-2.
	   MOVE 0 TO REP-CAP.
      * -=-=- INVALIDA REMITO PARA VENTA A RETIRAR -=-=-=-
      *     IF NUM-CONDI > 30 AND NUM-CONDI < 50
      *            GO TO NO-F10.
      *=-=-=-=-= MENSAJE DE CONFIRMACION =-=-=-==-=-==
	   MOVE "CONFIRMA : <F1>" TO MEN-1.
      *-=-=-=-=-=- SIN REMITO  =-=-=-=-=-=-=-=-=-=-=-
      *     MOVE SPACES TO MEN-2 MEN-3.
      *=-=-=-=-=-= CON REMITO =-=-=-=-=-=-=-=-=-=-=-=-=-
	   IF MA-REM = "S" OR MA-REM = "s"
	      IF Y = 2 OR Y = 8
      *            MOVE "ENTREG. y <F10>" TO MEN-2
      *            MOVE "C/REMITO       " TO MEN-3
		  MOVE "ENT.<F5>STOCK <" TO MEN-2
		  MOVE "F10>C/REMITO   " TO MEN-3

	       ELSE MOVE " ENTREGA "     TO MEN-2
		    MOVE SPACES          TO MEN-3.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   PERFORM CONF-CAMPOS.
	   PERFORM FECHA-PAN.
	   PERFORM MENS-PAN.
       NO-F10.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  MOVE 0 TO REP-CAP
		  GO TO TOT-IMP.
      *=-=-=-=- CON REMITO  REPARTO=-=-=-=-=-=-=-=-=-=


	   IF F05 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  PERFORM SELE-X
		  GO TO TOT-IMP.



	   IF F10 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  PERFORM SELE
		  GO TO TOT-IMP.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   GO TO CAP-NUME.
       SELE-X.
	   MOVE 0 TO REP-CAP.
	   IF Y = 2 MOVE 7 TO REP-CAP.
	   IF Y = 8 MOVE 6 TO REP-CAP.
	   IF Y = 7 OR Y = 5 OR Y = 4 OR Y = 1 MOVE 0 TO REP-CAP.

       SELE.
	   MOVE 0 TO REP-CAP.
	   IF Y = 2 MOVE 9 TO REP-CAP.
	   IF Y = 8 MOVE 8 TO REP-CAP.
	   IF Y = 7 OR Y = 5 OR Y = 4 OR Y = 1 MOVE 0 TO REP-CAP.
      *=-=-=-=- INVALIDA REMITO PARA VENTA A RETIRAR =-=-=-=
      *     IF NUM-CONDI > 30 AND NUM-CONDI < 50
      *              MOVE 0 TO REP-CAP.
      *=--=-=-=-=-=-=-=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-==-=-=-=-=-
       TOT-IMP.
	   IF IMP-IVA-ART01 NOT = 999
	      COMPUTE IMP-T-CAP ROUNDED = IMP-T-CAP + IMP-CAP
	      ELSE COMPUTE EXE-T-CAP ROUNDED = EXE-T-CAP + IMP-CAP.
	   COMPUTE CAP-II ROUNDED = CAP-II +  II-CAP.
      ************** VER PARA DESPLEGAR OTROS TOTALES ***************         
	   PERFORM DESP-IMP-T.
	   MOVE 0 TO IVA-I-SUB IVA-N-SUB OT-IMP-SUB.
	   IF INSCRIPTO = "R" OR INSCRIPTO = "N"
	     IF Y NOT = 8
	       COMPUTE IVA-I-SUB ROUNDED = IMP-T-CAP * IVA-1-CAP / 100.
	   IF INSCRIPTO = "N"
	    IF NUM-CONDI NOT = 50
	     IF Y NOT = 8
	       COMPUTE IVA-N-SUB ROUNDED = (IMP-T-CAP - SIN-9) * 
			   IVA-2-CAP / 100.
      ************* SUMA BASE PARA PERCEPCION *******************
	   IF Y = 8 GO TO TOT-IMP-01.
	   MOVE C-ING-BRUTOS TO X.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "R"
	       IF C-ING-BRUTOS < 3 
		 COMPUTE OT-IMP-SUB ROUNDED = ( IMP-T-CAP + EXE-T-CAP
		      + CAP-II ) * ALI-PER(X) / 100.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "E"
	       IF C-ING-BRUTOS < 3
		 COMPUTE OT-IMP-SUB ROUNDED = ( IMP-T-CAP + EXE-T-CAP
		      + CAP-II + IVA-I-SUB ) 
			* ALI-PER(X) / 100. 
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "N"
	       IF C-ING-BRUTOS < 3 
		 COMPUTE OT-IMP-SUB ROUNDED = ( IMP-T-CAP + EXE-T-CAP
		      + CAP-II + IVA-I-SUB + IVA-N-SUB ) 
		      * ALI-PER(X) / 100.
       TOT-IMP-01.
	   COMPUTE TOTAL-SUB = IMP-T-CAP + EXE-T-CAP + CAP-II +
		   IVA-I-SUB + IVA-N-SUB + OT-IMP-SUB.
	   PERFORM DESP-EXE-S THRU DESP-TOT-S.
      **************** MANDA ARMAR RENGLON *********************
	   PERFORM ARMA-RENG THRU ARMA-RENG0-F.
	   ADD 1 TO RENG-CAP.
	   GO TO CAP-NUME.
       CIERRE-FACTU.
      *=-=-=--=-=-=PARA VIOLLAZ-=-=-=-=-=-=-=-=
      *     IF Y = 7
      *       IF RENG-CAP < 40
      *         MOVE 0 TO RENG-CAP MON-COTIZA
      *         MOVE 15 TO SW3
      *         GO TO CAP-TOT.
      *     IF Y NOT = 7
      *=-=-=PARA OTROS Y VIOLLAZ =-=-=-=-=-=-=-=-=-=-=-=
	    IF RENG-CAP < 20
	      MOVE 0 TO RENG-CAP MON-COTIZA
	      MOVE 15 TO SW3
	      GO TO CAP-TOT.
      *=-=-=-=- PARA FARMACIA VILLA ELISA =-=-=-=-=-
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *          IF SUB-T-C-CAP = "B" OR SUB-T-C-CAP = "A"
      *             IF RENG-CAP < 15
      *               MOVE 0 TO RENG-CAP MON-COTIZA
      *               MOVE 15 TO SW3
      *               GO TO CAP-TOT.   
      *=-=-=- PARA SOTELO =-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *       IF SUB-T-C-CAP = "A"
      *         IF RENG-CAP < 27
      *           MOVE 0 TO RENG-CAP MON-COTIZA
      *           MOVE 15 TO SW3
      *           GO TO CAP-TOT.
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *       IF SUB-T-C-CAP = "B"
      *         IF RENG-CAP < 10
      *           MOVE 0 TO RENG-CAP MON-COTIZA
      *           MOVE 15 TO SW3
      *           GO TO CAP-TOT.
      *=-=-=-=-=-=-=-=-=-=-=========-=-=-=-=-=-=
	   MOVE "DIGITE <F1> P/C" TO MEN-1.
	   MOVE "ERRAR, EXCEDE R" TO MEN-2.
	   MOVE "ENGLONES       " TO MEN-3.
	   DISPLAY SPACES LINE 22 POSITION 19 SIZE 50. 
	   PERFORM MENS-PAN. 
	   PERFORM BORRA-CAP.
	   MOVE 7 TO SW3.
	   DISPLAY RENG-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   MOVE 0 TO NUM-CAP COD-ART02 COD-ART01.
	   MOVE 9 TO SW3.
	   MOVE SPACES TO DET-CAP.
	   PERFORM BORRA.
	   ACCEPT NUM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3) TAB CONVERT
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE 0 TO RENG-CAP MON-COTIZA 
		  MOVE 15 TO SW3
		  GO TO CAP-TOT.
	   GO TO CIERRE-FACTU.
       CAP-TOT.
	   MOVE 17 TO SW3.
	   COMPUTE TOT-CAP ROUNDED = IMP-T-CAP + EXE-T-CAP.
	   GO TO CAP-IVA.           
       CAP-IVA.
	   MOVE 0 TO IVA-CAP IVA-N-CAP OT-IMP-SUB OT-IMP-CAP 
		     PER-CAP.
	   IF Y = 8 
		    MOVE IMP-T-CAP TO NETO-CAP
		    MOVE EXE-T-CAP TO D-R-CAP EXE-CAP
		    MOVE 0 TO IVA-CAP IVA-N-CAP
		    COMPUTE IMP-INT ROUNDED = CAP-II * 1
		    COMPUTE TOT-CAP = TOT-CAP + IMP-INT
		    PERFORM DESP-II
		    GO TO  SIGO-TT.
	   IF INSCRIPTO = "C" 
		   MOVE IMP-T-CAP TO NETO-CAP
		   MOVE EXE-T-CAP TO D-R-CAP EXE-CAP
		   MOVE 0 TO IVA-CAP IVA-N-CAP
		   COMPUTE IMP-INT ROUNDED = CAP-II * 1
		   COMPUTE TOT-CAP = TOT-CAP + IMP-INT
		   PERFORM DESP-II 
		   GO TO SIGO-T.
	   IF INSCRIPTO = "E"
	       MOVE IMP-T-CAP TO NETO-CAP
	       MOVE EXE-CAP TO D-R-CAP EXE-CAP
	       MOVE 0 TO IVA-CAP IVA-N-CAP
	       COMPUTE IMP-INT ROUNDED = CAP-II * 1
	      IF SEC-PRO-EMP > 1 OR SEC-PRO-EMP < 5
	       IF C-ING-BRUTOS < 3
		 COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP
			+ IMP-INT ) * ALI-PER(X) / 100
		 ELSE MOVE 0 TO OT-IMP-CAP
	       ELSE MOVE 0 TO OT-IMP-CAP.
	   IF INSCRIPTO = "E"
	      IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
		IF C-ING-BRUTOS < 3
		 COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP
			+ IMP-INT ) * ALI-PER(X) / 100.
	   IF INSCRIPTO = "E"  
	       COMPUTE TOT-CAP = TOT-CAP + IMP-INT + OT-IMP-CAP
	       PERFORM DESP-II
	       GO TO SIGO-T.
	   MOVE IMP-T-CAP TO NETO-CAP.
	   MOVE EXE-T-CAP TO D-R-CAP EXE-CAP. 
	   COMPUTE IVA-CAP ROUNDED = NETO-CAP * IVA-1-CAP / 100.
	   COMPUTE IMP-INT ROUNDED = CAP-II * 1.
	   COMPUTE TOT-CAP = TOT-CAP + IVA-CAP + IMP-INT.
	   MOVE C-ING-BRUTOS TO X.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "R"
	       IF C-ING-BRUTOS < 3
		 COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		      IMP-INT ) * ALI-PER(X) / 100.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	     IF INSCRIPTO = "R"
	       IF C-ING-BRUTOS < 3
		 COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		      IMP-INT ) * ALI-PER(X) / 100.
	   PERFORM DESP-I.
	   IF INSCRIPTO = "N"
	     IF NUM-CONDI NOT = 50 
	      COMPUTE IVA-N-CAP ROUNDED = (NETO-CAP - SIN-9) * 
			 IVA-2-CAP / 100
	      PERFORM DESP-I-N
	      COMPUTE TOT-CAP ROUNDED = TOT-CAP + IVA-N-CAP.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	      IF INSCRIPTO = "N"
		IF C-ING-BRUTOS < 3
		  COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
			IMP-INT + IVA-CAP + IVA-N-CAP ) 
			* ALI-PER(X) / 100.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	      IF INSCRIPTO = "N"
		IF C-ING-BRUTOS < 3
		  COMPUTE OT-IMP-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
			IMP-INT + IVA-CAP + IVA-N-CAP ) 
			* ALI-PER(X) / 100.
	   COMPUTE TOT-CAP ROUNDED  = TOT-CAP + OT-IMP-CAP.
	   PERFORM DESP-II.
	   GO TO SIGO-T.
       SIGO-T.
	   MOVE C-ING-BRUTOS TO X.
	   IF INSCRIPTO = "C" GO TO SIGO-TT.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	    IF INSCRIPTO = "R"
	     IF C-ING-BRUTOS < 3 
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		 IMP-INT ) * ALI-PER(X) / 100.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	    IF INSCRIPTO = "E"
	     IF C-ING-BRUTOS < 3
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		  IMP-INT + IVA-CAP ) * ALI-PER(X)
		    / 100.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	    IF INSCRIPTO = "N"
	     IF C-ING-BRUTOS < 3 
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		 IMP-INT + IVA-CAP + IVA-N-CAP ) * 
		 ALI-PER(X) / 100.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	    IF INSCRIPTO = "R"
	     IF C-ING-BRUTOS < 3 
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		 IMP-INT ) * ALI-PER(X) / 100.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	    IF INSCRIPTO = "E"
	     IF C-ING-BRUTOS < 3
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		  IMP-INT + IVA-CAP ) * ALI-PER(X)
		    / 100.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	    IF INSCRIPTO = "N"
	     IF C-ING-BRUTOS < 3 
	      COMPUTE PER-CAP ROUNDED = ( NETO-CAP + EXE-CAP +
		 IMP-INT + IVA-CAP + IVA-N-CAP ) * 
		 ALI-PER(X) / 100.
       SIGO-TT.
	   PERFORM DESP-TOT.
      *************** COLOCA CANTIDAD A LISTAR *******************8
      *-=-PARA VIOLLAZ - TANO - GRANTON -  BACCI- -=-=
      *     IF Y = 2 OR Y = 7 OR Y = 9 OR Y = 4 OR Y = 5 OR Y = 1
      *           MOVE 1 TO CAP-LIS1
      *           ELSE MOVE 1 TO CAP-LIS1.
      *-=-=-=-=-= PARA BICHO -=-=-=-==-==-=-=-=-===-=-=-=-=-=-=-=
	   IF Y = 2 OR Y = 7 OR Y = 9 OR Y = 4 OR Y = 5     
		 MOVE 1 TO CAP-LIS1
		 ELSE MOVE 1 TO CAP-LIS1.
      *=-=-=-=-=- PARA SOTELO -=-=-=-=-=-=-==-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *         IF SUB-T-C-CAP = "A" MOVE 1 TO CAP-LIS1
      *         ELSE MOVE 2 TO CAP-LIS1.
      *     IF Y = 7 OR Y = 9 OR Y = 1
      *         MOVE 2 TO CAP-LIS1.
      *=-=-=-=- PARA FARMACIA =-=-=-=-=-==-=-==-=
      *      IF Y = 2 OR Y = 4 OR Y = 5
      *         MOVE 2 TO CAP-LIS1.
      *-=-=-=-= INVALID INGRESO DE CANT.A LISTAR =-=-=-=-=-=
	   GO TO COTI.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=          
	   MOVE "DIGITE CANTIDAD" TO MEN-1.
	   MOVE " DE COMPR.A LIS" TO MEN-2.
	   MOVE "TAR -   [ ]? - " TO MEN-3.
	   PERFORM MENS-PAN. 
	   MOVE 3 TO SW3.
	   ACCEPT CAP-LIS1 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3) TAB
			   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F10 GO TO COTI.
	   IF CAP-LIS1 > 5 GO TO SIGO-T.
	   IF CAP-LIS1 NOT = 0 GO TO SIGO.
	   MOVE 0 TO TOT-CAP P-D-R-CAP D-R-CAP.
	   GO TO CAP-NUME.
       COTI.
      *=-=-=-=-=-=-= PARA SOTELO =-=-=-=-=-=-==-==
      * Me anula moneda a convertir con vencimiento
      *     GO TO VENCI.
      *=-=-=-=-=-=-=-PARA FARMACIA =-=-=-=-=-=-=-=-=-==
      * Me anula moneda a convertir sin vencimiento
      *     GO TO SIGO
      *=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
	   MOVE "DIGITE MONEDA H" TO MEN-1.
	   MOVE "A CONVERTIR ---" TO MEN-2.
	   MOVE "----->  [ ]?   " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM BORRA.
	   MOVE 0 TO MON-CAP.
	   ACCEPT MON-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE 1 TAB
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3. 
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO SIGO-T.
	   IF MON-CAP = 0 GO TO SIGO.
	   MOVE MON-CAP TO NUM-MON MON-COTIZA.
	   MOVE NUM-EMP TO EMP-COTIZA.
	   MOVE DIA TO DD-COTIZA.
	   MOVE MES TO MM-COTIZA.
	   MOVE ANO TO AA-COTIZA.
	   OPEN INPUT MONEDAS.
	   READ MONEDAS INVALID KEY PERFORM ERROR-MON 100 TIMES
				     CLOSE MONEDAS 
				     GO TO COTI.
	   CLOSE MONEDAS.
	   OPEN INPUT COTIZA.
	   READ COTIZA INVALID KEY PERFORM ERROR-COT 100 TIMES
				   CLOSE COTIZA 
				   GO TO COTI.
	   CLOSE COTIZA.
	   GO TO SIGO.
      *=-=-=- INVALIDAR CUANDO NO TIENE VENCIMIENTO =-=-=-=-=-==           
      *     COPY "RUT1.CBL".
	   

       SIGO.
	   MOVE SPACES TO REF-CAP.
      *-=-=-=-=-=-= PARA TODOS =-=-=-===-=-=====
      *     IF Y = 2 OR Y = 4 OR Y = 5 OR Y = 1
      *=-=-=-=- PARA BICHO =-=-=-=-=-=-=-=-=-=-==-
	   IF Y = 2 OR Y = 4 OR Y = 5
      *=-=-=-=-=PARA SOTELO - (NUNCA PASA) -=-=-=--=-=
      *      IF Y = 0 
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-
	     MOVE SPACES TO MEN-1
	     MOVE SPACES TO MEN-2
	     MOVE SPACES TO MEN-3
	     PERFORM MENS-PAN
	     DISPLAY "REF:" LINE 25 POSITION 2 SIZE 5
	     ACCEPT REF-CAP LINE 25 POSITION 9 SIZE 20
			    NO BEEP TAB.


      ***     RUTINA POR CAJA Y VENTAS POR TURNO
      ***     ----------------------------------

	   MOVE 0 TO LAC-IMP-CHEQ LAC-TARJ-UTI
	   IF NUM-CONDI NOT < 30 GO TO NO-CONTADO.
	   IF MA-CAJA NOT = "S" AND MA-CAJA NOT = "s" GO TO NO-CONTADO.
       DECIDE-PAGO.
	   MOVE 0 TO LAC-IMP-CHEQ LAC-TARJ-UTI
	   DISPLAY 
	   "PAGA CON : -E- EFECTIVO; -C- CHEQUE; -T- TARJETA; -O- OTRO" 
	       LINE 25 POSITION 2 SIZE 77
	   ACCEPT TIPO-PAGO LINE 25 POSITION 70
	   IF TIPO-PAGO NOT = "E" AND
	      TIPO-PAGO NOT = "C" AND
	      TIPO-PAGO NOT = "T" AND
	      TIPO-PAGO NOT = "O" GO TO DECIDE-PAGO.
	   IF TIPO-PAGO = "E" GO TO NO-CONTADO.
	   IF TIPO-PAGO = "C"
	      DISPLAY "Digite Importe cheque..:" LINE 25 POSITION 2
			   SIZE 75
	      ACCEPT LAC-IMP-CHEQ LINE 25 POSITION 40 NO BEEP TAB
		  CONVERT
	      GO TO NO-CONTADO.
       CAP-TARJETA.
	   OPEN INPUT TARJ
	   IF TIPO-PAGO = "T"
	      DISPLAY "Digite Tarjeta utilizada..:" LINE 25 POSITION 2
			   SIZE 75
	      ACCEPT LAC-TARJ-UTI LINE 25 POSITION 40 NO BEEP TAB
		  CONVERT.
	   MOVE LAC-TARJ-UTI TO TAR-NUM
	   READ TARJ INVALID KEY CLOSE TARJ GO TO CAP-TARJETA.
	   DISPLAY TAR-NOM LINE 25 POSITION 44.
	   CLOSE TARJ.


       NO-CONTADO.

      *  AGREGADO POR TEMA CAJA ---------------------------------
     
	   MOVE NUM-EMP      TO LAC-EMP
	   MOVE SUC-EMP      TO LAC-SUC
	   MOVE VV           TO LAC-TURNO
	   MOVE CONDI-A      TO LAC-CONDI
	   MOVE MOV-CAP      TO LAC-COMPROB
	   MOVE RUBRO-CTA    TO LAC-RUBRO
	   MOVE CUENTA       TO LAC-CUENTA
	   MOVE SUB-CUENTA   TO LAC-SUB-CTA
	   MOVE TOT-CAP      TO LAC-IMPORTE
	   MOVE TIPO-PAGO    TO LAC-TIPO
	   MOVE AA-CAP       TO LAC-ANO
	   MOVE MM-CAP       TO LAC-MES
	   MOVE DD-CAP       TO LAC-DIA
	   IF Y = 1 OR Y = 7  GO TO SIN-CAJA.

	   IF Y = 4 MOVE 1   TO LAC-SUMA-RESTA
	    ELSE    MOVE 0   TO LAC-SUMA-RESTA.

	   IF MA-CAJA NOT = "S" AND MA-CAJA NOT = "s" GO TO SIN-CAJA.
	   OPEN I-O LACAJA
	   WRITE REG-LACAJA INVALID KEY DISPLAY " ".
	   CLOSE LACAJA.

       SIN-CAJA.
      *  FIN AGREGADO POR TEMA CAJA ---------------------------------



	   MOVE 0 TO RENG-CAP V.
	   CLOSE ART02.
	   OPEN I-O VENTURNO ART02 ARETIRAR.
	   MOVE 1 TO V.
	   MOVE 0 TO IMP-INS.
	   PERFORM GRAVO-RENG THRU GRAVO-RENG-F.
	   PERFORM GRAVO-RENG0.
	   CLOSE VENTURNO ART02 ARETIRAR.

	   OPEN I-O COMPRO.
	   MOVE MOV-CAP TO NUM-COMPRO.
	   REWRITE REG-COMPRO.
	   CLOSE COMPRO.
	   OPEN I-O EMPRESAS.
	   READ EMPRESAS INVALID KEY MOVE 0 TO IVA-C-EMP.
	   IF Y = 2 OR Y = 4 OR Y = 5
		COMPUTE IVA-C-EMP = IVA-C-EMP + 1
		MOVE IVA-C-EMP TO MOV-IVA MOV-INS MOV-RES.
	   REWRITE REG-EMP.
	   CLOSE EMPRESAS.

	   IF MA-IVA NOT = "S" AND MA-IVA NOT = "s" GO TO NO-VA-IVA.

	   OPEN I-O IVA.
	   IF Y = 2 OR Y = 4 OR Y = 5
      *-=-=-=-=-=-=PARA KAPPES-=-=-=-=-=
      *     MOVE 2 TO SUC-C-IVA.
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
		WRITE REG-IVA.
	   CLOSE IVA.
       NO-VA-IVA.

	   IF PER-RES NOT = 0
	     IF Y = 2 OR Y = 4 OR Y = 5
		OPEN I-O RES127
		WRITE REG-RES
		CLOSE RES127.
	   IF IMP-INS NOT = 0
	     IF Y = 2 OR Y = 4 OR Y = 5
		OPEN I-O INSDIR
		WRITE REG-INS
		CLOSE INSDIR.
	   OPEN INPUT ART02.
	   MOVE CAP-LIS1 TO CONT-S. 
	   MOVE 0 TO CONT-R CONT-F.
	   MOVE SPACES TO LF-ED
	   PERFORM IMPRE-FACTU THRU IMPRE-FACTU-F CAP-LIS1 TIMES.

      *     PERFORM IMPRE-FACTU THRU IMPRE-FACTU-F CAP-LIS1 TIMES.
      *     PERFORM IMPRE-FACTU THRU IMPRE-FACTU-F CAP-LIS1 TIMES.
      *     IF INSCRIPTO = "R" 
      *        PERFORM IMPRE-FACTU THRU IMPRE-FACTU-F CAP-LIS1 TIMES.

	   MOVE 0 TO CONT-R.
	   IF NUM-CONDI > 50 AND NUM-CONDI < 90
	      PERFORM GRABA-CC THRU GRABA-CC-F
	      PERFORM SIGO-P THRU SIGO-P-F.
	   IF IMP-REP > 0 
	       MOVE 1 TO CAP-LIS1 CONT-S
	       PERFORM AVI-REMI
	       PERFORM REPA THRU REPA-F
	       IF Y = 2 OR Y = 8
		 PERFORM IMPRE-REMI THRU IMPRE-REMI-F 
      *                  CAP-LIS1 TIMES
		 MOVE 0 TO LA-FACTURA-ED 
		 MOVE SPACES TO LF-ED
		 ELSE PERFORM IMPRE-FACTU THRU IMPRE-FACTU-F 1 TIMES.
	   MOVE 0 TO IMP-REP.
	   DISPLAY SPACES LINE 20 POSITION 10 SIZE 20.
      *=-=-=-=-= PARA SOTELO  =-=-=-=-=-=-=-=-=-=-
      *     IF SUB-T-C-CAP = "A"
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-===-=-=-==
      *********************** IF CONTROLO = "S" PERFORM ESPA 20 TIMES.
	   MOVE SPACES TO LIN-IMP02.
	   WRITE LIN-IMP02 AFTER PAGE.    
		 

	   MOVE COD-REP TO Y
	   IF Y = 2 MOVE "- FACTURA -" TO T-FAC-ED T-FAC1-ED.
	   IF Y = 8 MOVE "- PEDIDO -" TO T-FAC-ED T-FAC1-ED.
	   IF Y = 1 MOVE "PRESUPUESTO" TO T-FAC-ED T-FAC1-ED.
	   IF Y = 7 MOVE "- REMITO -" TO T-FAC-ED T-FAC1-ED.
	   IF Y = 4 MOVE "-N.CREDITO-" TO T-FAC-ED T-FAC1-ED.
	   IF Y = 5 MOVE "-N.DEBITO-" TO T-FAC-ED T-FAC1-ED.

	   IF NUM-CONDI  > 50 AND NUM-CONDI < 90 
	      IF MA-PAGARE = "S" OR MA-PAGARE = "s"
		     PERFORM PAGARE THRU PAGARE-F.

	   GO TO ENTRE-FACTU.

       AVI-REMI.
      *=-=-=-=-= PARA BICHO =-=-=-=-=-=-=-=-=-=-=-=-=
	  IF Y = 2 OR Y = 8
	     MOVE SPACES TO MEN-1 MEN-2 MEN-3
	     PERFORM MENS-PAN.
      *=-=-=-= AVISA PARA REMITO DISCONTINUO - PARA TANO =-=-=-==-=
      *       MOVE SPACES TO MEN-1 MEN-2 MEN-3
      *       PERFORM MENS-PAN
      *       DISPLAY " CARGUE REMITO Y DIGITE ENTER " LINE 25
      *            POSITION 2 BLINK REVERSE
      *       ACCEPT SW2 LINE 25 POSITION 1 SIZE 1
      *       PERFORM MENS-PAN.
      *=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       REPA.
	   MOVE SUB-T-C-OPE TO COM-2-REP.
	   MOVE SUC-C-OPE TO COM-3-REP.
	   MOVE MOV-CAP TO COM-4-REP.
	   MOVE 0 TO CONT-F.
      *=-=-=-=-=- PARA REMITO PEGADO A FACTURA - BICHO =-=-=-=-=-=-
	   IF Y = 2 OR Y = 8 GO TO REPA-F.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   MOVE 9 TO Y.
	   MOVE Y TO NUM-COMP.
	   MOVE SUC-EMP TO SUC-C-COMPRO.
	   MOVE SPACES TO SUB-T-C-COMPRO.
      *=-=-=-=-=-=- EMPRESA SIN SUCURSAL -=-=-=-=-=-=-=-=-
      *     MOVE 0 TO SUC-C-COMPRO.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   OPEN I-O COMPRO.
	   READ COMPRO WITH NO LOCK.
	   ADD 1 TO NUM-COMPRO.
	   MOVE NUM-COMPRO TO MOV-CAP.
	   REWRITE REG-COMPRO INVALID KEY WRITE REG-COMPRO.
	   CLOSE COMPRO.
	   MOVE "REMITO p/REP" TO T-FAC-ED.
       REPA-F.
	   EXIT.
       ARMA-RENG.
	   IF Y NOT = 4             
	    IF NUM-CONDI > 50 AND NUM-CONDI < 90
	      COMPUTE TOTAL-CREDITO = SALDO + TOTAL-SUB
	      IF TOTAL-CREDITO > SALDO-TOP GO TO EXCESO1.
       ARMA-RENG0.
	   MOVE RENG-CAP TO V.
	   MOVE COD-ART02 TO ART-VEC(V).
	   MOVE UNI-CAP TO UNI-VEC(V).
	   MOVE UNI-STOCK TO UNI-S-VEC(V).
	   MOVE UNI-V-ART02 TO UNI-V-VEC(V).
	   MOVE CANT1-ENVA TO CANT1-VEC(V).
	   MOVE FRA-CAP TO FRA-VEC(V).
	   MOVE LIN-1-ART01 TO LIN-1-VEC(V).
	   MOVE COSTO-STOCK-ART01 TO COS-VEC(V).
	   MOVE PRE-CAP TO PRE-VEC(V)
	   MOVE IMP-CAP TO IMP-VEC(V).
	   MOVE II-CAP TO II-VEC(V).
	   MOVE DET-NUM TO N-ART-VEC(V).
	   MOVE COM-CAP TO DET-VEC(V).
	   MOVE NOM-ENVA TO ENV-VEC(V).
	   MOVE REP-CAP TO REP-VEC(V).
	   IF IMP-IVA-ART01 NOT = 999
	      MOVE IVA-1-CAP TO ALI-VEC(V)
	      ELSE MOVE 0 TO ALI-VEC(V).
      *********** CODIFICA ACTIVIDADES P/ING.BRUTOS **************
	   IF INS-DIR-COD = 1 MOVE 300 TO ACT-PRO-VEC(V)
		GO TO SIGA-ACT-MUN.
	   IF SEC-PRO-EMP = 1
	    IF IMP-PRO-ART01 = 0 MOVE ACT-PRO-PRI TO ACT-PRO-VEC(V)
	       ELSE MOVE IMP-PRO-ART01 TO ACT-PRO-VEC(V).
	   IF SEC-PRO-EMP = 1
	     IF C-RES123 = "S"
	       IF IMP-PRO-ART01 = 0 
		  COMPUTE ACT-PRO-VEC(V) = ACT-PRO-PRI + 300
		ELSE COMPUTE ACT-PRO-VEC(V) = IMP-PRO-ART01 + 300. 
	   IF SEC-PRO-EMP = 2
	    IF INSCRIPTO = "C"
	      IF IMP-PRO-ART01 = 0 
		   COMPUTE ACT-PRO-VEC(V) = ACT-PRO-PRI - 300
	       ELSE COMPUTE ACT-PRO-VEC(V) = IMP-PRO-ART01 - 300.
	   IF SEC-PRO-EMP = 2
	    IF INSCRIPTO NOT = "C"
	      IF IMP-PRO-ART01 = 0
		 MOVE ACT-PRO-PRI TO ACT-PRO-VEC(V)
	       ELSE MOVE IMP-PRO-ART01 TO ACT-PRO-VEC(V).
	   IF SEC-PRO-EMP > 2
	    IF INSCRIPTO = "C"
	      IF IMP-PRO-ART01 = 0
		   COMPUTE ACT-PRO-VEC(V) = ACT-PRO-PRI - 600
	       ELSE COMPUTE ACT-PRO-VEC(V) = IMP-PRO-ART01 - 600.
	   IF SEC-PRO-EMP > 2
	    IF INSCRIPTO NOT = "C"
	      IF IMP-PRO-ART01 = 0
		 MOVE ACT-PRO-PRI TO ACT-PRO-VEC(V)
	       ELSE MOVE IMP-PRO-ART01 TO ACT-PRO-VEC(V).
      ************* CODIFICA ACTIVIDADES P/MUNICIPALIDAD *********
       SIGA-ACT-MUN.
	   IF IMP-MUN-ART01 = 0 MOVE ACT-MUN-PRI TO ACT-MUN-VEC(V)
	       ELSE MOVE IMP-MUN-ART01 TO ACT-MUN-VEC(V).
	   IF REP-CAP = 8 ADD 1 TO IMP-REP.
	   IF REP-CAP = 9 ADD 1 TO IMP-REP.
       ARMA-RENG0-F.
	   EXIT.
       EXCESO1.
	   MOVE "-- EXCEDE CREDI" TO MEN-1.
	   MOVE "TO --<F1> FACTU" TO MEN-2.
	   MOVE "RA <F2> VUELVE " TO MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F01 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  GO TO ARMA-RENG0.
	   IF F02 MOVE SPACES TO MEN-1 MEN-2 MEN-3
		  PERFORM MENS-PAN
		  MOVE 0 TO TOTAL-SUB IMP-T-CAP EXE-T-CAP CAP-II
			    IVA-I-SUB IVA-N-SUB OT-IMP-SUB

		  GO TO CAP-NUME.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3
	   PERFORM MENS-PAN.
	   GO TO EXCESO1.
       GRAVO-RENG.
	   IF UNI-VEC(V) = 0 GO TO GRAVO-RENG-F.
	   MOVE NUM-EMP TO EMP-OPE EMP-ART02 EMP-INS EMP-RES EMP-VT
			   AR-EMP.
	   MOVE SUC-EMP TO SUC-OPE SUC-ART02 SUC-INS SUC-RES SUC-VT
			   AR-SUC.
	   MOVE NUM-CONDI   TO CON-VT.

	   IF Y = 4 MOVE 2 TO TIP-OPE
	     ELSE MOVE 1 TO TIP-OPE.

	   MOVE VV TO TUR-VT
	   MOVE 0  TO MAR-VT


	   IF Y = 1 MOVE 0 TO TIP-OPE.
	   MOVE Y TO T-COM-OPE.
	   MOVE NOMBRE-C TO DET-OPE.
	   MOVE IGAN TO CUIT-OPE.
	   MOVE INSCRIPTO TO RESP-OPE.
	   MOVE MOV-CAP TO COM-OPE AR-COMPROB.
	   MOVE SUB-T-C-CAP TO SUB-T-C-OPE.
	   MOVE SUC-C-CAP TO SUC-C-OPE.
	   MOVE ALI-ALI TO ALI-OPE.
	   MOVE AA-CAP TO AA-OPE AA-IMP ANO-INS ANO-VAL-INS
			  ANO-RES ANO-VAL-RES AA-VT AR-ANO.
	   MOVE MM-CAP TO MM-OPE MM-IMP MES-INS MES-VAL-INS
			  MES-RES MES-VAL-RES MM-VT AR-MES.
	   MOVE DD-CAP TO DD-OPE DIA-INS DIA-VAL-INS
			  DIA-RES DIA-VAL-RES DD-VT AR-DIA.
	   MOVE V TO REN-OPE.
	   MOVE CUENTA TO CTA-OPE AR-CTA.
	   MOVE SUB-CUENTA TO SUB-CTA-OPE AR-S-CTA.
	   MOVE ART-VEC(V) TO ART-OPE COD-ART02 ART-VT ACOMODA-ART
	   MOVE ART-ACOMODA TO AR-ARTICULO.
	   MOVE UNI-VEC(V) TO UNI-OPE AR-CANTIDAD.
	   MOVE 0 TO AR-IMP-ARE AR-RET-DEP

	   MOVE RUBRO-CTA   TO AR-RUB
	   MOVE V           TO AR-RENG
	   IF ALI-VEC(V) = 0
	      MOVE PRE-VEC(V) TO D-R-OPE 
	     ELSE MOVE PRE-VEC(V) TO PRE-OPE.
	   MOVE IMP-VEC(V) TO IMP-OPE AR-IMPORTE.
	   IF ALI-VEC(V) = 0 MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2).
	   MOVE II-VEC(V) TO IMP-IMPU-OPE(3).
	   MOVE VEN-CAP TO VEN-OPE.
	   IF ART-VEC(V) NOT = 0
		MOVE DET-VEC(V) TO DET-OPE
		ELSE MOVE N-ART-VEC(V) TO DET-OPE.
	   MOVE 0 TO IND-OPE.
      *=-=-=-=  GRABA OPERACIONES -=-=-=-=-=-=-=-==-=-=-=-=-=

	   IF MA-ESTADIS NOT = "S" AND MA-ESTADIS NOT = "s"
	      GO TO SALTE-VENTURNO.

	   READ VENTURNO INVALID KEY MOVE 0 TO UNI-VT UNI-FINAL
					       IMP-VT.
	   COMPUTE UNI-VT = UNI-VT + UNI-VEC(V)
	   COMPUTE IMP-VT = IMP-VT + IMP-VEC(V)
	   WRITE REG-VENTURNO INVALID  KEY REWRITE REG-VENTURNO.

       SALTE-VENTURNO.
	   IF REP-VEC(V) < 6 OR 
	      REP-VEC(V) > 7 GO TO SALTE-MA-STOCK.
	   IF MA-STOCK NOT = "S" AND MA-STOCK NOT = "s" 
		    GO TO SALTE-MA-STOCK.
	   WRITE REG-ARETIRAR INVALID KEY 
		  DISPLAY " " LINE 25 POSITION 78.

       SALTE-MA-STOCK.

      *     IF Y = 2 OR Y = 7 OR Y = 4 OR Y = 5
      *        WRITE REG-OPERA INVALID KEY CLOSE OPERA GO TO CAP-NUME.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=
	   IF IMP-VEC(V) NOT = 0
	       ADD 1 TO CANT-ART.
      *=-=-=-=-=-= GRABA IMPUESTOS =-=-=-=-=-=-=-=-=-=-=-=-=-=-==
	   OPEN INPUT ACTI.
	   MOVE ACT-PRO-VEC(V) TO ACT-ACT.
	   MOVE 0 TO BASE-PRO BASE-MUN.
	   MOVE 2 TO IMP-ACT.
	   IF ACT-PRO-PRI NOT = ACT-PRO-VEC(V)
	       READ ACTI INVALID KEY MOVE 0 TO POR-ACT CAL-ACT VAL-1-ACT
			       VAL-2-ACT.
	   CLOSE ACTI.
	   IF ACT-PRO-PRI = ACT-PRO-VEC(V)
	      MOVE POR-ACT-PRO TO POR-ACT
	      MOVE CAL-ACT-PRO TO CAL-ACT
	      MOVE VAL-1-ACT-PRO TO VAL-1-ACT
	      MOVE VAL-2-ACT-PRO TO VAL-2-ACT.
	   MOVE NUM-EMP TO EMP-IMP.
	   MOVE SUC-EMP TO SUC-IMP.
	   MOVE 2 TO IMP-IMP.
	   MOVE ACT-PRO-VEC(V) TO ACT-IMP.
	   IF ACT-PRO-VEC(V) = 300
	       COMPUTE IMP-INS = IMP-VEC(V) + II-VEC(V). 
	   MOVE AA-CAP    TO AA-IMP
	   MOVE MM-CAP    TO MM-IMP
	   MOVE DD-CAP    TO DD-IMP
	   MOVE MOV-CAP   TO COMPRO-IMP

	   OPEN INPUT IMPU.
	   READ IMPU INVALID KEY MOVE 0 TO NET-IMP.
	   CLOSE IMPU.
	   IF INSCRIPTO = "C"
	     IF ALI-VEC(V) NOT = 0
		COMPUTE BASE-PRO ROUNDED = ( IMP-VEC(V) / ( 1 +
		   ( ALI-VEC(V) / 100 ) ) ) + II-VEC(V)
		ELSE COMPUTE BASE-PRO ROUNDED = IMP-VEC(V) + II-VEC(V).
	   IF INSCRIPTO NOT = "C"
	      COMPUTE BASE-PRO ROUNDED = IMP-VEC(V) + II-VEC(V).
	   IF CAL-ACT = 0 OR CAL-ACT = 1
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE POR-ACT TO POR-IMP
	      IF Y = 4
	       COMPUTE NET-IMP = NET-IMP - BASE-PRO
	       ELSE COMPUTE NET-IMP = NET-IMP + BASE-PRO.
	   IF CAL-ACT = 0 
	      MOVE VAL-2-ACT TO MIN-IMP
	      ELSE MOVE 0 TO MIN-IMP.
	   IF CAL-ACT = 2
	      MOVE VAL-1-ACT TO NET-IMP
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE 0 TO POR-IMP MIN-IMP.
	   IF CAL-ACT = 3
	      IF IMP-VEC(V) > VAL-2-ACT
		MOVE CAL-ACT TO CAL-IMP
		MOVE 0 TO POR-IMP
		MOVE VAL-2-ACT TO MIN-IMP
		IF Y = 4
		 COMPUTE NET-IMP = NET-IMP - VAL-1-ACT
		 ELSE COMPUTE NET-IMP = NET-IMP + VAL-1-ACT.
	   IF CAL-ACT = 4
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE 0 TO POR-IMP MIN-IMP
	      IF Y = 4
	       COMPUTE NET-IMP = NET-IMP - VAL-1-ACT
	       ELSE COMPUTE NET-IMP = NET-IMP + VAL-1-ACT.
	   OPEN I-O IMPU.
	   IF Y = 2 OR Y = 5 OR Y = 4
	     REWRITE REG-IMPU INVALID KEY WRITE REG-IMPU.
	   CLOSE IMPU.
	   OPEN INPUT ACTI.
	   MOVE ACT-MUN-VEC(V) TO ACT-ACT.
	   MOVE 3 TO IMP-ACT.
	   MOVE 0 TO BASE-MUN.
	   IF ACT-MUN-PRI NOT = ACT-MUN-VEC(V)
	       READ ACTI INVALID KEY MOVE 0 TO POR-ACT CAL-ACT VAL-1-ACT
			       VAL-2-ACT.
	   CLOSE ACTI.
	   IF ACT-MUN-PRI = ACT-MUN-VEC(V)
	      MOVE POR-ACT-MUN TO POR-ACT
	      MOVE CAL-ACT-MUN TO CAL-ACT
	      MOVE VAL-1-ACT-MUN TO VAL-1-ACT
	      MOVE VAL-2-ACT-MUN TO VAL-2-ACT.
	   MOVE NUM-EMP TO EMP-IMP.
	   MOVE SUC-EMP TO SUC-IMP.
	   MOVE 3 TO IMP-IMP.
	   MOVE ACT-MUN-VEC(V) TO ACT-IMP.
	   MOVE AA-CAP    TO AA-IMP
	   MOVE MM-CAP    TO MM-IMP
	   MOVE DD-CAP    TO DD-IMP
	   MOVE MOV-CAP   TO COMPRO-IMP

	   OPEN INPUT IMPU.
	   READ IMPU INVALID KEY MOVE 0 TO NET-IMP.
	   CLOSE IMPU.
	   IF INSCRIPTO = "C"
	      IF ALI-VEC(V) NOT = 0
		COMPUTE BASE-MUN ROUNDED = ( IMP-VEC(V) / ( 1 +
		       ( ALI-VEC(V) / 100 ) ) ) + II-VEC(V)
	      ELSE COMPUTE BASE-MUN ROUNDED = IMP-VEC(V) + II-VEC(V).
	   IF INSCRIPTO NOT = "C"
	     COMPUTE BASE-MUN ROUNDED = IMP-VEC(V) + II-VEC(V). 
	   IF CAL-ACT = 0 OR CAL-ACT = 1
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE POR-ACT TO POR-IMP
	      IF Y = 4
		COMPUTE NET-IMP = NET-IMP - BASE-MUN
		ELSE COMPUTE NET-IMP = NET-IMP + BASE-MUN.
	   IF CAL-ACT = 0 
	      MOVE VAL-2-ACT TO MIN-IMP
	      ELSE MOVE 0 TO MIN-IMP.
	   IF CAL-ACT = 2
	      MOVE VAL-1-ACT TO NET-IMP
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE 0 TO POR-IMP MIN-IMP.
	   IF CAL-ACT = 3 
		 IF IMP-VEC(V) > VAL-2-ACT
		MOVE CAL-ACT TO CAL-IMP
		MOVE 0 TO POR-IMP
		MOVE VAL-2-ACT TO MIN-IMP
		IF Y = 4
		 COMPUTE NET-IMP = NET-IMP - VAL-1-ACT
		 ELSE COMPUTE NET-IMP = NET-IMP + VAL-1-ACT.
	   IF CAL-ACT = 4
	      MOVE CAL-ACT TO CAL-IMP
	      MOVE 0 TO POR-IMP MIN-IMP
	      IF Y = 4
	       COMPUTE NET-IMP = NET-IMP - VAL-1-ACT
	       ELSE COMPUTE NET-IMP = NET-IMP + VAL-1-ACT.
	   OPEN I-O IMPU.
	   IF Y = 2 OR Y = 4 OR Y = 5
	      REWRITE REG-IMPU INVALID KEY WRITE REG-IMPU.
	   CLOSE IMPU.   
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   READ ART02 INVALID KEY ADD 1 TO V
			 GO TO GRAVO-RENG.
	   IF Y = 7
	     IF NUM-CONDI = 90
	      COMPUTE SAL-S-ART02 ROUNDED = SAL-S-ART02 - UNI-S-VEC(V).
	   IF Y = 2
	     IF NUM-CONDI < 31  
	      COMPUTE SAL-S-ART02  ROUNDED = SAL-S-ART02 - UNI-S-VEC(V).
	   IF Y = 2 
	     IF NUM-CONDI > 49
	      COMPUTE SAL-S-ART02 ROUNDED = SAL-S-ART02 - UNI-S-VEC(V).
	   IF Y = 2
	     IF NUM-CONDI > 30 AND NUM-CONDI < 50
		COMPUTE ENT-S-ART02 ROUNDED = ENT-S-ART02 -
				   UNI-S-VEC(V).
	   IF Y = 8
	     COMPUTE SAL-S-ART02 ROUNDED = SAL-S-ART02 - UNI-S-VEC(V).
	   IF Y = 4
	     COMPUTE SAL-S-ART02 ROUNDED = SAL-S-ART02 + UNI-S-VEC(V).
      *********** NOTA DE DEBITO NO TOCA EL STOCK ******************
      *************** NO TOCA LA ESTADISTICA ***********************
	   IF Y = 2 OR Y = 8 
	    COMPUTE COS-V-ART02 ROUNDED = (( COS-V-ART02 * UNI-V-ART02 )
	       + ( COS-VEC(V) * UNI-S-VEC(V) )) / ( UNI-V-ART02 +
		UNI-S-VEC(V) ).
	   IF Y = 4
	    COMPUTE COS-V-ART02 ROUNDED = (( COS-V-ART02 * UNI-V-ART02 )
	       - ( COS-VEC(V) * UNI-S-VEC(V) )) / ( UNI-V-ART02 -
		UNI-S-VEC(V) ).
	   MOVE PRE-VEC(V) TO PRE-BOL.
	   IF Y = 2 OR Y = 4 OR Y = 5
	     IF INSCRIPTO = "E" OR INSCRIPTO = "C"
	      IF ALI-VEC(V) NOT = 0
	       COMPUTE PRE-BOL ROUNDED = PRE-VEC(V) / (( ALI-VEC(V) /
		     100 ) + 1 )
	       ELSE COMPUTE PRE-BOL ROUNDED = PRE-VEC(V) * 1.
	   IF FRA-VEC(V) = "S"
	       COMPUTE PRE-BOL ROUNDED = PRE-BOL * CANT1-VEC(V).
	   IF Y = 2 OR Y = 8 
	    COMPUTE VAL-V-ART02 ROUNDED = (( VAL-V-ART02 * UNI-V-ART02 )
		+ ( PRE-BOL * UNI-S-VEC(V) )) / ( UNI-V-ART02 + 
		UNI-S-VEC(V) ).
	   IF Y = 4
	    COMPUTE VAL-V-ART02 ROUNDED = (( VAL-V-ART02 * UNI-V-ART02 )
		- ( PRE-BOL * UNI-S-VEC(V) )) / ( UNI-V-ART02 -
		UNI-S-VEC(V) ).
	   IF Y = 2 OR Y = 8
	     COMPUTE UNI-V-ART02 ROUNDED = UNI-V-ART02 + UNI-S-VEC(V).
	   IF Y = 4
	     COMPUTE UNI-V-ART02 ROUNDED = UNI-V-ART02 - UNI-S-VEC(V).
	   REWRITE REG-ART02 INVALID KEY MOVE 0 TO SAL-S-ART02.
      *=-=-=-=-=-= REMITOS P/OTRAS SUCURSALES=-=-=-=-=-=-=-
	   IF Y = 7 AND NUM-CONDI > 89
	      PERFORM OTRA-SUC.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   ADD 1 TO V.
	   GO TO GRAVO-RENG.
       GRAVO-RENG-F.
	   EXIT.
       OTRA-SUC.
	   MOVE CUENTA TO SUC-ART02
	   READ ART02 INVALID KEY 
	      MOVE 0 TO SAL-S-ART02 UNI-V-ART02 COS-V-ART02 VAL-V-ART02
			ENT-S-ART02.
	   IF Y NOT = 4
	     COMPUTE SAL-S-ART02 = SAL-S-ART02 + UNI-S-VEC(V)
	     ELSE COMPUTE SAL-S-ART02 = SAL-S-ART02 - UNI-S-VEC(V).
	   REWRITE REG-ART02 INVALID KEY WRITE REG-ART02.
	   MOVE SUC-EMP TO SUC-ART02.
       GRAVO-RENG0.
	   MOVE 0 TO REN-OPE NETO-IVA NO-GRA-IVA EXE-IVA DED-IVA RET-IVA.
	   MOVE 0 TO ART-OPE IMP-1-IVA IMP-2-IVA CRE-DEB-IVA PER-IVA.
	   MOVE 0 TO UNI-OPE IMP-RES PER-RES POR-RES POR-INS.
	   MOVE NETO-CAP TO PRE-OPE NETO-IVA.
	   MOVE EXE-CAP TO EXE-IVA D-R-OPE.
	   COMPUTE IMP-OPE = NETO-CAP + EXE-CAP + IVA-CAP + IVA-N-CAP
		      + IMP-INT + PER-CAP.
	   MOVE IVA-CAP TO IMP-IMPU-OPE(1) IMP-1-IVA.
	   MOVE IVA-N-CAP TO IMP-IMPU-OPE(2) IMP-2-IVA.
	   MOVE IMP-INT TO IMP-IMPU-OPE(3) NO-GRA-IVA.
	   MOVE PER-CAP TO IMP-IMPU-OPE(4) RET-IVA PER-RES.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	     IF INSCRIPTO = "R"
	       IF C-ING-BRUTOS < 3 
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	     IF INSCRIPTO = "E"
	       IF C-ING-BRUTOS < 3
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT
			 + IVA-CAP.
	   IF SEC-PRO-EMP = 1 AND C-RES123 = "S"
	     IF INSCRIPTO = "N"
	       IF C-ING-BRUTOS < 3
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT +
		       IVA-CAP + IVA-N-CAP.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "R"
	       IF C-ING-BRUTOS < 3 
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "E"
	       IF C-ING-BRUTOS < 3
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT
			 + IVA-CAP.
	   IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
	     IF INSCRIPTO = "N"
	       IF C-ING-BRUTOS < 3
		COMPUTE IMP-RES = NETO-CAP + EXE-CAP + IMP-INT +
		       IVA-CAP + IVA-N-CAP.
	   IF X NOT = 0
	       MOVE ALI-PER(X) TO POR-RES
	       ELSE MOVE 0 TO POR-RES.
	   MOVE ALI-INSDIR TO POR-INS.
	   MOVE 1 TO IVA-IVA IVA-RES IVA-INS.
      *=-=-=-=-=  MOVIMOS A PREPO ALICUOTA IVA =-=-=-=-=-= 
	   MOVE 1 TO ALI-IVA.
	   IF NUM-CONDI = 50 MOVE 1 TO ALI-IVA.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- 
	   MOVE Y TO TIP-COM-IVA TIP-COM-INS TIP-COM-RES.
	   MOVE SUC-C-CAP TO 
	   SUC-C-IVA SUC-C-INS SUC-C-RES.
	   MOVE SUB-T-C-CAP TO SUB-T-C-IVA SUB-T-C-INS SUB-T-C-RES.
	   MOVE MOV-CAP TO COM-IVA COM-INS COM-RES.
	   MOVE RUBRO-CTA TO RUBRO-IVA RUBRO-INS RUBRO-RES.
	   MOVE CUENTA TO CUE-IVA CUE-INS CUE-RES.
	   MOVE ING-BRUTOS TO I-BRU-INS I-BRU-RES.
	   MOVE C-ING-BRUTOS TO CAT-INS CAT-RES X.
	   MOVE DET-CAP0 TO DET-OPE.
	   MOVE NOMBRE-C TO DET-OPE DESCR-IVA DESC-INS DESC-RES.
	   MOVE IGAN TO CUIT-OPE CUIT-IVA.
	   MOVE INSCRIPTO TO RESP-OPE INS-IVA.
	   IF INSCRIPTO = "C" OR INSCRIPTO = "E"
	      COMPUTE NETO-IVA ROUNDED = NETO-CAP / ( 1 + (
			 IVA-1-CAP / 100 ) )
	      COMPUTE IMP-1-IVA = NETO-CAP - NETO-IVA.
	   COMPUTE CRE-DEB-IVA = IMP-1-IVA + IMP-2-IVA.
	   IF Y = 4
	     COMPUTE NETO-IVA ROUNDED = NETO-IVA * ( 0 - 1 )
	     COMPUTE IMP-1-IVA ROUNDED = IMP-1-IVA * ( 0 - 1 )
	     COMPUTE IMP-2-IVA ROUNDED = IMP-2-IVA * ( 0 - 1 )
	     COMPUTE NO-GRA-IVA ROUNDED = NO-GRA-IVA * ( 0 - 1 )
	     COMPUTE EXE-IVA ROUNDED  = EXE-IVA * ( 0 - 1 )
	     COMPUTE CRE-DEB-IVA ROUNDED = CRE-DEB-IVA * ( 0 - 1 )
	     COMPUTE RET-IVA ROUNDED = RET-IVA * ( 0 - 1 )
	     COMPUTE IMP-INS ROUNDED = IMP-INS * ( 0 - 1 )
	     COMPUTE IMP-RES ROUNDED = IMP-RES * ( 0 - 1 )
	     COMPUTE PER-RES ROUNDED = PER-RES * ( 0 - 1 ).
	   MOVE 0 TO IND-OPE.
      *=-=-=-=-=-=  GRABA OPERACION =-=-=-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 7 OR Y = 4 OR Y = 5 
      *        WRITE REG-OPERA INVALID KEY CLOSE OPERA 
      *                                 GO TO CAP-NUME.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       GRABA-CC.
	   IF Y = 7 OR Y = 1 OR Y = 8 OR Y = 3
	       GO TO GRABA-CC-F.
	   MOVE NUM-EMP TO EMP-MOV.
	   MOVE SUC-EMP TO SUC-EMP-MOV.
	   MOVE RUBRO-CTA TO RUBRO-MOV.
	   MOVE CUENTA TO CUENTA-MOV.
	   MOVE 0 TO SUB-CUENTA-MOV.
	   MOVE AA-CAP TO ANO-MOV.
	   MOVE MM-CAP TO MES-MOV.
	   MOVE DD-CAP TO DIA-MOV.
	   COMPUTE PMOV = PMOV + 1.
	   MOVE PMOV TO MOVIMIENTO-MOV.
	   IF Y = 2
	      MOVE 1 TO TIPO-MOV.
	   IF Y = 5
	      MOVE 1 TO TIPO-MOV.
	   IF Y = 4 
	      MOVE 2 TO TIPO-MOV.
	   MOVE Y TO TIPO-C-MOV.
	   MOVE SUB-T-C-CAP TO SUB-T-C-MOV.
	   MOVE SUC-C-CAP TO SUC-C-MOV.
	   MOVE MOV-CAP TO COMPRO-MOV.
	   MOVE IMP-OPE TO IMPORTE-MOV VALOR-PAGARE.
	   
	   MOVE "SEGUN DETALLE" TO COMENTARIO-MOV.
	   IF TIPO-MOV = 1
	       COMPUTE SALDO = SALDO + IMPORTE-MOV
	       ELSE COMPUTE SALDO = SALDO - IMPORTE-MOV.
	   IF TIPO-MOV = 2
	     IF SALDO-CAN NOT = 0
	       IF SALDO-CAN > IMPORTE-MOV
		    COMPUTE SALDO-CAN = SALDO-CAN - IMPORTE-MOV
	       ELSE MOVE 0 TO SALDO-CAN
		    MOVE "N" TO ESTADO.
	   OPEN I-O CUENTAS.
	   IF Y = 2 OR Y = 4 OR Y = 5
	     REWRITE REG-CTAS.
	   CLOSE CUENTAS.
	   OPEN I-O MOVI.
	   IF Y = 2 OR Y = 4 OR Y = 5
	      WRITE REG-MOV.
	   CLOSE MOVI.
       GRABA-CC-F.
	   EXIT.
       SIGO-P.
	   IF Y = 7 OR Y = 3 OR Y = 1 OR Y = 8 GO TO SIGO-P-F.
	   IF MA-PEND NOT = "S" AND MA-PEND NOT = "s" GO TO SIGO-P-F.


	   OPEN INPUT PENDI.
	   MOVE NUM-EMP TO EMP-PEND.
	   MOVE SUC-EMP TO SUC-EMP-PEND.
	   MOVE RUBRO-CTA TO RUBRO-PEND.
	   MOVE CUENTA TO CUENTA-PEND.
	   MOVE 0 TO SUB-CUENTA-PEND.
	   MOVE 0 TO PENDIENTE-PEND DIA-PEND MES-PEND ANO-PEND.
	   MOVE IMPORTE-MOV TO IMPORTE-INT.
	   START PENDI KEY IS NOT < CLAVE-PEND
		INVALID KEY GO TO SIGO-NUEVO.
       SIGO-P1.
	   READ PENDI NEXT AT END GO TO SIGO-NUEVO.
	   IF EMP-PEND NOT = NUM-EMP GO TO SIGO-NUEVO.
	   IF SUC-EMP-PEND NOT = SUC-EMP GO TO SIGO-NUEVO.
	   IF RUBRO-PEND NOT = RUBRO-CTA GO TO SIGO-NUEVO.
	   IF CUENTA-PEND NOT = CUENTA GO TO SIGO-NUEVO.
      *     IF SUB-CUENTA-PEND NOT = 0 GO TO SIGO-NUEVO.
	   IF D-H-PEND = TIPO-MOV GO TO SIGO-P1.
	   COMPUTE TOTAL-P-CAP = IMPORTE-PEND - ACUENTA-PEND.
	   IF Y = 2 OR Y = 4 OR Y = 5
	    IF TOTAL-P-CAP > IMPORTE-INT
	     COMPUTE ACUENTA-PEND = ACUENTA-PEND + IMPORTE-INT
	     CLOSE PENDI
	     OPEN I-O PENDI
	     REWRITE REG-PEND
	     CLOSE PENDI
	     GO TO SIGO-P-F.
	   IF Y = 2 OR Y = 4 OR Y = 5
	    IF TOTAL-P-CAP < IMPORTE-INT
	     COMPUTE IMPORTE-INT = IMPORTE-INT - TOTAL-P-CAP
	     CLOSE PENDI
	     OPEN I-O PENDI
	     DELETE PENDI
	     CLOSE PENDI
	     OPEN INPUT PENDI
	     GO TO SIGO-P1.
	   CLOSE PENDI.
	   OPEN I-O PENDI.
	   IF Y = 2 OR Y = 4 OR Y = 5
	      DELETE PENDI.
	   CLOSE PENDI.
	   GO TO SIGO-P-F.
       SIGO-NUEVO.
	   MOVE NUM-EMP TO EMP-PEND.
	   MOVE SUC-EMP TO SUC-EMP-PEND.
	   MOVE RUBRO-CTA TO RUBRO-PEND.
	   MOVE CUENTA TO CUENTA-PEND. 
	   MOVE 0 TO SUB-CUENTA-PEND.
	   COMPUTE PPEND = PPEND + 1.
	   OPEN I-O CUENTAS.
	   REWRITE REG-CTAS.
	   CLOSE CUENTAS.
	   MOVE PPEND TO PENDIENTE-PEND.
      *=-=-=-= CON VENCIMIENTO =-=-=-=-=-=-=-=
      *     MOVE DD-V-CAP TO DIA-PEND.
      *     MOVE MM-V-CAP TO MES-PEND.
      *     MOVE AA-V-CAP TO ANO-PEND.
      *=-=-=-== SIN VENCIMIENTO =-=-=-=-=-=-==
	   MOVE DIA-MOV TO DIA-PEND.
	   MOVE MES-MOV TO MES-PEND.
	   MOVE ANO-MOV TO ANO-PEND.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-==
	   MOVE TIPO-MOV TO D-H-PEND.
	   MOVE TIPO-C-MOV TO TIPO-PEND.
	   MOVE SUB-T-C-MOV TO SUB-T-PEND.
	   MOVE SUC-C-MOV TO SUC-T-PEND.
	   MOVE COMPRO-MOV TO COMPRO-PEND.
	   MOVE "SEGUN DETALLE" TO DETA-PEND.
	   MOVE IMPORTE-INT TO IMPORTE-PEND.
	   MOVE 0 TO ACUENTA-PEND.
	   MOVE DIA-MOV TO DIA-EMI-PEND.
	   MOVE MES-MOV TO MES-EMI-PEND.
	   MOVE ANO-MOV TO ANO-EMI-PEND.
	   MOVE PMOV TO REMATE-PEND.
	   CLOSE PENDI.
	   OPEN I-O PENDI.
	   IF Y = 2 OR Y = 4 OR Y = 5
	     WRITE REG-PEND.
	   CLOSE PENDI.
       SIGO-P-F.
	   EXIT.
       VERI-II.
	   MOVE 0 TO COD-II.
	   IF IMP-II-ART01 = 1 MOVE 9 TO COD-II.
	   IF IMP-II-ART01 = 1 MOVE 9 TO COD-II.
	   IF IMP-II-ART01 = 1 MOVE 9 TO COD-II.
	   IF IMP-II-ART01 = 1 MOVE 9 TO COD-II.
	   IF IMP-II-ART01 = 1 MOVE 9 TO COD-II.
	   IF IMP-II-ART01 = 0 MOVE 8 TO COD-II.
	   IF IMP-II-ART01 = 0 MOVE 8 TO COD-II.
	   IF IMP-II-ART01 = 0 MOVE 8 TO COD-II.
	   IF IMP-II-ART01 = 0 MOVE 8 TO COD-II.
	   IF IMP-II-ART01 = 0 MOVE 8 TO COD-II.
       VERI-II-F.
	   EXIT.
       CAP-ENV.
	   MOVE 25 TO SW3.
	   PERFORM BORRA.
	   ACCEPT NOM-ENVA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE TAB-SIZ(SW3) TAB.
       CAP-DET0.
	   MOVE 28 TO SW3.
	   MOVE SPACES TO DET-CAP0.
	   PERFORM BORRA.
	   ACCEPT DET-CAP0 LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		     SIZE 20 NO BEEP TAB.
	   MOVE DET-CAP0 TO NOMBRE-C.
	   START CUENTAS KEY IS NOT < NOMBRE-C
		 INVALID KEY GO TO CAP-DET0.
       CAP-DET1.
	   READ CUENTAS NEXT WITH NO LOCK AT END
		       MOVE "C" TO INSCRIPTO
		       GO TO SIGA-CTA.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   DISPLAY NOMBRE-C LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE 20.
	   MOVE 26 TO SW3.
	   DISPLAY CUENTA LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		      SIZE TAB-SIZ(SW3).
	   PERFORM SIGA-CTA.
	   PERFORM MEN-CONF.
	   MOVE 3 TO SW3.
	   PERFORM CAP-COD1.
	   IF F03 GO TO CAP-DET1.
	   IF F01 MOVE SPACES TO DET-CAP0 
		  GO TO SIGA-CTA1.
	   MOVE 26 TO SW3.
	   PERFORM BORRA.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   MOVE 0 TO CUENTA.
	   GO TO CAP-CTA.
       ESPA.
	   MOVE SPACES TO LIN-IMP02.
	   WRITE LIN-IMP02 AFTER 1.    
       LIMP-VEC.
      *=-=-=-=-=-=-=- PARA TODOS =-=-=-=-=-=-=-=-=-=-=-=-
	   PERFORM LIMP-V VARYING V FROM 1 BY 1 UNTIL
			     V > 20.
      *=-=-=-=-=-=-=-= PARA VIOLLAZ - SOTELO =-=-=-=-=-=-=-=-=-=-=-
      *      PERFORM LIMP-V VARYING V FROM 1 BY 1 UNTIL
      *                        V > 40.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
       LIMP-V.
	   MOVE 0 TO ART-VEC(V) UNI-VEC(V) PRE-VEC(V) IMP-VEC(V).
	   MOVE 0 TO T-UNI-VEC(V) D-R-VEC(V) COS-VEC(V) UNI-V-VEC(V).
	   MOVE 0 TO UNI-S-VEC(V) CANT1-VEC(V) REP-VEC(V).
	   MOVE 0 TO LIN-1-VEC(V) II-VEC(V) ALI-VEC(V).
	   MOVE SPACES TO N-ART-VEC(V) DET-VEC(V) ENV-VEC(V).
	   MOVE SPACES TO FRA-VEC(V) N-MAR-VEC(V).
       CARGO-DET.
	   MOVE 37 TO SW3.
	   MOVE SPACES TO COM-CAP ALF-CAP. 
	   PERFORM BORRA.
	   DISPLAY "COMENTARIO" LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
				SIZE 10 BLINK.
	   PERFORM CAP-ALF.
	   DISPLAY ALF-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			    SIZE TAB-SIZ(SW3).
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F03 MOVE SPACES TO COM-CAP.
	   MOVE ALF-CAP TO COM-CAP.
       CARGA-II.
	   MOVE 0 TO II-CAP.
	   DISPLAY "IMP.INTERNO   : " LINE 13 POSITION 3.
	   ACCEPT II-CAP LINE 13 POSITION 19
	       ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   MOVE II-CAP TO II-EDI.
	   DISPLAY II-EDI LINE 13 POSITION 19.
       CARGO-IMP-C.
	   MOVE " DIGITE: <F10> " TO MEN-1.
	   MOVE " -INS.DIRECTO- " TO MEN-2.     
	   PERFORM MENS-PAN.
	   MOVE 0 TO INS-DIR-COD.
	   MOVE 14 TO SW3.
	   ACCEPT IMP-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3) TAB
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP PERFORM CIERRO-A GO TO CARGO-MOD.
	   IF F02 GO TO CAP-NUME.
	   IF F10 MOVE 1 TO INS-DIR-COD
		  GO TO CARGO-IMP-C.
       CARGO-EMP.
	   MOVE SPACES TO MEN-1 MEN-2 MEN-3.
	   PERFORM MENS-PAN.
	   MOVE 6 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP CLOSE EMPRESAS MANEJO
			EXIT PROGRAM   
			STOP RUN.
	   IF NUM-CAP = 0 GO TO CARGO-EMP.
	   MOVE NUM-CAP TO NUM-EMP EMP-ART02 EMP-COMPRO EMP-IVA
			   EMP-RES EMP-INS MA-EMP.
	   MOVE 0 TO SUC-EMP.
	   READ EMPRESAS INVALID KEY GO TO CARGO-EMP.
	   MOVE 4 TO SW3.
	   DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
	   MOVE RAZ-SOC-EMP TO EMP-ED.
	   MOVE 5 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF ESCAP CLOSE EMPRESAS GO TO INI-MOD1.
	   IF F02 GO TO CARGO-EMP.
	   MOVE NUM-CAP TO SUC-EMP SUC-ART02 SUC-COMPRO SUC-IVA
		     SUC-RES SUC-INS MA-SUC.
	   READ EMPRESAS INVALID KEY GO TO CARGO-EMP.

      *     MOVE NUM-EMP TO LINK-EMP
      *     MOVE SUC-EMP TO LINK-SUC
      *     MOVE 2001    TO LINK-RUB

	   MOVE 4 TO SW3.
	   DISPLAY RAZ-SOC-EMP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
		       SIZE TAB-SIZ(SW3).
	   MOVE COM-1-EMP TO COM-1-EMP-ED.
	   MOVE COM-2-EMP TO COM-2-EMP-ED.
	   MOVE COM-3-EMP TO COM-3-EMP-ED.
	   MOVE COM-4-EMP TO COM-4-EMP-ED.
	   MOVE CALLE-EMP TO CALLE-EMP-ED.
	   MOVE COD-LOC-EMP TO CPOS-EMP-ED C-POS.
	   MOVE REP-LOC-EMP TO C-POS-1.
	   MOVE CUIT-EMP TO CUIT-EMP-ED.
	   MOVE IVA-EMP TO IVA-EMP-ED.
	   MOVE ING-BRUTOS-EMP TO I-BRUTOS-EMP-ED.
	   MOVE MUNI-EMP TO MUNI-EMP-ED.
	   MOVE JUB-EMP TO JUB-EMP-ED.
	   MOVE AP-JUB-EMP TO AP-JUB-EMP-ED.
	   OPEN INPUT CPOSTAL.
	   READ CPOSTAL INVALID KEY 
		  MOVE "******" TO NOM-POSTAL PRO-POSTAL.
	   CLOSE CPOSTAL.
	   MOVE PRO-POSTAL TO PRO-EMP-ED PRO-FAC-ED.
	   MOVE NOM-POSTAL TO LOC-EMP-ED LOC-FAC-ED.
	   MOVE ACT-MUN-EMP TO ACT-MUN-PRI.
	   MOVE ACT-PRO-EMP TO ACT-PRO-PRI.

	   READ MANEJO INVALID KEY DISPLAY "NO HAB. MANEJO" LINE 24
		   POSITION 1 SIZE 78
		   ACCEPT QQ
		   GO TO CARGO-EMP.


       CARGO-EMP-F.
	   EXIT.
       CARGO-ALI.
	   MOVE NUM-EMP TO EMP-ALI.
	   MOVE SUC-EMP TO SUC-ALI.
	   IF SUC-EMP = 0 MOVE 1 TO SUC-ALI.
	   PERFORM FECHA-ROT132.
	   MOVE 0 TO DD-ALI.
	   MOVE 0 TO MM-ALI.
	   MOVE 0 TO AA-ALI.
	   MOVE 1 TO ALI-ALI.
	   OPEN INPUT ALICUO.
	   START ALICUO KEY IS NOT < CLAVE-ALICUO
		   INVALID KEY CLOSE ALICUO
			       GO TO INI-MOD1.
       CARGO-ALI1.
	   READ ALICUO NEXT WITH NO LOCK AT END CLOSE ALICUO
						GO TO CARGO-ALI-F.
	   IF EMP-ALI NOT = NUM-EMP CLOSE ALICUO GO TO CARGO-ALI-F.
	   IF SUC-ALI NOT = SUC-EMP CLOSE ALICUO GO TO CARGO-ALI-F.
	   IF ALI-ALI NOT = 1 CLOSE ALICUO GO TO CARGO-ALI-F.
	   IF AA-ALI > ANO CLOSE ALICUO GO TO CARGO-ALI-F.
	   IF AA-ALI = ANO
	     IF MM-ALI > MES CLOSE ALICUO GO TO CARGO-ALI-F.
	   IF AA-ALI = ANO
	     IF MM-ALI = MES
	       IF DD-ALI > DIA CLOSE ALICUO GO TO CARGO-ALI-F.
	   MOVE IVA-1-ALI TO IVA-I IVA-1-EDI IVA-1-ED IVA-1-CAP.
	   MOVE IVA-2-ALI TO IVA-N IVA-2-EDI IVA-2-ED IVA-2-CAP.
	   GO TO CARGO-ALI1.
       CARGO-ALI-F.
	   EXIT.   
       CARGO-COM.
	   MOVE 20 TO SW3.
	   PERFORM BORRA.
	   MOVE SPACES TO ALF-CAP.
	   PERFORM CAP-ALF.
	   IF ESCAP  GO TO INI-MOD1.
	   IF ALF-CAP = SPACES MOVE ALF-CAP TO COM1
			       GO TO CARGO-COM-F.
	   MOVE ALF-CAP TO COM1.
       CARGO-COM2.
	   MOVE 21 TO SW3.
	   PERFORM BORRA.
	   MOVE SPACES TO ALF-CAP.
	   PERFORM CAP-ALF.
	   IF ESCAP  GO TO INI-MOD1.
	   IF F02 GO TO CARGO-COM.
	   IF ALF-CAP = SPACES MOVE ALF-CAP TO COM2
			       GO TO CARGO-COM-F.
	   MOVE ALF-CAP TO COM2.
       CARGO-COM3.
	   MOVE 22 TO SW3.
	   PERFORM BORRA.
	   MOVE SPACES TO ALF-CAP.
	   PERFORM CAP-ALF.
	   IF ESCAP GO TO INI-MOD1.
	   IF F02 GO TO CARGO-COM2.
	   IF ALF-CAP = SPACES MOVE ALF-CAP TO COM3
			       GO TO CARGO-COM-F.
	   MOVE ALF-CAP TO COM3.
       CARGO-COM-F.
	   EXIT.
       CARGO-FEC.
	   DISPLAY SPACES LINE 16 POSITION 12 SIZE 30.
	   MOVE 24 TO SW3.
	   PERFORM BORRA.
	   ACCEPT FEC-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE 6 TAB 
			  ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF ESCAP GO TO INI-MOD1.
	   IF F02 GO TO CARGO-COM.
	   IF DD-CAP > 31 OR DD-CAP < 1 GO TO CARGO-FEC.
	   IF MM-CAP > 12 OR MM-CAP < 1 GO TO CARGO-FEC.
	   MOVE FEC-CAP TO FEC-EDI.
	   DISPLAY DD-CAP LINE 15 POSITION 12 BLINK REVERSE.
	   DISPLAY "/" LINE 15 POSITION 14 BLINK REVERSE.
	   DISPLAY MM-CAP LINE 15 POSITION 15 BLINK REVERSE.
	   DISPLAY "/" LINE 15 POSITION 17 BLINK REVERSE.
	   DISPLAY AA-CAP LINE 15 POSITION 18 BLINK REVERSE.
	   IF AA-CAP NOT = ANO
	      DISPLAY "^^" LINE 16 POSITION 18 BLINK REVERSE
	      DISPLAY "ERROR !!!" LINE 16 POSITION 21.
	   IF MM-CAP NOT = MES 
	      DISPLAY "^^" LINE 16 POSITION 15 BLINK REVERSE
	      DISPLAY "ERROR !!!" LINE 16 POSITION 21.
	   IF DD-CAP NOT = DIA 
	      DISPLAY "^^" LINE 16 POSITION 12 BLINK REVERSE
	      DISPLAY "ERROR !!!" LINE 16 POSITION 21.
	   MOVE AA-CAP TO ANO-IVA ANO-VAL-IVA.
	   MOVE MM-CAP TO MES-IVA MES-VAL-IVA.
	   MOVE DD-CAP TO DIA-IVA DIA-VAL-IVA. 
       CARGO-VEN.
	   MOVE 30 TO SW3.
	   PERFORM BORRA.
	   PERFORM CAP-NUM.
	   IF F02 GO TO CARGO-FEC.
	   IF NUM-CAP = 0 GO TO CARGO-VEN.
	   MOVE NUM-CAP TO VEN-CAP CAJA-COMPRO.
       CARGO-C-MEN.
	   DISPLAY "  " LINE 19 POSITION 22 SIZE 2.
	   ACCEPT C-MEN-ST LINE 19 POSITION 22
		   NO BEEP TAB
		   ON EXCEPTION EXCEP MOVE SW3 TO SW3.
	   IF F02 GO TO CARGO-VEN.
	   IF F03 MOVE "S" TO C-MEN-ST.
	   IF C-MEN-ST = "n" MOVE "N" TO C-MEN-ST.
	   IF C-MEN-ST NOT = "N" MOVE "S" TO C-MEN-ST.

       CARGO-VV.
	   ACCEPT VV LINE 22 POSITION 30 NO BEEP TAB CONVERT
	   IF VV < 1 OR VV > 4 GO TO CARGO-VV.


       CARGO-FEC-F.
	   EXIT.
       IMPRE-FACTU.
	   ADD 1 TO CONT-F.
	   IF CONT-S NOT = 0
	      COMPUTE CONT-S = CONT-S - 1.
	   IF CONT-F = 1 MOVE "- ORIGINAL -" TO CANT-I-ED.
	   IF CONT-F = 2 MOVE "- DUPLICADO -" TO CANT-I-ED.
	   IF CONT-F = 3 MOVE "-TRIPLICADO-" TO CANT-I-ED.
	   IF CONT-F = 4 MOVE "CUADRUPLICADO" TO CANT-I-ED.
	   IF CONT-F = 5 MOVE "QUINTUPLICADO" TO CANT-I-ED.
      **************************     
	   MOVE 0 TO CONT-R CONT-I CONT-P.
	   MOVE MOV-CAP TO MOV-ED.
	   IF DET-CAP0 NOT = SPACES
		MOVE DET-CAP0 TO NOMBRE-C
		MOVE SPACES TO DIRE
		MOVE 0 TO COD-LOCA 
		MOVE SPACES TO IGAN TEL.
	   MOVE NOMBRE-C TO NOM-ED DET-OPE NOM1-ED NOM1-ED1.
	   MOVE DIRE TO DOM-ED DOM1-ED DOM1-ED1.
	   MOVE COD-LOCA TO C-POS CPOS-ED CPOS1-ED CPOS1-ED1.
	   MOVE COD-LOCA-1 TO C-POS-1
	   OPEN INPUT CPOSTAL.
	   READ CPOSTAL INVALID KEY 
		 MOVE LOC-M-CAP TO NOM-POSTAL PRO-POSTAL.
	   CLOSE CPOSTAL.
	   MOVE NOM-POSTAL TO LOC-ED LOC1-ED LOC1-ED1.
	   MOVE PRO-POSTAL TO PRO-ED.
	   MOVE IGAN TO IGAN-ED CUIT-OPE IGAN1-ED.
	   MOVE TEL TO TEL-ED TEL1-ED.
	   MOVE INSCRIPTO TO RESP-OPE.
	   MOVE RESP-EDI TO RES-ED RES1-ED.
	   MOVE AA-CAP TO AA-ED AA1-ED.
	   MOVE MM-CAP TO MM-ED MM1-ED.
	   MOVE DD-CAP TO DD-ED DD1-ED.
	   MOVE ING-BRUTOS TO ING-BRUTOS1-ED.
	   MOVE SPACES TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 1 MOVE "INSCRIPTO" TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 2 MOVE "NO INSCR." TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 3 MOVE "EXENTO" TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 4 MOVE "OT.JURISD." TO C-ING-BRUTOS1-ED.
      *=-=-=- CON VENCIMIENTOS =-=-=-=-=-=-=
      *     MOVE AA-V-CAP TO AA1-V-ED.
      *     MOVE MM-V-CAP TO MM1-V-ED.
      *     MOVE DD-V-CAP TO DD1-V-ED.
      *=-=-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=
	   MOVE CANT-ART TO CANT-ART-ED CANT-ART1-ED.
	   MOVE REF-CAP TO REF-ED.
      *=-=-=-= PARA TANO =-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y NOT = 2 OR Y NOT = 9 OR Y NOT = 7
      *=-=-=-=-=- PARA TODOS LOS QUE IMPRIMAN EN PAPEL -=
      *=-=-=-=-== OTROS COMPROBANTES
	   IF Y = 1 OR Y = 8
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      * PARA VIOLLAZ
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM TIT1-1
      *       WRITE LIN-IMP02 FROM TIT1-2
      *       WRITE LIN-IMP02 FROM TIT1-3
      *       WRITE LIN-IMP02 FROM TIT1-4
      *       WRITE LIN-IMP02 FROM LIN-10
      *       ADD 13 TO CONT-R.
      *-=-=-=-=- PARA OTROS QUE ESCRIBAN EN PAPEL =-=-=-=-=
      *=-=-=-=-= OTROS COMPROBANTES
	     WRITE LIN-IMP02 FROM TIT-1
	     WRITE LIN-IMP02 FROM TIT-2
	     WRITE LIN-IMP02 FROM TIT-3
	     WRITE LIN-IMP02 FROM TIT-4
	     WRITE LIN-IMP02 FROM TIT-5
	     WRITE LIN-IMP02 FROM TIT-6
	     WRITE LIN-IMP02 FROM LIN-10
	     WRITE LIN-IMP02 FROM LIN-10
	     WRITE LIN-IMP02 FROM LIN-10
	     WRITE LIN-IMP02 FROM TIT-7
	     WRITE LIN-IMP02 FROM LIN-10
	     WRITE LIN-IMP02 FROM TIT-8
	     ADD 12 TO CONT-R
	     WRITE LIN-IMP02 FROM IMP-151
	     WRITE LIN-IMP02 FROM IMP-15
	     WRITE LIN-IMP02 FROM IMP-151
	     ADD 3 TO CONT-R
	     GO TO IMPRE-F0.
	     
      *       COPY "R2-BACC.CBL".
      *       COPY "R2-SCAR.CBL".
      
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * PARA F.JOSE SCARAZZINI 8 LIN-10 - VIOLLAZ
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y = 2
      *  FALTA AGREGAR RENGLONES
      *-=-=-=-=-=PARA GRANTON 8 LIN-10 =-=-=-=-=-=-=-=-=             
	     

	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   IF Y = 4 OR Y = 5
	      WRITE LIN-IMP02 FROM TIT0-0
	      ELSE WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM TIT1-1.
	   WRITE LIN-IMP02 FROM TIT1-2.
	   WRITE LIN-IMP02 FROM TIT1-3.
	   WRITE LIN-IMP02 FROM TIT1-4.
	   ADD 12 TO CONT-R.
	   WRITE LIN-IMP02 FROM LIN-10.      
	   WRITE LIN-IMP02 FROM IMP-15.
	   WRITE LIN-IMP02 FROM LIN-10.
	   ADD 3 TO CONT-R.
	   
      *     COPY "R3-FARMA.CBL".
      *     COPY "R3-SOTE.CBL".
      
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      * IMPRESION YUSTY PARA TANO
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=           
	   

       IMPRE-F0.
	   MOVE SPACES TO DET-CAP0.
	   MOVE 0 TO CONT-REP CANT-ART. 
	   MOVE 1 TO V.
       IMPRE-F1.
      *=-=-=-=- PARA FARMACIA VILLA ELISA =-=-=-=-
      *     IF V > 15 GO TO IMPRE-FIN.
      *=-=-=-=-= PARA VIOLLAZ =-=-=-=-=-=-=-=-
      *     IF Y = 7
      *       IF V > 40 GO TO IMPRE-FIN.
      *     IF Y NOT = 7
      *=-=-=-=-=-=PARA TODOS Y VIOLLAZ =-=-=-=-=-=-=
	     IF V > 20 GO TO IMPRE-FIN.
      *=-=-=-=-=-=-= PARA SOTELO -=-=-=-=-=-=-=-=-=-=-=
      *     IF SUB-T-C-CAP = "A"
      *        IF V > 27 GO TO IMPRE-FIN.
      *     IF SUB-T-C-CAP = "B"
      *        IF V > 10 GO TO IMPRE-FIN.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   IF UNI-VEC(V) = 0 GO TO IMPRE-FIN.
	   MOVE UNI-VEC(V) TO CANT-UNI.
	   MOVE  UNI-1 TO UNI1-ED UNI11-ED UNI1-ED3.
	   IF UNI-2 = 0 MOVE SPACES TO COMA-ED COMA1-ED COMA-ED3
		 ELSE MOVE "," TO COMA-ED COMA1-ED COMA-ED3.
	   MOVE UNI-2 TO UNI2-ED UNI21-ED UNI2-ED3.
	   MOVE ART-VEC(V) TO ART-ED ART-ED3.
	   MOVE PRE-VEC(V) TO PRE-ED PRE1-ED PRE-ED3.
	   MOVE DET-VEC(V) TO DET-ED DET1-ED DET-ED3.
	   MOVE IMP-VEC(V) TO IMP-ED IMP1-ED IMP-ED3.
	   MOVE ALI-VEC(V) TO ALI-IVA-ED.
	   COMPUTE II-CAP ROUNDED = II-VEC(V) / UNI-VEC(V).
	   MOVE II-CAP TO I-INT1-ED.
	   MOVE N-ART-VEC(V) TO N-ART-ED N-ART1-ED N-ART-ED3.
	   MOVE ENV-VEC(V) TO ENV-ED.
	   IF LIN-1-VEC(V) = 999
	      MOVE 0 TO PRE-ED UNI1-ED UNI2-ED 
	      MOVE SPACES TO COMA-ED
	      COMPUTE CANT-ART = CANT-ART - 1.
	   IF IMP-VEC(V) = 0 MOVE 0 TO UNI1-ED UNI2-ED UNI11-ED UNI21-ED
			     MOVE 0 TO UNI1-ED3 UNI2-ED3
			     MOVE SPACES TO COMA-ED COMA1-ED COMA-ED3
	      ELSE ADD 1 TO CANT-ART.
      *=-=-=-=-=-=- NO IMPRIME IMPORTES =-=-=--=-=-===
      *=-=-=-=-=- EN ALGUNOS COMPROBANTES =-=-=-=-=-=-=-=-=-=
      *     IF Y = 9 MOVE 0 TO PRE-ED IMP-ED.
	   IF Y = 7 MOVE 0 TO PRE-ED IMP-ED.
      *     IF Y = 1 MOVE 0 TO PRE-ED IMP-ED.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   MOVE SPACES TO REP-ED.
      *****     IF REP-VEC(V) NOT = 0 MOVE "R" TO REP-ED.
	   IF REP-VEC(V) > 7 MOVE "R" TO REP-ED.
	   IF REP-VEC(V) = 0
	       IF Y = 9 ADD 1 TO V
			GO TO IMPRE-F1.
      *=-=-=-=-=-=- PARA TODOS =-=-=-=-=-=-=-=-=-=
	   WRITE LIN-IMP02 FROM IMP-16 AFTER 1.
      *=-=-=-=-= PARA FARMACIA VILLA ELISA =-=-=-=-=-=-
      *     WRITE LIN-IMP02 FROM IMP3-16 AFTER 1.
      *=-=-=-=-=-=- PARA SOTELO =-=-=-=-=-=-=-=-=-=
      *     IF INSCRIPTO = "R" OR INSCRIPTO = "N"
      *       IF ALI-VEC(V) NOT = 0
      *        COMPUTE FIN-INT ROUNDED = PRE-VEC(V) + II-CAP +
      *                ( PRE-VEC(V) * IVA-1-CAP / 100 )
      *        ELSE COMPUTE FIN-INT = PRE-VEC(V) + II-CAP.
      *     IF INSCRIPTO = "C" OR INSCRIPTO = "E"
      *           COMPUTE FIN-INT = PRE-VEC(V) + II-CAP.
      *     IF INSCRIPTO = "N"
      *      IF ALI-VEC(V) NOT = 0
      *        COMPUTE FIN-INT ROUNDED = FIN-INT + ( PRE-VEC(V) *
      *               IVA-2-CAP / 100 ).
      *     MOVE 0 TO II-CAP.
      *     IF SUB-T-C-CAP = "A"
      *       MOVE FIN-INT TO FIN1-ED
      *       WRITE LIN-IMP02 FROM IMP1-16 AFTER 1.
      *     IF SUB-T-C-CAP = "B"
      *       MOVE FIN-INT TO FIN2-ED
      *       MOVE UNI11-ED TO UNI12-ED
      *       MOVE COMA1-ED TO COMA2-ED
      *       MOVE UNI21-ED TO UNI22-ED
      *       MOVE N-ART1-ED TO N-ART2-ED
      *       MOVE DET1-ED TO DET2-ED
      *       MOVE ALI-IVA-ED TO ALI-IVA2-ED
      *       MOVE I-INT1-ED TO I-INT2-ED
      *       MOVE PRE1-ED TO PRE2-ED
      *       MOVE IMP1-ED TO IMP2-ED
      *       WRITE LIN-IMP02 FROM IMP2-16 AFTER 1.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   ADD 1 TO V.
	   ADD 1 TO CONT-R.
	   ADD 1 TO CONT-REP.
	   GO TO IMPRE-F1.
       IMPRE-FIN.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * PARA SERGIO GRANTON MOTOS
      * PARA BACCI VICENTE RAMON
      * PARA F.JOSE SCARAZZINI
      * PARA HIERRO SAN JOSE
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
	   COMPUTE V = 21 - V
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * PARA VIOLLAZ
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      * IF Y NOT = 7
      *    COMPUTE V = 21 - V
      *    ELSE COMPUTE V = 45 - V.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      * PARA TANO
      *=-=-=-=-=-==-=-=-=-===-=-=-=-===-=-=-=-====-====-==-==
      *     COMPUTE V = 20 - CONT-REP.
      *=-PARA OTROS-==-=-==-=-=-=-=-=-=-=-=-=-=-===-==-==-=-=-=-=
      *     IF Y = 9
      *         COMPUTE V = 21 - CONT-REP.
      *=-=-PARA SOTELO-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *      IF SUB-T-C-CAP = "A"
      *        COMPUTE V = 27 - CONT-REP.
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *      IF SUB-T-C-CAP = "B"
      *        COMPUTE V = 11 - CONT-REP.
      **=-=-=PARA FARMACIA VILLA ELISA=-=-=-=-=-=-==-==
      *     COMPUTE V = 15 - CONT-REP.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=
	   PERFORM ESPA V TIMES.
	   MOVE SPACES TO COM-REP-ED.
	   MOVE CANT-ART TO CANT-ART-ED CANT-ART1-ED.
	   IF Y = 9 MOVE COM-REP TO COM-REP-ED
		    MOVE CONT-REP TO CANT-ART-ED CANT-ART1-ED.
      * =-=-=-=- PARA FARMACIA VILLA ELISA =-=-=-=-=-==
      *     GO TO IMPRE-T-FAR.
      *=-=-=-==-=-=-=-=PARA TODOS =-=-=-=-=-=-=-=-=-=-===-
	   WRITE LIN-IMP02 FROM IMP-40 AFTER 1
      *=-=-=-=-=-=- PARA SOTELO =-=-=-=-=-=-=-=-=-=-=-=-=
      *     COMPUTE NET11-ED ROUNDED = D-R-OPE + PRE-OPE.
      *     IF SUB-T-C-CAP = "A"
      *        WRITE LIN-IMP02 FROM IMP1-17 AFTER 1
      *        ADD 1 TO CONT-R
      *        GO TO IMPRE-T-SOT-A.
      *     IF SUB-T-C-CAP = "B"
      *        MOVE NET11-ED TO NET12-ED
      *        MOVE CANT-ART1-ED TO CANT-ART2-ED
      *        WRITE LIN-IMP02 FROM IMP2-17 AFTER 1
      *        ADD 1 TO CONT-R
      *        GO TO IMPRE-T-SOT-B.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-=-=
	   MOVE COM1 TO COM1-ED COM1-ED1.
	   MOVE COM2 TO COM2-ED COM2-ED1.
	   MOVE COM3 TO COM3-ED.
	   MOVE REF-CAP TO REF-ED REF-ED1.
      *=-=-=-= INVALIDAR PARA FARMACIA VILLA ELISA =-=-=-==-   
	   WRITE LIN-IMP02 FROM LIN-10.
      *=-=-=-=- VER ESTA PARTE =-=-=-=-=-=-=
      *     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2) IMP-IMPU-OPE(3).
      *=-=-=-===-=-= PARA TODOS MENO SOTELO =-=-=-=-=-=-=-
	   IF Y = 8 MOVE 0 TO NET-ED
		    GO TO SIN-II.
	   IF Y = 9 MOVE 0 TO IMP-IMPU-OPE(1)
		    GO TO SIN-II.
      *=-=-=-=-=-=-=PARA LOS QUE GRABA-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     MOVE 0 TO REN-OPE.
      *     OPEN INPUT OPERA.
      *     READ OPERA INVALID KEY DISPLAY "ERROR F" EXIT PROGRAM
      *                                              STOP RUN.
      *     CLOSE OPERA.
      *=-=-===-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-  
	   MOVE IMP-IMPU-OPE(3) TO II-ED.
	   COMPUTE NET-ED ROUNDED = PRE-OPE + D-R-OPE.
      *=-=-=-=-=-=-=- PARA COMPROBANTES SIN IMPORTES =-=-=-==
	   IF Y = 7
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.
	   IF Y = 8
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.           
	   IF Y = 9 
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==
	   IF IMP-IMPU-OPE(3) = 0 GO TO SIN-II
	      ELSE GO TO CON-II.
       SIN-II.  
	   WRITE LIN-IMP02 FROM IMP-18.
	   WRITE LIN-IMP02 FROM IMP-193.
	   ADD 2 TO CONT-R.
	   MOVE IMP-IMPU-OPE(1) TO IVA-I-ED.
	   MOVE IMP-IMPU-OPE(2) TO IVA-N-ED.
	   MOVE IMP-IMPU-OPE(4) TO RES-IMP-ED.
	   IF INSCRIPTO NOT = "C"
	      IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
		 MOVE ALI-PER(C-ING-BRUTOS) TO POR-RES-ED. 
	   IF IMP-IMPU-OPE(1) = 0 
				  WRITE LIN-IMP02 FROM IMP-192
				  ADD 1 TO CONT-R
	       ELSE WRITE LIN-IMP02 FROM IMP-19
		    ADD 1 TO CONT-R.
	   IF IMP-IMPU-OPE(2) = 0
		   WRITE LIN-IMP02 FROM IMP-201
		   ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-20
		   ADD 1 TO CONT-R. 
	   IF IMP-IMPU-OPE(4) = 0
		WRITE LIN-IMP02 FROM IMP-203
		ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-202
		   ADD 1 TO CONT-R.
	   PERFORM ESPA 1 TIMES.
	   GO TO IMPRE-TOT.
       CON-II.
	   WRITE LIN-IMP02 FROM IMP-18.
	   WRITE LIN-IMP02 FROM IMP-191.
	   ADD 2 TO CONT-R.
	   MOVE IMP-IMPU-OPE(1) TO IVA-I-ED.
	   MOVE IMP-IMPU-OPE(2) TO IVA-N-ED.
	   MOVE IMP-IMPU-OPE(4) TO RES-IMP-ED.
	   IF INSCRIPTO NOT = "C"
	      IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
		 MOVE ALI-PER(C-ING-BRUTOS) TO POR-RES-ED. 
	   IF IMP-IMPU-OPE(1) = 0 
				  WRITE LIN-IMP02 FROM IMP-192
				  ADD 1 TO CONT-R
	       ELSE WRITE LIN-IMP02 FROM IMP-19
		    ADD 1 TO CONT-R.
	   IF IMP-IMPU-OPE(2) = 0
		   WRITE LIN-IMP02 FROM IMP-201
		   ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-20
		   ADD 1 TO CONT-R. 
	   IF IMP-IMPU-OPE(4) = 0
		WRITE LIN-IMP02 FROM IMP-203
		ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-202
		   ADD 1 TO CONT-R.
	   PERFORM ESPA 1 TIMES.
	   GO TO IMPRE-TOT.
       IMPRE-TOT.
      *=-=-=-=-=- SIN IMPORTE TOTAL =-=-=-=-=-=-=-
	   IF Y = 9
	       MOVE 0 TO IMP-OPE. 
	   IF Y = 7 
	       MOVE 0 TO IMP-OPE.
      *=-=-=-=-=- PARA VIOLLAZ =-=-=-=-=-=-=-=-
      *     IF Y = 7 GO TO IMPRE-FACTU-F.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   MOVE IMP-OPE TO TOT-ED TOT-1-ED.
	   MOVE REF-CAP TO REF-ED REF-ED1.
	   IF Y NOT = 9
	     IF MON-COTIZA NOT = 0
		COMPUTE EXT-CAP ROUNDED = IMP-OPE / VAL-COTIZA
		MOVE EXT-CAP TO EXT-ED
		MOVE NOM-MON TO MON-ED
		WRITE LIN-IMP02 FROM IMP-171
	     ELSE WRITE LIN-IMP02 FROM IMP-17.
	   ADD 1 TO CONT-R.
	   IF Y = 2
	     MOVE 0 TO CONT-S.
	   IF Y NOT = 2
	     IF CONT-S NOT = 0
	       WRITE LIN-IMP02 FROM LIN-10
	       WRITE LIN-IMP02 FROM IMP-21
	       WRITE LIN-IMP02 FROM IMP-22
	       ADD 3 TO CONT-R.
	   IF Y = 1 WRITE LIN-IMP02 FROM LIN-32.
	   IF Y = 8 WRITE LIN-IMP02 FROM LIN-30.
	   IF Y = 7 WRITE LIN-IMP02 FROM LIN-33.
	   IF Y = 9 WRITE LIN-IMP02 FROM LIN-34.
	   IF Y = 2  
	     IF NUM-CONDI < 31 OR NUM-CONDI > 49 PERFORM ESPA
		ELSE WRITE LIN-IMP02 FROM LIN-35.
	   IF CONT-S NOT = 0
	      PERFORM ESPA 4 TIMES.
	   IF CONT-S = 0 MOVE 0 TO CANT-ART.
	   IF Y = 9 MOVE 2 TO Y.
      *=-=-=-=-= PIE DE FACTURA =-=-=-=-====-=-=-=-=-===-=
      *     PERFORM ESPA 6 TIMES.
      *=-=-=-=-PARA BICHO SIN PIE DE FACTURA =-=-=-=-=-=-=-=-=
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-  
	   GO TO IMPRE-FACTU-F.
	   
      *     COPY RUT4.CBL.
      *=-=-=-=-=-=-=-= PARA SOTELO =-=-=-=-==-==-=====
      

       IMPRE-FACTU-F.
	   EXIT.

       IMPRE-REMI.
       
      **************************     
      
	   MOVE 0 TO CONT-R CONT-I CONT-P.
	   MOVE MOV-CAP TO MOV-ED.
	   
	   DISPLAY "ATENCION !!!  EMITE REMITO" LINE 25 POSITION 10
		  SIZE 69
	   ACCEPT QQ LINE 25 POSITION 60

      *****  AGREGA CONTADOR DE REMITOS ****************
	   MOVE MOV-CAP TO LA-FACTURA-ED
	   MOVE "Factura Nro.:" TO LF-ED.

	   MOVE SPACES TO SUB-T-C-COMPRO
	   MOVE 7  TO NUM-COMP
	   OPEN I-O COMPRO.
	   READ COMPRO WITH NO LOCK.
	   ADD 1 TO NUM-COMPRO.
	   MOVE NUM-COMPRO TO MOV-CAP.
	   REWRITE REG-COMPRO INVALID KEY WRITE REG-COMPRO.
	   CLOSE COMPRO.
      ****************************************************

	   IF DET-CAP0 NOT = SPACES
		MOVE DET-CAP0 TO NOMBRE-C
		MOVE SPACES TO DIRE
		MOVE 0 TO COD-LOCA 
		MOVE SPACES TO IGAN TEL.
	   MOVE NOMBRE-C TO NOM-ED DET-OPE NOM1-ED NOM1-ED1.
	   MOVE DIRE TO DOM-ED DOM1-ED DOM1-ED1.
	   MOVE COD-LOCA TO C-POS CPOS-ED CPOS1-ED CPOS1-ED1.
	   MOVE COD-LOCA-1 TO C-POS-1
	   OPEN INPUT CPOSTAL.
	   READ CPOSTAL INVALID KEY 
		 MOVE LOC-M-CAP TO NOM-POSTAL PRO-POSTAL.
	   CLOSE CPOSTAL.
	   MOVE NOM-POSTAL TO LOC-ED LOC1-ED LOC1-ED1.
	   MOVE PRO-POSTAL TO PRO-ED.
	   MOVE IGAN TO IGAN-ED CUIT-OPE IGAN1-ED.
	   MOVE TEL TO TEL-ED TEL1-ED.
	   MOVE INSCRIPTO TO RESP-OPE.
	   MOVE RESP-EDI TO RES-ED RES1-ED.
	   MOVE AA-CAP TO AA-ED AA1-ED.
	   MOVE MM-CAP TO MM-ED MM1-ED.
	   MOVE DD-CAP TO DD-ED DD1-ED.
	   MOVE ING-BRUTOS TO ING-BRUTOS1-ED.
	   MOVE SPACES TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 1 MOVE "INSCRIPTO" TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 2 MOVE "NO INSCR." TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 3 MOVE "EXENTO" TO C-ING-BRUTOS1-ED.
	   IF C-ING-BRUTOS = 4 MOVE "OT.JURISD." TO C-ING-BRUTOS1-ED.

	   MOVE CANT-ART TO CANT-ART-ED CANT-ART1-ED.
	   MOVE REF-CAP TO REF-ED.

      **********     IF Y = 1 OR Y = 8
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   IF Y = 4 OR Y = 5
	      WRITE LIN-IMP02 FROM TIT0-0
	      ELSE WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM LIN-10.
	   WRITE LIN-IMP02 FROM TIT1-1.
	   WRITE LIN-IMP02 FROM TIT1-2.
	   WRITE LIN-IMP02 FROM TIT1-3.
	   WRITE LIN-IMP02 FROM TIT1-4.
	   ADD 12 TO CONT-R.
	   WRITE LIN-IMP02 FROM LIN-10.      
	   WRITE LIN-IMP02 FROM IMP-15.
	   WRITE LIN-IMP02 FROM LIN-10.
	   ADD 3 TO CONT-R.


	   

       R-IMPRE-F0.
	   MOVE SPACES TO DET-CAP0.
	   MOVE 0 TO CONT-REP CANT-ART. 
	   MOVE 1 TO V.
       R-IMPRE-F1.

	     IF V > 20 GO TO R-IMPRE-FIN.

	   IF UNI-VEC(V) = 0 GO TO R-IMPRE-FIN.
	   IF REP-VEC(V) = 0 ADD 1 TO V
			     ADD 1 TO CONT-REP
			     GO TO R-IMPRE-F1.           
			     
	   MOVE UNI-VEC(V) TO CANT-UNI.
	   MOVE  UNI-1 TO UNI1-ED UNI11-ED UNI1-ED3.
	   IF UNI-2 = 0 MOVE SPACES TO COMA-ED COMA1-ED COMA-ED3
		 ELSE MOVE "," TO COMA-ED COMA1-ED COMA-ED3.
	   MOVE UNI-2 TO UNI2-ED UNI21-ED UNI2-ED3.
	   MOVE ART-VEC(V) TO ART-ED ART-ED3.
      *     MOVE PRE-VEC(V) TO PRE-ED PRE1-ED PRE-ED3.
	   MOVE 0           TO PRE-ED PRE1-ED PRE-ED3.
	
	   MOVE DET-VEC(V) TO DET-ED DET1-ED DET-ED3.
      *     MOVE IMP-VEC(V) TO IMP-ED IMP1-ED IMP-ED3.
	   MOVE 0           TO IMP-ED IMP1-ED IMP-ED3.

	   MOVE ALI-VEC(V) TO ALI-IVA-ED.
	   COMPUTE II-CAP ROUNDED = II-VEC(V) / UNI-VEC(V).
	   MOVE II-CAP TO I-INT1-ED.
	   MOVE N-ART-VEC(V) TO N-ART-ED N-ART1-ED N-ART-ED3.
	   MOVE ENV-VEC(V) TO ENV-ED.
	   IF LIN-1-VEC(V) = 999
	      MOVE 0 TO PRE-ED UNI1-ED UNI2-ED 
	      MOVE SPACES TO COMA-ED
	      COMPUTE CANT-ART = CANT-ART - 1.
	   IF IMP-VEC(V) = 0 MOVE 0 TO UNI1-ED UNI2-ED UNI11-ED UNI21-ED
			     MOVE 0 TO UNI1-ED3 UNI2-ED3
			     MOVE SPACES TO COMA-ED COMA1-ED COMA-ED3
	      ELSE ADD 1 TO CANT-ART.

	   IF Y = 7 MOVE 0 TO PRE-ED IMP-ED.

	   MOVE SPACES TO REP-ED.
      ****     IF REP-VEC(V) NOT = 0 MOVE "R" TO REP-ED.
	   IF REP-VEC(V) > 7 MOVE "R" TO REP-ED.
	   IF REP-VEC(V) = 0
	       IF Y = 9 ADD 1 TO V
			GO TO R-IMPRE-F1.
      *=-=-=-=-=-=- PARA TODOS =-=-=-=-=-=-=-=-=-=
	   WRITE LIN-IMP02 FROM IMP-16 AFTER 1.

	   ADD 1 TO V.
	   ADD 1 TO CONT-R.
	   ADD 1 TO CONT-REP.
	   GO TO R-IMPRE-F1.
       R-IMPRE-FIN.

	   COMPUTE V = 21 - V

	   PERFORM ESPA V TIMES.
	   MOVE SPACES TO COM-REP-ED.
	   MOVE CANT-ART TO CANT-ART-ED CANT-ART1-ED.
	   IF Y = 9 MOVE COM-REP TO COM-REP-ED
		    MOVE CONT-REP TO CANT-ART-ED CANT-ART1-ED.

	   WRITE LIN-IMP02 FROM IMP-40 AFTER 1

	   MOVE COM1 TO COM1-ED COM1-ED1.
	   MOVE COM2 TO COM2-ED COM2-ED1.
	   MOVE COM3 TO COM3-ED.
	   MOVE REF-CAP TO REF-ED REF-ED1.

	   WRITE LIN-IMP02 FROM LIN-10.

	   IF Y = 8 MOVE 0 TO NET-ED
		    GO TO R-SIN-II.
	   IF Y = 9 MOVE 0 TO IMP-IMPU-OPE(1)
		    GO TO R-SIN-II.

	   MOVE IMP-IMPU-OPE(3) TO II-ED.
	   COMPUTE NET-ED ROUNDED = PRE-OPE + D-R-OPE.
      *=-=-=-=-=-=-=- PARA COMPROBANTES SIN IMPORTES =-=-=-==
	   IF Y = 7
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.
	   IF Y = 8
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.           
	   IF Y = 9 
		     MOVE 0 TO IMP-IMPU-OPE(1) IMP-IMPU-OPE(2)
			  IMP-IMPU-OPE(3) IMP-IMPU-OPE(4) NET-ED.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==
	   IF IMP-IMPU-OPE(3) = 0 GO TO R-SIN-II
	      ELSE GO TO R-CON-II.
       R-SIN-II.  
	   MOVE 0 TO NET-ED IVA-I-ED
	   WRITE LIN-IMP02 FROM IMP-18.
	   WRITE LIN-IMP02 FROM IMP-193.
	   ADD 2 TO CONT-R.
	   MOVE IMP-IMPU-OPE(1) TO IVA-I-ED.
	   MOVE IMP-IMPU-OPE(2) TO IVA-N-ED.
	   MOVE IMP-IMPU-OPE(4) TO RES-IMP-ED.
	   MOVE 0 TO NET-ED IVA-I-ED IVA-N-ED.

	   IF INSCRIPTO NOT = "C"
	      IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
		 MOVE ALI-PER(C-ING-BRUTOS) TO POR-RES-ED. 
	   IF IMP-IMPU-OPE(1) = 0 
				  WRITE LIN-IMP02 FROM IMP-192
				  ADD 1 TO CONT-R
	       ELSE WRITE LIN-IMP02 FROM IMP-19
		    ADD 1 TO CONT-R.
	   IF IMP-IMPU-OPE(2) = 0
		   WRITE LIN-IMP02 FROM IMP-201
		   ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-20
		   ADD 1 TO CONT-R. 
	   IF IMP-IMPU-OPE(4) = 0
		WRITE LIN-IMP02 FROM IMP-203
		ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-202
		   ADD 1 TO CONT-R.
	   PERFORM ESPA 1 TIMES.
	   GO TO R-IMPRE-TOT.
       R-CON-II.
	   WRITE LIN-IMP02 FROM IMP-18.
	   WRITE LIN-IMP02 FROM IMP-191.
	   ADD 2 TO CONT-R.
	   MOVE IMP-IMPU-OPE(1) TO IVA-I-ED.
	   MOVE IMP-IMPU-OPE(2) TO IVA-N-ED.
	   MOVE IMP-IMPU-OPE(4) TO RES-IMP-ED.
	   IF INSCRIPTO NOT = "C"
	      IF SEC-PRO-EMP > 1 AND SEC-PRO-EMP < 5
		 MOVE ALI-PER(C-ING-BRUTOS) TO POR-RES-ED. 
	   IF IMP-IMPU-OPE(1) = 0 
				  WRITE LIN-IMP02 FROM IMP-192
				  ADD 1 TO CONT-R
	       ELSE WRITE LIN-IMP02 FROM IMP-19
		    ADD 1 TO CONT-R.
	   IF IMP-IMPU-OPE(2) = 0
		   WRITE LIN-IMP02 FROM IMP-201
		   ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-20
		   ADD 1 TO CONT-R. 
	   IF IMP-IMPU-OPE(4) = 0
		WRITE LIN-IMP02 FROM IMP-203
		ADD 1 TO CONT-R
	      ELSE WRITE LIN-IMP02 FROM IMP-202
		   ADD 1 TO CONT-R.
	   PERFORM ESPA 1 TIMES.
	   GO TO R-IMPRE-TOT.
       R-IMPRE-TOT.
      *=-=-=-=-=- SIN IMPORTE TOTAL =-=-=-=-=-=-=-
	   MOVE 0 TO IMP-OPE

	   IF Y = 9
	       MOVE 0 TO IMP-OPE. 
	   IF Y = 7 
	       MOVE 0 TO IMP-OPE.
      *=-=-=-=-=- PARA VIOLLAZ =-=-=-=-=-=-=-=-
      *     IF Y = 7 GO TO IMPRE-FACTU-F.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	   MOVE IMP-OPE TO TOT-ED TOT-1-ED.
	   MOVE REF-CAP TO REF-ED REF-ED1.
	   IF Y NOT = 9
	     IF MON-COTIZA NOT = 0
		COMPUTE EXT-CAP ROUNDED = IMP-OPE / VAL-COTIZA
		MOVE EXT-CAP TO EXT-ED
		MOVE NOM-MON TO MON-ED
		WRITE LIN-IMP02 FROM IMP-171
	     ELSE WRITE LIN-IMP02 FROM IMP-17.
	   ADD 1 TO CONT-R.
	   IF Y = 2
	     MOVE 0 TO CONT-S.
	   IF Y NOT = 2
	     IF CONT-S NOT = 0
	       WRITE LIN-IMP02 FROM LIN-10
	       WRITE LIN-IMP02 FROM IMP-21
	       WRITE LIN-IMP02 FROM IMP-22
	       ADD 3 TO CONT-R.
	   IF Y = 1 WRITE LIN-IMP02 FROM LIN-32.
	   IF Y = 8 WRITE LIN-IMP02 FROM LIN-30.
	   IF Y = 7 WRITE LIN-IMP02 FROM LIN-33.
	   IF Y = 9 WRITE LIN-IMP02 FROM LIN-34.
	   IF Y = 2  
	     IF NUM-CONDI < 31 OR NUM-CONDI > 49 PERFORM ESPA
		ELSE WRITE LIN-IMP02 FROM LIN-35.
	   IF CONT-S NOT = 0
	      PERFORM ESPA 4 TIMES.
	   IF CONT-S = 0 MOVE 0 TO CANT-ART.
	   IF Y = 9 MOVE 2 TO Y.
      *=-=-=-=-= PIE DE FACTURA =-=-=-=-====-=-=-=-=-===-=
      *     PERFORM ESPA 6 TIMES.
      *=-=-=-=-PARA BICHO SIN PIE DE FACTURA =-=-=-=-=-=-=-=-=
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-  
	   GO TO IMPRE-REMI-F.
	   
      *     COPY RUT4.CBL.
      *=-=-=-=-=-=-=-= PARA SOTELO =-=-=-=-==-==-=====
      
       
       
      *********************************** 
      *      REMITOS VIEJOS 
      *     COPY "REMIVIE.RUT".       
       
	   
      **************************************     
       IMPRE-REMI-F.
	   EXIT.
	   
       CONF-CAMPOS.
	   MOVE 045125 TO TAB-POS(1).
	   MOVE 250275 TO TAB-POS(2).
	   MOVE 254101 TO TAB-POS(3).
	   MOVE 072825 TO TAB-POS(4).
	   MOVE 072402 TO TAB-POS(5).
	   MOVE 071204 TO TAB-POS(6).
	   MOVE 074702 TO TAB-POS(7).
	   MOVE 072408 TO TAB-POS(8).
	   MOVE 071901 TO TAB-POS(49).
	   MOVE 072004 TO TAB-POS(50).
	   MOVE 091910 TO TAB-POS(9).
	   MOVE 093030 TO TAB-POS(10).
	   MOVE 103030 TO TAB-POS(11).
	   MOVE 121918 TO TAB-POS(12).
	   MOVE 141910 TO TAB-POS(13).
	   MOVE 161918 TO TAB-POS(14).
	   MOVE 185918 TO TAB-POS(15).
	   MOVE 195918 TO TAB-POS(16).
	   MOVE 215918 TO TAB-POS(17).
	   MOVE 196510 TO TAB-POS(18).
	   MOVE 206510 TO TAB-POS(19).
	   MOVE 111240 TO TAB-POS(20).
	   MOVE 121240 TO TAB-POS(21).
	   MOVE 131240 TO TAB-POS(22).
	   MOVE 165918 TO TAB-POS(23).
	   MOVE 151208 TO TAB-POS(24).
	   MOVE 125020 TO TAB-POS(25).
	   MOVE 082006 TO TAB-POS(26).
	   MOVE 145801 TO TAB-POS(27).
	   MOVE 083030 TO TAB-POS(28).
	   MOVE 086512 TO TAB-POS(29).
	   MOVE 171206 TO TAB-POS(30).
	   MOVE 181903 TO TAB-POS(31).
	   MOVE 191020 TO TAB-POS(32).
	   MOVE 221912 TO TAB-POS(33).
	   MOVE 222505 TO TAB-POS(34).
	   MOVE 223530 TO TAB-POS(35).
	   MOVE 111918 TO TAB-POS(36).
	   MOVE 096214 TO TAB-POS(37).
	   MOVE 205918 TO TAB-POS(38).
	   MOVE 081930 TO TAB-POS(40).
	   MOVE 091927 TO TAB-POS(41).
	   MOVE 101904 TO TAB-POS(42).
	   MOVE 102530 TO TAB-POS(43).
	   MOVE 095610 TO TAB-POS(44).
	   MOVE 111901 TO TAB-POS(45).
	   MOVE 112215 TO TAB-POS(46).
	   MOVE 115615 TO TAB-POS(47).
	   MOVE 121902 TO TAB-POS(48).
       BORRA-CAP.
	   MOVE 9 TO SW3.
	   PERFORM BORRA.
	   MOVE 10 TO SW3.
	   PERFORM BORRA.
	   MOVE 11 TO SW3.
	   PERFORM BORRA.
	   MOVE 37 TO SW3.
	   PERFORM BORRA.
	   MOVE 36 TO SW3.
	   PERFORM BORRA.
	   MOVE 12 TO SW3.
	   PERFORM BORRA.
	   MOVE 13 TO SW3.
	   PERFORM BORRA.
	   MOVE 14 TO SW3.
	   PERFORM BORRA.
	   DISPLAY SPACES LINE 13 POSITION 3 SIZE 35.
       BORRA-CAP1.
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
	   MOVE 23 TO SW3.
	   PERFORM BORRA.
	   MOVE 25 TO SW3.
	   PERFORM BORRA.
	   MOVE 26 TO SW3.
	   PERFORM BORRA.
	   MOVE 28 TO SW3.
	   PERFORM BORRA.
	   MOVE 29 TO SW3.
	   PERFORM BORRA.
	   MOVE 33 TO SW3.
	   PERFORM BORRA.
	   MOVE 34 TO SW3.
	   PERFORM BORRA.
	   MOVE 35 TO SW3.
	   PERFORM BORRA.
	   MOVE 38 TO SW3.
	   PERFORM BORRA.
	   DISPLAY SPACES LINE 17 POSITION 59 SIZE 18.
       BORRA-PANTA1.
	   MOVE 0 TO SW1.
	   PERFORM BORRA-P1 VARYING SW1 FROM 8 BY 1
			UNTIL SW1 > 12.
       BORRA-PANTA.
	   MOVE 0 TO SW1.
	   PERFORM BORRA-P1 VARYING SW1 FROM 7 BY 1 
			     UNTIL SW1 > 23.
       BORRA-P1.
	   DISPLAY SPACES LINE SW1 POSITION 2 SIZE 76.
       PANTA-MOD.
	   DISPLAY "EMPRESA: " LINE 7 POSITION 3.
	   DISPLAY " - SUC: " LINE 7 POSITION 16.
	   DISPLAY "COMENTARIOS:" LINE 10 POSITION 3.
	   DISPLAY "      1- " LINE 11 POSITION 3.
	   DISPLAY "      2- " LINE 12 POSITION 3.
	   DISPLAY "      3- " LINE 13 POSITION 3.
	   DISPLAY "FECHA  : " LINE 15 POSITION 3.
	   DISPLAY "CAJA   :" LINE 17 POSITION 3.
	   DISPLAY "MENSAJES DE STOCK : " LINE 19 POSITION 3.
	   DISPLAY " N : No emite mensajes " LINE 19 POSITION 25 
		       BLINK.
	   DISPLAY "TURNO..:" LINE 22 POSITION 3.

       PANTA-FACTU.
	   DISPLAY "COMPROB. Nro  : " LINE 7 POSITION 3.
	   DISPLAY "RENGLON Nro : " LINE 7 POSITION 36.
	   DISPLAY "CUENTA Nro    : " LINE 8 POSITION 3.
	   DISPLAY "ARTICULO Nro  : " LINE 9 POSITION 3.
	   DISPLAY "PRECIO FINAL  : " LINE 11 POSITION 3.
	   DISPLAY "PRECIO UNIT.  : " LINE 12 POSITION 3.
	   DISPLAY "ENVASE :" LINE 12 POSITION 40.
	   DISPLAY "CANT. UNIDADES: " LINE 14 POSITION 3.
	   DISPLAY "FRACCIONADO ???:" LINE 14 POSITION 40.
	   DISPLAY "-No-" LINE 14 POSITION 57 LOW REVERSE.
	   DISPLAY "CONDICION     : " LINE 18 POSITION 3. 
	   DISPLAY "IMPORTE RENG. : " LINE 16 POSITION 3.
	   DISPLAY "IMPORTES GRAV.: " LINE 16 POSITION 40.
	   DISPLAY "IMPORTES EXEN.: " LINE 17 POSITION 40
	   DISPLAY "IVA TASA GRAL.: " LINE 18 POSITION 40.
	   DISPLAY "IVA SOBRE TASA: " LINE 19 POSITION 40.
	   DISPLAY "OTROS IMP.\PER: " LINE 20 POSITION 40.
	   DISPLAY "T O T A L     : " LINE 21 POSITION 40 LOW REVERSE.
	   DISPLAY "STOCK ACTUAL  : " LINE 22 POSITION 3.
       PANTA-FACTU1.
	   DISPLAY "CUENTA Nro    : " LINE 8 POSITION 3.
	   DISPLAY "ARTICULO Nro  : " LINE 9 POSITION 3.
	   DISPLAY "PRECIO FINAL  : " LINE 11 POSITION 3.
	   DISPLAY "PRECIO UNIT.  : " LINE 12 POSITION 3.
	   DISPLAY "ENVASE :" LINE 12 POSITION 40.
       PANTA-CTA.
	   DISPLAY "NOMBRE        : " LINE 8 POSITION 3 REVERSE.
	   DISPLAY "DOMICILIO     : " LINE 9 POSITION 3 REVERSE.
	   DISPLAY "TEL.: " LINE 9 POSITION 50 REVERSE.
	   DISPLAY "CODIGO POSTAL : " LINE 10 POSITION 3 REVERSE.
	   DISPLAY "RESP. IVA     : " LINE 11 POSITION 3 REVERSE.
	   DISPLAY "CUIT: " LINE 11 POSITION 50 REVERSE.
	   DISPLAY "SITUAC.I.BRUT.: " LINE 12 POSITION 3 REVERSE.
	   DISPLAY "NRO : " LINE 12 POSITION 50 REVERSE.
       DESP-EXE-S.
	   DISPLAY SPACES LINE 17 POSITION 59 SIZE 18.
	   MOVE EXE-T-CAP TO EXE-SUB-ED.
	   DISPLAY EXE-SUB-ED LINE 17 POSITION 59.
       DESP-II-S.
	   MOVE 38 TO SW3.
	   PERFORM BORRA.
	   COMPUTE OT-IMP-SUB = OT-IMP-SUB + CAP-II.
	   MOVE OT-IMP-SUB TO II-SUB-ED.
	   DISPLAY II-SUB-ED LINE TAB-LIN(SW3) POSITION TAB-COL(SW3).
       DESP-I-S.
	   MOVE 15 TO SW3.
	   PERFORM BORRA.
	   MOVE IVA-I-SUB TO IVA-I-SUB-ED.
	   DISPLAY IVA-I-SUB-ED LINE TAB-LIN(SW3) 
		 POSITION TAB-COL(SW3).
       DESP-N-S.
	   MOVE 16 TO SW3.
	   PERFORM BORRA.
	   MOVE IVA-N-SUB TO IVA-N-SUB-ED.
	   DISPLAY IVA-N-SUB-ED LINE TAB-LIN(SW3)
		 POSITION TAB-COL(SW3).
       DESP-TOT-S.
	   MOVE 17 TO SW3.
	   PERFORM BORRA.
	   MOVE TOTAL-SUB TO TOTAL-SUB-ED.
	   DISPLAY TOTAL-SUB-ED LINE TAB-LIN(SW3)
		  POSITION TAB-COL(SW3).    
       DESP-I.
	   MOVE 15 TO SW3.
	   PERFORM BORRA.
	   MOVE IVA-CAP TO IVA-EDI.
	   DISPLAY IVA-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-II.
	   MOVE 38 TO SW3.
	   PERFORM BORRA.
	   COMPUTE OT-IMP-CAP = OT-IMP-CAP + IMP-INT.
	   MOVE OT-IMP-CAP TO II-EDI.
	   DISPLAY II-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			  SIZE TAB-SIZ(SW3).
       DESP-I-N.
	   MOVE 16 TO SW3.
	   PERFORM BORRA.
	   MOVE IVA-N-CAP TO IVA-N-EDI.
	   DISPLAY IVA-N-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			     SIZE TAB-SIZ(SW3). 
       DESP-DESC.
	   MOVE 10 TO SW3.
	   DISPLAY DET-NUM LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-MAR.
	   MOVE 11 TO SW3.
	   DISPLAY DET-MAR LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-PRE.
	   MOVE 12 TO SW3.
	   MOVE PRE-CAP TO PRE-EDI.
	   DISPLAY PRE-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-PRE-F.
	   MOVE 36 TO SW3.
	   MOVE FIN-CAP TO PRE-F-EDI.
	   DISPLAY PRE-F-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			     SIZE TAB-SIZ(SW3) REVERSE.
       DESP-ENV.
	   MOVE 25 TO SW3.
	   MOVE NOM-ENVA TO NOM-ENV-EDI.
	   DISPLAY ENV-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-DET.
	   MOVE 37 TO SW3.
	   DISPLAY COM-CAP LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-IMP.
	   MOVE 14 TO SW3.
	   MOVE IMP-CAP TO IMP-EDI.
	   DISPLAY IMP-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       DESP-IMP-T.
	   MOVE 23 TO SW3.
	   MOVE IMP-T-CAP TO IMP-T-EDI.
	   DISPLAY IMP-T-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			     SIZE TAB-SIZ(SW3).
       DESP-TOT.
	   MOVE 17 TO SW3.
	   MOVE TOT-CAP TO TOT-EDI.
	   DISPLAY TOT-EDI LINE TAB-LIN(SW3) POSITION TAB-COL(SW3)
			   SIZE TAB-SIZ(SW3).
       CIERRO-A.
	   CLOSE ART01 ART02 NUMERO MARCAS ENVASE IMP02.
       CIERRE-A.
	   CLOSE ART01 ART02 NUMERO MARCAS ENVASE IMP02.
       TIT-PAN.
	   MOVE " -PROGRAMA" TO TIT-1-PAN.
	   MOVE "  DE  FACT" TO TIT-2-PAN.
	   MOVE "URACION-  " TO TIT-3-PAN.
       OPC-MODA.
	   DISPLAY "<F1>  EMISION DE FACTURA" LINE 14 POSITION 18.
	   DISPLAY "<F2>  EMISION DE REMITO" LINE 15 POSITION 18.
	   DISPLAY "<F3>  EMISION DE N.DEBITO" LINE 16 POSITION 18.
	   DISPLAY "<F4>  EMISION DE N.CREDITO" LINE 17 POSITION 18.
	   DISPLAY "<F5>  EMISION DE PRESUPUESTO" LINE 18 POSITION 18.
	   DISPLAY "<F6>  EMISION DE PEDIDOS" LINE 19 POSITION 18.
	   DISPLAY "<ESC> VUELVE AL MENU " LINE 20 POSITION 18.
       COPY "PANTA.PRO".
       COPY "BORRA.PRO".
       COPY "MENSA.PRO".
       COPY "CAPTU.PRO".
       COPY "DESPLI.PRO".
       COPY "ROT132.PRO".
       COPY "CLAVES.PRO".
       COPY "PESOS.PRO".
      * COPY "MEMO.PRO".


       PAGARE.
	   DISPLAY "Emite PAGARE ?(S)" LINE 25 POSITION 10 BLINK
	   ACCEPT ELPAG LINE 25 POSITION 40 NO BEEP TAB
	   IF ELPAG = "S" OR ELPAG = "s" GO TO EMITE-PAGARE.
	   GO TO PAGARE-F.
       EMITE-PAGARE. 
	   DISPLAY "Titular ..:" LINE 25 POSITION 5 SIZE 74
	   MOVE NOMBRE-C TO NOMC-PAG
	   ACCEPT NOMC-PAG LINE 25 POSITION 25 NO BEEP TAB ECHO UPDATE
	   DISPLAY "Documento.:" LINE 25 POSITION 5 SIZE 74
	   ACCEPT DOC-PAG LINE 25 POSITION 25 NO BEEP TAB
	   MOVE DIRE TO DIR-PAG
	   DISPLAY "Direccion.:" LINE 25 POSITION 5 SIZE 74
	   ACCEPT DIR-PAG LINE 25 POSITION 25 NO BEEP TAB ECHO UPDATE
	   DISPLAY "Localidad.:" LINE 25 POSITION 5 SIZE 74
	   MOVE NOM-POSTAL TO LOC-PAG
	   ACCEPT LOC-PAG LINE 25 POSITION 25 NO BEEP TAB ECHO UPDATE
	   MOVE VALOR-PAGARE  TO IMP-PAG PTOT-GRAL

	   MOVE DD-CAP        TO DIA-PAG
	   MOVE 10            TO DIAV-PAG DIAV1-PAG
	   COMPUTE MES-AYUDA =  MM-CAP + 1
	   IF MES-AYUDA > 12 
		   MOVE 1 TO MESV-PAG MESV1-PAG
		   MOVE R-MMESES(MM-CAP) TO ELMES-PAG
		   COMPUTE ANOV-PAG = AA-CAP + 1
		   COMPUTE ANOV1-PAG = AA-CAP + 1
	     
	     ELSE  MOVE MES-AYUDA TO MESV-PAG MESV1-PAG
		   MOVE R-MMESES(MM-CAP) TO ELMES-PAG
		   MOVE AA-CAP    TO ANOV-PAG ANO-PAG ANOV1-PAG.

	    IF SUC-EMP = 1 MOVE "SAN JOSE  E.R. " TO LUG-PAG.
	    IF SUC-EMP = 2 MOVE "  COLON  E.R. " TO LUG-PAG.
	   PERFORM CONVERSOR THRU  FIN-CONVERSOR
	   MOVE PSALIDA-1  TO LASL
	   MOVE LASL1      TO PES-PAG
	   MOVE LASL2      TO PES1-PAG

	   CLOSE IMP02
	   MOVE MA-IMP-PAGARE   TO MIRTA    
	   OPEN OUTPUT IMP02.
	   WRITE LIN-IMP02 FROM R1-PAGARE AFTER 13
	   WRITE LIN-IMP02 FROM R2-PAGARE AFTER 4
	   WRITE LIN-IMP02 FROM R3-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R4-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R5-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R6-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R7-PAGARE AFTER 6
	   WRITE LIN-IMP02 FROM R8-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R9-PAGARE AFTER 2
	   WRITE LIN-IMP02 FROM R10-PAGARE AFTER 2.

	   CLOSE IMP02
	   MOVE MA-FACTURA-A TO MIRTA
	   OPEN OUTPUT IMP02.      

       PAGARE-F.
	   EXIT.

