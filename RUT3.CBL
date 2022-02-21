          
      *      RUT3.CBL     
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      * IMPRESION YUSTY PARA TANO
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       IF Y = 4 OR Y = 5
      *         WRITE LIN-IMP02 FROM TIT0-0
      *        ELSE WRITE LIN-IMP02 FROM LIN-10.
      *       WRITE LIN-IMP02 FROM LIN-10.
      *       WRITE LIN-IMP02 FROM LIN-10.
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM LIN-10
      *       WRITE LIN-IMP02 FROM TIT1-1
      *       WRITE LIN-IMP02 FROM TIT1-2
      *       WRITE LIN-IMP02 FROM TIT1-3
      *       ADD 12 TO CONT-R.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM IMP-15.
      *     WRITE LIN-IMP02 FROM IMP-151.
      *     ADD 3 TO CONT-R.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
      *  PARA FARMACIA VILLA ELISA
      *=-=-=-==-=-=-=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *        PERFORM ESPA 3 TIMES
      *        WRITE LIN-IMP02 FROM TIT0-0
      *        WRITE LIN-IMP02 FROM TIT0-1
      *        PERFORM ESPA 5 TIMES
      *        WRITE LIN-IMP02 FROM TIT1-1
      *        WRITE LIN-IMP02 FROM TIT1-2
      *        WRITE LIN-IMP02 FROM TIT1-3
      *        WRITE LIN-IMP02 FROM TIT1-4
      *        PERFORM ESPA 1 TIMES
      *        WRITE LIN-IMP02 FROM TIT1-5
      *        PERFORM ESPA 2 TIMES
      *        WRITE LIN-IMP02 FROM TIT1-6
      *        WRITE LIN-IMP02 FROM TIT1-7
      *        PERFORM ESPA
      *        WRITE LIN-IMP02 FROM IMP3-15
      *        PERFORM ESPA.        
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *   PARA SOTELO FACTURA "A" Y "B"
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      *     IF Y = 2 OR Y = 4 OR Y = 5
      *        IF SUB-T-C-CAP = "A"
      *            GO TO IMPRE-F-A
      *        ELSE GO TO IMPRE-F-B.
      **=-=-=-=-=-=-=-=-=-=-=-=-==-=-==
      * IMPRE-F-A.
      *     WRITE LIN-IMP02 FROM LIN-10
      *     WRITE LIN-IMP02 FROM LIN-10
      *     WRITE LIN-IMP02 FROM TIT1-1
      *     WRITE LIN-IMP02 FROM LIN-10
      *     IF Y = 4 OR Y = 5
      *       WRITE LIN-IMP02 FROM TIT1-10
      *       ELSE WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      **     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM TIT1-2.
      *     WRITE LIN-IMP02 FROM TIT1-3.
      *     WRITE LIN-IMP02 FROM TIT1-4.
      *     WRITE LIN-IMP02 FROM TIT1-5.
      *     WRITE LIN-IMP02 FROM TIT1-51.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM TIT1-6.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM TIT1-7.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM IMP1-15.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     MOVE SPACES TO DET-CAP0.
      *     MOVE 0 TO CONT-REP CANT-ART.
      *     MOVE 1 TO V.
      *     MOVE 28 TO CONT-R.
      *     GO TO IMPRE-F1.
      **-=-=-=-=-=-==-=-=-===-===-==-=-=-==
      * IMPRE-F-B.
      *     MOVE DD1-ED TO DD2-ED.
      *     MOVE MM1-ED TO MM2-ED.
      *     MOVE AA1-ED TO AA2-ED.
      *     MOVE DD-V-CAP TO DD1-V-ED.
      *     MOVE MM-V-CAP TO MM1-V-ED.
      *     MOVE AA-V-CAP TO AA1-V-ED.
      *     MOVE NOM1-ED TO NOM2-ED.
      *     MOVE CTA1-ED TO CTA2-ED.
      *     MOVE DOM1-ED TO DOM2-ED.
      *     MOVE CPOS1-ED TO CPOS2-ED.
      *     MOVE LOC1-ED TO LOC2-ED.
      *     MOVE TEL1-ED TO TEL2-ED.
      *     MOVE RES1-ED TO RES2-ED.
      *     MOVE IGAN1-ED TO IGAN2-ED.
      *     MOVE CONDI1-ED TO CONDI2-ED.
      *     MOVE ING-BRUTOS1-ED TO ING-BRUTOS2-ED.
      *     MOVE C-ING-BRUTOS1-ED TO C-ING-BRUTOS2-ED.
      *     IF Y = 2 MOVE SPACES TO T-FAC2-ED.
      *     IF Y = 4 OR Y = 5 MOVE T-FAC1-ED TO T-FAC2-ED.
      *     WRITE LIN-IMP02 FROM TIT2-1
      *     WRITE LIN-IMP02 FROM LIN-10
      *     WRITE LIN-IMP02 FROM TIT2-2.
      *     WRITE LIN-IMP02 FROM TIT2-3.
      *     WRITE LIN-IMP02 FROM TIT2-4.
      *     WRITE LIN-IMP02 FROM TIT2-5.
      *     WRITE LIN-IMP02 FROM TIT2-6.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM TIT2-7.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     WRITE LIN-IMP02 FROM IMP2-15.
      *     WRITE LIN-IMP02 FROM LIN-10.
      *     MOVE SPACES TO DET-CAP0.
      *     MOVE 0 TO CONT-REP CANT-ART.
      *     MOVE 1 TO V.
      *     MOVE 12 TO CONT-R.
      *     GO TO IMPRE-F1.
      *-=-=-=-=-=-==-=-=-===-===-==-=-=-==
