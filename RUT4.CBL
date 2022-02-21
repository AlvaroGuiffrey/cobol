
      *     RUT4.CBL
      *=-=-=-=-=-=-=-= PARA SOTELO =-=-=-=-==-==-=====
      * IMPRE-T-SOT-A.
      *     PERFORM ESPA 2 TIMES.
      *     MOVE IVA-1-CAP TO IVA-11-ED.
      *     MOVE IVA-2-CAP TO IVA-21-ED.
      *     WRITE LIN-IMP02 FROM IMP1-18 AFTER 1.
      *     MOVE PRE-OPE TO NET1-ED.
      *     MOVE IMP-IMPU-OPE(1) TO IVA-I1-ED.
      *     MOVE IMP-IMPU-OPE(2) TO IVA-N1-ED.
      *     MOVE IMP-IMPU-OPE(3) TO II1-ED.
      *     MOVE IMP-IMPU-OPE(4) TO PER1-ED.
      *     MOVE IMP-OPE TO TOT-11-ED.
      *     MOVE D-R-OPE TO EXE1-ED
      *     PERFORM ESPA.
      *     WRITE LIN-IMP02 FROM IMP1-19 AFTER 1.
      *     PERFORM ESPA.
      *     WRITE LIN-IMP02 FROM IMP1-20 AFTER 1.
      *     IF CONTROLO = "N" PERFORM ESPA 5 TIMES
      *      ELSE PERFORM ESPA 28 TIMES.
      *     GO TO IMPRE-FACTU-F.
      * IMPRE-T-SOT-B.
      *    PERFORM ESPA 2 TIMES.
      *     WRITE LIN-IMP02 FROM IMP2-18 AFTER 1.
      *     MOVE PRE-OPE TO NET2-ED.
      *     MOVE IMP-IMPU-OPE(3) TO II2-ED.
      *     MOVE D-R-OPE TO EXE2-ED.
      *     MOVE IMP-IMPU-OPE(1) TO IVA-I2-ED.
      *     MOVE IMP-IMPU-OPE(2) TO IVA-N2-ED.
      *     MOVE IMP-IMPU-OPE(4) TO PER2-ED.
      *     MOVE IMP-OPE TO TOT-12-ED.
      *     WRITE LIN-IMP02 FROM IMP2-19 AFTER 1.
      *     PERFORM ESPA 8 TIMES.
      *     MOVE 0 TO CONT-R.
      *     GO TO IMPRE-FACTU-F.
      **=-=-=-=-=PARA FARMACIA VILLA ELISA-=-=-==
      * IMPRE-T-FAR.
      *      MOVE REF-CAP TO REF-ED3.
      *      MOVE COM1 TO COM1-ED3.
      *      MOVE COM2 TO COM2-ED3.
      *      MOVE COM3 TO COM3-ED3.
      *      MOVE IMP-OPE TO TOT-ED3
      *      WRITE LIN-IMP02 FROM IMP3-19.
      *      WRITE LIN-IMP02 FROM IMP3-17.
      *      WRITE LIN-IMP02 FROM IMP3-18.
      *      PERFORM ESPA 7 TIMES.
      *      GO TO IMPRE-FACTU-F.
      *=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
