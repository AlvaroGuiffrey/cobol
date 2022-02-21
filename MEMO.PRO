000000*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      * RUTINAS PARA MEMORIAL DE CAJA
      *-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
       MEMO-ENT.
           PERFORM BORRA-PANTA.
           PERFORM PANTA-MEMO.
           MOVE TOT-ME-CAP TO TOT-ME-ED.
           MOVE 0 TO EMP-ME SUC-ME CAJA-ME AA-ME MM-ME DD-ME.
           MOVE 0 TO T-COM-ME SUC-C-ME COM-ME MOV-ME TIP-ME.
           MOVE 0 TO MON-ME VAL-ME COTI-ME.
           MOVE SPACES TO DET-ME SUB-T-C-ME.
           MOVE NUM-EMP TO EMP-ME.
           MOVE SUC-EMP TO SUC-ME.
           MOVE VEN-CAP TO CAJA-ME.
           MOVE ANO TO AA-CAP.
           MOVE MES TO MM-CAP.
           MOVE DIA TO DD-CAP.
           IF CLAVE-ME = "S" OR CLAVE-ME = "E" 
              PERFORM CARGA-T-C-ME THRU CARGA-COM-ME
              ELSE MOVE Y TO T-COM-ME T-COM-I-ME
                   MOVE SUB-T-C-OPE TO SUB-T-C-ME SUB-T-C-I-ME
                   MOVE SUC-C-OPE TO SUC-C-ME SUC-C-I-ME
                   MOVE COM-OPE TO COM-ME COM-I-ME.
           MOVE 1 TO MOV-ME.
           OPEN INPUT MEMO.
           READ MEMO INVALID KEY MOVE 0 TO MOV-ME
                               CLOSE MEMO
                               GO TO MEMO-1.
           START MEMO KEY IS NOT < CLAVE-MEMO
                  INVALID KEY CLOSE MEMO
                              MOVE 0 TO MOV-ME
                              GO TO MEMO-1.
       POSI-MEMO.
           READ MEMO NEXT AT END CLOSE MEMO
                                 MOVE MOV-I-ME TO MOV-ME
                                 GO TO MEMO-1.
           IF EMP-ME NOT = NUM-EMP GO TO SIGO-MEMO.
           IF SUC-ME NOT = SUC-EMP GO TO SIGO-MEMO.
           IF CAJA-ME NOT = VEN-CAP GO TO SIGO-MEMO.
           IF AA-ME NOT = ANO GO TO SIGO-MEMO.
           IF MM-ME NOT = MES GO TO SIGO-MEMO.
           IF DD-ME NOT = DIA GO TO SIGO-MEMO.
           IF T-COM-ME NOT = T-COM-I-ME GO TO SIGO-MEMO.
           IF SUB-T-C-ME NOT = SUB-T-C-I-ME GO TO SIGO-MEMO.
           IF SUC-C-ME NOT = SUC-C-I-ME GO TO SIGO-MEMO.
           IF COM-ME NOT = COM-I-ME GO TO SIGO-MEMO.
           MOVE MOV-ME TO MOV-I-ME.
           GO TO POSI-MEMO.
       SIGO-MEMO.
           MOVE NUM-EMP TO EMP-ME.
           MOVE SUC-EMP TO SUC-ME.
           MOVE VEN-CAP TO CAJA-ME.
           MOVE ANO TO AA-ME.
           MOVE MES TO MM-ME.
           MOVE DIA TO DD-ME.
           MOVE T-COM-I-ME TO T-COM-ME.
           MOVE SUB-T-C-I-ME TO SUB-T-C-ME.
           MOVE SUC-C-I-ME TO SUC-C-ME.
           MOVE COM-I-ME TO COM-ME.
       MEMO-1.
           ADD 1 TO MOV-ME.
           MOVE 0 TO CAM-ME-CAP.
           PERFORM BORRA-P-MEMO.
           PERFORM DESP-TOT-ME.
       MEMO-11.
           PERFORM CARGA-TIP-ME.
           IF CLAVE-ME = "E"
               IF TIP-ME > 19 GO TO MEMO-11.
           IF CLAVE-ME = "S"
               IF TIP-ME < 20 GO TO MEMO-11.
           IF CLAVE-ME = SPACES 
               IF TIP-ME > 19 GO TO MEMO-11.
           PERFORM CARGA-VAL-ME THRU CARGA-VAL-F-ME.
           IF TOT-ME-CAP < VAL-ME-$-CAP
              COMPUTE CAM-ME-CAP = TOT-ME-CAP - VAL-ME-$-CAP 
              MOVE 0 TO TOT-ME-CAP
              PERFORM DESP-TOT-ME
              PERFORM DESP-CAM-ME.
           IF TOT-ME-CAP = VAL-ME-$-CAP
              MOVE 0 TO TOT-ME-CAP
              PERFORM DESP-TOT-ME.
           IF TOT-ME-CAP > VAL-ME-$-CAP
              COMPUTE TOT-ME-CAP = TOT-ME-CAP - VAL-ME-$-CAP
              PERFORM DESP-TOT-ME.
           PERFORM CARGA-DET-ME.
           PERFORM GRABO-ME.
           IF TOT-ME-CAP NOT = 0 GO TO MEMO-1.
           IF CAM-ME-CAP NOT = 0 GO TO CAMBIO-ME.
           GO TO SALGO-ME. 
       CAMBIO-ME.
           ADD 1 TO MOV-ME.
           PERFORM BORRA-P-MEMO.
           PERFORM DESP-CAM-ME.
       CAMBIO-ME1.
           PERFORM CARGA-TIP-ME.
           IF TIP-ME = 60 ADD 1 TO MOV-ME
                          PERFORM BORRA-P-MEMO
                          PERFORM DESP-RED-ME
                          GO TO REDO-ME2.
           IF TIP-ME < 50 GO TO CAMBIO-ME1.
           PERFORM CARGA-VAL-ME THRU CARGA-VAL-F-ME.
           PERFORM GRABO-ME.
           IF VAL-ME-CAP < CAM-ME-CAP
              COMPUTE CAM-ME-CAP = CAM-ME-CAP - VAL-ME-CAP
              GO TO CAMBIO-ME.
           IF VAL-ME-CAP > CAM-ME-CAP
.............      COMPUTE RED-ME-CAP = 
              GO TO REDO-ME. 
           IF VAL-ME-CAP = CAM-ME-CAP
              GO TO SALGO-ME.
       REDO-ME.
           ADD 1 TO MOV-ME. 
           PERFORM BORRA-P-MEMO.
           PERFORM DESP-RED-ME.
       REDO-ME1.
           PERFORM CARGA-TIP-ME.
       REDO-ME2.
           IF TIP-ME NOT = 60 GO TO REDO-ME1.
           PERFORM GRABO-ME.
..........CONFIRMAR REDONDEO ..........
           