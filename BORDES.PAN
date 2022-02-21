           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM HORI VARYING I FROM 2 BY 1 UNTIL I > 79
           PERFORM VERT VARYING I FROM 1 BY 1 UNTIL I > 24
           PERFORM RINCO1
           DISPLAY "  /  /  " LINE 2 POSITION 71.
       VERT.
           DISPLAY "³" LINE I POSITION 1
           DISPLAY "³" LINE I POSITION 80.
           DISPLAY "³" LINE 2 POSITION 24
           DISPLAY "³" LINE 2 POSITION 69.
           

       HORI.
           DISPLAY "Ä" LINE 1 POSITION I
           DISPLAY "Ä" LINE 3 POSITION I
           DISPLAY "Ä" LINE 23 POSITION I
           DISPLAY "Ä" LINE 25 POSITION I.
       RINCO1.
           DISPLAY "¿" LINE 1 POSITION 80
           DISPLAY "À" LINE 25 POSITION 1
           DISPLAY "Ú" LINE 1 POSITION 1
           DISPLAY "Ù" LINE 25 POSITION 80.
       OTROS.
           DISPLAY "Ã" LINE 5  POSITION 1
           DISPLAY "Ã" LINE 22 POSITION 1.

