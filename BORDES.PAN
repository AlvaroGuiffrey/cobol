           DISPLAY SPACES LINE 1 POSITION 1 ERASE.
           PERFORM HORI VARYING I FROM 2 BY 1 UNTIL I > 79
           PERFORM VERT VARYING I FROM 1 BY 1 UNTIL I > 24
           PERFORM RINCO1
           DISPLAY "  /  /  " LINE 2 POSITION 71.
       VERT.
           DISPLAY "�" LINE I POSITION 1
           DISPLAY "�" LINE I POSITION 80.
           DISPLAY "�" LINE 2 POSITION 24
           DISPLAY "�" LINE 2 POSITION 69.
           

       HORI.
           DISPLAY "�" LINE 1 POSITION I
           DISPLAY "�" LINE 3 POSITION I
           DISPLAY "�" LINE 23 POSITION I
           DISPLAY "�" LINE 25 POSITION I.
       RINCO1.
           DISPLAY "�" LINE 1 POSITION 80
           DISPLAY "�" LINE 25 POSITION 1
           DISPLAY "�" LINE 1 POSITION 1
           DISPLAY "�" LINE 25 POSITION 80.
       OTROS.
           DISPLAY "�" LINE 5  POSITION 1
           DISPLAY "�" LINE 22 POSITION 1.

