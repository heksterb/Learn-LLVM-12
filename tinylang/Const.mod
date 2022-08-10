MODULE Const;

CONST
  Truth = NOT FALSE;
  Ten = 10;
  Hundred = Ten * Ten;

PROCEDURE Square;
VAR i : INTEGER;
BEGIN
  i := i - Ten;
  IF i * i = Hundred THEN i := 0 END.
END Square;

PROCEDURE Weird;
VAR i : INTEGER;
BEGIN
  i := i - Ten;
  i := i - (Ten + Hundred);
  i := i - (Ten * Hundred);
END Weird;

END Const.
