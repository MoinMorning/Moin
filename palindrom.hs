main :: IO ()
main = do
 z <- getLine
 if null z -- beendet die Schleife
    then return () --tue-nichts-Aktion
	else do
		if istPalindrom z
		   then putStrLn "ist ein Palindrom"
		   else putStrLn "ist kein Palindrom"
		main -- rekursive Aufruf fuer die Schleife
istPalindrom w = w == reverse w