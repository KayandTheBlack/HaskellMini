main = do
	x <- getLine
	if x /= "*" 
	then do
		bmi x
		main
	else return ()
	where 
		bmi x = putStrLn (name ++ ": " ++ bodyType (read w) (read h))
			where   [name, w, h] = words x
				bodyType w h 
					| bmindex < 18 = "underweight"
					| bmindex < 25 = "normal weight"
					| bmindex < 30 = "overweight"
					| bmindex < 40 = "obese"
					| otherwise    = "severely obese"
					where bmindex = w/(h*h)
