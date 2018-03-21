main = do
	x <- getLine
	if x /= "" then
		if startsByA x
		then putStrLn "Hello!"
		else putStrLn "Bye!"
	else putStrLn ""
	where
		startsByA ('a':xs) = True
		startsByA ('A':xs) = True
		startsByA _ = False
