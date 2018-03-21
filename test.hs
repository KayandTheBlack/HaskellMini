
--factorial :: Integer -> Integer no necesario, autotipa (infiere tipos)

factorial 0 = 1
factorial n = n * factorial (n-1)




potencia :: Integer -> Integer -> Integer

potencia x 0 = 1
potencia x n = x * potencia x (n-1)
