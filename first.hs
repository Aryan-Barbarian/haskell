expNaive base exp 
	| (exp == 0) = 1 
	| otherwise  = base * (expNaive base (exp - 1))

expFast :: (Num a, Eq b, Integral b) => a -> b -> a
expFast base ex
	| (ex == 0) = 1 
	| ((mod ex 2) == 0) = val * val 
	| otherwise = base * (expFast base (ex - 1))
	where 
		val = (expFast base (ex `div` 2) )