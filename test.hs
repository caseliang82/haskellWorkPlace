main = do
	b<-return "a"
	a<-return "b"
	putStr (a++b)