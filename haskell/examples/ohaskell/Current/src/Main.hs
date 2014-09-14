simpleDoubler :: Int -> Int
simpleDoubler value = 2 * value

main = do
	print (take 2 (replicate 100 "127.0.0.1"))
	print (take 5 (repeat "127.0.0.1"))
	print (simpleDoubler 10)

