import Control.Monad

main = do
	line <- getLine 
	let lineSum = sum . map read . words $ line
	when ( lineSum /= 0 ) $ do
		let [sleepHour, sleepMin, wakeHour, wakeMin] = map read $ words line
		let totalMins = minuteCount 0 sleepHour sleepMin wakeHour wakeMin 
		print totalMins 
		main

minuteCount :: (Eq a1, Eq a, Num a, Num a1, Num a2) => a2 -> a1 -> a -> a1 -> a -> a2
minuteCount n sHour sMin wHour wMin = 
    if sHour == wHour && sMin == wMin
   		then n
    else
        if sMin == 59
   	    	then 
        		if sHour == 23
            		then minuteCount (n + 1) 0 0 wHour wMin
            	else minuteCount (n + 1) (sHour + 1) 0 wHour wMin
        else minuteCount (n + 1) sHour (sMin + 1) wHour wMin
