import System.IO
import Random
import Text.Printf

-- record not necessary but I wanted to see how they work
data Ps = Ps { gtot :: Int
	       ,gsticks :: Int
	       ,grem :: Int
	      }

--getrand :: Int -> Int -> IO Int
getrand l h = getStdRandom $ randomR (l,h)

play ps = do
    printf "\nYour turn - enter a number from 1 to %d => " (gsticks ps)
    hFlush stdout
    ix <- getLine
    let x = read ix :: Int
    let tot = (gtot ps) - x
    if tot <= 0
	then putStrLn "You win !!!!!"
	else do
	    printf "%d sticks remaining\n" tot
	    let opt1 = mod tot $ (gsticks ps) + 1
	    optr <- getrand 1 (gsticks ps) -- count on lazy eval to not bother
	    let optpick =  if opt1 == 0 then optr else opt1
	    let ctot = if tot <= (gsticks ps) then tot else optpick
	    if tot - ctot <= 0
		then printf "I took %d sticks\nI win !!!!" ctot
		else do
		    printf "I took %d sticks\n%d sticks remaining\n" ctot (tot-ctot)
		    play (ps {gtot = tot - ctot})

newGame = do
    sticks <- getrand 7 11
    tot <- getrand (3*sticks) (4*sticks)
    printf "New game\nPile has %d sticks\nYou can take from 1 to %d\n"
	    tot sticks
    play (Ps tot sticks tot)

main = do
    newGame
