data Rec = Rec {zx :: Int, zy :: Int} deriving (Show)

initrec = Rec{zx=0,zy=0}

trec r@Rec{zx=0} = "Got Zero " ++ show r
trec _ = "Help"
