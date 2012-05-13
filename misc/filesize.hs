import IO

getFileSize :: String -> IO (Maybe Integer)
getFileSize path = catch
    (bracket (openFile path ReadMode) hClose $
	\h -> do
	    size <- hFileSize h
	    return (Just size) )
    (\e -> return Nothing)
