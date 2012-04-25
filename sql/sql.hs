import Database.SQLite3
import Text.Printf
import Control.Monad (when)
import Control.Exception (bracket)

-----------------------------------------------
execE conn sql = execEf conn sql step

exec conn sql = execf conn sql step

execEf conn sql f = flip catch
    (\e -> return (Left ("Failed: " ++ sql)))
    $ do
	bracket (prepare conn sql)
		(\stmt -> finalize stmt)
		(\stmt -> f stmt)
	return (Right "OK")

execf conn sql f = bracket
    (prepare conn sql)
    (\stmt -> finalize stmt)
    (\stmt -> f stmt)

-----------------------------------------------
insrows_sql = "insert into t1 values(?,?)"

insrows :: Int -> Int -> Statement -> IO ()
insrows i n stmt = do
    let c1 = "jerry " ++ (show i)
    bind stmt [SQLText c1, SQLInteger $ fromIntegral i]
    step stmt
    return ()
    when (i+1 < n) $ do
	reset stmt
	insrows (i+1) n stmt

-----------------------------------------------
prows_sql = "select * from t1 limit 3;"
    -- "select count (*) from t1;" -- will fail because of bad patter in let

prows stmt = do
    rc <- step stmt
    print rc
    if rc == Done then return ()
    else do
	cols <- columns stmt
	let (SQLText c1:SQLInteger i1:xs) = cols
	print cols
	printf "c1='%s'  i1=%d\n\n" c1 i1
	prows stmt

-----------------------------------------------

main = do
    flip catch (\e -> print e) $ do
	conn <- open "test.db"

	-- Ignore error
	execE conn "drop table t1"
	execE conn "drop table t1"

	exec conn "create table t1 (c1 text, c2 integer)"
	exec conn "begin"
	exec conn "delete from t1"

	execf conn insrows_sql (insrows 0 50000)
	execf conn prows_sql prows

	exec conn "commit"
	close conn
