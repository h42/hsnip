import Database.SQLite3
import Text.Printf
import Control.Monad
import System.IO.Error
import Control.Exception (bracket)

-----------------------------------------------
exec conn sql = bracket (prepare conn sql)
			(\stmt -> finalize stmt)
			(\stmt -> step stmt)

-----------------------------------------------
in_sql = "insert into t1 values(?,?)"

inserter conn cnt = bracket (prepare conn in_sql)
			    (\stmt -> finalize stmt)
			    (\stmt -> insrows 0 cnt stmt)

insrows i n stmt = do
    let c1 = "jerry " ++ (show (i::Int))
    bindText stmt 1 c1
    bindInt  stmt 2 i
    step stmt
    when (i+1 < n) $ do
	reset stmt
	insrows (i+1) n stmt

-----------------------------------------------
mysql = "select * from t1 limit 3;"
mysql2 = "select count (*) from t1;" -- will fail because of bad patter in let

sql1 conn = bracket (prepare conn mysql)
		    (\stmt -> finalize stmt)
		    (\stmt -> prows stmt)

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
	exec conn "drop table t1"
	exec conn "create table t1 (c1 text, c2 integer)"
	exec conn "begin"
	exec conn "delete from t1"

	inserter conn 50000
	sql1 conn

	exec conn "commit"
	close conn
