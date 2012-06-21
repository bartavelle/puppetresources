{- Horrible and hackish ... but useful
 
Typical ghci usage :
    
queryfunc <- initializedaemon "/home/user/git/puppet/"
c1 <- queryfunc "host1.test"
c2 <- queryfunc "host2"
diff c1 c2

-}
import System.Environment
import Puppet.Init
import Puppet.Daemon
import Puppet.Printers
import Puppet.Interpreter.Types
import Facter
import Data.List
import System.IO
import qualified Data.Map as Map
import Data.Algorithm.Diff
import qualified System.Log.Logger as LOG

usage = error "Usage: puppetresource puppetdir nodename"

initializedaemon puppetdir = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.DEBUG)
    rawfacts <- allFacts
    queryfunc <- initDaemon (genPrefs puppetdir)
    return (\nodename -> do
        let ofacts = genFacts rawfacts
            (hostname, ddomainname) = break (== '.') nodename
            domainname = tail $ ddomainname
            nfacts = genFacts [("fqdn", nodename), ("hostname", hostname), ("domain", domainname)]
            allfacts = Map.union ofacts nfacts
        o <- queryfunc nodename allfacts
        case o of
            Left err -> error err
            Right x -> return x
        )

showparam (k,v) = k ++ " => " ++ show v

showtdiff :: (DI, String) -> String
showtdiff (F, s) = "- " ++ s
showtdiff (S, s) = "+ " ++ s

textdiff :: ResolvedValue -> ResolvedValue -> [String]
textdiff (ResolvedString s1) (ResolvedString s2) = map showtdiff $ filter (\(x,_) -> x /= B) $ getDiff (lines s1) (lines s2)

showpdiff :: String -> ResolvedValue -> ResolvedValue -> [String]
showpdiff pname pval1 pval2
    | pname == "content" = ["# content\n"] ++ textdiff pval1 pval2
    | otherwise = ["- " ++ showparam (pname, pval1), "+ " ++ showparam (pname, pval2)]

paramdiff :: Map.Map String ResolvedValue -> String -> ResolvedValue -> [String] -> [String]
paramdiff lpmap rpname rpval curdiff = curdiff ++ newdiff
    where
        mlpval = Map.lookup rpname lpmap
        newdiff = case mlpval of
            Nothing -> []
            Just lpval ->
                if rpval == lpval
                    then []
                    else showpdiff rpname lpval rpval


getdiff :: ResIdentifier -> RResource -> RResource -> String
getdiff (rtype,rname) r1 r2 = rtype ++ "[" ++ rname ++ "]" ++ " {\n" ++ (concatMap (\x -> x ++"\n") difflist) ++ "}"
    where
        difflist = diffrelations ++ diffparams
        diffrelations = []
        p1 = rrparams r1
        p2 = rrparams r2
        onlyleft  = Map.difference p1 p2
        onlyright = Map.difference p2 p1
        diffparams = ol ++ or ++ distincts
        ol = map (\x -> "- " ++ showparam x) $ Map.toList onlyleft
        or = map (\x -> "+ " ++ showparam x) $ Map.toList onlyright
        distincts = Map.foldrWithKey (paramdiff p1) [] p2

rescompare :: RResource -> RResource -> Bool
rescompare (RResource _ a1 b1 c1 d1 _) (RResource _ a2 b2 c2 d2 _) = (a1==a2) && (b1==b2) && (c1==c2) && (d1==d2)

checkdiff :: FinalCatalog -> ResIdentifier -> RResource -> (FinalCatalog, [String]) -> (FinalCatalog, [String])
checkdiff refmap resid res (curseconds, curdiffs) = (newseconds, newdiffs)
    where
        myres = Map.lookup resid refmap
        newseconds = case myres of
            Nothing -> Map.insert resid res curseconds
            Just _  -> curseconds
        newdiffs = case myres of
            Nothing -> curdiffs
            Just x ->
                if rescompare x res
                    then curdiffs
                    else (getdiff resid x res) : curdiffs

diff :: FinalCatalog -> FinalCatalog -> IO ()
diff c1 c2 = do
    let onlyfirsts = Map.difference c1 c2
        (onlyseconds, differences) = Map.foldrWithKey (checkdiff c1) (Map.empty, []) c2
    print onlyfirsts
    print onlyseconds
    mapM_ putStrLn differences

main = do
    args <- getArgs
    let (puppetdir, nodename) | (length args /= 2) && (length args /= 3) = usage
                              | otherwise = (args !! 0, args !! 1)
    queryfunc <- initializedaemon puppetdir
    x <- queryfunc nodename
    if length args == 3
        then case (Map.lookup ("file",args !! 2) x) of
            Nothing -> error "File not found"
            Just r  -> case (Map.lookup "content" (rrparams r)) of
                Nothing -> error "This file has no content"
                Just (ResolvedString c)  -> putStrLn c
                Just x -> print x
        else putStrLn $ showFCatalog x
