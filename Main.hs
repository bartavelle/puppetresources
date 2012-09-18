{-|

Horrible and hackish ... but damn useful. This can be used as a standalone
executable or a ghci script for interactive usage. It comes with a sample Puppet
site (that I hope will gets more realistic later).

When given a single argument, it will try to parse the given file, and will
print the parsed values :

> $ puppetresources samplesite/manifests/site.pp
> node test.nod {
>     apt::builddep { Interpolable [Literal "glusterfs-server"]:
>     ;
>     }
>     ...

With two arguments, it will try to compute the catalog for a given node. The
first argument must be the path to the Puppet directory and the second the
name of the node. Note that regexp node names do not work yet.

> $ puppetresources samplesite test.nod
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/ppa.pp" (line 20, column 3)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 38, column 7)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 42, column 7)
> anchor {
>     "apt::builddep::glusterfs-server": #"samplesite/modules/apt/manifests/builddep.pp" (line 12, column 12)
>         name => "apt::builddep::glusterfs-server";
>     "apt::key/Add key: 55BE302B from Apt::Source debian_unstable": #"samplesite/modules/apt/manifests/key.pp" (line 32, column 16)
>         name => "apt::key/Add key: 55BE302B from Apt::Source debian_unstable";
> ...

When adding a file name as the third argument to the previous invocation, it
will display the value of the /content/ attribute of the named /file/ resource.

> $ puppetresources samplesite test.nod karmic.pref
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/ppa.pp" (line 20, column 3)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 38, column 7)
> The defined() function is not implemented for resource references. Returning true at "samplesite/modules/apt/manifests/key.pp" (line 42, column 7)
> # karmic
> Package: *
> Pin: release a=karmic
> Pin-Priority: 700

You can also just use a resource name :

> $ puppetresources samplesite test.nod 'exec[apt_update]'
> exec {
>   "apt_update": #"samplesite/modules/apt/manifests/update.pp" (line 4, column 10)
>       command     => "/usr/bin/apt-get update",
>       logoutput   => "false",
>       refreshonly => "true",
>       returns     => 0,
>       timeout     => 300,
>       tries       => 1,
>       try_sleep   => 0;
> }

With GHCI there are tons of things you can do. First initialize it

>>> queryfunc <- initializedaemon "./samplesite/"

You can now compute catalogs for various nodes.

>>> c1 <- queryfunc "test.nod"
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/ppa.pp" (line 20, column 3)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 38, column 7)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 42, column 7)
>>> c2 <- queryfunc "test2.nod"
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/ppa.pp" (line 20, column 3)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 38, column 7)
The defined() function is not implemented for resource references. Returning true at "./samplesite//modules/apt/manifests/key.pp" (line 42, column 7)

And you can check what the difference is between catalogs.

>>> diff c1 c2
file[karmic-updates.pref] {
# content
    + Pin-Priority: 750
    - Pin-Priority: 700
}

You also can manipulate catalogs.

>>> Map.size c1
25
>>> mapM_ print $ Map.toList $ Map.map (length . lines . (\x -> case x of (ResolvedString n) -> n) .fromJust . Map.lookup "content" . rrparams) $ Map.filter (Map.member "content" . rrparams) c1
(("file","debian_unstable.list"),3)
(("file","debian_unstable.pref"),4)
(("file","karmic-security.pref"),4)
(("file","karmic-updates.pref"),4)
(("file","karmic.pref"),4)

A typical usage of this tool is to compute a reference catalog, and then check the differences as you alter it. This can be done this way :

>>> reference <- queryfunc "test.nod"

And then run the following command every time you need to verify your changes are correct :

>>> queryfunc "test.nod" >>= diff reference

-}
module Main (initializedaemon, diff, main) where 

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
import Puppet.DSL.Loader
import System.Exit
import Control.Monad
import Control.Monad.Error (runErrorT)
import Puppet.DSL.Printer
import Data.Char (toLower)

usage = error "Usage: puppetresource puppetdir nodename [filename]"

{-| Does all the work of initializing a daemon for querying.
Returns the final catalog when given a node name. Note that this is pretty
hackish as it will generate facts from the local computer !
-}

initializedaemon :: String -> IO ([Char] -> IO FinalCatalog)
initializedaemon puppetdir = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.INFO)
    queryfunc <- initDaemon (genPrefs puppetdir)
    return (\nodename -> do
        o <- allFacts nodename >>= queryfunc nodename
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

{-| The diffing function, will output what is only in the first catalog, then
what is only in the second, and will end with a diff between common resources.
The /content/ parameter is handled in a special way so that the diff is
useful.
-}
diff :: FinalCatalog -> FinalCatalog -> IO ()
diff c1 c2 = do
    let onlyfirsts = Map.difference c1 c2
        (onlyseconds, differences) = Map.foldrWithKey (checkdiff c1) (Map.empty, []) c2
    if Map.null onlyfirsts
        then return ()
        else putStrLn $ "Only in the first  catalog:\n" ++ showFCatalog onlyfirsts
    if Map.null onlyseconds
        then return ()
        else putStrLn $ "Only in the second catalog:\n" ++ showFCatalog onlyseconds
    mapM_ putStrLn differences

doparse :: FilePath -> IO ()
doparse fp = do
    parsed <- runErrorT (parseFile fp)
    case parsed of
        Right ps -> putStrLn $ showAST ps
        Left err -> error err
    exitWith ExitSuccess

-- prints the content of a file
printContent :: String -> FinalCatalog -> IO ()
printContent filename catalog =
    case (Map.lookup ("file", filename) catalog) of
        Nothing -> error "File not found"
        Just r  -> case (Map.lookup "content" (rrparams r)) of
            Nothing -> error "This file has no content"
            Just (ResolvedString c)  -> putStrLn c
            Just x -> print x

-- pretty print a resource
-- if this resource has content, works like printContent
printResource :: String -> String -> FinalCatalog -> IO ()
printResource restype resname catalog =
    case (Map.lookup (restype, resname) catalog) of
        Nothing -> error $ "Resource " ++ restype ++ "[" ++ resname ++ "] does not exists."
        Just r  -> putStrLn $ showFCatalog $ Map.singleton (restype, resname) r

r2s :: ResolvedValue -> String
r2s (ResolvedString x)  = x
r2s x                   = show x

-- filters resources by type name
getResourcesOfType :: String -> FinalCatalog -> FinalCatalog
getResourcesOfType rtype = Map.filter (\r -> rrtype r == rtype)

-- just extract the names of the resources
getResourceNames :: FinalCatalog -> [String]
getResourceNames cat = map snd $ Map.keys cat

main :: IO ()
main = do
    args <- getArgs
    when (length args == 1) (doparse  (head args))
    let (puppetdir, nodename) | (length args /= 2) && (length args /= 3) = usage
                              | otherwise = (args !! 0, args !! 1)
        getresname :: String -> Maybe (String, String)
        getresname r =
            let isresname = ((head $ reverse r) == ']') && ('[' `elem` r)
                (rtype, rname) = break (== '[') r
            in if isresname
                then Just (map toLower rtype, tail $ reverse $ (tail $ reverse rname))
                else Nothing
        handlePrintResource resname cat
            = case (getresname resname) of
                Just (t,n) -> printResource t n cat
                Nothing    -> printContent resname cat

    queryfunc <- initializedaemon puppetdir
    x <- queryfunc nodename
    if length args == 3
        then handlePrintResource (args !! 2) x
        else putStrLn $ showFCatalog x
