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
import Data.List
import qualified Data.Map as Map
import Data.Algorithm.Diff
import qualified System.Log.Logger as LOG
import System.Exit
import Control.Monad
import Control.Monad.Error (runErrorT)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid hiding (First)
import Data.Maybe (isNothing)

import Facter

import Puppet.Init
import Puppet.Daemon
import Puppet.Printers
import Puppet.Interpreter.Types
import Puppet.Testing
import Puppet.DSL.Printer
import Puppet.DSL.Loader
import Puppet.JsonCatalog
import PuppetDB.Rest

tshow :: Show a => a -> T.Text
tshow = T.pack . show

usage = error "Usage: puppetresource puppetdir nodename [filename]"

addRequire :: FinalCatalog -> ((ResIdentifier, ResIdentifier), LinkInfo) -> FinalCatalog
addRequire curcat ((src, dst), (ltype, _, _, _)) =
        case Map.lookup src curcat of
            Nothing -> curcat
            Just res -> Map.insert src (res { rrelations = (ltype, dst) : rrelations res }) curcat

{-| Does all the work of initializing a daemon for querying.
Returns the final catalog when given a node name. Note that this is pretty
hackish as it will generate facts from the local computer !
-}

initializedaemonWithPuppet :: Maybe T.Text -> T.Text -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog))
initializedaemonWithPuppet purl puppetdir = do
    LOG.updateGlobalLogger "Puppet.Daemon" (LOG.setLevel LOG.WARNING)
    prefs <- genPrefs puppetdir
    let nprefs = case purl of
                     Nothing -> prefs
                     Just ur -> prefs { puppetDBquery = pdbRequest ur }
    queryfunc <- if isNothing purl
                     then testingDaemon Nothing puppetdir allFacts
                     else do
                         (q, _, _, _) <- initDaemon nprefs
                         return (\nodename -> allFacts nodename >>= q nodename)
    return (\nodename -> do
        o <- queryfunc nodename
        case o of
            Left err -> error err
            Right (c,m,e) -> return (foldl' addRequire c (Map.toList m), m, e)
        )

{-| A helper for when you don't want to use PuppetDB -}
initializedaemon :: T.Text -> IO (T.Text -> IO (FinalCatalog, EdgeMap, FinalCatalog))
initializedaemon = initializedaemonWithPuppet Nothing

showparam :: (T.Text, ResolvedValue) -> T.Text
showparam (k,v) = k <> " => " <> tshow v

showtdiff :: Diff T.Text -> T.Text
showtdiff (First s)  = "- " <> s
showtdiff (Second s) = "+ " <> s
showtdiff (Both _ _) = ""

textdiff :: ResolvedValue -> ResolvedValue -> [T.Text]
textdiff (ResolvedString s1) (ResolvedString s2) = map showtdiff $ filter (not . isBoth) $ getDiff (T.lines s1) (T.lines s2)
    where
        isBoth Both{} = True
        isBoth _ = False
textdiff a b = error ("hum? " ++ show a ++ " " ++ show b)

showpdiff :: T.Text -> ResolvedValue -> ResolvedValue -> [T.Text]
showpdiff pname pval1 pval2
    | pname == "content" = "# content\n" : textdiff pval1 pval2
    | otherwise = ["- " <> showparam (pname, pval1), "+ " <> showparam (pname, pval2)]

paramdiff :: Map.Map T.Text ResolvedValue -> T.Text -> ResolvedValue -> [T.Text] -> [T.Text]
paramdiff lpmap rpname rpval curdiff = curdiff ++ newdiff
    where
        mlpval = Map.lookup rpname lpmap
        newdiff = case mlpval of
            Nothing -> []
            Just lpval ->
                if rpval == lpval
                    then []
                    else showpdiff rpname lpval rpval

getdiff :: ResIdentifier -> RResource -> RResource -> T.Text
getdiff (rtype,rname) r1 r2 = rtype <> "[" <> rname <> "] {\n" <> T.intercalate "\n" difflist <> "\n}"
    where
        difflist = diffrelations ++ diffparams
        diffrelations = []
        p1 = rrparams r1
        p2 = rrparams r2
        onlyleft  = Map.difference p1 p2
        onlyright = Map.difference p2 p1
        diffparams = ole ++ ori ++ distincts
        ole = map (\x -> "- " <> showparam x) $ Map.toList onlyleft
        ori = map (\x -> "+ " <> showparam x) $ Map.toList onlyright
        distincts = Map.foldrWithKey (paramdiff p1) [] p2

rescompare :: RResource -> RResource -> Bool
rescompare (RResource _ a1 b1 c1 d1 _ _) (RResource _ a2 b2 c2 d2 _ _) = (a1==a2) && (b1==b2) && (c1==c2) && (d1==d2)

checkdiff :: FinalCatalog -> ResIdentifier -> RResource -> (FinalCatalog, [T.Text]) -> (FinalCatalog, [T.Text])
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
                    else getdiff resid x res : curdiffs

{-| The diffing function, will output what is only in the first catalog, then
what is only in the second, and will end with a diff between common resources.
The /content/ parameter is handled in a special way so that the diff is
useful.
-}
diff :: FinalCatalog -> FinalCatalog -> IO ()
diff c1 c2 = do
    let onlyfirsts = Map.difference c1 c2
        (onlyseconds, differences) = Map.foldrWithKey (checkdiff c1) (Map.empty, []) c2
    unless (Map.null onlyfirsts)  $ T.putStrLn $ "Only in the first  catalog:\n" <> showFCatalog onlyfirsts
    unless (Map.null onlyseconds) $ T.putStrLn $ "Only in the second catalog:\n" <> showFCatalog onlyseconds
    mapM_ T.putStrLn differences

doparse :: FilePath -> IO ()
doparse fp = do
    parsed <- runErrorT (parseFile fp)
    case parsed of
        Right ps -> putStrLn $ showAST ps
        Left err -> error err
    exitWith ExitSuccess

-- prints the content of a file
printContent :: T.Text -> FinalCatalog -> IO ()
printContent filename catalog =
    case Map.lookup ("file", filename) catalog of
        Nothing -> error "File not found"
        Just r  -> case Map.lookup "content" (rrparams r) of
            Nothing -> error "This file has no content"
            Just (ResolvedString c)  -> T.putStrLn c
            Just x -> print x

-- pretty print a resource
-- if this resource has content, works like printContent
printResource :: T.Text -> T.Text -> FinalCatalog -> IO ()
printResource restype resname catalog =
    case Map.lookup (restype, resname) catalog of
        Nothing -> error $ T.unpack $ "Resource " <> restype <> "[" <> resname <> "] does not exists."
        Just r  -> T.putStrLn $ showFCatalog $ Map.singleton (restype, resname) r

r2s :: ResolvedValue -> T.Text
r2s (ResolvedString x)  = x
r2s x                   = tshow x

-- filters resources by type name
getResourcesOfType :: T.Text -> FinalCatalog -> FinalCatalog
getResourcesOfType rtype = Map.filter (\r -> rrtype r == rtype)

-- just extract the names of the resources
getResourceNames :: FinalCatalog -> [T.Text]
getResourceNames cat = map snd $ Map.keys cat

main :: IO ()
main = do
    args <- fmap (map T.pack) getArgs
    let (rargs, puppeturl) = case args of
                             ("-r":pu:xs) -> (xs,   Just pu)
                             _            -> (args, Nothing)
    when (length rargs == 1) (doparse (T.unpack $ head rargs))
    let (puppetdir, nodename) | (length rargs /= 2) && (length rargs /= 3) = usage
                              | otherwise = (rargs !! 0, rargs !! 1)
        getresname :: T.Text -> Maybe (T.Text, T.Text)
        getresname r =
            let isresname = T.last r == ']' && T.isInfixOf "[" r
                (rtype, rname) = T.break (== '[') r
            in if isresname
                then Just (T.map toLower rtype, T.tail $ T.init rname)
                else Nothing
        handlePrintResource resname cat
            = case getresname resname of
                Just (t,n) -> printResource t n cat
                Nothing    -> printContent resname cat

    queryfunc <- initializedaemonWithPuppet puppeturl puppetdir
    (x,m,e) <- queryfunc nodename
    if length rargs == 3
        then if (rargs !! 2) == "JSON"
                 then do
                     let json = catalog2JSon nodename 1 x e m
                     BSL.putStrLn json
                 else handlePrintResource (rargs !! 2) x
        else do
            (tests,_) <- testCatalog puppetdir x []
            case tests of
                Right _ -> return ()
                Left rr -> error rr
            T.putStrLn $ showFCatalog x
