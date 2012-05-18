import System.Environment
import Puppet.Init
import Puppet.Daemon
import Puppet.Printers
import Facter
import Data.List
import System.IO
import qualified Data.Map as Map

usage = error "Usage: puppetresource puppetdir nodename"

main = do
    args <- getArgs
    let (puppetdir, nodename) | (length args /= 2) = usage
                                | otherwise = (args !! 0, args !! 1)
        prefs = genPrefs puppetdir
    queryfunc <- initDaemon prefs
    rawfacts <- allFacts
    let ofacts = genFacts rawfacts
        (hostname, ddomainname) = break (== '.') nodename
        domainname = tail $ ddomainname
        nfacts = genFacts [("fqdn", nodename), ("hostname", hostname), ("domain", domainname)]
        allfacts = Map.union ofacts nfacts
    resp <- queryfunc nodename allfacts
    case resp of
        Left err -> hPutStrLn stderr err
        Right x -> putStrLn $ showFCatalog x
