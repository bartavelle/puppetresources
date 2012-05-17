import System.Environment
import Puppet.Init
import Puppet.Daemon
import Facter
import Data.List 

usage = error "Usage: puppetresource puppetdir nodename"

main = do
    args <- getArgs
    let (puppetdir, nodename) | (length args /= 2) = usage
                                | otherwise = (args !! 0, args !! 1)
        prefs = genPrefs puppetdir
        facts = genFacts []
    queryfunc <- initDaemon prefs
    rawfacts <- allFacts
    mapM print (sort rawfacts)
    return ()
--    resp <- queryfunc nodename facts
--    print resp
