module Main where
import Control.Monad.Reader
import NCN.App
import NCN.Config.Openshift
import NCN.Scotty

main :: IO ()
main = getConfig >>= (runReaderT $ ncnScottyT (handleIndex >> handleToilets))
