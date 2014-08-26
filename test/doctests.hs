import           Control.Applicative
import           System.Process
import           Test.DocTest

main = doctest =<< lines <$> readProcess "git" ["ls-files", "src"] []
