module Main (
  main
            ) where
import           LighthouseApp (app)
import           Web.Spock

main :: IO ()
main = runSpock 8087 app
