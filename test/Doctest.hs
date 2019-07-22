module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)


main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XConstraintKinds"
        : "-XInstanceSigs"
        : "-XScopedTypeVariables"
        : "-XTypeApplications"
        : "-XViewPatterns"
        : "-XDerivingStrategies"
        : "-XGeneralizedNewtypeDeriving"
        : "-XDeriveGeneric"
        : sourceFiles
