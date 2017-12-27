module Main (main) where

import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, testGroup)
import Test.Tasty.Runners.AntXML (antXMLRunner)

import qualified Galois
import qualified Matrix
import qualified ReedSolomon
import qualified Vector

tests :: TestTree
tests = testGroup "Tests" [ Galois.tests
                          , Matrix.tests
                          , ReedSolomon.tests
                          , Vector.tests
                          ]

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = antXMLRunner : defaultIngredients
