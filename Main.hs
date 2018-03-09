{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Base16 (encode, decode)

import System.Exit

import FLib

main :: IO ()
main = do
    let input = fst $ decode "39129b3f7bbd7e17a39679b940018a737fc3bf430fcbc827029e67360aab3707"
    let expected = fst $ decode "1cc4789ed5ea69f84ad460941ba0491ff532c1af1fa126733d6c7b62f7ebcbcf"
    let actual = FLib.unsafeScalarBaseMult input
    putStrLn $ "expected: " ++ (show $ encode expected)
    putStrLn $ "  actual: " ++ (show $ encode actual)
    if actual == expected then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)
