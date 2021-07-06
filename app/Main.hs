{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Payments.Mpesa.Auth
main :: IO ()
main = do
    res <- fetchAccessToken "https://sandbox.safaricom.co.ke" "username" "password"
    case res of
        Left err -> putStrLn "bingo"
        Right body ->
            let token = aToken body
            in putStrLn token