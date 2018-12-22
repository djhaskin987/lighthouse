{-#LANGUAGE OverloadedStrings #-}
module Main where

import LighthouseApp (app)

import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

import Test.Hspec

main :: IO ()
main = hspec spec
spec :: Spec
spec =
  with (spockAsApp app) $ do
    describe "POST /assign-workloads" $ do
      it "is a minimal example" $
        post "/assign-workloads" "\
        \ { \
        \   \"nodes\": [ \
        \     { \
        \       \"id\": \"node-1\", \
        \       \"resources\": { \"cpu\": 2.0 } \
        \     } \
        \   ], \
        \   \"workloads\": [ \
        \     { \
        \       \"id\": \"workload-1\", \
        \       \"requirements\": { \"cpu\": 1.6 } \
        \     } \
        \   ] \
        \ }" `shouldRespondWith` "\
        \ { \
        \   \"successful\": true, \
        \   \"assignments\": { \
        \     \"workload-1\": \"node-1\" \
        \   } \
        \ }"
--     describe "GET /hello/:name" $ do
--       it "returns hello to spock" $
--         get "/hello/spock" `shouldRespondWith` "Hello spock"
--       it "returns hello to uhura" $
--         get "/hello/uhura" `shouldRespondWith` "Hello uhura"
-- 
