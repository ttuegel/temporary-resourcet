module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad ( liftM )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Resource
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.IO.Temp

main :: IO ()
main = defaultMain $ testGroup "temporary-resourcet"
    [ testGroup "createTempDirectory"
        [ testCase "release" $ assert test_createTempDirectory_release
        , testCase "runResourceT" $ assert test_createTempDirectory_runResourceT
        ]
    , testGroup "openBinaryTempFile"
        [ testCase "release" $ assert test_openBinaryTempFile_release
        , testCase "runResourceT" $ assert test_openBinaryTempFile_runResourceT
        ]
    , testGroup "openTempFile"
        [ testCase "release" $ assert test_openTempFile_release
        , testCase "runResourceT" $ assert test_openTempFile_runResourceT
        ]
    ]

test_createTempDirectory_release :: IO Bool
test_createTempDirectory_release =
    runResourceT $ do
        (key, path) <- createTempDirectory Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesDirectoryExist path

test_createTempDirectory_runResourceT :: IO Bool
test_createTempDirectory_runResourceT = do
    path <- runResourceT $ do
        (_, path) <- createTempDirectory Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesDirectoryExist path

test_openBinaryTempFile_release :: IO Bool
test_openBinaryTempFile_release =
    runResourceT $ do
        (key, path, _) <- openBinaryTempFile Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesFileExist path

test_openBinaryTempFile_runResourceT :: IO Bool
test_openBinaryTempFile_runResourceT = do
    path <- runResourceT $ do
        (_, path, _) <- openBinaryTempFile Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesFileExist path

test_openTempFile_release :: IO Bool
test_openTempFile_release =
    runResourceT $ do
        (key, path, _) <- openTempFile Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesFileExist path

test_openTempFile_runResourceT :: IO Bool
test_openTempFile_runResourceT = do
    path <- runResourceT $ do
        (_, path, _) <- openTempFile Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesFileExist path
