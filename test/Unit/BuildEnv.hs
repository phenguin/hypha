{-# LANGUAGE OverloadedStrings #-}
module Unit.BuildEnv (tests) where

import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.BuildEnv.Cabal
  (CabalStoreError (..), ghcDirVersion, mkCabalBuildEnv, resolveStoreDir)
import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Types.PackageId
  (PackageName (..), Version (..), PackageId (..), UnitId (..))

tests :: TestTree
tests = testGroup "Unit.BuildEnv"
  [ testMockReturnsConfiguredPaths
  , testCabalFindsAsync
  , testCabalLocateHaddock
  , testCabalMissingPackage
  , testGhcDirVersion
  , testAbiSuffixedVersion
  , testResolveByPlanUnits
  , testResolveLoneDir
  , testResolveAmbiguous
  , testResolveNoCompilerDir
  ]

testMockReturnsConfiguredPaths :: TestTree
testMockReturnsConfiguredPaths = testCase "Mock returns configured source paths" $ do
  let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      mock = emptyMock
        { mockPackages = Map.fromList
            [ (asyncId, (Just "/src/async", Just "/doc/async/index.html"))
            ]
        , mockGhcVersion = Version "9.6.7"
        }
      env = mkMockBuildEnv mock
  -- Check discoverInstalledPackages
  pkgs <- discoverInstalledPackages env
  pkgs @?= Set.singleton asyncId
  -- Check locatePackageSource
  src <- locatePackageSource env asyncId
  src @?= Just "/src/async"
  -- Check locateHaddockHtml
  doc <- locateHaddockHtml env asyncId
  doc @?= Just "/doc/async/index.html"
  -- Check ghcVersion
  ver <- ghcVersion env
  ver @?= Version "9.6.7"

testCabalFindsAsync :: TestTree
testCabalFindsAsync = testCase "Cabal impl finds async-2.2.5 in fake store fixture" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      -- Check that async is discovered
      pkgs <- discoverInstalledPackages env
      let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      assertBool "async should be in discovered packages" (asyncId `Set.member` pkgs)
      -- Check GHC version
      ver <- ghcVersion env
      ver @?= Version "9.6.7"

testCabalLocateHaddock :: TestTree
testCabalLocateHaddock = testCase "Cabal locateHaddockHtml hits stub index.html" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      haddock <- locateHaddockHtml env asyncId
      case haddock of
        Nothing -> error "Expected to find Haddock HTML"
        Just path -> do
          -- The path should end with the expected suffix
          assertBool "path should end with index.html" ("index.html" `isSuffixOf` path)
          assertBool "path should contain async-2.2.5" ("async-2.2.5" `isInfixOf` path)

testCabalMissingPackage :: TestTree
testCabalMissingPackage = testCase "Cabal impl returns Nothing for missing package" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      let missingId = PackageId (PackageName "nonexistent") (Version "1.0.0")
      -- locatePackageSource should return Nothing for missing package
      src <- locatePackageSource env missingId
      src @?= Nothing
      -- locateHaddockHtml should return Nothing for missing package
      doc <- locateHaddockHtml env missingId
      doc @?= Nothing

testGhcDirVersion :: TestTree
testGhcDirVersion = testCase "ghcDirVersion reads plain and ABI-suffixed store dirs" $ do
  ghcDirVersion "ghc-9.10.3"              @?= Just "9.10.3"
  ghcDirVersion "ghc-9.10.3-1be3"         @?= Just "9.10.3"
  ghcDirVersion "ghc-10.1.20260721-inplace" @?= Just "10.1.20260721"
  ghcDirVersion "ghc-"                    @?= Nothing
  ghcDirVersion "ghc-9.10.3x"             @?= Nothing
  ghcDirVersion "package.db"              @?= Nothing

testAbiSuffixedVersion :: TestTree
testAbiSuffixedVersion = testCase "Cabal impl drops the ABI suffix from the GHC version" $
  withSystemTempDirectory "hypha-store" $ \base -> do
    let dir = base </> "ghc-9.12.4-9710"
    createDirectoryIfMissing True dir
    result <- mkCabalBuildEnv dir
    case result of
      Left err  -> error ("Expected Right, got: " ++ show err)
      Right env -> ghcVersion env >>= (@?= Version "9.12.4")

-- | A store base with the given @ghc-*@ directories, each holding the
-- given unit directories.
withStore :: [(FilePath, [String])] -> (FilePath -> IO a) -> IO a
withStore dirs k = withSystemTempDirectory "hypha-store" $ \base -> do
  sequence_ [ createDirectoryIfMissing True (base </> d </> u)
            | (d, us) <- dirs, u <- "package.db" : us ]
  k base

testResolveByPlanUnits :: TestTree
testResolveByPlanUnits = testCase "resolveStoreDir picks the ABI dir holding the plan's units" $
  withStore [ ("ghc-9.12.4-6f4d", ["other-1.0-aaaa"])
            , ("ghc-9.12.4-9710", ["mrkdwn-nlt-0.6.0-05544cb8"])
            , ("ghc-9.10.3-1be3", ["mrkdwn-nlt-0.6.0-05544cb8"]) ] $ \base -> do
    let units = [UnitId "base-4.21.2.0", UnitId "mrkdwn-nlt-0.6.0-05544cb8"]
    r <- resolveStoreDir base (Just "9.12.4") units
    r @?= Right (base </> "ghc-9.12.4-9710")
    -- Without a compiler version the units alone decide.
    r' <- resolveStoreDir base Nothing [UnitId "other-1.0-aaaa"]
    r' @?= Right (base </> "ghc-9.12.4-6f4d")

testResolveLoneDir :: TestTree
testResolveLoneDir = testCase "resolveStoreDir takes the only dir for the compiler without evidence" $
  withStore [ ("ghc-9.10.3-1be3", []), ("ghc-9.6.7", []) ] $ \base -> do
    r <- resolveStoreDir base (Just "9.10.3") [UnitId "base-4.20.1.0"]
    r @?= Right (base </> "ghc-9.10.3-1be3")

testResolveAmbiguous :: TestTree
testResolveAmbiguous = testCase "resolveStoreDir reports several dirs rather than guessing" $
  withStore [ ("ghc-9.12.4-9710", []), ("ghc-9.12.4-6f4d", []) ] $ \base -> do
    r <- resolveStoreDir base (Just "9.12.4") []
    r @?= Left (AmbiguousStore base ["ghc-9.12.4-6f4d", "ghc-9.12.4-9710"])

testResolveNoCompilerDir :: TestTree
testResolveNoCompilerDir = testCase "resolveStoreDir names the compiler it found no dir for" $
  withStore [ ("ghc-9.6.7", []) ] $ \base -> do
    r <- resolveStoreDir base (Just "9.12.4") []
    r @?= Left (NoStoreForCompiler base (Just "9.12.4"))
    missing <- resolveStoreDir (base </> "nope") Nothing []
    missing @?= Left (StoreNotFound (base </> "nope"))
