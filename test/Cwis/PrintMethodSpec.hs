module Cwis.PrintMethodSpec (spec) where

import           Control.Arrow             ((>>>))
import           Control.Lens              hiding (elements)
import           Cwis.PrintMethod
import           Data.Default              (def)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (elements)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


spec :: Spec
spec =
    describe "commonOptions" $
        context "satisfies Lens laws:" $ do
            prop "1. You get back what you put in." $
                runCommonOptionsWrapper >>> \v -> do
                    let s = SecurityPrint "" "" "" def
                    view commonOptions (set commonOptions v s) == v
            prop "2. Putting back what you got doesn't change anything." $
                runCommonOptionsWrapper >>> SecurityPrint "" "" "" >>> \s ->
                    set commonOptions (view commonOptions s) s == s
            prop "3. Setting twice is the same as setting once." $ \wv wv' -> do
                    let v  = runCommonOptionsWrapper wv
                        v' = runCommonOptionsWrapper wv'
                        s  = SecurityPrint "" "" "" def
                    set commonOptions v' (set commonOptions v s) == set commonOptions v' s

newtype CommonOptionsWrapper = CommonOptionsWrapper
    { runCommonOptionsWrapper :: CommonOptions
    } deriving (Show, Eq)

instance Arbitrary CommonOptionsWrapper where
    arbitrary = fmap CommonOptionsWrapper $
        CommonOptions
            <$> arbitrary
            <*> arbitrary
            <*> (fmap runArbitEnum <$> arbitrary)
            <*> (runArbitEnum <$> arbitrary)
            <*> (fmap runArbitEnum <$> arbitrary)
            <*> (fmap runArbitEnum <$> arbitrary)
            <*> (runArbitEnum <$> arbitrary)
            <*> (runArbitEnum <$> arbitrary)
            <*> (fmap runArbitEnum <$> arbitrary)
            <*> (fmap runArbitEnum <$> arbitrary)

--- |A wrapper which implement 'Arbitrary' using 'Enum'.
newtype ArbitEnum a = ArbitEnum { runArbitEnum :: a }
    deriving (Show, Eq)

instance Enum a => Arbitrary (ArbitEnum a) where
    arbitrary = elements $ ArbitEnum <$> enumFrom (toEnum 0)
