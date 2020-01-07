module Cwis.PrintMethodSpec (spec) where

import           Control.Arrow             ((>>>))
import           Cwis.PrintMethod
import           Data.Default              (def)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (elements)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


spec :: Spec
spec = do
    describe "defaultSecurityPrint" $
        context "constructs a PrintMethod with required params" $ do
            prop "with a name required." $ \name pass filepath -> do
                let (SecurityPrint actual _ _ _) = defaultSecurityPrint name pass filepath
                actual `shouldBe` name
            prop "with a password required." $ \name pass filepath -> do
                let (SecurityPrint _ actual _ _) = defaultSecurityPrint name pass filepath
                actual `shouldBe` pass
            prop "with a filepath required." $ \name pass filepath -> do
                let (SecurityPrint _ _ actual _) = defaultSecurityPrint name pass filepath
                actual `shouldBe` filepath

    describe "actions of PrintMethodBuilderT" $ do
        context "numCopies" $
            prop "specifies the number of copies." $ \n -> do
                let (CommonOptions cpn _ _ _ _ _ _ _ _ _) = helpTestingAction $ numCopies n
                cpn `shouldBe` n
        context "doSort" $
            prop "specifies if you want to sort your papers." $ \b -> do
                let (CommonOptions _ colt _ _ _ _ _ _ _ _) = helpTestingAction $ doSort b
                colt `shouldBe` b
        context "onBothSides" $
            prop "specifies if you want to print on the both sides." $ fmap runWrapper >>> \b -> do
                let (CommonOptions _ _ dup _ _ _ _ _ _ _) = helpTestingAction $ onBothSides b
                dup `shouldBe` b
        context "colourMode" $
            prop "specifies a colour mode." $ runWrapper >>> \c -> do
                let (CommonOptions _ _ _ clr _ _ _ _ _ _) = helpTestingAction $ colourMode c
                clr `shouldBe` c
        context "withStaple" $
            prop "specifies if you want to staple your documents." $ fmap runWrapper >>> \s -> do
                let (CommonOptions _ _ _ _ stpl _ _ _ _ _) = helpTestingAction $ withStaple s
                stpl `shouldBe` s
        context "withPunch" $
            prop "specifies if you want to staple your documents." $ fmap runWrapper >>> \p -> do
                let (CommonOptions _ _ _ _ _ pnch _ _ _ _) = helpTestingAction $ withPunch p
                pnch `shouldBe` p
        context "outputTray" $
            prop "specifies an output tray." $ runWrapper >>> \t -> do
                let (CommonOptions _ _ _ _ _ _ ot _ _ _) = helpTestingAction $ outputTray t
                ot `shouldBe` t
        context "inputTray" $
            prop "specifies an input tray." $ runWrapper >>> \t -> do
                let (CommonOptions _ _ _ _ _ _ _ it' _ _) = helpTestingAction $ inputTray t
                it' `shouldBe` t
        context "paperSize" $
            prop "specifies a size of papers." $ fmap runWrapper >>> \s -> do
                let (CommonOptions _ _ _ _ _ _ _ _ siz _) = helpTestingAction $ paperSize s
                siz `shouldBe` s
        context "paperType" $
            prop "specifies a type of papers." $ fmap runWrapper >>> \t -> do
                let (CommonOptions _ _ _ _ _ _ _ _ _ med) = helpTestingAction $ paperType t
                med `shouldBe` t

        context "numCopies' takes a foldable argument like:" $ do
            prop "Maybe" $ \n -> do
                let (CommonOptions cpn _ _ _ _ _ _ _ _ _) = helpTestingAction $ numCopies' (Just n)
                cpn `shouldBe` n
            prop "List" $ \m n -> do
                let (CommonOptions cpn _ _ _ _ _ _ _ _ _) = helpTestingAction $ numCopies' [m, n]
                cpn `shouldBe` n
        context "when numCopies' takes an empty argument" $
            it "just does nothing" $
                helpTestingAction (numCopies' Nothing) `shouldBe` def

helpTestingAction :: PrintMethodBuilder a -> CommonOptions
helpTestingAction = commonOptions . execPrintMethodBuilder (defaultSecurityPrint "" "" "")

commonOptions :: PrintMethod -> CommonOptions
commonOptions (SecurityPrint _ _ _ c) = c

-- |A wrapper which implement 'Arbitrary' using 'Enum'.
newtype Wrapper a = Wrapper { runWrapper :: a }
    deriving (Show, Eq)

instance Enum a => Arbitrary (Wrapper a) where
    arbitrary = elements $ Wrapper <$> enumFrom (toEnum 0)
