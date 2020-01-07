module Cwis.PrintMethodSpec (spec) where

import           Cwis.PrintMethod
import           Data.Default              (def)
import           Test.Hspec
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Gen, elements)
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
            prop "specifies if you want to print on the both sides." $ \b -> do
                let b' = runDuplexWrapper <$> b
                    (CommonOptions _ _ dup _ _ _ _ _ _ _) = helpTestingAction $ onBothSides b'
                dup `shouldBe` b'
        context "colourMode" $
            prop "specifies a colour mode." $ \c -> do
                let c' = runColourWrapper c
                    (CommonOptions _ _ _ clr _ _ _ _ _ _) = helpTestingAction $ colourMode c'
                clr `shouldBe` c'
        context "withStaple" $
            prop "specifies if you want to staple your documents." $ \s -> do
                let s' = runStapleWrapper <$> s
                    (CommonOptions _ _ _ _ stpl _ _ _ _ _) = helpTestingAction $ withStaple s'
                stpl `shouldBe` s'
        context "withPunch" $
            prop "specifies if you want to staple your documents." $ \p -> do
                let p' = runPunchWrapper <$> p
                let (CommonOptions _ _ _ _ _ pnch _ _ _ _) = helpTestingAction $ withPunch p'
                pnch `shouldBe` p'
        context "outputTray" $
            prop "specifies an output tray." $ \t -> do
                let t' = runOutputTrayWrapper t
                    (CommonOptions _ _ _ _ _ _ ot _ _ _) = helpTestingAction $ outputTray t'
                ot `shouldBe` t'
        context "inputTray" $
            prop "specifies an input tray." $ \t -> do
                let t' = runInputTrayWrapper t
                    (CommonOptions _ _ _ _ _ _ _ it' _ _) = helpTestingAction $ inputTray t'
                it' `shouldBe` t'
        context "paperSize" $
            prop "specifies a size of papers." $ \s -> do
                let s' = runPaperSizeWrapper <$> s
                    (CommonOptions _ _ _ _ _ _ _ _ siz _) = helpTestingAction $ paperSize s'
                siz `shouldBe` s'
        context "paperType" $
            prop "specifies a type of papers." $ \t -> do
                let t' = runPaperTypeWrapper t
                    (CommonOptions _ _ _ _ _ _ _ _ _ med) = helpTestingAction $ paperType t'
                med `shouldBe` t'

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

getArbitrary :: Enum a => (a -> w) -> Gen w
getArbitrary f = elements $ f <$> enumFrom (toEnum 0)

-- Wrappers which implement Arbitrary

newtype DuplexWrapper = DuplexWrapper { runDuplexWrapper :: Duplex }
    deriving (Show, Eq)

instance Arbitrary DuplexWrapper where
    arbitrary = getArbitrary DuplexWrapper

newtype ColourWrapper = ColourWrapper { runColourWrapper :: Colour }
    deriving (Show, Eq)

instance Arbitrary ColourWrapper where
    arbitrary = getArbitrary ColourWrapper

newtype StapleWrapper = StapleWrapper { runStapleWrapper :: Staple }
    deriving (Show, Eq)

instance Arbitrary StapleWrapper where
    arbitrary = getArbitrary StapleWrapper

newtype PunchWrapper = PunchWrapper { runPunchWrapper :: Punch }
    deriving (Show, Eq)

instance Arbitrary PunchWrapper where
    arbitrary = getArbitrary PunchWrapper

newtype OutputTrayWrapper = OutputTrayWrapper { runOutputTrayWrapper :: OutputTray }
    deriving (Show, Eq)

instance Arbitrary OutputTrayWrapper where
    arbitrary = getArbitrary OutputTrayWrapper

newtype InputTrayWrapper = InputTrayWrapper { runInputTrayWrapper :: InputTray }
    deriving (Show, Eq)

instance Arbitrary InputTrayWrapper where
    arbitrary = getArbitrary InputTrayWrapper

newtype PaperSizeWrapper = PaperSizeWrapper { runPaperSizeWrapper :: PaperSize }
    deriving (Show, Eq)

instance Arbitrary PaperSizeWrapper where
    arbitrary = getArbitrary PaperSizeWrapper

newtype PaperTypeWrapper = PaperTypeWrapper { runPaperTypeWrapper :: PaperType }
    deriving (Show, Eq)

instance Arbitrary PaperTypeWrapper where
    arbitrary = getArbitrary PaperTypeWrapper
