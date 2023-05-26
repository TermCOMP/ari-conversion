module TRSConversion.Parse.ARI.RuleSpec where

import Data.Text (pack)
import Gen.Sig (genSig)
import Gen.Term (genTerm, genVars)
import qualified Hedgehog as H
import TRSConversion.Parse.ARI.Rule (parseAriRule)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Problem.Common.Rule (Rule (..))
import TRSConversion.Problem.Common.Term (Term, termFunArities)
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Unparse.Problem.Rule (unparseAriRule)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "properties" $
    it "roundtrip" $
      hedgehog $ do
        sig <- H.forAll genSig
        term1 <- H.forAll (genTerm sig genVars)
        term2 <- H.forAll (genTerm sig genVars)
        let rule = Rule term1 term2
        H.tripping
          rule
          (pack . show . unparseAriRule)
          (parse (ARI.toParser (parseAriRule sig)) "testinput")

sigOfTerm :: Eq f => Term f v -> [Sig f]
sigOfTerm t =
  case termFunArities t of
    Left _ -> error "should not happen"
    Right sigs -> sigs