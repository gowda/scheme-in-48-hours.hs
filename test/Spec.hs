import qualified SchemeSpec
import qualified SchemeSpec.EvaluatorSpec
import qualified SchemeSpec.ParserSpec
import qualified SchemeSpec.TypesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  SchemeSpec.spec
  SchemeSpec.EvaluatorSpec.spec
  SchemeSpec.ParserSpec.spec
  SchemeSpec.TypesSpec.spec
