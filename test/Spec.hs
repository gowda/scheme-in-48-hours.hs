import qualified EvaluatorSpec
import qualified LibSpec
import qualified ParserSpec
import Test.Hspec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
  LibSpec.spec
  EvaluatorSpec.spec
  ParserSpec.spec
  TypesSpec.spec
