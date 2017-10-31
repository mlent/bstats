import           Test.Tasty

-- import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
-- import qualified Test.Tasty.SmallCheck as SC
import qualified Spec

suites = [Spec.suite]

suite :: TestTree
suite = testGroup "bstats" suites

main :: IO ()
main = defaultMain suite
