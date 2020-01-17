import qualified SimpleLensSpec                as SLS
import qualified MicroLensSpec                 as MLS
import           Test.Hspec

spec = do
  describe "MicroLensSpec"  MLS.spec
  describe "SimpleLensSpec" SLS.spec

main = hspec spec
