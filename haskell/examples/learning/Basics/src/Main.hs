import Examples.Module1 (run)                           -- import only run function
import Examples.SimpleFunctions hiding (indicate1)      -- import everything except of indicate1
import Examples.HighOrderFunctions
import qualified Examples.LearnLists as L               -- using alias
import qualified Examples.SimpleClassesAndGenerics
import qualified Examples.DataTypes
import qualified Examples.DerivedTypes
import qualified Examples.Classes1
import qualified Examples.Classes2
import qualified Examples.Classes3
import qualified Examples.Exceptions1
import qualified Examples.Exceptions2
import qualified Examples.Monads.IO1
import qualified Examples.Monads.Monads1
import qualified Examples.Monads.Maybe1
import qualified Examples.Functors.Functor1
import qualified Examples.Functors.ApplicativeFunctor1
import qualified Examples.Recurssions1

main = do
--    Examples.Module1.run
--    Examples.SimpleFunctions.run
--    Examples.HighOrderFunctions.run
    L.run
--    Examples.SimpleClassesAndGenerics.run
--    Examples.DataTypes.run
--    Examples.DerivedTypes.run
--    Examples.Classes1.run
--    Examples.Classes2.run
--    Examples.Classes3.run
--    Examples.Exceptions1.run
--    Examples.Exceptions2.run
--    Examples.Monads.IO1.run
--    Examples.Monads.Monads1.run
--    Examples.Monads.Maybe1.run
--    Examples.Functors.Functor1.run
--    Examples.Functors.ApplicativeFunctor1.run
--    Examples.Recurssions1.run

