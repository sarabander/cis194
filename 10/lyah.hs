-- From LYAH

import Data.Char
import Data.List
import Control.Applicative

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
