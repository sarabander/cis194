{-# OPTIONS_GHC -Wall #-}

-- Katsetused 11. nädala loengukonspekti põhjal

import Control.Applicative hiding ((*>))
import AParser

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = const id <$> fa <*> fb
--(*>) fa fb = (\_ -> id) <$> fa <*> fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g = sequenceA . liftA g

{-
Tarvis on funktsiooni parameetritega f a ja f [a], mis akumuleerib a-d
loendisse [a].

f a -> f [a] -> f [a]
(:) <$> fa <*> fas
-}

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr ((<*>) . (<$>) (:)) $ pure []

-- Pikem ja selgem, kuid sõnaohtram variant:
--sequenceA = foldr combine $ pure [] where
--  combine fa fas = (:) <$> fa <*> fas

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = liftA (replicate n)

-- Parserite testimiseks
parseList :: [Parser [Char]]
parseList = [st, eel]

-- Definitsioon loengust
pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)
