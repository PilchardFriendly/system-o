{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module SystemO.Arbitraries (Noop (..), unNoop, unFill, unReducer) where

import Control.Lens
import Data.HashSet.InsOrd as HS
import Data.OpenApi as O
import Data.OpenApi.Lens ()
import Data.Text
import GHC.Base (coerce)
import Test.QuickCheck
import qualified Test.QuickCheck as Gen
import Test.QuickCheck.Instances.Text ()

-- A collection of new types and arbitraries.  Since the openapi library does not export arbitraries, 
-- and we avoid orphan instances, we are creating some NewTypes....

-- Tagged - helpful for diagnosing the case that failed
-- Good - lifting the arbitraries on to this avoid the orphan instances
-- Filled - A sizeable set of data that does something useful
-- Noop - An operation that should not change anything material to the subtyping relationship
-- Reducer - An operation that can materially change the subtyping relationship
--
newtype Tagged a = Tagged (String, a)
  deriving (Functor)

instance Show a => Show (Tagged a) where
  show t = "Tagged(" ++ taggedWith t ++ ", " ++ show (untag t) ++ ")"

tagged :: String -> a -> Tagged a
tagged t a = Tagged (t, a)

untag :: Tagged a -> a
untag (Tagged (_, a)) = a

taggedWith :: Tagged a -> String
taggedWith (Tagged (a,_)) = a

showTagged :: Tagged (a->b) -> String
showTagged t = show $ fmap (const ("fn" :: [Char])) t

newtype Good a = Good a
  deriving (Show)
  deriving (Functor)

instance Arbitrary (Good Tag) where
  arbitrary = Good <$> tag
    where
      tag =
        Tag
          <$> arbitrary
          <*> arbitrary
          <*> liftArbitrary docs
      docs :: Gen ExternalDocs
      docs = ExternalDocs <$> arbitrary <*> (unGood <$> arbitrary)

instance Arbitrary (Good (InsOrdHashSet Tag)) where
  arbitrary = do
    keys <- listOf (unGood <$> arbitrary)
    pure $ coerce $ Prelude.foldr HS.insert HS.empty keys

unGood :: Good a -> a
unGood = coerce

instance Arbitrary (Good Contact) where
  arbitrary = Good <$> go
    where
      go = go' 
        <$> arbitrary 
        <*> (unGood <$> arbitrary) 
        <*> arbitrary
      go' :: Text -> URL -> Text -> Contact
      go' n u e =
        mempty
            & (name ?~ n)
            & (url ?~ coerce u)
            & (email ?~ e)

instance Arbitrary (Good URL) where
  arbitrary = do
    domain <- Gen.elements ["com", "net", "org", "gov", "com.au"]
    pure . Good . URL $ "http://something." <> domain

newtype Noop = Noop (Tagged (OpenApi -> OpenApi))

instance Arbitrary Noop where
  arbitrary =
    Noop
      <$> oneof
        [ pure $ tagged "id" id,
          lensGen "info.description" $ set (info . description) . Just,
          lensGen "info.title" $ set $ info . title,
          lensGen "info.contact" $ set (info . contact) . (Just . unGood),
          lensGen "info.tags" $ set tags . unGood
        ]
    where
      lensGen :: (Show a, Arbitrary a) => String -> (a -> OpenApi -> OpenApi) -> Gen (Tagged (OpenApi -> OpenApi))
      lensGen tag l = do
        v <- arbitrary
        pure $ l <$> tagged (tag ++ show v) v

instance Show Noop where
  show (Noop (Tagged (s, _))) = "Noop (" ++ s ++ ")"

unNoop :: Noop -> OpenApi -> OpenApi
unNoop (Noop (Tagged (_, f))) = f

newtype Filled a = Filled a
    deriving Show
    deriving Functor

instance Arbitrary (Filled OpenApi) where
  arbitrary = undefined

unFill :: Filled a -> a
unFill = coerce

newtype Reducer = Reducer (Tagged (OpenApi -> OpenApi))
instance Show Reducer where
  show (Reducer t) = "Reducer(" ++ showTagged t ++ ")"

unReducer :: Reducer -> OpenApi -> OpenApi
unReducer = untag . coerce

instance Arbitrary Reducer where
  arbitrary = undefined





