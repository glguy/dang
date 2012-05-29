{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.QualName (
    ident
  , conident
  , symbol
  , namespace
  ) where

import QualName

import Control.Applicative ((<$>),(<*>))
import Test.QuickCheck

-- | Constructor identifiers.
conident :: Gen Name
conident  = (:) <$> upper <*> body

-- | Identifiers.
ident :: Gen Name
ident  = (:) <$> lower <*> body

-- | Symbols.
symbol :: Gen Name
symbol  = listOf1 (elements "-><|!@#$%^&*")

-- | Add a name-space to a name, qualifying it.
namespace :: (Namespace -> Name -> QualName) -> Gen Name -> Gen QualName
namespace k name = k <$> resize 4 (listOf conident) <*> name

upper :: Gen Char
upper  = choose ('A','Z')

lower :: Gen Char
lower  = choose ('a','z')

number :: Gen Char
number  = choose ('0','9')

body :: Gen Name
body  = listOf $ oneof
  [ lower
  , upper
  , number
  , elements "_'?!"
  ]

instance Arbitrary QualName where
  arbitrary = oneof
    [ simpleName <$> name
    , namespace primName name
    , namespace qualName name
    ]
    where
    name = oneof [conident,ident]
