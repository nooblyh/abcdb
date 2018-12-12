{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}

module Parser.AST where

data SQL
    = S SelectAST
    | I InsertAST
    | U UpdateAST

newtype Column = Column { getColumn :: String }

class ColumnType a where

instance ColumnType Int
instance ColumnType Bool
instance ColumnType String

-- select (* | col1, col2, ..) from table1, table2 , .. where <predicate>
data SelectAST = Select SelectTarget SelectFrom (Maybe Predicate)

data SelectTarget
    = Star -- *
    | Columns [Column]
 -- | Aggregate Aggregate

data SelectFrom
    = Tables [String]
    | Subquery SelectAST

data Predicate
    = BoolPd Bool
    | AndPd (PdArg Bool) (PdArg Bool)
    | OrPd  (PdArg Bool) (PdArg Bool)
    | NotPd (PdArg Bool)
    | forall a. ColumnType a => EqPd  (PdArg a) (PdArg a)
    | forall a. ColumnType a => NEqPd (PdArg a) (PdArg a)

data PdArg a where
    ArgPred   :: Predicate -> PdArg Bool
    ArgInt    :: Int       -> PdArg Int
    ArgString :: String    -> PdArg String
    ArgColumn :: ColumnType a => Column -> PdArg a

data InsertAST

data UpdateAST
