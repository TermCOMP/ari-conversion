module TRSConversion.Problem.CSTrs.CSTrs (
    CSTrs (..),
    ReplacementMap,
) where

import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.TrsSig (TrsSig)

type ReplacementMap f = [(f, [Int])]

data CSTrs f v = CSTrs
    { rules :: [Rule f v]
    -- ^ A list of the MSTRS rewrite rules
    , signature :: TrsSig f v
    -- ^ The signature (function symbols and corresponding sorts) for the MSTRS
    , replacementMap :: ReplacementMap f
    -- ^ The replacementMap
    }
    deriving (Show, Eq)