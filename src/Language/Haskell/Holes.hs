module Language.Haskell.Holes.TH
       ( holes )
       where


------------------------------------------------------------------------------
import Control.Monad               ( replicateM )

import Data.Char                   ( isDigit, ord )

import Language.Haskell.Meta.Parse ( parseExp )

import Language.Haskell.TH         ( Q, Exp, Pat, runIO )
import Language.Haskell.TH.Quote   ( QuasiQuoter ( QuasiQuoter,
                                                   quoteExp, quotePat ) )
import Language.Haskell.TH.Syntax  ( Exp ( LamE ), Pat ( VarP ),
                                     Name, showName, mkName, newName )


------------------------------------------------------------------------------
holes = QuasiQuoter { quoteExp = holesExp,
                      quotePat = fail "Quoting patterns is unsupported"
                    }


------------------------------------------------------------------------------
type Hole = Int


------------------------------------------------------------------------------
holesExp :: String -> Q Exp
holesExp s = do
  vars <- replicateM n holeName

  -- no patterns quasiquotations in my ghc
  let varsP    = map VarP vars
  let bodyStr  = map (uneither . subst vars) parsed
  bodyE <- parseExpQ $ foldr (++) "" bodyStr

  return $ LamE varsP bodyE

  where parsed = parseHoles s
        n      = maximum [ h | Right h <- parsed ] + 1

        subst :: [Name] -> Either String Hole -> Either String String
        subst vars (Left s)  = Left s
        subst vars (Right n) = Right $ showName (vars !! n)

        uneither (Left x)  = x
        uneither (Right x) = x

        -- hacky since parseExpQ expects NameS
        holeName = fmap (mkName . showName) $ newName "hole"


------------------------------------------------------------------------------
parseHole :: String -> Maybe (Hole, String)
parseHole ('%' : c : cs)
  | isDigit c && c /= '0' = Just (c2int c, cs)
  | otherwise             = Nothing
  where c2int c = ord c - ord '1'
parseHole _               = Nothing


------------------------------------------------------------------------------
parseHoles :: String -> [Either String Hole]
parseHoles s = go s "" []
  where go "" pre r = reverse (maybePre pre r)
        go s@(c : cs) pre r =
          case parseHole s of
            Nothing      -> go cs (c : pre) r
            Just (h, rs) -> go rs "" (Right h : (maybePre pre r))

        maybePre ""  r = r
        maybePre pre r = Left (reverse pre) : r


------------------------------------------------------------------------------
parseExpQ :: String -> Q Exp
parseExpQ = either fail return . parseExp


------------------------------------------------------------------------------
