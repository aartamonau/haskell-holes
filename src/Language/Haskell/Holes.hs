{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
module Language.Haskell.Holes
       ( holes )
       where


------------------------------------------------------------------------------
import Control.Monad               ( replicateM, liftM2 )

import Data.Char                   ( ord )

import Language.Haskell.Meta.Parse ( parseExp )

import Language.Haskell.TH         ( Q, Exp )
import Language.Haskell.TH.Quote   ( QuasiQuoter ( QuasiQuoter,
                                                   quoteExp, quotePat
#ifdef GHC7
                                                   , quoteType, quoteDec
#endif
                                                 ) )
import Language.Haskell.TH.Syntax  ( Exp ( LamE ), Pat ( VarP ),
                                     showName, mkName, newName,
                                     Loc ( Loc ), location )


import Text.Parsec                 ( Parsec, (<|>), runParser,
                                     many, many1, try, string,
                                     char, anyChar, oneOf, noneOf )


------------------------------------------------------------------------------
-- | Quasiquoter to define anonymous functions that can reference function
-- parameters by their number.
--
-- Example:
--
-- @
-- :t [$holes| %1 + %2 |]
-- [$holes| %1 + %2 |] :: (Num a) => a -> a -> a
-- @
--
-- * Only numbers 1 through 9 can be used to reference function parameters.
--
-- * Number of parameters that generated function takes is determined by the
--   maximum parameter number that is referenced in the function body.
--
-- * If '%' is used insided function body it should be surrounded by spaces to
--   avoid substitution (in case it's followed by some number).
holes :: QuasiQuoter
holes = QuasiQuoter { quoteExp  = holesExp
                    , quotePat  = fail "Patterns quotations are unsupported"
#ifdef GHC7
                    , quoteType = fail "Type quotations are unsupported"
                    , quoteDec  = fail "Declaration quotations are unsupported"
#endif
                    }


------------------------------------------------------------------------------
type Hole = Int


------------------------------------------------------------------------------
holesExp :: String -> Q Exp
holesExp s = do
  parsed <- parse pHoles s
  let n = maximum [ h | Right h <- parsed ]

  vars <- replicateM n holeName

  -- no patterns quasiquotations in my ghc
  let varsP    = map VarP vars
  let bodyStr  = map (uneither . subst vars) parsed
  bodyE <- parseExpQ $ foldr (++) "" bodyStr

  return $ LamE varsP bodyE

  where subst _    (Left s)  = Left s
        subst vars (Right n) = Right $ varName n
          where varName n = ' ' : showName (vars !! (n - 1)) ++ " "

        uneither (Left x)  = x
        uneither (Right x) = x

        -- hacky since parseExpQ expects NameS
        holeName = fmap (mkName . showName) $ newName "hole"

        parse p s = do
          Loc file _ _ start end <- location
          let fileInfo = (file ++ " (holes) " ++
                          show start ++ " -- " ++ show end)
          case runParser p () fileInfo s of
            Left err -> fail (show err)
            Right r  -> return r



------------------------------------------------------------------------------
pHoles :: Parsec String () [Either String Hole]
pHoles = many $
  fmap Right (try hole) <|> fmap Left quoted <|> fmap Left rest
  where hole = do
          char '%'
          d <- oneOf ['1' .. '9']
          return $ ord d - ord '0'
        quoted =
          liftM2 (:) (char '"') (manyTillIncluding anyChar (try end))
          where end = liftM2 (:) (noneOf "\\") (string "\"")
        rest =
          liftM2 (:) (char '%') (many $ noneOf "%\"")
          <|>
          many1 (noneOf "%\"")

        manyTillIncluding p end = scan
          where scan = end <|> liftM2 (:) p scan


------------------------------------------------------------------------------
parseExpQ :: String -> Q Exp
parseExpQ = either fail return . parseExp


------------------------------------------------------------------------------
