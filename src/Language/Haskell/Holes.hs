module Language.Haskell.Holes.TH
       ( holes )
       where


------------------------------------------------------------------------------
import Control.Monad               ( replicateM, liftM2 )

import Data.Char                   ( ord )

import Language.Haskell.Meta.Parse ( parseExp )

import Language.Haskell.TH         ( Q, Exp )
import Language.Haskell.TH.Quote   ( QuasiQuoter ( QuasiQuoter,
                                                   quoteExp, quotePat ) )
import Language.Haskell.TH.Syntax  ( Exp ( LamE ), Pat ( VarP ),
                                     showName, mkName, newName,
                                     Loc ( Loc ), location )


import Text.Parsec                 ( Parsec, (<|>), runParser,
                                     many, many1, try, string,
                                     char, anyChar, oneOf, noneOf )


------------------------------------------------------------------------------
holes :: QuasiQuoter
holes = QuasiQuoter { quoteExp = holesExp,
                      quotePat = fail "Quoting patterns is unsupported"
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
          where end = liftM2 (:) (noneOf ['\\']) (string "\"")
        rest =
          liftM2 (:) (char '%') (many $ noneOf ['%', '"'])
          <|>
          (many1 $ noneOf ['%', '"'])

        manyTillIncluding p end = scan
          where scan = end <|> liftM2 (:) p scan


------------------------------------------------------------------------------
parseExpQ :: String -> Q Exp
parseExpQ = either fail return . parseExp


------------------------------------------------------------------------------
