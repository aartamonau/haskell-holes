{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Control.Monad (forM_)
import Control.Arrow ((>>>), arr)
-- import Text.Pandoc

main :: IO ()
main = hakyll $ do
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    forM_ ["images/*"] $ \f -> do
        route   f idRoute
        compile f copyFileCompiler

    -- Pages
    forM_ pages $ \p -> do
        route   p $ setExtension "html"
        compile p $ readPageCompiler
            >>> arr (setField "projectUrl" projectUrl)
            >>> pageRenderPandoc
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Templates
    compile "templates/*" templateCompiler
  where
    pages = [ "index.markdown"
            ]

    projectUrl = "http://github.com/shaleny/haskell-holes"
