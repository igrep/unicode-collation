{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Text.Collate.TH
  ( genCollation
  , genCJKOverrides
  )
where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, qAddDependentFile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary as Binary ( encode )
import Text.Collate.Collation (parseCollation, parseCJKOverrides)
import Data.Text.Encoding (decodeUtf8)

genCollation :: FilePath -> Q Exp
genCollation fp = do
  qAddDependentFile fp
  lift =<< parseCollation . decodeUtf8 <$> runIO (B.readFile fp)

genCJKOverrides :: FilePath -> Q Exp
genCJKOverrides fp = do
  qAddDependentFile fp
  lift =<< parseCJKOverrides . decodeUtf8 <$> runIO (B.readFile fp)
