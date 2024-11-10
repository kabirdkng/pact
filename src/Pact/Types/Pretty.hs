{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Types.Pretty
  ( (<+>)
  , Annot(..)
  , Doc
  , Pretty(..)
  , RenderColor(..)
  , SomeDoc(..)
  , SpecialPretty(..)
  , abbrev
  , abbrevStr
  , align
  , angles
  , annotate
  , braces
  , bracesSep
  , brackets
  , bracketsSep
  , colon
  , commaBraces
  , commaBraces'
  , commaBrackets
  , dot
  , dquotes
  , encloseSep
  , equals
  , fillSep
  , hardline
  , hsep
  , indent
  , line
  , list
  , nest
  , parens
  , parensSep
  , prettyString
  , punctuate
  , putDoc
  , renderCompactString
  , renderCompactString'
  , renderCompactText
  , renderCompactText'
  , renderPrettyString
  , renderPrettyString'
  , renderString'
  , sep
  , showPretty
  , space
  , unAnnotate
  , viaShow
  , vsep
  ) where

import           Control.DeepSeq      (NFData(..))
import           GHC.Generics         (Generic(..))
import           Bound.Var
import           Data.Aeson           (Value(..))
import           Data.Foldable        (toList)
import           Data.Int
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Internal as PP
import qualified Data.Text.Prettyprint.Doc.Render.String as PP
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Term
import           Data.Text.Prettyprint.Doc.Render.Text as RText
import           Text.Trifecta.Delta hiding (prettyDelta)

import qualified Pact.JSON.Legacy.HashMap as LH
import qualified Pact.JSON.Legacy.Utils as JL

-- | Output rendering mode
data RenderColor = RColor | RPlain
  deriving (Eq, Show)

-- | Syntax element annotations
data Annot
  = Keyword     -- ^ Pact keywords (module, defun, etc)
  | Function    -- ^ Function names and calls
  | StringLit   -- ^ String literals
  | NumberLit   -- ^ Numeric literals
  | Comment     -- ^ Comments
  | Delimiter   -- ^ (), [], {}, etc
  | Operator    -- ^ Mathematical and logical operators 
  | TypeAnnot   -- ^ Type annotations
  | ModuleName  -- ^ Module names
  | Variable    -- ^ Variable names
  | Warning     -- ^ Warning messages
  | Error       -- ^ Error messages  
  | Success     -- ^ Success messages
  | Header      -- ^ Headers and section titles
  | Special     -- ^ Special forms and syntax
  | Default     -- ^ Default text color
  deriving (Generic, Eq, Show)

instance NFData Annot

type Doc = PP.Doc Annot

instance NFData (PP.Doc Annot)

instance Eq Doc where
  d1 == d2 = show d1 == show d2
  d1 /= d2 = show d1 /= show d2

colorFun :: Annot -> AnsiStyle
colorFun = \case
  Keyword    -> color Magenta <> bold           -- Keywords in bold magenta
  Function   -> color Cyan                      -- Functions in cyan
  StringLit  -> color Green                     -- Strings in green
  NumberLit  -> color Blue                      -- Numbers in blue  
  Comment    -> color White <> dim              -- Comments dimmed
  Delimiter  -> color Yellow                    -- Delimiters in yellow
  Operator   -> color White <> bold             -- Operators in bold white
  TypeAnnot  -> color Magenta <> dim            -- Types in dim magenta
  ModuleName -> color Blue <> bold              -- Modules in bold blue
  Variable   -> color Cyan <> dim               -- Variables in dim cyan
  Warning    -> color Yellow <> bold            -- Warnings in bold yellow
  Error      -> color Red <> bold               -- Errors in bold red
  Success    -> color Green <> bold             -- Success in bold green
  Header     -> color Green <> bold             -- Headers in bold green
  Special    -> color Red                       -- Special forms in red
  Default    -> mempty                          -- Default terminal color

-- | Pact's version of 'Pretty'
class Pretty a where
  pretty :: a -> Doc
  prettyList :: [a] -> Doc
  prettyList = list . map pretty

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty Char where
  pretty = PP.pretty
  prettyList = prettyString

instance Pretty Text where
  pretty = PP.pretty

instance Pretty Bool where
  pretty = PP.pretty

instance Pretty Integer where
  pretty = PP.pretty

instance Pretty Int where
  pretty = PP.pretty

instance Pretty Int64 where
  pretty = viaShow

instance Pretty () where
  pretty = PP.pretty

instance Pretty a => Pretty (Maybe a) where
  pretty = maybe mempty pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = tupled [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]

prettyString :: String -> Doc
prettyString = PP.pretty . pack

-- | Common formatting helpers
commaBraces, commaBrackets :: [Doc] -> Doc
commaBraces = encloseSep "{" "}" ","
commaBrackets = encloseSep "[" "]" ","

bracketsSep, parensSep, bracesSep :: [Doc] -> Doc
bracketsSep = brackets . sep
parensSep = parens . sep
bracesSep = braces . sep

commaBraces' :: (Foldable t, Pretty a) => t a -> Doc
commaBraces' = commaBraces . map pretty . toList

renderString' :: (Doc -> SimpleDocStream Annot) -> RenderColor -> Doc -> String
renderString' renderf colors doc = case colors of
  RColor -> unpack $ Term.renderStrict $ reAnnotateS colorFun $ renderf doc
  RPlain -> PP.renderString $ renderf $ unAnnotate doc

showPretty :: Pretty a => a -> String
showPretty = renderCompactString

-- | Various rendering functions for different formats
renderCompactString :: Pretty a => a -> String
renderCompactString = renderString' layoutReallyCompact RPlain . pretty

renderCompactString' :: Doc -> String
renderCompactString' = renderString' layoutReallyCompact RPlain

renderCompactText :: Pretty a => a -> Text
renderCompactText = RText.renderStrict . layoutReallyCompact . reAnnotate colorFun . pretty

renderCompactText' :: PP.Doc Annot -> Text
renderCompactText' = Term.renderStrict . layoutReallyCompact . reAnnotate colorFun

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString rc = renderString' (layoutPretty defaultLayoutOptions) rc . pretty

renderPrettyString' :: RenderColor -> Doc -> String
renderPrettyString' = renderString' $ layoutPretty defaultLayoutOptions

-- | The most compact possible layout
layoutReallyCompact :: PP.Doc ann -> SimpleDocStream ann
layoutReallyCompact doc = scan 0 [doc]
  where
    scan !col [] = PP.SEmpty
    scan !col (d:ds) = case d of
        PP.Fail            -> PP.SFail
        PP.Empty           -> scan col ds
        PP.Char c          -> PP.SChar c (scan (col+1) ds)
        PP.Text l t        -> PP.SText l t (scan (col+l) ds)
        PP.FlatAlt _ y     -> scan col (y:ds)
        PP.Line            -> PP.SLine 0 (scan 0 ds)
        PP.Cat x y         -> scan col (x:y:ds)
        PP.Nest _ x        -> scan col (x:ds)
        PP.Union _ y       -> scan col (y:ds)
        PP.Column f        -> scan col (f col:ds)
        PP.WithPageWidth f -> scan col (f PP.Unbounded:ds)
        PP.Nesting f       -> scan col (f 0:ds)
        PP.Annotated _ x   -> scan col (x:ds)

-- | Helper for abbreviated output
abbrev :: Pretty a => a -> Text
abbrev a = case Text.compareLength fullText 50 of
  GT -> Text.take 50 fullText <> "..."
  _  -> fullText
  where fullText = renderCompactText a

abbrevStr :: Pretty a => a -> String
abbrevStr = unpack . abbrev

-- | Helper to use pretty on functors
newtype SomeDoc = SomeDoc Doc

instance Pretty SomeDoc where
  pretty (SomeDoc doc) = doc

-- | Special pretty-printing for certain types
data SpecialPretty n
  = SPSpecial !Text
  | SPNormal !n

instance Pretty n => Pretty (SpecialPretty n) where
  pretty (SPSpecial t) = pretty t
  pretty (SPNormal n) = pretty n

-- | Pretty instances for common types
instance Pretty Value where
  pretty = \case
    Object hm -> commaBraces
      $ map (\(k, v) -> dquotes (pretty k) <> ": " <> pretty v)
      $ LH.toList (JL.legacyKeyMap hm)
    Array vs -> bracketsSep $ map pretty $ toList vs
    String s -> dquotes $ pretty s
    Number n -> viaShow n
    Bool b -> pretty b
    Null -> "null"

instance Pretty Delta where
  pretty d = case d of
    Columns c _ -> prettyDelta interactive 0 c
    Tab x y _ -> prettyDelta interactive 0 (nextTab x + y)
    Lines l c _ _ -> prettyDelta interactive l c
    Directed fn l c _ _ -> prettyDelta (unpack $ Text.decodeUtf8 fn) l c
    where
      prettyDelta source line' column' =
        prettyString source <> ":" <> pretty (line'+1) <> ":" <> pretty (column'+1)
      interactive = "(interactive)"

instance (Pretty a, Pretty b) => Pretty (Var a b) where
  pretty = \case
    B b -> pretty b
    F a -> pretty a
