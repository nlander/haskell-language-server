module Development.IDE.Types.Options
  ( IdeTesting(..)
  , ProgressReportingStyle(..)
  ) where

data ProgressReportingStyle
    = Percentage -- ^ Report using the LSP @_percentage@ field
    | Explicit   -- ^ Report using explicit 123/456 text
    | NoProgress -- ^ Do not report any percentage

newtype IdeTesting           = IdeTesting        Bool
