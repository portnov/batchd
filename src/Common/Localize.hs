
module Common.Localize
  (
    translationPolicy,
    __, __f,
    __s, __sf,
    Localized (..),
    module Text.Localize.IO
  ) where

import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Text.Localize
import Text.Localize.IO

translationPolicy :: LocatePolicy
translationPolicy =
  let local = localLocation "mo" 
      global = linuxLocation "batchd"
  in  global {lcBasePaths = lcBasePaths local ++ lcBasePaths global}

__s :: Localized m => TranslationSource -> m String
__s str = TL.unpack `fmap` (__ str)

__sf :: (Localized m, VarContainer vars) => TranslationSource -> vars -> m String
__sf str vars = TL.unpack `fmap` (__f str vars)

