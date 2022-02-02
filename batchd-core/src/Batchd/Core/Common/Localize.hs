-- | This module contains utilities for localization.
-- This also re-exports commonly used modules from @localize@ package.
module Batchd.Core.Common.Localize
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

-- | Standard batchd translation files location policy:
-- first check @"mo"@ subdirectory in current directory,
-- then check standard linux locations.
translationPolicy :: LocatePolicy
translationPolicy =
  let local = localLocation "mo" 
      global = linuxLocation "batchd"
  in  global {lcBasePaths = lcBasePaths local ++ lcBasePaths global}

-- | Variant of @__@, returning String.
__s :: (Localized m, MonadFail m) => TranslationSource -> m String
__s str = TL.unpack `fmap` (__ str)

-- | Variant of @__f@, returning String.
__sf :: (Localized m, MonadFail m, VarContainer vars) => TranslationSource -> vars -> m String
__sf str vars = TL.unpack `fmap` (__f str vars)

