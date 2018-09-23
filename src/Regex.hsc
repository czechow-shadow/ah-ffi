{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Regex where

import Protolude

--import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
                   deriving (Eq, Show)

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt }
                 deriving (Eq, Show)


newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
                       deriving (Eq, Show)

#{ enum PCREOption, PCREOption
 , caseless = PCRE_CASELESS
 , dollar_endonly = PCRE_DOLLAR_ENDONLY
 , dotall = PCRE_DOTALL
 }

#{ enum PCREInfo, PCREInfo
 , info_capturecount = PCRE_INFO_CAPTURECOUNT
 }

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0
