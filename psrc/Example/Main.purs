module Example.Main where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Random

foreign import data ScrollSpy :: !

foreign import scrollspy
    "$('body').scrollspy({ \
    \   target: '.bs-docs-sidebar', \
    \       offset: 40 \
    \});" :: forall r. Eff (scrollspy :: ScrollSpy | r) Unit

foreign import data Affix :: !

foreign import affix
    "$('#sidebar').affix({ \
    \    offset: { \
    \        top: 60 \
    \    } \
    \});" :: forall r. Eff (affix :: Affix | r) Unit

main = do
    rand <- random
    scrollspy
    affix
    trace $ "hello purescript: " ++ show rand
