module Example.Main where

import Debug.Trace
import Control.Monad.Eff

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
    trace "Setup stuff"
    -- Doc page stuff
    scrollspy
    affix

    -- Ajax request (attaching to a button)
