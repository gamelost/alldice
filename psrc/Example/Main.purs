module Example.Main where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Eff.Random

foreign import data Alert :: !

foreign import alert
	"function alert(s) { \
	\  return function() { \
	\    window.alert(s); \
	\  }; \
	\};" :: forall r. String -> Eff (alert :: Alert | r) Unit

main = do
	rand <- random
--	alert "Wooo!"
	trace $ "hello purescript: " ++ show rand
