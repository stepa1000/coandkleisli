module Data.CoAndKleisli where

import Control.Arrow
import qualified Control.Category as Cat
import Control.Comonad
import Control.Monad

{-
data ArrCoAndKleisli arr w m a b = ArrCoAndKlesli
  {unArrCoAndKleisli :: arr (w a) (m b)}
-}

data CoAndKleisli w m a b = CoAndKleisli
  {unCoAndKleisli :: w a -> m b}

{-
instance (Arrow arr, Comonad w, Monad m) => Cat.Category (ArrCoAndKleisli arr w m) where
  id = ArrCoAndKleisli (arr return . arr extract)
  (ArrCoAndKleisli a1) . (ArrCoAndKleisli a2) = ArrCoAndKleisli ( a1 . . a2)
-}

flipWM :: (Comonad w, Monad m) => w (m a) -> m (w a)
flipWM wma = do
  a <- extract wma
  return $ fmap (\_ -> a) wma

instance (Comonad w, Monad m) => Cat.Category (CoAndKleisli w m) where
  id = CoAndKleisli (return . extract)
  (CoAndKleisli a1) . (CoAndKleisli a2) = CoAndKleisli ((>>= a1) . flipWM . extend a2)

instance (Comonad w, Monad m) => Arrow (CoAndKleisli w m) where
  arr f = CoAndKleisli (return . f . extract)
  first (CoAndKleisli a) = CoAndKleisli $ \wxy -> do
    x2 <- a (extend (fst . extract) wxy)
    return (x2, snd $ extract wxy)

instance (Comonad w, Monad m) => ArrowChoice (CoAndKleisli w m) where
  left (CoAndKleisli a) = CoAndKleisli $ \we -> case extract we of
    Left x -> fmap Left $ a $ fmap (const x) we
    Right x -> return $ Right x

instance (Comonad w, Monad m) => ArrowApply (CoAndKleisli w m) where
  app = CoAndKleisli $ \ wfb -> (\(CoAndKleisli wf, b)-> wf $ fmap (const b) wfb) $ extract wfb