module Data.BiHoist where

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False

class (Equal x y ~ s)
  => BiHoist s m n x y
  | x m n -> y, y m n -> x where
  biHoist :: (forall a. m a -> n a) -> (forall b. n b -> m b) -> x -> y

instance (s ~ Equal (m a) (n a), s ~ 'False) => BiHoist s m n (m a) (n a) where
  biHoist fv _bk = fv

instance (s ~ Equal (n a) (m a), s ~ 'False) => BiHoist s m n (n a) (m a) where
  biHoist _fv bk = bk

instance ( BiHoist (Equal x y) m n x y, BiHoist (Equal b a) m n b a
         , s ~ 'False, s ~ Equal (a -> x) (b -> y) )
  => BiHoist 'False  m n (a -> x) (b -> y) where
  biHoist fv bk ax b = biHoist fv bk $ ax (biHoist fv bk b)

instance BiHoist 'True m n a a where
  biHoist _ _ = id

data M a

data N a

-- funcM :: Int -> M Int -> M Int
-- funcM = error "FIXME: funcM not implemented"

-- funN :: Int -> N Int -> N Int
-- funN =
--   let
--     fv :: M a -> N a
--     fv = (error "FIXME: not implemented")
--     bk :: N a -> M a
--     bk = (error "FIXME: not implemented")
--   in biHoist fv bk funcM
