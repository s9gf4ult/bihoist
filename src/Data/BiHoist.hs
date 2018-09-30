module Data.BiHoist where

type family Special m n x y :: Bool where
  Special m n (m a) (n a) = 'True
  Special m n (n a) (m a) = 'True
  Special m n (a -> x) (b -> y) = 'True
  Special m n a a = 'False

type family Same a b :: Bool where
  Same a a = 'True
  Same a b = 'False

class (Special m n x y ~ s)
  => BiHoist s m n x y
  | x m n -> y, y m n -> x where
  biHoist :: (forall a. m a -> n a) -> (forall b. n b -> m b) -> x -> y

instance BiHoist 'True m n (m a) (n a) where
  biHoist fv _bk = fv

instance BiHoist 'True m n (n a) (m a) where
  biHoist _fv bk = bk

instance (BiHoist (Special m n x y) m n x y, BiHoist (Special m n b a) m n b a)
  => BiHoist 'True m n (a -> x) (b -> y) where
  biHoist fv bk ax b = biHoist fv bk $ ax (biHoist fv bk b)

instance BiHoist 'False m n a a where
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
