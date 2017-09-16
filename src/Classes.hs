module Classes where

import Servant

class ToServantErr a where
    toServantErr :: a -> ServantErr
