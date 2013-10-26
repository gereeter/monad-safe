module Control.Monad.Safe (
      MonadSafe(..)
    
    , finally
    , bracket
    , bracket_
) where

import qualified Control.Exception (onException)
import Data.Monoid (Monoid)

import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Lazy as SL
import Control.Monad.Trans.State.Strict as SS
import Control.Monad.Trans.Writer.Lazy as WL
import Control.Monad.Trans.Writer.Strict as WS
import Control.Monad.Trans.RWS.Lazy as RWSL
import Control.Monad.Trans.RWS.Strict as RWSS
import Control.Monad.Trans.Reader

class Monad m => MonadSafe m where
    onException :: m a -> m b -> m a

finally :: MonadSafe m => m a -> m b -> m a
act `finally` finish = do
    ret <- act `onException` finish
    _ <- finish
    return ret

bracket :: MonadSafe m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket alloc close act = alloc >>= \x -> act x `finally` close x

bracket_ :: MonadSafe m => m a -> m b -> m c -> m c
bracket_ alloc close act = alloc >> (act `finally` close)

instance MonadSafe Identity where
    onException = const

instance MonadSafe Maybe where
    onException = const

instance MonadSafe (Either e) where
    Left e `onException` act = act >> Left e
    act `onException` _ = act

instance MonadSafe IO where
    onException = Control.Exception.onException

instance MonadSafe m => MonadSafe (ReaderT e m) where
    ReaderT act `onException` ReaderT handle = ReaderT $ \e -> act e `onException` handle e

instance MonadSafe m => MonadSafe (SL.StateT s m) where
    SL.StateT act `onException` SL.StateT handle = SL.StateT $ \s -> act s `onException` handle s

instance MonadSafe m => MonadSafe (SS.StateT s m) where
    SS.StateT act `onException` SS.StateT handle = SS.StateT $ \s -> act s `onException` handle s

instance (MonadSafe m, Monoid w) => MonadSafe (WL.WriterT w m) where
    WL.WriterT act `onException` WL.WriterT handle = WL.WriterT (act `onException` handle)

instance (MonadSafe m, Monoid w) => MonadSafe (WS.WriterT w m) where
    WS.WriterT act `onException` WS.WriterT handle = WS.WriterT (act `onException` handle)

instance (MonadSafe m, Monoid w) => MonadSafe (RWSL.RWST r w s m) where
    RWSL.RWST act `onException` RWSL.RWST handle = RWSL.RWST $ \r s -> act r s `onException` handle r s

instance (MonadSafe m, Monoid w) => MonadSafe (RWSS.RWST r w s m) where
    RWSS.RWST act `onException` RWSS.RWST handle = RWSS.RWST $ \r s -> act r s `onException` handle r s

instance (MonadSafe m, Error e) => MonadSafe (ErrorT e m) where
    ErrorT act `onException` ErrorT handle = ErrorT $ do
        result <- act `onException` handle
        case result of
            Right x -> return (Right x)
            Left e -> do
                hResult <- handle
                case hResult of
                    Left e' -> return (Left e')
                    _       -> return (Left e)

instance MonadSafe m => MonadSafe (MaybeT m) where
    MaybeT act `onException` MaybeT handle = MaybeT $ do
        result <- act `onException` handle
        case result of
            Nothing -> handle >> return result
            _       -> return result
