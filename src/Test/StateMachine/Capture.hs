{-# LANGUAGE ScopedTypeVariables #-}

module Test.StateMachine.Capture (module Test.StateMachine.Capture) where

import Prelude
import Control.Monad.Catch
import GHC.IO.Handle
import UnliftIO (MonadIO(..))
import System.IO

-- | Run an IO action while preventing and capturing all output to the given handles.
-- This will, as a side effect, create and delete a temp file in the temp directory
-- or current directory if there is no temp directory.
hCapture ::
  (MonadIO m, MonadMask m)
  => [Handle]
  -> Handle
  -> m a
  -> m a
hCapture handles tmpHandle action = go handles
  where
    go [] = do
      a <- action
      mapM_ (liftIO . hFlush) handles
      return a
    go (h:hs) = goBracket go tmpHandle h hs

goBracket ::
  (MonadIO m, MonadMask m)
  => ([Handle] -> m a)
  -> Handle
  -> Handle
  -> [Handle]
  -> m a
goBracket go tmpHandle h hs = do
  buffering <- liftIO $ hGetBuffering h
  let redirect = do
        old <- liftIO $ do
          h' <- hDuplicate h
          hDuplicateTo tmpHandle h
          pure h'
        return old
      restore old _ = liftIO $ do
        hDuplicateTo old h
        hSetBuffering h buffering
        hClose old
  fst <$> generalBracket redirect restore (\_ -> go hs)

capture :: (MonadMask m, MonadIO m) => Handle -> m a -> m a
capture h action = do
  mapM_ (liftIO . hFlush) [stdout, stderr]
  res <- hCapture [stderr, stdout] h action
  mapM_ (liftIO . hFlush) [stdout, stderr]
  pure res

newtype StdOutput = StdOutput { getStdOutput :: String }
