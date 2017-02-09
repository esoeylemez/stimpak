-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Stimpak.Internal.Core
    ( -- * Events
      RootEvent(..),
      Event(..),
      EventAction(..),
      derivedEvent,
      newEvent,
      readEvent,
      trigger
    )
    where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Codensity
import Data.Align
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Identity
import Data.Functor.Plus
import System.IO.Unsafe


data Event a =
    Event {
      _evAction :: Codensity STM (Maybe a),
      _evVar    :: TVar (Maybe (Maybe a))
    }

instance Align Event where
    alignWith f ev1 ev2 =
        derivedEvent $
            alignWith f <$> readEvent ev1 <*> readEvent ev2

    nil = derivedEvent (pure Nothing)

instance Alt Event where
    ev1 <!> ev2 =
        derivedEvent $
            liftA2 (<|>) (readEvent ev1) (readEvent ev2)

instance Apply Event where
    evf <.> evx =
        derivedEvent $
            liftA2 (<*>) (readEvent evf) (readEvent evx)

instance Bind Event where
    join ev =
        derivedEvent $
            readEvent ev >>=
            maybe (pure Nothing) readEvent

instance Functor Event where
    fmap f ev = derivedEvent (fmap f <$> readEvent ev)

instance Plus Event where
    zero = nil


newtype EventAction a =
    EventAction {
      runEventAction :: Codensity STM a
    }
    deriving (Applicative, Functor, Monad)


newtype RootEvent a =
    RootEvent {
      _reEvent :: Event a
    }


derivedEvent :: EventAction (Maybe a) -> Event a
derivedEvent (EventAction _evAction) = Event{..}
    where
    _evVar = unsafePerformIO (newTVarIO Nothing)
    {-# NOINLINE _evVar #-}


newEvent :: IO (RootEvent a, Event a)
newEvent = do
    _evVar <- newTVarIO (Just Nothing)
    let _evAction =
            error ("Trying to update root event. Unless you " ++
                   "abused the internal API this is a bug in Stimpak.")
        ev = Event{..}
    pure (RootEvent ev, ev)


readEvent :: Event a -> EventAction (Maybe a)
readEvent Event{..} =
    EventAction $ Codensity $ \k ->
        readTVar _evVar >>=
        maybe (runCodensity _evAction $ \mx -> do
                   writeTVar _evVar (Just mx)
                   k mx <* writeTVar _evVar Nothing)
              k


trigger :: [DSum RootEvent Identity] -> EventAction a -> IO a
trigger evs = atomically . triggerSTM evs


triggerSTM :: [DSum RootEvent Identity] -> EventAction a -> STM a
triggerSTM evs (EventAction c) =
    lowerCodensity $ do
        for_ evs $ \(RootEvent Event{..} :=> Identity x) ->
            Codensity $ \k -> do
                writeTVar _evVar (Just (Just x))
                k () <* writeTVar _evVar (Just Nothing)
        c
