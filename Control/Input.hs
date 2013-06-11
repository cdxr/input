{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Control.Input where


import Control.Monad.Operational
import Control.Monad.IO.Class


data InputI o i a where
    ShowPrompt  :: InputI o i o
    LocalPrompt :: (o -> o) -> InputI o i a -> InputI o i a
    ReadInput   :: InputI o i i
    WriteOutput :: o -> InputI o i ()


type InputT o i = ProgramT (InputI o i)

type MonadInput o i m = (Operational (InputI o i) m, Monad m)


interpretInput
    :: (Functor m, Monad m)
    => (o -> m ())     -- | output
    -> m i             -- | input
    -> InputT o i m a  -- | computation
    -> o               -- | prompt
    -> m a
interpretInput w r t s = interpretM (eval w r s) t
  where
    eval :: (Monad m)
         => (p -> m ())
         -> m i
         -> p
         -> InputI p i a -> m a
    eval w r p i = case i of
        ShowPrompt      -> return p
        LocalPrompt f t -> eval w r (f p) t
        ReadInput       -> r
        WriteOutput s   -> w s


type TerminalT = InputT String String

runTerminalT :: (Functor m, MonadIO m) => TerminalT m a -> String -> m a
runTerminalT = interpretInput (liftIO . putStr) (liftIO getLine)


showPrompt :: (MonadInput o i m) => m o
showPrompt = singleton ShowPrompt

output :: (MonadInput o i m) => o -> m ()
output = singleton . WriteOutput

input :: (MonadInput o i m) => m i
input = singleton ReadInput

prompt :: (MonadInput o i m) => m i
prompt = showPrompt >>= output >> input

outputLine :: (MonadInput String i m) => String -> m ()
outputLine s = output s >> output "\n"


test :: IO ()
test = runTerminalT p "> "
  where
    p = prompt >>= outputLine
