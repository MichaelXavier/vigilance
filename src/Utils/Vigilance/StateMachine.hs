module Utils.Vigilance.StateMachine ( StateMachine(..)
                                    , StateMachineM
                                    , runStateMachine
                                    , transition) where
import Control.Applicative ( (<$>)
                           , (<*))
import Data.Map (Map)
import Data.Map as M
import Control.Monad.Trans (lift)
import Control.Monad.Reader
import Control.Monad.State

data StateMachine s m a = StateMachine { transitions :: Map (s,s) (Maybe (m a)) }

type StateMachineM s m a = ReaderT (StateMachine s m a) (StateT s m) a
type StateMachineM' s m a = ReaderT (StateMachine s m a) (StateT s m) (Maybe a)

runStateMachine :: Monad m => s -> StateMachine s m a -> ReaderT (StateMachine s m a) (StateT s m) b -> m (b, s) --todo: update StateMachineM type params
runStateMachine currentState sm action = runStateT (runReaderT action sm) currentState

transition :: (MonadPlus m, Functor m, Ord s) => s -> StateMachineM' s m a
transition newState = do oldState <- get
                         ts       <- asks transitions
                         case M.lookup (oldState, newState) ts of
                           Just (Just trans) -> runTrans trans <* setState
                           Just Nothing      -> return Nothing <* setState
                           Nothing           -> mzero -- monadbasecontrol evindetly helps this from being a nightmare?
  where runTrans = lift . lift . fmap Just
        setState = put newState
