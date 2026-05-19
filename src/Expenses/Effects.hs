module Expenses.Effects where

import Control.Monad.Except (liftEither)
import CustomPrelude hiding (Reader, runReader)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.Reader.Static
import Expenses.Server.AppM (Env)
import Servant.Server (Handler, ServerError)

type AppM = Eff '[Reader Env, Concurrent, Error ServerError, IOE]

naturalTransformation :: forall a. Env -> AppM a -> Handler a
naturalTransformation env app = do
  let io :: IO (Either ServerError a) =
        app
          & runReader env
          & runConcurrent
          & runErrorNoCallStack @ServerError
          & runEff

  either <- liftIO io
  liftEither either
