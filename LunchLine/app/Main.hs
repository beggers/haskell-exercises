{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe(fromMaybe)
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Int
  deriving Show
|]

data Env = Env { envConn :: SqlBackend }

newtype AppM a = AppM (ReaderT Env IO a)
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

weeklyBudget :: Int
weeklyBudget = 100

spend :: Int -> LineItem -> Int
spend n e = n - (lineItemAmount e)

runAppM :: MonadIO m => Env -> AppM a -> m a
runAppM env (AppM x) = liftIO $ runReaderT x env

runDB :: ReaderT SqlBackend IO a -> AppM a
runDB body = do
    conn <- asks envConn
    liftIO $ runSqlConn body conn

main :: IO ()
main = do
    runStderrLoggingT $
        withSqliteConn ":memory:" $ \conn -> do
            (runAppM (Env conn)) . runDB $ runMigration migrateAll
            runAppM (Env conn) appMain

appMain :: AppM ()
appMain = do
    let selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne
    total <- runDB $ do
        insert_ $ LineItem "Pizza" 11
        insert_ $ LineItem "Burger" 10
        selectSum $ do
            items <- from $ table @LineItem
            pure $ sum_ $ items ^. LineItemAmount
    let remainingBudget = weeklyBudget - total
    liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget