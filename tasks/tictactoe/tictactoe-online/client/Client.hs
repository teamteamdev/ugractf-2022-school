import System.Console.Haskeline
import Data.Conduit
import Data.Word
import System.Exit
import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Trans.Class
import qualified Data.ByteString.UTF8 as BS
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import Control.Monad.IO.Class

import TicTacToe.Types

myClientSettings :: ClientSettings
myClientSettings = clientSettings 13713 "tictactoe.s.2022.ugractf.ru"

initGame :: (MonadIO m, MonadFail m) => ConduitT Packet Packet m ()
initGame = do
  yield $ GreetPacket { greetMagic = yurtaMagic, greetVersion = yurtaVersion }
  await >>= \case
    Just (GreetPacket {..}) | greetMagic == yurtaMagic && greetVersion >= yurtaVersion -> sendLicense
    Just (ErrorPacket code) -> fail $ "Server error: " ++ show code
    _ -> fail "Unexpected packet"

sendLicense :: (MonadIO m, MonadFail m) => ConduitT Packet Packet m ()
sendLicense = do
  -- let key = "tri" <> "al_" <> "key"
  let key = "f846360dec3d7676"
  yield $ LicensePacket { licenseKey = ByteStringS2 key }
  await >>= \case
    Just (OKPacket { }) -> return ()
    Just (ErrorPacket code) -> fail $ "Server error: " ++ show code
    _ -> fail "Server error"

showHelp :: IO ()
showHelp = do
  putStrLn "Команды:"
  putStrLn "help: Показать это сообщение"
  putStrLn "quit: Выйти из игры"
  putStrLn "new X|O: Начать новую игру за крестики или нолики"
  putStrLn "turn X Y: Сделать ход: поставить свой символ на заданную клетку доски (отсчёт с нуля)"

getCommand :: ConduitT () Packet (InputT IO) ()
getCommand = do
  minput <- lift $ getInputLine "% "
  case filter (/= "") <$> words <$> minput of
    Nothing -> yield DisconnectPacket
    Just ["quit"] -> yield DisconnectPacket
    Just ["help"] -> do
      liftIO showHelp
      getCommand
    Just ["new", "X"] -> do
      -- liftIO $ putStrLn "Данная опция не оплачена!"
      yield $ NewGamePacket SideX
      getCommand
    Just ["new", "O"] -> do
      yield $ NewGamePacket SideO
      getCommand
    Just ["turn", xStr, yStr] -> do
      let x = read xStr :: Word8
          y = read yStr :: Word8
      if x >= 3 || y >= 3 then
        liftIO $ putStrLn "Неверные координаты"
      else
        yield $ TurnPacket x y
      getCommand
    Just _ -> do
      liftIO $ putStrLn "Неизвестная команда"
      liftIO showHelp
      getCommand

showResults :: (MonadIO m, MonadFail m) => ConduitT Packet Void m ()
showResults = do
  await >>= \case
    Just OKPacket -> liftIO $ putStrLn "ОК"
    Just (EchoPacket dat) -> liftIO $ putStrLn $ BS.toString $ unS2 dat
    Just (TurnPacket x y) -> liftIO $ putStrLn $ "Противник сделал ход: (" ++ show x ++ ", " ++ show y ++ ")"
    Just (EndGamePacket winner) -> do
      let winnerStr =
            case winner of
              Nothing -> "ничья"
              Just SideX -> "крестики"
              Just SideO -> "нолики"
      liftIO $ putStrLn $ "Игра закончена, победитель: " ++ winnerStr
    Just (ErrorPacket err) -> liftIO $ putStrLn $ "Ошибка: " <> show err
    Just DisconnectPacket -> do
      liftIO $ putStrLn "Сервер отключился"
      liftIO $ exitWith ExitSuccess
    Just _ -> fail "Неожиданный пакет"
    Nothing -> fail "Неожиданное отключение"
  showResults

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    h <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar h ())
    return h

main :: IO ()
main = do
  putStrLn "Добро пожаловать в испытательную версию игры Крестики-Нолики Про Онлайн!"
  putStrLn "В испытательной версии разрешается играть только за нолики."
  putStrLn "Поддержите создателей проекта и купите полную версию игры!"
  putStrLn ""
  putStrLn "Нажмите любую клавишу..."
  _ <- getChar

  runTCPClient myClientSettings $ \appData -> do
    let appInput = appSource appData .| conduitDecode
    let appOutput :: (MonadThrow m, MonadIO m) => ConduitT Packet Void m ()
        appOutput = conduitEncode .| appSink appData
    (appInput', ()) <- runConduit $ appInput =$$+ initGame .| appOutput

    outputThread <- forkThread $ runConduit $ appInput' =$$+- showResults
    runInputT defaultSettings $ do
      runConduit $ getCommand .| appOutput
    takeMVar outputThread
