import Data.Maybe
import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Serialization.Binary
import Control.Monad.IO.Class

import TicTacToe.Types
import TicTacToe.Game

echoPacket :: String -> Packet
echoPacket = EchoPacket . ByteStringS2 . BS.fromString

myServerSettings :: ServerSettings
myServerSettings = serverSettings 13713 "*"

prefix :: String
prefix = "ugra_tic_tac_reverse_takes_time_"

secret2 :: ByteString
secret2 = "a5xyEHF4b1gcK8rLUdc8oL2FM21Grwvu"

salt2Size :: Int
salt2Size = 12

getFlag :: ByteString -> String
getFlag license = prefix ++ postfix
  where postfix = take salt2Size $ show $ hmacGetDigest (hmac secret2 license :: HMAC SHA256)

initGame :: (MonadIO m, MonadFail m) => ConduitT Packet Packet m ()
initGame =
  await >>= \case
    Just (GreetPacket {..})
        | greetMagic == yurtaMagic -> do
            if greetVersion > yurtaVersion
            then do
              yield $ ErrorPacket IncompatibleVersion
              fail "Incompatible version"
            else do
              yield $ GreetPacket { greetMagic = yurtaMagic, greetVersion = yurtaVersion }
              checkLicense
    Just _ -> do
      yield $ ErrorPacket UnexpectedPacket
      fail "Unexpected packet"
    Nothing -> fail "Client disconnected"

checkLicense :: (MonadIO m, MonadFail m) => ConduitT Packet Packet m ()
checkLicense =
  await >>= \case
    Just (LicensePacket {..}) -> do
      let key = unS2 licenseKey
      yield OKPacket
      let secret =
            if key == "trial_key" then
              Nothing
            else
              Just key
      liftIO $ print secret
      yield $ echoPacket "Добро пожаловать"
      mainMenu secret
    Just (ErrorPacket code) -> fail $ "Client error: " ++ show code
    Just _ -> do
      yield $ ErrorPacket UnexpectedPacket
      fail "Unexpected packet"
    Nothing -> fail "Client disconnected"

defaultHandler :: (MonadIO m, MonadFail m) => Maybe ByteString -> Maybe Packet -> ConduitT Packet Packet m ()
defaultHandler license packet =
  case packet of
    Just (NewGamePacket {..}) ->
      case newSide of
        SideX
          | isNothing license -> do
              yield $ ErrorPacket NotLicensed
              mainMenu license
          | otherwise -> do
            yield OKPacket
            awaitTurn license SideO emptyXOField
        SideO -> makeTurn license SideX emptyXOField
    Just DisconnectPacket -> do
      yield $ echoPacket "Пока-пока!"
      yield DisconnectPacket
      return ()
    Just (ErrorPacket code) -> fail $ "Client error: " ++ show code
    Just _ -> do
      yield $ ErrorPacket UnexpectedPacket
      fail "Unexpected packet"
    Nothing -> fail "Client disconnected"

mainMenu :: (MonadIO m, MonadFail m) => Maybe ByteString -> ConduitT Packet Packet m ()
mainMenu license = do
  await >>= defaultHandler license

handleResult :: (MonadIO m, MonadFail m) => Maybe ByteString -> GameSide -> Maybe GameSide -> ConduitT Packet Packet m ()
handleResult license side result = do
  case license of
    Just lic | side == SideO -> yield $ echoPacket $ "Подарочный код на другие игры нашей компании: " ++ getFlag lic
    _ -> return ()
  yield $ EndGamePacket result
  mainMenu license

makeTurn :: (MonadIO m, MonadFail m) => Maybe ByteString -> GameSide -> XOField -> ConduitT Packet Packet m ()
makeTurn license side field = do
  liftIO $ putStrLn $ "Looking for optimal turns for side " <> show side
  case bestMove side field of
    Nothing -> fail "Impossible"
    Just (_bestScore, (x, y)) -> do
      yield $ TurnPacket (fromIntegral x) (fromIntegral y)
      case placeSymbol x y side field of
        XOFinished result -> handleResult license side result
        XOInProgress newField -> do
          liftIO $ putStrLn "Making turn: "
          liftIO $ print newField
          yield $ echoPacket $ "Текущая доска:\n" ++ show newField
          awaitTurn license side newField
        XOIllegalTurn -> fail "Impossible"

awaitTurn :: (MonadIO m, MonadFail m) => Maybe ByteString -> GameSide -> XOField -> ConduitT Packet Packet m ()
awaitTurn license side field =
  await >>= \case
    Just (TurnPacket {..}) ->
      case placeSymbol (fromIntegral turnX) (fromIntegral turnY) (oppositeSide side) field of
        XOFinished result -> handleResult license side result
        XOInProgress newField -> do
          liftIO $ putStrLn "Текущая доска: "
          liftIO $ print newField
          makeTurn license side newField
        XOIllegalTurn -> do
          yield $ ErrorPacket IllegalTurn
          awaitTurn license side field
    packet -> defaultHandler license packet

main :: IO ()
main = do
  runTCPServer myServerSettings $ \appData -> do
    runConduit $ appSource appData .| conduitDecode .| initGame .| conduitEncode .| appSink appData
