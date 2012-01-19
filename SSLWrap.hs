module SSLWrap(mapSSL,myForkIO) where

import qualified OpenSSL.Session as SSL ( context, contextSetVerificationMode, contextSetDefaultCiphers
                                        , VerificationMode(VerifyPeer), contextSetCAFile, connection, shutdown
                                        , ShutdownType(Bidirectional), connect, write, read
                                        )
import OpenSSL                          ( withOpenSSL )
import Network                   as N   ( listenOn, PortID(PortNumber), accept, PortNumber )
import Network.Socket            as S   ( withSocketsDo, sClose, socket, Family(AF_INET), SocketType(Stream)
                                        , connect, SockAddr(SockAddrInet) )
import Network.BSD                      ( getHostByName, defaultProtocol, hostAddress )
import qualified Data.ByteString as B   ( hGetSome, hPut, null )
import Control.Exception                ( bracket, finally, bracketOnError )
import Control.Concurrent               ( killThread, takeMVar, MVar, ThreadId, newEmptyMVar, forkIOUnmasked
                                        , putMVar
                                        )
import Control.Monad                    ( liftM2 )
import qualified System.IO       as I   ( hClose, hSetBuffering, BufferMode(NoBuffering) )


mapSSL :: FilePath -> PortNumber -> String -> PortNumber -> IO ()
mapSSL cafile in_port out_host out_port = withSocketsDo$ withOpenSSL$ do
    bracket
      (N.listenOn (PortNumber in_port))
      sClose
      $ \sockin -> do
    (h,_,_) <- N.accept sockin
    I.hSetBuffering h I.NoBuffering
    bracketOnError
      (socket AF_INET Stream defaultProtocol)
      sClose
      $ \sout -> do
    he <- getHostByName out_host
    S.connect sout (SockAddrInet (fromIntegral out_port) (hostAddress he))

    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
    SSL.contextSetVerificationMode ctx (SSL.VerifyPeer True False Nothing)
    SSL.contextSetCAFile ctx cafile
    bracket
      (SSL.connection ctx sout)
      (flip SSL.shutdown SSL.Bidirectional)
      $ \ssl -> do
    SSL.connect ssl
  
    bracket
      (liftM2 (,) (myForkIO (readWriteLoop h ssl)) (myForkIO (readWriteLoop0 h ssl)))
      (\((_,t0),(_,t1)) -> killThread t0 >> killThread t1)
      $ \((m0,_),(m1,_)) -> do

    takeMVar m0
    takeMVar m1
    
  where
    readWriteLoop h ssl = do
       b <- B.hGetSome h 1024 
       SSL.write ssl b
       if B.null b then SSL.shutdown ssl SSL.Bidirectional
         else readWriteLoop h ssl
    readWriteLoop0 h ssl = do
       b <- SSL.read ssl 1024 
       B.hPut h b
       if B.null b then I.hClose h
         else readWriteLoop0 h ssl

myForkIO :: IO () -> IO (MVar (),ThreadId)
myForkIO io = do
     m <- newEmptyMVar
     t <- forkIOUnmasked (io `finally` putMVar m ())
     return (m,t)




