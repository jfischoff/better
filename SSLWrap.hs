module SSLWrap where

import qualified OpenSSL.Session as SSL 
import OpenSSL                          ( withOpenSSL )
import Network                   as N   ( listenOn, PortID(PortNumber), accept, PortNumber )
import Network.Socket            as S   
import Network.BSD                      
import qualified Data.ByteString as B   ( hGetSome, hPut, null )
import Control.Exception                ( bracket, finally, bracketOnError )
import Control.Concurrent               ( killThread, takeMVar, MVar, ThreadId, newEmptyMVar, forkIOUnmasked
                                        , putMVar
                                        )
import Control.Monad                    ( liftM2 )
import qualified System.IO       as I   ( hClose, hSetBuffering, BufferMode(NoBuffering) )


mapSSL :: FilePath -> PortNumber -> String -> PortNumber -> IO SSL.SSL
mapSSL cafile in_port out_host out_port = withSocketsDo $ withOpenSSL $ do
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
                return ssl







