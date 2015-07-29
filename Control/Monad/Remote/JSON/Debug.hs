-- | A tracing version of the Session, that states, as JSON objects, what is sent and received.
traceSession :: String -> Session -> Session
traceSession msg (Session t nt) = Session t nt'
  where
          nt' :: SessionAPI a -> IO a
          nt' (Sync v)  = do
                  putStrLn $ msg ++ ": sync " ++ LT.unpack (decodeUtf8 (encode v))
                  r <- nt (Sync v)
                  putStrLn $ msg ++ ": ret " ++ LT.unpack (decodeUtf8 (encode r))
                  return r
          nt' (Async v) = do
                  putStrLn $ msg ++ ": async " ++ LT.unpack (decodeUtf8 (encode v))
                  nt (Async v)
