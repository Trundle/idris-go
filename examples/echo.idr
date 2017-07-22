module Main

import Go

-- XXX make it total

%include Go "bufio"
%include Go "net"

go : GIO () -> GIO ()
go action = gocall (Function "Go") (Raw (GIO ()) -> GIO ()) (MkRaw action)

Listener : Type
Listener = GoInterface "net.Listener"

Conn : Type
Conn = GoInterface "net.Conn"

Addr : Type
Addr = GoInterface "net.Addr"

implementation Show Addr where
  show addr = unsafePerformIO $ gocall (Method addr "String") (Addr -> GIO String) addr

GoError : Type
GoError = GoInterface "error"

toEither : (a, Maybe GoError) -> Either GoError a
toEither (_, Just e) = Left e
toEither (x, Nothing) = Right x

errorDesc : GoError -> String
errorDesc error = unsafePerformIO $ gocall (Method error "Error") (GoError -> GIO String) error

accept : Listener -> GIO (Either GoError Conn)
accept listener =
  map toEither $ gocall (Method listener "Accept") (Listener -> GIO (Conn, Maybe GoError)) listener

listen : String -> String -> GIO (Either GoError Listener)
listen net laddr =
   map toEither $ gocall (Function "net.Listen")
           (String -> String -> GIO (Listener, Maybe GoError))
           net laddr

remoteAddr : Conn -> Addr
remoteAddr conn =
  unsafePerformIO $ gocall (Method conn "RemoteAddr") (Conn -> GIO Addr) conn

Reader : Type
Reader = GoInterface "bufio.Reader"

Writer : Type
Writer = GoInterface "bufio.Writer"

-- XXX should be a reader, not a Conn
newReader : Conn -> GIO Reader
newReader conn = do
  (MkGoPtr val) <- gocall (Function "bufio.NewReader") (Conn -> GIO (GoPtr Reader)) conn
  pure val

newWriter : Conn -> GIO Writer
newWriter conn = do
  (MkGoPtr val) <- gocall (Function "bufio.NewWriter") (Conn -> GIO (GoPtr Writer)) conn
  pure val

readString : Reader -> Byte -> GIO (Either GoError String)
readString reader delim = map toEither $
  gocall (Method reader "ReadString") (Reader -> Byte -> GIO (String, Maybe GoError)) reader delim

writeString : Writer -> String -> GIO (Int, Maybe GoError)
writeString writer s =
  gocall (Method writer "WriteString") (Writer -> String -> GIO (Int, Maybe GoError)) writer s

flush : Writer -> GIO (Maybe GoError)
flush writer = gocall (Method writer "Flush") (Writer -> GIO (Maybe GoError)) writer

echoServer : Listener -> GIO ()
echoServer listener = 
  do Right conn <- accept listener
       | Left e => putStrLn' ("Accepting went wrong: " ++ errorDesc e)
     putStrLn' $ "Got a client! It's " ++ show (remoteAddr conn)
     go $ do
       reader <- newReader conn
       writer <- newWriter conn
       handleLine reader writer
     echoServer listener
  where
    handleLine : Reader -> Writer -> GIO ()
    handleLine reader writer = do
      Right line <- readString reader (MkByte '\n')
        | Left e => putStrLn' ("Could not read line: " ++ errorDesc e)
      (_, error) <- writeString writer line
      case error of
        Just e => putStrLn' ("Could not send line back: " ++ errorDesc e)
        Nothing => pure ()
      flush writer
      handleLine reader writer


main : GIO ()
main = do
  Right listener <- listen "tcp" ":1234"
    | Left e => putStrLn' ("Could not create listener: " ++ errorDesc e)
  echoServer listener
