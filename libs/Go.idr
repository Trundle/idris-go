module Go

public export
data GoInterface : String -> Type where
  MkInterface : (iface : String) -> GoInterface iface

public export
data Go_FFI_Call = Function String
  | Method (GoInterface iface) String

mutual
  ||| Go foreign types
  public export
  data Go_Types : Type -> Type where
    Go_Str       : Go_Types String
    Go_Unit      : Go_Types ()
    Go_Interface : Go_Types (GoInterface a)
    Go_Nilable   : Go_Types a -> Go_Types (Maybe a)
    Go_Any       : Go_Types (FFI_C.Raw a)
    -- Note that this is actually only valid as return value
    Go_MultiVal  : (Go_Types a, Go_Types b) -> Go_Types (a, b)

  public export
  FFI_Go : FFI
  FFI_Go = MkFFI Go_Types Go_FFI_Call String

  public export
  GIO : Type -> Type
  GIO = IO' FFI_Go

public export
%inline
gocall : (f : Go_FFI_Call) -> (ty : Type) -> {auto fty : FTy FFI_Go [] ty} -> ty
gocall f ty = foreign FFI_Go f ty
