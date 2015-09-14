data CannotShow = CannotShow
                deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
    deriving (Show)