import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, IOException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Time.Clock

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
              -> Permissions
              -> Maybe Integer
              -> UTCTime
              -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\e -> return Nothing) :: IOException -> IO (Maybe Integer)) $
    bracket (openFile path ReadMode) hClose $ \h -> do
                                                        size <- hFileSize h
                                                        return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
                   where check name = do
                           perms <- getPermissions name
                           size <- getFileSize name
                           modified <- getModificationTime name
                           return (p name perms size modified)

myTest path _ (Just size) _ =
    takeExtension path == ".hs" && size > 131072
myTest _ _ _ _ = False

type InfoP a = FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)