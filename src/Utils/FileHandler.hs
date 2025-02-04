module Utils.FileHandler where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, SomeException)
import Utils.Error

readLatexFile :: FilePath -> IO (Either CompilerError T.Text)
readLatexFile path = catch
    (Right <$> TIO.readFile path)
    (\(e :: SomeException) -> return $ Left $ ProcessingError $ show e)

writeOutputFile :: FilePath -> T.Text -> IO (Either CompilerError ())
writeOutputFile path content = catch
    (Right <$> TIO.writeFile path content)
    (\(e :: SomeException) -> return $ Left $ ProcessingError $ show e)
