module Game.Sxako.DataFiles (
  loadDataFile,
  loadDataFileStrict,
) where

import qualified Codec.Compression.Lzma as Lzma
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Paths_sxako

loadDataFile :: FilePath -> IO BSL.ByteString
loadDataFile fpPre = do
  fp <- getDataFileName fpPre
  let process =
        if ".xz" `isSuffixOf` fpPre
          then Lzma.decompress
          else id
  process <$> BSL.readFile fp

loadDataFileStrict :: FilePath -> IO BS.ByteString
loadDataFileStrict = fmap BSL.toStrict . loadDataFile
