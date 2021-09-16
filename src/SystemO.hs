module SystemO (someFunc,exampleApi) where
import System.FilePath
import Data.OpenApi as O
import Data.Aeson as J
import Data.Text.Lazy.Encoding
import Data.Text.Lazy
import qualified Data.ByteString.Lazy as B
emptyApi :: FilePath
emptyApi =  (takeDirectory $ takeDirectory $ __FILE__) </> "examples/api-empty.yml"

someFunc :: Int
someFunc = 0

exampleApi :: IO (Maybe Text)
exampleApi = do
    bytes <- B.readFile emptyApi
    pure $ transform <$> parse bytes
        where
            parse :: B.ByteString -> Maybe O.OpenApi
            parse = J.decode
            transform :: O.OpenApi -> Text
            transform = decodeUtf8 . J.encode