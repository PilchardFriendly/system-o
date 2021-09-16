module SystemO (someFunc,exampleApi, emptyApi, isSubApiOf) where
import System.FilePath
import Data.OpenApi as O
import Data.Aeson as J
import Data.Text.Lazy.Encoding
import Data.Text.Lazy
import qualified Data.ByteString.Lazy as B
import SystemO.SubApi
emptyApiPath :: FilePath
emptyApiPath =  (takeDirectory . takeDirectory $ __FILE__) </> "examples/api-empty.yml"

someFunc :: Int
someFunc = 0

emptyApi :: O.OpenApi
emptyApi = mempty


exampleApi :: IO (Maybe Text)
exampleApi = do
    bytes <- B.readFile emptyApiPath
    pure $ transform <$> parse bytes
        where
            parse :: B.ByteString -> Maybe O.OpenApi
            parse = J.decode
            transform :: O.OpenApi -> Text
            transform = decodeUtf8 . J.encode