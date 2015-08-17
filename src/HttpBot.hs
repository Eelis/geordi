{-# LANGUAGE UnicodeSyntax #-}

import qualified RequestEval
import qualified Data.ByteString.UTF8 as UTF8
import qualified Cxx.Show
import qualified Request

import Snap.Http.Server (quickHttpServe)
import Control.Monad.IO.Class (liftIO)
import Snap.Core (writeBS, urlDecode, rqQueryString, getRequest)

import Prelude hiding ((.))
import Util

main :: IO ()
main = do
    eval ← RequestEval.evaluator
    quickHttpServe $ do
      qs <- rqQueryString . getRequest
      case urlDecode qs of
        Nothing -> writeBS $ UTF8.fromString "error: urlDecode"
        Just req -> do
          resp <- liftIO (eval (UTF8.toString req) context extra_env)
          writeBS $ UTF8.fromString $ Request.response_output resp
  where
    extra_env = []
    context = Request.Context Cxx.Show.noHighlighting False []

-- Example use: docker run -p 1234:1234 -it geordi geordi-http --port=1234 --no-compression
