{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Generic helpers for Error handling in HeadedMegaParsec.
module PostgresqlSyntax.Extras.Error where

import Control.Monad.State.Strict
  ( MonadState (get, put),
    runState,
  )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import PostgresqlSyntax.Prelude hiding (cons, fromList, head, init, last, reverse, tail, uncons)
import Text.Megaparsec hiding (errorBundlePrettyWith)
import Text.Megaparsec.Error (ErrorItem)

-- | Render all Megaparsec parsing errors as a list of position ('Text.Megaparsec.SourcePos') and messages.
errorBundlePrettyStruct ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  NonEmpty (SourcePos, String)
errorBundlePrettyStruct ParseErrorBundle {bundleErrors, bundlePosState} =
  fst $ attachSourcePosAndMessage renderError bundleErrors bundlePosState
  where
    renderError epos e = (epos, parseErrorTextPretty e)

-- | A custom version of 'Text.Megaparsec.attachSourcePos' to provide the NonEmpty list of errors with their position while only traversing the errors list once.
attachSourcePosAndMessage ::
  (TraversableStream s) =>
  -- | Format function for a single 'ParseError' and its 'SourcePos'
  (SourcePos -> ParseError s e -> (SourcePos, String)) ->
  -- | The collection of items
  NonEmpty (ParseError s e) ->
  -- | Initial 'PosState'
  PosState s ->
  -- | The collection with 'SourcePos'es added and the final 'PosState'
  (NonEmpty (SourcePos, String), PosState s)
attachSourcePosAndMessage format xs = runState (traverse f xs)
  where
    f a = do
      pst <- get
      let pst' = reachOffsetNoLine (errorOffset a) pst
      put pst'
      let position = pstateSourcePos pst'
      return $ format position a

-- | Provide an equivalent to megaparsec's 'Text.Megaparsec.chunk' in a context where we manipulate the texts directly.
chunkFailure ::
  (Token s ~ Char, MonadParsec e s m) =>
  -- | expected chunk
  Text ->
  -- | actual (given) chunk
  Text ->
  m a
chunkFailure expectedTxt givenTxt = failure (Just (errorItemConverter givenTxt)) (Set.fromList (pure $ errorItemConverter expectedTxt))
  where
    -- Note that Text.foldr' (the strict version) should probably be used instead but it doesn't exist before Text 2.x.
    errorItemConverter t = (Tokens ((Text.foldr (NonEmpty.cons) (pure (' ' :: Char)) t)))
