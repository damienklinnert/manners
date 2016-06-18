{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import qualified Pact as P
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy.Char8 as BLC
import Text.PrettyPrint.ANSI.Leijen
import Data.Aeson.Encode.Pretty (encodePretty)

type Path = String

contractStart :: Path -> P.ContractDescription -> Doc
contractStart path contract =
  vsep
    [ text ("using contract at: " ++ path)
    , text ("consumer: " ++ P.serviceName (P.contractConsumer contract))
    , text ("provider: " ++ P.serviceName (P.contractProvider contract))
    ]

verifyStart :: P.Interaction -> Doc
verifyStart interaction =
  hang 4 $
    (text "-" <+> underline (text (P.interactionDescription interaction))) </>
    (text "with state:" <+> text (M.fromMaybe "N/A" (P.interactionState interaction)))

headers :: P.Headers -> Doc
headers (P.Headers hs) =
  vcat (map (\(k, v) -> (fillBreak (maxHeaderLength + 1) (string k <> ":") <+> string v)) hs)
  where
    maxHeaderLength =
      maximum (map (\(k, _) -> length k) hs)

validationError :: P.ValidationError -> Doc
validationError err =
  case err of

    P.MethodValidationError left right ->
      vcat
        [ hang 2 (text "expected method:" <$$> green (string left))
        , hang 2 (text "actual method:" <$$> red (string right))
        ]

    P.PathValidationError left right ->
      vcat
        [ hang 2 (text "expected path:" <$$> green (string left))
        , hang 2 (text "actual path:" <$$> red (string right))
        ]

    P.QueryValidationError left right ->
      vcat
        [ hang 2 (text "expected query:" <$$> green (string (show left)))
        , hang 2 (text "actual query:" <$$> red (string (show right)))
        ]

    P.StatusValidationError left right ->
      vcat
        [ hang 2 (text "expected status code:" <$$> green (int left))
        , hang 2 (text "actual status code:" <$$> red (int right))
        ]

    P.HeaderValidationError left right ->
      vcat
        [ hang 2 (text "expected headers:" <$$> green (headers left))
        , hang 2 (text "actual headers:" <$$> red (headers right))
        ]

    P.BodyValidationError left right ->
      vcat
        [ hang 2 (text "expected body:" <$$> green (string (BLC.unpack (encodePretty left))))
        , hang 2 (text "actual body:" <$$> red (string (BLC.unpack (encodePretty right))))
        ]

validationErrors :: [P.ValidationError] -> Doc
validationErrors errs =
  vcat (map (\err -> validationError err <> line) errs)
