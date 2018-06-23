
-- | Utils to help unpack LSP messages.
module DDC.Driver.LSP.Protocol.Parse where
import qualified Text.JSON      as J


-- | Take the `Just` option of a `Maybe`.
takeJust :: Maybe a -> Maybe a
takeJust x = x


-- | Take the `Right` option of an `Either`.
takeRight :: Either a b -> Maybe b
takeRight e
 = case e of
        Left _  -> Nothing
        Right b -> Just b


-- | Take the fields of an Object.
takeObject :: J.JSValue -> Maybe [(String, J.JSValue)]
takeObject vv
 = case vv of
        J.JSObject fs   -> Just $ J.fromJSObject fs
        _               -> Nothing


-- | Take the value of a string.
takeString :: J.JSValue -> Maybe String
takeString vv
 = case vv of
        J.JSString js   -> Just $ J.fromJSString js
        _               -> Nothing


-- | Take the value of a rational.
takeRational :: J.JSValue -> Maybe Rational
takeRational vv
 = case vv of
        J.JSRational _ r -> Just r
        _               -> Nothing


-- | Lookup a boolean in a nested object.
takeObjPathBool :: J.JSValue -> [String] -> Maybe (Maybe Bool)
takeObjPathBool jv []
 = case jv of
        J.JSBool b      -> Just (Just b)
        _               -> Nothing

takeObjPathBool jv (p : ps)
 = case jv of
        J.JSObject fs
         -> case lookup p (J.fromJSObject fs) of
                Nothing  -> Just Nothing
                Just jv' -> takeObjPathBool jv' ps

        _ -> Nothing


-- | Parse a possibly null value.
withNull :: J.JSValue -> (J.JSValue -> Maybe a) -> Maybe (Maybe a)
withNull vv f
 = case vv of
        J.JSNull -> Just Nothing
        _        -> case f vv of
                        Nothing -> Nothing
                        Just x  -> Just (Just x)

