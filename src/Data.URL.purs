module Data.URL
  ( (#)
  , (&)
  , (/)
  , (?)
  , Parts
  , Path(..)
  , pathFromString
  , URL
  , addHash
  , addQuery
  , addSegment
  , class QueryParam
  , fromParts
  , fromString
  , hash
  , host
  , parts
  , password
  , path
  , port
  , query
  , setQuery
  , protocol
  , queryParamTuple
  , resolve
  , setHost
  , setPassword
  , setPort
  , setProtocol
  , setUsername
  , toString
  , username
  ) where

import Prelude

import Data.Array as Array
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, intercalate)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (wrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

class QueryParam a where
  queryParamTuple :: a -> String /\ Array String

instance QueryParam String where
  queryParamTuple s = s /\ [ "" ]
else instance QueryParam (String /\ String) where
  queryParamTuple (k /\ v) = k /\ [ v ]
else instance QueryParam (String /\ Array String) where
  queryParamTuple = identity

foreign import data URL :: Type

instance Show URL where
  show u = "(URL " <> show (toString u) <> ")"

instance Eq URL where
  eq a b = parts a == parts b

instance Ord URL where
  compare a b = compare (parts a) (parts b)

type Parts =
  { path :: Path
  , query :: Map String (Array String)
  , hash :: Maybe String
  , host :: String
  , port :: Maybe Int
  , protocol :: String
  , username :: Maybe String
  , password :: Maybe String
  }

data Path
  = PathEmpty
  | PathAbsolute (Array String)
  | PathRelative (Array String)

derive instance Generic Path _
derive instance Eq Path
derive instance Ord Path
instance Show Path where
  show = genericShow

foreign import fromStringImpl :: String -> Nullable URL

foreign import hashImpl :: URL -> String
foreign import hrefImpl :: URL -> String
foreign import hostImpl :: URL -> String
foreign import passwordImpl :: URL -> String
foreign import pathnameImpl :: URL -> String
foreign import portImpl :: URL -> String
foreign import protocolImpl :: URL -> String
foreign import usernameImpl :: URL -> String
foreign import queryKeysImpl :: URL -> Array String
foreign import queryLookupImpl :: String -> URL -> Array String
foreign import querySetAllImpl :: Array { k :: String, vs :: Array String } -> URL -> URL
foreign import queryPutImpl :: String -> Array String -> URL -> URL
foreign import queryAppendImpl :: String -> Nullable String -> URL -> URL
foreign import queryDeleteImpl :: String -> URL -> URL

foreign import setHashImpl :: String -> URL -> URL
foreign import setHostImpl :: String -> URL -> URL
foreign import setPasswordImpl :: String -> URL -> URL
foreign import setPathnameImpl :: String -> URL -> URL
foreign import setPortImpl :: String -> URL -> URL
foreign import setProtocolImpl :: String -> URL -> URL
foreign import setUsernameImpl :: String -> URL -> URL

fromString :: String -> Maybe URL
fromString = Nullable.toMaybe <<< fromStringImpl

toString :: URL -> String
toString = hrefImpl

fromParts :: Parts -> URL
fromParts u =
  let
    empty = unsafePartial fromJust $ fromString "http://0.0.0.0"

    perhaps :: forall a b. (a -> b -> b) -> Maybe a -> b -> b
    perhaps f a = maybe identity f a

    many :: forall f a b. Foldable f => (b -> a -> b) -> f a -> b -> b
    many f as b = foldl f b as
  in
    setQuery u.query
      $ many addSegment (pathSegments u.path)
      $ setHost u.host
      $ perhaps setUsername u.username
      $ perhaps setPassword u.password
      $ perhaps setHash u.hash
      $ perhaps setPort u.port
      $ setProtocol u.protocol
      $ setHost u.host
      $ empty

parts :: URL -> Parts
parts u =
  { path: path u
  , query: query u
  , host: host u
  , port: port u
  , username: username u
  , password: password u
  , hash: hash u
  , protocol: protocol u
  }

query :: URL -> Map String (Array String)
query u =
  let
    ks = queryKeysImpl u
    vals k = queryLookupImpl k u
  in
    Map.fromFoldable $ map (\k -> k /\ vals k) ks

setQuery :: Map String (Array String) -> URL -> URL
setQuery qs u =
  let
    asRecord = foldlWithIndex (\k a vs -> a <> [ { k, vs } ]) [] qs
  in
    querySetAllImpl asRecord u

pathFromString :: String -> Path
pathFromString s =
  let
    segments = filter (not <<< String.null) <<< String.split (wrap "/")
  in
    maybe PathEmpty PathAbsolute
      $ filter (not <<< Array.null)
      $ Just
      $ segments s

pathSegments :: Path -> Array String
pathSegments (PathEmpty) = []
pathSegments (PathAbsolute s) = s
pathSegments (PathRelative s) = s

path :: URL -> Path
path = pathFromString <<< pathnameImpl

addSegment :: URL -> String -> URL
addSegment u s = resolve (PathRelative [ s ]) u

infixl 3 addSegment as /

addHash :: URL -> String -> URL
addHash u s = setHash s u

infixl 3 addHash as #

addQuery :: forall q. QueryParam q => URL -> q -> URL
addQuery u p =
  let
    k /\ vs = queryParamTuple p
    q = query u
    q'
      | Just _ <- Map.lookup k q = Map.update (Just <<< append vs) k q
      | otherwise = Map.insert k vs q
  in
    setQuery q' u

infixl 3 addQuery as ?
infixl 3 addQuery as &

resolve :: Path -> URL -> URL
resolve p u =
  case p /\ path u of
    PathRelative to /\ PathAbsolute from -> setPathnameImpl (intercalate "/" $ from <> to) u
    PathRelative to /\ _ -> setPathnameImpl (intercalate "/" to) u
    PathAbsolute to /\ _ -> setPathnameImpl (intercalate "/" to) u
    PathEmpty /\ _ -> u

hash :: URL -> Maybe String
hash = filter (not <<< String.null) <<< Just <<< String.replace (wrap "#") (wrap "") <<< hashImpl

host :: URL -> String
host = hostImpl

password :: URL -> Maybe String
password = filter (not <<< String.null) <<< Just <<< passwordImpl

port :: URL -> Maybe Int
port = Int.fromString <<< portImpl

protocol :: URL -> String
protocol = String.replace (wrap ":") (wrap "") <<< protocolImpl

username :: URL -> Maybe String
username = filter (not <<< String.null) <<< Just <<< usernameImpl

setHash :: String -> URL -> URL
setHash = setHashImpl

setHost :: String -> URL -> URL
setHost = setHostImpl

setPassword :: String -> URL -> URL
setPassword = setPasswordImpl

setPort :: Int -> URL -> URL
setPort = setPortImpl <<< Int.toStringAs Int.decimal

setProtocol :: String -> URL -> URL
setProtocol = setProtocolImpl

setUsername :: String -> URL -> URL
setUsername = setUsernameImpl
