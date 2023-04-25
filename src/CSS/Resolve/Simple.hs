{-# LANGUAGE TupleSections #-}

module CSS.Resolve.Simple (
  Element (..),
  AttrValue (..),
  Selector (..),
  Rule (..),
  element,
  attrId,
  attrClass,
  select',
  select,
  resolve,
  setAttr,
  showHtml
) where

import qualified Data.Map as M
import Control.Monad.Extra (whenMaybeM)
import Control.Monad.State
import Data.Maybe
import Data.List (sortBy)

data Selector
  = SelectorUniversal
  | SelectorType String
  | SelectorClass String
  | SelectorId String
  | SelectorDescendent Selector Selector
  | SelectorChild Selector Selector
  | SelectorList [Selector]
  deriving (Show, Eq)

data Rule = Rule
  { selector :: Selector
  , rules :: [(String, String)]
  } deriving (Show, Eq)

data Specificity = Specificity
  { spNumIdSel :: Int
  , spNumClassSel :: Int
  , spNumTypeSel :: Int
  , spSourcePosition :: Maybe Int
  } deriving (Eq, Ord)

data Element = Element
  { elName :: String
  , elAttrs :: M.Map String AttrValue
  , elChildren :: [Element]
  } deriving (Show, Eq)

data AttrValue
  = NoneValue
  | TextValue String
  | DictValue (M.Map String String)
  deriving (Show, Eq)

type Resolve = State [Element]

instance Show Specificity where
  show x = "Specificity "
        <> show (spNumIdSel x) <> " "
        <> show (spNumClassSel x) <> " "
        <> show (spNumTypeSel x) <> " "
        <> maybe "?" show (spSourcePosition x)

class ShowHtml a where
  showHtml :: a -> String

instance ShowHtml Element where
  showHtml = nest 0 where
    nest d x | null (elChildren x) = ind d $ "<" <> tag x <> " />"
    nest d x = ind d ("<" <> tag x <> ">")
             ++ concatMap (nest $ d + 1) (elChildren x)
             ++ ind d ("</" <> elName x <> ">")
    ind d s = replicate (d * 2) ' ' <> s <> "\n"
    tag x = elName x <> attrs (M.toList $ elAttrs x)
    attrs = concatMap (\(k, v) -> " " <> k <> "=\"" <> showHtml v <> "\"")

instance ShowHtml AttrValue where
  showHtml NoneValue = "undefined"
  showHtml (TextValue x) = x
  showHtml (DictValue kv) = f (M.toList kv) where
    f [] = ""
    f [x] = showKV x
    f (x:xs) = showKV x <> ";" <> f xs
    showKV (k, v) = k <> ":" <> v

element :: String -> [(String, AttrValue)] -> [Element] -> Element
element name attrs = Element name (M.fromList attrs)

getAttr :: Element -> String -> AttrValue
getAttr el = fromMaybe NoneValue . (`M.lookup` elAttrs el)

setAttr :: Element -> String -> AttrValue -> Element
setAttr el name value = el { elAttrs = M.insert name value $ elAttrs el }

attrId :: String -> (String, AttrValue)
attrId s = ("id", TextValue s)

attrClass :: String -> (String, AttrValue)
attrClass s = ("class", TextValue s)

withParent :: Element -> Resolve a -> Resolve a
withParent el m = do
  modify (el :)
  a <- m
  modify tail
  return a

-- | Gets the elements in the DOM that match the specified selector.
select :: Selector -> Element -> [Element]
select sel root = fst <$> select' sel root

select' :: Selector -> Element -> [(Element, Specificity)]
select' sel root = evalState (f root) [] where
  f el = do
    m <- match sel el
    spec <- specificity sel el
    xs <- withParent el $ mapM f (elChildren el)
    return $ [(el, spec) | m] ++ concat xs

match :: Selector -> Element -> Resolve Bool
match SelectorUniversal _     = return True
match (SelectorType name) el  = return $ elName el == name
match (SelectorId name) el    = return $ getAttr el "id" == TextValue name
match (SelectorClass name) el = return $ f $ getAttr el "class" where
  f (TextValue s) = elem name $ words s -- process like class="flex panel"
  f _             = False
match (SelectorDescendent par sub) el = do
  s <- get
  m <- mapM (match par) s
  if or m then match sub el else return False
match (SelectorChild par sub) el = do
  s <- get
  let f [] = return False
      f (x:_) = match par x
  m <- f s
  if m then match sub el else return False
match (SelectorList sel) el = or <$> mapM (`match` el) sel

-- | Compute the specificity of a selector.
-- Keeps the specificity if the selector does not match the element.
specificity' :: Specificity -> Selector -> Element -> Resolve Specificity
specificity' spec0 sel el = f spec0 <$> filter' sel where
  f spec SelectorUniversal   = spec -- Does not modify the specificity.
  f spec (SelectorType _)    = spec { spNumTypeSel  = spNumTypeSel  spec + 1 }
  f spec (SelectorClass _)   = spec { spNumClassSel = spNumClassSel spec + 1 }
  f spec (SelectorId _)      = spec { spNumIdSel    = spNumIdSel    spec + 1 }
  f spec (SelectorDescendent par sub) = f (f spec par) sub
  f spec (SelectorChild par sub)      = f (f spec par) sub
  f spec (SelectorList xs)            = foldl f spec xs
  filter' (SelectorList xs) = do
    SelectorList <$> filterM (`match` el) xs
  filter' s = do
    m <- match s el
    -- Replace with universal selector if the element does not
    -- match the selector, this will not change the priority.
    return $ if m then s else SelectorUniversal

-- | Compute the specificity of a selector.
-- If the selector does not match the element then the specificity is 0-0-0.
specificity :: Selector -> Element -> Resolve Specificity
specificity = specificity' emptySpec

emptySpec :: Specificity
emptySpec = posnSpec Nothing

posnSpec :: Maybe Int -> Specificity
posnSpec = Specificity 0 0 0

-- | Resolve the CSS rules for the DOM.
resolve :: [Rule] -> Element -> Element
resolve css root = evalState (cascade root) [] where
  -- Set incremental position specificity to each CSS rust-set.
  css' = zip css $ posnSpec . Just <$> [0 ..]

  collect el = do
    let matchRule (r, spec) = do
          let s = selector r
          whenMaybeM (match s el) $
            (, rules r) <$> specificity' spec s el
    matched <- catMaybes <$> mapM matchRule css'
    let cmp a b = compare (fst a) (fst b)
        merge = M.fromList . concatMap snd
    return $ merge (sortBy cmp matched)

  cascade el = do
    style <- collect el
    let merge a b | M.null a = b
        merge a (DictValue b) = DictValue (M.union a b)
        merge a _ = DictValue a
        el' = setAttr el "style" $ merge style (getAttr el "style")
    xs <- withParent el $ mapM cascade (elChildren el)
    return $ el' { elChildren = xs }
