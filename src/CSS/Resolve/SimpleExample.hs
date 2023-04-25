module CSS.Resolve.SimpleExample (example) where

import CSS.Resolve.Simple

example :: IO ()
example = do
  let sel = SelectorChild (SelectorId "window") (SelectorType "p")
  putStrLn "select '#window p': "
  mapM_ print (select' sel simpleElements)
  putStrLn $ showHtml $ resolve styles simpleElements

simpleElements :: Element
simpleElements =
  element "div" [attrId "window"] [
    element "p" [attrClass "title"] [],
    element "p" [attrId "brief", attrClass "content"] [],
    element "div" [attrClass "panel flex"] [
      element "img" [attrClass "icon"] [],
      element "p" [attrClass "content-text"] [],
      element "a" [attrId "ref-link"] []
    ],
    element "p" [attrClass "footnote"] []
  ]

styles :: [Rule]
styles = [
  Rule SelectorUniversal [("font-family", "Times"), ("color", "#222")],
  Rule (SelectorType "div") [("margin", "3px")],
  Rule (SelectorClass "window") [("background", "#aaa")],
  Rule (SelectorClass "panel") [("background", "#fff"), ("border-radius", "6px")],
  Rule (SelectorClass "flex") [("display", "flex"), ("flex-direction", "row")],
  Rule (SelectorClass "icon") [("border", "5px solid red")],
  Rule (SelectorChild (SelectorId "window") (SelectorType "p"))
    [("background-color", "yellow"), ("border-radius", "6px")],
  Rule (SelectorChild (SelectorClass "flex") SelectorUniversal) [("flex", "1 1")]
  ]
