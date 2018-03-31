{-# LANGUAGE OverloadedStrings #-}

import DSL


setFruit = cat [
                 pat  [ "SELFRUIT" ],
                 temp [
                        think [
                        setlocal  "props" [ readset "props" ],
                        setlocal  "pos" [ srai ["RANDOM 16"] ],
                        setglobal "fr_props" [
                                  srai [ "QUITARESPACIOS ", srai ["SELECTITEM ", getlocal "pos",
                                        " IN ", getlocal "props"] ] ],
                        setglobal "fruit" [
                                  srai ["TOP ", getglobal "fr_props"]
                                          ] ]
                        
                      {-]-},
                      "De acuerdo, ya he pensado una fruta"
                      ]
               ]

sizeQuest   = [["Es ", set "sizes"]]
flavorQuest = [["Es ", set "flavors"]]
vitQuest   = [["Tiene vitamina ", set "vit"]]
colorQuest = [["Es ", set "colors"]]
fruitQuest = [["Es ", set "fruits"]]


propCat propQuest idxs = for propQuest $ (\x ->
          cat [
                pat x,
                temp [
                       think [ setlocal "prop" [star] ],
                       condv "prop" ([
                           li2 [] [ val [ srai ["SELECTITEM ", Text $ show idx, " IN ",
                                               getglobal "fr_props"] ] , " Sí" ] | idx <- idxs
                                     ] ++
                                     [
                           li2 [] [ "No" ]
                                     ])
                     ]
              ])

sizeCat = propCat sizeQuest [5]
flavorCat = propCat flavorQuest [6]
vitCat = propCat vitQuest [7,8,9]
colorCat = propCat colorQuest [2,3,4]
fruitCat = propCat fruitQuest [1]

startGame = ["start","Emp+ ^ juego ^ yo ^ fruta",
             "Jug+ ^ fruta+"]

for = flip map

doc1 = for startGame $ (\x ->
          cat [
                pat [x],
                temp [ srai [ "SELFRUIT" ] ]
              ])

doc = doc1 ++ sizeCat ++ flavorCat ++
      vitCat ++ colorCat ++ fruitCat ++ [setFruit]


main :: IO ()
main = do putStrLn $ renderDocument doc
