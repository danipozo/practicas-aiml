{-# LANGUAGE OverloadedStrings #-}

import DSL


setFruit = cat [
                 pat  [ "SELFRUIT" ],
                 temp [
                        think [
                        setlocal "props" [ readset "props" ],
                        setlocal  "list_fruit" [ readset "fruits" ],
                        setlocal  "pos" [ srai ["RANDOM 16"] ],
                        setglobal "fruit" [
                                  srai ["SELECTITEM ", getlocal "pos",
                                        " IN ", getlocal "list_fruit"]
                                          ],
                        setglobal "fr_props" [
                                  srai [ "QUITARESPACIOS ", srai ["SELECTITEM ", getlocal "pos",
                                        " IN ", getlocal "props"] ] ]
                      ],
                      "De acuerdo, ya he pensado una fruta"
                      ]
               ]

sizeQuest = [["Es ", set "sizes"]]

sizeCat = for sizeQuest $ (\x ->
          cat [
                pat x,
                temp [
                       setlocal "size" [ srai ["SELECTITEM 5 IN ",
                                               getglobal "fr_props"] ],
                       condn "size" [
                         
                                    ]
                     ]
              ])

startGame = ["start","Empe+ ^ juego ^ fruta",
             "Jug+ ^ fruta+"]

for = flip map

doc1 = for startGame $ (\x ->
          cat [
                pat [x],
                temp [ srai [ "SELFRUIT" ] ]
              ])

doc = doc1 ++ sizeCat ++ [setFruit]


main :: IO ()
main = do putStrLn $ renderDocument doc
