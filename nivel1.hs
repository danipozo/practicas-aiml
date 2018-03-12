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

sizeQuest   = [["Es ", set "sizes"]]
flavorQuest = [["Es ", set "flavors"]]
vitAQuest   = [["Tiene vitamina a"]]

colorQuest = [["Es ", set "colors"]]

-- sizeCat = for sizeQuest $ (\x ->
--           cat [
--                 pat x,
--                 temp [
--                        setlocal "size" [ srai ["SELECTITEM 5 IN ",
--                                                getglobal "fr_props"] ],
--                        condv "size" [
--                            li2 [] [ val [star], " Sí" ],
--                            li2 [] [ "No" ]
--                                     ]
--                      ]
--               ])

propCat propQuest index = for propQuest $ (\x ->
          cat [
                pat x,
                temp [
                       setlocal "size" [ srai ["SELECTITEM ", Text $ show index, " IN ",
                                               getglobal "fr_props"] ],
                       condv "size" [
                           li2 [] [ val [star], " Sí" ],
                           li2 [] [ "No" ]
                                    ]
                     ]
              ])

sizeCat = propCat sizeQuest 5
flavorCat = propCat flavorQuest 6

startGame = ["start","Empe+ ^ juego ^ fruta",
             "Jug+ ^ fruta+"]

for = flip map

doc1 = for startGame $ (\x ->
          cat [
                pat [x],
                temp [ srai [ "SELFRUIT" ] ]
              ])

doc = doc1 ++ sizeCat ++ flavorCat ++ [setFruit]


main :: IO ()
main = do putStrLn $ renderDocument doc
