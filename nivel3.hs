{-# LANGUAGE OverloadedStrings #-}

import DSL


setFruit = cat [
                 pat  [ "SETFRUIT" ],
                 temp [
                        think [
                        setglobal  "props" [ readset "props" ],
                        setglobal  "askprops" [],
                        setglobal  "bfruitasked" ["n"],
                        setlocal  "pos" [ srai ["RANDOM 16"] ],
                        setglobal "fr_props" [
                                  srai [ "QUITARESPACIOS ", srai ["SELECTITEM ", getlocal "pos",
                                        " IN ", getglobal "props"] ] ],
                        setglobal "fruit" [
                                  srai ["TOP ", getglobal "fr_props"]
                                          ] ]
                        
                      {-]-},
                      "De acuerdo, piensa una fruta"
                      ]
               ]


-- testProp prop idxs fr_props = cat
--                    [
--                      pat [ "TEST ", set "number", " ^" ]            
--                    ]

testProp prop idxs fr_props y n = [
                            think [ setlocal "_prop_" [ prop ] ],
                            condv "_prop_" ([
                                 li2 [] [
                                      val [ setlocal "_prop_" [ srai [ "SELECTITEM ", Text $ show idx, " IN ",  srai [ "QUITARESPACIOS ", fr_props ] ]  ]
                                        ], y
                                           ] | idx <- idxs 
                            
                              ]++[
                                 li2 [] [n]
                                 ])
                            ]

fruitCat = cat [
           pat [ "FRUIT ", set "number" ],
           temp [
              srai [ "SELECTITEM ", star, " IN ", getglobal "props" ]
                ]
            ]

fruit n = srai [ "FRUIT ", Text $ show n ]
fruit_ n = srai [ "FRUIT ", n ]

processYesAns = cat [
              pat [ "PROCESSYESANS" ],
              temp [
                think [setlocal "indices" [""],
                setlocal "cont"    [ srai [ "COUNT ", getglobal "props" ] ] ],

                -- getglobal "askpropidx",
                -- getglobal "askprop",
                think [setlocal "prop" [ srai [ "SELECTITEM ", getglobal "askpropidx", " IN ", srai [ "QUITARESPACIOS ", fruit_ (getlocal "cont") ] ] ] ],

                condv "cont" [
                  li "1" [],
                  li2 [] [
                    condn "askprop" [
                          li2 [] [ val [ getlocal "prop" ] ], -- Igual
                          
                          li2 [] [
                              -- "Justo antes de ADDITEM",
                              -- "Comparando con ", getlocal "prop", " es /=",
                              -- fruit_ "5",
                              think [setlocal "indices" [
                                    getlocal "indices", " ", getlocal "cont"
                                                 ] ]
                                    ]
                          ],
                          think [setlocal "cont" [ mapget "predecessor" (getlocal "cont") ],
                          setlocal "prop" [ srai [ "SELECTITEM ", getglobal "askpropidx", " IN ", srai [ "QUITARESPACIOS ", fruit_ (getlocal "cont") ] ] ] ],
                          loop
                      
                    ]
                  ],

                  think [ setlocal "item" [ srai ["ITERATE ", getlocal "indices"] ] ],
                  condv "item" [
                     li "$" [],
                     li2 [] [
                       think [setglobal "props" [ srai
                                           [ "REMOVEITEM ", getlocal "item",
                                             " IN ", getglobal "props" ] ] ],
                       think [setlocal "item" [ srai ["NEXT"] ] ],
                       loop
                            ]
                               ]
                
                   ]
              ]

processNoAns = cat [
                pat [ "PROCESSNOANS" ],
                temp [
                think [ setlocal "indices" [""],
                setlocal "cont"    [ srai [ "COUNT ", getglobal "props" ] ] ],

                -- getglobal "askpropidx",
                -- getglobal "askprop",
                think [setlocal "prop" [ srai [ "SELECTITEM ", getglobal "askpropidx", " IN ", srai [ "QUITARESPACIOS ", fruit_ (getlocal "cont") ] ] ] ],

                condv "cont" [
                  li "0" [],
                  li2 [] [
                    condn "askprop" [
                          li2 [] [
                                 val [ getlocal "prop" ], -- "Igual",
                                 think [setlocal "indices" [
                                    getlocal "indices", " ", getlocal "cont" ]
                                                 ]
                                 ],
                          li2 [] [ -- "Justo antes de ADDITEM",
                              -- "Comparando ", getglobal "askprop", " con ", getlocal "prop", " es /="
                                 ]
                          
                          ],
                          think [setlocal "cont" [ mapget "predecessor" (getlocal "cont") ],
                          setlocal "prop" [ srai [ "SELECTITEM ", getglobal "askpropidx", " IN ", srai [ "QUITARESPACIOS ", fruit_ (getlocal "cont") ] ] ] ],
                          loop
                      
                    ]
                  ],

                  -- Eliminar frutas según la respuesta
                  think [ setlocal "item" [ srai ["ITERATE ", getlocal "indices"] ] ],
                  condv "item" [
                     li "$" [],
                     li2 [] [
                       think [setglobal "props" [ srai
                                           [ "REMOVEITEM ", getlocal "item",
                                             " IN ", getglobal "props" ] ],
                       setlocal "item" [ srai ["NEXT"] ] ],
                       loop
                            ]
                               ]

                     ]
                   ]

ansText = ["Si", "no"]

processAns = cat [
              pat ["PROCESSANS ^"],
              temp [

                --  getglobal "bfruitasked",
                condn "bfruitasked" [
                 li2 [] [ val ["y"],
                  think [setlocal "nposs" [ srai ["COUNT ", getglobal "props"] ],
                  setlocal "yn_" [star] ],
                  condv "yn_" [
                            li2 [] [val ["n"],
                  condv "nposs" [
                     li2 [] [val ["1"], " Me estás engañando"],
                     li2 [] []
                     ] ],
                            li2 [] ["Qué bueno soy"]
                            ]
                        ],
                 
                 li2 [] [ val ["n"],
                  think [setlocal "yn" [star] ],
                  
                  condv "yn" [
                      li "y" [ srai ["PROCESSYESANS"] ],
                      li "n" [ srai ["PROCESSNOANS"] ]
                               ]
                    ]
                  ]
                 ]
               ]

makeQuestion = cat [
                 pat ["MAKEQUESTION"],
                 temp [
                    --setlocal "idx" [star_ [index_ 1]],
                    --setlocal "prop" [star_ [index_ 2]],
                    condn "askpropidx" [
                       
                       li "7" [
                          condn "askprop" [
                               li "none" [
                                  "No tiene vitamina ", getglobal "askprop2"
                                         ],
                               li2 [] [
                                  "Tiene vitamina ", getglobal "askprop"
                                      ]
                                          ] 
                              ],
                       li "8" [
                          "Tiene vitamina ", getglobal "askprop"
                              ],
                       li "9" [
                          "Tiene vitamina ", getlocal "askprop"
                              ],
                       li2 [] [
                         condn "askprop" [
                            li "none" [
                              "No es ", getglobal "askprop2"
                                      ],
                            li2 []  [
                                "Es ", getglobal "askprop" 
                                    ]
                                      ]
                              ]
                                ]
                      ]
                   ]

askQuestion = cat [
               pat [ "QUESTION" ],
               temp [
                 think [setlocal "fr1" [ srai [ "QUITARESPACIOS ", fruit 1 ] ],
                 setlocal "fr2" [ srai [ "QUITARESPACIOS ", fruit 2 ] ],
                 setlocal "cont" ["2"],
                 setlocal "prop" [ srai ["SELECTITEM ", getlocal "cont", " IN ", getlocal "fr1"] ],

                 
                 setlocal "nfruits" [ srai [ "COUNT ", getglobal "props" ] ] ],
                 condv "nfruits" [
                       li "1" [
                       -- Base case: just one fruit
                           think [setglobal "askedfruit" [ srai [ "TOP ",
                                         srai [ "QUITARESPACIOS ", getglobal "props" ] ] ],
                           setglobal "bfruitasked" ["y"]],
                           "Es la fruta: ", getglobal "askedfruit"
                           
                              ],
                       li2 [] [
                       -- There is more than one fruit on the list
                         condv "prop" [
                               li2 [] [ val [ srai ["SELECTITEM ", getlocal "cont", " IN ", getlocal "fr2"] ],
                                        think [ setlocal "cont" [ mapget "successor" (getlocal "cont")],
                                        setlocal "prop" [ srai ["SELECTITEM ", getlocal "cont",
                                                                " IN ", getlocal "fr1"] ] ],
                                        -- " QUESTION: igual ",
                                        loop ],
                               li2 [] [ -- "QUESTION: Distinto"
                                      ]
                                      ],
                           think [setglobal "askpropidx" [ getlocal "cont" ],
                           setglobal "askprop" [ getlocal "prop" ],
                           setglobal "askprop2" [ srai ["SELECTITEM ", getlocal "cont", " IN ", getlocal "fr2"] ] ],
                           
                           srai ["MAKEQUESTION "]
                           -- "¿Es ", getlocal "prop", "?"
                              ]
                              ]
                                 ]
                 
                 
                  ]


questions = [ ["Emp+ ^ juego ^ tu ^ fruta"], -- setFruit
              ["^ otra pregunta ^", "^ una pregunta ^"]          -- askQuestion
            ]

--              ["Si", "No"]                   -- processAns

routinesText = ["SETFRUIT",
                "QUESTION"]

questionRoutines = zip questions routinesText


routines = [setFruit, fruitCat, makeQuestion, askQuestion, processYesAns, processNoAns, processAns]

for = flip map

text = foldl (++) [] [
    for qs $ (\x ->
           cat [
            pat [ x ],
            temp [ srai [r] ]
               ]
                )
  | (qs, r) <- questionRoutines ]


ynCat yn ynText = cat [
            pat [ynText],
            temp [ srai ["PROCESSANS ", yn] ]
               ]


doc = routines ++ text ++ [ynCat "y" "Si", ynCat "n" "No"]

main :: IO ()
main = do putStrLn $ renderDocument doc
