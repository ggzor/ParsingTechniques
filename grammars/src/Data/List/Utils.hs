module Data.List.Utils where

eraseInsertAt :: [a] -> [a] -> Int -> Int -> [a] 
eraseInsertAt xs repl erase idx = 
  let (init, tail) = splitAt idx xs
  in  init ++ (repl ++ drop erase tail)