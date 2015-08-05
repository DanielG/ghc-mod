module Warnings (zoo) where

zoo :: [a] -> ()
zoo x = case x of
          [] -> undefined
