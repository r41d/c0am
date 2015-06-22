{-# LANGUAGE TemplateHaskell #-}
module C0AMformat where

import Data.List
import Data.Maybe
import Control.Applicative

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import C0Types
import C0AMtypes

c0amFormatQuickCheckProperties = $(testGroupGenerator)

-- determines identical Counters
doubles :: [Command] -> [(Counter,Counter)]
doubles [] = []
doubles (a@(a1,a2):b@(b1,b2):cs) = [(a1, b1) | a2 == NOP && b2 == NOP] ++ doubles (b:cs)
doubles (a:bs) = doubles bs


-- unify duplicate counters        /---->
exchangeCounter :: Command -> (Counter,Counter) -> Command
exchangeCounter (r@(cnt,cmd)) (c@(c1,c2)) | cnt == c1                  = (c2,cmd)
                                          | cmd == JMP c1              = (cnt,JMP c2)
                                          | cmd == JMC c1              = (cnt,JMC c2)
                                          | cnt == c1 && cmd == JMP c1 = (c2,JMP c2)
                                          | cnt == c1 && cmd == JMC c1 = (c2,JMC c2)
                                          | otherwise                  = r

-- call exchangeCounters repeatedly until the list doesnt change anymore
exchangeAll :: [Command] -> [(Counter,Counter)] -> [Command]
exchangeAll rs [] = rs
exchangeAll rs cs = until (\r -> r == exchangeCounters r cs)
                          (`exchangeCounters` cs)
                          rs
  where
    exchangeCounters rs cs = exchangeCounter <$> rs <*> cs
    -- apply exchangeCounter on a whole list of [Command]

unifyCounters :: [Command] -> [Command]
unifyCounters cmds = exchangeAll cmds (doubles cmds)


-- erase NOPs, shift the address to the next command
eraseNOPs :: [Command] -> [Command]
eraseNOPs [] = []
eraseNOPs [x] = [x]
eraseNOPs xs = foldr f [] xs
              where a `f` b | null b       = [a]
                            | snd a == NOP = (fst a, snd (head b)) : tail b
                            | otherwise    = a : b


genLinList :: [Command] -> [(Counter,Int)]
genLinList rs = genLinList' rs 1
    where genLinList' :: [Command] -> Int -> [(Counter,Int)]
          genLinList' []               n = []
          genLinList' (r@(cnt,cmd):rs) n = [(cnt, n) | cnt /= E] ++ genLinList' rs (n+1)


adjustJumps :: [Command] -> [Command]
adjustJumps cmds = adjustJumps' cmds (genLinList cmds)
    where adjustJumps' :: [Command] -> [(Counter,Int)] -> [Command]
          adjustJumps' [] _ = []
          adjustJumps' (r@(cnt,cmd):rsl) ci =
              case cmd of JMP x -> (cnt, JMP (C (fromJust (lookup x ci)) E))
                          JMC x -> (cnt, JMC (C (fromJust (lookup x ci)) E))
                          _     -> r
              : adjustJumps' rsl ci

finalize :: [Command] -> String
finalize c = (intercalate "\n" . map (show . CommandLin)) lin
  where lin = (zip [1..] . map snd) c

