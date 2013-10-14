module C0AMformat where

{-
 - Legacy implementations with trailing '
 -}

import Data.List
import Data.Maybe
import C0Types
import C0AMtypes
import Control.Applicative
import Test.QuickCheck

--import Debug.Trace (trace)
trace _ x = x


-- determines identical Counters
doubles :: [Command] -> [(Counter,Counter)]
doubles [] = []
doubles (a@(a1,a2):b@(b1,b2):cs) = [(a1, b1) | a2 == NOP && b2 == NOP] ++ doubles (b:cs)
doubles (a:bs) = doubles bs


-- unify duplicate counters
--                                 ----->
exchangeCounter :: Command -> (Counter,Counter) -> Command
exchangeCounter (r@(cnt,cmd)) (c@(c1,c2)) | cnt == c1                  = (c2,cmd)
                                          | cmd == JMP c1              = (cnt,JMP c2)
                                          | cmd == JMC c1              = (cnt,JMC c2)
                                          | cnt == c1 && cmd == JMP c1 = (c2,JMP c2)
                                          | cnt == c1 && cmd == JMC c1 = (c2,JMC c2)
                                          | otherwise                  = r


-- apply exchangeCounter on a whole list of [Command]
exchangeCounters :: [Command] -> [(Counter,Counter)] -> [Command]
exchangeCounters rs cs = exchangeCounter <$> rs <*> cs


-- call exchangeCounters repeatedly until the list doesnt change anymore
exchangeAll :: [Command] -> [(Counter,Counter)] -> [Command]
exchangeAll rs [] = rs
exchangeAll rs cs = until (\r -> r == exchangeCounters r cs)
                          (`exchangeCounters` cs)
                          rs


unifyCounters :: [Command] -> [Command]
unifyCounters cmds = exchangeAll cmds (doubles cmds)


-- remove double NOPs with same address
sumNOPs :: [Command] -> [Command]
sumNOPs = foldr f []
              where a `f` b | null b                      = [a]
                            | snd a == NOP && a == head b = b
                            | otherwise                   = a : b
-- DECPRECATED version below
sumNOPs' :: [Command] -> [Command]
sumNOPs' [] = []
sumNOPs' [x] = [x]
sumNOPs' (x:xs)
                | snd x == NOP && x == y = sumNOPs' xs
                | otherwise              = x : sumNOPs' xs
                where y = head xs


-- erase NOPs, shift the address to the next command
eraseNOPs :: [Command] -> [Command]
eraseNOPs = foldr f []
              where a `f` b | null b       = [a]
                            | snd a == NOP = (fst a, snd (head b)) : tail b
                            | otherwise    = a : b
-- DECPRECATED version below
eraseNOPs' :: [Command] -> [Command]
eraseNOPs' [] = []
eraseNOPs' [x] = [x]
eraseNOPs' (x:xs)
                  | snd x == NOP = (fst x, snd y) : eraseNOPs' (tail xs)
                  | otherwise    = x : eraseNOPs' xs
                  where y = head xs


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

linearize :: [Command] -> [(Int,Operation)]
linearize = zip [1..] . map snd

finalize :: [(Int,Operation)] -> String
finalize = intercalate "\n" . map (show . CommandLin)

