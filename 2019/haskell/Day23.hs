module Main where

import Data.Foldable
import Data.Function ((&))
import Data.Maybe (listToMaybe)
import Data.Monoid(Last(Last))
import Data.Sequence (Seq(Empty,(:<|),(:|>)),singleton,adjust)
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Intcode

data NIC = NIC {mach :: IC
               ,bufIn :: Seq Integer
               ,bufOut :: [Integer]
               }

-- runs one step of the machine, updating the IO buffers
-- also returns whether this machine is idle
data Idle1 = IdleBuf -- input buffer is empty, and did no output: may be idle
           | IdleRead -- read on an empty buffer: is idle
           | IdleNo -- not idle: non-empty input buffer or did some output

step1 :: NIC -> (NIC,Idle1)
step1 nic = case run1 (mach nic) of
  Cont1 m -> (nic{mach=m},if null $ bufIn nic then IdleBuf else IdleNo)
  In1 f -> let (i,bi,idle) = case bufIn nic of
                 Empty -> (-1,Empty,IdleRead)
                 x :<| xs -> (x,xs,IdleNo)
           in (nic{mach=f i,bufIn=bi},idle)
  Out1 o m -> (nic{mach=m,bufOut=o:bufOut nic}, IdleNo)
  Halt1 m -> (nic{mach=m}, error "NIC unexpectedly halted")

-- runs one step of each machine, piping the buffers appropriately
-- returns the messages sent outside the network also
-- also returns whether the network was idle this tick: all queues empty and no outputs happened
-- if not, return None, if so, return the set of machines which requested input
-- (Then the defininion of "idle network" for part b will be that there have been a sequence of
-- idle ticks, during which there was a read by each machine)
data Idle = NotIdle | Idle (S.Set Int)
instance Semigroup Idle where
  Idle as <> Idle bs = Idle (as <> bs)
  _ <> _ = NotIdle
instance Monoid Idle where
  mempty = Idle S.empty
step :: Seq NIC -> (Seq NIC,[(Int,Integer,Integer)],Idle)
step ms = let tmp = fmap step1 ms
              ms' = fmap fst tmp
              idles = fmap snd tmp
              (ms'',outs) = doBuf ms'
          in (ms'',outs,idleInfo idles)
  where doBuf' :: NIC -> ([(Int,Integer,Integer)] , NIC)
        doBuf' n = case bufOut n of
          [y,x,d] -> ([(fromInteger d,x,y)],n{bufOut=[]})
          _ -> ([],n)
        doBuf :: Seq NIC -> (Seq NIC,[(Int,Integer,Integer)])
        doBuf ns = let (msgs,ns') = traverse doBuf' ns
                       l = Seq.length ns
                   in foldr (\(d,x,y) ((nics,outOfNetwork)) ->
                               if 0<=d && d < l
                               then (adjust (\n -> n{bufIn=bufIn n :|> x :|> y}) d nics,outOfNetwork)
                               else (nics,(d,x,y) : outOfNetwork))
                            (ns',[]) msgs
        idleInfo :: Seq Idle1 -> Idle
        idleInfo is = zip [0..] (toList is) & foldMap (\(n,i) -> case i of
                                                          IdleBuf -> Idle S.empty
                                                          IdleRead -> Idle $ S.singleton n
                                                          IdleNo -> NotIdle
                                                          )

-- run, grabbing all out-of-network messages
parta :: Seq NIC -> [(Int,Integer,Integer)]
parta ns = let (ns',oon,_) = step ns
           in oon ++ parta ns'


dbg :: Seq NIC -> String
dbg = unwords . map (show.toList.bufIn) . toList

-- run, logging the packets sent to address 255
partb :: Seq NIC -> [(Integer,Integer)]
partb initialNs = go (Last Nothing) S.empty initialNs
  where allMachs = S.fromList [0..length initialNs-1]
        sendNAT x y ns = adjust (\m -> m{bufIn=bufIn m:|>x:|>y}) 0 ns
        go :: Last (Integer,Integer) -> S.Set Int -> Seq NIC -> [(Integer,Integer)]
        go nat idle ns = let (ns',oon,idle1) = step ns
                             idle' = case idle1 of
                               NotIdle -> S.empty
                               Idle is -> S.union idle is
                             nat' = listToMaybe $ reverse
                                  $ map (\(_,x,y) -> (x,y))
                                  $ filter (\(d,_,_) -> d==255) oon
                             nat'' = nat <> Last nat'
                         in if idle' == allMachs
                            then -- know nat' == Nothing, as no outputs happened
                              case nat of
                                Last Nothing -> go nat'' idle' ns'
                                Last (Just (x,y)) -> (x,y) : go nat'' idle' (sendNAT x y ns')
                            else go nat'' idle' ns'

-- elements which are equal to the next element in the list
dups :: Eq a => [a] -> [a]
dups xs = map fst $ filter (uncurry (==)) $ zip xs $ tail xs

main :: IO ()
main = do nic <- readIntcode "../data/day23"
          let network = map (\i -> NIC{mach=nic,bufIn=singleton i,bufOut=[]}) [0..49]
          putStr "part a: first packet sent to 255 is "
          print $ head $ filter (\(d,_,_) -> d == 255) $ parta $ Seq.fromList network

          putStr "part b: first y value the NAT sends twice in a row: "
          print $ head $ dups $ map snd $ partb $ Seq.fromList network
