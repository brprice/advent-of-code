module Main where

import Data.Sequence (Seq(Empty,(:<|),(:|>)),singleton,adjust)
import qualified Data.Sequence as Seq

import Intcode

data NIC = NIC {mach :: IC
               ,bufIn :: Seq Integer
               ,bufOut :: [Integer]
               }

-- runs one step of the machine, updating the IO buffers
step1 :: NIC -> NIC
step1 nic = case run1 (mach nic) of
  Cont1 m -> nic{mach=m}
  In1 f -> let (i,bi) = case bufIn nic of
                 Empty -> (-1,Empty)
                 x :<| xs -> (x,xs)
           in nic{mach=f i,bufIn=bi}
  Out1 o m -> nic{mach=m,bufOut=o:bufOut nic}
  Halt1 m -> nic{mach=m}

-- runs one step of each machine, piping the buffers appropriately
-- returns the messages sent outside the network also
step :: Seq NIC -> (Seq NIC,[(Int,Integer,Integer)])
step = doBuf . fmap step1
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

-- run, grabbing all out-of-network messages
parta :: Seq NIC -> [(Int,Integer,Integer)]
parta ns = let (ns',oon) = step ns
           in oon ++ parta ns'

main :: IO ()
main = do nic <- readIntcode "../data/day23"
          let network = map (\i -> NIC{mach=nic,bufIn=singleton i,bufOut=[]}) [0..49]
          putStr "part a: first packet sent to 255 is "
          print $ head $ filter (\(d,_,_) -> d == 255) $ parta $ Seq.fromList network
