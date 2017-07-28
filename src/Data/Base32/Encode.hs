module Data.Base32.Encode
  (
  ) where

import qualified Data.Bits             as BI
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Function         ((&))
import qualified Data.Vector.Storable  as VS
import           Data.Word


convertToVector :: String -> VS.Vector Word8
convertToVector = VS.fromList . BS.unpack . BSC.pack


type CurrentIndex = Int

getNextFiveBits :: VS.Vector Word8 -> CurrentIndex -> (Char, VS.Vector Word8, CurrentIndex)
getNextFiveBits vec curIdx
  | curIdx <= 3 && not (VS.null vec) =
    let
      char =
        charFromFirst5FromWord8 $ flip BI.shiftL curIdx $ VS.head vec
    in
      (char, vec, curIdx + 5)
  | VS.length vec > 2 =
    let
      fst = vec VS.! 0
      snd = vec VS.! 1
      idxSnd = 5 - (7-curIdx)
      char = 'A'
    in
      (char, VS.tail vec, idxSnd)
  | otherwise =
    error "not enough bits"

charFromFirst5FromWord8 :: Word8 -> Char
charFromFirst5FromWord8 byte =
  byte & flip BI.shiftR 3 & toChar
{-
0123 4567
0
0100 0001
0100 0___
0100 0001
____ _xxx
0000 1000

1
0100 0001
_100 00__
1000 0010
____ _xxx
0001 0000

2
0100 0001
__00 000_
0000 0100
____ _xxx
0000 0000

3
0100 0001
___0 0001
0000 1000
____ _xxx
0000 1000

0123 4567 0123 4567
4
0100 0001 0100 0010
____ 0001 0___ ____ idxSnd = 5 - (8 - curIdx) = 1
0001 xxxx xxxx xxx0 fst shiftL by curIdx, snd shiftR 8 - idxSnd
0001 xxxx ____ 0xxx snd shiftL by 3
0001 0xxx           bitwise or fst and snd

0123 4567 0123 4567
5
0100 0001 0100 0010
____ _001 01__ ____ idxSnd = 5 - (8 - curIdx) = 2
001x xxxx xxxx xx01 fst shiftL by curIdx, snd shiftR 8 - idxSnd
001x xxxx ___0 1xxx snd shiftL by 3
0010 1xxx           bitwise or fst and snd

0123 4567 0123 4567
6
0100 0001 0100 0010
____ __01 010_ ____ idxSnd = 5 - (8 - curIdx) = 3
01xx xxxx xxxx x010 fst shiftL by curIdx, snd shiftR 8 - idxSnd
01xx xxxx __01 0xxx snd shiftL by 3
0101 0xxx           bitwise or fst and snd

0123 4567 0123 4567
7
0100 0001 0100 0010
____ ___1 0100 ____ idxSnd = 5 - (8 - curIdx) = 4
1xxx xxxx xxxx 0100 fst shiftL by curIdx, snd shiftR 8 - idxSnd
1xxx xxxx _010 0xxx snd shiftL by 3
1010 0xxx           bitwise or fst and snd
-}



toChar :: Word8 -> Char
toChar  0 = 'A'
toChar  1 = 'B'
toChar  2 = 'C'
toChar  3 = 'D'
toChar  4 = 'E'
toChar  5 = 'F'
toChar  6 = 'G'
toChar  7 = 'H'
toChar  8 = 'I'
toChar  9 = 'J'
toChar 10 = 'K'
toChar 11 = 'L'
toChar 12 = 'M'
toChar 13 = 'N'
toChar 14 = 'O'
toChar 15 = 'P'
toChar 16 = 'Q'
toChar 17 = 'R'
toChar 18 = 'S'
toChar 19 = 'T'
toChar 20 = 'U'
toChar 21 = 'V'
toChar 22 = 'W'
toChar 23 = 'X'
toChar 24 = 'Y'
toChar 25 = 'Z'
toChar 26 = '2'
toChar 27 = '3'
toChar 28 = '4'
toChar 29 = '5'
toChar 30 = '6'
toChar 31 = '7'
toChar _  = '='
