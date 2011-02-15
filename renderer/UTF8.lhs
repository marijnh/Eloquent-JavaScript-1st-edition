Copyright (c) 2002, members of the Haskell Internationalisation Working
Group All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
* Neither the name of the Haskell Internationalisation Working Group nor
   the names of its contributors may be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

This module provides lazy stream encoding/decoding facilities for UTF-8,
the Unicode Transformation Format with 8-bit words.

2002-09-02  Sven Moritz Hallberg <pesco@gmx.de>


> module UTF8
>   ( encode, decode,
>     encodeOne, decodeOne,
>   ) where

> import Data.Char (ord, chr)
> import Data.Word (Word8, Word16, Word32)
> import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))



///- UTF-8 in General -///

Adapted from the Unicode standard, version 3.2,
Table 3.1 "UTF-8 Bit Distribution" (excluded are UTF-16 encodings):

  Scalar                    1st Byte  2nd Byte  3rd Byte  4th Byte
          000000000xxxxxxx  0xxxxxxx
          00000yyyyyxxxxxx  110yyyyy  10xxxxxx
          zzzzyyyyyyxxxxxx  1110zzzz  10yyyyyy  10xxxxxx
  000uuuzzzzzzyyyyyyxxxxxx  11110uuu  10zzzzzz  10yyyyyy  10xxxxxx

Also from the Unicode standard, version 3.2,
Table 3.1B "Legal UTF-8 Byte Sequences":

  Code Points         1st Byte  2nd Byte  3rd Byte  4th Byte
    U+0000..U+007F    00..7F
    U+0080..U+07FF    C2..DF    80..BF
    U+0800..U+0FFF    E0        A0..BF    80..BF
    U+1000..U+CFFF    E1..EC    80..BF    80..BF
    U+D000..U+D7FF    ED        80..9F    80..BF
    U+D800..U+DFFF    ill-formed
    U+E000..U+FFFF    EE..EF    80..BF    80..BF
   U+10000..U+3FFFF   F0        90..BF    80..BF    80..BF
   U+40000..U+FFFFF   F1..F3    80..BF    80..BF    80..BF
  U+100000..U+10FFFF  F4        80..8F    80..BF    80..BF



///- Encoding Functions -///

Must the encoder ensure that no illegal byte sequences are output or
can we trust the Haskell system to supply only legal values?
For now I include error case for the surrogate values U+D800..U+DFFF and
out-of-range scalars.

The function is pretty much a transscript of table 3.1B with error checks.
It dispatches the actual encoding to functions specific to the number of
required bytes.

> encodeOne :: Char -> [Word8]
> encodeOne c
>-- The report guarantees in (6.1.2) that this won't happen:
>--   | n < 0       = error "encodeUTF8: ord returned a negative value"
>     | n < 0x0080  = encodeOne_onebyte n8
>     | n < 0x0800  = encodeOne_twobyte n16
>     | n < 0xD800  = encodeOne_threebyte n16
>     | n < 0xE000  = error "encodeUTF8: ord returned a surrogate value"
>     | n < 0x10000       = encodeOne_threebyte n16
>-- Haskell 98 only talks about 16 bit characters, but ghc handles 20.1.
>     | n < 0x10FFFF      = encodeOne_fourbyte n32
>     | otherwise  = error "encodeUTF8: ord returned a value above 0x10FFFF"
>     where
>     n = ord c            :: Int
>     n8 = fromIntegral n  :: Word8
>     n16 = fromIntegral n :: Word16
>     n32 = fromIntegral n :: Word32


With the above, a stream decoder is trivial:

> encode :: [Char] -> [Word8]
> encode = concatMap encodeOne


Now follow the individual encoders for certain numbers of bytes...
          _
         / |  __  ___  __ __
        / ^| //  /__/ // //
       /.==| \\ //_  // //
It's  //  || // \_/_//_//_  and it's here to stay!

> encodeOne_onebyte :: Word8 -> [Word8]
> encodeOne_onebyte cp = [cp]


00000yyyyyxxxxxx -> 110yyyyy 10xxxxxx

> encodeOne_twobyte :: Word16 -> [Word8]
> encodeOne_twobyte cp = [(0xC0.|.ys), (0x80.|.xs)]
>     where
>     xs, ys :: Word8
>     ys = fromIntegral (shiftR cp 6)
>     xs = (fromIntegral cp) .&. 0x3F


zzzzyyyyyyxxxxxx -> 1110zzzz 10yyyyyy 10xxxxxx

> encodeOne_threebyte :: Word16 -> [Word8]
> encodeOne_threebyte cp = [(0xE0.|.zs), (0x80.|.ys), (0x80.|.xs)]
>     where
>     xs, ys, zs :: Word8
>     xs = (fromIntegral cp) .&. 0x3F
>     ys = (fromIntegral (shiftR cp 6)) .&. 0x3F
>     zs = fromIntegral (shiftR cp 12)


000uuuzzzzzzyyyyyyxxxxxx -> 11110uuu 10zzzzzz 10yyyyyy 10xxxxxx

> encodeOne_fourbyte :: Word32 -> [Word8]
> encodeOne_fourbyte cp = [0xF0.|.us, 0x80.|.zs, 0x80.|.ys, 0x80.|.xs]
>     where
>     xs, ys, zs, us :: Word8
>     xs = (fromIntegral cp) .&. 0x3F
>     ys = (fromIntegral (shiftR cp 6)) .&. 0x3F
>     zs = (fromIntegral (shiftR cp 12)) .&. 0x3F
>     us = fromIntegral (shiftR cp 18)



///- Decoding -///

The decoding is a bit more involved. The byte sequence could contain all
sorts of corruptions. The user must be able to either notice or ignore these
errors.

I will first look at the decoding of a single character. The process
consumes a certain number of bytes from the input. It returns the
remaining input and either an error and the index of its occurance in the
byte sequence or the decoded character.

> data Error

The first byte in a sequence starts with either zero, two, three, or four
ones and one zero to indicate the length of the sequence. If it doesn't,
it is invalid. It is dropped and the next byte interpreted as the start
of a new sequence.

>     = InvalidFirstByte

All bytes in the sequence except the first match the bit pattern 10xxxxxx.
If one doesn't, it is invalid. The sequence up to that point is dropped
and the "invalid" byte interpreted as the start of a new sequence. The error
includes the length of the partial sequence and the number of expected bytes.

>     | InvalidLaterByte Int      -- the byte at relative index n was invalid

If a sequence ends prematurely, it has been truncated. It dropped and
decoding stops. The error reports the actual and expected lengths of the
sequence.

>     | Truncated Int Int         -- only n of m expected bytes were present

Some sequences would represent code points which would be encoded as a
shorter sequence by a conformant encoder. Such non-shortest sequences are
considered erroneous and dropped. The error reports the actual and
expected number of bytes used.

>     | NonShortest Int Int       -- n instead of m bytes were used

Unicode code points are in the range of [0..0x10FFFF]. Any values outside
of those bounds are simply invalid.

>     | ValueOutOfBounds

There is no such thing as "surrogate pairs" any more in UTF-8. The
corresponding code points now form illegal byte sequences.

>     | Surrogate
>       deriving (Show, Eq)


Second, third, and fourth bytes share the common requirement to start
with the bit sequence 10. So, here's the function to check that property.

> first_bits_not_10 :: Word8 -> Bool
> first_bits_not_10 b
>     | (b.&.0xC0) /= 0x80  = True
>     | otherwise           = False


Erm, OK, the single-character decoding function's return type is a bit
longish. It is a tripel:

 - The first component contains the decoded character or an error
   if the byte sequence was erroneous.
 - The second component contains the number of bytes that were consumed
   from the input.
 - The third component contains the remaining bytes of input.

> decodeOne :: [Word8] -> (Either Error Char, Int, [Word8])
> decodeOne bs@(b1:rest)
>     | b1 < 0x80   = decodeOne_onebyte bs
>     | b1 < 0xC0   = (Left InvalidFirstByte, 1, rest)
>     | b1 < 0xE0   = decodeOne_twobyte bs
>     | b1 < 0xEE   = decodeOne_threebyte bs
>     | b1 < 0xF5   = decodeOne_fourbyte bs
>     | otherwise   = (Left ValueOutOfBounds, 1, rest)
> decodeOne [] = error "UTF8.decodeOne: No input"


0xxxxxxx -> 000000000xxxxxxx

> decodeOne_onebyte :: [Word8] -> (Either Error Char, Int, [Word8])
> decodeOne_onebyte (b:bs) = (Right (cpToChar b), 1, bs)
> decodeOne_onebyte[] = error "UTF8.decodeOne_onebyte: No input (can't happen)"

> cpToChar :: Integral a => a -> Char
> cpToChar = chr . fromIntegral


110yyyyy 10xxxxxx -> 00000yyyyyxxxxxx

> decodeOne_twobyte :: [Word8] -> (Either Error Char, Int, [Word8])
> decodeOne_twobyte (_:[])
>     = (Left (Truncated 1 2), 1, [])
> decodeOne_twobyte (b1:b2:bs)
>     | b1 < 0xC2            = (Left (NonShortest 2 1), 2, bs)
>     | first_bits_not_10 b2 = (Left (InvalidLaterByte 1), 1, (b2:bs))
>     | otherwise            = (Right (cpToChar result), 2, bs)
>     where
>     xs, ys, result :: Word32
>     xs = fromIntegral (b2.&.0x3F)
>     ys = fromIntegral (b1.&.0x1F)
>     result = shiftL ys 6 .|. xs
> decodeOne_twobyte[] = error "UTF8.decodeOne_twobyte: No input (can't happen)"


1110zzzz 10yyyyyy 10xxxxxx -> zzzzyyyyyyxxxxxx

> decodeOne_threebyte :: [Word8] -> (Either Error Char, Int, [Word8])
> decodeOne_threebyte (_:[])   = threebyte_truncated 1
> decodeOne_threebyte (_:_:[]) = threebyte_truncated 2
> decodeOne_threebyte bs@(b1:b2:b3:rest)
>     | first_bits_not_10 b2
>         = (Left (InvalidLaterByte 1), 1, drop 1 bs)
>     | first_bits_not_10 b3
>         = (Left (InvalidLaterByte 2), 2, drop 2 bs)
>     | result < 0x0080
>         = (Left (NonShortest 3 1), 3, rest)
>     | result < 0x0800
>         = (Left (NonShortest 3 2), 3, rest)
>     | result >= 0xD800 && result < 0xE000
>         = (Left Surrogate, 3, rest)
>     | otherwise
>         = (Right (cpToChar result), 3, rest)
>     where
>     xs, ys, zs, result :: Word32
>     xs = fromIntegral (b3.&.0x3F)
>     ys = fromIntegral (b2.&.0x3F)
>     zs = fromIntegral (b1.&.0x0F)
>     result = shiftL zs 12 .|. shiftL ys 6 .|. xs
> decodeOne_threebyte[]
>  = error "UTF8.decodeOne_threebyte: No input (can't happen)"

> threebyte_truncated :: Int -> (Either Error Char, Int, [Word8])
> threebyte_truncated n = (Left (Truncated n 3), n, [])


11110uuu 10zzzzzz 10yyyyyy 10xxxxxx -> 000uuuzzzzzzyyyyyyxxxxxx

> decodeOne_fourbyte :: [Word8] -> (Either Error Char, Int, [Word8])
> decodeOne_fourbyte (_:[])     = fourbyte_truncated 1
> decodeOne_fourbyte (_:_:[])   = fourbyte_truncated 2
> decodeOne_fourbyte (_:_:_:[]) = fourbyte_truncated 3
> decodeOne_fourbyte bs@(b1:b2:b3:b4:rest)
>     | first_bits_not_10 b2
>         = (Left (InvalidLaterByte 1), 1, drop 1 bs)
>     | first_bits_not_10 b3
>         = (Left (InvalidLaterByte 2), 2, drop 2 bs)
>     | first_bits_not_10 b4
>         = (Left (InvalidLaterByte 3), 3, drop 3 bs)
>     | result < 0x0080
>         = (Left (NonShortest 4 1), 4, rest)
>     | result < 0x0800
>         = (Left (NonShortest 4 2), 4, rest)
>     | result < 0x10000
>         = (Left (NonShortest 4 3), 4, rest)
>     | result > 0x10FFFF
>         = (Left ValueOutOfBounds, 4, rest)
>     | otherwise
>         = (Right (cpToChar result), 4, rest)
>     where
>     xs, ys, zs, us, result :: Word32
>     xs = fromIntegral (b4 .&. 0x3F)
>     ys = fromIntegral (b3 .&. 0x3F)
>     zs = fromIntegral (b2 .&. 0x3F)
>     us = fromIntegral (b1 .&. 0x07)
>     result = xs .|. shiftL ys 6 .|. shiftL zs 12 .|. shiftL us 18
> decodeOne_fourbyte[]
>  = error "UTF8.decodeOne_fourbyte: No input (can't happen)"

> fourbyte_truncated :: Int -> (Either Error Char, Int, [Word8])
> fourbyte_truncated n = (Left (Truncated n 4), n, [])


The decoder examines all input, recording decoded characters as well as
error-index pairs along the way.

> decode :: [Word8] -> ([Char], [(Error,Int)])
> decode bytes = iter 0 [] [] bytes
>     where
>     iter :: Int -> [Char] -> [(Error,Int)] -> [Word8]
>          -> ([Char], [(Error,Int)])
>     iter _ cs es [] = (reverse cs, reverse es)
>     iter idx cs es bs
>         = case decodeOne bs of
>           (Left e, n, rest)  -> iter (idx+n) cs     ((e,idx):es) rest
>           (Right c, n, rest) -> iter (idx+n) (c:cs) es           rest

