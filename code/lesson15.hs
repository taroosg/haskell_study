data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = div alphabetSize 2
        -- 移動する数を算出
        offset = fromEnum c + halfAlphabet
        -- 何番目か，がリストの範囲外になるので剰余を計算して収める
        rotation = mod offset alphabetSize

largestChaNumber :: Int
largestChaNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEmcrypt = rotN sizeOfAlphabet charToEmcrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where rot4l = rotN alphaSize
        alphaSize = 1 + fromEnum (maxBound ::  FourLetterAlphabet)

data ThreeLetterAlphabet = Alpha | Bete | Kappa deriving (Show, Enum, Bounded)
threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Bete,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where rot3l = rotN alphaSize
        alphaSize = 1 + fromEnum (maxBound ::  ThreeLetterAlphabet)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where rotation = mod offset n
        halfN  = div n 2
        offset = if even n
          then fromEnum c + halfN
          else 1 + fromEnum c + halfN

threeLettedecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLettedecoder vals = map rot3ldecoder vals
  where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
        rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where rotChar = rotN alphaSize
        alphaSize = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where rotCharDecoder = rotNdecoder alphaSize
        alphaSize = 1 + fromEnum (maxBound :: Char)

-- 2つの値をxorする関数
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

-- 値のペアをxorする関数
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (reminder == 0)
  then False : intToBits' nextVal
  else True : intToBits' nextVal
  where reminder = mod n 2
        nextVal = div n 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where trueLocations = filter (\x -> fst x == True) (zip bits indices)
        indices = [size-1, size-2 .. 0]
        size = length bits

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair-> (fst pair) `xor` (snd pair)) (zip padBits plaintextBits)
  where padBits = map charToBits pad
        plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
  where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])


main = do
  print(encode myOTP "Ldcqj%Nf{bog`")