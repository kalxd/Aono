module XG.Type where

import System.Random

data Kimochi = Ureshi -- 高兴
             | Hutsuu -- 普通
             | Kanashii -- 悲伤
             deriving (Enum, Show)

instance Random Kimochi where
    randomR (x, y) g = (toEnum i, g')
        where pair = (fromEnum x, fromEnum y)
              (i, g') = randomR pair g

    random g = (toEnum r, g')
        where (r, g') = randomR (0, 2) g

randomKimochi :: IO Kimochi
randomKimochi = randomIO
