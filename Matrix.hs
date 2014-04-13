module Matrix (Matrix,(><),fromList,toList,(@@),rows,cols,det)where

import Data.List

data Matrix = Matrix{
    m,n :: Int, --m:行数, n:列数
    elems :: [Double]
}

instance Show Matrix where
    show matrix = show (m matrix)++"*"++show (n matrix)
        ++ "\n[" ++ splitShow (n matrix) (elems matrix) ++ "]"

splitShow n [] = []
splitShow n xs =  concat.map (flip (++) "\n".format.show) $ split n xs where
    format [] = []
    format (c:cs)
        | c == '[' || c == ']' = format cs
        | c == ',' = " , " ++ format cs
        | otherwise = c:format cs
    
(><) :: Int -> Int -> [Double] -> Matrix
(><) m' n' vs = Matrix {m = m', n = n', elems = vs }

fromList :: [[Double]] -> Matrix
fromList xs = Matrix {m = length xs, n = length $ head xs, elems = concat xs}

toList :: Matrix -> [[Double]]
toList mat = split (n mat) (elems mat)

split _ [] = []
split n xs = as:split n bs where
    (as,bs) = splitAt n xs

rows mat = m mat
cols mat = n mat
trans mat = Matrix {m = n mat, n = m mat, elems = map (mat @@) indexs} where
    indexs = [(j,i)| i<-[1..m mat], j<-[1..n mat]]
det mat = case n mat /= m mat of
    True -> undefined
    False -> case n mat of
        1 -> sum $ elems mat
        2 -> mat @@ (1,1) * mat @@ (2,2) - mat @@ (1,2) * mat @@ (2,1)
        n -> sum $ map (flip detSub mat) [(1,j) | j <- [1..n]] 
    
--行列式のための小行列
detSub index mat = coef * det mat' where
    subs = [(k,l) | k <- [1..m mat], l <- [1..n mat], k /= fst index, l /= snd index]
    coef = (-1)^(fst index+snd index)*mat @@ index
    elems' = map (mat @@) subs
    mat' = mat{n = n mat-1, m = m mat-1, elems = elems'}

mat @@ (i,j) = elems mat !! ((i-1)*(n mat)+j-1)