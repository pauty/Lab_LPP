import Data.Char
import System.IO

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

myLength :: [a] -> Int
myLength [] = 0 
myLength (x:xs) = 1 + myLength xs

safetail1 ::  [a] -> [a]
safetail1 [] = []
safetail1 (x:xs) = xs


safetail2 :: [a] -> [a]
safetail2 xs =  if null xs == True then [] else tail(xs)

safetail3 :: [a] -> [a]
safetail3 xs | null xs == True = []
             | otherwise  = tail(xs)
             
--(||) :: Bool->Bool->Bool
--(||) True _ = True
--(||) _ True = True

--(&&) :: Bool->Bool->Bool
--(&&) a b = if a then a==b else False    

--(&&) :: Bool->Bool->Bool
--(&&) a b = if a then b else False         
                              
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve (x:xs) | even (length (x:xs)) == True = (take len (x:xs), drop len (x:xs))
             | otherwise = ([],[])
             where len = (length(x:xs) `div` 2)
             
                      
euclid :: (Num a, Ord a) => a -> a -> a 
--euclid :: Int -> Int -> Int
euclid m n  | m == 0 = n
            | m < n = euclid (n-m) m 
            | otherwise  = euclid (m-n) n
  
fibo :: Int -> Int
fibo n = fibor 0 1 n  

fibor :: Int -> Int -> Int -> Int
fibor m n k |  k == 0 = m
            |  k == 1 = n
            |  otherwise = fibor n (m+n) (k-1) 
            
sumsquare :: Int
sumsquare = sum (map (^2) [1..100])

-- [(x,y) | x <- [1,2,3], y <- [4,5,6]] single comprehension, two generators
--concat[[(a, b) |b<-[4,5,6]] | a <-[1,2,3]] two comprehensions, single generator

sumsquare2 :: Int
sumsquare2 = sum [ x^2 | x <-[1..100] ]

replicateX :: Int -> a -> [a]
replicateX n x = [ x | c<-[1..n]]  

phyts :: Int-> [(Int,Int,Int)] 
phyts n = [(a,b,c) | a <- [1..n] , b <- [1..n] , c <- [1..n] , (a^2) + (b^2) == c^2]

factors :: Int -> [Int]
factors n = [ x | x <- [1..(n-1)], (mod n x) == 0 ]

perfectlist :: Int -> [Int]
perfectlist n = [ x | x <-[1..n] , x == sum(factors x)]

elim :: Eq a => a -> [a] -> [a]
elim e [] = []
elim e (x:xs) | e == x = xs
              | otherwise =  x : (elim e xs)

permutations :: Eq a => [a] -> [[a]]
permutations xs | null xs = [[]]
                | length xs == 1 = [xs]
                | otherwise = [ y:ys | y<-xs , ys<-permutations(elim y xs) ] 


scalarProduct:: Num a => [a]->[a]->a
scalarProduct xs ys | length xs == length ys = sum [ (xs!!k)*(ys!!k) | k<-[0..length xs -1]]  
                    | otherwise = -999

ord1:: Ord a => [a]->Bool
ord1 [] = True          
ord1 xs = and[ xs!!(k-1)<=xs!!(k) | k<-[1..length xs-1]]


substring :: [Char]->[Char]->Int
substring s [] = -1 
substring [] l = 0
substring s l | iniequal s l = 0
              | otherwise = if y == -1 then -1 else 1 + y
              where y =  substring s (tail l)

iniequal :: [Char]->[Char]->Bool 
iniequal [] _ = True
iniequal xs [] = False
iniequal xs ys = (head(xs)==head(ys)) && (iniequal (tail xs) (tail ys))


substring3 :: [Char]->[Char]->Int->Int  --sorrentino's version
substring3 s l n | null s = 0
                 | null l = -1
                 | otherwise = if (iniequal s l) then n else (substring3 s (tail l) (n+1))

nextwordlen :: [Char]->Int
nextwordlen xs | (null xs) || (isSpace (head xs)) = 0
               | otherwise = 1 + nextwordlen (tail xs)

parole :: [Char]->[[Char]]
parole [] = []
parole xs | not (isSpace (head xs)) = (take nwl xs) : (parole(drop (nwl+1) xs))
          | otherwise = parole (tail xs)
          where nwl = nextwordlen xs


listefin:: [a]->[[a]]
listefin xs | null xs = []
            | otherwise  = (tail xs) : listefin(tail xs)
            
listeini:: [a]->[[a]]
listeini xs = [(reverse(drop n r)) | n<-[1..length xs-1]]
               where r = reverse (xs)
               
listSearch :: Eq a => a->[a]->Int --restituisce posizione prima occorrenza elemento (0 incluso) se presente, -1 altrimenti
listSearch e xs | null xs = -1
                | e == head xs = 0
                | otherwise = if next == -1 then -1 else next + 1
                where next = listSearch e (tail xs)

unico :: Eq a =>  [a]->[a]
unico xs = [x | x<-xs , listSearch x (elim x xs) == -1]

permCoppie :: [(a,b)]->[(a,b)]
permCoppie c = [ (fst x, snd y) | x<-c, y<-c]

applyM :: [(a->b->c)]->[a]->[b]->[c]
applyM fs xs ys | null fs || null xs || null ys = []
                | otherwise = ((head fs) (head xs) (head ys)) : (applyM (tail fs) (tail xs) (tail ys))
                
applyMReal :: [(a->b->c)]->[a]->[b]->[c]
applyMReal fs xs ys = [ f x y | f<-fs, x<-xs, y<-ys] 

applyMS :: [(a->a->c)]->[a]->[a]->[c]
applyMS fs xs ys = [ f x y | f<-fs, x<-xs, y<-ys]  

compose :: [a->a]->a->a
compose fs v = foldl (flip (.)) id fs $ v

applyF :: [(a->a)]->[a]->[a]
applyF fs xs = [ compose fs x | x<-xs]  

composeM :: [(a->b->c)]->[(d->a)]->[(e->b)]->[(d->e->c)]
composeM fs gs hs = [ (\x y -> f (g x) (h y)) | f<-fs , g<-gs, h<-hs]

composeMS :: [(a->a->b)]->[(c->a)]->[(c->b)]
composeMS fs gs = [ (\x -> f (g1 x) (g2 x)) | f<-fs, g1<-gs, g2<-gs]

goodMatrix :: Num a => [[a]] -> Bool
goodMatrix [] = True
goodMatrix (x:xs) | null xs = True
                  | otherwise = (length x == length (head xs)) && goodMatrix (tail xs)   

toColoumn :: Int->[[a]]->[a]
toColoumn c xs = [ (x!!c) | x<-xs]   

multRC :: Num a => [a]->[a]->a
multRC xs ys = sum[(xs!!c)*(ys!!c) | c<-[0..length xs-1]]   


multMatrix :: Num a=> [[a]]->[[a]]->[[a]]
multMatrix xs ys = if goodMatrix xs && goodMatrix ys && (length(head xs) == length ys)
                   then [ [ multRC x (toColoumn c ys) | c<-[0..length(head ys)-1] ] | x<-xs ]
                   else [[-1]]    

squareMatrix :: Num  a => [[a]] -> Bool
squareMatrix xs = (length(head xs) == length xs) && goodMatrix xs  

detMatrix :: Num a => [[a]] -> a  
detMatrix xs | length xs == 2 = (((xs!!0)!!0) * ((xs!!)1!!1)) - (((xs!!0)!!1) * ((xs!!1)!!0))
             | otherwise = sum [ ((-1)^j) * ((head xs)!!j) * (detMatrix(rmRC 0 j xs)) | j<-[0..length xs-1]]
   
-- rimuove riga r e colonna j dalla matrice xs; Warning: assume matrice ben formata!             
rmRC :: Num a => Int -> Int -> [[a]] -> [[a]]
rmRC r c xs = [ [(xs!!i)!!j | j<-[0..length(head xs)-1], j /= c ] | i<-[0..length xs-1], i /= r]       

modifica :: Num b => [(a, b)]->Char->[([Char],b)]
modifica xs c = [ (c:[],snd x +1) | x<-xs]  

parenthesis :: [Char]->Bool
parenthesis [] = False
parenthesis xs = (pexpr xs) == []

pexpr :: [Char]->[Char]
pexpr [] = []
pexpr (x:xs) | x == '(' = if not(null result) && head(result) == ')' then pexpr(safetail1 result) else "no" 
             | x == ')' = (x:xs)
             | isSpace x = pexpr xs
             where result = pexpr xs
             
--foldr1 :: (a->a->a)->[a]->a         
--foldr1 f [x] = x
--foldr1 f (x:xs) = f x (foldr1 f xs)
 
myMaximum :: Ord a => [a]->a   
myMaximum xs = foldr1 max xs

myReverse :: [a]->[a]
myReverse xs = foldl (\xs x-> x:xs) [] xs

--myReverse1 :: [a]->[a]
--myReverse1 xs = foldr (\x xs-> xs : x) [] xs

myProduct :: Num a => [a]->a
myProduct xs = foldr1 (*) xs

myCurry :: ((a,b)->c) -> (a->b->c)
myCurry = (\f a b -> f (a,b))

myUncurry :: (a->b->c) -> ((a,b)->c)
myUncurry = (\f (a,b) -> f a b)


foldE :: Num a => (a->a->a)->(a->a->a)->[[a]]->a -----------
foldE a m p= foldr a 0 [(foldr m 1 arg) | arg<-p]

dec2int:: [Int]->Int
dec2int xs = foldl (\x y -> (x*10) + y) 0 xs



readLine :: IO String
readLine = do x <- getChar
              if x == '\DEL' then do putLine("\ESC[1D")
                                     readLine
              else if x == '\n' then return []
                   else do xs <- readLine
                           return (x:xs)
                                
mgetLine :: IO String
mgetLine = do x <- getChar
              if x == '\n' then return []
              else
                  do xs <- mgetLine
                     return (x:xs)
              
for :: (Ord a,Show a) => a -> (a->Bool) -> (a->a) -> (a->IO()) -> IO()
for i p f job = if p i then do job i                             
                               for (f i) p f job
                else return ()
                
printSC :: IO ()
printSC = do str<-getLine
             c<-getChar
             putChar('\n')
             selectivePutLine c str
             
putLine :: String->IO ()
putLine [] = putChar('\n')
putLine str = do putChar (head str)
                 putLine (tail str)
                 
toUpperLine :: String->String
toUpperLine [] = []
toUpperLine str = toUpper(head str) : toUpperLine(tail str)
             
selectivePutLine :: Char->String->IO ()
selectivePutLine c [] = putChar('\n')
selectivePutLine c str = if (head str) /= c then do putChar (head str)
                                                    selectivePutLine c (tail str)
                         else selectivePutLine c (tail str)
                         
rF :: IO ()
rF = do filename<-getLine
        txt<-readFile filename
        putLine txt
     
ioFile :: IO ()
ioFile = do putLine "please insert input file name"
            filename_in<-getLine
            putLine "please insert output file name"
            filename_out<-getLine
            txt<-readFile filename_in
            writeFile filename_out (toUpperLine txt)
            putLine ("write in file "++filename_out++" done.")

rKwF :: IO ()
rKwF = do putLine "please insert some text"
          txt<-getLine
          putLine "please insert output file name"
          filename<-getLine
          writeFile filename txt
          
rFI :: IO Int
rFI = do putLine "please insert integers file name"
         filename<-getLine
         integers<-readFile filename
         return (read integers :: Int)
         
factIO :: String -> IO ()
factIO c = do putLine ("Insert n. Enter "++c++" to exit")
              n<-getLine
              if n == c then return ()
              else do print (fac (read n::Int))
                      factIO c                          
                           
anagrammi :: IO [String]
anagrammi = do putLine "Please insert a word"
               w<-getLine
               ws<-readFile "anagrammi.txt"
               return [ x | x<-(parole ws), isAnagramma x w ] 
               
isAnagramma :: String -> String -> Bool
isAnagramma x y | length x /= length y = False
                | otherwise = (rmCharRec x y) == ""
                
rmCharRec :: String-> String -> String
rmCharRec [] x = x
rmCharRec x [] = []
rmCharRec x y = rmCharRec (tail x) (elim (head x) y)
                            
merge :: Ord a => [a]->[a]->[a]
merge xs [] = xs
merge [] xs = xs
merge xs ys = if head(xs) < head(ys) then head(xs) : (merge (tail xs) ys)
              else head(ys) : (merge xs (tail ys))
              
mergeSort :: Ord a => [a]->[a]
mergeSort [x] = [x]
mergeSort xs | length xs == 2 = if xs!!0 < xs!!1 then xs else (reverse xs)
             | otherwise = merge (mergeSort (take l xs)) (mergeSort (drop l xs))
               where l = (length xs) `div` 2
               



