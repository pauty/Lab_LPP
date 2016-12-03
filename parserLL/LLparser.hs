import Data.Char
import System.IO

data Symbol = NT String | T String
              deriving (Show, Eq)

derivationSym = "->"

endSym=(T "$")

epsilonSym=(T "EPS")

errorSym =(T "")

type Production = (Symbol,[Symbol])

type ProductionFirst = (Production,[Symbol])

type Follow = (Symbol,[Symbol])

type TableEntry = (Symbol,Symbol,Production)

callMe :: IO ()
callMe = do f<-readFile("gram2.txt")
            testNT f

testNT :: String -> IO ()
testNT f = do let nt_list = getNonterminalList f
              let prods = getProductions f nt_list
              let t_list = getTerminalList nt_list prods
              let prod_firsts = getAllProductionFirst nt_list prods
              putStr "\n\n"
              printProductionFirstList prod_firsts "\n"
              putStr "\n\n"
              let follows = getAllFollows nt_list prods
              printFollowList follows "\n"
              putStr "\n\n"
              putStrLn "> Testing disjoint first:"
              let error_first = testAllDisjointFirsts nt_list prod_firsts
              if null error_first 
                then putStrLn "OK."
                else printErrorFirst error_first
              putStrLn "\n> Testing disjoint follow:"
              let error_follow = testAllDisjointFollows nt_list prod_firsts follows
              if null error_follow 
                then putStrLn "OK."
                else printErrorFollow error_follow
              let table = generateTable prod_firsts follows
              printTable table "\n"
              putStr "\n\n NOW FEED ME A LINE TO ANALYZE, MORTAL \n"
              input <- getLine
              let input_syms = (stringToSymbolList input t_list)
              let start_sym = nt_list!!0
              analizeString (start_sym:[endSym]) (input_syms++[endSym]) table


generateTable :: [ProductionFirst] -> [Follow] -> [TableEntry]
generateTable [] _ = []
generateTable (p:ps) fws = if nullable pfirst
                           then [ (phead, s, prod) | s<-(elimEpsilonSym pfirst) ] ++ [ (phead, s, prod) | s<-follow_syms ] ++ (generateTable ps fws)
                           else [ (phead, s, prod) | s<-pfirst ] ++ (generateTable ps fws)
                           where phead = fst (fst p)
                                 prod = fst p 
                                 pfirst = snd p
                                 follow_syms = snd(selectSymFollow phead fws)

stringToSymbolList :: String -> [Symbol] -> [Symbol]
stringToSymbolList s tl = strToSymList ws tl
                          where ws = strWords s

strToSymList :: [String] -> [Symbol] -> [Symbol]
strToSymList [] _ = [] --error
strToSymList [x] tl = if containSym (T x) tl then [(T x)] else []
strToSymList (x:xs) tl = if containSym (T x) tl then (T x) : (strToSymList xs tl) else []

analizeString :: [Symbol] -> [Symbol] -> [TableEntry] -> IO ()
analizeString [] [] _ = putStrLn "\nAnalisis Successful!"
analizeString stack input table | (head stack) == epsilonSym = analizeString (tail stack) input table
                                | (head stack) == (head input) = do putStr "\nSTACK: "
                                                                    printSymList stack " " []
                                                                    putStr "\tINPUT: "
                                                                    printSymList input " " []
                                                                    putStr "\tACTION: matched "
                                                                    printSym (head input) "'"
                                                                    analizeString (tail stack) (tail input) table
                                | not (isNT (head stack)) = do putStr " !! Error - Unexpected symbol "
                                                               printSym (head input) "'"
                                                               putStr "\nAnalisis Interrupted.\n"
                                | not (null valid_entry) = do putStr "\nSTACK: "
                                                              printSymList stack " " []
                                                              putStr "\tINPUT: "
                                                              printSymList input " " []
                                                              putStr "\tACTION: chosen "
                                                              printProduction (trd3(head valid_entry))
                                                              putStr "\n"
                                                              analizeString ((snd(trd3(head valid_entry))++(tail stack))) input table
                                | otherwise = do putStr "\nSTACK: "
                                                 printSymList stack " " []
                                                 putStr "\tINPUT: "
                                                 printSymList input " " []
                                                 putStrLn " !! Error - Unexpected symbol tempor "
                                where valid_entry = [ x | x<-table , (fst3 x)==(head stack) , (snd3 x)==(head input)] --replace

{-
stringToSymbolList :: String -> [Symbol] -> [Symbol] -> [(Bool,Symbol)]
stringToSymbolList [] _ _ = []
stringToSymbolList (x:xs) ntl tl | isSpace x || x=='\t' || x=='\n' = stringToSymbolList xs ntl tl 
                                 | otherwise = if sym /= errorSym
                                               then (True,sym) : (stringToSymbolList (drop symbolLenght sym) ntl tl)
                                               else (False,sym) : (stringToSymbolList (drop symbolLenght sym) ntl tl)
                                 where sym = readSymbol (x:xs) [] ntl tl

readSymbol :: String -> String -> [Symbol] -> [Symbol] -> Symbol
readSymbol [] _ _ _ = errorSym
readSymbol (x:xs) readstr | containSym 
-} 

selectSymFollow :: Symbol -> [Follow] -> Follow
selectSymFollow sym [] = (sym, []) -- error, should never be reached
selectSymFollow sym (f:fs) = if sym==(fst f) then f else selectSymFollow sym fs

--take the list of all Nonterminal and all ProductionsFirsts, and return pair of 
testAllDisjointFirsts :: [Symbol] -> [ProductionFirst] -> [(ProductionFirst,ProductionFirst)]
testAllDisjointFirsts [] _ = []
testAllDisjointFirsts ntl pfirsts = (testDisjointFirst sympfst) ++ (testAllDisjointFirsts (tail ntl) pfirsts)
                          where sympfst = [ p | p<-pfirsts, (fst(fst p)) == (head ntl) ]

--take all production firsts of a specific NT A and verifies if 
testDisjointFirst :: [ProductionFirst] -> [(ProductionFirst,ProductionFirst)]
testDisjointFirst pfs = [ ( (pfs!!i1), (pfs!!i2) ) | i1<-[0..(length pfs)-1] , i2<-[i1..(length pfs)-1], i1/=i2, not ( disjointSymSets (snd(pfs!!i1)) (snd(pfs!!i2)) ) ]

--take list of all Nonterminals, all PrdouctionFirsts and all Follows 
testAllDisjointFollows :: [Symbol] -> [ProductionFirst] -> [Follow] -> [([ProductionFirst],Follow)]
testAllDisjointFollows [] _ _ = []
testAllDisjointFollows ntl pfirsts fws = if null (fst test) 
                                         then (testAllDisjointFollows (tail ntl) pfirsts fws)
                                         else test : (testAllDisjointFollows (tail ntl) pfirsts fws)
                                         where test = testDisjointFollow sympfst fw
                                               sympfst = [ p | p<-pfirsts, (fst(fst p)) == (head ntl) ]
                                               fw = head [ f | f<-fws , (fst f)==(head ntl)]

--take all production first of a specific NT and its follow 
testDisjointFollow :: [ProductionFirst] -> Follow -> ([ProductionFirst],Follow)
testDisjointFollow pfs fw = ( prodsf, fw )
                            where prodsf =[ (pfs!!i2) | i1<-[0..(length pfs)-1], i2<-[0..(length pfs)-1], nullable (snd (pfs!!i1)) , i1/=i2 , not (disjointSymSets (snd(pfs!!i2)) (snd fw)) ]

disjointSymSets :: [Symbol] -> [Symbol] -> Bool
disjointSymSets sym1 sym2 = (length [ s | s<-sym1 , containSym s sym2]) == 0

--FUNZIONI DI STAMPA

printTable :: [TableEntry] -> String -> IO()
printTable [] _ = return ()
printTable [t] _ = printTableEntry t
printTable (t:ts) separator = do printTableEntry t
                                 putStr separator
                                 printTable ts separator

printTableEntry :: TableEntry -> IO()
printTableEntry t = do putStr "( "
                       printSym  (fst3 t) []
                       putStr ", "
                       printSym (snd3 t) "'"
                       putStr ", "
                       printProduction (trd3 t)
                       putStr " )"

printErrorFirst :: [(ProductionFirst,ProductionFirst)] -> IO()
printErrorFirst [] = return ()
printErrorFirst (e:es) = do  putStr " !! Error - two productions of nonterminal "
                             printSym (fst (fst (fst e))) []
                             putStrLn" don't have disjoint first sets:"
                             printProductionFirst (fst e)
                             putChar '\n'
                             printProductionFirst (snd e)
                             putChar '\n'
                             printErrorFirst es

printErrorFollow :: [([ProductionFirst],Follow)] -> IO()
printErrorFollow [] = return ()
printErrorFollow (e:es) = do putStr " !! - Error: "
                             printSym (fst (snd e)) []
                             putStrLn " is nullable but its follow and first aren't disjoint sets: "
                             printProductionFirstList (fst e) "\n"
                             putStr "\n"
                             printFollow (snd e)
                             putStr "\n"
                             printErrorFollow es 


printProductionFirstList :: [ProductionFirst] -> String -> IO()
printProductionFirstList [] _ = return ()
printProductionFirstList [p] separator = printProductionFirst p
printProductionFirstList (x:xs) separator = do printProductionFirst x
                                               putStr separator
                                               printProductionFirstList xs separator

printProductionFirst :: ProductionFirst -> IO()
printProductionFirst pf = do printProduction (fst pf) 
                             putStr "\t\t { "
                             printSymList (snd pf) ", " "'"
                             putStr " }"

printProduction :: Production -> IO ()
printProduction p = do printSym (fst p) []
                       putStr " -> "
                       printSymList (snd p) " " []

printFollowList :: [Follow] -> String -> IO()
printFollowList [] _ = return ()
printFollowList [f] _ = printFollow f
printFollowList (f:fs) separator = do printFollow f 
                                      putStr separator
                                      printFollowList fs separator

printFollow :: Follow ->IO()
printFollow f= do putStr "Follow ( "
                  printSym (fst f) []
                  putStr " ) = { "
                  printSymList (snd f) ", " "'"
                  putStr " }"

printSymList :: [Symbol] -> String -> String -> IO()
printSymList [] _ _= return ()
printSymList [s] _ marker = printSym s marker
printSymList (x:xs) separator marker = do printSym x marker
                                          putStr separator
                                          printSymList xs separator marker

printSym :: Symbol -> String -> IO ()
printSym (NT str) marker = putStr (marker++str++marker)
printSym (T str) marker = putStr (marker++str++marker)


--END FUNZIONI DI STAMPA

getTerminalList :: [Symbol] -> [Production] -> [Symbol]
getTerminalList ntl prods =  [ s | prod<-prods, s<-(snd prod), not (isNT s) ]

getAllProductionFirst :: [Symbol] -> [Production] -> [ProductionFirst]
getAllProductionFirst [] _ = []
getAllProductionFirst ntl prods = (calcProductionFirst (filterProd (head ntl) prods) prods) ++ (getAllProductionFirst (tail ntl) prods)

getAllFollows :: [Symbol] -> [Production] -> [Follow] 
getAllFollows [] _ = []
getAllFollows (x:xs) prods = (calcFollow x prods) : (getAllFollows xs  prods)

allFirstTerms :: [ProductionFirst] -> [Symbol]
allFirstTerms f = concat[ snd x | x<-f ]

--gets productions of a specific NT, and all of the productions (for the recursive call)
calcProductionFirst :: [Production] -> [Production] -> [ProductionFirst]
calcProductionFirst symprods prods = [ (x, elimDuplicateSym(calcFirst (snd x) prods)) | x<-symprods ] 

calcFirst :: [Symbol] -> [Production] -> [Symbol]
calcFirst [] prods = [epsilonSym]
calcFirst prodsyms prods | not (isNT(first_sym)) = [first_sym] 
                         | otherwise = if nullable tmp_first 
                                       then elimEpsilonSym(tmp_first) ++ (calcFirst (tail prodsyms) prods)
                                       else tmp_first
                          where tmp_first = allFirstTerms ( calcProductionFirst (filterProd first_sym prods) prods )
                                first_sym = head prodsyms

-- returns true if array of symobols syms contains epsilon symbol
nullable :: [Symbol] -> Bool
nullable syms = containSym epsilonSym syms

-- take a NT symbol, all of the productions
calcFollow :: Symbol -> [Production] -> Follow
calcFollow sym prods = ( sym, elimDuplicateSym(safeCalcFollow sym prods []) )

elimDuplicateSym :: [Symbol]->[Symbol]
elimDuplicateSym [] = []
elimDuplicateSym syms = elimDupSym syms []

elimDupSym :: [Symbol] -> [Symbol] -> [Symbol]
elimDupSym [] set = set
elimDupSym (x:xs) set = if containSym x set then elimDupSym xs set else elimDupSym xs (x:set)

safeCalcFollow :: Symbol -> [Production] -> [Symbol] -> [Symbol]
safeCalcFollow sym prods bannedsyms | containSym sym bannedsyms = []
                                    | sym==(fst (prods!!0)) = endSym : follow_syms -- test if the sym is the starting symbol: if so, adds $ to its follow
                                    | otherwise =  follow_syms
                                    where prods_with_sym = [x | x<-prods, containSym sym (snd x)]
                                          follow_syms = concat[calcFollowRec sym p prods bannedsyms | p<-prods_with_sym ]

containSym :: Symbol -> [Symbol] -> Bool
containSym sym [] = False
containSym sym (x:xs) = if sym==x then True else containSym sym xs                     

calcFollowRec :: Symbol -> Production -> [Production] -> [Symbol] -> [Symbol]
calcFollowRec sym p prods bannedsyms  | nullable tmp_follow = (safeCalcFollow (fst p) prods ban_syms) ++ (elimEpsilonSym tmp_follow) -- production contains sym B in last position, or prod is of the form A -> aBC with fist C nullable               
                                      | otherwise = tmp_follow
                                      where pbody = snd p
                                            sympos = [ i | i<-[1..(length pbody)] , pbody!!(i-1) == sym ]
                                            tmp_follow = concat[ calcFirst (drop i pbody) prods | i<-sympos ]
                                            ban_syms = sym : bannedsyms 
                           
elimEpsilonSym :: [Symbol] -> [Symbol]
elimEpsilonSym [] = []
elimEpsilonSym (x:xs) | x==epsilonSym = elimEpsilonSym xs
                      | otherwise = x : (elimEpsilonSym xs)

isNT :: Symbol -> Bool
isNT (NT sym) = True
isNT (T sym) = False

filterProd :: Symbol->[Production]->[Production]
filterProd sym prods = [ x | x<-prods, fst(x)==sym]

--wrapper for getProds, calls it on fake, temporary Nonterminal
getProductions :: String->[Symbol]->[Production]
getProductions s ntl = getProds s (NT "") ntl

getProds :: String->Symbol->[Symbol]->[Production]
getProds [] _ _ = []
getProds s nt ntl | null sn = []  
                        | (head sn =='|') = (nt, getProductionBody prodstr ntl): (getProds (consumeLine sn) nt ntl)
                        | otherwise = (newNT, getProductionBody prodstr2 ntl) : (getProds (consumeLine sn) newNT ntl)
                        where sn = consumeEmptyStr s
                              newNT = getNT sn
                              prodstr = tail (take (lineLength sn) sn) --can never be []: sn is at least "|"
                              prodstr2 = drop ((subString derivationSym sn)+(length derivationSym)) (take (lineLength sn) sn)

getProductionBody :: String->[Symbol]->[Symbol]
getProductionBody s ntl = if null ws then [epsilonSym] else getProdBody ws ntl
                          where ws = strWords s 

getProdBody :: [String] -> [Symbol] -> [Symbol]
getProdBody [] ntl = []
getProdBody (w:ws) ntl = if stringIsNT w ntl 
                    then (NT w) : getProdBody ws ntl
                    else (T w) : getProdBody ws ntl

stringIsNT:: String -> [Symbol] -> Bool
stringIsNT s [] = False
stringIsNT s ntl = if (compSym (head ntl) s) then True else stringIsNT s (tail ntl)

compSym :: Symbol -> String -> Bool 
compSym (NT sym) str = sym==str
compSym (T sym) str = sym==str

symbolLenght :: Symbol -> Int ----------------------unused
symbolLenght (NT sym) = length sym
symbolLenght (T sym) = length sym

getNonterminalList :: String->[Symbol]
getNonterminalList [] = []
getNonterminalList s =  if not (null sn) && (head(sn)/='|')
                        then (getNT sn) : (getNonterminalList (consumeLine sn))
                        else getNonterminalList (consumeLine sn)
                        where sn = consumeEmptyStr s

consumeEmptyStr :: String->String
consumeEmptyStr [] = []
consumeEmptyStr (x:xs) | isSpace x || x=='\t' || x=='\n' || x=='\r'  = consumeEmptyStr xs
                       | x=='@' = consumeEmptyStr (consumeComment xs)
                       | otherwise = (x:xs)

consumeComment:: String -> String
cosumeComment [] = []
consumeComment (x:xs) = if x=='@' then xs else consumeComment xs

consumeLine :: String->String
consumeLine [] = []
consumeLine (x:xs) = if x=='\n' then xs else consumeLine xs

lineLength :: String->Int
lineLength [] = 0
lineLength (x:xs) | x=='\n' = 0
                  | next<0 = -1
                  | otherwise = 1 + next 
                  where next = lineLength xs

getNT :: String -> Symbol
getNT s = NT (readNT s [])

readNT :: String->String->String
readNT [] delim = []
readNT (x:xs) delim | isSpace x || x=='\t' || x=='\n' = readNT xs []
                    | x=='-' && null delim = readNT xs "-"
                    | x=='>' && delim=="-" = []
                    | otherwise = x:(delim++(readNT xs []))

------------------

subString :: [Char]->[Char]->Int
subString s [] = -1 
subString [] l = 0
subString s l | iniequal s l = 0
              | otherwise = if y == -1 then -1 else 1 + y
              where y =  subString s (tail l)

iniequal :: [Char]->[Char]->Bool 
iniequal [] _ = True
iniequal xs [] = False
iniequal xs ys = (head(xs)==head(ys)) && (iniequal (tail xs) (tail ys))

nextWordLen :: [Char]->Int
nextWordLen xs | (null xs) || (isSpace (head xs)) = 0
               | otherwise = 1 + nextWordLen (tail xs)

--returns an array of all the strWords in a line 
strWords :: [Char]->[[Char]]
strWords [] = []
strWords xs | head(xs)=='\n' = []
         | not (isSpace (head xs)) = (take nwl xs) : (strWords(drop (nwl+1) xs))
         | otherwise = strWords (tail xs)
         where nwl = nextWordLen xs

fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd3 (_,_,a) = a