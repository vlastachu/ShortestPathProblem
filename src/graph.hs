{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import           Data.Functor     ((<$>))
import           Data.Vector      ((//), (!), fromList, Vector)
import qualified Data.Vector as V
import           Data.List        (find)
import           Data.Maybe       (fromJust, catMaybes, mapMaybe)
import 			 Diagrams.Prelude
import 			 Diagrams.Backend.SVG.CmdLine
import 			 Diagrams.TwoD.Arrow
import qualified Data.Map.Strict as M

minBy f = foldr1 (\x y -> if f x < f y then x else y)

type Weight = Int
type NodeId = Int
type Edge   = (NodeId, Weight)
edgeId      = fst
edgeWeight  = snd
--adjacency list representation - на самом деле это отображение из NodeId в [Edge], но очевидно
--NodeId это Int поэтому для оптимизации используем вектор (который индексируется Int'ом)
type ALP = Vector [Edge] 

buildALP :: [[Weight]] -> ALP
--поле размером n на n 
buildALP field = addLines field 0 (V.replicate (n*n) [] :: ALP) 
    where n = (length field `div` 2) + 1

addLines :: [[Weight]] -> Int -> ALP -> ALP
addLines [] _ alp = alp
addLines (line:lines) lineN alp 
    | even lineN = addHorLine line (realN * (len + 1)) 0 alp'
    | odd  lineN = addVerLine line realN 0 len alp'   -- лишняя проверка для красоты
        where len = length line
              realN = lineN `div` 2 --  в действительности номер строки в 2 раза меньше
              alp'  = addLines lines (lineN + 1) alp

addVerLine :: [Weight] -> Int -> Int -> Int -> ALP -> ALP
addVerLine [] _ _ _ alp = alp
addVerLine (weight: ws) lineN itemN lineLength alp = addWeight weight from to alp'
    where from = lineN * lineLength + itemN
          to   = from + lineLength
          alp' = addVerLine ws lineN (itemN + 1) lineLength alp -- модифицированный alp 
                                           --в котором все остальные веса уже добавлены

addHorLine :: [Weight] -> Int -> Int -> ALP -> ALP
addHorLine [] _ _ alp = alp
addHorLine (weight: ws) beginN itemN alp = addWeight weight from to alp'
    where from = beginN + itemN
          to   = from + 1
          alp' = addHorLine ws beginN (itemN + 1) alp

addWeight :: Weight -> NodeId -> NodeId -> ALP -> ALP
--добавляем веса туда и обратно
addWeight weight from to alp = alp // [(from, (to, weight):(alp ! from)),
                                       (to, (from, weight):(alp ! to))]

beginTraverse :: ALP -> Maybe [NodeId]
beginTraverse alp = snd <$> traverse alp defaultBestPath [] (nodeCount - 1) 0
	where 
		nodeCount = V.length alp
		defaultBestPath = V.replicate nodeCount Nothing // [(0, Just 0)]

traverse :: ALP -> Vector (Maybe Int) -> [NodeId] -> NodeId -> NodeId -> Maybe (Vector (Maybe Int) ,[NodeId])
traverse alp bestPath traversed end current  
    | current == end = Just (bestPath, traversed')
    | null filteredNodes = Nothing -- зашел в тупик. если бы были отрицательные веса то стоило бы рассмотреть этот случай
    | null paths = Nothing -- дальше только тупики
    | otherwise = Just $ minBy fst paths
        where traversed' = current : traversed
              edges = alp ! current
              subTotal = fromJust $ bestPath ! current 
              isBetter node (Just x) = subTotal + edgeWeight node < x --короткий путь лучше длинного
              isBetter _ Nothing = True --что-либо лучше чем ничего
              filtering node = (edgeId node `notElem` traversed) && isBetter node (bestPath ! edgeId node)
              --nodes = edgeId <$> edges
              filteredNodes = filter filtering edges --пройденные не нужны
              paths = catMaybes(map_ currentBestPath (edgeId <$> filteredNodes))
              map_ bestPath_ (node:nodes) = case traverse alp bestPath_ traversed' end node of
              	  Just (bp, p) -> Just (bp, p) : map_ bp nodes
              	  Nothing      -> Nothing : map_ bestPath_ nodes
              map_ _ [] = []
              currentBestPath = bestPath // map (\node -> (edgeId node, Just $ subTotal + edgeWeight node)) filteredNodes

sumWeights :: ALP -> [NodeId] -> Weight
sumWeights _    []  = 0
sumWeights _ (_:[]) = 0
sumWeights alp (current:(next:other)) = weightBetween alp current next + sumWeights alp (next:other)

weightBetween alp x y = case find (\ edge -> y == edgeId edge) edges of
		Just weight -> edgeWeight weight
		Nothing     -> error $ show (x,y)
    where edges = alp ! x

showWeight alp x y = show $ weightBetween alp x y

node :: Int -> Int -> Diagram B R2
node i x = (nodeText i <> circle 1)  #lw 0.05 #fc lightblue #named (show i) #moveTo pt
	where pt = p2 (1, 1) # translate (4 * fromIntegral x  ^& 0)

nodeText i = text (show i) #fc darkslateblue 
						   #font "Quartz MS" 
						   #translate (0 ^& 0) 

label i txt = text txt #scaleX 0.8 
					   #scaleY 0.8 
					   #translate ((-0.7 + 4 * fromIntegral i) ^& 1.5)

verLabel i n txt = text txt #scaleX 0.8 
							#scaleY 0.8 
							#translate ((0.2 + 4 * fromIntegral (i `mod` n)) ^& (-1  + 4 * fromIntegral (i `div` n)))

lineArrow = with  & arrowHead  .~ noHead & headSize .~ 0.6 

redArrow = with & arrowHead .~ spike & headSize .~ 0.9 & headColor .~red 


--(text "1" # fc black # translate (3.5 + 4 * 1 ^& 3.3)) <> 
connectedLines :: ALP -> Int -> Diagram B R2
connectedLines alp n = square (n' + 3)  #translate ((n'/2) ^& (n'/2)) 
				<> foldr (\i sum -> verLabel i n (showWeight alp (i-n) i) <> connectOutside' lineArrow (show (i-n)) (show i) sum # lw 0.05) (linez alp n) [n..(n*n-1)]
	where n' = fromIntegral n*4
linez alp n = foldr (\i sum -> sum <> (connectedLine alp (n*(i-1)) (n*i - 1)  # translate (0 ^& 4 * fromIntegral (i-1)))) (connectedLine alp 0 (n-1)) [2..n]
connectedLine alp beg end =  foldr (\i sum ->connectOutside' lineArrow (show (i-1)) (show i) sum # lw 0.05) (line alp beg end) [(beg +1)..end]
line alp beg end = foldr (\i sum -> sum <> label (i-beg) (showWeight alp (i-1) i) <> node i (i - beg)) (node beg 0) [(1+beg)..end]

showPath :: [NodeId] -> Diagram B R2 -> Diagram B R2
showPath [] _ = square 0
showPath (_:[]) diag = diag
showPath (current:(next:etc)) diag = connectOutside' redArrow (show current) (show next) (showPath (next:etc) diag)

out :: String -> IO (Diagram SVG R2)
out filename = do 
		content <- readFile filename
		let parsed = reverse $ map (map (read::String->Int) . words) (lines content)
		let alp = buildALP parsed
		let path = beginTraverse alp
		case path of
			Just x  -> print (sumWeights alp x, x)
			Nothing -> print "there is no path. bad graph"
		return $ showPath (reverse $ fromJust path) $ connectedLines alp (1 + length parsed `div` 2)
--main = print (sumWeights alp1 <$> beginTraverse alp1)


main = mainWith out