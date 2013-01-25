

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq)

data Inbalance = Balanced | LL | RR | LR | RL deriving (Show, Eq)

(+++) :: (Eq a, Ord a) => Tree a -> a -> Tree a
(+++) = insert

instance (Show a, Eq a) => Show (Tree a) where
    show tree = draw tree (height tree)
        where
            h = height tree
            draw tree 0 = drawLevel tree (h)
            draw tree n =  drawLevel tree (h - n) ++ "\n" ++ drawLine tree (h - n) ++ "\n" ++ draw tree (n - 1)
            drawNode a = case length (show a) of
                1 -> " " ++ show a ++ " "
                2 -> " " ++ show a
                _ -> show a

            drawLevel Nil _ = ""
            drawLevel (Node a l r) 0 = replicate (width l) ' ' ++ drawNode a ++ replicate (width r) ' '
            drawLevel (Node a l r) n = drawLevel l (pred n) ++ "   " ++ drawLevel r (pred n)

            drawLine (Nil) _ = ""
            drawLine (Node a l r) 0 = 
                replicate (leftSpace l) ' ' ++
                replicate (leftLine l) '-' ++
                midLine ++
                replicate (rightLine r) '-' ++
                replicate (rightSpace r) ' '
                where midLine
                        | l /= Nil || r /= Nil = "-"
                        | otherwise = " "

            drawLine (Node a l r) n = drawLine l (pred n) ++ "   " ++ drawLine r (pred n)

            leftSpace Nil = 1
            leftSpace (Node _ l _) = width l + 1

            leftLine Nil = 0
            leftLine (Node _ _ r) = width r + 3

            rightLine Nil = 0
            rightLine (Node _ l _) = width l + 3

            rightSpace Nil = 1
            rightSpace (Node _ _ r) = width r + 1

width Nil = 0
width (Node a l r) = 3 + width l + width r

search :: (Eq a, Ord a) => Tree a -> a -> Bool
search Nil _ = False
search (Node a l r) b
    | a == b = True
    | a >  b = search l b
    | a <  b = search r b

height :: Tree a -> Int
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

balanced :: Tree a -> Inbalance
balanced Nil = Balanced
balanced (Node _ Nil r)      
    | height r < 2 = Balanced
    | height r >= 2 && height rl > height rr = RL
    | height r >= 2 && height rr > height rl = RR
    where (Node _ rl rr) = r
balanced (Node _ l Nil) 
    | height l < 2 = Balanced
    | height l >= 2 && height ll > height lr = LL
    | height l >= 2 && height lr > height ll = LR
    where (Node _ ll lr) = l
balanced (Node _ (Node a ll lr) (Node b rl rr))
    | difference heightL heightR < 2 = Balanced
    | heightL - heightR > 1 && heightLL > heightLR = LL
    | heightL - heightR > 1 && heightLR > heightLL = LR
    | heightR - heightL > 1 && heightRR > heightRL = RR
    | heightR - heightL > 1 && heightRL > heightRR = RL
    where   difference a b = abs (a - b)
            heightL = height (Node a ll lr)
            heightR = height (Node b rl rr)
            heightLL = height ll
            heightLR = height lr
            heightRR = height rr
            heightRL = height rl

insert :: (Eq a, Ord a) => Tree a -> a -> Tree a
insert Nil a = Node a Nil Nil
insert (Node a l r) b
    | a == b = balance $ Node a   l               r
    | a >  b = balance $ Node a   (insert l b)    r
    | a <  b = balance $ Node a   l               (insert r b)

balance :: Tree a -> Tree a
balance Nil     = Nil
balance tree    = case balanced tree of
    Balanced    -> Node a (balance l) (balance r)
    LL          -> rotateLL $ Node a (balance l) (balance r)
    LR          -> rotateLR $ Node a (balance l) (balance r)
    RR          -> rotateRR $ Node a (balance l) (balance r)
    RL          -> rotateRL $ Node a (balance l) (balance r)
    where (Node a l r) = tree

rotateLL (Node x (Node y l b) a) = (Node y l (Node x b a))  
rotateRR (Node x a (Node y b r)) = (Node y (Node x a b) r)  
rotateLR (Node x (Node y b (Node c l r)) a) = (Node c (Node y b l) (Node x r a))
rotateRL (Node x a (Node y (Node c l r) b)) = (Node c (Node x a l) (Node y r b))

ll = Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 7 Nil Nil)
rr = Node 6 (Node 5 Nil Nil) (Node 8 (Node 7 Nil Nil) (Node 10 (Node 9 Nil Nil) (Node 11 Nil Nil))) 
lr = Node 6 (Node 2 (Node 1 Nil Nil) (Node 4 (Node 3 Nil Nil) (Node 5 Nil Nil))) (Node 7 Nil Nil)
rl = Node 6 (Node 5 Nil Nil) (Node 10 (Node 8 (Node 7 Nil Nil) (Node 9 Nil Nil)) (Node 11 Nil Nil))
