{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge Nil r              = r
merge (Node k v ll rl) r = Node k v ll (merge rl r)

{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton k v = Node k v Nil Nil 

    toAscList Nil            = []
    toAscList (Node k v l r) = toAscList l ++ [(k, v)] ++ toAscList r

    alter f k' Nil                     = let v' = f Nothing in
        case v' of Nothing  -> Nil
                   Just val -> Node k' v Nil Nil
    alter f k' (Node k v l r) | k' < k = Node k v (alter f k' l) r
    alter f k' (Node k v l r) | k' > k = Node k v l (alter f k' r)
    alter f k' (Node _ v l r)          = let v' = (f $ Just v) in
        case v' of Nothing  -> merge l r
                   Just val -> Node k' val l r

    lookup _  Nil                     = Nothing
    lookup k' (Node k _ l _) | k' < k = Map.lookup k' l
    lookup k' (Node k _ _ r) | k' > k = Map.lookup k' r
    lookup _  (Node _ v _ _)          = Just v

    null Nil = True
    null _   = False

    size Nil            = 0
    size (Node _ _ l r) = size l + 1 + size r
