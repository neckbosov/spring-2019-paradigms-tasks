{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Data.Maybe
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
merge left Nil           = left
merge Nil right          = right
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

    toAscList Nil                   = []
    toAscList (Node k v left right) = toAscList left ++ [(k, v)] ++ toAscList right

    alter f key Nil                          = let new_v = (f Nothing) in
        if isNothing new_v then Nil else Node key (fromJust new_v) Nil Nil
    alter f key (Node k v left right)  | key < k = Node k v (alter f key left) right
    alter f key (Node k v left right)  | key > k = Node k v left (alter f key right)
    alter f key (Node _ v left right)        = let new_v = (f $ Just v) in
        if isNothing new_v then merge left right else Node key (fromJust new_v) left right

    lookup _ Nil                            = Nothing
    lookup key (Node k _ left _)  | key < k = Map.lookup key left
    lookup key (Node k _ _ right) | key > k = Map.lookup key right
    lookup _ (Node _ v _ _)                 = Just v

    null Nil = True
    null _   = False

    size Nil = 0
    size (Node _ _ left right) = size left + 1 + size right
