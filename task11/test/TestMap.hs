{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Data.Maybe
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Smoke tests" [
            testCase "Test empty" $
                let tr = empty :: m Int String in Map.null tr @?= True
            ,
            testCase "Test singleton" $
                let tr = singleton 6 "a" :: m Int String in 
                (size tr == 1) && member 6 tr && (fromJust (Map.lookup 6 tr) == "a") @?= True
            ,
            testCase "Test fromList" $
                let tr = fromList [(6, "a"), (6, "b")] :: m Int String in 
                (size tr == 1) && member 6 tr && (fromJust (Map.lookup 6 tr) == "b") @?= True
            ,
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
            ,
            testCase "Test insert" $
                let tr = insert 6 "a" empty :: m Int String in 
                (size tr == 1) && member 6 tr && (fromJust (Map.lookup 6 tr) == "a") @?= True
            ,
            testCase "Test insertWith" $
                let tr = insertWith (++) 6 "b" (singleton 6 "a") :: m Int String in 
                (size tr == 1) && member 6 tr && (fromJust (Map.lookup 6 tr) == "ba") @?= True
            ,
            testCase "Test insertWithKey" $
                let tr = insertWithKey (\k new_v old_v -> show k ++ new_v ++ old_v) 6 "b" (singleton 6 "a") :: m Int String in 
                (size tr == 1) && member 6 tr && (fromJust (Map.lookup 6 tr) == "6ba") @?= True
            ,
            testCase "Test delete" $
                let tr = delete 6 (insert 6 "a" empty) :: m Int String in 
                Map.null tr && isNothing (Map.lookup 6 tr) @?= True
            ,
            testCase "Test adjust" $
                let tr = adjust ("b" ++) 6 (singleton 6 "a") :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "ba"
            ,
            testCase "Test adjustWithKey" $
                let tr = adjustWithKey (\k v -> show k ++ v) 6 (singleton 6 "a") :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "6a"
            ,
            testCase "Test update" $
                let tr = update (const Nothing) 6 (singleton 6 "a") :: m Int String in 
                Map.null tr @?= True
            ,
            testCase "Test updateWithKey" $
                let tr = updateWithKey (\k v -> Just $ show k ++ v) 6 (singleton 6 "a") :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "6a"
            ,
            testCase "Test alter" $
                let tr = alter (fmap $ (++) "b") 6 (singleton 6 "a") :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "ba"
            ,
            testCase "Test lookup" $
            let tr = singleton 6 "a" :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "a"
            ,
            testCase "Test lookup" $
            let tr = singleton 6 "a" :: m Int String in 
                fromJust (Map.lookup 6 tr) @?= "a"
            ,
            testCase "Test member" $
            let tr = singleton 6 "a" :: m Int String in 
                member 6 tr @?= True
            ,
            testCase "Test notMember" $
            let tr = singleton 6 "a" :: m Int String in 
                notMember 4 tr @?= True
            ,
            testCase "Test null" $
            let tr = singleton 6 "a" :: m Int String in 
                Map.null tr @?= False
            ,
            testCase "Test size" $
            let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                size tr @?= 3
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
