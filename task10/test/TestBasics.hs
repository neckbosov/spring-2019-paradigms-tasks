import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [5..] @?= 5

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on infinite list" $
        take 4 (tail' [5..]) @?= [6..9]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' works on infinite list" $
        take' 4 [5..] @?= [5..8]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' works on infinite list" $
        take 4 (drop' 4 [5..]) @?= [9..12]  
    
    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]
        
    , testCase "filter' works on infinite list" $
        take 4 (filter' (\x -> x `mod` 4 == 0) [5..]) @?= [8, 12, 16, 20]
        
    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' can be used for calculating power tower" $
        foldl'' (flip (^)) 1 [2, 3, 4] @?= 4 ^ (3 ^ 2)

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on both infinite lists" $
        take 4 (concat' [3..] [5..]) @?= [3..6]

    , testCase "concat' works when left list is infinite" $
        take 4 (concat' [3..] [5, 6, 7]) @?= [3..6]

    , testCase "concat' works when left list is infinite" $
        take 4 (concat' [3, 4] [8..]) @?= [3, 4, 8, 9]   
        
    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
