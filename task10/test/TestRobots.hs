import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        died = robot "Died" 40 0
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        
        , testCase "Test for getAttack" $
            getAttack died @?= 40
         
        , testCase "Test for getHealth" $
            getHealth walter @?= 50

        , testCase "Test for setName" $
            setName "kek" walter @?= robot "kek" 50 50
         
        , testCase "Test for setAttack" $
            setAttack 4 walter @?= robot "Walter" 4 50
         
        , testCase "Test for setHealth" $
            setHealth 4 walter @?= robot "Walter" 50 4

        , testCase "Test for isAlive True" $
            isAlive walter @?= True
        
        , testCase "Test for isAlive False" $
            isAlive died @?= False

        , testCase "Test for fight" $
            fight walter died @?= robot "Died" 40 (-50)

        , testCase "Test for fight died attacker" $
            fight died walter @?= walter

        , testCase "Test for multiRoundFight" $
            multiRoundFight 5 walter died @?= (walter, robot "Died" 40 (-150))

        , testCase "Test for maxByHealth" $
            maxByHealth walter died @?= walter

        , testCase "Test for threeRoundFight" $
            threeRoundFight walter died @?= walter
        ]
