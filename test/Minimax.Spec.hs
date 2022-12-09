import Test.Hspec
import Board
import Minimax

main :: IO()
main = hspec $ do
    describe "evaluate" $ do
        it "returns 0 if draw" $ do
            let board = [[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ]
            evaluate board 3 0 `shouldBe` Just 0

        it "returns -50 if X wins" $ do
            let board = [[]
                        ,[]
                        ,[  O,O,O]
                        ,[X,X,X,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            evaluate board 3 0 `shouldBe` Just (-50)

        it "returns 50 if O wins" $ do
            let board = [[]
                        ,[]
                        ,[O,O,O,O]
                        ,[  X,X,X]
                        ,[      X]
                        ,[]
                        ,[]
                        ]
            evaluate board 2 0 `shouldBe` Just 50

    describe "minimax" $ do
        it "returns the column to win the game" $ do
            let board = [[]
                        ,[]
                        ,[O,O,O]
                        ,[X,X,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            minimax board `shouldBe` 2

        it "returns the column to prevent the other player to win the game" $ do
            let board = [[]
                        ,[]
                        ,[  O,O]
                        ,[X,X,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            minimax board `shouldBe` 3

        it "returns the column to prevent the other player to win the game" $ do
            let board = [[]
                        ,[X]
                        ,[X]
                        ,[X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            minimax board `shouldBe` 4