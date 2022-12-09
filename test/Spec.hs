import Test.Hspec
import Board

main :: IO()
main = hspec $ do
    describe "columnInRange" $ do
        it "returns false if out of range" $
            columnInRange (-1) `shouldBe` False

        it "returns false if out of range" $
            columnInRange 7 `shouldBe` False
            
        it "returns true if in range" $
            columnInRange 3 `shouldBe` True

    describe "noMoreMoves" $ do
        it "returns false if empty board" $
            noMoreMoves emptyBoard `shouldBe` False
        
        it "returns true if full board" $ do
            let board = [[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ,[O,O,O,X,X,X]
                        ,[X,X,X,O,O,O]
                        ]
            noMoreMoves board `shouldBe` True

    describe "checkBoard" $ do
        it "returns false if empty board" $
            checkLine emptyBoard 3 `shouldBe` False

        it "returns true if vertical line at the bottom" $ do
            let board = [[]
                        ,[]
                        ,[  O,O,O]
                        ,[X,X,X,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` True

        it "returns true if vertical line at the top" $ do
            let board = [[]
                        ,[]
                        ,[]
                        ,[X,X,X,X,O,O]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` True

        it "returns false if no horizontal line" $ do
            let board = [[O]
                        ,[X]
                        ,[X]
                        ,[X]
                        ,[O]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` False

        it "returns true if horizontal line left" $ do
            let board = [[  X]
                        ,[O,X]
                        ,[O,X]
                        ,[  X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` True

        it "returns true if horizontal line right" $ do
            let board = [[]
                        ,[]
                        ,[]
                        ,[  X]
                        ,[O,X]
                        ,[O,X]
                        ,[  X]
                        ]
            checkLine board 3 `shouldBe` True

        it "returns true if diagonal right line, from right" $ do
            let board = [[      X]
                        ,[    X,O]
                        ,[  X,X,O]
                        ,[X,O,O,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` True            

        it "returns true if diagonal right line, from left" $ do
            let board = [[      X]
                        ,[    X,O]
                        ,[  X,X,O]
                        ,[X,O,O,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 0 `shouldBe` True

        it "returns true if diagonal right line, from middle" $ do
            let board = [[      X]
                        ,[    X,O]
                        ,[  X,X,O]
                        ,[X,O,O,X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 2 `shouldBe` True

        it "returns true if diagonal left line, from right" $ do
            let board = [[X,O,O,X]
                        ,[  X,X,O]
                        ,[    X,O]
                        ,[      X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 3 `shouldBe` True

        it "returns true if diagonal left line, from right" $ do
            let board = [[X,O,O,X]
                        ,[  X,X,O]
                        ,[    X,O]
                        ,[      X]
                        ,[]
                        ,[]
                        ,[]
                        ]
            checkLine board 0 `shouldBe` True