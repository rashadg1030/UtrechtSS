main :: IO ()
main = putStrLn "Test suite not yet implemented"

repoCabalUrlSpec :: Spec
repoCabalUrlSpec = describe "repoCabalUrl" $ do
    it "should equal issueWantedCabalUrl when passed in issueWantedRepo" $
        repoCabalUrl issueWantedRepo
            `shouldBe` issueWantedCabalUrl
    it "should equal nonExistentCabalUrl when passed in nonExistentRepo" $
        repoCabalUrl nonExistentRepo
            `shouldBe` nonExistentCabalUrl
