module ParserSpec
  ( spec
  ) where

import Test.Hspec (Spec (..), describe, it, shouldBe)
import Parser (Command (..), parseCommand)
import Options.Applicative(ParserResult (..), getParseResult)


spec :: Spec
spec = do
  describe "Wrong commands" $ do
    it "Failure: dirrr" $
      (getParseResult $ parseCommand "dirrr") `shouldBe` Nothing
    it "Failure: cd cd cd" $
      (getParseResult $ parseCommand "cd cd cd") `shouldBe` Nothing
    it "Failure: _" $
      (getParseResult $ parseCommand "-_-") `shouldBe` Nothing

  describe "Parsing 'cd'" $ do
    it "Success: cd tmp" $
      (getParseResult $ parseCommand "cd tmp") `shouldBe` Just (CD "tmp")
    it "Success: cd .." $
      (getParseResult $ parseCommand "cd ..") `shouldBe` Just (CD "..")
    it "Failure: cd" $
      (getParseResult $ parseCommand "cd") `shouldBe` Nothing

  describe "Parsing 'dir'" $ do
    it "Success: dir" $
      (getParseResult $ parseCommand "dir") `shouldBe` Just DIR
    it "Failure: dir tmp" $
      (getParseResult $ parseCommand "dir tmp") `shouldBe` Nothing

  describe "Parsing 'ls'" $ do
    it "Success: ls tmp" $
      (getParseResult $ parseCommand "ls tmp") `shouldBe` Just (LS "tmp")
    it "Failure: ls" $
      (getParseResult $ parseCommand "ls") `shouldBe` Nothing

  describe "Parsing 'mkdir'" $ do
    it "Success: mkdir tmp" $
      (getParseResult $ parseCommand "mkdir tmp") `shouldBe` Just (MKDIR "tmp")
    it "Failure: mkdir" $
      (getParseResult $ parseCommand "mkdir") `shouldBe` Nothing

  describe "Parsing 'cat'" $ do
    it "Success: cat tmp" $
      (getParseResult $ parseCommand "cat tmp") `shouldBe` Just (CAT "tmp")
    it "Failure: cat" $
      (getParseResult $ parseCommand "cat") `shouldBe` Nothing

  describe "Parsing 'touch'" $ do
    it "Success: touch tmp" $
      (getParseResult $ parseCommand "touch tmp") `shouldBe` Just (TOUCH "tmp")
    it "Failure: touch" $
      (getParseResult $ parseCommand "touch") `shouldBe` Nothing

  describe "Parsing 'rmdir'" $ do
    it "Success: rmdir tmp" $
      (getParseResult $ parseCommand "rmdir tmp") `shouldBe` Just (RMDIR "tmp")
    it "Failure: rmdir" $
      (getParseResult $ parseCommand "rmdir") `shouldBe` Nothing

  describe "Parsing 'rm'" $ do
    it "Success: rm tmp" $
      (getParseResult $ parseCommand "rm tmp") `shouldBe` Just (RM "tmp")
    it "Failure: rm" $
      (getParseResult $ parseCommand "rm") `shouldBe` Nothing

  describe "Parsing 'edit'" $ do
    it "Success: edit tmp some" $
      (getParseResult $ parseCommand "edit tmp some") `shouldBe` Just (EDIT "tmp" ["some"])
    it "Success: edit tmp some more text" $
      (getParseResult $ parseCommand "edit tmp some more text")
        `shouldBe` Just (EDIT "tmp" ["some", "more", "text"])
    it "Failure: edit tmp" $
      (getParseResult $ parseCommand "edit tmp") `shouldBe` Nothing
    it "Failure: edit" $
      (getParseResult $ parseCommand "edit") `shouldBe` Nothing

  describe "Parsing 'find'" $ do
    it "Success: find tmp" $
      (getParseResult $ parseCommand "find tmp") `shouldBe` Just (FIND "tmp")
    it "Failure: find" $
      (getParseResult $ parseCommand "find") `shouldBe` Nothing

  describe "Parsing 'info'" $ do
    it "Success: info tmp" $
      (getParseResult $ parseCommand "info tmp") `shouldBe` Just (INFO "tmp")
    it "Failure: info" $
      (getParseResult $ parseCommand "info") `shouldBe` Nothing

  describe "Parsing 'infodir'" $ do
    it "Success: infodir tmp" $
      (getParseResult $ parseCommand "infodir tmp") `shouldBe` Just (INFODIR "tmp")
    it "Failure: infodir" $
      (getParseResult $ parseCommand "infodir") `shouldBe` Nothing

  describe "Parsing 'help'" $ do
    it "Success: help" $
      (getParseResult $ parseCommand "help") `shouldBe` Just HELP
    it "Failure: help me pls" $
      (getParseResult $ parseCommand "help me pls") `shouldBe` Nothing

  describe "Parsing 'exit'" $ do
    it "Success: exit" $
      (getParseResult $ parseCommand "exit") `shouldBe` Just EXIT
    it "Failure: fast exit" $
      (getParseResult $ parseCommand "fast exit") `shouldBe` Nothing

  describe "Parsing 'clr'" $ do
    it "Success: clr" $
      (getParseResult $ parseCommand "clr") `shouldBe` Just CLR
    it "Failure: clr all" $
      (getParseResult $ parseCommand "clr all") `shouldBe` Nothing

  describe "Parsing 'copy'" $ do
    it "Success: copy fileToCopy newFile" $
      (getParseResult $ parseCommand "copy fileToCopy newFile")
        `shouldBe` Just (COPY "fileToCopy" "newFile")
    it "Failure: copy" $
      (getParseResult $ parseCommand "copy") `shouldBe` Nothing
    it "Failure: copy file" $
      (getParseResult $ parseCommand "copy file") `shouldBe` Nothing
