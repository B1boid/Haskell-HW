module Task5Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Task4 (example)
import Task5 (printScript)

-- | Unit tests for Task5
spec :: Spec
spec = do
  describe "Task5" $ do
    it "Convert HalyavaScript example to JavaScript code" $ do
      printScript example `shouldBe`
        "function main(){\n"<>
        "  var v0=\"Hello!\";\n"<>
        "  var v1=5;\n"<>
        "  var v2=False;\n"<>
        "  v0=(v0)<>\" Your rank is \";\n"<>
        "  while ((v2)==False) {\n"<>
        "    v1=(v1)-1;\n"<>
        "    if ((v1)<0) {\n"<>
        "      v2=True;\n"<>
        "    } else {\n"<>
        "      v0=(v0)<>\"*\";\n"<>
        "    }\n"<>
        "  }\n"<>
        "  return v0\n"<>
        "}"
