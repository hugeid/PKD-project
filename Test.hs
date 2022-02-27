import Test.HUnit

import Game
import Logic
import Visuals


test1 = TestCase $ assertEqual "characterCounts" (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')



tests = []

runtests = runTestTT $ TestList tests