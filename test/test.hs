import HTML
import Test.Tasty
  ( TestTree
  , defaultMain
  , testGroup)

import Test.Tasty.HUnit
  ( (@?=)
  , testCase)


main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [email]


email = testGroup "Email formatting tests"
  [testCase "Doesn't do anything" $ True `compare` True @?= EQ]


emailBlocks =
  [ Email "EU Direct | 0.4-3.4mm Keyless Chuck Universal Electric Grinding Chuck For Dremel Rotary Tool" "Currently out of stock"
  , Email "EU Direct | 6pc HSS Circular Saw Blade Set For Metal & Dremel Rotary Tools" "In stock, usually dispatched in 1 business day"
  , Email "EU Direct | 6mm x 6mm 3 Flute HSS  Aluminium End Mill Cutter Extended CNC Bit " "Only 5 units,dispatched in 1 business day"
  , Email "EU Direct | Mist Coolant Lubrication Spray System Unit For CNC Lathe Milling Machine New" "Only 8 units,dispatched in 1 business day"
  , Email "EU Direct | Mist Coolant Lubrication Spray System Unit For CNC Lathe Milling Machine New" "Only 3 units,dispatched in 1 business day"]
