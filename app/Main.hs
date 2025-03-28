module Main (main) where

import Text.Printf (printf)

import DayOne


main :: IO ()
main = do
  a <- readInput
  printf "[Part 1] %d\n" (part1 a)
  printf "[Part 2] %d\n" (part2 a)

