{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= run'
  where
    run' args = case (headOr "" args) of
                   "" -> error "Need an argument"
                   x  -> run x

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run fn = do
  (_, contents) <- getFile fn
  otherContents <- getFiles $ lines contents
  printFiles otherContents


getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles fns = sequence $ map getFile fns

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fn = readFile fn >>= (\content -> return (fn, content))

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles = void . sequence . (map (\x -> uncurry printFile x))

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fn cnts = void $ putStr "============ " >> putStrLn fn >> putStrLn cnts
