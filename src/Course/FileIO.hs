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

$ runhaskell FileIO.hs "files.txt"
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
main = 
-- getArgs :: IO (List Chars)
--   (<*>) :: Apply f => f (a          ->  b) -> f   a           -> f  b
--                      IO (List Chars -> ()) -> IO (List Chars) -> IO ()
--     (<*> getArgs) :: IO (List Chars -> b) -> IO b
--                      IO (List Chars -> ())                    -> IO ()
--   _undefined <*> getArgs       -- _undefined :: IO (List Chars -> ())

--       run :: Chars -> IO ()
-- (map run) :: List Chars ->  List (IO ())
-- (readFile <$>) :: List Chars -> List (IO Chars)
--  void getArgs
  getArgs >>= \a -> case a of
                     Nil -> putStrLn "no CLAs"
                     h:._ -> run h -- 2015-09-17T0958

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run toc =
  {- readFile toc >>= (\f -> 
     (getFiles . lines) f) >>= \c ->
      printFiles c
   -}

  readFile toc >>= (getFiles . lines) >>= printFiles -- 2015-09-17T1023

  {- do 
      f <- readFile toc
      c <- (getFiles . lines) f
      printFiles c
   -}

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
-- getFiles fs = sequence (getFile <$> fs) -- 2015-09-17T1029
getFiles = sequence . (getFile <$>) -- 2015-09-17T1032
--   error "todo: Course.FileIO#getFiles"

getFile ::
  FilePath
  -> IO (FilePath, Chars)
-- getFile f =
-- --  readFile f >>= \c -> pure (f, c) -- 2015-09-17T1035
-- --  (\c -> (f, c)) <$> readFile f -- 2015-09-17T1041
--   ((,) f) <$> readFile f -- 2015-09-17T1042
getFile = lift2 (<$>) (,) readFile -- 2015-09-17T1044

printFiles ::
  List (FilePath, Chars)
  -> IO ()
--printFiles x = void $ sequence ((\(p, c) -> printFile p c) <$> x) -- 17T1051
--printFiles x = void $ sequence (uncurry printFile <$> x) -- 17T1052
printFiles = void . sequence . (<$>) (uncurry printFile) -- 17T1055
 -- error "todo: Course.FileIO#printFiles"

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp c =
  putStrLn ("The name of the file: " ++ fp) *>
  putStrLn c
--  error "todo: Course.FileIO#printFile"

