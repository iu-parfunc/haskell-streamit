

module Examples.Hello.HelloWorld  where

import Language.StreamIt

intSource :: Filter Void Int ()
intSource = do
  x <- int
  init' $ do
    x <== 0

  work Rate {pushRate=1, popRate=0, peekRate=0} $ do
    (.++)x
    push(ref x)

incr :: Filter Int Int ()
incr = do
  work (Rate 1 1 0) $ do
    x <- pop
    push (x+1)

intPrinter :: Filter Int Void ()
intPrinter = do
  work (Rate 0 1 0) $ do
    x <- pop 
    println x
    return ()

loop 0 = return ()
loop n = do add incr
            loop (n-1)

main :: StreamIt Void Void ()
main = pipeline $ do
  add intSource
  loop 10
  add intPrinter
