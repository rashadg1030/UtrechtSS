{-# LANGUAGE InstanceSigs #-}

module MTA.Teletype where

import Prelude hiding (getLine)


data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)

echo :: Teletype String
echo = Get (\c -> Put c echo)

-- Exercise 1. Write a Teletype-program getLine which reads characters until it finds a newline character,
-- and returns the complete string.

-- getLine :: Teletype String
-- getLine = Get (\c -> if isNewline c then End else Put c getLine )

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _    = False

instance Functor Teletype where
    fmap f (End x)   = End (f x)
    fmap f (Get g)   = Get (fmap f . g)
    fmap f (Put c x) = Put c (fmap f x)

-- Exercise 2. Define sensible Applicative and Monad instances for Teletype.

instance Applicative Teletype where
    pure :: a -> Teletype a
    pure x = End x

    (<*>) :: Teletype (a -> b) -> Teletype a -> Teletype b
    End f <*> End x       = End (f x)
    Get f <*> Get g       = Get (\c -> f c <*> g c)
    Put c t <*> Put c' t' = Put c $ t <*> t' -- ?????

-- instance Monad Teletype where
--     return = pure

--     (>>=) :: Teletype a -> (a -> Teletype b) -> Teletype b
--     End x >>= f = f x
--     Get f >>= g = Get

-- I just submitted my application to Tsuru, but what I ended up doing thanks
-- to the fact that we can assume that whatever packets exist will show up at
-- most 3 seconds later is keep a rolling buffer of packets that fit into that
-- three second window. Whatever fell outside of the window on the low end was
-- therefore able to be output in sorted order, which meant that as long as there
-- was always approximately enough memory to hold the last 3 seconds worth of
-- data, we wouldn't exceed memory constraints. In terms of an efficient data
-- structure for this strategy, I used minheaps for the buffer and unioned new
-- data into the old heap, which ended up being performant enough. On my machine
-- without any parallelism or concurrency tuning, I could churn through 30 seconds
-- of data and output (print to the terminal) a sufficiently sorted dataset in 3
-- seconds (5.9 MB of captured network traffic) or so with only about 5% of that
-- time being GC. For reference, printing unsorted took about 1.3 seconds, and was
-- essentially constant in memory usage at about 7000 bytes (according to heap
-- profiling). The -r version ended up topping out at 120k max of heap space usage
-- (which couldn't really be helped, since we have to hold on to 3 seconds worth of
-- data no matter which way I look at it), but due to the rolling buffer strategy
-- ended up looking more like a see-saw with periods of allocation and deallocation.
