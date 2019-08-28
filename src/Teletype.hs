module Teletype where

data Teletype a = End a
                | Get (Char -> Teletype a)
                | Put Char (Teletype a)

echo = Get (\c -> Put c echo)