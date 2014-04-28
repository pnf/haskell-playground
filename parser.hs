import Control.Monad

newtype Parser a = Parser (String -> [(a,String)])

item :: Parser Char
item  = Parser (\cs -> case cs of
                   ""     -> []
                   (c:cs) -> [(c,cs)])



instance Monad Parser where
      return a = Parser (\cs -> [(a,cs)])
      p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

parse :: Parser t -> String -> [(t,String)]
parse (Parser p) = p

{--
p  = do {c <- item; item; d <- item; return (c,d)}
parse p "abcde"
parse (item >>= (\c -> item >>= (\_ -> item >>= (\d -> return (c,d))))) "abcde"
--}


instance MonadPlus Parser where
      mzero = Parser (\_ -> [])
      mplus p q = Parser (\cs -> parse p cs ++ parse q cs)


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                          [] -> []
                          (x:xs) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat pred = do {c <- item; if pred c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c==)


string (c:cs) = do {char c; string cs; return (c:cs)}
string [] = return ""

{--
parse (string "foo") "foobar"
-- [("foo","bar")]

string2 (c:cs) = (char c) >>= (\_ -> (string2 cs) >>= (\_ -> return (c:cs)))
string2 [] = return ""

--}

many   :: Parser a -> Parser [a]
many p  = many1 p +++ return []
many1  :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- This won't work because the last invocation of p fails returning mzero, which,
-- when encountered on the rhs of the comprehension in bind, results in an empty
-- list of tuples, so return (a:as) is never run!
-- Above, however, the concatenation of mzero with the inocuous return combinator
-- gives us something on which to invoke return (a:as).
-- This is truly twisted.
many' p = do {a <- p; as <- many' p; return (a:as)}
