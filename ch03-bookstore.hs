data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

myInfo = Book 92423432 "Fooey for Frances" ["Atticus Finch", "Oglemore"]

myGetId (Book id name authors) = id
