import Debug.Trace
{--
(def fact-maker (fn [g]
           (fn [n] (if (= 0 n) 1 (* n (g (- n 1)))))))

(defn Y [f]
  ((fn [g] (g g))
   (fn [h] (fn [n] ((f (h h)) n)))))

(defn Y [f]
  ((fn [g] (g g))
   (fn [h] (fn [n] ((f (h h)) n)))))


(defn Y [f]
  ((fn [g] (g g))
   (fn [h] (fn [n] ((f (h h)) n)))))

;((Y fact-maker) 5)
--}

{--
yc :: (Int -> Int) -> (Int -> Int)
yc f = ((\ g -> g g) (\ h -> (\ n -> ( (f (h h)) n)) ))
--}

fm = (\ g -> (\ n -> if n==0 then 1 else n * (g (n - 1))))

fix :: (a -> a) -> a
fix f = f (fix f)

factabs fact 0 = 1               -- factabs is F from the lambda calculus example
factabs fact x = x * fact (x-1)

{--

(def collatz (fn [g]
           (fn [n] (cond (= 1 n) n (even? n) (g (/ n 2)) (odd? n) (g (+ 1 (* 3 n))))))

--}

-- yc f = (\ g -> g g) (\ h -> (\ n -> ( (f (h h)) n)) ))
-- yc f = (\ g -> g g) (\ h -> (\ n -> ( ((f::(Integer->Integer)->Integer->Integer) (h h)) n)) )

newtype Mu a = Roll { unroll :: Mu a -> a }
yc :: (a -> a) -> a
yc = \f -> (\g -> f (unroll g g)) $ Roll (\h -> f (unroll h h))
{--
yc = \f -> (\g -> g g           )        (\h -> (f (h h)))
yc = \f -> (\g -> g g) (\h -> (f (h h)))
--}




collatz :: (Integer -> Integer) -> Integer -> Integer
collatz g = fnn where
     fnn n | n==1 = trace (show n) n
           | even n = trace (show n) (g (quot n 2))
           | otherwise = trace (show n) (g (1 + 3*n))
