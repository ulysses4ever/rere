|| "Haskell98" Void
void ::= Void void

|| example
||
|| v :: void
|| v = error "cheat"
||
|| n :: num
|| n = absurd v
||
absurd :: void -> *
absurd unused = seq unused (absurd unused)

|| Maybe
||

maybe * ::= Nothing
            | Just *

mapMaybe :: (* -> **) -> maybe * -> maybe **
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

traverseMaybeMaybe :: (* -> maybe **) -> maybe * -> maybe (maybe **)
traverseMaybeMaybe f Nothing  = Just Nothing
traverseMaybeMaybe f (Just x) = mapMaybe Just (f x)

map2Maybe :: (* -> ** -> ***) -> maybe * -> maybe ** -> maybe ***
map2Maybe f (Just x) (Just y) = Just (f x y)
map2Maybe f x        y        = Nothing

elimMaybe :: ** -> (* -> **) -> maybe * -> **
elimMaybe n j Nothing  = n
elimMaybe n j (Just x) = j x

fromMaybe :: * -> maybe * -> *
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

|| Regular expressions
||

re * ::= Nul
         | Eps
				 | Ch char
				 | App (re *) (re *)
				 | Alt (re *) (re *)
				 | Star (re *)
         | Var *
    		 | Let (re *) (re (maybe *))
				 | Fix (re (maybe *))

mapRe :: (* -> **) -> re * -> re **
mapRe f Nul       = Nul
mapRe f Eps       = Eps
mapRe f (Ch c)    = Ch c
mapRe f (App r s) = App (mapRe f r) (mapRe f s)
mapRe f (Alt r s) = Alt (mapRe f r) (mapRe f s)
mapRe f (Star r)  = Star (mapRe f r)
mapRe f (Var a)   = Var (f a)
mapRe f (Let r s) = Let (mapRe f r) (mapRe (mapMaybe f) s)
mapRe f (Fix r)   = Fix (mapRe (mapMaybe f)  r)

traverseReMaybe :: (* -> maybe **) -> re * -> maybe (re **)
traverseReMaybe f Nul       = Just Nul
traverseReMaybe f Eps       = Just Eps
traverseReMaybe f (Ch c)    = Just (Ch c)
traverseReMaybe f (App r s) = map2Maybe App (traverseReMaybe f r) (traverseReMaybe f s)
traverseReMaybe f (Alt r s) = map2Maybe Alt (traverseReMaybe f r) (traverseReMaybe f s)
traverseReMaybe f (Star r)  = mapMaybe Star (traverseReMaybe f r)
traverseReMaybe f (Var x)   = mapMaybe Var (f x)
traverseReMaybe f (Let r s) = map2Maybe Let (traverseReMaybe f r) (traverseReMaybe (traverseMaybeMaybe f) s)
traverseReMaybe f (Fix r)   = mapMaybe Fix (traverseReMaybe (traverseMaybeMaybe f) r)

liftRe :: re * -> re (maybe *)
liftRe = mapRe Just

|| Nullability
||

nullable :: re void -> bool
nullable r = nullable' (mapRe absurd r)

nullable' :: re bool -> bool
nullable' Nul       = False
nullable' Eps       = True
nullable' (Ch c)    = False
nullable' (App r s) = nullable' r & nullable' s
nullable' (Alt r s) = nullable' r \/ nullable' s
nullable' (Star r)  = True
nullable' (Var a)   = a
nullable' (Let r s) = nullable' (mapRe (fromMaybe (nullable' r)) s)
nullable' (Fix r)   = nullable' (mapRe (fromMaybe False) r)

|| Derivative

deriv :: char -> re void -> re void
deriv c r = deriv' c (mapRe absurd r)

deriv' :: char -> re (bool, *, *) -> re *
deriv' c Nul       = Nul
deriv' c Eps       = Nul
deriv' c (Ch x)    = Eps, if c == x
                   = Nul, otherwise
deriv' c (Alt r s) = alt_ (deriv' c r) (deriv' c s)
deriv' c (App r s) = alt_ (deriv' c s) (app_ (deriv' c r) (mapRe trdOf3 s)), if nullable' (mapRe fstOf3 r)
                   =                    app_ (deriv' c r) (mapRe trdOf3 s) , otherwise
deriv' c (Star r)  = app_ (deriv' c r)	(mapRe trdOf3 (Star r))
deriv' c (Var x)   = Var (sndOf3 x)
deriv' c (Let r s) = let_ (mapRe trdOf3 r)
                     (let_ (liftRe (deriv' c r))
										 (deriv' c (mapRe (elimMaybe
										 		(nullable' (mapRe fstOf3 r), Nothing, Just Nothing)
												bimapFF)
											s)))
deriv' c (Fix r)   = let_ (mapRe trdOf3 (Fix r))
                     (fix_
										 (deriv' c (mapRe (elimMaybe
										 		(nullable' (mapRe fstOf3 (Fix r)), Nothing, Just Nothing)
												bimapFF)
											r)))

bimapFF :: (bool, *, *) -> (bool, maybe (maybe *), maybe (maybe *))
bimapFF (x,y,z) = (x, Just (Just y), Just (Just z))

|| Match
||

match :: re void -> [char] -> bool
match re []     = nullable re
match re (c:cs) = match (deriv c re) cs

|| Smart constructors
||

ch_ :: [char] -> re *
ch_ = alts_ . map Ch

star_ :: re * -> re *
star_ Nul      = Eps
star_ Eps      = Eps
star_ (Star r) = Star r
star_ r        = Star r

alt_ :: re * -> re * -> re *
alt_ Nul       s         = s
alt_ r         Nul       = r
alt_ (Let r s) p         = let_ r (alt_ s (liftRe p))
alt_ r         (Let p s) = let_ p (alt_ (liftRe r) s)
alt_ r         s         = Alt r s

alts_ :: [re *] -> re *
alts_ = foldr alt_ Nul

app_ :: re * -> re * -> re *
app_ Nul       s         = Nul
app_ r         Nul       = Nul
app_ Eps       s         = s
app_ r         Eps       = r
app_ (Let r s) p         = let_ r (app_ s (liftRe p))
app_ r         (Let p s) = let_ p (app_ (liftRe r) s)
app_ r         s         = App r s

apps_ :: [re *] -> re *
apps_ = foldr app_ Eps

let_ :: re * -> re (maybe *) -> re *
let_ r s = subst s (elimMaybe r Var), if cheap_ r
let_ r s = postlet_ r s

postlet_ :: re * -> re (maybe *) -> re *
postlet_ r s = fromMaybe (Let r s) (unused s)

cheap_ :: re * -> bool
cheap_ Nul     = True
cheap_ Eps     = True
cheap_ (Ch c)  = True
cheap_ (Var x) = True
cheap_ other   = False

fix_ :: re (maybe *) -> re *
fix_ r = Nul, if isNul (subst r (elimMaybe Nul Var))
fix_ r = fromMaybe (Fix r) (unused r)

|| Helpers
||

isNul :: re * -> bool
isNul Nul = True
isNul oth = False

unused :: re (maybe *) -> maybe (re *)
unused = traverseReMaybe id

subst :: re * -> (* -> re **) -> re **
subst Nul       k = Nul
subst Eps       k = Eps
subst (Ch c)    k = Ch c
subst (Var x)   k = k x
subst (App r s) k = app_ (subst r k) (subst s k)
subst (Alt r s) k = alt_  (subst r k) (subst s k)
subst (Star r)  k = star_ (subst r k)
subst (Let r s) k = let_ (subst r k) (subst s (elimMaybe (Var Nothing) (mapRe Just . k)))
subst (Fix r)   k = fix_ (subst r (elimMaybe (Var Nothing) (mapRe Just . k)))

|| Triple
||

fstOf3 :: (*,**,***) -> *
fstOf3 (x,y,z) = x

sndOf3 :: (*,**,***) -> **
sndOf3 (x,y,z) = y

trdOf3 :: (*,**,***) -> ***
trdOf3 (x,y,z) = z

|| Example regular expressions
||

|| Fancy way to say Nul
ex0 :: re void
ex0 = Fix (Var Nothing)

ex0b :: re void
ex0b = fix_ (Var Nothing)

ex4 :: re void
ex4 = fix_ (alt_ Eps (app_ (Ch 'a') (app_ (Var Nothing) (Ch 'b'))))

|| [True,False,True,True]
ex4run :: [bool]
ex4run = map (match ex4) ["", "a", "ab", "aaaabbbb"]

var0 = Var Nothing
var1 = Var (Just Nothing)
var2 = Var (Just (Just Nothing))

ex7 :: re void
ex7 = let_ (app_ (ch_ "0123456789") (star_ (ch_ "0123456789")))
      (fix_ (let_ (alt_ var1 (app_ (ch_ "(") (app_ var0 (ch_ ")"))))
            (let_ (fix_ (alt_ (app_ var1 (app_ (ch_ "+") var0)) var1))
            (alt_ (app_ var0 (app_ (ch_ "*") var2)) var0))))

ex7run :: [bool]
ex7run = map (match ex7) ["", "123","123*(321+(123*244))"]

input :: [char]
input = "1000*(2020+(20*30))*(20+3)*((30+20)*10000)+123123123*12313"

bench = match ex7 input
