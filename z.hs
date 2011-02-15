data Vector =  Complex Float Float deriving (Show)

{- Vector addition -}
(.+) :: Vector -> Vector -> Vector
(Complex i j) .+ (Complex l m) = Complex (i+l) (j+m)
infix 6 .+

{- Vector subtraction -}
(.-) :: Vector -> Vector -> Vector
(Complex i j) .- (Complex l m) = Complex (i-l) (j-m)
infix 6 .-

{- Cross Product / Multiplication -}
(.*) :: Vector -> Vector -> Vector
(Complex i j) .* (Complex l m) = Complex (i*l - j*m) (i*m + j*l)
infix 7 .*

(./) :: Vector -> Vector -> Vector
a ./ b = a .* (invert b)
infix 7 ./

{- Scalar multiplication -}
times :: Float -> Vector -> Vector
times m (Complex i j) = Complex (m*i) (m*j)

{- DotProduct -}
dot :: Vector -> Vector -> Float
dot (Complex i j) (Complex l m) = i*l + j*m

{- Magnitude -}
mag :: Vector -> Float
mag a = sqrt (dot a a)

{- Negate -}
neg :: Vector -> Vector
neg (Complex i j) = Complex (-i) (-j)

{- Angle between vectors-}
angle :: Vector -> Vector -> Float
angle a b = acos ((dot a b) / ((mag a) * mag(b)))

{- Conjugate of complex number -}
conjugate :: Vector -> Vector
conjugate (Complex i j) = Complex i (-j)

{- Reciprocal -}
invert :: Vector -> Vector
invert (Complex i j) = Complex (i / (i^2 + j^2)) (-(j / (i^2 + j^2)))

{- Real component -}
real:: Vector -> Float
real (Complex i j) = i

{- Imaginary component -}
imag :: Vector -> Float
imag (Complex i j) = j

{- Mobius translation by d/c -}
translate :: Vector -> Vector -> Vector -> Vector
translate c d z = z .+ (d ./ c)

{- Mobius dilation and rotation -}
dilate :: Vector -> Vector -> Vector -> Vector -> Vector -> Vector
dilate a b c d z = (neg $ (a.*d .- b.*c) ./ (c.*c)) .* z

{- Mobius -}
mobius :: Vector -> Vector -> Vector -> Vector -> Vector -> Vector
mobius a b c d z = (a.*z .+ b) ./ (c.*z .+ d)
