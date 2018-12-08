data T=T[T][Int]
s a b=(drop a b,take a b)
z=(\(d,[e,f])->(\(g,h)->T h<$>s f g)$foldr(\_(r,t)->(:t)<$>z r)(d,[])[1..e]).s 2
p=snd.z.map read.words
q(T t i)=sum$i++(q<$>t)
i l n=take 1$drop(n-1)l
r(T[]l)=sum l
r(T t l)=sum.map r$i t=<<l
main=interact$show.(\x->[q x,r x]).p