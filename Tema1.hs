module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Array
solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple (n,t) = let max=1000
                        addIdentic 0 t = t
                        addIdentic x t = ((x,x,max): (addIdentic (x-1) t) )
                        tnou=addIdentic n t
                        b=((1,1,0),(n,n,n))
                        m=Data.Array.listArray b [(completare i j k)|(i,j,k)<-range b]
                        completare i j 0 = let cost=let r=filter (\(a,b,c)->(a==i&&b==j) || (a==j&&b==i) ) tnou in 
                                                        (if r==[] then max else let third tuple=let (_,_,a)=tuple in 
                                                                                                    a in 
                                                                                    third (head r) ) in 
                                               (cost,(if cost==max then [] else [i,j]))
                        completare i j k
                            | i==j = (max,[])
                            | ( ( fst(m!(i,j,k-1)) ) <= ( ( fst (m!(i,k,k-1)))+(fst (m!(k,j,k-1))) ) ) = (m!(i,j,k-1))
                            | otherwise = ( ( (fst (m!(i,k,k-1)))+(fst (m!(k,j,k-1))) ),( (snd (m!(i,k,k-1)))++(tail (snd (m!(k,j,k-1))) ) ) )
                    in (  if ((snd (m!(1,n,n)))==[]) then Nothing else Just ( (snd (m!(1,n,n))),(fst (m!(1,n,n))))  )
solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts (n,si,taxe,t) = let max=1000
                               addIdentic 0 t = t
                               addIdentic x t = ((x,x,max): (addIdentic (x-1) t) )
                               tnou=addIdentic n t
                               b=((1,1,0),(n,n,n))
                               m=Data.Array.listArray b [(completare i j k)|(i,j,k)<-range b]
                               first tuple=let{(a,_,_)=tuple}in a
                               second tuple=let{(_,a,_)=tuple}in a
                               third tuple=let{(_,_,a)=tuple}in a
                               completare i j 0 = let cost=let r=filter (\(a,b,c)->(a==i&&b==j) || (a==j&&b==i) ) tnou in 
                                                               (if r==[] then max else third (head r) ) in 
                                                      (cost,(if (cost==max) || (  si<(taxe!!(j-1))  ) then [] else [(i,0),( j,(taxe!!(j-1)) )] ),(if (cost==max) || (  si<(taxe!!(j-1))  ) then 0 else((taxe!!(j-1))) ))
                               completare i j k
                                   | i==j = (max,[],0)
                                   | let s =  ( (  (third (m!(i,k,k-1))) + (third (m!(k,j,k-1)))  ) ) + (third (m!(1,i,k-1))) + (third (m!(j,n,k-1)))
                                         c1 = (first (m!(i,k,k-1))) == max
                                         c2 = (first (m!(k,j,k-1))) == max
                                         dif = ( ( first (m!(i,j,k-1)) ) - ( ( first (m!(i,k,k-1)))+(first (m!(k,j,k-1))) ) )
                                         dif_pierderi = ( ( third (m!(i,j,k-1)) ) - ( ( third (m!(i,k,k-1)))+(third (m!(k,j,k-1))) ) ) in
                                         ( c1 || c2 || (dif<0) || ((dif==0) && (dif_pierderi<=0)) || ( (  si - s  ) < 0 ) ) = (m!(i,j,k-1))     
                                   | otherwise = let s = (   (third (m!(i,k,k-1))) + (third (m!(k,j,k-1)))  ) 
                                                     op =(\(x,y)->(x,y+ (third (m!(i,k,k-1))) ))
                                                     st = (second (m!(i,k,k-1)))
                                                     dr = map op (tail (second (m!(k,j,k-1))) ) in
                                                     ( ( (first (m!(i,k,k-1)))+(first (m!(k,j,k-1))) ),( st++dr),s )
                               op = (\(x,y)->(x,si - y ))
                               sir=map op (second (m!(1,n,n)))
                               ans = ((first (m!(1,n,n))),sir,(third (m!(1,n,n)))) in
                               (if (second ans)==[] then Nothing else Just ((second ans),(first ans))  ) 