{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.

--Helpers:
powerdividor (Power x) = x

polynomialdividor3 (Polynomial z) = polynomialdividor z
    
polynomialdividor (a:rest)
    | (a:rest)==[] = ""
    | rest==[] = polynomialdividor2 a
    | otherwise = polynomialdividor2 a ++ " + " ++ polynomialdividor rest
    
polynomialdividor2 (x,(Power z))
    | x==0 = ""
    | x==1 = (show (Power z))
    | x== -1 = "-" ++ (show (Power z))    
    | z==0 = (show x)
    | otherwise = (show x) ++ (show (Power z))

expdividor (Exponential a) = polynomialdividor4 a
polynomialdividor4 (Polynomial z)
    | z==[] = "0"
    | otherwise = polynomialdividor5 z
polynomialdividor5 (a:rest)
    | rest==[]  = polynomialdividor6 a
    | otherwise = polynomialdividor6 a ++ " + " ++ polynomialdividor rest
    
polynomialdividor6 (x,(Power z))
    | x==0 = ""
    | x==1 = (show (Power z))
    | x== -1 = "-" ++ (show (Power z))
    | z==0 = (show x)
    | otherwise = (show x) ++ (show (Power z))

tridividor (Sin a) = "sin"
tridividor (Cos a) = "cos"

tridiv (Sin a) = polynomialdividor4 a
tridiv (Cos a) = polynomialdividor4 a


constdividor (Const a) = a
pwdividor1 (Pw a b) = a
pwdividor2 (Pw a b) = b
trigdividor1 (Trig a b c) = a
trigdividor2 (Trig a b c) = b
trigdividor3 (Trig a b c) = c
expdividor1 (Exp a b c) = a
expdividor2 (Exp a b c) = b
expdividor3 (Exp a b c) = c


termdividor (Const a) = "const"
termdividor (Pw a b) = "pw"
termdividor (Trig a b c) = "trig"
termdividor (Exp a b c) = "exp"

pwrdividor (Power m) = m

poly2 (Polynomial a) = a
polydividor (Polynomial a) m =evalhelper a m
mm1 (a,b) = a
mm2 (a,b) = b

evalhelper (a1:rest) z
    | rest==[] =  ((fromInteger (mm1 a1)) * ( (function (mm2 a1) z)))
    | otherwise =  ((fromInteger (mm1 a1)) * ((function (mm2 a1) z)) + evalhelper rest z)

tridiv2 (Sin a) = a
tridiv2 (Cos a) = a

expodivid (Exponential m)=m

including (a1:rest)
    | rest==[] && a1/= '^' = False
    | a1== '^' = True
    | otherwise = including rest

including2 (a1:rest)
    | (isSpace a1) = True
    | rest==[] = False
    | otherwise = including2 rest

isSpace x
    |x== ' ' = True
    | otherwise = False
    

-- INSTANCES FOR POWER

instance Show Power where
    show mr
        | (powerdividor mr)==0 = "1"
        | (powerdividor mr)==1 = "x"
        | otherwise = "x^" ++ (show (powerdividor mr))
    

instance Eq Power where
    a == b
        | (powerdividor a) == (powerdividor b) = True
        | otherwise = False


instance Ord Power where
    a <= b
        | (powerdividor a) <= (powerdividor b) = True
        | otherwise = False


instance Evaluable Power where
    function a = f where
        f x = getRounded (fromInteger x^(pwrdividor a))
    

instance Differentiable Power where
    derivative mr
        |(pwrdividor mr)==0 = [Const 0]
        |(pwrdividor mr)==1 = [Const 1]
        |otherwise = [Pw (pwrdividor mr) (Power ((pwrdividor mr)-1))]



-- INSTANCES FOR POLYNOMIAL

instance Show Polynomial where
    show mr = polynomialdividor3 mr
    

instance Eq Polynomial where
    a == b
        | (length (poly2 a)) == (length (poly2 b)) && (polyeq (poly2 a) (poly2 b)) = True
        | otherwise = False

polyeq (a1:rest1) (b1:rest2)
    | rest1==[] && a1==b1 = True
    | a1/=b1 = False
    | otherwise = polyeq rest1 rest2


instance Evaluable Polynomial where
    function a = f where
        f x
            | (poly2 a)==[] = 0.0
            | otherwise = getRounded (polydividor a x)
            

instance Differentiable Polynomial where
    derivative mr = polydiv1 mr
            
polydiv1 (Polynomial a)= polydiv2 a

polydiv2 (a1:rest)
    | rest==[] = polydiv3 a1
    | otherwise = (polydiv3 a1) ++ (polydiv2 rest)

polydiv3 (intt,pwrr)
    | (pwrdividor pwrr)==0 = []
    | (pwrdividor pwrr)==1 = [Const intt]
    | otherwise = [Pw (intt*(pwrdividor pwrr)) (Power ((pwrdividor pwrr)-1))]




-- INSTANCES FOR TRIGONOMETRIC

instance Show Trigonometric where
    show mr 
        | (tridividor mr) == "sin" && (length (tridiv mr)) >=3 && (including (tridiv mr)) = "sin(" ++ (tridiv mr) ++ ")"
        | (tridividor mr) == "sin" && (length (tridiv mr)) >=3 && (including2 (tridiv mr)) = "sin(" ++ (tridiv mr) ++ ")"
        | (tridividor mr) == "sin" && (tridiv mr) == "" = "sin0"
        | (tridividor mr) == "sin" = "sin" ++ (tridiv mr)
        | (tridividor mr) == "cos" && (length (tridiv mr)) >=3 && (including (tridiv mr)) = "cos(" ++ (tridiv mr) ++ ")"
        | (tridividor mr) == "cos" && (length (tridiv mr)) >=3 && (including2 (tridiv mr)) = "cos(" ++ (tridiv mr) ++ ")"
        | (tridividor mr) == "cos" && (tridiv mr) == "" = "cos0"
        | (tridividor mr) == "cos" = "cos" ++ (tridiv mr)
    

instance Eq Trigonometric where
    a == b
        | (tridividor a) == (tridividor b) && (tridiv2 a) == (tridiv2 b) = True
        | otherwise = False


instance Evaluable Trigonometric where
    function a = f where
        f x
            | (tridividor a)=="sin" = getRounded (sin(function (tridiv2 a) x))
            | (tridividor a)=="cos" = getRounded (cos(function (tridiv2 a) x))



instance Differentiable Trigonometric where
    derivative mr
        | (tridividor mr)=="sin" = tridifsin (tridiv2 mr) mr
        | (tridividor mr)=="cos" = tridifcos (tridiv2 mr) mr
        
tridifsin (Polynomial a) mr = tridifsin2 a mr

tridifsin2 (a1:rest) mr
    |rest==[] = tridifsin3 a1 mr
    | otherwise = (tridifsin3 a1 mr) ++ (tridifsin2 rest mr)

tridifsin3 (intt,pwrr) mr
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Trig intt (Power 0) (Cos (tridiv2 mr))]
    | otherwise = [Trig (intt*(pwrdividor pwrr)) (Power ((pwrdividor pwrr)-1)) (Cos (tridiv2 mr))]

tridifcos (Polynomial a) mr = tridifcos2 a mr

tridifcos2 (a1:rest) mr
    |rest==[] = tridifcos3 a1 mr 1
    | otherwise = (tridifcos3 a1 mr 1) ++ (tridifcos2 rest mr)

tridifcos3 (intt,pwrr) mr s
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Trig ((fromInteger(-1))*intt) (Power 0) (Sin (tridiv2 mr))]
    | otherwise = [Trig ((fromInteger(-1))*intt*(pwrdividor pwrr)) (Power ((pwrdividor pwrr)-1)) (Sin (tridiv2 mr))]




-- INSTANCES FOR EXPONENTIAL


instance Show Exponential where
    show mr 
        | (expdividor mr) == "0" = "1"
        | (expdividor mr) == "" = "1"
        | (expdividor mr) == "1" = "e"
        | (length (expdividor mr)) >=3 && (including (expdividor mr)) = "e^(" ++ (expdividor mr) ++ ")"
        | (length (expdividor mr)) >=3 && (including2 (expdividor mr)) = "e^(" ++ (expdividor mr) ++ ")"
        |otherwise = "e^" ++ (expdividor mr)


        
instance Eq Exponential where
    a == b
        | (expodivid a) == (expodivid b) = True
        | otherwise = False

instance Evaluable Exponential where
    function a = f where
        f x = getRounded (exp (function (expodivid a) x))
            


instance Differentiable Exponential where
    derivative mr = expdif mr mr
    
expdif (Exponential a) mr = expdif2 a mr

expdif2 (Polynomial a) mr = expdif3 a mr

expdif3 (a1:rest) mr
    |rest==[] = expdif4 a1 mr
    | otherwise = (expdif4 a1 mr) ++ (expdif3 rest mr)

expdif4 (intt,pwrr) mr
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Exp intt (Power 0) mr]
    | otherwise = [Exp (intt*(pwrdividor pwrr)) (Power ((pwrdividor pwrr)-1)) mr]




-- INSTANCES FOR TERM


instance Show Term where
    show mr
        |(termdividor mr) == "const" = show (constdividor mr)
        |(termdividor mr) == "pw" && (show (pwdividor1 mr)) == "0" = "0"
        |(termdividor mr) == "pw" && (show (pwdividor1 mr)) == "1" = (show (pwdividor2 mr))
        |(termdividor mr) == "pw" && (show (pwdividor1 mr)) == "-1" = "-" ++ (show (pwdividor2 mr))
        |(termdividor mr) == "pw" && (powerdividor (pwdividor2 mr))==0 = (show (pwdividor1 mr))
        |(termdividor mr) == "pw" = (show (pwdividor1 mr)) ++ (show (pwdividor2 mr))
        |(termdividor mr) == "trig" && (show (Pw (trigdividor1 mr) (trigdividor2 mr))) == "1" =  (show (trigdividor3 mr))
        |(termdividor mr) == "trig" && (show (Pw (trigdividor1 mr) (trigdividor2 mr))) == "-1" = "-" ++ (show (trigdividor3 mr))
        |(termdividor mr) == "trig" && (show (Pw (trigdividor1 mr) (trigdividor2 mr))) == "0" =  "0"
        |(termdividor mr) == "trig" = (show (Pw (trigdividor1 mr) (trigdividor2 mr))) ++ (show (trigdividor3 mr))
        |(termdividor mr) == "exp" && (show (Pw (expdividor1 mr) (expdividor2 mr))) == "1" = (show (expdividor3 mr))
        |(termdividor mr) == "exp" && (show (Pw (expdividor1 mr) (expdividor2 mr))) == "-1" = "-" ++ (show (expdividor3 mr))
        |(termdividor mr) == "exp" && (show (Pw (expdividor1 mr) (expdividor2 mr))) == "0" =  "0"
        |(termdividor mr) == "exp" = (show (Pw (expdividor1 mr) (expdividor2 mr))) ++ (show (expdividor3 mr))



instance Eq Term where
    a == b
        | (termdividor a) == (termdividor b) && (termdividor a)=="const" && (constdividor a)== (constdividor b) = True
        | (termdividor a) == (termdividor b) && (termdividor a)=="pw" && (pwdividor1 a)== (pwdividor1 b) && (pwdividor2 a)== (pwdividor2 b) = True
        | (termdividor a) == (termdividor b) && (termdividor a)=="trig" && (trigdividor1 a)== (trigdividor1 b) && (trigdividor2 a)== (trigdividor2 b) &&  (trigdividor3 a)== (trigdividor3 b) = True
        | (termdividor a) == (termdividor b) && (termdividor a)=="exp" && (expdividor1 a)== (expdividor1 b) && (expdividor2 a)== (expdividor2 b) &&  (expdividor3 a)== (expdividor3 b) = True
        | otherwise = False


instance Evaluable Term where
    function a = f where
        f x
            |(termdividor a) == "const" = getRounded (fromInteger(constdividor a))
            |(termdividor a) == "pw" =  getRounded (fromInteger((pwdividor1 a)) * (function (pwdividor2 a) x))
            |(termdividor a) == "trig" = getRounded ((function (Pw (trigdividor1 a) (trigdividor2 a)) x) * (function (trigdividor3 a) x))
            |(termdividor a) == "exp" = getRounded ((function (Pw (expdividor1 a) (expdividor2 a)) x) * (function (expdividor3 a) x))



instance Differentiable Term where
    derivative mr
        |(termdividor mr) == "const" = []
        |(termdividor mr) == "pw" && (pwrdividor (pwdividor2 mr))==0 = [Const 0]
        |(termdividor mr) == "pw" && (pwrdividor (pwdividor2 mr))==1 = [Const (fromInteger(pwdividor1 mr))]
        |(termdividor mr) == "pw" && (pwdividor1 mr) == 0 = [Const 0]
        |(termdividor mr) == "pw" = [Pw ((fromInteger(pwdividor1 mr))*(pwrdividor (pwdividor2 mr))) (Power ((pwrdividor (pwdividor2 mr))-1))]
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="sin" && (pwrdividor (trigdividor2 mr))==0 = (tridifsinterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr))      
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="sin" && (pwrdividor (trigdividor2 mr))==1 = (tridifsinterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr)) ++ [Trig ((trigdividor1 mr)*(pwrdividor (trigdividor2 mr))) (Power 0) (trigdividor3 mr)]        
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="sin" && (trigdividor1 mr)==0 = [Const 0]     
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="sin"  = (tridifsinterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr)) ++ [Trig ((trigdividor1 mr)*(pwrdividor (trigdividor2 mr))) (Power ((pwrdividor (trigdividor2 mr))-1)) (trigdividor3 mr)]        
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="cos" && (pwrdividor (trigdividor2 mr))==0 = (tridifcosterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr)) 
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="cos" && (pwrdividor (trigdividor2 mr))==1 = (tridifcosterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr)) ++ [Trig ((trigdividor1 mr)*(pwrdividor (trigdividor2 mr))) (Power 0) (trigdividor3 mr)]  
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="cos" && (trigdividor1 mr)==0 = [Const 0]  
        |(termdividor mr) == "trig" && (tridividor (trigdividor3 mr))=="cos" = (tridifcosterm (tridiv2 (trigdividor3 mr)) (trigdividor3 mr) (trigdividor1 mr) (trigdividor2 mr)) ++ [Trig ((trigdividor1 mr)*(pwrdividor (trigdividor2 mr))) (Power ((pwrdividor (trigdividor2 mr))-1)) (trigdividor3 mr)]  
        |(termdividor mr) == "exp" && (pwrdividor (expdividor2 mr))==0 = expdifterm (expdividor3 mr) (expdividor3 mr) (expdividor1 mr) (expdividor2 mr)
        |(termdividor mr) == "exp" && (expdividor1 mr)==0 = [Const 0]
        |(termdividor mr) == "exp" = (expdifterm (expdividor3 mr) (expdividor3 mr) (expdividor1 mr) (expdividor2 mr)) ++ [Exp ((expdividor1 mr)*(pwrdividor (expdividor2 mr))) (Power ((pwrdividor (expdividor2 mr))-1)) (expdividor3 mr)]  

expdifterm (Exponential a) mr coef pwr = expdifterm2 a mr coef pwr

expdifterm2 (Polynomial a) mr coef pwr = expdifterm3 a mr coef pwr

expdifterm3 (a1:rest) mr coef pwr
    |rest==[] = expdifterm4 a1 mr coef pwr
    | otherwise = (expdifterm4 a1 mr coef pwr) ++ (expdifterm3 rest mr coef pwr)

expdifterm4 (intt,pwrr) mr coef pwr
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Exp (intt*coef) pwr mr]
    | otherwise = [Exp (intt*(pwrdividor pwrr)*coef) (Power ((pwrdividor pwrr)-1+(pwrdividor pwr))) mr]




tridifsinterm (Polynomial a) mr coef pwr = tridifsinterm2 a mr coef pwr

tridifsinterm2 (a1:rest) mr coef pwr
    |rest==[] = tridifsinterm3 a1 mr coef pwr
    | otherwise = (tridifsinterm3 a1 mr coef pwr) ++ (tridifsinterm2 rest mr coef pwr)

tridifsinterm3 (intt,pwrr) mr coef pwr
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Trig (intt*coef) pwr (Cos (tridiv2 mr))]
    | otherwise = [Trig (intt*(pwrdividor pwrr)*coef) (Power ((pwrdividor pwrr)-1+(pwrdividor pwr))) (Cos (tridiv2 mr))]

tridifcosterm (Polynomial a) mr coef pwr = tridifcosterm2 a mr coef pwr

tridifcosterm2 (a1:rest) mr coef pwr
    |rest==[] = tridifcosterm3 a1 mr coef pwr
    | otherwise = (tridifcosterm3 a1 mr coef pwr) ++ (tridifcosterm2 rest mr coef pwr)

tridifcosterm3 (intt,pwrr) mr coef pwr
    | (pwrdividor pwrr) ==0 = []
    | (pwrdividor pwrr) ==1 = [Trig ((fromInteger(-1))*intt*coef) pwr (Sin (tridiv2 mr))]
    | otherwise = [Trig ((fromInteger(-1))*intt*(pwrdividor pwrr)*coef) (Power ((pwrdividor pwrr)-1+(pwrdividor pwr))) (Sin (tridiv2 mr))]






-- INSTANCES FOR [TERM]

instance Evaluable [Term] where
    function a = f where
        f x = getRounded (ff1 a x)

ff1 (a1:rest) x
    | rest==[] = function a1 x
    | otherwise = (function a1 x) + (ff1 rest x)


instance Differentiable [Term] where
    derivative mr = ff2 mr

ff2 (a1:rest)
    | rest==[] = derivative a1
    | otherwise = (derivative a1) ++ (ff2 rest)


