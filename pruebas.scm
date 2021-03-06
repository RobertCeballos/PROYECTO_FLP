;PRUEBAS INTERPRETE (estas son algunas de las expresiones que ya funcionan en el interprete)

+{3 4 5}
*{A B}
<{B C}

isdet?{B}
isfree?{A}


local X
in
set X = 5
X
end


local X Y
  in
    set B=Y
    set Y=X
    X
end


local X
 in
  local Y
   in
    set Y=12
    set Y=X
   end
  X
end


local X Y
in
set X = ~5.0
set 3.2 = Y
*{X Y}
end


local X Y Z
in
set X = ~5.0
set 3.2 = Y
set Z= *{X Y}
Z
end

local X Y Z
in
set X = +{2.0 4.0}
set 3.2 = Y
set Z=*{X Y}
Z
end

local I F
 in
  set I =1
  set F =9
 for Y in I .. F do
  +{Y Y}
 end
end

;Celdas
local
Cell
in
set Cell = newcell{88}
end

local
Cell 
in
set Cell = newcell{99}
@{Cell}
end

local
Cell X
in
set Cell = newcell{22}
set X = @{Cell}
X
end

local
Cel1 Cel2
in
set Cel1 = newcell{44}
set Cel2 = Cel1
@{Cel2}
end

local Cell 
in
set Cell = newcell{44}
iscell?{Cell}
end

local Cell
in
set Cell=newcell{44}
setcell{Cell 88}
@{Cell}
end

local X Cell in
set Cell=newcell{44}
set X = +{@{Cell} @{Cell}}
X
end

;******************CICLOS******************

local X Cell
Ini Fin
in
set Ini=1
set Fin=9
set Cell=newcell{0}
for Y in  Ini .. Fin do
setcell{Cell +{@{Cell} Y}}
end
@{Cell}
end

;******************PUERTO******************

local Y X P
in
set P = newport{X}
set Y = P
isport?{Y}
end

local X P
in
set P=newport{X}
send{P 33}
send{P 22}    
end

;******************Registros********************
local
X
in
set X = miregistro('Campo1':15 campo2:10)
X
end


local
X
in
set X = miregistro(campo1:8 campo2:9)
X
end

local
Rec X
in
set Rec = miregistro(campo1:X campo2:4)
set X = 10
*{.Rec.campo1 .Rec.campo2}
end


local
Rec X Y
in
set Rec = miregistro(campo1:X campo2:4)
set X = 10
set Y = *{.Rec.campo1 .Rec.campo2}
Y
end


local
Rec1 X Y
in
miregistro(campo1:X campo2:4)
end



local X Y Z
in
set X = miregistro(campo1:_ campo2:Y)
set .X.campo2 = 2
set Z = +{Y 1} 
Z
end

local
Rec X Y
in
set Rec = miregistro(campo1:X campo2:_)
set X = 10
set .Rec.campo2 = .Rec.campo1 
.Rec.campo2
end

local X Y Z
in
set Y=A
set X = miregistro(campo1:_ campo2:Y)
.X.campo2
end


local
Rec X Y Z
in
set Rec = miregistro(campo1:Z campo2:_)
set Y = X
set X = 10
set Z= X
Y
end


 local
Rec X Y
in
set Rec = miregistro(campo1:X campo2:_)
set X = 10
set .Rec.campo2 = A
.Rec.campo2
end


local
Rec1 Rec2 X
in
set Rec1 = miregistro(campo1:X campo2:_)
set Rec2 = miregistro(campo1:X campo2:_)
set Rec1 = Rec2
end


local
Rec1 Rec2 X
in
set Rec1 = miregistro(campo1:X campo2:_)
set Rec2 = miregistro(campo2:5 campo3:_ campo1: X)
set Rec1 = Rec2
end

local
Rec1 Rec2 X
in
set Rec1 = miregistro(campo1:X campo2:_ campo3:_)
set Rec2 = miregistro(campo2:5 campo3:_ campo1: X)
set Rec1 = Rec2
set .Rec1.campo1 = 45
.Rec2.campo1
end

local
Rec1 Rec2 X Y
in
set Rec1 = miregistro(campo1:X campo2:4)
set Rec2 = miregistro(campo1:5 campo2:Y)
set Rec1 =  Rec2
.Rec1.campo1
end

;OJOOOOO
local
Rec1 Rec2 X Y Z
in
set Rec1 = miregistro(campo1:X campo2:4)
set Rec2 = miregistro(campo1:Z campo2:Y)
set Rec1 =  Rec2 
set .Rec1.campo1 = 89
Z
end

 
local Rec1 Rec2 Rec3
in 
set Rec1 = miRegInterno(campo1:miReg(campo1:50 campo2:40) campo2:3)
set Rec2 = .Rec1.campo1
Rec2
end
 
 
local Rec1 Rec2 Rec3
in 
set Rec1 = miRegInterno(campo1:miReg(campo1:_ campo2:40) campo2:3)
set Rec2 = .Rec1.campo1
set .Rec2.campo1 = 78
.Rec2.campo1
end
 
local Rec1 Rec2 Rec3 X
in 
set Rec1 = miReg(campo1:miRegInterno(camp1:X camp2:_) campo2:3)
set Rec2 = .Rec1.campo1  
set .Rec2.camp1 = 56
X
end
 
 
local Rec1 Rec2 Rec3
in 
set Rec3 = miReg(campo1:_ campo2: _)
set Rec1 = miRegInterno(campo1:Rec3 campo2:3)
set Rec2 = .Rec1.campo1
set .Rec2.campo1 = 78
.Rec3.campo1
end


local A R1 R2
in
   set R1 = hola(campo:otto(n:7 b:A) m:_)
   set R2 = hola(campo:otto(n:7 b:7) m:A)
   set R1 = R2
   .R1.campo
end

local A R1 R2 F
in
   set R1 = hola(campo:otto(n:7 b:F) m:_)
   set R2 = hola(campo:otto(n:7 b:_) m:A)
   set R1 = R2
   set F=3
   .R1.campo
end



local A R1 R2 F
in
   set R1 = hola(campo:otto(n:unoMas(campito: F) b:F) m:_)
   set R2 = hola(campo:otto(n:unoMas(campito: _) b:_) m:A)
   set R1 = R2
   set F=3
   .R1.campo
end


local A R1 R2 R3 R4 F 
in
   set R1 = hola(campo:otto(n:unoMas(campito: F) b:F) m:_)
   set R2 = hola(campo:otto(n:unoMas(campito: _) b:_) m:A)
   set R1 = R2
   set R3 = .R1.campo
   set R4 = .R3.n
   set F=3
   +{.R4.campito 6}
end





;**********REGISTROS-CELDAS*******************
local
Cell X Rec Y
in
set Cell = newcell{22}
set Rec = miRegistro(campo1: 2 campo2:Cell)
set X = .Rec.campo2
set Y =@{X} 
Y
end

local X Cell Rec
Ini Fin
in
set Ini=1
set Fin=9
set Cell=newcell{0}
set Rec = miReg(camp1:Cell camp2:3)
for Y in  Ini .. Fin do
setcell{Cell +{@{Cell} Y}}
end
set X =.Rec.camp1
@{X}
end

;*************Procedimientos******************

local X D 
in 
proc{X Y} +{Y 1}end  
set D ={X 2}end

local X Y Z 
in 
proc {X Y Z} set Y=Z end 
set Y=2 {X Y Z} 
Z 
end

local X Y Z 
in 
proc {X Y Z} set Y=Z end 
set Y=2 {X Y Z} 
end

local X 
in 
set X = proc{$ Y Z} +{Y Z}end 
X 
end

local X 
in 
set X = proc{$ Y Z} +{Y Z}end 
{X 2 3} 
end

local X Y Z 
in 
set X = proc{$ Y Z} set Y = Z end 
set Y = 2 {X Y Z} 
end

local X Y Z 
in 
set X = proc{$ Y Z} set Y = Z end 
set Y = 2 
{X Y Z} 
Z 
end

local X 
in fun{X Y Z} 
set Y = Z 
end 
X 
end

local X Y 
in fun{X Y Z} 
set Y = Z 
end 
{X 2 Y} 
Y 
end

