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

local {I F}
 in
  set I =1
  set F =9
 for Y in I .. F do
  +{Y Y}
 end
end


;Registros
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




