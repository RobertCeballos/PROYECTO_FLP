;PRUEBAS INTERPRETE (estas son algunas de las expresiones que ya funcionan en el interprete)

+{3 4 5}
*{A B}
<{B C}

isdet?{B}
isfree?{A}


local{X}
in
set X = 5
X
end


local{X Y}
  in
    set B=Y
    set Y=X
    X
end


local{X}
 in
  local{Y}
   in
    set Y=12
    set Y=X
   end
  X
end


local{X Y}
in
set X = ~5.0
set 3.2 = Y
*{X Y}
end


local{X Y Z}
in
set X = ~5.0
set 3.2 = Y
set Z= *{X Y}
Z
end

local{X Y Z}
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
