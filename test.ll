; ModuleID = 'Liva'

%test = type <{ i32 }>
%calculator = type <{ i32, i32 }>

@tmp = private unnamed_addr constant [18 x i8] c" constructor ok!\0A\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"%d%s\00"

declare i32 @printf(i8*, ...)

declare i8* @malloc(i32)

define %test* @test.constructor() {
entry:
  %this = alloca %test
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %test*
  %tmp2 = load %test* %tmp1
  store %test %tmp2, %test* %this
  %.key = getelementptr inbounds %test* %this, i32 0, i32 0
  store i32 1, i32* %.key
  ret %test* %this
}

define i32 @calculator.addition(%calculator* %this, i32 %x, i32 %y) {
entry:
  %z = alloca i32
  %addtmp = add i32 %x, %y
  store i32 %addtmp, i32* %z
  %z1 = load i32* %z
  ret i32 %z1
}

define %calculator* @calculator.constructor() {
entry:
  %this = alloca %calculator
  %tmp = call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1** null, i32 1) to i32))
  %tmp1 = bitcast i8* %tmp to %calculator*
  %tmp2 = load %calculator* %tmp1
  store %calculator %tmp2, %calculator* %this
  %.key = getelementptr inbounds %calculator* %this, i32 0, i32 0
  store i32 0, i32* %.key
  ret %calculator* %this
}

define i32 @main() {
entry:
  %a = alloca i32
  %b = alloca i32
  %c = alloca i32
  store i32 10, i32* %a
  store i32 40, i32* %b
  store i32 4, i32* %c
  %obj = alloca %calculator
  %tmp = call %calculator* @calculator.constructor()
  %tmp1 = load %calculator* %tmp
  store %calculator %tmp1, %calculator* %obj
  %c2 = load i32* %c
  %tmp3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0), i32 %c2, i8* getelementptr inbounds ([18 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}
