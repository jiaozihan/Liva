; ModuleID = 'Liva'

%test = type <{ i32 }>

@tmp = private unnamed_addr constant [6 x i8] c"ggggg\00"
@tmp1 = private unnamed_addr constant [3 x i8] c"%s\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %a = alloca i32
  store i32 1, i32* %a
  %tmp = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @tmp1, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}

define i32 @test.add(%test* %this) {
entry:
  ret i32 1
}
