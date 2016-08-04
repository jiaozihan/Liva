; ModuleID = 'Liva'

%test = type <{ i32 }>

@tmp = private unnamed_addr constant [2 x i8] c"\0A\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"%d%s\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %a = alloca i32
  %b = alloca i32
  %c = alloca i32
  store i32 1, i32* %a
  store i32 2, i32* %b
  %a1 = load i32* %a
  store i32 %a1, i32* %c
  %c2 = load i32* %c
  %tmp = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0), i32 %c2, i8* getelementptr inbounds ([2 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}

define i32 @test.add(%test* %this) {
entry:
  ret i32 1
}
